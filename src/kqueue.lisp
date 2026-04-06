(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; kqueue event system — Layer 2's event-driven backbone.
;;;
;;; Runs a dedicated thread that blocks on kevent(), dispatches events to
;;; container callbacks, and signals the reconciler as needed.
;;;
;;; Watched events:
;;;   EVFILT_PROC  — jail init process exits      → :on-crash
;;;   EVFILT_VNODE — changes to dataset files      → :on-change
;;;   EVFILT_TIMER — periodic health-check         → :on-health
;;;
;;; Callbacks from the container's :callbacks a-list are executed in fresh
;;; threads to avoid blocking the kqueue loop.  The udata field encodes a
;;; lookup key into a name→container registry.
;;; --------------------------------------------------------------------------

(defvar *kqueue-fd* nil
  "File descriptor for the main kqueue instance.")

(defvar *kqueue-thread* nil
  "Handle to the running kqueue event loop thread.")

(defvar *kqueue-registry* (make-hash-table :test 'equal)
  "Maps udata integer keys → container name strings, so the kqueue loop
   can look up which container an event belongs to.")

(defvar *kqueue-registry-lock* (bt:make-lock "kqueue-registry-lock"))

(defvar *kqueue-next-id* 0
  "Monotonically increasing counter for generating udata keys.")

(defun next-kqueue-id ()
  "Return the next unique kqueue udata identifier."
  (bt:with-lock-held (*kqueue-registry-lock*)
    (incf *kqueue-next-id*)))

(defun register-kqueue-name (id name)
  "Associate udata ID with a container NAME in the registry."
  (bt:with-lock-held (*kqueue-registry-lock*)
    (setf (gethash id *kqueue-registry*) name)))

(defun lookup-kqueue-name (id)
  "Look up the container name for udata ID."
  (bt:with-lock-held (*kqueue-registry-lock*)
    (gethash id *kqueue-registry*)))

;;; --- Initialization ------------------------------------------------------

(defun init-kqueue ()
  "Create the kqueue file descriptor.  Must be called before any watches."
  (setf *kqueue-fd* (make-kqueue))
  (clrhash *kqueue-registry*)
  (setf *kqueue-next-id* 0)
  (log-event :kqueue-initialized :fd *kqueue-fd*)
  *kqueue-fd*)

(defun shutdown-kqueue ()
  "Stop the kqueue thread and close the file descriptor."
  (when (and *kqueue-thread* (bt:thread-alive-p *kqueue-thread*))
    (setf *shutdown-requested* t)
    (bt:join-thread *kqueue-thread*)
    (setf *kqueue-thread* nil))
  (when *kqueue-fd*
    (close-kqueue *kqueue-fd*)
    (setf *kqueue-fd* nil))
  (log-event :kqueue-shutdown)
  t)

;;; --- Watch registration --------------------------------------------------

(defun watch-jail-process (container pid)
  "Register EVFILT_PROC on PID so we detect when a jail's init process exits.
   Fires the :on-crash callback."
  (let ((id (next-kqueue-id)))
    (register-kqueue-name id (container-name container))
    (register-kevent *kqueue-fd*
                     :ident  pid
                     :filter +evfilt-proc+
                     :flags  (logior +ev-add+ +ev-enable+ +ev-oneshot+)
                     :fflags +note-exit+
                     :udata  id)
    (log-event :kqueue-watch-proc
               :container (container-name container)
               :pid pid
               :udata id)
    id))

(defun watch-dataset-changes (container)
  "Register EVFILT_VNODE on the container's data directory so we detect
   filesystem changes.  Fires the :on-change callback.
   Returns the udata id, or NIL if the path doesn't exist."
  (let ((path (format nil "/~A/data" (dataset-path container))))
    (when (probe-file path)
      (let* ((fd (sb-posix:open path sb-posix:o-rdonly 0))
             (id (next-kqueue-id)))
        (register-kqueue-name id (container-name container))
        (register-kevent *kqueue-fd*
                         :ident  fd
                         :filter +evfilt-vnode+
                         :flags  (logior +ev-add+ +ev-enable+ +ev-clear+)
                         :fflags (logior +note-write+ +note-rename+ +note-delete+)
                         :udata  id)
        (log-event :kqueue-watch-vnode
                   :container (container-name container)
                   :path path
                   :udata id)
        id))))

(defun register-health-timer (container &key (interval-ms 30000))
  "Register EVFILT_TIMER to fire periodically for health checks.
   Fires the :on-health callback every INTERVAL-MS milliseconds."
  (let ((id (next-kqueue-id)))
    (register-kqueue-name id (container-name container))
    (register-kevent *kqueue-fd*
                     :ident  id
                     :filter +evfilt-timer+
                     :flags  (logior +ev-add+ +ev-enable+)
                     :fflags 0
                     :udata  id)
    ;; EVFILT_TIMER uses the data field for the interval in milliseconds.
    ;; We set it via the fflags/data — on FreeBSD, kevent data is the period.
    ;; Re-register with the interval encoded.  The register-kevent helper
    ;; sets data to 0, so we do a raw kevent call here.
    (cffi:with-foreign-object (change '(:struct kevent))
      (setf (cffi:foreign-slot-value change '(:struct kevent) 'ident)  id
            (cffi:foreign-slot-value change '(:struct kevent) 'filter) +evfilt-timer+
            (cffi:foreign-slot-value change '(:struct kevent) 'flags)  (logior +ev-add+ +ev-enable+)
            (cffi:foreign-slot-value change '(:struct kevent) 'fflags) 0
            (cffi:foreign-slot-value change '(:struct kevent) 'data)   interval-ms
            (cffi:foreign-slot-value change '(:struct kevent) 'udata)  (cffi:make-pointer id))
      (%kevent *kqueue-fd* change 1 (cffi:null-pointer) 0 (cffi:null-pointer)))
    (log-event :kqueue-watch-timer
               :container (container-name container)
               :interval-ms interval-ms
               :udata id)
    id))

;;; --- Event → callback key mapping ----------------------------------------

(defun event->callback-key (event)
  "Map a kqueue-event struct to the callback keyword used in container
   :callbacks a-lists."
  (let ((filter (kqueue-event-filter event)))
    (cond
      ((= filter +evfilt-proc+)  :on-crash)
      ((= filter +evfilt-vnode+) :on-change)
      ((= filter +evfilt-timer+) :on-health)
      (t nil))))

;;; --- Main event loop -----------------------------------------------------

(defun dispatch-callback (container callback-key event)
  "Look up and invoke the callback for CALLBACK-KEY on CONTAINER.
   Runs the handler in a fresh thread to avoid blocking the kqueue loop."
  (let ((handler (cdr (assoc callback-key (container-callbacks container)))))
    (when handler
      (bt:make-thread
       (lambda ()
         (handler-case
             (progn
               (log-event :kqueue-callback-start
                          :container (container-name container)
                          :key callback-key)
               (funcall handler container event)
               (log-event :kqueue-callback-done
                          :container (container-name container)
                          :key callback-key))
           (error (e)
             (log-event :kqueue-callback-error
                        :container (container-name container)
                        :key callback-key
                        :detail (princ-to-string e)))))
       :name (format nil "clbsd-callback-~A-~A"
                      (container-name container) callback-key)))))

(defun kqueue-loop ()
  "Main body for the kqueue thread.  Blocks on kevent(), dispatches events.
   Exits when *shutdown-requested* is T."
  (log-event :kqueue-thread-started)
  (loop
    (when *shutdown-requested*
      (log-event :kqueue-thread-exiting)
      (return))
    (handler-case
        (let ((events (poll-kevents *kqueue-fd*
                                    :max-events 32
                                    :timeout-ms 1000)))
          (dolist (ev events)
            (let* ((uid  (kqueue-event-udata ev))
                   (name (lookup-kqueue-name uid))
                   (c    (when name
                           (bt:with-lock-held (*state-lock*)
                             (find-container name))))
                   (key  (event->callback-key ev)))
              (log-event :kqueue-event
                         :ident (kqueue-event-ident ev)
                         :filter (kqueue-event-filter ev)
                         :container name
                         :callback-key key)
              (when (and c key)
                (dispatch-callback c key ev)))))
      (error (e)
        (log-event :kqueue-loop-error :detail (princ-to-string e))
        (sleep 1)))))

(defun start-kqueue-thread ()
  "Start the kqueue event loop thread.  Idempotent."
  (unless (and *kqueue-thread* (bt:thread-alive-p *kqueue-thread*))
    (setf *kqueue-thread*
          (bt:make-thread #'kqueue-loop :name "clbsd-kqueue"))
    (log-event :kqueue-thread-launched))
  *kqueue-thread*)
