(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Reconciliation loop — background thread that converges actual system
;;; state toward *clbsd-state*.
;;;
;;; This is the sole component that reads dirty-p and performs side effects
;;; against jails, Docker, and ZFS.  All other threads (kqueue, Swank,
;;; callback workers) only SIGNAL the reconciler; they never call backend
;;; functions directly.
;;;
;;; Thread safety:
;;;   *reconciler-lock*    — guards the condition variable wait/signal.
;;;   *state-lock*         — guards reads/writes of *clbsd-state*.
;;;   The reconciler acquires *state-lock* to snapshot state, then releases
;;;   it before performing (potentially slow) backend calls.
;;; --------------------------------------------------------------------------

(defvar *reconciler-lock* (bt:make-lock "reconciler-lock")
  "Lock used with *reconciler-condvar* to wake the reconciler thread.")

(defvar *reconciler-condvar* (bt:make-condition-variable
                               :name "reconciler-condvar")
  "Condition variable signalled whenever *clbsd-state* is mutated.")

(defvar *state-lock* (bt:make-lock "state-lock")
  "Guards all reads and writes of *clbsd-state*.")

(defvar *reconciler-thread* nil
  "Handle to the running reconciler thread, or NIL.")

(defun signal-reconciler ()
  "Wake the reconciler thread.  Safe to call from any thread."
  (bt:with-lock-held (*reconciler-lock*)
    (bt:condition-notify *reconciler-condvar*)))

;;; --- Observation ---------------------------------------------------------

(defun observe-running-containers ()
  "Build the set of currently running container names from both backends.
   Returns a list of strings."
  (append (or (observe-jails) nil)
          (or (observe-docker-containers) nil)))

;;; --- Diff ----------------------------------------------------------------

(defun compute-diff (desired observed-names)
  "Compare DESIRED (list of container structs) against OBSERVED-NAMES
   (list of name strings).  Returns three values:
     1. TO-CREATE  — containers in desired but not observed
     2. TO-DESTROY — names in observed but not in desired
     3. DIRTY      — containers in desired that have dirty-p set"
  (let ((desired-names (mapcar #'container-name desired))
        (to-create  '())
        (to-destroy '())
        (dirty      '()))
    (dolist (c desired)
      (cond
        ((not (member (container-name c) observed-names :test #'string=))
         (push c to-create))
        ((%container-dirty-p c)
         (push c dirty))))
    (dolist (name observed-names)
      (unless (member name desired-names :test #'string=)
        (push name to-destroy)))
    (values (nreverse to-create)
            (nreverse to-destroy)
            (nreverse dirty))))

;;; --- Apply ---------------------------------------------------------------

(defun apply-creates (containers)
  "Create and start each container in the list."
  (dolist (c containers)
    (handler-case
        (progn
          (create c)
          (start c)
          (log-event :reconciler-created :container (container-name c)))
      (error (e)
        (setf (%container-state c) :error)
        (log-event :reconciler-create-failed
                   :container (container-name c)
                   :detail (princ-to-string e))))))

(defun apply-destroys (names)
  "Stop and destroy containers by name.  Looks up structs in *clbsd-state*
   for the destroy call; if not found, attempts a raw jail/docker stop."
  (dolist (name names)
    (handler-case
        (let ((c (find-container name)))
          (if c
              (destroy c)
              (progn
                (handler-case (run-cmd (list "jail" "-r" name))
                  (shell-error () nil))
                (handler-case (docker-cmd (list "rm" "-f" name))
                  (shell-error () nil))))
          (log-event :reconciler-destroyed :container name))
      (error (e)
        (log-event :reconciler-destroy-failed
                   :container name
                   :detail (princ-to-string e))))))

(defun apply-dirty (containers)
  "Reconfigure dirty containers by stopping, reconfiguring, and restarting."
  (dolist (c containers)
    (handler-case
        (progn
          (when (running-p c) (stop c))
          (when (eq (container-type c) :jail)
            (write-jail-conf c))
          (start c)
          (setf (%container-dirty-p c) nil)
          (log-event :reconciler-reconfigured :container (container-name c)))
      (error (e)
        (setf (%container-state c) :error)
        (log-event :reconciler-reconfig-failed
                   :container (container-name c)
                   :detail (princ-to-string e))))))

(defun apply-diff (desired observed-names)
  "Compute and apply the full diff between desired and observed state."
  (multiple-value-bind (to-create to-destroy dirty)
      (compute-diff desired observed-names)
    (when to-destroy (apply-destroys to-destroy))
    (when to-create  (apply-creates to-create))
    (when dirty      (apply-dirty dirty))))

;;; --- Main loop -----------------------------------------------------------

(defun reconcile-once (desired)
  "Run one reconciliation pass: observe, diff, apply, regenerate nginx."
  (log-event :reconciler-start)
  (let ((observed (observe-running-containers)))
    (apply-diff desired observed)
    (write-nginx-conf desired)
    (reload-nginx))
  (log-event :reconciler-done))

(defun reconciler-loop ()
  "Main body for the reconciler thread.  Sleeps until signalled, then
   runs one reconciliation pass.  Exits when *shutdown-requested* is T."
  (log-event :reconciler-thread-started)
  (loop
    (bt:with-lock-held (*reconciler-lock*)
      (bt:condition-wait *reconciler-condvar* *reconciler-lock*))
    (when *shutdown-requested*
      (log-event :reconciler-thread-exiting)
      (return))
    (handler-case
        (let ((snapshot (bt:with-lock-held (*state-lock*)
                          (copy-list *clbsd-state*))))
          (reconcile-once snapshot))
      (error (e)
        (log-event :reconciler-error :detail (princ-to-string e))
        (warn "CLBSD reconciler error: ~A" e)))))

(defun start-reconciler ()
  "Start the reconciler background thread.  Idempotent."
  (unless (and *reconciler-thread* (bt:thread-alive-p *reconciler-thread*))
    (setf *reconciler-thread*
          (bt:make-thread #'reconciler-loop :name "clbsd-reconciler"))
    (log-event :reconciler-started))
  *reconciler-thread*)

(defun stop-reconciler ()
  "Request the reconciler thread to exit and wait for it."
  (when (and *reconciler-thread* (bt:thread-alive-p *reconciler-thread*))
    (setf *shutdown-requested* t)
    (signal-reconciler)
    (bt:join-thread *reconciler-thread*)
    (setf *reconciler-thread* nil)
    (log-event :reconciler-stopped))
  t)
