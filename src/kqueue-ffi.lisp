(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; kqueue(2) FFI — thin CFFI bindings to FreeBSD's kqueue/kevent syscalls.
;;;
;;; No established Common Lisp kqueue library exists, so we bind the three
;;; primitives directly:
;;;   kqueue()  — create a new kqueue file descriptor
;;;   kevent()  — register interest / poll for events
;;;   struct kevent — the event descriptor
;;;
;;; Higher-level Lisp wrappers (make-kqueue, register-kevent, poll-kevents)
;;; are provided at the bottom.  The kqueue event loop in kqueue.lisp builds
;;; on these.
;;; --------------------------------------------------------------------------

;;; --- Constants (from sys/event.h) ----------------------------------------

;; Filters
(defconstant +evfilt-read+    -1)
(defconstant +evfilt-write+   -2)
(defconstant +evfilt-vnode+   -4)
(defconstant +evfilt-proc+    -5)
(defconstant +evfilt-timer+   -7)

;; Actions / flags
(defconstant +ev-add+       #x0001)
(defconstant +ev-enable+    #x0004)
(defconstant +ev-disable+   #x0008)
(defconstant +ev-delete+    #x0002)
(defconstant +ev-oneshot+   #x0010)
(defconstant +ev-clear+     #x0020)
(defconstant +ev-eof+       #x8000)
(defconstant +ev-error+     #x4000)

;; fflags for EVFILT_VNODE
(defconstant +note-delete+   #x0001)
(defconstant +note-write+    #x0002)
(defconstant +note-extend+   #x0004)
(defconstant +note-attrib+   #x0008)
(defconstant +note-link+     #x0010)
(defconstant +note-rename+   #x0020)
(defconstant +note-revoke+   #x0040)

;; fflags for EVFILT_PROC
(defconstant +note-exit+     #x80000000)
(defconstant +note-fork+     #x40000000)
(defconstant +note-exec+     #x20000000)
(defconstant +note-track+    #x00000001)

;;; --- CFFI struct and function definitions --------------------------------

(cffi:defcstruct kevent
  "struct kevent from sys/event.h"
  (ident  :uintptr)    ; identifier (fd, pid, etc.)
  (filter :short)      ; filter type (EVFILT_*)
  (flags  :unsigned-short)
  (fflags :unsigned-int)
  (data   :intptr)     ; filter-specific data
  (udata  :pointer))   ; opaque user data

(cffi:defcfun ("kqueue" %kqueue) :int
  "Create a new kqueue.  Returns a file descriptor or -1 on error.")

(cffi:defcfun ("kevent" %kevent) :int
  "Register events with / retrieve events from a kqueue."
  (kq       :int)
  (changelist :pointer)
  (nchanges   :int)
  (eventlist  :pointer)
  (nevents    :int)
  (timeout    :pointer))

(cffi:defcfun ("close" %close) :int
  (fd :int))

;;; --- Timespec (needed for kevent timeout) ---------------------------------

(cffi:defcstruct timespec
  (tv-sec  :long)
  (tv-nsec :long))

;;; --- Lisp wrappers -------------------------------------------------------

(defun make-kqueue ()
  "Create a new kqueue.  Returns the file descriptor (integer).
   Signals an error if the syscall fails."
  (let ((fd (%kqueue)))
    (when (minusp fd)
      (error "kqueue() failed with errno"))
    fd))

(defun close-kqueue (fd)
  "Close a kqueue file descriptor."
  (%close fd))

(defun register-kevent (kq-fd &key ident filter flags fflags (udata 0))
  "Register a single event with the kqueue.
   IDENT  — identifier (PID for PROC, fd for VNODE, arbitrary for TIMER).
   FILTER — one of the +EVFILT-*+ constants.
   FLAGS  — bitwise OR of +EV-*+ constants (typically +EV-ADD+ | +EV-ENABLE+).
   FFLAGS — filter-specific flags (e.g. +NOTE-EXIT+ for PROC).
   UDATA  — integer stored as opaque user data (we encode container index)."
  (cffi:with-foreign-object (change '(:struct kevent))
    (setf (cffi:foreign-slot-value change '(:struct kevent) 'ident)  ident
          (cffi:foreign-slot-value change '(:struct kevent) 'filter) filter
          (cffi:foreign-slot-value change '(:struct kevent) 'flags)  flags
          (cffi:foreign-slot-value change '(:struct kevent) 'fflags) fflags
          (cffi:foreign-slot-value change '(:struct kevent) 'data)   0
          (cffi:foreign-slot-value change '(:struct kevent) 'udata)
          (cffi:make-pointer udata))
    (let ((ret (%kevent kq-fd change 1 (cffi:null-pointer) 0 (cffi:null-pointer))))
      (when (minusp ret)
        (error "kevent() registration failed for ident ~A filter ~A" ident filter))
      ret)))

(defstruct kqueue-event
  "Lisp-friendly representation of a returned kevent."
  (ident  0 :type integer)
  (filter 0 :type integer)
  (flags  0 :type integer)
  (fflags 0 :type integer)
  (data   0 :type integer)
  (udata  0 :type integer))

(defun poll-kevents (kq-fd &key (max-events 32) (timeout-ms nil))
  "Block on the kqueue until events arrive (or TIMEOUT-MS elapses).
   Returns a list of KQUEUE-EVENT structs.  If TIMEOUT-MS is NIL, blocks
   indefinitely."
  (cffi:with-foreign-object (events '(:struct kevent) max-events)
    (cffi:with-foreign-object (ts '(:struct timespec))
      (let ((ts-ptr (if timeout-ms
                        (progn
                          (setf (cffi:foreign-slot-value ts '(:struct timespec) 'tv-sec)
                                (floor timeout-ms 1000)
                                (cffi:foreign-slot-value ts '(:struct timespec) 'tv-nsec)
                                (* (mod timeout-ms 1000) 1000000))
                          ts)
                        (cffi:null-pointer))))
        (let ((n (%kevent kq-fd (cffi:null-pointer) 0 events max-events ts-ptr)))
          (when (minusp n)
            (error "kevent() poll failed"))
          (loop for i below n
                collect (let ((ev (cffi:mem-aptr events '(:struct kevent) i)))
                          (make-kqueue-event
                           :ident  (cffi:foreign-slot-value ev '(:struct kevent) 'ident)
                           :filter (cffi:foreign-slot-value ev '(:struct kevent) 'filter)
                           :flags  (cffi:foreign-slot-value ev '(:struct kevent) 'flags)
                           :fflags (cffi:foreign-slot-value ev '(:struct kevent) 'fflags)
                           :data   (cffi:foreign-slot-value ev '(:struct kevent) 'data)
                           :udata  (cffi:pointer-address
                                    (cffi:foreign-slot-value ev '(:struct kevent)
                                                             'udata))))))))))
