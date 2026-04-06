(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Swank server management — SLIME endpoint for live interaction.
;;;
;;; Swank is loaded at runtime via REQUIRE rather than declared as an ASDF
;;; dependency, so CLBSD can load on systems without Swank installed.
;;; The operator connects from Emacs with M-x slime-connect.
;;;
;;; Through this REPL the operator has full access to:
;;;   *clbsd-state*   — read or mutate desired state
;;;   Layer 1 API     — (start c), (stop c), (snapshot c "label"), etc.
;;;   *event-log*     — query recent events
;;;   load-init-file  — re-apply declarative config
;;; --------------------------------------------------------------------------

(defvar *swank-server* nil
  "Handle to the running Swank server, or NIL.")

(defun ensure-swank-loaded ()
  "Load Swank if it is not already present."
  (unless (find-package :swank)
    (handler-case
        (progn
          #+quicklisp (ql:quickload :swank :silent t)
          #-quicklisp (require :swank))
      (error (e)
        (log-event :swank-load-failed :detail (princ-to-string e))
        (warn "Could not load Swank: ~A" e)
        (return-from ensure-swank-loaded nil))))
  t)

(defun start-clbsd-swank (&key (port *swank-port*))
  "Start a Swank server on PORT.  Idempotent — does nothing if already running.
   Returns the port number or NIL on failure."
  (when (ensure-swank-loaded)
    (unless *swank-server*
      (let ((create-server (find-symbol "CREATE-SERVER" :swank)))
        (when create-server
          (funcall create-server :port port :dont-close t)
          (setf *swank-server* port)
          (log-event :swank-started :port port))))
    *swank-server*))

(defun stop-clbsd-swank ()
  "Stop the Swank server if running."
  (when *swank-server*
    (let ((stop-server (find-symbol "STOP-SERVER" :swank)))
      (when stop-server
        (handler-case (funcall stop-server *swank-server*)
          (error () nil))))
    (setf *swank-server* nil)
    (log-event :swank-stopped))
  t)
