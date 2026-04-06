(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Built-in kqueue / lifecycle handlers.
;;;
;;; These are default callbacks the operator can reference by name in a
;;; container's :callbacks a-list.  They are safe to call from any thread
;;; (callback workers spawned by the kqueue loop).
;;;
;;; Custom handlers follow the same signature: (container event) where
;;; EVENT is a kqueue-event struct (or NIL for lifecycle-only hooks).
;;; --------------------------------------------------------------------------

(defun auto-restart-handler (container event)
  "Mark the container as stopped and signal the reconciler, which will
   see the discrepancy between desired (:running) and observed (:stopped)
   state and restart it.  This is the standard crash-recovery handler."
  (declare (ignore event))
  (log-event :auto-restart :container (container-name container))
  (setf (%container-state container) :stopped)
  (signal-reconciler))

(defun log-and-alert-handler (container event)
  "Log the event for observability.  The operator can replace or wrap this
   handler to add external notifications (email, webhook, etc.)."
  (log-event :alert
             :container (container-name container)
             :filter (when event (kqueue-event-filter event))
             :ident  (when event (kqueue-event-ident event))
             :detail (format nil "~A" event)))
