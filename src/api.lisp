(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Unified container API — generic functions that dispatch on
;;; (container-type c) to the jail or docker backend.
;;;
;;; The caller never needs to know which backend is active.  Method bodies
;;; contain only delegation to the backend functions and state bookkeeping.
;;; --------------------------------------------------------------------------

;;; --- Lifecycle -----------------------------------------------------------

(defgeneric create (container)
  (:documentation "Provision a new container: create ZFS dataset, pull image or
   clone template, write config.  Does NOT start the container."))

(defmethod create ((c container))
  (ecase (container-type c)
    (:jail   (create-jail c))
    (:docker (create-docker c))))

(defgeneric start (container)
  (:documentation "Start an existing container."))

(defmethod start ((c container))
  (ecase (container-type c)
    (:jail   (start-jail c))
    (:docker (start-docker c)))
  (setf (%container-state c) :running)
  (setf (%container-dirty-p c) nil)
  t)

(defgeneric stop (container)
  (:documentation "Stop a running container gracefully."))

(defmethod stop ((c container))
  (ecase (container-type c)
    (:jail   (stop-jail c))
    (:docker (stop-docker c)))
  (setf (%container-state c) :stopped)
  t)

(defgeneric restart (container)
  (:documentation "Stop then start a container."))

(defmethod restart ((c container))
  (stop c)
  (start c))

(defgeneric destroy (container)
  (:documentation "Remove a container entirely: stop, delete config, destroy dataset."))

(defmethod destroy ((c container))
  (ecase (container-type c)
    (:jail   (destroy-jail c))
    (:docker (destroy-docker c)))
  (setf (%container-state c) :stopped)
  t)

;;; --- Introspection -------------------------------------------------------

(defgeneric running-p (container)
  (:documentation "Return T if the container is currently running on the host."))

(defmethod running-p ((c container))
  (ecase (container-type c)
    (:jail   (jail-running-p c))
    (:docker (docker-running-p c))))

(defgeneric status (container)
  (:documentation "Return the observed state keyword for the container.
   Queries the host/VM rather than trusting the struct's cached state."))

(defmethod status ((c container))
  (if (running-p c) :running :stopped))

;;; --- REPL access ---------------------------------------------------------

(defgeneric connect-repl (container)
  (:documentation "Open a SLIME connection into the container's Lisp image.
   Only supported for jail containers that run an SBCL image with Swank."))

(defmethod connect-repl ((c container))
  (ecase (container-type c)
    (:jail
     (let ((ip (or (container-ip-addr c) (jail-ip c))))
       (when ip
         (log-event :repl-connect :container (container-name c) :ip ip)
         (format nil "Connect SLIME to ~A:4005" ip))))
    (:docker
     (error "connect-repl is not supported for Docker containers."))))
