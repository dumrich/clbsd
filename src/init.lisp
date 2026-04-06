(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Boot / shutdown orchestration — the SBCL image lifecycle.
;;;
;;; boot-clbsd runs the full §2.1 startup sequence:
;;;   1. Compute derived config
;;;   2. Boot the Docker Bhyve VM
;;;   3. Start the routing jail
;;;   4. Load the operator's declarative init file
;;;   5. Start the reconciler thread
;;;   6. Start the kqueue thread
;;;   7. Start the Swank server
;;;
;;; shutdown-clbsd reverses this:
;;;   1. Stop Swank
;;;   2. Stop kqueue thread
;;;   3. Stop reconciler thread
;;;   4. Stop all managed containers
;;;   5. Stop routing jail
;;;   6. Shut down Docker Bhyve VM
;;; --------------------------------------------------------------------------

(defun compute-derived-config ()
  "Fill in config values that depend on other config.
   Called once at the beginning of boot-clbsd."
  (unless *routing-jail-dataset*
    (setf *routing-jail-dataset*
          (format nil "/~A/~A" *zpool-root* *routing-jail-name*)))
  (unless *routing-jail*
    (setf *routing-jail*
          (make-container :name  *routing-jail-name*
                          :type  :jail
                          :dataset (format nil "~A/~A"
                                           *zpool-root* *routing-jail-name*)
                          :state :stopped))))

(defun start-routing-jail ()
  "Ensure the routing jail (Nginx) is running."
  (handler-case
      (unless (jail-running-p *routing-jail*)
        (start-jail *routing-jail*)
        (setf (%container-state *routing-jail*) :running))
    (error (e)
      (log-event :routing-jail-start-failed :detail (princ-to-string e))
      (warn "Could not start routing jail: ~A" e))))

(defun stop-routing-jail ()
  "Stop the routing jail."
  (handler-case
      (when (jail-running-p *routing-jail*)
        (stop-jail *routing-jail*)
        (setf (%container-state *routing-jail*) :stopped))
    (error (e)
      (log-event :routing-jail-stop-failed :detail (princ-to-string e)))))

(defun load-init-file (&optional (path *init-file-path*))
  "Load the operator's declarative init file.  This typically sets
   *clbsd-state* to a list of container structs.  After loading,
   signals the reconciler to converge."
  (if (probe-file path)
      (progn
        (log-event :init-file-loading :path path)
        (load path)
        (log-event :init-file-loaded :path path
                   :containers (length *clbsd-state*))
        (signal-reconciler))
      (log-event :init-file-not-found :path path)))

(defun stop-all-containers ()
  "Stop every container in *clbsd-state*."
  (bt:with-lock-held (*state-lock*)
    (dolist (c *clbsd-state*)
      (handler-case
          (when (running-p c)
            (stop c))
        (error (e)
          (log-event :container-stop-failed
                     :container (container-name c)
                     :detail (princ-to-string e)))))))

(defun register-kqueue-watches ()
  "After the init file is loaded and the reconciler has run once, register
   kqueue watches for all containers that have relevant callbacks."
  (bt:with-lock-held (*state-lock*)
    (dolist (c *clbsd-state*)
      (when (assoc :on-change (container-callbacks c))
        (watch-dataset-changes c))
      (when (assoc :on-health (container-callbacks c))
        (register-health-timer c)))))

;;; --- Public entry points -------------------------------------------------

(defun boot-clbsd ()
  "Full boot sequence.  Call this to start the CLBSD control plane."
  (setf *shutdown-requested* nil)
  (log-event :boot-start)

  ;; 1. Derived config
  (compute-derived-config)
  (log-event :config-ready
             :zpool-root *zpool-root*
             :docker-host *docker-host*
             :swank-port *swank-port*)

  ;; 2. Boot Docker VM
  (handler-case
      (progn
        (boot-docker-vm)
        (log-event :docker-vm-ready))
    (error (e)
      (log-event :docker-vm-boot-failed :detail (princ-to-string e))
      (warn "Docker VM failed to boot — Docker containers will be unavailable: ~A" e)))

  ;; 3. Routing jail
  (start-routing-jail)

  ;; 4. Reconciler thread (must be running before init file load)
  (start-reconciler)

  ;; 5. Load init file (sets *clbsd-state*, signals reconciler)
  (load-init-file)

  ;; 6. kqueue
  (init-kqueue)
  (register-kqueue-watches)
  (start-kqueue-thread)

  ;; 7. Swank
  (start-clbsd-swank)

  (log-event :boot-complete)
  (format t "~&CLBSD is live.  Connect via SLIME on port ~D.~%" *swank-port*)
  t)

(defun shutdown-clbsd ()
  "Full shutdown sequence.  Reverses boot-clbsd."
  (log-event :shutdown-start)

  ;; 1. Swank
  (stop-clbsd-swank)

  ;; 2. kqueue
  (shutdown-kqueue)

  ;; 3. Reconciler
  (stop-reconciler)

  ;; 4. All managed containers
  (stop-all-containers)

  ;; 5. Routing jail
  (stop-routing-jail)

  ;; 6. Docker VM
  (handler-case (shutdown-docker-vm)
    (error (e)
      (log-event :docker-vm-shutdown-failed :detail (princ-to-string e))))

  (log-event :shutdown-complete)
  (format t "~&CLBSD shut down.~%")
  t)
