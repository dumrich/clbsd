(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Docker / Alpine Bhyve VM backend
;;;
;;; Docker requires a real Linux kernel.  FreeBSD jails share the host kernel
;;; and the Linuxulator does not implement Linux namespaces or cgroups.
;;; CLBSD therefore runs a minimal Alpine Linux guest under Bhyve and talks
;;; to its Docker daemon over a TCP-exposed socket.
;;;
;;; The Bhyve VM is infrastructure — not a user-facing container.  Its
;;; lifecycle is tied to the CLBSD image: booted on init, shut down on exit.
;;;
;;; Volume passthrough:
;;;   FreeBSD ZFS dataset  →  virtio-9p/NFS  →  Alpine mount  →  Docker -v
;;; --------------------------------------------------------------------------

;;; --- Docker CLI ----------------------------------------------------------

(defun docker-cmd (args)
  "Run a docker CLI command against the Bhyve VM's daemon.
   ARGS is a list of strings (e.g. '(\"ps\" \"--format\" \"json\")).
   Returns stdout as a string.  Signals SHELL-ERROR on failure."
  (run-cmd (cons "docker" args)
           :environment (list (format nil "DOCKER_HOST=~A" *docker-host*))))

;;; --- Bhyve VM management -------------------------------------------------

(defvar *bhyve-vm-name* "alpine-docker"
  "Name of the Bhyve VM that hosts the Docker daemon.")

(defvar *bhyve-cpus* "2"
  "Number of vCPUs allocated to the Docker Bhyve VM.")

(defvar *bhyve-memory* "1G"
  "Memory allocated to the Docker Bhyve VM.")

(defun docker-vm-disk-path ()
  "Path to the Alpine VM disk image on the host."
  (format nil "/~A/vm/alpine.img" *zpool-root*))

(defun boot-docker-vm ()
  "Start the Alpine Bhyve VM.  Called once during CLBSD init.
   Blocks until the VM is booted (or errors)."
  (log-event :bhyve-booting :vm *bhyve-vm-name*)
  (run-cmd (list "bhyvectl" "--create"
                 (format nil "--vm=~A" *bhyve-vm-name*)))
  (run-cmd (list "bhyve"
                 "-c" *bhyve-cpus*
                 "-m" *bhyve-memory*
                 "-s" "0,hostbridge"
                 "-s" "1,lpc"
                 "-s" "2,virtio-net,tap0"
                 "-s" (format nil "3,virtio-blk,~A" (docker-vm-disk-path))
                 "-s" (format nil "4,virtio-9p,clbsd-volumes=/~A" *zpool-root*)
                 "-l" "com1,stdio"
                 *bhyve-vm-name*))
  (log-event :bhyve-booted :vm *bhyve-vm-name*)
  t)

(defun shutdown-docker-vm ()
  "Gracefully shut down the Alpine Bhyve VM."
  (log-event :bhyve-shutting-down :vm *bhyve-vm-name*)
  (handler-case
      (docker-cmd '("run" "--rm" "--privileged" "alpine" "poweroff"))
    (shell-error () nil))
  (handler-case
      (run-cmd (list "bhyvectl" "--destroy"
                     (format nil "--vm=~A" *bhyve-vm-name*)))
    (shell-error () nil))
  (log-event :bhyve-stopped :vm *bhyve-vm-name*)
  t)

(defun docker-vm-running-p ()
  "Return T if the Docker Bhyve VM appears to be running."
  (handler-case
      (progn
        (docker-cmd '("info" "--format" "{{.ServerVersion}}"))
        t)
    (error () nil)))

;;; --- Container lifecycle -------------------------------------------------

(defun docker-volume-path (container)
  "Host-side path for this Docker container's persistent data volume."
  (format nil "/~A/~A/data" *zpool-root* (container-name container)))

(defun docker-volume-guest-path (container)
  "Guest-side (inside the Bhyve VM) mount path for the volume."
  (format nil "/clbsd-volumes/~A/data" (container-name container)))

(defun create-docker (container)
  "Pull the Docker image and create the ZFS dataset for volume storage."
  (create-dataset container :template "freebsd-14@base")
  (docker-cmd (list "pull" (container-image container)))
  (log-event :container-created :container (container-name container) :type :docker)
  t)

(defun start-docker (container)
  "Start a Docker container inside the Bhyve VM."
  (let ((args (list "run" "-d"
                    "--name" (container-name container)
                    "-v" (format nil "~A:/data"
                                 (docker-volume-guest-path container)))))
    (when (container-port container)
      (setf args (append args
                         (list "-p" (format nil "~D:~D"
                                            (container-port container)
                                            (container-port container))))))
    (setf args (append args (list (container-image container))))
    (docker-cmd args)
    (log-event :container-started :container (container-name container) :type :docker)
    t))

(defun stop-docker (container)
  "Stop a Docker container inside the Bhyve VM."
  (docker-cmd (list "stop" (container-name container)))
  (log-event :container-stopped :container (container-name container) :type :docker)
  t)

(defun destroy-docker (container)
  "Remove a Docker container and destroy its ZFS dataset."
  (handler-case (docker-cmd (list "rm" "-f" (container-name container)))
    (shell-error () nil))
  (destroy-dataset container)
  (log-event :container-destroyed :container (container-name container) :type :docker)
  t)

;;; --- Introspection -------------------------------------------------------

(defun docker-running-p (container)
  "Return T if a Docker container with this name is currently running."
  (handler-case
      (let ((output (docker-cmd (list "inspect" "-f" "{{.State.Running}}"
                                      (container-name container)))))
        (string= (string-trim '(#\Space #\Newline) output) "true"))
    (error () nil)))

(defun docker-container-ip (container)
  "Resolve the IP address of a running Docker container from docker inspect."
  (handler-case
      (let ((output (docker-cmd
                     (list "inspect" "-f"
                           "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}"
                           (container-name container)))))
        (let ((ip (string-trim '(#\Space #\Newline) output)))
          (when (plusp (length ip)) ip)))
    (error () nil)))

;;; --- Observation (used by reconciler) ------------------------------------

(defun observe-docker-containers ()
  "Return a list of names of currently running Docker containers.
   Returns NIL on failure (e.g. VM not running)."
  (handler-case
      (let* ((json-str (docker-cmd '("ps" "--format" "{{json .}}")))
             (lines    (uiop:split-string json-str :separator '(#\Newline))))
        (loop for line in lines
              for trimmed = (string-trim '(#\Space #\Return) line)
              when (plusp (length trimmed))
                collect (let ((parsed (yason:parse trimmed)))
                          (gethash "Names" parsed))))
    (error () nil)))
