(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Jail backend — thin wrapper over FreeBSD jail(8).
;;;
;;; CLBSD writes /etc/jail.conf fragments directly; it does not use
;;; bastille, cbsd, or any third-party jail manager.
;;;
;;; Every jail's rootfs is a ZFS dataset cloned from a template snapshot.
;;; Networking (IP assignment, bridge config) is the operator's responsibility;
;;; CLBSD writes the ip4.addr verbatim from the container struct.
;;; --------------------------------------------------------------------------

(defun jail-conf-path (container)
  "Path to this jail's config fragment under /etc/jail.conf.d/."
  (format nil "/etc/jail.conf.d/~A.conf" (container-name container)))

(defun generate-jail-conf-fragment (container)
  "Return a jail.conf(5) fragment string for CONTAINER."
  (format nil "~A {
  path = \"/~A\";
  ip4.addr = ~A;
  exec.start = \"/bin/sh /etc/rc\";
  exec.stop  = \"/bin/sh /etc/rc.shutdown\";
  mount.devfs;
  persist;
}~%"
          (container-name container)
          (dataset-path container)
          (or (container-ip-addr container) "inherit")))

(defun write-jail-conf (container)
  "Write the jail.conf fragment for CONTAINER to /etc/jail.conf.d/."
  (let ((path (jail-conf-path container))
        (conf (generate-jail-conf-fragment container)))
    (alexandria:write-string-into-file conf path
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
    (log-event :jail-conf-written :container (container-name container) :path path)
    path))

(defun remove-jail-conf (container)
  "Delete the jail.conf fragment for CONTAINER."
  (let ((path (jail-conf-path container)))
    (when (probe-file path)
      (delete-file path)
      (log-event :jail-conf-removed :container (container-name container) :path path))
    t))

;;; --- Lifecycle -----------------------------------------------------------

(defun create-jail (container)
  "Provision a new jail: clone ZFS dataset, write jail.conf, create jail."
  (create-dataset container)
  (write-jail-conf container)
  (run-cmd (list "jail" "-c" (container-name container)))
  (log-event :container-created :container (container-name container) :type :jail)
  t)

(defun start-jail (container)
  "Start an existing jail."
  (run-cmd (list "service" "jail" "start" (container-name container)))
  (log-event :container-started :container (container-name container) :type :jail)
  t)

(defun stop-jail (container)
  "Stop a running jail."
  (run-cmd (list "service" "jail" "stop" (container-name container)))
  (log-event :container-stopped :container (container-name container) :type :jail)
  t)

(defun destroy-jail (container)
  "Remove a jail completely: stop, remove config, destroy ZFS dataset."
  (handler-case (run-cmd (list "jail" "-r" (container-name container)))
    (shell-error () nil))
  (remove-jail-conf container)
  (destroy-dataset container)
  (log-event :container-destroyed :container (container-name container) :type :jail)
  t)

;;; --- Introspection -------------------------------------------------------

(defun jail-running-p (container)
  "Return T if a jail with CONTAINER's name is currently running."
  (handler-case
      (let ((output (run-cmd (list "jls" "-j" (container-name container)
                                   "--libxo" "json"))))
        (declare (ignore output))
        t)
    (shell-error () nil)))

(defun jail-ip (container)
  "Resolve the IP address for a running jail from jls output."
  (handler-case
      (let* ((json-str (run-cmd (list "jls" "-j" (container-name container)
                                      "--libxo" "json")))
             (parsed   (yason:parse json-str))
             (jails    (gethash "jail-information" parsed))
             (jail     (when jails (gethash "jail" jails)))
             (first    (when (and jail (> (length jail) 0))
                         (aref jail 0))))
        (when first
          (gethash "ipv4" first)))
    (error () nil)))

;;; --- Observation (used by reconciler) ------------------------------------

(defun observe-jails ()
  "Return a list of names of currently running jails, parsed from jls JSON.
   Returns NIL on failure (e.g. no jails running)."
  (handler-case
      (let* ((json-str (run-cmd (list "jls" "--libxo" "json")))
             (parsed   (yason:parse json-str))
             (info     (gethash "jail-information" parsed))
             (jails    (when info (gethash "jail" info))))
        (when jails
          (map 'list (lambda (j) (gethash "name" j)) jails)))
    (error () nil)))
