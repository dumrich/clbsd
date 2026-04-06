(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Dynamic ingress — nginx.conf generation and reload.
;;;
;;; A dedicated routing jail contains Nginx.  CLBSD owns its config file;
;;; the operator must not edit nginx.conf by hand.
;;;
;;; container-ip resolves differently per backend:
;;;   :jail   → reads ip4.addr from jls
;;;   :docker → queries docker inspect inside the Bhyve VM
;;;
;;; TLS is out of scope for v0.  The operator places certs in the routing
;;; jail's dataset and extends the Nginx template.
;;; --------------------------------------------------------------------------

(defvar *nginx-conf-template*
  "worker_processes auto;
events {
  worker_connections 1024;
}
http {
  ~A
}~%"
  "Top-level nginx.conf template.  ~A is replaced with server blocks.")

(defun container-ip (container)
  "Resolve the IP address of a running container.
   For jails, reads from jls.  For Docker, queries docker inspect.
   Caches the result in the container's ip-addr slot."
  (or (container-ip-addr container)
      (let ((ip (ecase (container-type container)
                  (:jail   (jail-ip container))
                  (:docker (docker-container-ip container)))))
        (when ip
          (setf (%container-ip-addr container) ip))
        ip)))

(defun container->server-block (container)
  "Generate an Nginx server {} block string for CONTAINER.
   Returns NIL if the container has no domain or its IP cannot be resolved."
  (let ((domain (container-domain container))
        (port   (container-port container))
        (ip     (container-ip container)))
    (when (and domain port ip)
      (format nil "  server {
    listen 80;
    server_name ~A;

    location / {
      proxy_pass http://~A:~D;
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
    }
  }" domain ip port))))

(defun generate-nginx-conf (containers)
  "Pure function.  Return a complete nginx.conf string for all containers
   that have a non-nil :domain.  Containers whose IP cannot be resolved
   are silently skipped."
  (let* ((routable (remove-if-not #'container-domain containers))
         (blocks   (remove nil (mapcar #'container->server-block routable)))
         (body     (format nil "~{~A~^~%~%~}" blocks)))
    (format nil *nginx-conf-template* body)))

(defun nginx-conf-path ()
  "Absolute path to the nginx.conf inside the routing jail's dataset."
  (let ((ds (or *routing-jail-dataset*
                (format nil "/~A/~A" *zpool-root* *routing-jail-name*))))
    (format nil "~A/usr/local/etc/nginx/nginx.conf" ds)))

(defun write-nginx-conf (containers)
  "Generate nginx.conf from CONTAINERS and write it to the routing jail's
   dataset.  Returns the path written."
  (let ((conf (generate-nginx-conf containers))
        (path (nginx-conf-path)))
    (ensure-directories-exist path)
    (alexandria:write-string-into-file conf path
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
    (log-event :nginx-conf-written :path path
               :containers (mapcar #'container-name
                                   (remove-if-not #'container-domain containers)))
    path))

(defun reload-nginx (&optional (routing *routing-jail*))
  "Trigger a graceful Nginx reload inside the routing jail.
   ROUTING defaults to *routing-jail*."
  (when routing
    (run-cmd (list "jexec" (container-name routing) "service" "nginx" "reload"))
    (log-event :nginx-reload :jail (container-name routing))
    t))
