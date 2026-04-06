(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Global configuration — set by the operator before calling boot-clbsd,
;;; or overridden in clbsd-init.lisp.
;;; --------------------------------------------------------------------------

(defvar *zpool-root* "tank/clbsd"
  "Root ZFS dataset under which all CLBSD datasets are created.
   The pool must already exist on the host.")

(defvar *docker-host* "tcp://10.0.0.2:2375"
  "Address of the Docker daemon inside the Alpine Bhyve VM.
   Set during CLBSD init from the operator's config.")

(defvar *routing-jail-name* "routing"
  "Name of the dedicated Nginx routing jail.")

(defvar *routing-jail-dataset* nil
  "ZFS dataset path for the routing jail.  Computed at boot from *zpool-root*
   if left NIL.")

(defvar *swank-port* 4005
  "TCP port on which the Swank server listens for SLIME connections.")

(defvar *init-file-path* "clbsd-init.lisp"
  "Path to the operator's declarative init file, loaded at boot.")

(defvar *clbsd-state* nil
  "The desired state: a list of container structs.  Mutated by the operator
   (init file, SLIME REPL) and read by the reconciler.")

(defvar *routing-jail* nil
  "Container struct for the dedicated Nginx routing jail.
   Managed internally — not part of *clbsd-state*.")

(defvar *shutdown-requested* nil
  "Set to T by shutdown-clbsd to signal background threads to exit.")
