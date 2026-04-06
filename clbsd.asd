(defsystem "clbsd"
  :description "A Common Lisp control plane for declarative FreeBSD homelab infrastructure"
  :version "0.1.0"
  :license "BSD-2-Clause"
  :depends-on ("bordeaux-threads" "cffi" "yason" "alexandria")
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "config")
               (:file "shell")
               (:file "event-log")
               (:file "container")
               (:file "zfs")
               (:file "jail")
               (:file "docker")
               (:file "api")
               (:file "ingress")
               (:file "reconciler")
               (:file "kqueue-ffi")
               (:file "kqueue")
               (:file "handlers")
               (:file "swank")
               (:file "init")))
