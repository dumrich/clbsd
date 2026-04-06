(defpackage #:clbsd
  (:use #:cl)
  (:export
   ;; config
   #:*zpool-root*
   #:*docker-host*
   #:*routing-jail-name*
   #:*routing-jail-dataset*
   #:*swank-port*
   #:*init-file-path*
   #:*clbsd-state*
   #:*routing-jail*

   ;; shell
   #:run-cmd
   #:shell-error
   #:shell-error-command
   #:shell-error-exit-code
   #:shell-error-stderr

   ;; event log
   #:log-event
   #:query-log
   #:clear-log
   #:*event-log*

   ;; container struct
   #:container
   #:make-container
   #:copy-container
   #:container-name
   #:container-type
   #:container-dataset
   #:container-image
   #:container-domain
   #:container-port
   #:container-state
   #:container-dirty-p
   #:container-callbacks
   #:container-ip-addr
   #:find-container

   ;; zfs
   #:create-dataset
   #:destroy-dataset
   #:snapshot
   #:rollback
   #:export-dataset

   ;; jail backend
   #:create-jail
   #:start-jail
   #:stop-jail
   #:destroy-jail
   #:jail-running-p
   #:observe-jails
   #:generate-jail-conf-fragment
   #:jail-ip

   ;; docker backend
   #:docker-cmd
   #:create-docker
   #:start-docker
   #:stop-docker
   #:destroy-docker
   #:docker-running-p
   #:observe-docker-containers
   #:boot-docker-vm
   #:shutdown-docker-vm

   ;; unified api
   #:create
   #:start
   #:stop
   #:restart
   #:destroy
   #:running-p
   #:status
   #:connect-repl

   ;; ingress
   #:generate-nginx-conf
   #:container->server-block
   #:container-ip
   #:write-nginx-conf
   #:reload-nginx

   ;; reconciler
   #:signal-reconciler
   #:reconciler-loop
   #:reconcile-once
   #:observe-running-containers
   #:*reconciler-lock*
   #:*reconciler-condvar*
   #:*reconciler-thread*

   ;; kqueue ffi
   #:make-kqueue
   #:register-kevent
   #:poll-kevents
   #:close-kqueue

   ;; kqueue
   #:init-kqueue
   #:shutdown-kqueue
   #:watch-jail-process
   #:watch-dataset-changes
   #:register-health-timer
   #:kqueue-loop
   #:*kqueue-fd*
   #:*kqueue-thread*

   ;; handlers
   #:auto-restart-handler
   #:log-and-alert-handler

   ;; swank
   #:start-clbsd-swank
   #:stop-clbsd-swank

   ;; init
   #:boot-clbsd
   #:shutdown-clbsd
   #:load-init-file))
