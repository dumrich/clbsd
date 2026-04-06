(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; ZFS dataset operations — thin wrappers over the zfs(8) CLI.
;;;
;;; CLBSD manages datasets beneath *zpool-root* (e.g. "tank/clbsd").
;;; The pool itself must exist before CLBSD starts.
;;;
;;; Layout:
;;;   <zpool>/clbsd/
;;;     templates/freebsd-14@base      snapshot used for jail clones
;;;     <name>/                         rootfs (jail) or scratch (docker)
;;;     <name>/data/                    persistent data
;;;     routing/                        routing jail rootfs
;;;     vm/alpine.img                   Bhyve VM disk image
;;; --------------------------------------------------------------------------

(defun dataset-path (container)
  "Return the full ZFS dataset path for CONTAINER, falling back to
   *zpool-root*/<name> if the container's dataset slot is nil."
  (or (container-dataset container)
      (format nil "~A/~A" *zpool-root* (container-name container))))

(defun create-dataset (container &key (template "freebsd-14@base"))
  "Clone the ZFS template snapshot into a new dataset for CONTAINER.
   Also creates a child /data dataset for persistent storage."
  (let* ((ds   (dataset-path container))
         (snap (format nil "~A/templates/~A" *zpool-root* template)))
    (run-cmd (list "zfs" "clone" snap ds))
    (run-cmd (list "zfs" "create" (format nil "~A/data" ds)))
    (log-event :dataset-created :container (container-name container) :dataset ds)
    ds))

(defun destroy-dataset (container &key (recursive t))
  "Destroy the ZFS dataset for CONTAINER.  Recursive by default so child
   datasets (/data, snapshots) are also removed."
  (let ((ds (dataset-path container)))
    (run-cmd (list* "zfs" "destroy"
                    (append (when recursive '("-r")) (list ds))))
    (log-event :dataset-destroyed :container (container-name container) :dataset ds)
    t))

(defun snapshot (container &optional label)
  "Create a ZFS snapshot of CONTAINER's dataset.
   LABEL defaults to <name>-<unix-timestamp>."
  (let* ((ds   (dataset-path container))
         (tag  (or label
                   (format nil "~A-~D"
                           (container-name container)
                           (get-universal-time))))
         (snap (format nil "~A@~A" ds tag)))
    (run-cmd (list "zfs" "snapshot" snap))
    (log-event :snapshot-created :container (container-name container) :snapshot snap)
    snap))

(defun rollback (container label)
  "Roll back CONTAINER's dataset to the snapshot identified by LABEL."
  (let ((snap (format nil "~A@~A" (dataset-path container) label)))
    (run-cmd (list "zfs" "rollback" "-r" snap))
    (log-event :snapshot-rollback :container (container-name container) :snapshot snap)
    t))

(defun export-dataset (container dest-host &key dest-dataset)
  "Send CONTAINER's dataset to DEST-HOST via zfs send | ssh zfs receive.
   DEST-DATASET defaults to the same dataset path on the remote."
  (let* ((ds      (dataset-path container))
         (target  (or dest-dataset ds))
         (cmd     (format nil "zfs send -R ~A | ssh ~A zfs receive ~A"
                          ds dest-host target)))
    (run-cmd (list "sh" "-c" cmd))
    (log-event :dataset-exported
               :container (container-name container)
               :dest-host dest-host
               :dest-dataset target)
    t))
