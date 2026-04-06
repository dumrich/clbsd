(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Event log — thread-safe, append-only, in-memory ring buffer.
;;; Every significant action in the control plane is recorded here.
;;; Layer 3 (Emacs) reads this log to populate the org-mode dashboard.
;;; --------------------------------------------------------------------------

(defvar *log-lock* (bt:make-lock "event-log-lock")
  "Dedicated lock for the event log.  Separate from the reconciler lock
   to avoid contention on high-frequency log writes.")

(defvar *event-log* (make-array 4096 :fill-pointer 0 :adjustable t)
  "Append-only vector of event plists.  Each entry is
   (:time <universal-time> :type <keyword> ...props).")

(defun log-event (type &rest plist)
  "Append a timestamped event to *event-log*.  Thread-safe.
   Returns the new entry.

   TYPE is a keyword such as :container-started, :reconciler-error, etc.
   PLIST contains arbitrary keyword-value pairs for the event payload."
  (let ((entry (list* :time (get-universal-time) :type type plist)))
    (bt:with-lock-held (*log-lock*)
      (vector-push-extend entry *event-log*))
    entry))

(defun query-log (&key type since limit)
  "Return a list of log entries matching the given filters.
   TYPE  — if non-nil, only entries whose :type matches.
   SINCE — if non-nil, only entries with :time >= SINCE.
   LIMIT — if non-nil, return at most this many (most recent first)."
  (bt:with-lock-held (*log-lock*)
    (let ((results '()))
      (loop for i from (1- (fill-pointer *event-log*)) downto 0
            for entry = (aref *event-log* i)
            when (and (or (null type)
                          (eq type (getf entry :type)))
                      (or (null since)
                          (>= (getf entry :time) since)))
              do (push entry results)
            when (and limit (>= (length results) limit))
              do (return))
      (nreverse results))))

(defun clear-log ()
  "Reset the event log.  Primarily for testing."
  (bt:with-lock-held (*log-lock*)
    (setf (fill-pointer *event-log*) 0))
  t)
