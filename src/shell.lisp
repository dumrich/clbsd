(in-package #:clbsd)

;;; --------------------------------------------------------------------------
;;; Shell command execution — thin wrapper over uiop:run-program that
;;; captures stdout/stderr, signals a condition on failure, and logs every
;;; invocation to the event log.
;;; --------------------------------------------------------------------------

(define-condition shell-error (error)
  ((command   :initarg :command   :reader shell-error-command)
   (exit-code :initarg :exit-code :reader shell-error-exit-code)
   (stderr    :initarg :stderr    :reader shell-error-stderr))
  (:report (lambda (c s)
             (format s "Shell command failed (exit ~D): ~A~%stderr: ~A"
                     (shell-error-exit-code c)
                     (shell-error-command c)
                     (shell-error-stderr c)))))

(defun run-cmd (command &key environment)
  "Execute COMMAND (a list of strings) via uiop:run-program.
   Returns stdout as a string.  Signals SHELL-ERROR on non-zero exit.
   Every invocation is recorded in the event log."
  (log-event :shell-cmd :command command)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program command
                         :output :string
                         :error-output :string
                         :ignore-error-status t
                         :environment environment)
    (unless (zerop exit-code)
      (log-event :shell-error
                 :command command
                 :exit-code exit-code
                 :stderr stderr)
      (error 'shell-error
             :command command
             :exit-code exit-code
             :stderr stderr))
    (log-event :shell-ok :command command :exit-code exit-code)
    stdout))
