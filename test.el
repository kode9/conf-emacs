;;; test.el -*- lexical-binding: t; -*-

(defun succeed (name)
  "Test success."
  (message "# TEST [%s] SUCCESS" name))

(defun fail (name err)
  "Test failure."
  (message "# TEST [%s] FAIL `%s`" name err)
  (let (kill-emacs-hook)
    (kill-emacs 1)))

(let ((debug-on-error nil)
      (debug-on-signal nil)
      (force-load-messages nil))
  (let ((test "load"))
    (condition-case err
        (progn
          (load (expand-file-name "early-init.el" user-emacs-directory) nil 'nomessage 'nosuffix 'must-suffix)
          (load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage 'nosuffix 'must-suffix)
          (succeed test))
      (error (fail test err))))
  (let ((test "quit"))
    (add-hook #'kill-emacs-hook (lambda () (succeed test)) 'append)
    (condition-case err
        (kill-emacs 0)
      (error (fail test err)))))

;;; test.el ends here
