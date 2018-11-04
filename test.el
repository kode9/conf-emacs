;;; foo.el -*- lexical-binding: t; -*-

(defun fail ()
  "Test failure."
  (message "# Test: FAILURE")
  (let (kill-emacs-hook)
    (kill-emacs 1)))

(let ((debug-on-error t)
      (force-load-messages nil))
  (condition-case err
      (load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage 'nosuffix 'must-suffix)
    ((debug error)
     (message "Error in init: %s" (error-message-string err))
     (fail)))
  (add-hook #'kill-emacs-hook (lambda () (message "# Test: SUCCESS")) 'append)
  (condition-case err
      (kill-emacs 0)
    ((debug error)
     (message "Error at exit: %s" (error-message-string err))
     (fail))))

;;; foo.el ends here
