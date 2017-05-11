;;; pluc-settings.el --- Misc elisp settings           -*- lexical-binding: t; -*-
;;
;;
;; Author: Pierre-Luc Perrier <pluc@the-pluc.net>
;;
;;; Commentary:
;;
;;; Code:

(eval-and-compile
  (defconst pluc-cache-dir (expand-file-name ".cache" user-emacs-directory)
    "Directory used to store the different cached files.")
  (defconst pluc-custom-file (expand-file-name "custom.el" pluc-cache-dir)
    "File used to store settings from Customization UI."))

(ignore-errors (make-directory pluc-cache-dir))

(setq custom-file pluc-custom-file)
(load custom-file 'no-error 'no-message)

(provide 'pluc-settings)
;;; pluc-settings.el ends here
