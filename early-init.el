;;; early-init.el -*- lexical-binding: t; -*-

;; Raise garbage collector thresholds for initialization to improve startup
;; time. They will be restored after initialization (see below).
;; https://emacs.stackexchange.com/a/34367
(customize-set-variable 'gc-cons-threshold (* 1000 1000 1000))
(customize-set-variable 'gc-cons-percentage 90)

;; Don't load expired byte-compiled files
(customize-set-variable 'load-prefer-newer t)

(customize-set-variable 'menu-bar-mode nil)        ; No menu
(customize-set-variable 'tool-bar-mode nil)        ; No toolbar
(customize-set-variable 'scroll-bar-mode nil)      ; No scrollbar
(customize-set-variable 'display-time-mode nil)    ; No time / load / mail in modeline
(customize-set-variable 'size-indication-mode nil) ; No buffer size in modeline
(customize-set-variable 'line-number-mode nil)     ; Display current line in modeline
(customize-set-variable 'column-number-mode nil)   ; Display current column in modecolumn

;; Don't check file handlers during startup
;; https://github.com/MatthewZMD/.emacs.d#unset-file-name-handler-alist
(defvar abz--file-name-handler-alist-original file-name-handler-alist)
(defun abz--restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist' to its original value."
  (setq file-name-handler-alist abz--file-name-handler-alist-original)
  (makunbound 'abz--file-name-handler-alist-original))
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook #'abz--restore-file-name-handler-alist)
