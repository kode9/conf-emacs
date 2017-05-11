;;; init.el --- Pluc's GNU Emacs configuration       -*- lexical-binding: t; -*-
;;
;; Author: Pierre-Luc Perrier <pluc@the-pluc.net>
;;
;;; Commentary:
;;
;;; Code:

(customize-set-variable 'load-prefer-newer t) ; Don't load expired byte-compiled files

;; Required by package.el
(when (boundp 'package-initialize) (package-initialize 'no-activate))

;; Cask: Automatic installation and updates of packages listed in a
;; Cask file. http://github.com/cask/cask
(eval-and-compile (require 'cask (expand-file-name "cask/cask.el" user-emacs-directory)))
(cask-initialize)

;; use-package: simplify package loading, settings, bindings, and
;; more. https://github.com/jwiegley/use-package
(eval-when-compile
  (customize-set-variable 'use-package-verbose nil)        ; Report about loading and configuration details.
  (customize-set-variable 'use-package-debug nil)          ; Display expanded code
  (customize-set-variable 'use-package-expand-minimally t) ; Make the expanded code as minimal as possible
  (require 'use-package))

(use-package diminish :demand t :ensure t) ; :diminish support for use-package
(use-package bind-key :demand t :ensure t) ; :bind support for use-package

;; Pallet: Keep track of package installations in concordance with
;; Cask. https://github.com/rdallasgray/pallet
(use-package pallet
  :config (pallet-mode t))

;; Some non-packaged stuff
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submodules
;; Comment out the ones you do not want
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory where to find submodules
(eval-and-compile (defconst pluc-dir (expand-file-name "pluc/" user-emacs-directory) "Local packages directory"))

(use-package pluc-theme :load-path pluc-dir)   ; Color theme (only zenburn ATM)
(use-package pluc-custom :load-path pluc-dir)  ; Basic setup
(use-package pluc-ido :load-path pluc-dir)     ; InteractivelyDoThings
(use-package pluc-editing :load-path pluc-dir) ; Common edition settings
(use-package pluc-devel :load-path pluc-dir)   ; Development settings
(use-package pluc-tools :load-path pluc-dir)   ; External tools integration

;; Debug init file
(use-package bug-hunter
  :defer t)

;; Profile emacs startup
(use-package esup
  :defer t)

(bind-key* "C-c S" 'align-comments)
(bind-key* "C-x C-r" 'toggle-sudo)

;; Autoscoll compilation buffer and stop on first error
(set 'compilation-scroll-output 'first-error)
;; Skip warnings when jumping between errors
(set 'compilation-skip-threshold 2)

;; Shorten long file-name targets. https://github.com/lewang/scf-mode
(autoload 'scf-mode "scf-mode" "SCF Mode" t)
(add-hook 'compilation-mode-hook (lambda () (scf-mode t)))

;; Stop asking yes/no before compile when a compilation is already
;; running. ftp://download.tuxfamily.org/user42/compilation-always-kill.el
(autoload 'compilation-always-kill-mode "compilation-always-kill" "Compilation kill" t)
(compilation-always-kill-mode t)

(diminish 'compilation-in-progress)
(diminish 'auto-revert-mode)

;; Custom hooks
(defun dtw()
  "Delete trailing whitespaces"
  (interactive)
  (delete-trailing-whitespace))

(defun indent-buffer()
  "Indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun untab-buffer()
  "Transform tabs to spaces in the whole buffer"
  (interactive)
  (untabify (point-min) (point-max) nil))

(defun dev-hooks()
  "Progmod hooks"
  (format-buffer))

(define-minor-mode pluc-mode
  "Clean buffers."
  :lighter " pluc"
  :global t
  (if pluc-mode
      (progn
        (add-hook 'before-save-hook 'dev-hooks nil t))
    (remove-hook 'before-save-hook 'dev-hooks t)))

;; Clean for any files
(add-hook 'before-save-hook 'dtw)
(add-hook 'prog-mode-hook #'pluc-mode)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; keys
(global-set-key [(control c) (c)] 'comment-or-uncomment-region)
;; (global-set-key [(control c) (v)] 'uncomment-region)
(global-set-key [(control c) (x)] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control x) (control k)] 'kill-some-buffers)

;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (diff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

(provide 'init)
;;; init.el ends here
