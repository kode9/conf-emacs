;;; early-init.el -*- lexical-binding: t; -*-

;; Raise garbage collector thresholds for initialization to improve startup
;; time. They will be restored after initialization (see below).
;; https://emacs.stackexchange.com/a/34367
(customize-set-variable 'gc-cons-threshold (* 1000 1000 1000))
(customize-set-variable 'gc-cons-percentage 90)

;; Disable package.el
(customize-set-variable 'package-enable-at-startup nil)

;; Lisp
(customize-set-variable 'load-prefer-newer t)            ; Don't load expired byte-compiled files
(customize-set-variable 'ad-redefinition-action 'accept) ; Do not warn about advice redefinitions

;; Debug
(customize-set-variable 'message-log-max 500) ; Keep that many lines in the message buffer

;; Initialization
(customize-set-variable 'inhibit-startup-screen t)             ; Inhibits the startup screen
(customize-set-variable 'initial-buffer-choice nil)            ; Starts with the *scratch* buffer if no file passed
(customize-set-variable 'initial-major-mode 'fundamental-mode) ; Major mode for the *scratch* buffer
(customize-set-variable 'initial-scratch-message nil)          ; No message in the *scratch* buffer

;; Frames
(setq frame-title-format "⸗ %b (%&) ⸗")                ; Title bar
(customize-set-variable 'menu-bar-mode nil)            ; No menu
(customize-set-variable 'tool-bar-mode nil)            ; No toolbar
(customize-set-variable 'scroll-bar-mode nil)          ; No scrollbar
(customize-set-variable 'display-time-mode nil)        ; No time / load / mail in modeline
(customize-set-variable 'size-indication-mode nil)     ; No buffer size in modeline
(customize-set-variable 'line-number-mode nil)         ; Display current line in modeline
(customize-set-variable 'column-number-mode nil)       ; Display current column in modecolumn
(setq initial-frame-alist '((fullscreen . maximized))) ; Start the initial frame maximized
(setq default-frame-alist '((fullscreen . maximized))) ; Start subsequent frames maximized

;; Cursor
(customize-set-variable 'cursor-in-non-selected-windows 'hollow) ; Cursor when window is not selected

;; Display
(customize-set-variable 'ctl-arrow nil)                     ; Display control characters as '\xx'
(customize-set-variable 'cursor-type '(hbar . 4))           ; Cursor when window is selected
(customize-set-variable 'highlight-nonselected-windows t)   ; Keep Highlightning region
(customize-set-variable 'visible-bell nil)                  ; Don't try the flash
(customize-set-variable 'ring-bell-function nil)            ; Don't ring the bell
(customize-set-variable 'truncate-lines nil)                ; Don't truncate long lines (avoid horizontal scrolling)
(customize-set-variable 'truncate-partial-width-windows 40) ; Well still truncate if frame width is small
(customize-set-variable 'word-wrap t)                       ; Wrap long lines

;; Don't check file handlers during startup
;; https://github.com/MatthewZMD/.emacs.d#unset-file-name-handler-alist
(defvar abz--file-name-handler-alist-original file-name-handler-alist)

(defun abz--restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist' to its original value."
  (setq file-name-handler-alist abz--file-name-handler-alist-original)
  (makunbound 'abz--file-name-handler-alist-original))

(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook #'abz--restore-file-name-handler-alist)

;; Disable the use of X resources
;; https://github.com/raxod502/radian/blob/54f9680e81767dc5d036d2c4d6672021ca422784/emacs/early-init.el#L38
(defun abz--advice-disable-x-resource-application ()
  "Disable `x-apply-session-resources'.
I don't use them.")

(advice-add #'x-apply-session-resources :override
            #'abz--advice-disable-x-resource-application)

;;; Package manager straight.el (https://github.com/raxod502/straight.el)

;; Use radox502's mirror of GNU ELPA (https://github.com/emacs-straight)
(customize-set-variable 'straight-recipes-gnu-elpa-use-mirror t)
;; Shallow clone
(customize-set-variable 'straight-vc-git-default-clone-depth 1)
;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
;; https://github.com/raxod502/radian/blob/54f9680e81767dc5d036d2c4d6672021ca422784/emacs/radian.el#L492
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (customize-set-variable 'straight-check-for-modifications '(watch-files find-when-checking))
  (customize-set-variable 'straight-check-for-modifications '(find-at-startup find-when-checking)))

;; Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package: simplify package loading, settings, bindings, and more. https://github.com/jwiegley/use-package
(eval-when-compile (straight-use-package 'use-package))
(customize-set-variable 'straight-use-package-by-default t) ; Install packages by default in `use-package` forms
(customize-set-variable 'use-package-always-defer t)        ; Use deferred loading by default
(customize-set-variable 'use-package-always-demand nil)     ; Inhibit deferred loading by default
(customize-set-variable 'use-package-expand-minimally nil)  ; Make the expanded code as minimal as possible
(customize-set-variable 'use-package-verbose t)             ; Report about loading and configuration details
(customize-set-variable 'use-package-compute-statistics t)  ; Report about loading and configuration details
