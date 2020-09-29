;;; early-init.el -*- lexical-binding: t; -*-

;; Raise garbage collector thresholds for initialization to improve startup
;; time. They will be restored after initialization (see below).
;; https://emacs.stackexchange.com/a/34367
(customize-set-variable 'gc-cons-threshold (* 1000 1000 1000))
(customize-set-variable 'gc-cons-percentage 90)

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
