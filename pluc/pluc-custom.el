;;; pluc-custom.el --- Basic configuration           -*- lexical-binding: t; -*-
;;
;;
;; Author: Pierre-Luc Perrier <pluc@the-pluc.net>
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(progn
  (setq frame-title-format "⸗ %b (%&) ⸗")

  (defalias 'yes-or-no-p 'y-or-n-p) ; Just use 'y'/'n' even for yes-or-no-p

  ;; Initialization
  (customize-set-variable 'inhibit-startup-screen t)             ; Inhibits the startup screen
  (customize-set-variable 'initial-buffer-choice t)              ; Starts with the *scratch* buffer
  (customize-set-variable 'initial-major-mode 'fundamental-mode) ; Major mode for the *scratch* buffer
  (customize-set-variable 'initial-scratch-message nil)          ; No message in the *scratch* buffer

  ;; Display
  (customize-set-variable 'ctl-arrow nil)                          ; Display control characters as '\xx'
  (customize-set-variable 'cursor-type '(hbar . 4))                ; Cursor when window is selected
  (customize-set-variable 'cursor-in-non-selected-windows 'hollow) ; Cursor when window is not selected
  (customize-set-variable 'highlight-nonselected-windows t)        ; Keep Highlightning region
  (customize-set-variable 'visible-bell nil)                       ; Don't try the flash
  (customize-set-variable 'ring-bell-function nil)                 ; Don't ring the bell
  (customize-set-variable 'truncate-lines nil)                     ; Don't truncate long lines (avoid horizontal scrolling)
  (customize-set-variable 'truncate-partial-width-windows 40)      ; Well still truncate if frame width is small
  (customize-set-variable 'word-wrap t)                            ; Wrap long lines

  ;; Uniquify buffer names
  (customize-set-variable 'uniquify-after-kill-buffer-p t)           ; Update buffer names when one is killed
  (customize-set-variable 'uniquify-buffer-name-style 'post-forward) ; 'name|foo/bar'
  (customize-set-variable 'uniquify-separator "/")                   ; pose-forward becomes 'name/foo/bar'
  (customize-set-variable 'uniquify-strip-common-suffix t)           ; Strip common directories

  ;; Debug
  (customize-set-variable 'message-log-max 500) ; Keep that many lines in the message buffer

  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))                            ; No menu
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))                            ; No toolbar
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))                        ; No scrollbar
  (when (fboundp 'display-time-mode) (display-time-mode -1))                    ; No time / load / mail in modeline
  (when (fboundp 'size-indication-mode) (size-indication-mode -1))              ; No buffer size in modeline
  (when (fboundp 'line-number-mode) (line-number-mode nil))                     ; Display current line in modeline
  (when (fboundp 'column-number-mode) (column-number-mode nil))                 ; Display current column in modecolumn
  (when (fboundp 'display-battery-mode) (display-battery-mode -1))              ; No battery status in modeline
  (when (fboundp 'prefer-coding-system) (prefer-coding-system 'utf-8))          ; Give priority to UTF-8
  (when (fboundp 'set-language-environment) (set-language-environment "UTF-8")) ; Default input method

  ;; Minibuffer history
  (customize-set-variable 'savehist-file (locate-user-emacs-file ".cache/history")) ; Minibuffer history location
  (savehist-mode nil)                                                               ; Enable minibuffer history

  ;; Auto-saving
  (customize-set-variable 'auto-save-default t)           ; Enable auto-save
  (customize-set-variable 'auto-save-file-name-transforms ; File names
                          `((".*" ,(locate-user-emacs-file ".cache/auto-save/") t)))
  (customize-set-variable 'auto-save-list-file-prefix     ; Auto-save list
                          (locate-user-emacs-file ".cache/auto-save/list-"))
  (customize-set-variable 'auto-save-timeout 31)
  (customize-set-variable 'auto-save-visited-file-name nil)
  (customize-set-variable 'delete-auto-save-files t)      ; Delete when buffer is saved or killed without modifications

  ;; Automatic backup on first save
  (customize-set-variable 'backup-by-copying t)      ; Always copy (no rename)
  (customize-set-variable 'backup-directory-alist `(("." . ,(locate-user-emacs-file ".cache/backup/"))))
  (customize-set-variable 'delete-old-versions t)
  (customize-set-variable 'kept-old-versions 5)      ; Number of oldest backups
  (customize-set-variable 'kept-new-versions 5)      ; Number of newest backups
  (customize-set-variable 'make-backup-files t)      ; Enabel backup on first save
  (customize-set-variable 'vc-make-backup-files nil) ; Let VCS' do their job
  (customize-set-variable 'version-control t)        ; Use numbered backups

  (customize-set-variable 'show-paren-style 'expression)  ; Show full expression
  (customize-set-variable 'show-paren-delay 0.01)         ; Delay before showing
  (when (fboundp 'show-paren-mode) (show-paren-mode nil)) ; Highlights parenthesis

  ;; VCS
  (customize-set-variable 'vc-follow-symlinks t) ; Always follow symlinks to files under VC

  ;; windmove: Navigate between windows using directions
  (customize-set-variable 'windmove-wrap-around t) ; Cycle
  (when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings 'super)) ; Use super + arrows
  )

;; Customization
;;; Took from https://github.com/lunaryorn/.emacs.d
(defconst pluc-custom-file (locate-user-emacs-file "custom.el") "File used to store settings from Customization UI.")
(use-package cus-edit :defer t :config (setq custom-file pluc-custom-file) :init (load pluc-custom-file 'no-error 'no-message))

;; Remote access
(use-package tramp
  :defer t
  :config
  (setq tramp-auto-save-directory (locate-user-emacs-file ".tramp")) ;; Put auto-save files in this directory
  )

;; Gets the mouse out of the cursor
(use-package avoid
  :init
  (setq mouse-avoidance-threshold 10) ; When is it too close
  :config
  (mouse-avoidance-mode 'exile)       ; Exile it to top-right corder, but allow it to come back
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow undo/redo windows configuration ;;
;; Keys: "C-c LEFT" and "C-c RIGHT"  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'winner-mode) (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server (emacsclient) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package server :config (unless (server-running-p) (server-start)))

;; Automatic resizing of windows using the golden ration and keeping
;; the one with the focus
;; bigger. https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :disabled t
  :diminish golden-ratio-mode
  :init
  (setq golden-ratio-auto-scale nil) ; If not nil, keep frames narrow on wide screens
  (setq split-width-threshold nil)   ; Prevent additional windows creation
  :config
  (golden-ratio-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paradox: better package menu        ;;
;; http://github.com/Malabarba/paradox ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paradox
  :defer t
  :init
  (setq paradox-github-token t)             ; Don't ask for GitHub token
  (setq paradox-execute-asynchronously nil) ; Don't try to do things asynchronously
  (setq paradox-automatically-star nil)     ; Do not star automatically when (un)installing
  )

;; Ibuffer
(use-package ibuffer
  :init
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("grep"  (mode . ag-mode)) ; ag (silver searcher) buffers
                 ("devel" (or
                           (mode . c++-mode)
                           (mode . c-mode)
                           (mode . python-mode))))))
        ibuffer-filter-group-name-face 'font-lock-string-face)
  :bind (("C-x C-b" . ibuffer)))

(provide 'pluc-custom)
;;; pluc-custom.el ends here
