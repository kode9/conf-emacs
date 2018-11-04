;;; pluc-custom.el --- Basic configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 Pierre-Luc Perrier

;; Author: Pierre-Luc Perrier <dev@the-pluc.net>

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;

;;; Code:

(require 'pluc-settings)
(require 'cl-lib)

;;;###autoload
(defun abz--init-custom ()
  "Initialise custom configuration variables."
  (setq frame-title-format "⸗ %b (%&) ⸗")

  (defalias 'yes-or-no-p 'y-or-n-p) ; Just use 'y'/'n' even for yes-or-no-p

  ;; Initialization
  (customize-set-variable 'inhibit-startup-screen t)             ; Inhibits the startup screen
  (customize-set-variable 'initial-buffer-choice nil)            ; Starts with the *scratch* buffer if no file passed
  (customize-set-variable 'initial-major-mode 'fundamental-mode) ; Major mode for the *scratch* buffer
  (customize-set-variable 'initial-scratch-message nil)          ; No message in the *scratch* buffer
  ;; Start the initial frame full-screen
  (add-hook #'emacs-startup-hook (lambda () (cl-pushnew (cons 'fullscreen 'fullboth) initial-frame-alist)))
  ;; Start subsequent frames maximized
  (add-hook #'emacs-startup-hook (lambda () (cl-pushnew (cons 'fullscreen 'maximized) default-frame-alist)))

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
  (customize-set-variable 'mouse-wheel-progressive-speed nil)      ; Constant mouse wheel speed

  ;; Uniquify buffer names
  (customize-set-variable 'uniquify-after-kill-buffer-p t)           ; Update buffer names when one is killed
  (customize-set-variable 'uniquify-buffer-name-style 'post-forward) ; 'name|foo/bar'
  (customize-set-variable 'uniquify-separator "/")                   ; pose-forward becomes 'name/foo/bar'
  (customize-set-variable 'uniquify-strip-common-suffix t)           ; Strip common directories

  ;; Debug
  (customize-set-variable 'message-log-max 500) ; Keep that many lines in the message buffer
  (customize-set-variable 'ad-redefinition-action 'accept) ; Do not warn about advice redefinitions

  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))                            ; No menu
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))                            ; No toolbar
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))                        ; No scrollbar
  (when (fboundp 'tooltip-mode) (tooltip-mode -1))                              ; Tooltips in minibuffer
  (when (fboundp 'display-time-mode) (display-time-mode -1))                    ; No time / load / mail in modeline
  (when (fboundp 'size-indication-mode) (size-indication-mode -1))              ; No buffer size in modeline
  (when (fboundp 'line-number-mode) (line-number-mode nil))                     ; Display current line in modeline
  (when (fboundp 'column-number-mode) (column-number-mode nil))                 ; Display current column in modecolumn
  (when (fboundp 'prefer-coding-system) (prefer-coding-system 'utf-8))          ; Give priority to UTF-8
  (when (fboundp 'set-language-environment) (set-language-environment "UTF-8")) ; Default input method

  ;; History file
  (customize-set-variable 'savehist-file
                          (expand-file-name "history" pluc-cache-dir)) ; Minibuffer history location

  ;; Minibuffer history
  (customize-set-variable 'history-length 100) ; Maximum history
  (customize-set-variable 'history-delete-duplicates t) ; Remove duplicates
  (savehist-mode nil) ; Enable minibuffer history

  ;; Buffers
  (customize-set-variable 'confirm-nonexistent-file-or-buffer
                          'after-completion) ; Ask confirmation for new file/buffer only after completion

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
  (customize-set-variable 'backup-by-copying t) ; Always copy (no rename)
  (customize-set-variable 'backup-directory-alist `(("." . ,(locate-user-emacs-file ".cache/backup/"))))
  (customize-set-variable 'delete-old-versions t)
  (customize-set-variable 'kept-old-versions 0)    ; Number of oldest backups
  (customize-set-variable 'kept-new-versions 10)   ; Number of newest backups
  (customize-set-variable 'make-backup-files t)    ; Enable backup on first save
  (customize-set-variable 'vc-make-backup-files t) ; Also backup files under VCS
  (customize-set-variable 'version-control t)      ; Use numbered backups

  (customize-set-variable 'show-paren-style 'expression)  ; Show full expression
  (customize-set-variable 'show-paren-delay 0.01)         ; Delay before showing
  (when (fboundp 'show-paren-mode) (show-paren-mode nil)) ; Highlights parenthesis

  ;; VCS
  (customize-set-variable 'vc-follow-symlinks t) ; Always follow symlinks to files under VC

  ;; windmove: Navigate between windows using directions
  (customize-set-variable 'windmove-wrap-around t) ; Cycle
  (when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings 'super)) ; Use super + arrows

  ;; Set default browser
  (customize-set-variable 'browse-url-generic-program "xdg-open") ; The browser used by browse-url-generic
  (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
  ;; Always select the help window
  (customize-set-variable 'help-window-select t))

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory
        (expand-file-name "local/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name ".cache/" user-emacs-directory)))

;; List of recently visited files (built-in)
(use-package recentf
  :demand t
  :straight nil
  :init
  (customize-set-variable 'recentf-max-saved-items 40)
  :config
  (add-to-list 'recentf-exclude pluc-local-dir)
  (add-to-list 'recentf-exclude pluc-cache-dir))

;; Autorevert
(use-package autorevert
  :straight nil
  :diminish auto-revert-mode
  :init
  ;; Also auto-revert buffer-menu and dired buffers
  (customize-set-variable 'global-auto-revert-non-file-buffers t)
  ;; Be quiet
  (customize-set-variable 'auto-revert-verbose nil)
  :hook (after-init . global-auto-revert-mode))

;; Gets the mouse out of the cursor
(use-package avoid
  :init
  (setq mouse-avoidance-threshold 10) ; When is it too close
  :config
  (mouse-avoidance-mode 'jump))       ; Exile it to top-right corder, but allow it to come back

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow undo/redo windows configuration ;;
;; Keys: "C-c LEFT" and "C-c RIGHT"  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'winner-mode) (winner-mode 1))

;; Emacs server (emacsclient)
(use-package server
  :if (display-graphic-p nil)
  :init
  (defun pluc-server-start () (unless (server-running-p) (server-start)))
  (add-hook 'after-init-hook 'pluc-server-start))

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

;; Dired
(use-package dired
  :straight nil
  :init
  ;; Try to guess a default target directory
  (customize-set-variable 'dired-dwim-target t)
  :bind (:map dired-mode-map
              ;; Search only in filenames
              ("C-s" . dired-isearch-filenames)))

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
