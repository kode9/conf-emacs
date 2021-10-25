;;; abz-custom.el --- Basic configuration           -*- lexical-binding: t; -*-

;; Copyright (C) Pierre-Luc Perrier

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

(require 'abz-settings)
(require 'cl-lib)
(require 'use-package)

;;;###autoload
(defun abz--init-custom ()
  "Initialise custom configuration variables."
  (defalias 'yes-or-no-p 'y-or-n-p) ; Just use 'y'/'n' even for yes-or-no-p

  ;; Uniquify buffer names
  (customize-set-variable 'uniquify-after-kill-buffer-p t)           ; Update buffer names when one is killed
  (customize-set-variable 'uniquify-buffer-name-style 'post-forward) ; 'name|foo/bar'
  (customize-set-variable 'uniquify-separator "/")                   ; pose-forward becomes 'name/foo/bar'
  (customize-set-variable 'uniquify-strip-common-suffix t)           ; Strip common directories

  (when (fboundp 'prefer-coding-system) (prefer-coding-system 'utf-8))          ; Give priority to UTF-8
  (when (fboundp 'set-language-environment) (set-language-environment "UTF-8")) ; Default input method

  ;; Scrolling
  (customize-set-variable 'mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control . nil))))
  (customize-set-variable 'mouse-wheel-progressive-speed nil)
  (customize-set-variable 'scroll-step 1)
  (customize-set-variable 'scroll-margin 2)
  (customize-set-variable 'scroll-conservatively 4)
  (customize-set-variable 'scroll-preserve-screen-position t)

  ;; History file
  (customize-set-variable 'savehist-file
                          (expand-file-name "history" abz-cache-dir)) ; Minibuffer history location

  ;; Minibuffer
  (customize-set-variable 'enable-recursive-minibuffers t)   ; Allow minibuffer commands while in the minibuffer
  (customize-set-variable 'minibuffer-depth-indicate-mode t) ; Show recursion depth in the minibuffer
  (customize-set-variable 'history-length 100)               ; Maximum history
  (customize-set-variable 'history-delete-duplicates t)      ; Remove duplicates
  (savehist-mode nil)                                        ; Enable minibuffer history

  ;; Buffers
  (customize-set-variable 'confirm-nonexistent-file-or-buffer
                          'after-completion) ; Ask confirmation for new file/buffer only after completion

  (customize-set-variable 'delete-by-moving-to-trash t)

  ;; Automatic backup on first save
  (customize-set-variable 'backup-by-copying t) ; Always copy (no rename)
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

  ;; Always select the help window
  (customize-set-variable 'help-window-select t))

;; Window nagivation (built-in)
(use-package windmove
  :straight nil
  :demand t
  :custom
  (windmove-wrap-around t "Whether movement off the edge of the frame wraps around")
  :config
  (windmove-default-keybindings (list 'super)))

;; Tooltips (built-in)
(use-package tooltip
  :straight nil
  :demand t
  :init
  (customize-set-variable 'tooltip-delay 0.4)            ; Secondes before showing tooltips
  (customize-set-variable 'tooltip-hide-delay 59)        ; Secondes before showing tooltips (0 disables tooltips)
  (customize-set-variable 'tooltip-reuse-hidden-frame t) ; Don't recreate tooltip frames all the time
  (customize-set-variable 'x-gtk-use-system-tooltips t)  ; t: use system tooltips, nil: use built-in
  (customize-set-variable 'tooltip-frame-parameters '((name . "tooltip")
                                                      (internal-border-width . 6)
                                                      (border-width . 0)
                                                      (no-special-glyphs . t)))
  :config
  ;; If nil show tooltip at mouse, if negative show in minibuffer
  (tooltip-mode nil))

;; Auto-Saving (built-in)
;; Saves visited files in a separate flags at regular intervals.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save.html
;;
;; auto-save-list-file-prefix handled by no-littering
;;
;; TODO Check
;;  * https://github.com/bbatsov/super-save/
;;  * https://github.com/ChillarAnand/real-auto-save
(use-package files
  :straight nil
  :demand t
  :init
  ;; Enable Auto-save in separated file
  (customize-set-variable 'auto-save-default t)
  ;; Disable 'real' auto-save (auto-save to the visited file)
  (if (version< "26.1" emacs-version)
      (customize-set-variable 'auto-save-visited-file-name nil)
    (customize-set-variable 'auto-save-visited-mode nil))
  ;; Save all files in `abz-cache-dir` instead of in the same directory as the visited file
  (customize-set-variable 'auto-save-file-name-transforms
                          `((".*" ,(abz--locate-data-dir "auto-save/") t)))
  (customize-set-variable 'auto-save-interval 1000)   ; Number of inputs between auto-saves
  (customize-set-variable 'auto-save-timeout 101)     ; Idle time before auto-save
  (customize-set-variable 'delete-auto-save-files t)) ; Keep auto-save files

;; List of recently visited files (built-in)
(use-package recentf
  :straight nil
  :demand t
  :init
  (customize-set-variable 'recentf-max-saved-items 40)
  :config
  (add-to-list 'recentf-exclude abz-data-dir)
  (add-to-list 'recentf-exclude abz-cache-dir))

;; Auto-Revert: revert files when they change on disk (built-in)
(use-package autorevert
  :straight nil
  :diminish auto-revert-mode
  :init
  (customize-set-variable 'global-auto-revert-non-file-buffers t) ; Also auto-revert buffer-menu and dired buffers
  (customize-set-variable 'auto-revert-verbose nil)               ; Be quiet
  :hook (after-init . global-auto-revert-mode))

;; avoid: Gets the mouse out of the cursor (built-in)
(use-package avoid
  :straight nil
  :if (display-mouse-p)
  :init
  (customize-set-variable 'mouse-avoidance-threshold 12)
  (customize-set-variable 'mouse-avoidance-nudge-dist 18)
  (defun abz--mouse-avoidance-mode ()
    (mouse-avoidance-mode 'jump))
  :hook (after-init . abz--mouse-avoidance-mode))

;; Winner: record window configuration and allow undo/redo (built-in)
;; Default keybindings: `C-c <left>` and `C-c <right>`
(use-package winner
  :straight nil
  :init
  (defun abz--winner-mode ()
    (winner-mode 1))
  :hook (after-init . abz--winner-mode))

;; Emacs server (emacsclient)
(use-package server
  :straight nil
  :if (display-graphic-p nil)
  :commands server-running-p
  :functions server-start
  :init
  (defun abz--server-start ()
    "Starts Emacs server if necessary."
    (unless (server-running-p)
      (server-start)))
  :hook (after-init . abz--server-start))

;; browse-url: pass a URL to a WWW browser (built-in)
(use-package browse-url
  :straight nil
  :init
  (customize-set-variable 'browse-url-browser-function #'browse-url-default-browser) ; Default function for URLs
  (customize-set-variable 'browse-url-mailto-function nil)                           ; Default function for mailto
  (customize-set-variable 'browse-url-firefox-new-window-is-tab t)                   ; Open in new tab instead of new window
  (customize-set-variable 'browse-url-epiphany-new-window-is-tab t)                  ; Open in new tab instead of new window
  :bind ("<C-M-return>" . browse-url))

;; Paradox: better package menu
;; http://github.com/Malabarba/paradox
(use-package paradox
  :straight nil
  :disabled
  :init
  (setq paradox-github-token t)             ; Don't ask for GitHub token
  (setq paradox-execute-asynchronously nil) ; Don't try to do things asynchronously
  (setq paradox-automatically-star nil))    ; Do not star automatically when (un)installing

;; Dired file explorer (built-in)
(use-package dired
  :straight nil
  :init
  (customize-set-variable 'dired-auto-revert-buffer t)                      ; Revert buffer on revisiting
  (customize-set-variable 'dired-clean-confirm-killing-deleted-buffers nil) ; Don't ask to kill buffer visiting deleted files
  (customize-set-variable 'dired-dwim-target t)                             ; Try to guess a default target directory
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames) ;; Search only in filenames
              ("C-P" . dired-up-directory)))

;; Open dired at the directory of the current file (built-in)
(use-package dired-x
  :straight nil
  :init
  (customize-set-variable 'dired-bind-jump t))

;; Various dired improvements
;;
;; TODO Configure & enable.
(use-package dired-hacks
  :disabled
  :straight nil
  :bind (:map dired-mode-map
              ([remap dired-previous-line] . dired-hacks-previous-file)
              ([remap dired-next-line] . dired-hacks-next-file))
  :after dired)

;; Ibuffer: buffer list (built-in)
;;
;; TODO Define a sort function (`define-ibuffer-sorter`) that sort a) by recently used and b) by mode (e.g dired)
(use-package ibuffer
  :straight nil
  :init
  ;; TODO Add custom for major-mode list to hide.
  (defun abz--ibuffer-hidden-buffers-predicate (buffer)
    (with-current-buffer buffer
      (member major-mode '(Buffer-menu-mode
                           ibuffer-mode
                           minibuffer-inactive-mode))))
  ;; Buffers to never show
  (customize-set-variable 'ibuffer-never-show-predicates #'(abz--ibuffer-hidden-buffers-predicate))
  ;; Buffers to show conditionally (`C-u g`)
  (customize-set-variable 'ibuffer-maybe-show-predicates '("^\\*Warnings\\*$"
                                                           "^\\*Compile-Log\\*$"
                                                           "^\\*straight-process\\*$"))
  (customize-set-variable 'ibuffer-use-other-window t)           ; Show in another window by default
  (customize-set-variable 'ibuffer-show-empty-filter-groups nil) ; Hide empty groups
  :bind ([remap list-buffers] . ibuffer))

;; Group buffers in ibuffer list by projectile project
(use-package ibuffer-projectile
  :after ibuffer
  :commands ibuffer-projectile-set-filter-groups
  :init
  (defun abz--ibuffer-set-filter-groups ()
    (setq ibuffer-filter-groups
          (append (ibuffer-projectile-generate-filter-groups)
                  #'(("Prog" (or
                              (derived-mode . prog-mode)
                              (derived-mode . cmake-mode)))
                     ("Search" (or (mode . grep-mode)
                                   (mode . ag-mode)))
                     ("Doc" (or (mode . Info-mode)
                                (mode . Man-mode)))
                     ("Dired" (mode . dired-mode))
                     ("Magit" (derived-mode . magit-mode))
                     ("Special" (name . "^ *\\*.*\\*$"))))))
  (defun abz--ibuffer-group-by-projectile ()
    (setq ibuffer-hook #'abz--ibuffer-set-filter-groups)
    (let ((ibuf (get-buffer "*Ibuffer*")))
      (when ibuf
        (with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t)))))
  ;; TODO Find something better if there is a font with symbols
  (customize-set-variable 'ibuffer-projectile-prefix "∞ ")
  :hook (ibuffer . abz--ibuffer-group-by-projectile))

;; immortal-scratch: respawn scratch buffer
;; https://bitbucket.org/jpkotta/immortal-scratch/
(use-package immortal-scratch
  :straight (:type git
                   :host nil
                   :repo "https://notabug.org/pluc/immortal-scratch.el.git"
                   :branch "master")
  :hook (after-init . immortal-scratch-mode))

(provide 'abz-custom)

;;; abz-custom.el ends here
