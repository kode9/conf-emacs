;;; abz-tools.el --- External tools integration -*- lexical-binding: t; -*-

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

(require 'abz)
(require 'abz-settings)
(require 'files)
(require 'use-package)

;; https://github.com/BurntSushi/ripgrep
;; https://github.com/ggreer/the_silver_searcher
;; https://github.com/monochromegane/the_platinum_searcher
;; https://sift-tool.org/
;; https://github.com/gvansickle/ucg
;; https://git-scm.com/docs/git-grep
;;
;; TODO Use `:set` to check the executable exists, and if not, revert back to one that does.
;;;###autoload
(defcustom abz-grep-command
  (cond
   ((executable-find "ripgrep") 'ripgrep)
   ((executable-find "ag") 'ag)
   ((executable-find "pt") 'pt)
   ((executable-find "sift") 'sift)
   ((executable-find "ucg") 'ucg)
   ((executable-find "grep") 'grep))
  "The search tool to use."
  :type '(choice (const ripgrep)
                 (const ag)
                 (const pt)
                 (const sift)
                 (const ucg)
                 (const grep))
  :tag "Grep command"
  :group 'abz
  :group 'external)

;; ag the silver searcher: a better grep alternative
(use-package ag
  :if (eql abz-grep-command 'ag)
  :init
  (customize-set-variable 'ag-arguments #'("--smart-case"))      ; Additional arguments
  (customize-set-variable 'ag-context-lines nil)                 ; Number of context lines
  (customize-set-variable 'ag-executable (executable-find "ag")) ; Command to use
  (customize-set-variable 'ag-group-matches t)                   ; Group matches by file
  (customize-set-variable 'ag-highlight-search t)                ; Highlight search terms
  (customize-set-variable 'ag-reuse-buffers t)                   ; Use a single buffer
  :bind*
  ;; Search STRING in DIR
  ("C-c a A" . ag)
  ("C-c a a" . ag-project)
  ;; Search REGEX in DIR
  ("C-c a R" . ag-regexp)
  ("C-c a r" . ag-project-regexp)
  ;; Search STRING in DIR, limited FILE TYPES
  ("C-c a s" . ag-files)
  ("C-c a S" . ag-project-files)
  ;; Find FILES in DIR
  ("C-c a F" . ag-dired)
  ("C-c a f" . ag-project-dired)
  ;; Find FILES matching REGEX in DIR
  ("C-c a D" . ag-dired-regexp)
  ("C-c a d" . ag-project-regexp))

;; Edit ag buffers inplace
(use-package wgrep-ag
  :after ag
  :commands wgrep-ag-setup
  :hook (ag-mode . wgrep-ag-setup))

;; (debug-on-variable-change 'magit-auto-revert-mode)

;; Commit in a wip/ branch on some actions
;; Part of magit
(use-package magit-wip
  :straight nil
  :after magit
  :demand
  :diminish magit-wip-mode
  :custom
  (magit-wip-merge-branch nil "Reset wip branch on new commits")
  :config
  (magit-wip-mode))

;; Auto-revert tracked files buffers
;; Part of magit
(use-package magit-autorevert
  :unless (bound-and-true-p global-auto-revert-mode)
  :straight nil
  :after magit
  :demand
  :config
  (message "abz: magit autorevert %s" (bound-and-true-p global-auto-revert-mode))
  (unless (bound-and-true-p global-auto-revert-mode)
    (message "abz: magit autorevert activage")
    (magit-auto-revert-mode)))

;; Magit: A Git Porcelain inside Emacs
;; https://magit.vc/
(use-package magit
  :ensure-system-package git
  :init
  (customize-set-variable 'magit-no-confirm '('safe-with-wip))      ; Disable confirmation for wip-mode' safe operations
  (customize-set-variable 'magit-diff-paint-whitespace 'uncommited) ; Highlight whipespaces on uncommited changes
  (customize-set-variable 'magit-diff-refine-hunk t)                ; Show word-granularity diff for current hunk
  (customize-set-variable 'magit-save-repository-buffers 'dontask)  ; Save file visiting buffers without asking
  (customize-set-variable 'magit-refs-show-commit-count 'branch)    ; Show commit counts for branches
  (customize-set-variable 'magit-module-sections-nested nil)
  ;; Status sections
  (customize-set-variable 'magit-status-sections-hook '(magit-insert-status-headers
                                                        magit-insert-merge-log
                                                        magit-insert-rebase-sequence
                                                        magit-insert-am-sequence
                                                        magit-insert-sequencer-sequence
                                                        magit-insert-bisect-output
                                                        magit-insert-bisect-rest
                                                        magit-insert-bisect-log
                                                        magit-insert-unpulled-from-upstream
                                                        magit-insert-unpushed-to-upstream-or-recent
                                                        magit-insert-unpulled-from-pushremote
                                                        magit-insert-unpushed-to-pushremote
                                                        magit-insert-unstaged-changes
                                                        magit-insert-staged-changes
                                                        magit-insert-untracked-files
                                                        magit-insert-stashes))
  ;; Default sections visibility
  (customize-set-variable 'magit-section-initial-visibility-alist '((modules . hide)
                                                                    (stashes . show)
                                                                    (untracked . hide)))
  ;; Initial pointer position
  (customize-set-variable 'magit-status-initial-section '(((unstaged) (status))
                                                          ((staged) (status))
                                                          ((unpushed . "@{push}..") (status))
                                                          ((unpushed . "@{upstream}..") (status))
                                                          1))
  ;; Status header format
  (customize-set-variable 'magit-status-headers-hook '(magit-insert-user-header
                                                       magit-insert-repo-header
                                                       magit-insert-diff-filter-header
                                                       magit-insert-remote-header
                                                       magit-insert-upstream-branch-header
                                                       magit-insert-push-branch-header
                                                       magit-insert-head-branch-header
                                                       magit-insert-tags-header
                                                       magit-insert-error-header
                                                       magit-insert-modules))
  ;; Status margin (hidden by default)
  (customize-set-variable 'magit-status-margin '(nil "%Y-%m-%d" magit-log-margin-width t 8))
  (customize-set-variable 'magit-blame-echo-style 'margin)
  :config
  ;; Don't show diff when committing by default
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  ;; Tell VC to not handle git
  (when (boundp 'vc-handled-backends)
    (customize-set-variable 'vc-handled-backends (delq 'Git vc-handled-backends)))
  :bind
  (:map abz-map
        :prefix "C-c m"
        :prefix-map abz-map-magit
        :prefix-docstring "Prefix keymap for magit"
        ("s" ("Status" . magit-status))
        ("b" ("Blame" . magit-blame))
        ("d" ("Diff working" . magit-diff-working-tree))
        ("l" ("Log file" . magit-log-buffer-file))
        ("L" ("Log all" . magit-log-all))))

;; Step through historic versions of git controlled file
;; https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine
  :ensure-system-package git
  :bind
  ("C-c m h" . git-timemachine))

;; Display commit at line in a popup
(use-package git-messenger
  :ensure-system-package git
  :init
  (customize-set-variable 'git-messenger:show-detail t)
  (customize-set-variable 'git-messenger:use-magit-popup t)
  :bind
  ("C-c m m" . git-messenger:popup-message))

(use-package pass
  :if (> emacs-major-version 24))

(provide 'abz-tools)

;;; abz-tools.el ends here
