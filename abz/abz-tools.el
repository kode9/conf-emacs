;;; abz-tools.el --- External tools integration -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Pierre-Luc Perrier

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

;; Remote access
(use-package tramp
  :straight nil
  :init
  (customize-set-variable 'tramp-default-method "ssh") ; Better than SCP
  (customize-set-variable 'tramp-auto-save-directory
                          (expand-file-name ".cache/tramp/auto-save" user-emacs-directory)) ; Keep auto-save files in local
  (customize-set-variable 'tramp-backup-directory-alist
                          `(("." . ,(expand-file-name ".cache/tramp/backup" user-emacs-directory)))) ; Backup files
  (customize-set-variable 'tramp-persistency-file-name
                          (expand-file-name ".cache/tramp/history" user-emacs-directory)) ; Connection history
  ;; Disable version control for tramp files
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (use-package vc
    :straight nil
    :init
    (customize-set-variable 'vc-ignore-dir-regexp
                            (format "\\(%s\\)\\|\\(%s\\)"
                                    vc-ignore-dir-regexp
                                    tramp-file-name-regexp)))
  :config
  (setenv "SHELL" "/bin/bash"))

;; http://www.emacswiki.org/emacs/TrampMode
;;;###autoload
(defun toggle-sudo ()
  "Reopen the current file as root."
  (interactive)
  (when buffer-file-name (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

;; Magit: A Git Porcelain inside Emacs
;; https://magit.vc/
(use-package magit
  :init
  (customize-set-variable 'magit-wip-merge-branch nil)              ; Reset wip branch on new commits
  (customize-set-variable 'magit-no-confirm '('safe-with-wip))      ; Disable confirmation for wip-mode' safe operations
  (customize-set-variable 'magit-diff-paint-whitespace 'uncommited) ; Highlight whipespaces on uncommited changes
  (customize-set-variable ' magit-diff-refine-hunk t)               ; Show word-granularity diff for current hunk
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
  (magit-wip-mode)                                     ; Commit in a wip/ branch on some actions
  (when (and (boundp 'global-auto-revert-mode) (not global-auto-revert-mode))
    (magit-auto-revert-mode))                          ; Auto-revert tracked files buffers
  (remove-hook 'server-switch-hook 'magit-commit-diff) ; Don't show diff when committing by default
  (when (boundp 'vc-handled-backends)                   ; Tell VC to not handle git
    (customize-set-variable 'vc-handled-backends (delq 'Git vc-handled-backends)))
  :diminish magit-wip-mode
  :bind
  ("C-c m s" . magit-status)
  ("C-c m b" . magit-blame)
  ("C-c m d" . magit-diff-working-tree)
  ("C-c m l" . magit-log-buffer-file)
  ("C-c m L" . magit-log-all))

;; Step through historic versions of git controlled file
;; https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine
  :bind
  ("C-c m h" . git-timemachine))

;; Display commit at line in a popup
(use-package git-messenger
  :init
  (customize-set-variable 'git-messenger:show-detail t)
  (customize-set-variable 'git-messenger:use-magit-popup t)
  :bind
  ("C-c m m" . git-messenger:popup-message))

(use-package pass
  :if (> emacs-major-version 24))

(provide 'abz-tools)

;;; abz-tools.el ends here
