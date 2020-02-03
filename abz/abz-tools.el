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

;; magit: (awesome) git frontend
(use-package magit
  :bind
  ("C-c m s" . magit-status)
  ("C-c m b" . magit-blame)
  ("C-c m d" . magit-diff-working-tree)
  ("C-c m l" . magit-log-buffer-file)
  ("C-c m L" . magit-log-all)
  :config
  (customize-set-variable 'vc-handled-backends (delq 'Git vc-handled-backends)) ; Tell VC to not handle git
  (setq magit-revert-buffers 'silent)      ; Revert buffers silently
  (setq magit-save-repository-buffers t)   ; Ask confirmation when saving buffers
  (setq magit-refs-show-commit-count 'all) ; Show counts for branches and tags
  ;; Status header format
  (setq magit-status-headers-hook '(magit-insert-error-header
                                    magit-insert-repo-header
                                    magit-insert-user-header
                                    magit-insert-diff-filter-header
                                    magit-insert-remote-header
                                    magit-insert-upstream-branch-header
                                    magit-insert-push-branch-header
                                    magit-insert-head-branch-header
                                    magit-insert-tags-header))
  ;; Status buffer name format
  (setq magit-status-buffer-name-format "*magit-status＠%b*"))

(use-package pass
  :if (> emacs-major-version 24))

(provide 'abz-tools)

;;; abz-tools.el ends here
