;;; pluc-tools.el --- External tools integration           -*- lexical-binding: t; -*-
;;
;;
;; Author: Pierre-Luc Perrier <pluc@the-pluc.net>
;;
;;; Commentary:
;;
;;; Code:

;; Remote access
(use-package tramp
  :defer t
  :init
  (customize-set-variable 'tramp-default-method "ssh") ; Better than SCP
  (customize-set-variable 'tramp-auto-save-directory
                          (expand-file-name ".cache/tramp/auto-save" user-emacs-directory)) ; Keep auto-save files in local
  (customize-set-variable 'tramp-backup-directory-alist
                          `(("." . ,(expand-file-name ".cache/tramp/backup" user-emacs-directory)))) ; Backup files
  (customize-set-variable 'tramp-persistency-file-name
                          (expand-file-name ".cache/tramp/history" user-emacs-directory)) ; Connection history
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
  :init
  (setq ag-highlight-search t) ; Highlight search terms
  (setq ag-reuse-buffer t)     ; Use a single buffer
  :bind*
  ;; Search STRING in DIR
  ("C-c a a" . ag)
  ("C-c a A" . ag-project)
  ;; Search REGEX in DIR
  ("C-c a r" . ag-regexp)
  ("C-c a R" . ag-project-regexp)
  ;; Search STRING in DIR, limited FILE TYPES
  ("C-c a f" . ag-files)
  ("C-c a F" . ag-project-files)
  ;; Find FILES in DIR
  ("C-c A d" . ag-dired)
  ("C-c A D" . ag-project-dired)
  ;; Find FILES matching REGEX in DIR
  ("C-c A r" . ag-dired-regexp)
  ("C-c A R" . ag-project-regexp)
  :config
  ;; Edit ag buffers inplace
  (use-package wgrep-ag
    :commands wgrep-ag-setup
    :init (add-hook 'ag-mode-hook #'wgrep-ag-setup)))

;; magit: (awesome) git frontend
(use-package magit
  :bind
  ("C-c m s" . magit-status)
  ("C-c m b" . magit-blame)
  ("C-c m d" . magit-diff-working-tree)
  ("C-c m l" . magit-log-buffer-file)
  ("C-c m L" . magit-log-all)
  :config
  (setq magit-revert-buffers 'silent)      ; Revert buffers silently
  (setq magit-save-repository-buffers t)   ; Ask confirmation when saving buffers
  (setq magit-refs-show-commit-count 'all) ; Show counts for branches and tags
  ;; Status header format
  (setq magit-status-headers-hook '(magit-insert-repo-header
                                    magit-insert-user-header
                                    magit-insert-diff-filter-header
                                    magit-insert-remote-header
                                    magit-insert-tags-header
                                    magit-insert-upstream-header
                                    magit-insert-head-header))
  (setq magit-status-buffer-name-format "*magit-status＠%b*"); Status buffer name format
  )

(provide 'pluc-tools)
;;; pluc-tools.el ends here
