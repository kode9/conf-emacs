;; External tools integration

;; ag the silver searcher
;;; https://github.com/ggreer/the_silver_searcher
;;; https://github.com/Wilfred/ag.el
(use-package ag
  :init
  (setq ag-highlight-search t) ;; Highlight search terms
  (setq ag-reuse-buffer t) ;; Use a single buffer
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
  )

;; magit: Git frontend
;;; http://magit.vc/
(use-package magit
  :bind
  ("C-c m s" . magit-status)
  ("C-c m b" . magit-blame)
  ("C-c m d" . magit-diff-working-tree)
  ("C-c m l" . magit-log-buffer-file)
  ("C-c m L" . magit-log-all)
  :config
  (setq magit-revert-buffers 'silent) ;; Revert buffers silently
  (setq magit-save-repository-buffers t) ;; Ask confirmation when saving buffers
  (setq magit-refs-show-commit-count 'all) ;; Show counts for branches and tags
  )

(provide 'pluc-tools)
