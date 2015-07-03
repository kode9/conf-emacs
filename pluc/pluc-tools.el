;; External tools integration

;; ag the silver searcher
;;; https://github.com/ggreer/the_silver_searcher
;;; https://github.com/Wilfred/ag.el
(use-package ag
  :init
  (setq ag-highlight-search t) ;; Highlight search terms
  (setq ag-reuse-buffer t) ;; Use a single buffer
  )

(provide 'pluc-tools)
