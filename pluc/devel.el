;; Development packages and customisation

;; https://github.com/rejeep/enclose.el
;; Enclose cursor within punctuation pairs.
(use-package enclose
  :config
  (enclose-global-mode t))

;; https://github.com/rejeep/wrap-region.el
;; Wrap text with punctation or tag
(use-package wrap-region
  :config
  (wrap-region-global-mode t))
