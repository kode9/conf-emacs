;; Development packages and customisation

;; Enclose cursor within punctuation pairs.
;; https://github.com/rejeep/enclose.el
(use-package enclose
  :config
  (enclose-global-mode t))

;; Wrap text with punctation or tag
;; https://github.com/rejeep/wrap-region.el
(use-package wrap-region
  :config
  (wrap-region-global-mode t))
