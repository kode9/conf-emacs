;; Development packages and customisation

;; Enclose cursor within punctuation pairs.
;; https://github.com/rejeep/enclose.el
(use-package enclose
  :disabled t
  ;; :init
  ;; (add-to-list 'enclose-except-modes 'ido-mode)
  :config
  (enclose-global-mode t))

;; Wrap text with punctation or tag
;; https://github.com/rejeep/wrap-region.el
(use-package wrap-region
  :config
  (wrap-region-global-mode t))

;; clang-format
(use-package clang-format)

(defun format-buffer()
  "Format buffer if clang-format is available and the buffer is cc-mode derived, otherwise call indent-buffer"
  (interactive)
  (unless (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode (clang-format-buffer) t) (indent-buffer)))

(provide 'pluc-devel)
