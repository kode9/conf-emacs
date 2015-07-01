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
  :diminish wrap-region-mode
  :config
  (wrap-region-global-mode t))

;; clang-format
(use-package clang-format)

(defun format-buffer()
  "Format buffer if clang-format is available and the buffer is cc-mode derived, otherwise call indent-buffer"
  (interactive)
  (unless (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode (clang-format-buffer) t) (indent-buffer)))

;; git-gutter+: View, stage and revert Git changes straight from the
;; buffer.
;;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :defer t
  :functions git-gutter+-mode
  :diminish git-gutter+-mode
  :init
  (add-hook 'prog-mode-hook 'git-gutter+-mode))

(provide 'pluc-devel)
