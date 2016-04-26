;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package zenburn-theme :demand)

;; Fonts
(use-package dynamic-fonts
  :init
  (customize-set-variable 'dynamic-fonts-set-alternatives t) ; Fill up alternative fonts
  (customize-set-variable 'dynamic-fonts-preferred-monospace-point-size 10)
  (customize-set-variable 'dynamic-fonts-preferred-monospace-fonts
                          '("Hack" "Dejavu Sans Mono" "courier" "fixed"))
  (customize-set-variable 'dynamic-fonts-preferred-proportional-fonts
                          '("Open Sans" "Roboto" "Dejavu Sans" "arial" "fixed"))
  (customize-set-variable 'dynamic-fonts-preferred-proportional-point-size 10)
  :config
  (dynamic-fonts-setup))

(use-package spaceline-config
  :config (spaceline-emacs-theme))

(provide 'pluc-theme)
;;; pluc-theme.el ends here
