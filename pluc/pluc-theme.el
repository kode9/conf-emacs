;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package zenburn-theme :demand)

;; Fonts
;;; I'd like to try out "Fira Code" when Emacs get ligatures support
(use-package dynamic-fonts
  :demand
  :init
  (setq
   dynamic-fonts-set-alternatives t ;; Fill up alternative fonts
   dynamic-fonts-preferred-monospace-fonts '("Hack-10"
					     "Meslo LG S DZ-10"
					     "Envy Code R-11"
					     "Fira Mono-10"
					     "Inconsolata-11"
					     "Droid Sans Mono-10"
					     "Dejavu Sans Mono-10"
					     "Cousine-10"
					     "Liberation Mono-10"
					     "Anonymous Pro-11"
					     "BPmono-10")
   dynamic-fonts-preferred-monospace-point-size nil
   dynamic-fonts-preferred-proportional-fonts '("Open Sans"
						"Roboto"
						"Dejavu Sans"
						"Arimo"
						"Droid Sans"
						"Liberation Sans"
						"Verdana"
						"Arial")
   dynamic-fonts-preferred-proportional-point-size 11)
  :config (dynamic-fonts-setup))

(use-package spaceline-config
  :config (spaceline-emacs-theme))

(provide 'pluc-theme)
