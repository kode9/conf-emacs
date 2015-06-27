;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common editing settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode 1) ;; Typing replaces current selection

;; Parenthesis
(setq show-paren-style 'expression) ;; Show full expression
(setq show-paren-delay 0.01)        ;; Delay before showing
(show-paren-mode 1)                 ;; Highlights parenthesis

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(use-package bind-key
  :bind
  ("RET" . newline-and-indent) ;; Indent on new line
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Increase selected region by semantic units  ;;
;; https://github.com/magnars/expand-region.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind
  ("C-a" . er/expand-region)
  ("M-a" . er/contract-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drag stuff (lines, words, region, etc...) around ;;
;; https://github.com/rejeep/drag-stuff.el	    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package drag-stuff
  :config
  (drag-stuff-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual feedback on yanks, undo, etc		     ;;
;; http://www.emacswiki.org/emacs/VolatileHighlights ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))
