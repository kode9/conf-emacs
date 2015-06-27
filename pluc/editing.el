;; https://github.com/rejeep/drag-stuff.el
;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :config
  (drag-stuff-global-mode t))

;; Visual feedback on yanks, undo, etc
;; http://www.emacswiki.org/emacs/VolatileHighlights
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))
