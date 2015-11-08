;;; pluc-editing.el --- General edition configuration             -*- lexical-binding: t; -*-
;;
;; Author: Pierre-Luc Perrier <pluc@the-pluc.net>
;;
;;; Commentary:
;;
;;; Code:

(setq
 show-paren-style 'expression ; Show full expression
 show-paren-delay 0.01        ; Delay before showing
 )

(delete-selection-mode 1) ; Typing replaces current selection
(show-paren-mode 1) ; Highlights parenthesis

(bind-key "RET" 'reindent-then-newline-and-indent) ; Indent on new line

;; Highlight current line
(use-package hl-line
  :init
  (setq global-hl-line-sticky-flag t) ; Keep highlight in all windows
  :config
  (global-hl-line-mode nil))

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
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual feedback on yanks, undo, etc		     ;;
;; http://www.emacswiki.org/emacs/VolatileHighlights ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; Kill ring visualizer / browser
(use-package browse-kill-ring
  :demand t
  :init
  (setq browse-kill-ring-highlight-current-entry nil
	browse-kill-ring-highlight-inserted-item nil
	browse-kill-ring-separator "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	browse-kill-ring-show-preview t
	browse-kill-ring-separator-face 'font-lock-comment-face)
  (defun yank-pop-forwards (n)
    "Cycle the 'kill-ring' forward (reverse of 'yank-pop').
With argument N go to the nth entry."
    (interactive "p")
    (yank-pop (- n)))
  :bind*
  (("C-y" . yank)
   ("M-y" . yank-pop)
   ("M-Y" . yank-pop-forwards)
   ("C-S-y" . browse-kill-ring)))

;; Treat undo history as a tree
(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :init
  (setq
   undo-tree-auto-save-history t ; Save undo tree to a file
   undo-tree-history-directory-alist `(("." . ,(expand-file-name ".cache/undo" user-emacs-directory)))
   undo-tree-enable-undo-in-region t)
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))) ; Compress undo tree files
  :config
  (global-undo-tree-mode)
  :bind*
  (("C-n" . undo-tree-undo)
   ("C-," . undo-tree-redo)
   ("M-n" . undo-tree-switch-branch)
   ("C-n" . undo-tree-visualize)))

(provide 'pluc-editing)
;;; pluc-editing.el ends here
