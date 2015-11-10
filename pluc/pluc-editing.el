;;; pluc-editing.el --- General edition configuration             -*- lexical-binding: t; -*-
;;
;; Author: Pierre-Luc Perrier <pluc@the-pluc.net>
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(progn
  ;; Editing basics
  (customize-set-variable 'goal-column nil)                           ; Use 'line-move-visual'
  (customize-set-variable 'line-move-visual t)                        ; Move point by visual line
  (customize-set-variable 'mark-even-if-inactive t)                   ; Mark stays even when inactive
  (customize-set-variable 'mode-require-final-newline t)              ; Ask major-modes to add a final newline before saving
  (customize-set-variable 'parse-sexp-ignore-comments t)              ; Threat comments as whitespaces
  (customize-set-variable 'require-final-newline t)                   ; Add a final new line before saving
  (customize-set-variable 'shift-select-mode t)                       ; Mark with shift
  (customize-set-variable 'show-trailing-whitespace t)                ; Highlight trailing whitespaces
  (customize-set-variable 'tab-width 2)                               ; Tab stops width (display)
  (customize-set-variable 'track-eol t)                               ; Keep vertical motion at the end of the lines
  (customize-set-variable 'transient-mark-mode t)
  (customize-set-variable 'words-include-escapes t)                   ; Treat espace chars as part of the words
  (when (fboundp 'delete-selection-mode) (delete-selection-mode nil)) ; Typing replaces current selection

  ;; Killing and yanking
  (customize-set-variable 'delete-active-region 'kill)            ; Kill region instead of delete
  (customize-set-variable 'kill-do-not-save-duplicates t)         ; Do not duplicate equivalent consecutive kills
  (customize-set-variable 'kill-ring-max 4096)                    ; Kill-ring capacity
  (customize-set-variable 'kill-whole-line t)                     ; When killing a whole line, also remove the terminating newline
  (customize-set-variable 'save-interprogram-paste-before-kill t) ; Save clipboard before killing within emacs
  (customize-set-variable 'select-active-regions 'only)           ; Only temporary region set the selection
  (customize-set-variable 'x-select-enable-clipboard t)           ; Use the clipboard
  )

;;;###autoload
(defun align-comments (begin end)
  "Align comments within region between BEGIN and END."
  (interactive "*r")
  (let ((indent-tabs-mode nil))
    (align-regexp begin end (concat "\\(\\s-*\\)"
                                    (regexp-quote comment-start)))))

(bind-keys
 ("RET" . reindent-then-newline-and-indent) ; Indent on new line
 ("C-c s" . sort-lines))

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
;; https://github.com/rejeep/drag-stuff.el				  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual feedback on yanks, undo, etc							 ;;
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
