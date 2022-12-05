;;; abz-editing.el --- Editing configuration -*- lexical-binding: t; -*-

;; Copyright (C) Pierre-Luc Perrier

;; Author: Pierre-Luc Perrier <dev@the-pluc.net>

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;

;;; Code:

(require 'abz)
(require 'abz-settings)
(require 'use-package)

(progn
  ;; Editing basics
  (customize-set-variable 'delete-trailing-lines t)                   ; Ask `delete-trailing-whitespace' to remove trailing lines
  (customize-set-variable 'goal-column nil)                           ; Use 'line-move-visual'
  (customize-set-variable 'line-move-visual t)                        ; Move point by visual line
  (customize-set-variable 'mark-even-if-inactive t)                   ; Mark stays even when inactive
  (customize-set-variable 'mode-require-final-newline t)              ; Ask major-modes to add a final newline before saving
  (customize-set-variable 'parse-sexp-ignore-comments t)              ; Threat comments as whitespaces
  (customize-set-variable 'require-final-newline t)                   ; Add a final new line before saving
  (customize-set-variable 'shift-select-mode t)                       ; Mark with shift
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

  ;; Indentation
  (customize-set-variable 'indent-tabs-mode nil)                 ; Do not insert tabs when indenting
  (customize-set-variable 'tab-always-indent 'complete)          ; TAB indent or complete
  (customize-set-variable 'fill-column 80)                       ; Columns before line wrapping
  (customize-set-variable 'emacs-lisp-docstring-fill-column nil) ; Respect fill-column
  (customize-set-variable 'c-basic-offset 2)                     ; Indentation offset

  (customize-set-variable 'undo-outer-limit 26214400)                      ; Maximum information in a single undo command. 25MiB
  (customize-set-variable 'warning-suppress-types '((undo discard-info)))) ; Be quiet

;; Remember last cursor position in all files
(use-package saveplace
  :straight nil
  :init
  (customize-set-variable 'save-place-limit nil)
  (customize-set-variable 'save-place-save-skipped nil)
  (customize-set-variable 'save-place-version-control t)
  :hook (after-init . save-place-mode))

(use-package abbrev
  :straight nil
  :diminish)

;;;###autoload
(defun align-comments (begin end)
  "Align comments within region between BEGIN and END."
  (interactive "*r")
  (let ((indent-tabs-mode nil))
    (align-regexp begin end (concat "\\(\\s-*\\)"
                                    (regexp-quote comment-start)))))

;; (built-in) Highlight current line
(use-package hl-line
  :disabled
  :straight nil
  :custom
  (global-hl-line-sticky-flag t "Keep highlighting lines in all windows")
  :hook
  (after-init . global-hl-line-mode))

;; Increase selected region by semantic units
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :demand t
  :bind
  ("C-=" . er/expand-region)
  ("M-=" . er/contract-region))

;; Drag stuff (lines, words, region, etc...) around
;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :diminish drag-stuff-mode
  :commands drag-stuff-define-keys
  :config
  (drag-stuff-define-keys)
  :hook (after-init . drag-stuff-global-mode))

;; Visual feedback on yanks, undo, etc
;; http://www.emacswiki.org/emacs/VolatileHighlights
(use-package volatile-highlights
  :custom-face (vhl/default-face ((t (:background nil :underline t))))
  :hook (after-init . volatile-highlights-mode)
  :diminish volatile-highlights-mode)

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

;; undo-tree: undo-redo as a tree, with visualizer.
;; https://elpa.gnu.org/packages/undo-tree.html
;;
;; no-littering: `undo-tree-history-directory-alist`
(use-package undo-tree
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history nil)     ; Don't save undo tree to a file
  (undo-tree-enable-undo-in-region nil) ; Don't enable in regions
  (undo-tree-visualizer-diff t)         ; Display diff by default in visualizer
  (undo-tree-visualizer-timestamps t)   ; Show timestamps
  :preface
  ;; Advice the function that compute the filename to compress history files
  (let ((ext (cond
              ;; `auto-compression-mode' does not seem to know a lot of formats (e.g lz4) but it knows zstd
              ((executable-find "zstd") ".zst")
              ((executable-find "gzip") ".gz")
              (""))))
    (defun abz--undo-tree-make-history-file-name-append-compression (filename)
      "Append file extension to `undo-tree-save-history` uses compression."
      (concat filename ext)))
  :init
  (advice-add 'undo-tree-make-history-save-file-name
              :filter-return #'abz--undo-tree-make-history-file-name-append-compression)
  :bind (:map undo-tree-visualizer-mode-map
              ("RET" . undo-tree-visualizer-quit)
              ("q" . undo-tree-visualizer-abort))
  :hook (after-init . global-undo-tree-mode))

;; AsciiDoc
(use-package adoc-mode
  :mode "\\.a\\(?:scii\\)?doc\\'")

;; Ispell/Flyspell
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(use-package ispell
  :straight nil
  :custom
  (ispell-program-name (cond ((executable-find "aspell"))
                             ((executable-find "hunspell"))
                             ((executable-find "ispell"))))
  (ispell-extra-args (cond ((string-match  "aspell$" ispell-program-name) #'("--sug-mode=normal"
                                                                             "--dont-run-together"
                                                                             "--run-together-limit=4"
                                                                             "--run-together-min=2")))))

(use-package flyspell
  :straight nil
  :custom
  (flyspell-default-dictionary "english")
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(use-package backup-each-save
  :demand t
  :init
  (setq
   backup-each-save-mirror-location (abz--locate-data-dir "each-save/")
   backup-each-save-time-format "%Y%m%d-%H%M%S")
  :hook (after-save . backup-each-save))

(use-package rainbow-mode)

;; Edit multiple regions simultaneously in a buffer or a region
;; https://github.com/victorhge/iedit
(use-package iedit
  :bind (("C-c e" . iedit-mode)))

(use-package multiple-cursors)

(use-package goto-last-change)

(use-package lice
  :custom
  (lice:comment-style 'extra-line)
  (lice:mode-comments '((c-mode :comment-start "/*" :comment-end "*/")
                        (c++-mode :comment-start "//" :comment-continue "//" :comment-end "//")
                        (cmake-mode :comment-start "#" :comment-continue "#" :comment-end "#" :comment-empty-lines nil)
                        (java-mode :comment-start "/*" :comment-end "*/")
                        (groovy-mode :comment-start "/*" :comment-end "*/")
                        (nxml-mode :comment-continue "   "))))

;; Collaborative live editing
(use-package crdt
  :straight (:type git
                   :host nil
                   :repo "https://code.librehq.com/qhong/crdt.el"
                   :branch "master")
  :custom
  (crdt-ask-for-name nil)
  (crdt-ask-for-password nil)
  :commands
  crdt-share-buffer
  crdt-connect)

;;;;;;;;;;;;;;;;;;
;; key bindings ;;
;;;;;;;;;;;;;;;;;;

(bind-keys
 ("RET" . reindent-then-newline-and-indent) ; Indent on new line
 ("M-g" . goto-line)
 ("C-c s" . sort-lines)
 ("S-SPC" . abz-mark-next-paragraph)
 ("S-M-SPC" . abz-mark-previous-paragraph))

(bind-keys*
 ("C-c c" . abz-comment-dwim)
 ("M-;" . abz-comment-dwim)
 ("C-;" . abz-clone-and-comment-dwim))

(provide 'abz-editing)

;;; abz-editing.el ends here
