;;; abz.el --- Some useful elisp functions -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Pierre-Luc Perrier

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

(require 'files)
(require 'simple)
(require 'tabify)

(defalias 'abz-beginning-of-line? 'bolp)

(defalias 'abz-end-of-line? 'eolp)

(defun abz-empty-line? ()
  "Return t if point is at an empty line, i.e not a single character."
  (and (abz-beginning-of-line?) (abz-end-of-line?)))

(defun abz-non-empty-line-position-backward ()
  "Return the character position of the first non-empty line.

Search for an empty line going backward from the current position, and return
the largest position (last character) of the first non-empty line, or
`point-min' if no such line exists."
  (save-excursion
    (while (and (abz-empty-line?) (= (forward-line -1) 0)))
    (line-end-position)))

(defun abz-non-empty-line-position-forward ()
  "Return the character position of the first non-empty line.

Search for an empty line going forward from the current position, and return the
largest position (last character) of the first non-empty line, or `point-max' if
no such line exists."
  (save-excursion
    (while (and (abz-empty-line?) (= (forward-line 1) 0)))
    (line-end-position)))

(defun abz-region-positions-dwim ()
  "Return positions (beg . end) of the current region, (sub)line, or paragraph.

* If region is active, return its beginning and end positions.

* If point is on an empty line, return the previous paragraph positions
  according to `backward-paragraph'.

* If point is on the middle of a line, return the current position and the end
  of line.

* If the point is at the beginning or end of line, return the whole current line
  positions."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let
        ((beg (cond
               ((abz-empty-line?) (save-excursion (forward-paragraph -1) (point)))
               ((abz-end-of-line?) (line-beginning-position))
               ((point)))))
      (cons beg (abz-non-empty-line-position-backward)))))

;;;###autoload
(defun abz-mark-previous-paragraph ()
  "Set the mark and call `backward-paragraph."
  (interactive)
  (set-mark-command nil)
  (backward-paragraph 1))

;;;###autoload
(defun abz-mark-next-paragraph ()
  "Set the mark and call `forward-paragraph."
  (interactive)
  (set-mark-command nil)
  (forward-paragraph 1))

;;;###autoload
(defun abz-comment-dwim ()
  "Comment or uncomment the region as defined by `abz-region-positions-dwim'."
  (interactive)
  (let
      ((region (abz-region-positions-dwim)))
    (comment-or-uncomment-region (car region) (cdr region))
    t))

;;;###autoload
(defun abz-clone-and-comment-dwim ()
  "Clone the region as defined by `abz-region-positions-dwim' and comment the previous one."
  (interactive)
  (let*
      ((region (abz-region-positions-dwim))
       (beg (car region))
       (end (cdr region))
       (content (buffer-substring-no-properties beg end)))
    (goto-char end)
    (newline)
    (insert content)
    (comment-region beg end)
    (goto-char (- (point) (- end beg)))
    (when (abz-empty-line?) (forward-line))))

;;;###autoload
(defun abz-delete-trailing-whitespace ()
  "Deletes trailing whitespaces and lines.

Acts as `delete-trailing-whitespace' with `delete-trailing-lines' and
`require-final-newline' set to true."
  (interactive)
  (let ((delete-trailing-lines t)
        (require-final-newline t))
    (call-interactively #'delete-trailing-whitespace)))

;;;###autoload
(defun abz-indent-region (START END)
  (interactive "rP")
  (cond ((and (bound-and-true-p lsp-mode)
              (lsp-feature? "textDocument/formatting"))
         (call-interactively #'lsp-format-region))
        (call-interactively #'indent-region)))

;;;###autoload
(defun abz-indent-buffer ()
  (interactive)
  (cond ((and (bound-and-true-p lsp-mode)
              (lsp-feature? "textDocument/formatting"))
         (call-interactively #'lsp-format-buffer))
        ((and (derived-mode-p #'c++-mode)
              (fboundp #'projectile-project-root)
              (file-readable-p (expand-file-name ".clang-format" (projectile-project-root))))
         (call-interactively #'clang-format-buffer))
        (t
         (indent-region (point-min) (point-max) nil))))

;;;###autoload
(defun abz-indent-dwim ()
  "Indent a region or a buffer.

Same as `indent-region' but indents the whole buffer no region is active."
  (interactive)
  (if (use-region-p)
      (call-interactively #'abz-indent-region)
    (call-interactively #'abz-indent-buffer)))

;;;###autoload
(defun abz-untabify-dwim ()
  "Convert all tabs to spaces in a region or a buffer.

Same as `untabify' but indents the whole buffer no region is active."
  (interactive)
  (if (use-region-p)
      (call-interactively #'untabify)
    (untabify (point-min) (point-max) nil)))

(provide 'abz)

;;; abz.el ends here
