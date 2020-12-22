;;; abz.el --- Some useful elisp functions -*- lexical-binding: t; -*-

;; Copyright (C) 2018 PERRIER Pierre-Luc <dev@the-pluc.net>

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
(require 'subr-x)
(require 'tabify)
(require 'use-package)

(declare-function straight-normalize-all "straight")
(declare-function straight-pull-all "straight")
(declare-function straight-rebuild-all "straight")

(declare-function projectile-project-root "projectile")

(declare-function clang-format-region "clang-format")
(declare-function clang-format-buffer "clang-format")

(declare-function lsp-feature? "lsp-mode")
(declare-function lsp-format-buffer "lsp-mode")
(declare-function lsp-format-region "lsp-mode")

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

(defun abz--lsp-mode? ()
  "Return t if `lsp-mode' is active."
  (bound-and-true-p lsp-mode))

(defun abz--lsp-format? ()
  "Return t if `lsp-mode' is active and can handle formatting."
  (and (abz--lsp-mode?)
       (fboundp 'lsp-feature?)
       (lsp-feature? "textDocument/formatting")))

(defun abz--lsp-clang-format? ()
  "Return t if `clang-format' is available and can be used."
  (and (derived-mode-p #'c++-mode)
       (fboundp #'clang-format-buffer)
       (fboundp #'projectile-project-root)
       (file-readable-p (expand-file-name ".clang-format" (projectile-project-root)))))

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
  "Indent the current region using the best tool available.

Indent the region between `START' and `END'."
  (interactive "rP")
  (cond ((abz--lsp-format?)
         (lsp-format-region START END))
        ((indent-region START END))))

;;;###autoload
(defun abz-indent-buffer ()
  "Indent the current buffer using the best tool available."
  (interactive)
  (cond ((abz--lsp-format?)
         (call-interactively #'lsp-format-buffer))
        ((abz--lsp-clang-format?)
         (call-interactively #'clang-format-buffer))
        ((fboundp 'indent-region)
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

(defun abz--straight-upgrade-all ()
  "Upgrade all packages with `straight'."
  (let ((debug-on-error t)
        (buffer "*Messages*"))
    (message "Upgrade all packages..")
    (pop-to-buffer-same-window buffer)
    (redisplay)
    (message "Normalize packages..")
    (redisplay)
    (straight-normalize-all)
    (message "Normalize packages... Done.")
    (message "Pull packages..")
    (redisplay)
    (straight-pull-all)
    (message "Pull packages... Done.")
    (message "Normalize packages..")
    (redisplay)
    (straight-normalize-all)
    (message "Normalize packages... Done.")
    (message "Build packages..")
    (redisplay)
    (straight-rebuild-all)
    (message "Build packages... Done")
    (message "Upgrade all packages... Done.")
    (redisplay)
    (princ (buffer-string) #'external-debugging-output)
    (kill-emacs)))

(defun abz--straight-upgrade-all-async-sentinel (PROCESS EVENT)
  "Sentinel function for `abz--straight-upgrade-all'.

Asks to restart Emacs when `PROCESS' emits the event `EVENT'."
  (when (y-or-n-p (format "Process %s %s.  Restart Emacs?" PROCESS EVENT))
    (use-package restart-emacs
      :functions restart-emacs
      :custom
      (restart-emacs-restore-frames t))
    (restart-emacs)))

(defun abz--straight-upgrade-all-async ()
  "Upgrade all packages in another process."
  (make-process
   :name "abz-upgrade"
   :buffer "*abz-upgrade*"
   :command `(,(expand-file-name invocation-name invocation-directory)
              "-u"
              ,(or (and (not (string-empty-p init-file-user)) init-file-user) (user-login-name))
              "-f"
              "abz--straight-upgrade-all")
   :sentinel #'abz--straight-upgrade-all-async-sentinel))

;;;###autoload
(defun abz-straight-upgrade-all ()
  "Upgrade all packages."
  (interactive)
  (abz--straight-upgrade-all-async))

(provide 'abz)

;;; abz.el ends here
