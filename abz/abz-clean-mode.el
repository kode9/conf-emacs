;;; abz-clean-mode.el --- Development minor mode -*- lexical-binding: t; -*-

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

(require 'abz)

(defvar abz--clean-mode-dont-indent-parent-modes
  '(fundamental-mode text-mode makefile-mode)
  "Don't indent major modes that derive from one of this modes.")

(defvar abz--clean-mode-dont-untabify-parent-modes
  '(makefile-mode)
  "Don't untabify major modes that derive from one of this modes.")

(defun abz--clean-buffer ()
  "Indent, untabify and remove trailing whispaces in current buffer."
  (deactivate-mark)
  (unless (apply #'derived-mode-p abz--clean-mode-dont-indent-parent-modes)
    (abz-indent-dwim))
  (unless (or (and (derived-mode-p #'c++-mode)
                   (fboundp #'projectile-project-root)
                   (file-readable-p (expand-file-name ".clang-format" (projectile-project-root))))
              (and (bound-and-true-p lsp-mode)
                   (lsp-feature? "textDocument/formatting")))
    (unless (or (bound-and-true-p indent-tabs-mode)
                (apply #'derived-mode-p abz--clean-mode-dont-untabify-parent-modes))
      (abz-untabify-dwim))
    (abz-delete-trailing-whitespace)))

(define-minor-mode abz-clean-mode
  "Cleans buffer before saving.

When called interactively, toggles ‘abz-clean-mode’.  With prefix ARG, enables
‘abz-clean-mode’ if ARG is positive, and disables it otherwise.it.

When called from Lisp, toggles ‘abz-clean-mode’ if ARG is 'toggle', disables it
if ARG is zero or negative, and enables it otherwise."
  :group 'abz
  :group 'editing
  :lighter " ABZ"
  (if abz-clean-mode
      (add-hook 'before-save-hook #'abz--clean-buffer 'append 'local)
    (remove-hook 'before-save-hook #'abz--clean-buffer 'local)))

(define-globalized-minor-mode abz-clean-global-mode abz-clean-mode
  abz-clean-mode)

(provide 'abz-clean-mode)

;;; abz-clean-mode.el ends here
