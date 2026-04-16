;;; abz-consult.el --- Consult integration -*- lexical-binding: t; -*-

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

;; Search, navigation, and completion enhancements via consult.
;; Loaded conditionally when any defcustom in abz-settings.el selects
;; a consult variant (see `abz--consult-needed-p').

;;; Code:

(require 'abz-settings)
(require 'use-package)

;; Consulting completing-read
;; https://github.com/minad/consult
(use-package consult
  :demand t
  :custom
  (consult-narrow-key "<" "Key for narrowing during consult commands")
  (consult-preview-key 'any "Show preview for any key press")
  :config
  ;; Use projectile for project root detection
  (when (fboundp 'projectile-project-root)
    (setq consult-project-function
          (lambda (_may-prompt) (projectile-project-root))))
  :bind
  (;; goto-map (M-g) — navigation commands
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("M-g b" . consult-bookmark)
   ("M-g M-m" . consult-man)
   ("M-g M-i" . consult-info)
   ("M-g r" . consult-register)
   ("M-g k" . consult-kmacro)
   ;; search-map (M-s) — secondary access
   ("M-s l" . consult-line)
   ("M-s r" . consult-ripgrep)
   ("M-s g" . consult-grep)
   ("M-s f" . consult-find)
   ("M-s G" . consult-git-grep)
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-r" . consult-history)
   ("M-s" . consult-history)))

;; Consult + embark integration
;; https://github.com/oantolin/embark
(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult interface for flycheck errors
;; https://github.com/minad/consult-flycheck
(use-package consult-flycheck
  :when (eq abz-diagnostics-list 'consult)
  :after (consult flycheck))

;; Consult xref integration
(when (eq abz-xref-show 'consult)
  (customize-set-variable 'xref-show-xrefs-function #'consult-xref)
  (customize-set-variable 'xref-show-definitions-function #'consult-xref))

;; Consult imenu
(when (eq abz-imenu 'consult)
  (bind-key "M-g i" #'consult-imenu))

;; Consult compile-error
(when (eq abz-compile-navigation 'consult)
  (bind-key "M-g e" #'consult-compile-error))

(provide 'abz-consult)

;;; abz-consult.el ends here
