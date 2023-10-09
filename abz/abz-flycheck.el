;;; abz-flycheck.el --- Flycheck syntax checker -*- lexical-binding: t; -*-

;; Copyright (C) PERRIER Pierre-Luc <dev@the-pluc.net>
;;
;; Author: PERRIER Pierre-Luc <dev@the-pluc.net>
;; Maintainer: PERRIER Pierre-Luc <dev@the-pluc.net>
;; Homepage: https://github.com/kode9
;; Keywords: tools, convenience
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;; Code:

(require 'use-package)

;; On the fly error checking
(use-package flycheck
  :diminish flycheck-mode
  :custom
  (flycheck-highlighting-mode 'sexps "Highlight whole expression around the error column")
  (flycheck-emacs-lisp-load-path 'inherit "Use the current session `load-path'")
  (flycheck-emacs-lisp-initialize-packages nil "Don't call `package-initialize'")
  :bind
  (("C-c f f" . flycheck-next-error)
   ("C-c f p" . flycheck-previous-error)
   ("C-c f l" . flycheck-list-errors)
   ("C-c f c" . flycheck-clear))
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

;; Show errors under point in pos-tip popups
(use-package flycheck-pos-tip
  :disabled
  :after flycheck
  :demand t
  :commands flycheck-pos-tip-mode
  :init
  (customize-set-variable 'flycheck-pos-tip-timeout 10)
  :config
  (flycheck-pos-tip-mode +1))

;; Flycheck integration
(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'abz-flycheck)

;;; abz-flycheck.el ends here
