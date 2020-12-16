;;; abz-diagnostics.el --- Syntax checkers -*- lexical-binding: t; -*-

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

;;;###autoload
(defcustom abz-diagnostics-backend :flycheck
  "The syntax checker backend."
  :type '(choice
          (const :tag "Flycheck" :flycheck)
          (const :tag "Flymake (built-in)" :flymake)
          (const :tag "Disable syntax checking" :none))
  :group 'abz
  :group 'tools
  :group 'flycheck)

(cond ((eq abz-diagnostics-backend :flycheck)
       (require 'abz-flycheck))
      ((eq abz-diagnostics-backend :flymake)
       (use-package flymake
         :straight nil
         :hook (prog-mode . flymake-mode)))
      (eq abz-diagnostics-backend :none)
      (nil))

(provide 'abz-diagnostics)

;;; abz-diagnostics.el ends here
