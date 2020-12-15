;;; abz-completion.el --- Completion framework -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Pierre-Luc Perrier

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

;;;###autoload
(defcustom abz-completion-framework 'ivy
  "Completion framework to use."
  :type '(radio (const :tag "Ido" ido)
                (const :tag "Ivy" ivy)
                (const :tag "Helm" helm))
  :tag "Completion framework"
  :group 'abz
  :group 'convenience)

(use-package abz-ido
  :if (eql abz-completion-framework 'ido)
  :demand t
  :straight nil)

(use-package abz-ivy
  :if (eql abz-completion-framework 'ivy)
  :demand t
  :straight nil)

(provide 'abz-completion)

;;; abz-completion.el ends here
