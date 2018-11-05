;;; pluc-completion.el --- Completion framework -*- lexical-binding: t; -*-

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

;;;###autoload
(defcustom pluc-completion-framework 'ido
  "Completion framework to use."
  :type '(radio (const :tag "Ido" ido)
                (const :tag "Ivy" ivy)
                (const :tag "Helm" helm))
  :tag "Completion framework"
  :group 'abz
  :group 'convenience)

(use-package pluc-ido
  :if (eql pluc-completion-framework 'ido)
  :demand t
  :straight nil)

(provide 'pluc-completion)
;;; pluc-completion.el ends here
