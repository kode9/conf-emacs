;;; abz-completion.el --- Completion framework -*- lexical-binding: t; -*-

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

;; https://github.com/raxod502/selectrum/issues/274

;;; Code:

(require 'use-package)

;;;###autoload
(defcustom abz-completion-framework 'ivy
  "The completion frontend to use."
  :type '(radio
          (const :tag "Built-in" built-in)
          (const :tag "Vertico" vertico)
          (const :tag "Ivy" ivy))
  :tag "Completion framework"
  :group 'abz
  :group 'convenience)

(cl-case abz-completion-framework
  (ivy (require 'abz-ivy))
  (vertico (require 'abz-vertico)))

(provide 'abz-completion)

;;; abz-completion.el ends here
