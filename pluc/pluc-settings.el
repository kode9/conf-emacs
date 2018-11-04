;;; pluc-settings.el --- Config customization -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Pierre-Luc Perrier

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
(defcustom pluc-local-dir (expand-file-name "local" user-emacs-directory)
  "Directory used to store local configuration files."
  :type 'directory
  :group 'pluc)

;;;###autoload
(defcustom pluc-cache-dir (expand-file-name ".cache" user-emacs-directory)
  "Directory used to store runtime cache files."
  :type 'directory
  :group 'pluc)

;;;###autoload
(defcustom pluc-custom-file (expand-file-name "custom.el" pluc-local-dir)
  "File used to store settings from Customization UI."
  :set-after '(pluc-cache-dir)
  :type 'file
  :group 'pluc)

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory pluc-local-dir)
  (setq no-littering-var-directory pluc-cache-dir))

;;;###autoload
(defun abz--init-settings ()
  "Initialise settings."
  (ignore-errors (make-directory pluc-cache-dir))
  (setq custom-file pluc-custom-file)
  (load custom-file 'no-error 'no-message))

(provide 'pluc-settings)
;;; pluc-settings.el ends here
