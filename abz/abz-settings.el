;;; abz-settings.el --- Config customization -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Pierre-Luc Perrier

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

(require 'use-package)

(defgroup abz nil
  "Custom group."
  :prefix "abz-"
  :group 'emacs
  :link '(url-link :tag "Homepage" "https://git.the-pluc.net/conf-emacs.git/"))

;;;###autoload
(defcustom abz-local-dir (expand-file-name "local" user-emacs-directory)
  "Directory used to store local configuration files."
  :type 'directory
  :tag "Local configuration directory"
  :group 'abz
  :group 'environment)

;;;###autoload
(defcustom abz-cache-dir (expand-file-name ".cache" user-emacs-directory)
  "Directory used to store runtime cache files."
  :type 'directory
  :tag "Local cache directory"
  :group 'abz
  :group 'environment)

;;;###autoload
(defcustom abz-custom-file (expand-file-name "custom.el" abz-local-dir)
  "File used to store settings from Customization UI."
  :set-after '(abz-local-dir)
  :tag "Custom file location"
  :type 'file
  :group 'abz
  :group 'environment)

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory abz-local-dir)
  (setq no-littering-var-directory abz-cache-dir))

;;;###autoload
(defun abz--init-settings ()
  "Initialise settings."
  (ignore-errors (make-directory abz-cache-dir))
  (setq custom-file abz-custom-file)
  (load custom-file 'no-error 'no-message))

(defconst abz--os-is-arch?
  (and (executable-find "pacman") t))

;;;###autoload
(defun abz-os-is-arch? ()
  "Return t if the current OS is Arch Linux."
  abz--os-is-arch?)

(defconst abz--os-is-debian-derivative?
  (and (executable-find "apt") t))

;;;###autoload
(defun abz-os-is-debian-derivative? ()
  "Return t if the current OS is Debian or a derivative."
  abz--os-is-debian-derivative?)

(provide 'abz-settings)

;;; abz-settings.el ends here
