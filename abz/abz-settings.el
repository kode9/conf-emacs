;;; abz-settings.el --- Config customization -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'files)
(require 'subr-x)
(require 'use-package)
(require 'xdg)

(defgroup abz nil
  "Custom group."
  :prefix "abz-"
  :group 'emacs
  :link '(url-link :tag "Homepage" "https://git.the-pluc.net/conf-emacs.git/"))

(defun abz--make-set-custom-dir (&optional oldvalue)
  "Create a function to be used to set a `defcustom' directory variable.

The function takes a `SYMBOL' and a `VALUE' as arguments and set `SYMBOL' to
`VALUE' and call `make-directory' on `VALUE'.

If optional `OLDVALUE' is non-nil it will first try to rename `OLDVALUE' to
`VALUE'."
  (lambda (symbol value)
    (when (and (stringp oldvalue)
               (not (or (string-empty-p oldvalue)
                        (file-equal-p oldvalue value)))
               (file-exists-p oldvalue))
      (if (file-exists-p value)
          (warn "Both %s and %s exist" oldvalue value)
        (let ((filename (directory-file-name value)))
          (make-directory (file-name-directory filename) t)
          (rename-file oldvalue filename))))
    (set-default symbol value)
    (make-directory value t)))

(defcustom abz-config-local-dir
  (abbreviate-file-name (expand-file-name "local/" user-emacs-directory))
  "Directory used to store local configuration files."
  :type 'directory
  :group 'abz
  :group 'environment
  :set (abz--make-set-custom-dir (expand-file-name "local/" user-emacs-directory)))

(defcustom abz-data-dir
  (abbreviate-file-name (expand-file-name "emacs/" (xdg-data-home)))
  "Directory used to store data files."
  :type 'directory
  :group 'abz
  :group 'environment
  :set (abz--make-set-custom-dir (expand-file-name ".cache/" user-emacs-directory)))

(defcustom abz-cache-dir
  (abbreviate-file-name (expand-file-name "emacs/" (xdg-cache-home)))
  "Directory used to store runtime cache files."
  :type 'directory
  :group 'abz
  :group 'environment
  :set (abz--make-set-custom-dir))

(defun abz--locate-config-file (filename)
  "Return an absolute path to `FILENAME' under `abz-config-local-dir'."
  (make-directory abz-config-local-dir t)
  (expand-file-name (convert-standard-filename filename)
                    abz-config-local-dir))

(defun abz--locate-data-dir (dirname)
  "Make sure `DIRNAME' exists under `abz-data-dir' and return its absolute path."
  (let ((dir (expand-file-name (convert-standard-filename dirname)
                               abz-data-dir)))
    (make-directory dir t)
    dir))

(defun abz--locate-cache-dir (dirname)
  "Make sure `DIRNAME' exists under `abz-cache-dir' and return its absolute path."
  (let ((dir (expand-file-name (convert-standard-filename dirname)
                               abz-cache-dir)))
    (make-directory dir t)
    dir))

(defcustom abz-custom-file
  (abz--locate-config-file "custom.el")
  "File used to store settings from Customization UI."
  :set-after '(abz-config-local-dir)
  :type 'file
  :group 'abz
  :group 'environment)

(defun abz-find-custom-file()
  "Open the custom file."
  (interactive)
  (find-file abz-custom-file))

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory abz-config-local-dir)
  (setq no-littering-var-directory abz-data-dir))

(defconst abz--os-is-arch?
  (and (executable-find "pacman") t))

(defun abz-os-is-arch? ()
  "Return t if the current OS is Arch Linux."
  abz--os-is-arch?)

(defconst abz--os-is-debian-derivative?
  (and (executable-find "apt") t))

(defun abz-os-is-debian-derivative? ()
  "Return t if the current OS is Debian or a derivative."
  abz--os-is-debian-derivative?)

;; Custom file location
(setq custom-file abz-custom-file)
(load custom-file 'no-error 'no-message)

(provide 'abz-settings)

;;; abz-settings.el ends here
