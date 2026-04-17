;;; abz-remote.el --- Remote work configuration -*- lexical-binding: t; -*-

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

;; Centralized configuration for remote development over TRAMP and
;; display proxies (XPRA, waypipe).

;;; Code:

(require 'abz-settings)
(require 'use-package)

;;;; Customization

(defcustom abz-remote-tramp-shell "/usr/bin/dash"
  "Preferred shell for TRAMP internal commands on remote hosts.
Dash is POSIX-minimal with lower overhead than bash. Used for
TRAMP's own stat/test/cat operations, not for interactive shells."
  :type 'string
  :group 'abz)

(defcustom abz-remote-tramp-shell-fallback "/bin/sh"
  "Fallback shell if `abz-remote-tramp-shell' is not available."
  :type 'string
  :group 'abz)

(defcustom abz-remote-tramp-magit-lightweight t
  "When non-nil, reduce magit sections and features over TRAMP.
Drops expensive status sections (unpulled, unpushed, bisect, etc.)
and headers (tags, modules, etc.) to minimize SSH roundtrips."
  :type 'boolean
  :group 'abz)

(defcustom abz-remote-display-method 'xpra
  "Preferred display proxy for remote Emacs GUI sessions."
  :type '(radio
          (const :tag "XPRA (persistent sessions, X11/Wayland)" xpra)
          (const :tag "Waypipe (lightweight, Wayland-native)" waypipe))
  :group 'abz)

(defcustom abz-remote-daemon-name "work"
  "Default Emacs daemon socket name for remote sessions."
  :type 'string
  :group 'abz)

;;;; TRAMP core tuning

;; tramp: Transparent Remote Access, Multiple Protocol (built-in)
;; no-littering:
;;   - tramp-auto-save-directory
;;   - tramp-persistency-file-name
(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 1 "Only show errors (default 3). Reduces format overhead.")
  (tramp-completion-reread-directory-timeout 120
   "Seconds before re-reading directory during completion (default nil).")
  (tramp-backup-directory-alist
   `(("." . ,(abz--locate-data-dir "backup/tramp")))
   "Backup files location")
  :init
  ;; Disable version control for tramp files
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (use-package vc
    :straight nil
    :init
    (customize-set-variable 'vc-ignore-dir-regexp
                            (format "\\(%s\\)\\|\\(%s\\)"
                                    vc-ignore-dir-regexp
                                    tramp-file-name-regexp)))
  :config
  ;; Prevent remote programs from inheriting local fish shell
  (setenv "SHELL" "/bin/bash")
  ;; Reuse existing ControlMaster sockets (Emacs 30+)
  (when (boundp 'tramp-use-connection-share)
    (customize-set-variable 'tramp-use-connection-share t))
  ;; Cache remote file attributes longer (default 10s)
  (customize-set-variable 'remote-file-name-inhibit-cache 60))

(provide 'abz-remote)

;;; abz-remote.el ends here
