;;; init.el --- Pluc's GNU Emacs Initialization File -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2020 PERRIER Pierre-Luc <dev@the-pluc.net>

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

(require 'map)
(require 'xdg)

;; Backward compatibility
(unless (version<= "27" emacs-version)
  (load (expand-file-name "early-init" user-emacs-directory) nil 'nomessage))

;; straight.el: package manager
;; https://github.com/radian-software/straight.el

;; Shallow clone
(customize-set-variable 'straight-vc-git-default-clone-depth 1)
;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
;; https://github.com/radian-software/radian/blob/6a9f84ea50fe12f4655265b047c3ac72d467fa87/emacs/radian.el#L550
(customize-set-variable 'straight-check-for-modifications
                        (if (and (executable-find "watchexec")
                                 (executable-find "python3"))
                            '(watch-files find-when-checking)
                          '(find-at-startup find-when-checking)))

;; Bootstrap
(defvar bootstrap-version)
(defconst straight-base-dir (expand-file-name (convert-standard-filename "emacs/")
                                              (xdg-cache-home)))
(let ((bootstrap-file (expand-file-name
                       (convert-standard-filename "straight/repos/straight.el/bootstrap.el")
                       straight-base-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package: package loading
;; https://github.com/jwiegley/use-package
(declare-function straight-use-package "straight") ; Silence byte-compiler
(straight-use-package 'use-package)
(customize-set-variable 'straight-use-package-by-default t "Install packages by default in `use-package` forms")
(customize-set-variable 'use-package-always-defer t "Use deferred loading by default")
(customize-set-variable 'use-package-always-demand nil "Inhibit deferred loading by default")
(customize-set-variable 'use-package-expand-minimally nil "Make the expanded code as minimal as possible")
(customize-set-variable 'use-package-verbose t "Report about loading and configuration details")
(customize-set-variable 'use-package-compute-statistics t "Report about loading and configuration details")
(require 'use-package) ; Silence byte-compiler

;; Ensure environment variables inside Emacs look the same as in the user's shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :demand
  :when (or (memq window-system '(mac ns x))
            (daemonp))
  :commands
  exec-path-from-shell-copy-env
  exec-path-from-shell-initialize
  :config
  (exec-path-from-shell-copy-env "CPM_SOURCE_CACHE")
  (exec-path-from-shell-initialize))

;; Adds the keyword `:ensure-system-package' to `use-package'
(use-package use-package-ensure-system-package
  :demand t)

;; Customize mode lighters. use-package integration with `:diminish`.
(use-package diminish :demand t)
;; Macros to define key bindings. use-package integration with `:bind`.
(use-package bind-key :demand t)

;; Tweak garbage collector
(defconst gc-threshold (* 4 1000 1000))
(defconst gc-percentage 0.5)
(use-package gcmh
  :demand t
  :commands gcmh-mode
  :preface
  (defun abz--restore-gc-parameters ()
    "Restore garbage collector thresholds to sane values."
    (customize-set-variable 'gc-cons-threshold gc-threshold)
    (customize-set-variable 'gc-cons-percentage gc-percentage)
    (gcmh-mode 1))
  (defun abz--restore-gc-parameters-when-idle ()
    "Call `abz--restore-gc-parameters' when Emacs becomes idle."
    (run-with-idle-timer 9 nil #'abz--restore-gc-parameters))
  :custom
  (gcmh-verbose nil)
  (gcmh-low-cons-threshold gc-threshold)
  :diminish gcmh-mode
  :hook (emacs-startup . abz--restore-gc-parameters-when-idle))

;; Some non-packaged stuff
(add-to-list 'load-path (locate-user-emacs-file "vendor"))

;; Directory where to find submodules
(eval-and-compile (defconst abz-site-dir (locate-user-emacs-file "abz")
                    "Local packages directory"))
(add-to-list 'load-path abz-site-dir)

;; This emacs configuration variables
(require 'abz-settings)

;; tramp: Transparent Remote Access, Multiple Protocol (built-in)
;; no-littering:
;;   - tramp-auto-save-directory
;;   - tramp-persistency-file-name
(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
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
  (setenv "SHELL" "/bin/bash"))

;; Wraps package managers
(use-package system-packages
  :functions system-packages-install
  :config
  (require 'tramp)
  (when (executable-find "paru")
    (add-to-list 'system-packages-supported-package-managers
                 '(paru . ((default-sudo . nil)
                           (install . "paru -S")
                           (search . "paru -Ss")
                           (uninstall . "paru -Rnsv")
                           (update . "paru")
                           (clean-cache . "paru -Sc")
                           (change-log . "paru -Qc")
                           (log . "cat /var/log/paru.log")
                           (get-info . "paru -Qi")
                           (get-info-remote . "paru -Si")
                           (list-files-provided-by . "paru -qQl")
                           (owning-file . "paru -Qo")
                           (owning-file-remote . "paru -F")
                           (verify-all-packages . "paru -Qkk")
                           (verify-all-dependencies . "paru -Dk")
                           (remove-orphaned . "paru -Rnsuv $(paru -Qtdq)")
                           (list-installed-packages . "paru -Qe")
                           (list-installed-packages-all . "paru -Q")
                           (list-dependencies-of . "paru -Qi")
                           (noconfirm . "--noconfirm"))))
    (setq system-packages-package-manager 'paru)
    (setq system-packages-use-sudo nil)))

;; Install some useful system packages
;;
;; There is no way (that I found) to evaluate an expression within the
;; `ensure-system-package' keyword of `use-package' i.e to discriminate by OS so
;; it's easier just to list them all here.
;;
;; TODO We could collect all missing packages and make a single call to system-packages-install
(defun abz--ensure-system-package (target package)
  "If `TARGET' does not exists, install `PACKAGE'.

`TARGET' can be either a symbol or a string.

The package is installed with `system-packages-install'.

TODO: Accept a list of packages."
  (message "Check target '%s' for package '%s'" target package)
  (cond
   ((or (null target) (null package)) (message "nullp"))
   ((and (stringp target) (file-exists-p target)) (message "file exist"))
   ((and (symbolp target) (executable-find (symbol-name target))) (message "executable exist"))
   ((system-packages-install (symbol-name package)))))

(let ((packages `(;; Fonts
                  ,(cond
                    ((abz-os-is-arch?) #'("/usr/share/licenses/ttf-iosevka/" . ttf-iosevka))
                    (t #'(nil . nil)))
                  ,(cond
                    ((abz-os-is-arch?) #'("/usr/share/fonts/TTF/FiraCode-Regular.ttf" . ttf-fira-code))
                    ((abz-os-is-debian-derivative?) #'("/usr/share/fonts-firacode/" . fonts-firacode)))
                  ,(cond
                    ((abz-os-is-arch?) #'("/usr/share/fonts/TTF/Hack-Regular.ttf" . ttf-hack))
                    ((abz-os-is-debian-derivative?) #'("/usr/share/fonts/truetype/hack/" . fonts-hack)))
                  ;; openssh: secure shell client
                  ,(cond
                    ((abz-os-is-arch?) #'(ssh . openssh))
                    ((abz-os-is-debian-derivative?) #'(ssh . openssh-client)))
                  ;; pass: password manager
                  (pass . pass)
                  ;; ripgrep: grep alternative
                  (rg . ripgrep)
                  ;; ag: grep alternative
                  ,(cond
                    ((abz-os-is-arch?) #'(ag . the_silver_searcher))
                    ((abz-os-is-debian-derivative?) #'(ag . silversearcher-ag)))
                  ;; fd: find alternative
                  ,(cond
                    ((abz-os-is-debian-derivative?) #'(fdfind . fd-find))
                    (#'(fd . fd)))
                  )))
  (map-do #'abz--ensure-system-package packages))

;; Basic setup
(require 'abz-custom)
(abz--init-custom)

;; Color theme (only zenburn ATM)
(require 'abz-theme)

;; Completion framework
(require 'abz-completion)

;; Common edition settings
(require 'abz-editing)

;; Development settings
(require 'abz-devel)

;; External tools integration
(require 'abz-tools)

;; Debug init file
(use-package bug-hunter)

;; Profile emacs startup
(use-package esup
  :custom
  ;; https://github.com/jschaf/esup/issues/54
  (esup-depth 0))

(bind-key* "C-c S" 'align-comments)
(bind-key* "C-x C-r" 'toggle-sudo)
(bind-key* "C-c b r" 'revert-buffer)

;; Shorten long file-name targets. https://github.com/lewang/scf-mode
(use-package scf-mode
  :commands scf-mode
  :hook (compilation-mode . (lambda () (scf-mode t))))

;; keys
(global-set-key [(control c) (x)] 'compile)
(global-set-key [(control x) (control k)] 'kill-some-buffers)

(defun command-line-diff ()
  "Usage: Emacs -diff file1 file2."
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (diff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
(use-package ansi-color
  :straight nil
  :init
  (defun abz--colorize-region (start end)
    "Colorize from `start` to `end`"
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region start end)))
  (defun abz--colorize-buffer ()
    "Colorize the current buffer"
    (interactive)
    (abz--colorize-region (point-min) (point-max)))
  (use-package compile
    :straight nil
    :custom
    (compilation-environment '("TERM=xterm")))
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package keyfreq
  :hook ((after-init . keyfreq-mode)
         (after-init . keyfreq-autosave-mode)))

;; Required by package.el
;; (package-initialize)

(provide 'init)

;;; init.el ends here
