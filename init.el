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

(unless (version<= "27" emacs-version)
  (load (expand-file-name "early-init" user-emacs-directory) nil 'nomessage))

;; use-package: simplify package loading, settings, bindings, and more. https://github.com/jwiegley/use-package
(eval-when-compile (straight-use-package 'use-package))
(customize-set-variable 'straight-use-package-by-default t) ; Install packages by default in `use-package` forms
(customize-set-variable 'use-package-always-defer t)        ; Use deferred loading by default
(customize-set-variable 'use-package-always-demand nil)     ; Inhibit deferred loading by default
(customize-set-variable 'use-package-expand-minimally nil)  ; Make the expanded code as minimal as possible
(customize-set-variable 'use-package-verbose t)             ; Report about loading and configuration details
(customize-set-variable 'use-package-compute-statistics t)  ; Report about loading and configuration details

;; Ensure environment variables inside Emacs look the same as in the user's shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :demand
  :when (or (memq window-system '(mac ns x))
            (daemonp))
  :config
  (exec-path-from-shell-copy-env "CPM_SOURCE_CACHE")
  (exec-path-from-shell-initialize))

;; Customize mode lighters. use-package integration with `:diminish`.
(use-package diminish :demand t)
;; Macros to define key bindings. use-package integration with `:bind`.
(use-package bind-key :demand t)

;; Tweak garbage collector
(use-package gcmh
  :demand t
  :init
  (let ((gc-threshold (* 4 1000 1000))
        (gc-percentage 0.5))
    (customize-set-variable 'gcmh-verbose nil)
    (customize-set-variable 'gcmh-low-cons-threshold gc-threshold)
    (defun abz--restore-gc-parameters ()
      "Restore garbage collector thresholds to sane values."
      (message "abz: Restore GC parameters")
      (customize-set-variable 'gc-cons-threshold gc-threshold)
      (customize-set-variable 'gc-cons-percentage gc-percentage)
      (gcmh-mode 1))
    (defun abz--restore-gc-parameters-when-idle ()
      "Call `abz--restore-gc-parameters' when Emacs becomes idle."
      (run-with-idle-timer 61 nil #'abz--restore-gc-parameters)))
  :diminish gcmh-mode
  :hook (emacs-startup . abz--restore-gc-parameters-when-idle))

;; Some non-packaged stuff
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;; Directory where to find submodules
(eval-and-compile (defconst abz-site-dir (expand-file-name "abz" user-emacs-directory)
                    "Local packages directory"))
(add-to-list 'load-path abz-site-dir)

;; This emacs configuration variables
(require 'abz-settings)
(abz--init-settings)

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
  :disabled
  :straight nil
  :commands (ansi-color-apply-on-region abz/colorize-region)
  :init
  (defun abz/colorize-region (start end)
    "Colorize from `start` to `end`"
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region start end)))
  (defun abz-colorize-buffer ()
    "Colorize the current buffer"
    (interactive)
    (abz/colorize-region (point-min) (point-max)))
  (defun abz/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (abz/colorize-region compilation-filter-start (point)))
  (customize-set-variable 'compilation-environment #'("TERM=xterm"))
  :hook (compilation-filter . abz/colorize-compilation))

(use-package keyfreq
  :hook ((after-init . keyfreq-mode)
         (after-init . keyfreq-autosave-mode)))

;; Required by package.el
;; (package-initialize)

(provide 'init)

;;; init.el ends here
