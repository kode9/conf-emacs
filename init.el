;;; init.el --- Pluc's GNU Emacs Initialization File -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2018  Pierre-Luc Perrier

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

;; Raise garbage collector thresholds for initialization to improve startup
;; time. They will be restored after initialization.
;; https://emacs.stackexchange.com/a/34367
(customize-set-variable 'gc-cons-threshold (* 1000 1000 1000))
(customize-set-variable 'gc-cons-percentage 90)

(defun abz--restore-gc-parameters ()
  "Restore garbage collector thresholds to sane values."
  (customize-set-variable 'gc-cons-threshold (* 4 1000 1000))
  (customize-set-variable 'gc-cons-percentage 0.5))

(defun abz--restore-gc-parameters-when-idle ()
  "Call `abz--restore-gc-parameters' when Emacs becomes idle."
  (run-with-idle-timer 61 nil #'abz--restore-gc-parameters))

;; Launch idle timer to restore GC after startup
(add-hook 'emacs-startup-hook #'abz--restore-gc-parameters-when-idle)

;; Don't load expired byte-compiled files
(customize-set-variable 'load-prefer-newer t)

;; Bootstrap straight.el (https://github.com/raxod502/straight.el)

;; Use radox502's mirror of GNU ELPA (https://github.com/emacs-straight)
(customize-set-variable 'straight-recipes-gnu-elpa-use-mirror t)

(defvar bootstrap-version)

(eval-and-compile (let ((bootstrap-file
                         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
                        (bootstrap-version 5))
                    (unless (file-exists-p bootstrap-file)
                      (with-current-buffer
                          (url-retrieve-synchronously
                           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                           'silent 'inhibit-cookies)
                        (goto-char (point-max))
                        (eval-print-last-sexp)))
                    (load bootstrap-file nil 'nomessage)))

;; use-package: simplify package loading, settings, bindings, and more. https://github.com/jwiegley/use-package
(eval-when-compile (straight-use-package 'use-package))
(customize-set-variable 'straight-use-package-by-default t) ; Install packages by default in `use-package` forms
(customize-set-variable 'use-package-always-defer nil)      ; Use deferred loading by default
(customize-set-variable 'use-package-always-demand nil)     ; Inhibit deferred loading by default
(customize-set-variable 'use-package-expand-minimally nil)  ; Make the expanded code as minimal as possible
(customize-set-variable 'use-package-verbose nil)           ; Report about loading and configuration details

;; Customize mode lighters. use-package integration with `:diminish`.
(use-package diminish)
;; Macros to define key bindings. use-package integration with `:bind`.
(use-package bind-key)

;; Some non-packaged stuff
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;; Directory where to find submodules
(eval-and-compile (defconst pluc-site-dir (expand-file-name "pluc" user-emacs-directory)
                    "Local packages directory"))
(add-to-list 'load-path pluc-site-dir)

;; This emacs configuration variables
(use-package pluc-settings
  :demand t
  :straight nil
  :config
  (abz--init-settings))

;; Basic setup
(use-package pluc-custom
  :demand t
  :straight nil
  :config
  (abz--init-custom))

;; Color theme (only zenburn ATM)
(use-package pluc-theme
  :demand t
  :straight nil)

;; Completion framework
(use-package pluc-completion
  :demand t
  :straight nil)

;; Common edition settings
(use-package pluc-editing
  :demand t
  :straight nil)

;; Development settings
(use-package pluc-devel
  :demand t
  :straight nil)

;; External tools integration
(use-package pluc-tools
  :demand t
  :straight nil)

;; Debug init file
(use-package bug-hunter
  :defer t)

;; Profile emacs startup
(use-package esup
  :defer t)

(bind-key* "C-c S" 'align-comments)
(bind-key* "C-x C-r" 'toggle-sudo)

;; Shorten long file-name targets. https://github.com/lewang/scf-mode
(use-package scf-mode
  :hook (compilation-mode . (lambda () (scf-mode t))))

;; keys
(global-set-key [(control c) (c)] 'comment-or-uncomment-region)
;; (global-set-key [(control c) (v)] 'uncomment-region)
(global-set-key [(control c) (x)] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control x) (control k)] 'kill-some-buffers)

(defun command-line-diff ()
  "Usage: Emacs -diff file1 file2."
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (diff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
(use-package ansi-color
  :disabled t
  :commands (ansi-color-apply-on-region pluc/colorize-region)
  :init
  (defun pluc/colorize-region (start end)
    "Colorize from `start` to `end`"
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region start end)))
  (defun pluc-colorize-buffer ()
    "Colorize the current buffer"
    (interactive)
    (pluc/colorize-region (point-min) (point-max)))
  (defun pluc/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (pluc/colorize-region compilation-filter-start (point)))
  (customize-set-variable 'compilation-environment #'("TERM=xterm"))
  :hook (compilation-filter . pluc/colorize-compilation))

;; Required by package.el
;; (package-initialize)

(provide 'init)

;;; init.el ends here
