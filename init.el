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

;; Don't load expired byte-compiled files
(customize-set-variable 'load-prefer-newer t)

;; Cask: Automatic installation and updates of packages listed in a
;; Cask file. http://github.com/cask/cask
(eval-and-compile (require 'cask (expand-file-name "cask/cask.el" user-emacs-directory)))
(cask-initialize)

;; Pallet: Keep track of package installations in concordance with
;; Cask. https://github.com/rdallasgray/pallet
(require 'pallet)
(pallet-mode t)

;; use-package: simplify package loading, settings, bindings, and
;; more. https://github.com/jwiegley/use-package
(eval-when-compile
  (customize-set-variable 'use-package-verbose nil)        ; Report about loading and configuration details.
  (customize-set-variable 'use-package-debug nil)          ; Display expanded code
  (customize-set-variable 'use-package-expand-minimally t) ; Make the expanded code as minimal as possible
  (require 'use-package))

(use-package diminish) ; :diminish support for use-package
(use-package bind-key) ; :bind support for use-package

;; Some non-packaged stuff
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;; Directory where to find submodules
(eval-and-compile (defconst pluc-site-dir (expand-file-name "pluc" user-emacs-directory)
                    "Local packages directory"))
(add-to-list 'load-path pluc-site-dir)

(use-package pluc-settings   :ensure nil) ;
(use-package pluc-theme      :ensure nil) ; Color theme (only zenburn ATM)
(use-package pluc-custom     :ensure nil) ; Basic setup
(use-package pluc-completion :ensure nil) ; Completion framework
(use-package pluc-editing    :ensure nil) ; Common edition settings
(use-package pluc-devel      :ensure nil) ; Development settings
(use-package pluc-tools      :ensure nil) ; External tools integration

;; Debug init file
(use-package bug-hunter
  :defer t)

;; Profile emacs startup
(use-package esup
  :defer t)

(bind-key* "C-c S" 'align-comments)
(bind-key* "C-x C-r" 'toggle-sudo)

;; Shorten long file-name targets. https://github.com/lewang/scf-mode
(autoload 'scf-mode "scf-mode" "SCF Mode" t)
(add-hook 'compilation-mode-hook (lambda () (scf-mode t)))

(diminish 'auto-revert-mode)

;; Custom hooks
(defun dtw()
  "Delete trailing whitespaces"
  (interactive)
  (delete-trailing-whitespace))

(defun indent-buffer()
  "Indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun untab-buffer()
  "Transform tabs to spaces in the whole buffer"
  (interactive)
  (untabify (point-min) (point-max) nil))

(defun dev-hooks()
  "Progmod hooks"
  (format-buffer))

(define-minor-mode pluc-mode
  "Clean buffers."
  :lighter " pluc"
  :global t
  (if pluc-mode
      (progn
        (add-hook 'before-save-hook 'dev-hooks nil t))
    (remove-hook 'before-save-hook 'dev-hooks t)))

;; Clean for any files
(add-hook 'before-save-hook 'dtw)
(add-hook 'prog-mode-hook #'pluc-mode)

;; keys
(global-set-key [(control c) (c)] 'comment-or-uncomment-region)
;; (global-set-key [(control c) (v)] 'uncomment-region)
(global-set-key [(control c) (x)] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control x) (control k)] 'kill-some-buffers)

;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (diff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
(use-package ansi-color
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
  (add-hook 'compilation-filter-hook #'pluc/colorize-compilation)
  (customize-set-variable 'compilation-environment #'("TERM=xterm")))

;; Required by package.el
;; (package-initialize)

(provide 'init)
;;; init.el ends here
