;;; abz-theme.el --- Fonts & Faces -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Pierre-Luc Perrier

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
(require 'abz-settings)

;;;###autoload
(defcustom abz-font-default-size 10
  "Default font size."
  :tag "Default font size"
  :type 'integer
  :group 'abz
  :group 'font-selection)

(use-package zenburn-theme
  :demand t)

;; Fonts
(use-package dynamic-fonts
  :functions dynamic-fonts-setup
  :init
  (customize-set-variable 'dynamic-fonts-set-alternatives t) ; Fill up alternative fonts
  (customize-set-variable 'dynamic-fonts-preferred-monospace-point-size abz-font-default-size)
  (customize-set-variable 'dynamic-fonts-preferred-monospace-fonts
                          '("Iosevka"
                            "Monoisome"
                            "Monoid"
                            "Hack"
                            "Dejavu Sans Mono"
                            "courier"
                            "fixed"))
  (customize-set-variable 'dynamic-fonts-preferred-proportional-fonts
                          '("Open Sans"
                            "Roboto"
                            "Dejavu Sans"
                            "arial"
                            "fixed"))
  (customize-set-variable 'dynamic-fonts-preferred-proportional-point-size abz-font-default-size)
  :hook (after-init . dynamic-fonts-setup))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package spaceline-config
  :disabled t
  :straight spaceline
  :functions spaceline-emacs-theme
  :hook (after-init . spaceline-emacs-theme))

(use-package doom-modeline
  :init
  (customize-set-variable 'doom-modeline-height 30)
  (customize-set-variable 'doom-modeline-bar-width 2)
  (customize-set-variable 'doom-modeline-project-detection 'projectile)
  (customize-set-variable 'doom-modeline-buffer-file-name-style 'relative-from-project)
  (customize-set-variable 'doom-modeline-icon (display-graphic-p))
  (customize-set-variable 'doom-modeline-buffer-state-icon t)
  (customize-set-variable 'doom-modeline-buffer-modification-icon t)
  (customize-set-variable 'doom-modeline-unicode-fallback t)
  (customize-set-variable 'doom-modeline-minor-modes t)
  (customize-set-variable 'doom-modeline-enable-word-count nil)
  (customize-set-variable 'doom-modeline-buffer-encoding nil)
  (customize-set-variable 'doom-modeline-indent-info nil)
  (customize-set-variable 'doom-modeline-checker-simple-format nil)
  :hook (after-init . doom-modeline-mode))

(use-package display-line-numbers
  :straight nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package guru-mode
  :diminish
  :init
  (setq guru-warn-only t)
  :hook (prog-mode . guru-mode))

(provide 'abz-theme)

;;; abz-theme.el ends here
