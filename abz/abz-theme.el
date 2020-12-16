;;; abz-theme.el --- Fonts & Faces -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 PERRIER Pierre-Luc <dev@the-pluc.net>

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

(require 'abz-settings)
(require 'hl-line)
(require 'use-package)

;;;###autoload
(defcustom abz-font-default-size 10
  "Default font size."
  :tag "Default font size"
  :type 'integer
  :group 'abz
  :group 'font-selection)

(use-package zenburn-theme
  :demand t
  :config (and (version<= "27" emacs-version) (enable-theme 'zenburn)))

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
  :disabled
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
  :init
  (customize-set-variable 'display-line-numbers-type t) ; Absolute number
  (customize-set-variable 'display-line-numbers-widen t) ; Disregard any narrowing
  (customize-set-variable 'display-line-numbers-width-start t) ; Compute necessary width
  (set-face-attribute 'line-number nil :weight 'semi-light)
  (set-face-attribute 'line-number-current-line nil :box nil :inverse-video t)
  :hook (prog-mode . display-line-numbers-mode))

(use-package guru-mode
  :diminish
  :init
  (setq guru-warn-only t)
  :hook (prog-mode . guru-mode))

;; Automatic symbol highlighting, navigation & edition
;; https://github.com/gennad/auto-highlight-symbol
;; Alternative, but slower: https://github.com/fgeller/highlight-thing.el
(use-package auto-highlight-symbol
  :diminish
  :init
  (advice-add 'ahs-highlight-p :before-while (lambda ()
                                               "Disable highlight-symbol-mode when region is active."
                                               (not (region-active-p))))
  :custom
  (ahs-idle-interval 0.1 "Idle delay before highlighting")
  (ahs-suppress-log t "Don't log")
  (ahs-face-check-include-overlay nil)
  :custom-face
  (ahs-face ((t . (
                   :foreground ,(face-attribute 'default :background)
                   :background ,(face-attribute 'font-lock-preprocessor-face :foreground)))))
  ;; TODO this is only good if hl-line is enabled, otherwise it should use the
  ;; default face. The best would be to change the highlight function to only
  ;; highlight the symbols not on the current point.
  (ahs-plugin-defalt-face ((t . (
                                 :inherit ,hl-line-face
                                 :foreground ,(face-attribute 'default :foreground)))))
  :hook
  (prog-mode . auto-highlight-symbol-mode))

(use-package dimmer
  :demand t
  :config
  (dimmer-mode t))

(provide 'abz-theme)

;;; abz-theme.el ends here
