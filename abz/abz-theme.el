;;; abz-theme.el --- Fonts & Faces -*- lexical-binding: t; -*-

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

(use-package jit-lock
  :straight nil
  :custom
  (jit-lock-chunk-size 3500)
  (jit-lock-defer-time 0)
  (jit-lock-stealth-time 8))

(use-package all-the-icons
  :if (display-graphic-p)
  :demand t
  :commands all-the-icons-install-fonts
  :config
  ;; If non-nil, don't compact font caches during GC
  ;; https://github.com/domtronn/all-the-icons.el#slow-rendering
  (setq inhibit-compacting-font-caches t)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package dashboard
  :if (display-graphic-p)
  :requires all-the-icons
  :demand t
  :functions dashboard-jump-to-recent-files
  :commands dashboard-setup-startup-hook
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-items #'(
                      (projects . 5)
                      (recents  . 5)
                      ;; (bookmarks . 5)
                      ;; (agenda . 5)
                      ;; (registers . 5)
                      ))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(;; line1
     ((,(all-the-icons-faicon "unlock-alt" :height 1.1 :v-adjust 0.0)
       "Pass"
       "Password Store"
       (lambda (&rest _) (pass))))))
  :config
  (dashboard-setup-startup-hook)
  :hook
  (dashboard-after-initialize-hook . (lambda
                                       (&rest _)
                                       (switch-to-buffer dashboard-buffer-name)
                                       (with-current-buffer dashboard-buffer-name
                                         (with-selected-window (get-buffer-window)
                                           ;; TODO Does not work
                                           (setq-local show-trailing-whitespace nil)
                                           (dashboard-jump-to-recent-files)
                                           (redisplay)
                                           )))))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package spaceline-config
  :disabled
  :straight spaceline
  :functions spaceline-emacs-theme
  :hook (after-init . spaceline-emacs-theme))

(use-package doom-modeline
  :disabled
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

(use-package mood-line
  :hook (after-init . mood-line-mode))

(use-package zenburn-theme
  :disabled
  :demand t
  :config
  (load-theme 'zenburn 'no-confirm))

(use-package doom-themes
  :disabled
  :demand t
  :config
  (load-theme 'doom-molokai 'no-confirm))

(use-package ample-theme
  :disabled
  :demand t
  :config
  (load-theme 'ample 'no-confirm))

(use-package moe-theme
  :disabled
  :demand t
  :config
  (load-theme 'moe-dark 'no-confirm))

;; TODO: Try out zerodark-setup-modeline-format
(use-package zerodark-theme
  :disabled
  :functions (true-color-p)
  :demand t
  :config
  ;; https://github.com/radian-software/radian/blob/ae20e19222a64bc032e0bd857878c60b7f496a58/emacs/radian.el#L5378
  ;; I don't know exactly the purpose of these faces, but it does look better.
  (let ((background-purple (if (true-color-p) "#48384c" "#5f5f5f"))
        (class '((class color) (min-colors 89)))
        (green (if (true-color-p) "#98be65" "#87af5f"))
        (orange (if (true-color-p) "#da8548" "#d7875f"))
        (purple (if (true-color-p) "#c678dd" "#d787d7"))
        ;; Added by me
        ;; (refined-added (if (true-color-p) "#1e8967" "#1e8967"))
        ;; (refined-removed (if (true-color-p) "#b33c49" "#b33c49"))
        )
    (custom-theme-set-faces
     'zerodark
     `(vertico-current
       ((,class (:background
                 ,background-purple
                 :weight bold
                 :foreground ,purple))))
     `(prescient-primary-highlight ((,class (:foreground ,orange))))
     `(prescient-secondary-highlight ((,class (:foreground ,green))))
     `(completions-common-part nil)
     ;; Added by me
     ;; `(smerge-refined-added ((,class (:background ,refined-added))))
     ;; `(smerge-refined-removed ((,class (:background ,refined-removed))))
     `(smerge-refined-added ((,class (:inherit diff-refine-added))))
     `(smerge-refined-removed ((,class (:inherit diff-refine-removed))))
     `(show-paren-match ((,class (:underline t))))))
  (enable-theme 'zerodark))

(use-package modus-themes
  :straight t
  :demand t
  :config
  ;; (require-theme 'modus-themes)
  (load-theme 'modus-vivendi-tinted))

;; Fonts
(use-package dynamic-fonts
  :functions dynamic-fonts-setup
  :init
  (customize-set-variable 'dynamic-fonts-set-alternatives t) ; Fill up alternative fonts
  (customize-set-variable 'dynamic-fonts-preferred-monospace-point-size abz-font-default-size)
  (customize-set-variable 'dynamic-fonts-preferred-monospace-fonts
                          '(
                            "Iosevka"
                            "Fira Code"
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

(use-package display-line-numbers
  :straight nil
  :custom
  (display-line-numbers-type t "Absolute line numbers")
  (display-line-numbers-grow-only t "Do not shrink line number width")
  (display-line-numbers-width-start t "Count number of lines to use for line number width")
  :init
  (set-face-attribute 'line-number nil :weight 'semi-light)
  (set-face-attribute 'line-number-current-line nil :box nil :inverse-video t)
  :hook (prog-mode . display-line-numbers-mode))

(use-package guru-mode
  :disabled
  :diminish
  :init
  (setq guru-warn-only t)
  :hook (prog-mode . guru-mode))

;; Automatic symbol highlighting, navigation & edition
;; https://github.com/gennad/auto-highlight-symbol
;; Alternative, but slower: https://github.com/fgeller/highlight-thing.el
(use-package auto-highlight-symbol
  :disabled t
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
  :hook (after-init . dimmer-mode))

;; Better Emacs help buffers
;; https://github.com/Wilfred/helpful
(use-package helpful
  :demand t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h p" . helpful-at-point))

;; Display key bindings
;; https://github.com/justbur/emacs-which-key/
(use-package which-key
  :custom
  (which-key-compute-remaps t "Show remapped command")
  (which-key-sort-order 'which-key-local-then-key-order "Sorting order")
  (which-key-max-display-columns 4 "Maximum number of columns to display")
  :hook
  (after-init . which-key-mode))

;; Use posframe to display which-key
(use-package which-key-posframe
  :after which-key
  :commands
  (which-key-posframe--show-buffer)
  :config
  (advice-add #'which-key-posframe--show-buffer
              :around (defun abz--advice-which-key-posframe--show-buffer (function &rest args)
                        "Make `which-key-posframe` at least 25% of window width and height."
                        (let ((which-key-posframe-parameters `((min-height . ,(round (* (window-height) 0.25)))
                                                               (min-width . ,(round (* (window-width) 0.25))))))
                          (apply function args))))
  :custom
  (which-key-posframe-poshandler #'posframe-poshandler-frame-top-center)
  :hook
  (which-key-mode . which-key-posframe-mode))

(use-package abz
  :straight nil
  :init
  (defun abz-kill-current-buffer ()
    "Kill the current buffer.

Calls `kill-buffer' on the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))
  :bind
  (:map abz-map
        ([remap kill-buffer] ("Kill buffer" . abz-kill-current-buffer))))

(provide 'abz-theme)

;;; abz-theme.el ends here
