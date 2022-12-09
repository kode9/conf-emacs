;;; abz-vertico.el --- Vertico integration -*- lexical-binding: t; -*-

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

(require 'use-package)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     ;; Other available extensions:
                     ;; vertico-buffer
                     ;; vertico-flat
                     ;; vertico-grid
                     ;; vertico-mouse
                     ;; vertico-multiform
                     ;; vertico-quick
                     ;; vertico-reverse
                     ;; vertico-unobtrusive
                     :includes (vertico-directory
                                vertico-indexed
                                vertico-repeat))
  :commands
  vertico-mode
  vertico--format-candidate
  :demand t
  :custom
  (vertico-count 20 "Maximal number of candidates to show")
  (vertico-cycle t "Enable cycling for vertico-next and vertico-previous")
  (vertico-resize t "How to resize the Vertico minibuffer window")
  :init
  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  :config
  (vertico-mode +1)
  (vertico-indexed-mode +1)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  ;; https://github.com/minad/vertico/issues/291#issuecomment-1304909746
  (defun abz--vertico-select-first (state)
    "Select first candidate instead of the prompt."
    (when (> (alist-get 'vertico--total state) 0)
      (setf (alist-get 'vertico--index state) 0))
    state)
  (advice-add #'vertico--recompute :filter-return #'abz--vertico-select-first)
  :bind
  (("M-r" . vertico-repeat)
   :map vertico-map
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   ("C-DEL" . vertico-directory-delete-word))
  :hook
  (minibuffer-setup . vertico-repeat-save)
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package minibuffer
  :disabled
  :straight nil
  :custom
  (completion-styles '(flex basic partial-completion emacs22)))

(use-package orderless
  :disabled
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-flex orderless-literal))
  (completion-styles '(orderless basic)))

;; TODO: This is not only for completion and should be configured somewhere else
(use-package prescient
  :commands
  prescient-persist-mode
  :custom
  (prescient-filter-method #'(literal initialism fuzzy))
  (prescient-sort-full-matches-first t)
  :config
  (prescient-persist-mode +1))

(use-package vertico-prescient
  ;; https://github.com/radian-software/prescient.el/issues/137
  :straight (vertico-prescient :type git :flavor melpa :files ("vertico-prescient.el") :host github :repo "radian-software/prescient.el")
  :demand t
  :after vertico
  :config
  (vertico-prescient-mode +1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :commands
  marginalia-mode
  :custom
  (marginalia-align 'center "Alignment of the annotations")
  (marginalia-max-relative-age 0 "Maximum relative age in seconds displayed by the file annotator. 0 to never show "a relative age)
  ;; https://github.com/minad/marginalia/issues/110
  ;; https://github.com/bbatsov/projectile/issues/1664
  (marginalia-command-categories '((projectile-find-file . project-file)
                                   (projectile-find-dir . project-file)
                                   (projectile-switch-project . file)
                                   (imenu . imenu)
                                   (recentf-open . file))
                                 "Associate commands with a completion category")
  :init
  (marginalia-mode +1)
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle)))

(use-package all-the-icons-completion
  :commands all-the-icons-completion-mode
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico-posframe
  :commands
  vertico-posframe-mode
  :custom
  (vertico-posframe-parameters '((left-fringe . 8)
                                 (right-fringe . 8))
                               "The frame parameters used by vertico-posframe")
  :init
  (vertico-posframe-mode +1))

;; Mini-buffer actions
(use-package embark
  ;; :init
  ;; ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  ;; :config
  ;; ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))
  :bind
  (("M-," . embark-act)
   ("M-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

;; TODO: Move somewhere else
(use-package ctrlf
  :hook
  (after-init . ctrlf-mode))

(provide 'abz-vertico)

;;; abz-vertico.el ends here
