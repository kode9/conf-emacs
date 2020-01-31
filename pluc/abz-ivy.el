;;; abz-ivy.el --- Ivy/Counsel/Sweeper configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Pierre-Luc Perrier

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

;; https://oremacs.com/swiper/

;;; Code:

;; TODO
;;  * fuzzy (see also filtering / sorting in README)
;;  * tab to complete directory, C-d / C-j to dired
;;  * counsel-grep-or-swiper
;;  * flx
;;  * smex
;;  * highlighting
;;  * remove `^` from regex
;;  * don't stop swiper when pressing return all the way
;;  * actions
;; completion (TAB)
;;  * counsel-grep-or-swiper
;; switch-to-buffer-other-window && 'ivy-switch-buffer-other-window
;; fill-paragraph
;; useful commands in README cheatsheet
;; M-x => show last-used commands on top => amx => counsel-minibuffer-history
;; jumping (see mark-ring/pop-to-mark-command/bookmark-jump/counsel-mark-ring)
;; '(user-full-name "Pierre-Luc Perrier")
;; '(user-mail-address "dev@the-pluc.net"))
;; C-x " == C-x 3
;; prescient

(eval-when-compile (require 'use-package))

;; Ivy will use flx for filtering if it's loaded
(use-package flx
  :demand t)

;; Counsel will use amx (or smex) for M-x if it's loaded
(use-package amx
  :demand t)

;; Minibuffer actions
(use-package hydra
  :demand t)

(use-package ivy
  :demand t
  :after flx
  :diminish
  :commands ivy-mode
  :init
  (customize-set-variable 'ivy-count-format "(%d/%d) ")            ; Candidate count (current/max)
  (customize-set-variable 'ivy-use-virtual-buffers t)              ; add recent files and bookmarks to `ivy-switch-buffer`
  (customize-set-variable 'ivy-wrap t)                             ; Wrap around candidates
  (customize-set-variable 'ivy-height 15)                          ; Number of visible candidates
  (customize-set-variable 'confirm-nonexistent-file-or-buffer nil) ; Don't ask confirmation for new files
  (customize-set-variable 'ivy-on-del-error-function 'ignore)      ; Don't exit the completion when there is nothing left to delete
  ;; Faces
  (custom-set-faces
   '(ivy-minibuffer-match-face-2 ((t . (:background nil :box t))))
   '(ivy-minibuffer-match-face-3 ((t . (:background nil :box t))))
   '(ivy-minibuffer-match-face-4 ((t . (:background nil :box t))))
   '(ivy-current-match           ((t . (:background nil :box nil :inverse-video t)))))
  ;; Completion styles
  (setq ivy-re-builders-alist '((swiper . ivy--regex-ignore-order)
                                (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode +1)
  (with-eval-after-load 'projectile (customize-set-variable 'projectile-completion-system 'ivy))
  :bind ("C-c C-r" . ivy-resume))

(use-package swiper
  :demand t
  :after ivy
  :bind ("C-s" . swiper))

(use-package counsel
  :demand t
  :functions counsel-mode
  :config
  ;; Remap built-in commands to their counsel counterpart
  (counsel-mode +1)
  :bind ("C-x _ RET" . counsel-unicode-char)
  :after (swiper amx))

(use-package ivy-hydra
  :demand t
  :after (ivy hydra))

(provide 'abz-ivy)

;;; abz-ivy.el ends here

;; LocalWords:  abz
