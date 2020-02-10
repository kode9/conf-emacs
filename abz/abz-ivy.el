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

(require 'abz-theme) ; all-the-icons, dimmer
(require 'use-package)

;; Ivy will use flx for filtering if it's loaded
(use-package flx :demand t)

;; Counsel will use amx (or smex) for M-x if it's installed
(use-package amx)

(use-package ivy
  :demand t
  :after flx
  :diminish
  :commands ivy-mode
  :custom
  (confirm-nonexistent-file-or-buffer nil "Don't ask confirmation for new files")
  (ivy-count-format "(%d/%d) " "Candidate count (current/max)")
  (ivy-extra-directories nil "Add this to the front of the list when completing file names")
  (ivy-height 15 "Number of visible candidates")
  (ivy-initial-inputs-alist nil "Initial characters in the input string")
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create "Create directory on demand")
  (ivy-on-del-error-function 'ignore "Don't exit the completion when there is nothing left to delete")
  (ivy-use-virtual-buffers t "Add recent files and bookmarks to `ivy-switch-buffer'")
  (ivy-wrap t "Wrap around candidates")
  :custom-face
  ;; Faces
  (ivy-minibuffer-match-face-2 ((t . (:background nil :box t))))
  (ivy-minibuffer-match-face-3 ((t . (:background nil :box t))))
  (ivy-minibuffer-match-face-4 ((t . (:background nil :box t))))
  (ivy-current-match           ((t . (:background nil :box nil :inverse-video t))))
  :init
  ;; Completion styles
  (setq ivy-re-builders-alist '((swiper . ivy--regex)
                                (swiper-isearch . ivy--regex)
                                (Man-completion-table . ivy--regex-ignore-order)
                                (woman . ivy--regex-ignore-order)
                                (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode +1)
  (with-eval-after-load 'projectile (customize-set-variable 'projectile-completion-system 'ivy))
  :bind ("C-c C-r" . ivy-resume))

(use-package swiper
  :demand t
  :after ivy
  :bind ("C-s" . swiper-isearch))

(use-package counsel
  :commands counsel-mode
  :after swiper
  :demand t
  :config
  ;; Remap built-in commands to their counsel counterpart
  (counsel-mode +1)
  :bind ("C-x _ RET" . counsel-unicode-char))

;; Hydra actions in ivy minibuffer
(use-package ivy-hydra
  :after ivy
  :demand t)

;; Use posframe to show ivy candidates
;; https://github.com/tumashu/ivy-posframe
(use-package ivy-posframe
  :after ivy
  :demand t
  :commands ivy-posframe-mode
  :preface
  (defun abz--ivy-posframe-get-size ()
    "Size of a ivy-posframe.

Compared to the default, `ivy-posframe-min-width' is set to
- `ivy-posframe-min-width' if defined
- maximum of (ratio * window-width) and (1/split * ratio * frame-width) otherwise.

with ratio=0.75 and split=2.

So it will take 75% of the window width if not split, but won't shrink too much
if it is split more than `split' frames."
    (list
     :height ivy-posframe-height
     :width ivy-posframe-width
     :min-height (or ivy-posframe-min-height
                     (let ((height (+ ivy-height 1)))
                       (min height (or ivy-posframe-height height))))
     :min-width (or ivy-posframe-min-width
                    (let* ((ratio 0.75)
                           (split 2)
                           (split-ratio (/ 1.0 split))
                           (width (round (* (window-width) ratio))))
                      (max (min width (or ivy-posframe-width width)) (round (* (frame-width) split-ratio ratio)))))))
  :custom
  (ivy-posframe-size-function #'abz--ivy-posframe-get-size "Function to compute the size of the frame")
  (ivy-posframe-style 'window-center "Default position")
  (ivy-posframe-display-functions-alist '((complete-symbol . ivy-posframe-display-at-point)
                                          (swiper . ivy-posframe-display-at-frame-top-center)
                                          (swiper-isearch . ivy-posframe-display-at-frame-top-center)
                                          (t . ivy-posframe-display))
                                        "Per-command position")
  (ivy-posframe-border-width 2 "Frame border width")
  (ivy-posframe-hide-minibuffer nil "Keep showing minibuffer")
  :custom-face
  (ivy-posframe-border ((t :background ,(face-attribute 'warning :foreground))))
  :config
  ;; Don't dimmer posframe buffer
  (when (boundp 'dimmer-buffer-exclusion-regexps)
    (add-to-list 'dimmer-buffer-exclusion-regexps ivy-posframe-buffer))
  (ivy-posframe-mode 1)
  :diminish ivy-posframe-mode)

(use-package ivy-rich
  :after ivy
  :demand t
  :commands (ivy-format-function-line ivy-rich-mode)
  :custom
  (ivy-rich-parse-remote-buffer nil "Don't work too much through tramp")
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :straight (:type git :host github :repo "seagle0128/all-the-icons-ivy-rich")
  :after (all-the-icons ivy)
  :demand t
  :commands all-the-icons-ivy-rich-mode
  :config
  (all-the-icons-ivy-rich-mode 1))

(provide 'abz-ivy)

;;; abz-ivy.el ends here
