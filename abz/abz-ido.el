;;; abz-ido.el --- InteractivelyDoThings -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020, 2020 Pierre-Luc Perrier

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

(use-package ido
  :init
  (customize-set-variable 'ido-enable-flex-matching t) ; Fuzzy matching
  (customize-set-variable 'ido-use-filename-at-point nil) ; Do not match filename at point
  (customize-set-variable 'ido-enable-last-directory-history t) ; Remember latest selected directories
  (customize-set-variable 'ido-save-directory-list-file
                          (expand-file-name ".cache/.ido.last" user-emacs-directory)) ; ido cache file
  (customize-set-variable 'ido-max-work-directory-list 50) ; Max working directory history
  (customize-set-variable 'ido-max-work-file-list 100) ; Max file history
  (customize-set-variable 'ido-confirm-unique-completion nil) ; Do not wait for RET on unique completion
  (customize-set-variable 'ido-use-virtual-buffers 't) ; Use virtual buffers
  (customize-set-variable 'ido-use-faces t) ; Enable ido highlights
  :config
  (ido-mode t)
  (ido-everywhere t) ; Toggle use of Ido for all buffer/file reading.
  ;; Better fuzzy matching
  (use-package flx-ido
    :init (customize-set-variable 'flx-ido-use-faces nil) ; Disable flx highlights
    :config (flx-ido-mode t))
  ;; ido everywhere and more
  (use-package ido-completing-read+
    :config (ido-ubiquitous-mode t))
  ;; Makes ido-mode display vertically
  (use-package ido-vertical-mode
    :config (ido-vertical-mode t))
  ;; ido for M-x
  (use-package smex
    :init
    (customize-set-variable 'smex-save-file (locate-user-emacs-file ".cache/.smex-items")) ; Persistence
    (customize-set-variable 'smex-history-length 20) ; Recent commands
    :bind (("M-x" . smex) ("M-X" . smex-major-mode-commands))
    :config (smex-initialize)))

(provide 'abz-ido)

;;; abz-ido.el ends here
