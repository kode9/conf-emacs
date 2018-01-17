;;; pluc-theme.el --- Fonts & Faces -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 Pierre-Luc Perrier

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

(use-package zenburn-theme :demand)

;; Fonts
(use-package dynamic-fonts
  :init
  (customize-set-variable 'dynamic-fonts-set-alternatives t) ; Fill up alternative fonts
  (customize-set-variable 'dynamic-fonts-preferred-monospace-point-size 8)
  (customize-set-variable 'dynamic-fonts-preferred-monospace-fonts
                          '("Monoid" "Hack" "Dejavu Sans Mono" "courier" "fixed"))
  (customize-set-variable 'dynamic-fonts-preferred-proportional-fonts
                          '("Open Sans" "Roboto" "Dejavu Sans" "arial" "fixed"))
  (customize-set-variable 'dynamic-fonts-preferred-proportional-point-size 10)
  :config
  (dynamic-fonts-setup))

(use-package spaceline-config
  :config (spaceline-emacs-theme))

(provide 'pluc-theme)
;;; pluc-theme.el ends here
