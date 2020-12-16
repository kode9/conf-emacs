;;; abz-company.el --- Company completion framework -*- lexical-binding: t; -*-

;; Copyright (C) PERRIER Pierre-Luc <dev@the-pluc.net>
;;
;; Author: PERRIER Pierre-Luc <dev@the-pluc.net>
;; Maintainer: PERRIER Pierre-Luc <dev@the-pluc.net>
;; Homepage: https://github.com/kode9
;; Keywords: abbrev, convenience, matching
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;; Code:

(require 'use-package)

;; Auto completion framework
(use-package company
  :diminish company-mode
  :custom
  (company-auto-commit nil "Don't auto commit the completion e.g on SPACE")
  (company-dabbrev-downcase nil "Make dabbrev keep case")
  (company-dabbrev-ignore-case nil "Make dabbrev case-sensitive")
  (company-dabbrev-other-buffers t "Make dabbrev search only in buffers with the same major-mode")
  (company-idle-delay 0.25 "Seconds before starting completion")
  (company-minimum-prefix-length 2 "Minimum numbers of characters to start completion")
  (company-require-match nil "Let me type whatever I want")
  (company-show-numbers t "Quick access to first 10 candidates")
  (company-tooltip-align-annotations t "Align annotations to the right tooltip border")
  (company-tooltip-limit 15 "The maximum number of **visible** candidates")

  (company-frontends #'(company-pseudo-tooltip-unless-just-one-frontend
                        company-preview-if-just-one-frontend
                        company-echo-metadata-frontend)
                     "Active frontends")
  ;; TODO backends: company-clang
  (company-backends #'((company-files
                        company-capf
                        :with company-yasnippet
                        :with company-keywords
                        :with company-dabbrev-code
                        :with company-dabbrev
                        :with company-abbrev))
                    "Globally enabled backends")
  (company-transformers #'(company-sort-by-backend-importance
                           company-sort-by-occurrence
                           company-sort-prefer-same-case-prefix)
                        "Candidate sorting")
  (company-global-modes #'(not gud-mode help-mode)
                        "Manage modes where global-company-mode is enabled")
  :hook
  (after-init  . global-company-mode)
  ;; TODO I guess this should be refactored
  (cmake-mode . (lambda () (set (make-local-variable 'company-backends) #'((company-files
                                                                            company-cmake
                                                                            company-capf
                                                                            :with company-yasnippet
                                                                            :with company-keywords
                                                                            :with company-dabbrev-code
                                                                            :with company-dabbrev
                                                                            :with company-abbrev)))))
  (text-mode . (lambda () (set (make-local-variable 'company-backends) #'((company-files
                                                                           company-ispell
                                                                           company-capf
                                                                           :with company-yasnippet
                                                                           :with company-keywords
                                                                           :with company-dabbrev-code
                                                                           :with company-dabbrev
                                                                           :with company-abbrev)))))
  (adoc-mode . (lambda () (set (make-local-variable 'company-backends) #'((company-files
                                                                           company-ispell
                                                                           company-capf
                                                                           :with company-yasnippet
                                                                           :with company-keywords
                                                                           :with company-dabbrev-code
                                                                           :with company-dabbrev
                                                                           :with company-abbrev))))))

;; Cycle candidates using TAB. Part of company. Insert itself in `company-frontends'.
(use-package company-tgn
  :disabled
  :straight nil
  :after company
  :init
  (company-tng-mode))

;; Company frontend with icons using a child frame
;; Can differentiate backends with colors
(use-package company-box
  :diminish
  :custom
  (company-box-frame-behavior 'point "Frame position behavior")
  (company-box-icons-alist 'company-box-icons-all-the-icons "Which icons/images to use")
  (company-box-scrollbar nil "No scrollbar")
  ;; I don't know why but I can't see company-preview-if-just-one-frontend when there is a single candidate
  (company-box-show-single-candidate t "Display single candidate if it's the only frontend")
  (company-tooltip-maximum-width 120 "The maximum width of the tooltip’s inner area")

  (company-box-doc-delay 0.25 "Delay before showing doc")
  (company-box-doc-enable t "Show candidate documentation")

  ;; Debug
  (company-box-backends-colors #'((company-yasnippet . (:all "DarkGoldenrod"))
                                  (company-elisp . (:all "YellowGreen"))
                                  (company-dabbrev . (:all "PaleVioletRed"))
                                  (company-files . (:all "SaddleBrown"))
                                  (company-capf . (:all "DodgerBlue"))
                                  (company-keywords . (:all "LightGoldenrod"))
                                  (company-cmake . (:all "ForestGreen"))
                                  (company-clang . (:all "red3"))
                                  (company-dabbrev-code . (:all "DarkTurquoise")))
                               "List of colors to use for specific backends")
  :config
  (delq 'company-echo-metadata-frontend company-frontends)
  :hook (company-mode . company-box-mode))

;; Company frontend using posframe
;; Shows the current backend at the bottom
(use-package company-posframe
  :disabled
  :hook
  (company-mode . company-posframe-mode))

;; Show help next to the candidate
;; Not compatible with company-box (use company-box-doc)
(use-package company-quickhelp
  :disabled
  :commands company-quickhelp-mode
  :custom
  (company-quickhelp-use-propertized-text t)
  (company-quickhelp-delay 0.25)
  :hook
  (company-mode . company-quickhelp-local-mode))

(use-package company-c-headers
  :disabled
  :after company
  :demand t
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/v1/")
  (add-to-list 'company-backends 'company-c-headers nil))

(provide 'abz-company)

;;; abz-company.el ends here

;; Local Variables:
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode))
;; End:
