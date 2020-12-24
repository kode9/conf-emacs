;;; abz-lsp.el --- LSP mode -*- lexical-binding: t; -*-

;; Copyright (C) PERRIER Pierre-Luc <dev@the-pluc.net>

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

;;; Code:

(require 'use-package)

(use-package lsp-ui
  :commands
  lsp-ui-mode
  :custom
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position #'top)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-delay 0.2)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t "Show diagnostics messages in sideline")
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-update-mode #'point)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . #'lsp-ui-peek-find-references)))

(use-package lsp-ivy
  :commands
  lsp-ivy-workspace-symbol
  lsp-ivy-global-workspace-symbol)

(use-package which-key)

(use-package lsp-mode
  :commands lsp
  :init
  (setq read-process-output-max (* 3 1024 1024))
  :custom
  (lsp-before-save-edits nil "Apply edits suggested by the language server before saving a document")
  (lsp-completion-enable t "Enable `completion-at-point' integration")
  (lsp-completion-provider :none "The completion backend. Fucks with company-backends")
  (lsp-diagnostics-provider :auto "The checker backend provider (auto use flycheck, fallback to flymake)")
  (lsp-eldoc-enable-hover nil "Display hover info with `eldoc'")
  (lsp-eldoc-render-all nil "Display all of the info returned by document/onHover")
  (lsp-enable-file-watchers t)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-snippet nil "Propose snippets on completion")
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-text-document-color t)
  (lsp-enable-xref t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.500)
  (lsp-imenu-container-name-separator "⦿")
  (lsp-imenu-show-container-name t)
  (lsp-keep-workspace-alive t)
  (lsp-keymap-prefix "C-c l")
  (lsp-log-io t)
  (lsp-print-performance nil)
  (lsp-response-timeout 5)
  (lsp-server-trace t)
  (lsp-signature-auto-activate #'(:after-completion :on-trigger-char)) ; nil
  (lsp-signature-render-documentation nil "Display signature documentation in `eldoc'")
  (lsp-rust-server 'rust-analyzer "Which LSP server to use (`rust-analyzer' or `rls')")
  :hook
  ((c++-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

;; Part of lsp-mode
(use-package lsp-completion
  :straight nil
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2415
  :hook (lsp-completion-mode . (lambda () (delq 'company-capf company-backends))))

(use-package yasnippet
  :commands yas-minor-mode
  :after
  lsp-mode
  :hook
  (lsp-mode . yas-minor-mode))

(use-package beacon
  :hook (after-init . beacon-mode))

(provide 'abz-lsp)

;;; abz-lsp.el ends here
