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

(require 'abz-settings)
(require 'use-package)

(defcustom abz-lsp-use-lsp-booster nil
  "If non nil, use https://github.com/blahgeek/emacs-lsp-booster."
  :tag "Use lsp-booster"
  :type 'boolean
  :group 'abz)

(use-package lsp-ui
  :preface
  (declare-function lsp-find-references "lsp-mode")
  (declare-function lsp-find-definition "lsp-mode")
  :custom
  ;; lsp-ui-sideline: Show informations of the symbols on the current line. It also show flycheck diagnostics and LSP code actions
  (lsp-ui-sideline-delay 0.2 "Seconds to wait before showing sideline")
  (lsp-ui-sideline-diagnostic-max-lines 3 "Number of diagnostic message lines")
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate nil "Ignore duplicates when there is a same symbol with the same contents")
  (lsp-ui-sideline-show-code-actions t "Show code actions in sideline")
  (lsp-ui-sideline-show-diagnostics nil "Show diagnostics messages in sideline")
  (lsp-ui-sideline-show-hover nil "Show hover messages in sideline")
  (lsp-ui-sideline-update-mode 'line "When to update information. 'line or 'point")
  ;; lsp-ui-peek: Add peek feature
  (lsp-ui-peek-enable nil "enable ‘lsp-ui-peek’")
  (lsp-ui-peek-show-directory t "Show the directory of files")
  ;; lsp-ui-doc: Show object documentation at point in a child frame
  (lsp-ui-doc-enable t "Enable lsp-ui-doc")
  (lsp-ui-doc-position 'top "Where to display the doc (top, bottom or at-point)")
  (lsp-ui-doc-side 'right "Where to display the doc (left or right)")
  (lsp-ui-doc-delay 0.2 "Number of seconds before showing the doc")
  (lsp-ui-doc-show-with-cursor nil "When non-nil, move the cursor over a symbol to show the doc") ; NOTE: A bit slow
  (lsp-ui-doc-show-with-mouse t "When non-nil, move the mouse pointer over a symbol to show the doc")
  ;; lsp-ui-imenu: Show imenu entries
  ;; (lsp-ui-imenu-kind-position "Place to show entries kind")
  ;; (lsp-ui-imenu-buffer-position "Place to show the buffer window")
  ;; (lsp-ui-imenu-window-width "Set window width")
  ;; (lsp-ui-imenu-window-fix-width "When non-nil, the window will not be resizable (eg. unaffected by balance-windows)")
  ;; (lsp-ui-imenu--custom-mode-line-format "Mode line format")
  ;; (lsp-ui-imenu-auto-refresh "Auto refresh when necessary")
  ;; (lsp-ui-imenu-refresh-delay "Delay to refresh imenu")
  :bind
  (:map lsp-ui-mode-map
        ;; ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
        ;; ([remap xref-find-references] . #'lsp-ui-peek-find-references)
        ([remap xref-find-definitions] . #'lsp-find-definition)
        ([remap xref-find-references] . #'lsp-find-references))
  :hook
  ;; sideline seems to work fine without lsp to display flycheck diagnostics
  (lisp-mode . lsp-ui-sideline-mode)
  (emacs-lisp-mode . lsp-ui-sideline-mode))

(use-package lsp-ivy
  :disabled
  :commands
  lsp-ivy-workspace-symbol
  lsp-ivy-global-workspace-symbol)

(use-package which-key
  :straight nil)

(use-package lsp-mode
  :functions
  lsp-register-client
  make-lsp-client
  lsp-activate-on
  lsp-async-start-process
  lsp-stdio-connection
  :defines
  lsp-clients-clangd-library-directories
  :init
  (setq read-process-output-max (* 1024 1024))
  :preface
  ;; https://github.com/blahgeek/emacs-lsp-booster
  (when abz-lsp-use-lsp-booster
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                (setcar orig-result command-from-exec-path))
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result))))
  :custom
  (lsp-before-save-edits nil "Apply edits suggested by the language server before saving a document")
  (lsp-diagnostics-provider :flycheck "Diagnostics backend")
  (lsp-enable-file-watchers t "If non-nil lsp-mode will watch the files in the workspace if the server has requested that")
  (lsp-enable-folding nil)
  (lsp-enable-indentation t "Indent regions using the file formatting functionality provided by the language server")
  (lsp-enable-links nil "Make links clickable")
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting t "Highlight references of the symbol at point")
  (lsp-enable-text-document-color t "Enable textDocument/documentColor integration")
  (lsp-enable-xref t "Enable xref integration")
  (lsp-headerline-breadcrumb-enable t "Whether to enable breadcrumb on headerline")
  (lsp-keymap-prefix "C-c l")
  (lsp-modeline-diagnostics-enable nil "Whether to show diagnostics on modeline")
  (lsp-semantic-tokens-enable nil)

  ;; Server
  (lsp-idle-delay 0.45)
  (lsp-keep-workspace-alive nil)
  (lsp-log-io nil "Log all messages from the language server to a *lsp-log* buffer")
  (lsp-print-performance nil)
  (lsp-response-timeout 10)
  (lsp-server-trace nil)

  ;; Completion
  (lsp-completion-provider :capf "Use completion-at-point functions as the primary backend")
  (lsp-completion-enable t "Enable `completion-at-point' integration")
  (lsp-completion-show-detail nil "Whether or not to show detail of completion candidates")
  (lsp-completion-show-kind t "Whether or not to show kind of completion candidates")
  (lsp-enable-snippet nil "Propose snippets on completion")

  ;; Imenu
  (lsp-enable-imenu (not (eq abz-imenu 'disabled)) "Enable imenu when abz-imenu is not disabled")
  (lsp-imenu-container-name-separator "⦿")
  (lsp-imenu-show-container-name t)

  ;; (lsp-signature-auto-activate '(:after-completion
  ;;                                :on-trigger-char
  ;;                                :on-server-request) "Auto activate signature conditions")

  ;; Eldoc
  (lsp-signature-render-documentation nil "Display signature documentation in `eldoc'")
  (lsp-eldoc-enable-hover nil "Display hover info with `eldoc'")
  (lsp-eldoc-render-all nil "Display all of the info returned by document/onHover")

  ;; Rust
  (lsp-rust-server 'rust-analyzer "Which LSP server to use (`rust-analyzer' or `rls')")

  ;; C/C++
  (lsp-clients-clangd-args `("--all-scopes-completion"
                             "--background-index"
                             "--clang-tidy"
                             "--completion-style=bundled"
                             "--enable-config"
                             "--header-insertion-decorators=0"
                             "--header-insertion=iwyu"
                             "-j" ,(format "%d" (max (- (num-processors 'query) 1) 1))
                             "--limit-references=500"
                             "--limit-results=500"
                             "--log=error"
                             "--pch-storage=memory"))
  :config
  (cl-pushnew "[/\\\\]build\\'" lsp-file-watch-ignored-directories :test #'string=)
  ;; https://github.com/neocmakelsp/neocmakelsp
  (defun abz--lsp-cmakeneo--download-server (_client callback error-callback update?)
    "Install/update neocmakelsp CMake language server using `cargo

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t."
    (lsp-async-start-process
     callback
     error-callback
     "cargo" "install" "neocmakelsp" (when update? "--force")))
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection '("neocmakelsp" "stdio"))
                        :activation-fn (lsp-activate-on "cmake")
                        :priority 0
                        :server-id 'cmakeneo))
  (add-to-list 'lsp-disabled-clients 'cmakels)

  ;; lsp-booster
  (when abz-lsp-use-lsp-booster
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
  :hook
  ((c-mode-common . lsp-deferred)
   (c-ts-base-mode . lsp-deferred)
   (cmake-mode . lsp-deferred)
   (cmake-ts-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (python-ts-mode . lsp-deferred)
   (dockerfile-mode . lsp-deferred)
   (dockerfile-ts-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)))

;; Part of lsp-mode
;; (use-package lsp-completion
;;   :straight nil
;;   :hook ((lsp-mode . lsp-completion-mode)
;;          (lsp-managed-mode . (lambda ()
;;                                (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t)))))

(use-package dap-mode
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  ;; :hook
  ;; (add-hook dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  )

(use-package dap-lldb
  :straight nil)

(use-package yasnippet
  :disabled
  :commands yas-minor-mode
  :after
  lsp-mode
  :hook
  (lsp-mode . yas-minor-mode))

(provide 'abz-lsp)

;;; abz-lsp.el ends here
