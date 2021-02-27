;;; abz-devel.el --- Development packages -*- lexical-binding: t; -*-

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

(require 'abz-tools)
(require 'cl-extra)
(require 'f)
(require 's)
(require 'use-package)
(require 'abz-completion)

;;;###autoload
(cl-defun abz-process-window (process &optional (all-frames (selected-frame)))
  "Get the window currently displaying the buffer of PROCESS, or nil if none.

ALL-FRAMES specify which frames to consider as described in `get-buffer-window'."
  (get-buffer-window (process-buffer process) all-frames))

;;;###autoload
(defun abz-select-process-window (process)
  "Select the window currently displaying the buffer of PROCESS."
  (select-window (abz-process-window process) 'norecord))

;; Highlight trailing whitespaces
(add-hook 'prog-mode-hook #'(lambda () (setq show-trailing-whitespace t)))

;; Compilation
(use-package compile
  :straight nil
  :init
  ;; Kill a running compilation process without asking before starting a new one
  (customize-set-variable 'compilation-always-kill t)
  ;; Autoscoll compilation buffer and stop on first error
  (customize-set-variable 'compilation-scroll-output 'first-error)
  ;; Automatically jump to the first error during compilation
  (customize-set-variable 'compilation-auto-jump-to-first-error nil)
  ;; Skip 'info' and 'warnings' when jumping between errors
  (customize-set-variable 'compilation-skip-threshold 2)
  (add-hook 'compilation-start-hook #'abz-select-process-window)
  :bind  (:map compilation-mode-map
               ("n" . compilation-next-error)
               ("p" . compilation-previous-error)
               ("M-p" . compilation-previous-file)
               ("M-n" . compilation-next-file)))

;; Wrap text with punctation or tag
;; https://github.com/rejeep/wrap-region.el
(use-package wrap-region
  :diminish wrap-region-mode
  :commands wrap-region-global-mode
  :config
  (wrap-region-global-mode t))

;; clang-format
(use-package clang-format
  :load-path ("/usr/share/clang"))

;; git-gutter+: View, stage and revert Git changes straight from the
;; buffer.
;;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :disabled
  :functions git-gutter+-mode
  :diminish git-gutter+-mode
  :init
  (add-hook 'prog-mode-hook 'git-gutter+-mode))

;; which-function: Show function at cursor
(use-package which-func
  :defines which-func-keymap
  ;; Displayed when current function is unknown
  :config
  (setq which-func-unknown "∅")
  (customize-set-variable 'which-func-format `("λ⌜"
                                               (:propertize which-func-current
                                                            local-map ,which-func-keymap
                                                            face which-func
                                                            mouse-face mode-line-highlight
                                                            help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
                                               "⌟"))
  :hook (prog-mode . which-function-mode))

;; Beautify the operators in codes.Requires tuning/patching for a lot
;; of things: templates, includes,
;; etc. http://www.emacswiki.org/emacs/SmartOperator
(use-package smart-operator
  :disabled
  :commands smart-insert-operator-hook
  :init
  (add-hook 'c-mode-common-hook 'smart-insert-operator-hook))

;; Color keywords such as TODO in comments and strings
(use-package fic-mode
  :disabled
  :diminish fic-mode
  :init
  (add-hook #'prog-mode-hook #'fic-mode)
  :config
  ;; Colors match zenburn (dark gray / yellow)
  (set-face-attribute 'fic-face nil
                      :background "#2B2B2B"
                      :foreground "#F0DFAF")
  (set-face-attribute 'fic-author-face nil
                      :background nil
                      :foreground "#F0DFAF"
                      :underline nil
                      :slant 'italic
                      :height 0.95))

;; Projectile: project management
;;
;; no-literring:
;;  - projectile-cache-file
;;  - projectile-known-projects-file
(use-package projectile
  :preface
  (defun abz--projectile-modeline-function ()
    "The function to use to generate project-specific mode-line."
    (format " π⌜%s⌟" (or (projectile-project-name) "∅")))
  :custom
  (compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  (projectile-find-dir-includes-top-level t "Add top-level dir to projectile-find-dir")
  (projectile-mode-line-function #'abz--projectile-modeline-function)
  (projectile-require-project-root nil "Consider the current directory the root")
  (projectile-switch-project-action #'magit-status "Function to call when switching project")
  (projectile-auto-discover nil "Discover projects in `projectile-project-search-path'")
  (projectile-generic-command (let ((fd (cl-some #'executable-find '("fd" "fdfind"))))
                                (if fd
                                    (string-join `(,fd "-0" "-t f" "-c never" ".") " ")
                                  (string-join `("find" "." "-type f" "-print0") " ")))
                              "Command used by projectile to get the files in a generic project")
  :config
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
  (run-with-idle-timer 59 t #'projectile-cleanup-known-projects)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map
        ("o" . nil) ; Bound on projectile-multi-occur by default
        ("o f" . projectile-find-file-other-window)
        ("o b" . projectile-switch-to-buffer-other-window)
        ("o s" . projectile-display-buffer)
        ("o a" . projectile-find-other-file-other-window))
  :hook
  (after-init . projectile-mode))

;;;;;;;;;;;;;;;;;
;; Major Modes ;;
;;;;;;;;;;;;;;;;;

;; C++
(use-package cc-mode
  :straight nil
  :mode ("\\.\\(?:inl\\|h\\)\\'" . c++-mode)
  :config
  (c-toggle-auto-newline 1))

;; OpenGL shaders
(use-package glsl-mode
  :mode "\\.\\(?:[vfg]s\\|glsl\\|vert\\|frag\\|geom\\|ksh\\)\\'")

;; Qt QML
(use-package qml-mode
  :mode "\\.qml\\'")

;; NVidia CUDA
;;; https://github.com/chachi/cuda-mode
;;; Got errors with the packaged version.
(use-package cuda-mode
  :load-path "vendor/"
  :mode "\\.cuh?\\'")

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'")

;; Shell scripts
(use-package sh-script
  :straight nil
  :mode (("/PKGBUILD\\'" . sh-mode) ; Arch Linux PKGBUILD
         ("\\.zsh\\'" . sh-mode)))

;; CSV
(use-package csv-mode
  :mode "\\.csv\\'")

;; CMake
(use-package cmake-mode
  :straight nil
  :mode
  ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Better syntax highlightning for CMake
;;; Disabled: It is VERY slow. I'm not sure if it's a clash with
;;; something else.
(use-package cmake-font-lock
  :disabled
  :commands cmake-font-lock-activate
  :init
  ;; cmake-font-lock-activate must be called BEFORE fic-mode. Since
  ;; cmake-mode will call prog-mode hooks and then after cmake-mode
  ;; hooks, the workaroung is to add cmake-font-lock-activate at the
  ;; start of the prog-mode hooks.
  (remove-hook 'cmake-mode-hook 'cmake-font-lock-activate) ; package autoloads
  (defun abz-cmake-font-lock-activate()
    "Call cmake-font-lock-activate only for cmake-mode."
    (when (derived-mode-p #'cmake-mode)
      (cmake-font-lock-activate)))
  (add-hook 'prog-mode-hook #'abz-cmake-font-lock-activate))

;; Scilab
(use-package scilab-mode
  :straight nil
  :mode "\\.\\(?:sci\\|sce\\)\\'")

;; SQL
(use-package sql
  :functions sql
  :mode ("\\.sql\\'" . sql-mode)
  :config
  (use-package sql-indent) ; Better indentation
  )

;; Apache
(use-package apache-mode
  :mode "/\\.htaccess\\'")

;; Xdefaults
(use-package conf-mode
  :straight nil
  :mode ("\\.xrdb\\'" . conf-xdefaults-mode))

;; Markdown
;;; TODO Have a look at alternative markdown implementations
(use-package markdown-mode
  :mode "\\.\\(?:md\\|mdwn\\|mdml\\|markdown\\)\\'")

;; GDB
;;; TODO
;;;   * configure speedbar (for watch expressions)
;;;   * configure layout if possible
;;; Check out http://www.emacswiki.org/emacs/Frame_Tiling_Commands
(use-package gdb-mi
  :straight nil
  :init
  (setq
   gdb-many-windows t                ; Multiple window layout
   gdb-show-main t                   ; Display both gud and the main source (if no many-windows)
   gdb-thread-buffer-verbose-names t ; Show long thread names like ‘Thread 0x4e2ab70 (LWP 1983)’
   gdb-thread-buffer-arguments t     ; Show function arguments in threads buffer
   gdb-thread-buffer-locations t     ; Show file information or library names in threads buffer
   gdb-thread-buffer-addresses t     ; Show addresses for thread frames in threads buffer
   gdb-non-stop-setting t            ; Try to use non-stop mode
   gdb-switch-when-another-stopped t ; Switch to a thread that stopped even if we're already stopped in a thread
   gdb-show-changed-values t         ; Highlight changed/out of scope variables
   gdb-delete-out-of-scope nil       ; Keep out of scope variables
   gdb-use-colon-colon-notation t    ; Use function::variable format
   gdb-stack-buffer-locations t      ; Show file information or library names in stack buffers
   gdb-stack-buffer-addresses t      ; Show frame addresses in stack buffers
   )
  (defun gdb-frame ()
    (interactive)
    (select-frame (make-frame '((name . "GDB"))))
    (toggle-frame-maximized)
    (call-interactively 'gdb))
  :bind
  (("C-c g a" . gdb-display-disassembly-buffer)
   ("C-c g m" . gdb-display-memory-buffer)
   ("C-c g r" . gdb-restore-windows)))

;; cmake-ide
(use-package cmake-ide
  :disabled
  :demand t
  :init
  ;; Persistent build directories under `XDG_CACHE_HOME/cmake-ide/`
  (when-let ((cache-dir (getenv "XDG_CACHE_HOME")))
    (customize-set-variable 'cmake-ide-build-pool-dir (concat (file-name-as-directory (expand-file-name cache-dir "~"))
                                                              "cmake-ide"))
    (customize-set-variable 'cmake-ide-build-pool-use-persistent-naming t))
  ;; Use `cmake --build <DIR>` as the compile command
  ;; We advise the function because the custom variable `cmake-ide-compile-command` is not given the build directory
  (defun abz--cide-get-compile-command (dir)
    (let ((r (combine-and-quote-strings `(,cmake-ide-cmake-command "--build" ,dir))))
      (message "abz--cide-get-compile-command(%s) => %s" dir r)
      r)
    )
  (advice-add 'cide--get-compile-command :before-until #'abz--cide-get-compile-command)
  :config
  ;; Add `-DCPM_SOURCE_CACHE=$XDG_CACHE_HOME/CPM` (for CPM.cmake) when necessary
  (when-let ((cache-dir (and (not (getenv "CPM_SOURCE_CACHE")) (getenv "XDG_CACHE_HOME"))))
    (add-to-list 'cmake-ide-cmake-args
                 (combine-and-quote-strings `("-DCPM_SOURCE_CACHE"
                                              ,(concat (file-name-as-directory (expand-file-name cache-dir "~")) "CPM"))
                                            "=")))
  (add-to-list 'cmake-ide-src-extensions ".cu")
  (cmake-ide-setup))

;; YAML
(use-package yaml-mode
  :init)

;; TOML
(use-package toml-mode)

;; Rust
(use-package rust-mode
  :disabled
  :init
  (customize-set-variable 'rust-format-on-save nil)) ; run rustfmt on save

;; Code completion
(use-package racer
  :disabled
  :custom
  (racer-rust-src-path (f-join (s-trim-right
                                (shell-command-to-string
                                 (format "%s --print sysroot" (executable-find "rustc")))) "lib/rustlib/src/rust/library"))
  :hook ((rustic-mode . racer-mode)
         (rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

;; Cargo commands
(use-package cargo
  :disabled
  :after rust-mode
  :init
  :hook (rust-mode . cargo-minor-mode))

(use-package rustic
  :custom
  (rustic-lsp-client 'lsp-mode "LSP frontend (`lsp-mode' or `eglot')")
  (rustic-lsp-server 'rust-analyzer "Which LSP server to use (`rust-analyzer' or `rls')")
  (rustic-lsp-setup-p t "Setup LSP related stuff automatically")
  ;; Path to rust std sources changed from `src` subdirectory to `library`.
  (rustic-racer-rust-src-path (f-join (s-trim-right
                                       (shell-command-to-string (format "%s --print sysroot" (executable-find "rustc"))))
                                      "lib/rustlib/src/rust/library"))
  :config
  ;; Debug
  (advice-add #'rustic-compile :before (lambda () (message "=~= abz: Call to rustic-compile")))
  (advice-add #'rustic-format-buffer :before (lambda () (message "=~= abz: Call to rustic-format-buffer"))))

(use-package dockerfile-mode
  :init
  (customize-set-variable 'dockerfile-use-sudo t))

(use-package ssh-config-mode)

;; Buttonize URL and e-mail addresses in comments and strings.
(use-package goto-addr
  :straight nil
  :hook (prog-mode . goto-address-prog-mode))

;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(use-package eldoc-mode
  :straight nil
  :init
  (customize-set-variable 'eldoc-idle-delay 0)           ; Delay before printing
  (customize-set-variable 'eldoc-minor-mode-string nil)) ; Don't show in modeline

(use-package csharp-mode)

(use-package elpy
  :disabled
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable)))

;; Python env manager
(use-package poetry)

;; Arch Linux PKGBUILD
(use-package pkgbuild-mode)

;; Fish shell
(use-package fish-mode)

(use-package abz-clean-mode
  :straight nil
  :hook (after-init . abz-clean-global-mode))

;; Load submodules
(add-to-list 'load-path (expand-file-name "languages"
                                          (file-name-directory load-file-name)))
(mapc 'require (list
                'abz-golang
                'abz-web
                ))

(require 'abz-diagnostics)
(require 'abz-company)
(require 'abz-lsp)

(provide 'abz-devel)

;;; abz-devel.el ends here
