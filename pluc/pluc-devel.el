;;; pluc-devel.el --- Development packages -*- lexical-binding: t; -*-

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

;;;###autoload
(cl-defun abz-process-window (process &optional (all-frames (selected-frame)))
  "Get the window currently displaying the buffer of PROCESS, or nil if none.

ALL-FRAMES specify which frames to consider as described in `get-buffer-window'.
"
  (get-buffer-window (process-buffer process) all-frames))

;;;###autoload
(defun abz-select-process-window (process)
  "Select the window currently displaying the buffer of PROCESS."
  (select-window (abz-process-window process) 'norecord))

;; Compilation
(use-package compile
  :ensure nil
  :defer t
  :init
  ;; Kill a running compilation process without asking before starting a new one
  (customize-set-variable 'compilation-always-kill t)
  ;; Autoscoll compilation buffer and stop on first error
  (customize-set-variable 'compilation-scroll-output 'first-error)
  ;; Automatically jump to the first error during compilation
  (customize-set-variable 'compilation-auto-jump-to-first-error nil)
  ;; Skip 'info' and 'warnings' when jumping between errors
  (customize-set-variable 'compilation-skip-threshold 2)
  :bind (:map compilation-mode-map
              ("n" . compilation-next-error)
              ("p" . compilation-previous-error)
              ("M-p" . compilation-previous-file)
              ("M-n" . compilation-next-file))
  :hook
  ;; Switch to compilation buffer window
  (compilation-start . abz-select-process-window))

;; Wrap text with punctation or tag
;; https://github.com/rejeep/wrap-region.el
(use-package wrap-region
  :diminish wrap-region-mode
  :config
  (wrap-region-global-mode t))

;; clang-format
(use-package clang-format
  :load-path ("/usr/share/clang"))

(defun format-buffer()
  "Format buffer if clang-format is available and the buffer is cc-mode derived, otherwise call indent-buffer"
  (interactive)
  (unless
      (and
       (member major-mode '(c-mode c++-mode))
       (clang-format-buffer)
       t)
    (indent-buffer)))

;; git-gutter+: View, stage and revert Git changes straight from the
;; buffer.
;;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :disabled t
  :defer t
  :functions git-gutter+-mode
  :diminish git-gutter+-mode
  :init
  (add-hook 'prog-mode-hook 'git-gutter+-mode))

;; which-function: Show function at cursor
(use-package which-func
  :init (which-function-mode nil)
  :config
  (setq which-func-unknown "∅") ; Displayed when current function is unknown
  )

;; Beautify the operators in codes.Requires tuning/patching for a lot
;; of things: templates, includes,
;; etc. http://www.emacswiki.org/emacs/SmartOperator
(use-package smart-operator
  :disabled t
  :defer t
  :commands smart-insert-operator-hook
  :init
  (add-hook 'c-mode-common-hook 'smart-insert-operator-hook))

;; Color keywords such as TODO in comments and strings
(use-package fic-mode
  :defer t
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
(use-package projectile
  :init
  (customize-set-variable 'projectile-known-projects-file
                          (expand-file-name
                           ".cache/projectile-bookmarks.eld" user-emacs-directory)) ; known projects file
  (customize-set-variable 'projectile-switch-project-action #'magit-status) ; Function to call when switching project
  (customize-set-variable 'projectile-find-dir-includes-top-level t) ; Add top-level dir to projectile-find-dir
  (customize-set-variable 'projectile-mode-line '(:propertize
                                                  (:eval
                                                   (format "[%s]"
                                                           (projectile-project-name)))
                                                  face font-lock-constant-face)) ; modeline
  :config
  (projectile-mode t)
  (run-with-idle-timer 59 t #'projectile-cleanup-known-projects))

;;;;;;;;;;;;;;;;;
;; Major Modes ;;
;;;;;;;;;;;;;;;;;

;; C++
(use-package cc-mode
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
  :mode (("/PKGBUILD\\'" . sh-mode) ; Arch Linux PKGBUILD
         ("\\.zsh\\'" . sh-mode)))

;; CSV
(use-package csv-mode
  :mode "\\.csv\\'")

;; CMake
(use-package cmake-mode
  :defer t
  ;; This seems to be already done by the package
  ;; :mode "\\.cmake\\'" "/CMakeLists\\.txt\\'"
  )

;; Better syntax highlightning for CMake
;;; Disabled: It is VERY slow. I'm not sure if it's a clash with
;;; something else.
(use-package cmake-font-lock
  :disabled t
  :defer t
  :commands cmake-font-lock-activate
  :init
  ;; cmake-font-lock-activate must be called BEFORE fic-mode. Since
  ;; cmake-mode will call prog-mode hooks and then after cmake-mode
  ;; hooks, the workaroung is to add cmake-font-lock-activate at the
  ;; start of the prog-mode hooks.
  (remove-hook 'cmake-mode-hook 'cmake-font-lock-activate) ; package autoloads
  (defun pluc-cmake-font-lock-activate()
    "Call cmake-font-lock-activate only for cmake-mode."
    (when (derived-mode-p #'cmake-mode)
      (cmake-font-lock-activate)))
  (add-hook 'prog-mode-hook #'pluc-cmake-font-lock-activate))

;; Scilab
(use-package scilab-mode
  :ensure nil
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
  :defer t
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
  :disabled t
  :config
  (customize-set-variable 'cmake-ide-src-extensions  ; A list of file extensions that qualify as source files.
                          '(".c" ".cpp" ".C" ".cxx" ".cc" ".cu"))
  (cmake-ide-setup))

;; Auto completion
(use-package company
  :diminish company-mode
  :init
  (setq
   company-idle-delay 0.1                              ; Seconds before starting completion
   company-minimum-prefix-length 2                     ; Minimum numbers of characters to start completion
   company-tooltip-align-annotations t                 ; Align annotations to the right tooltip border
   company-show-numbers  t                             ; Quick access to first 10 candidates
   company-transformers '(company-sort-by-occurrence)) ; Sort candidates by occurence
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends)) ; Remove CEDET
  (use-package company-c-headers
    :config
    (add-to-list 'company-c-headers-path-system "/usr/include/c++/v1/")
    (add-to-list 'company-backends 'company-c-headers nil))
  (use-package company-quickhelp
    :init (setq company-quickhelp-delay 0.75)
    :config (company-quickhelp-mode 1)))

;; On the fly error checking
(use-package flycheck
  :diminish flycheck-mode
  :init
  (customize-set-variable 'flycheck-highlighting-mode 'sexps) ; Highlight whole expression around the error column
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  (("C-c f f" . flycheck-next-error)
   ("C-c f p" . flycheck-previous-error)
   ("C-c f l" . flycheck-list-errors)
   ("C-c f c" . flycheck-clear))
  :config
  (use-package flycheck-cask)
  (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)
  (use-package flycheck-pos-tip ; Show errors under point in pos-tip popups
    :init
    (customize-set-variable 'flycheck-pos-tip-timeout 10)
    :config
    (flycheck-pos-tip-mode)))

(use-package web-mode
  :init
  (customize-set-variable 'web-mode-code-indent-offset 2)
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  :mode
  "\\.php\\'")

;; YAML
(use-package yaml-mode
  :defer t
  :init
  (use-package flycheck-yamllint
    :defer t
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-yamllint-setup)))

;; TOML
(use-package toml-mode
  :defer t)

;; rust
(use-package rust-mode
  :defer t
  :init
  (customize-set-variable 'rust-format-on-save t) ; run rustfmt on save
  ;; cargo commands
  (use-package cargo
    :defer t
    :init
    (add-hook 'rust-mode-hook #'cargo-minor-mode))
  ;; Flycheck integration
  (use-package flycheck-rust
    :defer t
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  ;; Code completion
  (use-package racer
    :defer t
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

(use-package dockerfile-mode
  :defer t
  :init
  (customize-set-variable 'dockerfile-use-sudo t))

(use-package ssh-config-mode)

(provide 'pluc-devel)
;;; pluc-devel.el ends here
