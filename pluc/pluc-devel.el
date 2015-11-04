;; Development packages and customisation

;; Enclose cursor within punctuation pairs.
;; https://github.com/rejeep/enclose.el
(use-package enclose
  :disabled t
  ;; :init
  ;; (add-to-list 'enclose-except-modes 'ido-mode)
  :config
  (enclose-global-mode t))

;; Wrap text with punctation or tag
;; https://github.com/rejeep/wrap-region.el
(use-package wrap-region
  :diminish wrap-region-mode
  :config
  (wrap-region-global-mode t))

;; clang-format
(use-package clang-format
  :load-path "/usr/share/clang")

(defun format-buffer()
  "Format buffer if clang-format is available and the buffer is cc-mode derived, otherwise call indent-buffer"
  (interactive)
  (unless (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode (clang-format-buffer) t) (indent-buffer)))

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
  (setq which-func-unknown "∅") ;; Displayed when current function is unknown
  )

;; Beautify the operators in codes
;;; Requires tuning/patching for a lot of things: templates, includes,
;;; etc. http://www.emacswiki.org/emacs/SmartOperator
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
  (setq
   projectile-find-dir-includes-top-level t ;; Add top-level dir to projectile-find-dir
   projectile-mode-line '(:propertize
			  (:eval (concat " " (projectile-project-name)))
			  face font-lock-constant-face))
  :config
  (projectile-global-mode)
  (run-with-idle-timer 59 t #'projectile-cleanup-known-projects))

;; Auto completion
(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.1) ;; Seconds before starting completion
  (setq company-minimum-prefix-length 2) ;; Minimum numbers of characters to start completion
  (setq company-tooltip-align-annotations t) ;; Align annotations to the right tooltip border
  (setq company-show-numbers  t) ;; Quick access to first 10 candidates
  (setq company-transformers '(company-sort-by-occurrence)) ;; Sort candidates by occurence
  (delete 'company-semantic company-backends) ;; Remove semantic backend
  )

;;;;;;;;;;;;;;;;;
;; Major Modes ;;
;;;;;;;;;;;;;;;;;

;; C++
(use-package cc-mode
  :mode ("\\.\\(?:inl\\|h\\)\\'" . c++-mode))

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
  :mode (("/PKGBUILD\\'" . sh-mode) ;; Arch Linux PKGBUILD
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
  (remove-hook 'cmake-mode-hook 'cmake-font-lock-activate) ;; package autoloads
  (defun pluc-cmake-font-lock-activate()
    "Call cmake-font-lock-activate only for cmake-mode."
    (when (derived-mode-p #'cmake-mode)
      (cmake-font-lock-activate)))
  (add-hook 'prog-mode-hook #'pluc-cmake-font-lock-activate))

;; Scilab
(use-package scilab-mode
  :mode "\\.\\(?:sci\\|sce\\)\\'")

;; SQL
(use-package sql
  :functions sql
  :mode ("\\.sql\\'" . sql-mode)
  :config
  (use-package sql-indent) ;; Better indentation
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
   gdb-many-windows t ;; Multiple window layout
   gdb-show-main t ;; Display both gud and the main source (if no many-windows)
   gdb-thread-buffer-verbose-names t ;; Show long thread names like ‘Thread 0x4e2ab70 (LWP 1983)’
   gdb-thread-buffer-arguments t ;; Show function arguments in threads buffer
   gdb-thread-buffer-locations t ;; Show file information or library names in threads buffer
   gdb-thread-buffer-addresses t ;; Show addresses for thread frames in threads buffer
   gdb-non-stop-setting t ;; Try to use non-stop mode
   gdb-switch-when-another-stopped t ;; Switch to a thread that stopped even if we're already stopped in a thread
   gdb-show-changed-values t ;; Highlight changed/out of scope variables
   gdb-delete-out-of-scope nil ;; Keep out of scope variables
   gdb-use-colon-colon-notation t ;; Use function::variable format
   gdb-stack-buffer-locations t ;; Show file information or library names in stack buffers
   gdb-stack-buffer-addresses t ;; Show frame addresses in stack buffers
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

(provide 'pluc-devel)
