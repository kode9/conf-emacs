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
(use-package clang-format)

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
  (setq fic-highlighted-words '("TODO" "BUG" "HACK" "FIXME" "KLUDGE"))
  (setq fic-foreground-color "#000000") ;; Black
  (setq fic-background-color "#DFAF8F") ;; Orange
  (add-hook 'prog-mode-hook 'turn-on-fic-mode))

;; Projectile: project management
(use-package projectile
  :init (projectile-global-mode)
  :config
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (setq projectile-find-dir-includes-top-level t) ;; Add top-level dir to projectile-find-dir
  (setq projectile-mode-line '(:propertize
			       (:eval (concat " " (projectile-project-name)))
			       face font-lock-constant-face)))

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
  :mode ("\\.cmake\\'" "/CMakeLists\\.txt\\'")
  :init
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate) ;; Better syntax highlightning
  :config
  (use-package cmake-font-lock))

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

(provide 'pluc-devel)
