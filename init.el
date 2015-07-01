;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file
;; Pierre-Luc Perrier <pluc@the-pluc.net>
;;
;; TODO
;; - Better backup and autosave
;;     '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
;;     '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
;; - Auto chmod +x: http://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave
;; - (setq-default save-place t)
;; - more documentation...
;; - user.el
;; - better kill ring (or even helm!)
;; - better undo/redo (check undo-tree)
;; - C++14
;; - Search regex
;;
;; REMINDER
;;   - ffap
;;   - re-builder
;;   - irony-mode
;;   - pkgbuild-mode
;;   - CMake
;;   - gitattributes-mode
;;   - gitconfig-mode
;;   - gitignore-mode
;;   - editorconfig
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cask: Project management for Emacs package development	        ;;
;; http://github.com/cask/cask					        ;;
;; 								        ;;
;; Automatic installation and updates of packages listed in a Cask file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cask (expand-file-name "cask/cask.el" user-emacs-directory))
(cask-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package: A use-package declaration for simplifying your .emacs ;;
;; https://github.com/jwiegley/use-package			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq use-package-verbose t) ;; Uncomment to see package loading in *Messages*
(eval-when-compile (require 'use-package))
(use-package bind-key :ensure :defer t) ;; :bind support for use-package
(use-package diminish :ensure :defer t) ;; :diminish support for use-package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pallet: A package management tool for Emacs, using Cask. ;;
;; https://github.com/rdallasgray/pallet		    ;;
;; 							    ;;
;; Keep track of installed packages using Cask		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pallet
  :ensure
  :demand
  :config (pallet-mode t))

;; Some non-packaged stuff
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submodules
;; Comment out the ones you do not want
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory where to find submodules
(eval-and-compile (setq pluc-dir (expand-file-name "pluc/" user-emacs-directory)))

(use-package pluc-custom :load-path pluc-dir) ;; Basic setup
(use-package pluc-theme :load-path pluc-dir) ;; Color theme (only zenburn ATM)
(use-package pluc-ido :load-path pluc-dir) ;; InteractivelyDoThings
(use-package pluc-editing :load-path pluc-dir) ;; Common edition settings
(use-package pluc-devel :load-path pluc-dir) ;; Development settings


;; Ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
               ("ADMS" (filename . "adms/"))
               ("DEV" (or
		       (mode . c++-mode)
		       (mode . c-mode)
		       (mode . python-mode)))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; remplace buffermenu

;; Bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks.cache")

;; windmove: move with alt + arrow
(windmove-default-keybindings 'super)

;; Show region size and maximum column number in mode-line
;;; http://www.emacswiki.org/emacs/modeline-posn.el
;;; Does not work with powerline
(set 'modelinepos-column-limit 100)

;; https://github.com/pmarinov/clean-aindent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; View, stage and revert Git changes straight from the buffer.
;;; https://github.com/nonsequitur/git-gutter-plus
(global-git-gutter+-mode t)

;; Autoscoll compilation buffer and stop on first error
(set 'compilation-scroll-output 'first-error)
;; Skip warnings when jumping between errors
(set 'compilation-skip-threshold 2)

;; Shorten long file-name targets
;;; https://github.com/lewang/scf-mode
;;; Seems to work in grep, but not in compile :(
(autoload 'scf-mode "scf-mode" "SCF Mode" t)
(add-hook 'compilation-mode-hook (lambda () (scf-mode t)))

;; Stop asking yes/no before compile when a compilation is already running
;;; ftp://download.tuxfamily.org/user42/compilation-always-kill.el
(autoload 'compilation-always-kill-mode "compilation-always-kill" "Compilation kill" t)
(compilation-always-kill-mode t)

;; Color FIXME/TODO/BUG/KLUDGE in comments and strings
(add-hook 'c-mode-common-hook 'turn-on-fic-mode)

;; Banner comments
(autoload 'line-comment-banner "line-comment-banner" "Comment banner" t)
(global-set-key [(control c) (b)] (lambda () (interactive) (line-comment-banner 80)))

;; Major modes
;;; Load
(autoload 'glsl-mode "glsl-mode" nil t)
(autoload 'qml-mode "qml-mode" "QML mode" t)
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(autoload 'cmake-mode "cmake-mode" "Cmake Mode." t)
(autoload 'cuda-mode "cuda-mode" "Cuda Mode." t)
(autoload 'apache-mode "apache-mode" nil t)
(autoload 'haxe-mode "haxe-mode" "Haxe Mode." t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(autoload 'scilab-mode "scilab-mode" "Scilab mode." t)
;;;; Filename patterns
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\|mdwn\\|mdml\\)\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.\\(sci\\|sce\\)\\'" . scilab-mode))
(add-to-list 'auto-mode-alist '("\\.axl\\'" . xml-mode)) ; Axel modeler
(add-to-list 'auto-mode-alist '("\\.F90\\'" . f90-mode))
(add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.xrdb\\'" . conf-xdefaults-mode))
(add-to-list 'auto-mode-alist '("\\.\\(glsl\\|vert\\|frag\\|geom\\|vs\\|ksh\\|fs\\|gs\\)\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.lk\\'" . js-mode)) ; LeekScript (LeekWars)
(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;; Minor modes
(autoload 'align-string "align-string" "Align string." t)
;;; Insert operators with surrounding spaces smartly
(autoload 'smart-operator "smart-operator-mode" "Smart operator." t)
;;; https://github.com/alamaison/emacs-cmake-project
(autoload 'cmake-project-mode "cmake-project")
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

;;; Better SQL indentation
(eval-after-load "sql" (load-library "sql-indent"))

;; Custom hooks
(defun dtw()
  "Delete trailing whitespaces"
  (interactive)
  (delete-trailing-whitespace))

(defun indent-buffer()
  "Indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun dev-hooks()
  "Progmod hooks"
  (format-buffer))

(define-minor-mode pluc-mode
  "Clean buffers."
  :lighter " pluc"
  :global t
  (if pluc-mode
      (progn
        (add-hook 'before-save-hook 'dev-hooks nil t))
    (remove-hook 'before-save-hook 'dev-hooks t)))

;; Clean for any files
(add-hook 'before-save-hook 'dtw)
(add-hook 'prog-mode-hook #'pluc-mode)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; DuckDuckGo
(require 'ddg-search) ;; Search functions. Need both ddg (packaged) and ddg-mode

;;; Additional function to search the current region. Copy/paste from google-search.el
(defvar ddg-search-maxlen 50
  "Maximum string length of search term.  This prevents you from accidentally
sending a five megabyte query string to Netscape.")

(defun duckduckgo-region ()
  "Search the current region on DuckDuckGo."
  (interactive)
  (let (start end term url)
    (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
        (progn
          (setq start (region-beginning)
                end   (region-end))
          (if (> (- start end) ddg-search-maxlen)
              (setq term (buffer-substring start (+ start ddg-search-maxlen)))
            (setq term (buffer-substring start end)))
          (duckduckgo-web term))
      (beep)
      (message "Region not active"))))

(global-set-key [(control c) (control s)] 'duckduckgo-web)
(global-set-key [(control c) (s)] 'duckduckgo-region)

;; keys
(global-set-key [(control c) (c)] 'comment-or-uncomment-region)
;; (global-set-key [(control c) (v)] 'uncomment-region)
(global-set-key [(control c) (x)] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control x) (control k)] 'kill-some-buffers)

;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (diff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

