;;; early-init.el --- -*- lexical-binding: t; -*-

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

;;; Code:

;; Move native compiled files to XDG_CACHE_HOME
;;
;; NOTE As of 2021-02-28 this does not work completely yet, there are still a
;; few binaries created in `user-emacs-directory'/eln-cache/. Removing the
;; directory from `comp-eln-load-path' and or deleting it from the file system
;; will just make it recompile them at the next startup.
(eval-and-compile
  (when (boundp 'comp-eln-load-path)
    (let* ((env (getenv "XDG_CACHE_HOME"))
           (cache-home (if (or (null env) (not (file-name-absolute-p env)))
                           (expand-file-name "~/.cache")
                         env))
           (comp-eln-default-load-path (expand-file-name "eln-cache/" user-emacs-directory))
           (comp-eln-new-load-path (expand-file-name
                                    (convert-standard-filename "emacs/native/")
                                    cache-home)))
      (unless (file-equal-p comp-eln-default-load-path comp-eln-new-load-path)
        (push comp-eln-new-load-path comp-eln-load-path)))))

;; Raise garbage collector thresholds for initialization to improve startup
;; time. They will be restored after initialization (see below).
;; https://emacs.stackexchange.com/a/34367
(customize-set-variable 'gc-cons-threshold (* 1000 1000 1000))
(customize-set-variable 'gc-cons-percentage 90)

;; Package.el
(customize-set-variable 'package-enable-at-startup nil "Disable package.el")

;; Lisp
(customize-set-variable 'load-prefer-newer t "Don't load expired byte-compiled files")
(customize-set-variable 'ad-redefinition-action 'accept "Do not warn about advice redefinitions")

;; Debug
(customize-set-variable 'message-log-max 500 "Keep that many lines in the message buffer")

;; Initialization
(customize-set-variable 'inhibit-startup-screen t "Inhibits the startup screen")
(customize-set-variable 'initial-buffer-choice nil "Starts with the *scratch* buffer if no file passed")
(customize-set-variable 'initial-major-mode 'fundamental-mode "Major mode for the *scratch* buffer")
(customize-set-variable 'initial-scratch-message nil "No message in the *scratch* buffer")

;; Frames
(setq frame-title-format "⸗ %b (%&) ⸗") ; Title bar format
(customize-set-variable 'menu-bar-mode nil "No menu")
(when (fboundp 'tool-bar-mode)
  (customize-set-variable 'tool-bar-mode nil "No toolbar"))
(customize-set-variable 'scroll-bar-mode nil "No scrollbar")
(customize-set-variable 'display-time-mode nil "No time / load / mail in modeline")
(customize-set-variable 'size-indication-mode nil "No buffer size in modeline")
(customize-set-variable 'line-number-mode nil "Display current line in modeline")
(customize-set-variable 'column-number-mode nil "Display current column in modecolumn")
(customize-set-variable 'initial-frame-alist '((fullscreen . maximized)) "Start the initial frame maximized")
(customize-set-variable 'frame-resize-pixelwise t "frame sizes can increase/decrease by one pixel")
(customize-set-variable 'window-divider-default-places 'right-only)
(customize-set-variable 'window-divider-default-bottom-width 1)
(customize-set-variable 'window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Cursor
(customize-set-variable 'cursor-in-non-selected-windows 'hollow "Cursor when window is not selected")

;; Display
(customize-set-variable 'ctl-arrow nil "Display control characters as '\xx'")
(customize-set-variable 'cursor-type '(hbar . 4) "Cursor when window is selected")
(customize-set-variable 'highlight-nonselected-windows t "Keep Highlightning region")
(customize-set-variable 'visible-bell nil "Don't try the flash")
(customize-set-variable 'ring-bell-function 'ignore "Don't ring the bell")
(customize-set-variable 'truncate-lines nil "Don't truncate long lines (avoid horizontal scrolling)")
(customize-set-variable 'truncate-partial-width-windows 40 "Well still truncate if frame width is small")
(customize-set-variable 'word-wrap t "Wrap long lines")

;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;; Don't check file handlers during startup
;; https://github.com/MatthewZMD/.emacs.d#unset-file-name-handler-alist
(defvar abz--file-name-handler-alist-original file-name-handler-alist)

(defun abz--restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist' to its original value."
  (setq file-name-handler-alist abz--file-name-handler-alist-original)
  (makunbound 'abz--file-name-handler-alist-original))

(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook #'abz--restore-file-name-handler-alist)

;; Disable the use of X resources
;; https://github.com/raxod502/radian/blob/54f9680e81767dc5d036d2c4d6672021ca422784/emacs/early-init.el#L38
(defun abz--advice-disable-x-resource-application ()
  "Disable `x-apply-session-resources'.
I don't use them.")

(advice-add #'x-apply-session-resources :override
            #'abz--advice-disable-x-resource-application)

(provide 'earl-init)

;;; early-init.el ends here
