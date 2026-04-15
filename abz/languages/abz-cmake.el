;;; abz-cmake.el --- CMake language integration -*- lexical-binding: t; -*-

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

;; cmake-ts-mode (Emacs built-in) ships font-lock queries incompatible with
;; the available tree-sitter-cmake grammar on Emacs <= 30.2.  Fall back to the
;; legacy cmake-mode on affected versions.  Bump `abz-cmake-ts-min-version'
;; after verifying cmake-ts-mode works correctly on a newer Emacs release.

;;

;;; Code:

(defcustom abz-cmake-ts-min-version "30.3"
  "Minimum Emacs version at which `cmake-ts-mode' is considered safe to use.
On versions strictly below this string, cmake-mode is used instead."
  :tag "Minimum Emacs version to use cmake-ts-mode"
  :type 'string
  :group 'abz)

(defun abz-cmake--ts-mode-usable-p ()
  "Return non-nil if `cmake-ts-mode' is safe on the running Emacs version."
  (version<= abz-cmake-ts-min-version emacs-version))

(use-package cmake-ts-mode
  :if (abz-cmake--ts-mode-usable-p)
  :straight nil
  :init
  (abz--log "abz: before cmake-ts-mode")
  :config
  (abz--log "abz: after cmake-ts-mode")
  :mode ("\\(CMakeLists\\.txt\\|\\.cmake\\)\\'"))

(use-package cmake-mode
  :if (not (abz-cmake--ts-mode-usable-p))
  :straight nil
  :preface
  (cl-defun abz--cleanup-treesit-auto-recipe-list ()
    (when (boundp 'treesit-auto-recipe-list)
      (abz--log "abz: clean treesit-auto-recipe-list")
      (cl-delete-if (lambda (r)
                      (eq (treesit-auto-recipe-lang r) 'cmake))
                    treesit-auto-recipe-list)))
  :init
  (abz--log "abz: before cmake-mode")
  :config
  (abz--log "abz: after cmake-mode")
  :hook (global-treesit-auto-mode . abz--cleanup-treesit-auto-recipe-list)
  :mode ("\\(CMakeLists\\.txt\\|\\.cmake\\)\\'"))

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
      r))
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

(provide 'abz-cmake)

;;; abz-cmake.el ends here
