;;; abz-settings.el --- Config customization -*- lexical-binding: t; -*-

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

(require 'files)
(require 'subr-x)
(require 'use-package)
(require 'xdg)

(defgroup abz nil
  "Custom group."
  :prefix "abz-"
  :group 'emacs
  :link '(url-link :tag "Homepage" "https://git.the-pluc.net/conf-emacs.git/"))

(defcustom abz-debug-elisp nil
  "When t, add some debug messages."
  :type 'boolean
  :group 'abz)

(defconst abz--log-buffer-name "*abz-log*")

(defun abz--log-buffer-get ()
  "Get or create the `abz--log-buffer-name' buffer."
  (cond ((get-buffer abz--log-buffer-name))
        ((let ((buffer (get-buffer-create abz--log-buffer-name)))
           (with-current-buffer buffer
             (view-mode 1)
             (buffer-disable-undo))
           buffer))))

(defun abz--log (format &rest args)
  "Log messages to the `abz--log-buffer-name' buffer when `abz-debug-elisp' is t.

FORMAT and ARGS are the same as in `message'."
  (when abz-debug-elisp
    (with-current-buffer (abz--log-buffer-get)
      (let ((inhibit-read-only t)
            (message (apply 'format format args)))
        (goto-char (point-max))
        (insert message)
        (insert "\n")
        (goto-char (point-max))))))

(defun abz--make-set-custom-dir (&optional oldvalue)
  "Create a function to be used to set a `defcustom' directory variable.

The function takes a `SYMBOL' and a `VALUE' as arguments and set `SYMBOL' to
`VALUE' and call `make-directory' on `VALUE'.

If optional `OLDVALUE' is non-nil it will first try to rename `OLDVALUE' to
`VALUE'."
  (lambda (symbol value)
    (when (and (stringp oldvalue)
               (not (or (string-empty-p oldvalue)
                        (file-equal-p oldvalue value)))
               (file-exists-p oldvalue))
      (if (file-exists-p value)
          (warn "Both %s and %s exist" oldvalue value)
        (let ((filename (directory-file-name value)))
          (make-directory (file-name-directory filename) t)
          (rename-file oldvalue filename))))
    (set-default symbol value)
    (make-directory value t)))

(defcustom abz-config-local-dir
  (abbreviate-file-name (expand-file-name "local/" user-emacs-directory))
  "Directory used to store local configuration files."
  :type 'directory
  :group 'abz
  :group 'environment
  :set (abz--make-set-custom-dir (expand-file-name "local/" user-emacs-directory)))

(defcustom abz-data-dir
  (abbreviate-file-name (expand-file-name "emacs/" (xdg-data-home)))
  "Directory used to store data files."
  :type 'directory
  :group 'abz
  :group 'environment
  :set (abz--make-set-custom-dir (expand-file-name ".cache/" user-emacs-directory)))

(defcustom abz-cache-dir
  (abbreviate-file-name (expand-file-name "emacs/" (xdg-cache-home)))
  "Directory used to store runtime cache files."
  :type 'directory
  :group 'abz
  :group 'environment
  :set (abz--make-set-custom-dir))

(defcustom abz-undo-limit (* 128 1000 1000)
  "Keep no more undo information once it exceeds this size."
  :type 'integer
  :group 'abz)

(defcustom abz-undo-strong-limit (* 256 1000 1000)
  "Don't keep more than this much size of undo information."
  :type 'integer
  :group 'abz)

(defcustom abz-undo-outer-limit (* 512 1000 1000)
  "Outer limit on size of undo information for one command."
  :type 'integer
  :group 'abz)

(defun abz--locate-config-file (filename)
  "Return an absolute path to `FILENAME' under `abz-config-local-dir'."
  (expand-file-name (convert-standard-filename filename)
                    abz-config-local-dir))

(defun abz--locate-data-dir (dirname)
  "Make sure `DIRNAME' exists under `abz-data-dir' and return its absolute path."
  (let ((dir (expand-file-name (convert-standard-filename dirname)
                               abz-data-dir)))
    (make-directory dir t)
    dir))

(defun abz--locate-cache-dir (dirname)
  "Make sure `DIRNAME' exists under `abz-cache-dir' and return its absolute path."
  (let ((dir (expand-file-name (convert-standard-filename dirname)
                               abz-cache-dir)))
    (make-directory dir t)
    dir))

(defcustom abz-custom-file
  (abz--locate-config-file "custom.el")
  "File used to store settings from Customization UI."
  :set-after '(abz-config-local-dir)
  :type 'file
  :group 'abz
  :group 'environment)

(defun abz-find-custom-file()
  "Open the custom file."
  (interactive)
  (find-file abz-custom-file))

(use-package no-littering
  :demand t
  :commands
  (no-littering-theme-backups)
  :init
  (setq no-littering-etc-directory abz-config-local-dir)
  (setq no-littering-var-directory abz-data-dir)
  :config
  (no-littering-theme-backups))

(defconst abz--os-is-arch?
  (and (executable-find "pacman") t))

(defun abz-os-is-arch? ()
  "Return t if the current OS is Arch Linux."
  abz--os-is-arch?)

(defconst abz--os-is-debian-derivative?
  (and (executable-find "apt") t))

(defun abz-os-is-debian-derivative? ()
  "Return t if the current OS is Debian or a derivative."
  abz--os-is-debian-derivative?)

;; Custom file location
(setq custom-file abz-custom-file)
(load custom-file 'no-error 'no-message)

;;; Completion framework

;;;###autoload
(defcustom abz-completion-framework 'vertico
  "The completion frontend to use."
  :type '(radio
          (const :tag "Built-in" built-in)
          (const :tag "Vertico" vertico))
  :tag "Completion framework"
  :group 'abz
  :group 'convenience)

;;; Search and navigation

(defcustom abz-search-backend
  (cond
   ((or (executable-find "rg") (executable-find "ripgrep")) 'ripgrep)
   ((executable-find "ag") 'ag)
   (t 'grep))
  "The project-wide search tool to use.
Consult variants provide live preview and embark integration.
Non-consult variants use dedicated packages."
  :type '(radio
          (const :tag "Consult + ripgrep" consult-ripgrep)
          (const :tag "Consult + grep" consult-grep)
          (const :tag "ripgrep (rg.el)" ripgrep)
          (const :tag "The Silver Searcher (ag.el)" ag)
          (const :tag "grep (built-in)" grep))
  :tag "Search backend"
  :group 'abz
  :group 'external)

(defcustom abz-line-search 'consult-line
  "In-buffer search command bound to \\[isearch-forward].
`consult-line' provides minibuffer-based search with live narrowing.
`ctrlf' provides modernized isearch (browser-style).
`isearch' is the built-in incremental search."
  :type '(radio
          (const :tag "consult-line" consult-line)
          (const :tag "ctrlf" ctrlf)
          (const :tag "isearch (built-in)" isearch))
  :tag "In-buffer search"
  :group 'abz
  :group 'convenience)

(defcustom abz-buffer-switcher 'consult-buffer
  "Buffer switching command bound to \\[switch-to-buffer].
`consult-buffer' shows buffers, recent files, and bookmarks with live preview."
  :type '(radio
          (const :tag "consult-buffer" consult-buffer)
          (const :tag "Default" default))
  :tag "Buffer switcher"
  :group 'abz
  :group 'convenience)

(defcustom abz-compile-navigation 'consult
  "Compilation error navigation style.
`consult' provides minibuffer-based jump-to-any-error with live preview.
`default' uses the custom compilation-mode-map bindings (n/p/C-n/C-p/M-n/M-p)."
  :type '(radio
          (const :tag "Consult" consult)
          (const :tag "Default" default))
  :tag "Compilation navigation"
  :group 'abz
  :group 'tools)

(defcustom abz-diagnostics-list 'consult
  "Error list command bound to \\[flycheck-list-errors].
Independent from `abz-diagnostics-backend' which controls the checker engine."
  :type '(radio
          (const :tag "Consult" consult)
          (const :tag "Flycheck" flycheck)
          (const :tag "Flymake" flymake))
  :tag "Diagnostics list"
  :group 'abz
  :group 'tools)

(defcustom abz-xref-show 'consult
  "Xref results display backend.
`consult' shows results in the minibuffer with live preview and embark actions.
`default' uses the built-in xref buffer."
  :type '(radio
          (const :tag "Consult" consult)
          (const :tag "Default" default))
  :tag "Xref display"
  :group 'abz
  :group 'tools)

(defcustom abz-imenu 'disabled
  "Symbol navigation via imenu.
When set to a non-disabled value, also enables `lsp-enable-imenu'."
  :type '(radio
          (const :tag "Consult" consult)
          (const :tag "Default" default)
          (const :tag "Disabled" disabled))
  :tag "Imenu"
  :group 'abz
  :group 'convenience)

(defcustom abz-yank-handler 'consult
  "Kill ring browsing command bound to \\[yank-pop].
`browse-kill-ring' on \\`C-S-y' is always available regardless of this setting."
  :type '(radio
          (const :tag "Consult" consult)
          (const :tag "browse-kill-ring" browse-kill-ring)
          (const :tag "Default" default))
  :tag "Yank handler"
  :group 'abz
  :group 'convenience)

(defcustom abz-proportional-window-resize t
  "When non-nil, splitting a window resizes all windows proportionally.
When nil, only the split window is halved."
  :type 'boolean
  :tag "Proportional window resize"
  :group 'abz
  :group 'convenience)

;;; Display and scrolling

(defcustom abz-fast-scroll t
  "When non-nil, skip fontification when scrolling at high speed.
Trades precision for responsiveness in large buffers."
  :type 'boolean
  :tag "Fast scrolling"
  :group 'abz)

(defcustom abz-pixel-scroll t
  "When non-nil, enable pixel-precise smooth scrolling.
Uses `pixel-scroll-precision-mode' for smoother scrolling on modern displays."
  :type 'boolean
  :tag "Pixel-precise scrolling"
  :group 'abz)

(defcustom abz-switch-to-buffer-obey-display-actions t
  "When non-nil, `switch-to-buffer' respects `display-buffer-alist' rules.
Makes window management more predictable with custom display rules."
  :type 'boolean
  :tag "Obey display actions on buffer switch"
  :group 'abz
  :group 'convenience)

;;; Development

(defcustom abz-next-error-highlight t
  "Highlight error messages in the compilation buffer.
When t, highlight the current error message.
When `keep', highlight all visited error messages.
When nil, no highlighting."
  :type '(choice (const :tag "Highlight current error" t)
                 (const :tag "Highlight all visited errors" keep)
                 (const :tag "No highlighting" nil))
  :tag "Highlight error messages"
  :group 'abz
  :group 'tools)

(defun abz--consult-needed-p ()
  "Return non-nil if any defcustom selects a consult variant."
  (or (memq abz-search-backend '(consult-ripgrep consult-grep))
      (eq abz-line-search 'consult-line)
      (eq abz-buffer-switcher 'consult-buffer)
      (eq abz-compile-navigation 'consult)
      (eq abz-diagnostics-list 'consult)
      (eq abz-xref-show 'consult)
      (eq abz-imenu 'consult)
      (eq abz-yank-handler 'consult)))

(provide 'abz-settings)

;;; abz-settings.el ends here
