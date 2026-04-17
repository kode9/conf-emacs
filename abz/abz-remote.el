;;; abz-remote.el --- Remote work configuration -*- lexical-binding: t; -*-

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

;; Centralized configuration for remote development over TRAMP and
;; display proxies (XPRA, waypipe).

;;; Code:

(require 'abz-settings)
(require 'use-package)

;;;; Customization

(defcustom abz-remote-tramp-shell "/usr/bin/dash"
  "Preferred shell for TRAMP internal commands on remote hosts.
Dash is POSIX-minimal with lower overhead than bash. Used for
TRAMP's own stat/test/cat operations, not for interactive shells."
  :type 'string
  :group 'abz)

(defcustom abz-remote-tramp-shell-fallback "/bin/sh"
  "Fallback shell if `abz-remote-tramp-shell' is not available."
  :type 'string
  :group 'abz)

(defcustom abz-remote-tramp-magit-lightweight t
  "When non-nil, reduce magit sections and features over TRAMP.
Drops expensive status sections (unpulled, unpushed, bisect, etc.)
and headers (tags, modules, etc.) to minimize SSH roundtrips."
  :type 'boolean
  :group 'abz)

(defcustom abz-remote-display-method 'xpra
  "Preferred display proxy for remote Emacs GUI sessions."
  :type '(radio
          (const :tag "XPRA (persistent sessions, X11/Wayland)" xpra)
          (const :tag "Waypipe (lightweight, Wayland-native)" waypipe))
  :group 'abz)

(defcustom abz-remote-daemon-name "work"
  "Default Emacs daemon socket name for remote sessions."
  :type 'string
  :group 'abz)

;;;; TRAMP core tuning

;; tramp: Transparent Remote Access, Multiple Protocol (built-in)
;; no-littering:
;;   - tramp-auto-save-directory
;;   - tramp-persistency-file-name
(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 1 "Only show errors (default 3). Reduces format overhead.")
  (tramp-completion-reread-directory-timeout 120
   "Seconds before re-reading directory during completion (default nil).")
  (tramp-backup-directory-alist
   `(("." . ,(abz--locate-data-dir "backup/tramp")))
   "Backup files location")
  :init
  ;; Disable version control for tramp files
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (use-package vc
    :straight nil
    :init
    (customize-set-variable 'vc-ignore-dir-regexp
                            (format "\\(%s\\)\\|\\(%s\\)"
                                    vc-ignore-dir-regexp
                                    tramp-file-name-regexp)))
  :config
  ;; Prevent remote programs from inheriting local fish shell
  (setenv "SHELL" "/bin/bash")
  ;; Reuse existing ControlMaster sockets (Emacs 30+)
  (when (boundp 'tramp-use-connection-share)
    (customize-set-variable 'tramp-use-connection-share t))
  ;; Cache remote file attributes longer (default 10s)
  (customize-set-variable 'remote-file-name-inhibit-cache 60))

;;;; Connection-local profiles

;; Use dash as TRAMP's internal shell for lower overhead.
;; Interactive shells (M-x shell) still use bash.
(connection-local-set-profile-variables
 'abz-remote-connection-profile
 `((shell-file-name . ,abz-remote-tramp-shell)
   (explicit-shell-file-name . "/bin/bash")))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh")
 'abz-remote-connection-profile)

(connection-local-set-profiles
 '(:application tramp :protocol "sshx")
 'abz-remote-connection-profile)

;;;; Magit over TRAMP

(defun abz--remote-magit-lighten ()
  "Reduce magit sections and features when visiting a remote repository.
Controlled by `abz-remote-tramp-magit-lightweight'."
  (when (and abz-remote-tramp-magit-lightweight
             (file-remote-p default-directory))
    ;; Minimal status sections: headers, unstaged, staged, untracked, stashes
    (setq-local magit-status-sections-hook
                '(magit-insert-status-headers
                  magit-insert-unstaged-changes
                  magit-insert-staged-changes
                  magit-insert-untracked-files
                  magit-insert-stashes))
    ;; Minimal headers: branch, upstream, errors
    (setq-local magit-status-headers-hook
                '(magit-insert-head-branch-header
                  magit-insert-upstream-branch-header
                  magit-insert-error-header))
    ;; Disable expensive features
    (setq-local magit-diff-refine-hunk nil)
    (setq-local magit-refs-show-commit-count nil)
    (setq-local magit-refresh-status-buffer nil)))

(with-eval-after-load 'magit
  (add-hook 'magit-status-mode-hook #'abz--remote-magit-lighten))

;; Suppress auto-revert for remote buffers
(with-eval-after-load 'autorevert
  (defun abz--remote-inhibit-auto-revert ()
    "Disable auto-revert-mode in remote buffers."
    (when (and buffer-file-name (file-remote-p buffer-file-name))
      (auto-revert-mode -1)))
  (add-hook 'find-file-hook #'abz--remote-inhibit-auto-revert))

;;;; LSP over TRAMP

;; Disable expensive features for remote buffers
(with-eval-after-load 'lsp-mode
  (defun abz--remote-lsp-tune ()
    "Tune LSP settings for remote buffers."
    (when (file-remote-p default-directory)
      (setq-local lsp-enable-file-watchers nil)
      (setq-local lsp-idle-delay 1.0)))
  (add-hook 'lsp-configure-hook #'abz--remote-lsp-tune)

  ;; Remote LSP clients
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("clangd"
                                                            "--all-scopes-completion"
                                                            "--background-index"
                                                            "--clang-tidy"
                                                            "--completion-style=bundled"
                                                            "--header-insertion-decorators=0"
                                                            "--header-insertion=iwyu"
                                                            "--limit-references=500"
                                                            "--limit-results=500"
                                                            "--log=error"
                                                            "--pch-storage=memory"))
                    :activation-fn (lsp-activate-on "c" "cpp" "objective-c" "cuda")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote
                    :library-folders-fn (lambda (_workspace)
                                          (bound-and-true-p lsp-clients-clangd-library-directories))))

  ;; Python LSP (pylsp)
  ;; https://github.com/python-lsp/python-lsp-server
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :activation-fn (lsp-activate-on "python")
                    :major-modes '(python-mode python-ts-mode)
                    :remote? t
                    :server-id 'pylsp-remote))

  ;; CMake LSP (neocmakelsp)
  ;; https://github.com/neocmakelsp/neocmakelsp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("neocmakelsp" "stdio"))
                    :activation-fn (lsp-activate-on "cmake")
                    :major-modes '(cmake-mode cmake-ts-mode)
                    :remote? t
                    :server-id 'cmakeneo-remote)))

;;;; Miscellaneous remote tuning

;; Exclude TRAMP paths from recentf to avoid stat calls on startup
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude tramp-file-name-regexp))

;; Ensure projectile uses alien indexing for remote projects
(with-eval-after-load 'projectile
  (defun abz--remote-projectile-tune ()
    "Ensure alien indexing and caching for remote projects."
    (when (file-remote-p default-directory)
      (setq-local projectile-indexing-method 'alien)
      (setq-local projectile-enable-caching t)))
  (add-hook 'projectile-before-project-cache-hook #'abz--remote-projectile-tune)
  (add-hook 'projectile-find-file-hook #'abz--remote-projectile-tune))

;;;; SSH host completion

(defvar abz--remote-ssh-hosts-cache nil
  "Cached list of SSH hosts from ~/.ssh/config.")

(defvar abz--remote-ssh-config-mtime nil
  "Modification time of ~/.ssh/config when cache was populated.")

(defun abz--remote-ssh-config-file ()
  "Return the path to the SSH config file."
  (expand-file-name "~/.ssh/config"))

(defun abz--remote-parse-ssh-hosts ()
  "Parse Host entries from ~/.ssh/config, excluding wildcards."
  (let ((config-file (abz--remote-ssh-config-file))
        hosts)
    (when (file-readable-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (while (re-search-forward "^Host\\s-+\\(.+\\)" nil t)
          (let ((host-line (match-string 1)))
            (dolist (host (split-string host-line))
              (unless (string-match-p "[*?!]" host)
                (push host hosts)))))))
    (nreverse hosts)))

(defun abz--remote-ssh-hosts ()
  "Return SSH hosts with caching. Invalidates when config file changes."
  (let* ((config-file (abz--remote-ssh-config-file))
         (mtime (and (file-exists-p config-file)
                     (file-attribute-modification-time
                      (file-attributes config-file)))))
    (when (or (null abz--remote-ssh-hosts-cache)
              (not (equal mtime abz--remote-ssh-config-mtime)))
      (setq abz--remote-ssh-hosts-cache (abz--remote-parse-ssh-hosts))
      (setq abz--remote-ssh-config-mtime mtime))
    abz--remote-ssh-hosts-cache))

(defun abz--remote-read-host ()
  "Prompt for a remote host with completion from SSH config."
  (completing-read "Remote host: " (abz--remote-ssh-hosts) nil nil))

(provide 'abz-remote)

;;; abz-remote.el ends here
