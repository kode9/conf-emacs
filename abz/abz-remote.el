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

;;;; Prerequisite checking

(defvar abz--remote-prereq-cache (make-hash-table :test 'equal)
  "Per-host prerequisite check results. Keys are HOST strings.")

(defun abz--remote-check-executable (host command)
  "Check if COMMAND exists on HOST via SSH. Return t or nil."
  (zerop (call-process "ssh" nil nil nil host
                       (format "command -v %s >/dev/null 2>&1"
                               (shell-quote-argument command)))))

(defun abz--remote-check-emacs-pgtk (host)
  "Check if Emacs on HOST is built with pgtk support."
  (zerop (call-process "ssh" nil nil nil host
                       "emacs --batch --eval '(when (eq (framep (selected-frame)) (quote pgtk)) (kill-emacs 0))' 2>/dev/null; exit $?")))

(defun abz--remote-check-prerequisites (host)
  "Check prerequisites on HOST for the current display method.
Returns an alist of (ITEM . STATUS) where STATUS is t or a string
describing what to install. Results are cached per session."
  (or (gethash host abz--remote-prereq-cache)
      (let ((results nil)
            (method abz-remote-display-method))
        ;; Check dash
        (push (cons 'dash
                    (if (abz--remote-check-executable host "dash")
                        t
                      "Install dash: sudo apt install dash"))
              results)
        ;; Check display proxy
        (let ((proxy (symbol-name method)))
          (push (cons method
                      (if (abz--remote-check-executable host proxy)
                          t
                        (format "Install %s: sudo apt install %s" proxy proxy)))
                results))
        ;; Check emacs
        (push (cons 'emacs
                    (if (abz--remote-check-executable host "emacs")
                        t
                      "Install emacs: sudo apt install emacs"))
              results)
        ;; For waypipe, check pgtk
        (when (eq method 'waypipe)
          (push (cons 'emacs-pgtk
                      (if (and (abz--remote-check-executable host "emacs")
                               (abz--remote-check-emacs-pgtk host))
                          t
                        "Remote Emacs must be built with pgtk support for waypipe"))
                results))
        (setq results (nreverse results))
        (puthash host results abz--remote-prereq-cache)
        results)))

(defun abz--remote-prerequisites-met-p (results)
  "Return t if all prerequisites in RESULTS are met."
  (cl-every (lambda (pair) (eq (cdr pair) t)) results))

(defun abz--remote-report-missing (results)
  "Display missing prerequisites from RESULTS."
  (let ((missing (cl-remove-if (lambda (pair) (eq (cdr pair) t)) results)))
    (when missing
      (message "Missing prerequisites:\n%s"
               (mapconcat (lambda (pair) (format "  - %s" (cdr pair)))
                          missing "\n")))))

;;;; Display proxy commands

(defun abz--remote-xpra-start-attach (host)
  "Start an XPRA session on HOST with emacsclient and attach."
  (let ((daemon-name abz-remote-daemon-name))
    ;; Start daemon if not running
    (unless (zerop (call-process "ssh" nil nil nil host
                                 (format "emacsclient -s %s -e t 2>/dev/null"
                                         (shell-quote-argument daemon-name))))
      (message "Starting Emacs daemon '%s' on %s..." daemon-name host)
      (call-process "ssh" nil nil nil host
                    (format "emacs --daemon=%s"
                            (shell-quote-argument daemon-name))))
    ;; Attach via xpra
    (let ((buf (format "*xpra:%s*" host)))
      (message "Attaching to %s via XPRA..." host)
      (start-process buf buf "xpra" "start+attach"
                     (format "ssh://%s" host)
                     "--start"
                     (format "emacsclient -c -s %s"
                             (shell-quote-argument daemon-name))))))

(defun abz--remote-waypipe-connect (host)
  "Connect to remote Emacs on HOST via waypipe."
  (let ((daemon-name abz-remote-daemon-name))
    ;; Start daemon if not running
    (unless (zerop (call-process "ssh" nil nil nil host
                                 (format "emacsclient -s %s -e t 2>/dev/null"
                                         (shell-quote-argument daemon-name))))
      (message "Starting Emacs daemon '%s' on %s..." daemon-name host)
      (call-process "ssh" nil nil nil host
                    (format "emacs --daemon=%s"
                            (shell-quote-argument daemon-name))))
    ;; Connect via waypipe
    (let ((buf (format "*waypipe:%s*" host)))
      (message "Connecting to %s via waypipe..." host)
      (start-process buf buf "waypipe" "ssh" host
                     "emacsclient" "-c" "-s" daemon-name))))

;;;###autoload
(defun abz-remote-emacs ()
  "Connect to a remote Emacs daemon via the configured display proxy.
Starts the daemon if not running. Checks prerequisites on first use."
  (interactive)
  (let* ((host (abz--remote-read-host))
         (prereqs (abz--remote-check-prerequisites host)))
    (if (abz--remote-prerequisites-met-p prereqs)
        (cl-case abz-remote-display-method
          (xpra (abz--remote-xpra-start-attach host))
          (waypipe (abz--remote-waypipe-connect host)))
      (abz--remote-report-missing prereqs))))

;;;###autoload
(defun abz-remote-emacs-stop ()
  "Stop a remote Emacs daemon."
  (interactive)
  (let* ((host (abz--remote-read-host))
         (daemon-name abz-remote-daemon-name))
    (message "Stopping Emacs daemon '%s' on %s..." daemon-name host)
    (call-process "ssh" nil nil nil host
                  (format "emacsclient -s %s -e '(kill-emacs)'"
                          (shell-quote-argument daemon-name)))
    (message "Daemon '%s' on %s stopped." daemon-name host)))

;;;###autoload
(defun abz-remote-shell ()
  "Open a shell on a remote host via TRAMP."
  (interactive)
  (let* ((host (abz--remote-read-host))
         (default-directory (format "/ssh:%s:~/" host)))
    (shell (format "*shell:%s*" host))))

;;;###autoload
(defun abz-remote-status ()
  "Show status of remote sessions and prerequisites."
  (interactive)
  (let ((host (abz--remote-read-host)))
    ;; Clear cached results to force re-check
    (remhash host abz--remote-prereq-cache)
    (let ((prereqs (abz--remote-check-prerequisites host))
          (daemon-name abz-remote-daemon-name))
      (with-output-to-temp-buffer "*remote-status*"
        (princ (format "Remote status for %s\n" host))
        (princ (format "Display method: %s\n" abz-remote-display-method))
        (princ (format "Daemon name: %s\n\n" daemon-name))
        (princ "Prerequisites:\n")
        (dolist (pair prereqs)
          (princ (format "  %s: %s\n"
                         (car pair)
                         (if (eq (cdr pair) t) "OK" (cdr pair)))))
        ;; Check if daemon is running
        (princ (format "\nDaemon '%s': %s\n"
                       daemon-name
                       (if (zerop (call-process "ssh" nil nil nil host
                                                (format "emacsclient -s %s -e t 2>/dev/null"
                                                        (shell-quote-argument daemon-name))))
                           "running"
                         "not running")))))))

;;;; Keymap

(with-eval-after-load 'abz
  (define-prefix-command 'abz-map-remote)
  (define-key abz-map (kbd "C-c r") 'abz-map-remote)
  (define-key abz-map-remote (kbd "e") #'abz-remote-emacs)
  (define-key abz-map-remote (kbd "q") #'abz-remote-emacs-stop)
  (define-key abz-map-remote (kbd "s") #'abz-remote-shell)
  (define-key abz-map-remote (kbd "i") #'abz-remote-status))

(provide 'abz-remote)

;;; abz-remote.el ends here
