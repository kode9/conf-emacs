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
(require 'tramp)
(require 'use-package)
(require 'cl-lib)

;;;; Customization

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
  (tramp-default-method "sshx"
   "Use sshx over ssh. The ssh method interacts with the remote login
shell and must parse its prompt, which fails with non-POSIX shells
like fish (e.g. prompts using characters outside [#$%>]). The sshx
method starts /bin/sh directly via RemoteCommand, bypassing the
login shell entirely.")
  (tramp-verbose 1 "Only show errors (default 3). Reduces format overhead.")
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

;;;; tramp-rpc: high-performance TRAMP backend using JSON-RPC

;; https://github.com/ArthurHeymans/emacs-tramp-rpc
;; Replaces shell-based file operations with a lightweight Rust server
;; on the remote, communicating via MessagePack-RPC over SSH. Provides
;; batch operations, filesystem watch, and fast project root detection.
;; Access remote files with /rpc:user@host:/path.
(use-package tramp-rpc
  :straight (tramp-rpc :host github :repo "ArthurHeymans/emacs-tramp-rpc")
  :after tramp)

;;;; Magit over TRAMP

;; Magit: lightweight mode for remote repositories
;; https://magit.vc/
(use-package magit
  :straight nil
  :preface
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
  :hook
  (magit-status-mode . abz--remote-magit-lighten))

;; autorevert: suppress auto-revert for remote buffers (built-in)
(use-package autorevert
  :straight nil
  :preface
  (defun abz--remote-inhibit-auto-revert ()
    "Disable auto-revert-mode in remote buffers."
    (when (and buffer-file-name (file-remote-p buffer-file-name))
      (auto-revert-mode -1)))
  :config
  (add-hook 'find-file-hook #'abz--remote-inhibit-auto-revert))

;;;; LSP over TRAMP

;; lsp-mode: remote-specific tuning and TRAMP client registrations
;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :straight nil
  :preface
  (defun abz--remote-lsp-tune ()
    "Tune LSP settings for remote buffers."
    (when (file-remote-p default-directory)
      (setq-local lsp-enable-file-watchers nil)
      (setq-local lsp-idle-delay 1.0)))
  :hook
  (lsp-configure . abz--remote-lsp-tune)
  :config
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

;; recentf: exclude TRAMP paths to avoid stat calls on startup (built-in)
(use-package recentf
  :straight nil
  :config
  (add-to-list 'recentf-exclude tramp-file-name-regexp))

;; Projectile: remote project tuning
;; https://github.com/bbatsov/projectile
(use-package projectile
  :straight nil
  :preface
  (defun abz--remote-projectile-ignore-remote (path)
    "Return non-nil if PATH is a remote TRAMP path."
    (file-remote-p path))
  (defun abz--remote-projectile-tune ()
    "Ensure alien indexing and caching for remote projects."
    (when (file-remote-p default-directory)
      (setq-local projectile-indexing-method 'alien)
      (setq-local projectile-enable-caching t)))
  :custom
  ;; Prevent TRAMP paths from being saved to known projects.
  ;; Remote projects trigger TRAMP connections on startup when
  ;; projectile tries to verify them.
  (projectile-ignored-project-function
   #'abz--remote-projectile-ignore-remote
   "Ignore remote TRAMP projects.")
  :hook
  ((projectile-after-switch-project . abz--remote-projectile-tune)
   (projectile-find-file . abz--remote-projectile-tune)))

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

(defun abz--remote-ssh-command (host command &optional buffer)
  "Run COMMAND on HOST via SSH using /bin/sh for POSIX compatibility.
Returns the exit code. If BUFFER is non-nil, capture output there."
  (call-process "ssh" nil (or buffer nil) nil host
                (format "/bin/sh -c %s" (shell-quote-argument command))))

(defun abz--remote-check-executable (host command)
  "Check if COMMAND exists on HOST via SSH. Return t or nil."
  (zerop (abz--remote-ssh-command
          host (format "command -v %s >/dev/null 2>&1"
                       (shell-quote-argument command)))))

(defun abz--remote-check-emacs-pgtk (host)
  "Check if Emacs on HOST is built with pgtk support."
  (zerop (abz--remote-ssh-command
          host "emacs --batch --eval '(unless (featurep (quote pgtk)) (kill-emacs 1))'")))

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

;;;; Daemon management

(defun abz--remote-daemon-running-p (host daemon-name)
  "Return t if Emacs daemon DAEMON-NAME is running on HOST."
  (zerop (abz--remote-ssh-command
          host (format "emacsclient -s %s -e t 2>/dev/null"
                       (shell-quote-argument daemon-name)))))

(defun abz--remote-list-daemons (host)
  "Return a list of running Emacs daemon socket names on HOST."
  (let ((output (with-temp-buffer
                  (abz--remote-ssh-command
                   host "ls /run/user/$(id -u)/emacs/ 2>/dev/null || ls /tmp/emacs$(id -u)/ 2>/dev/null"
                   t)
                  (buffer-string))))
    (when (and output (not (string-empty-p (string-trim output))))
      (split-string (string-trim output) "\n" t))))

(defun abz--remote-read-daemon-name (host)
  "Prompt for a daemon name with completion from running daemons on HOST.
With no prefix argument, return `abz-remote-daemon-name'.
With prefix argument, prompt with completion from running daemons."
  (if current-prefix-arg
      (let ((running (abz--remote-list-daemons host)))
        (completing-read (format "Daemon name (default %s): " abz-remote-daemon-name)
                         running nil nil nil nil abz-remote-daemon-name))
    abz-remote-daemon-name))

(defun abz--remote-ensure-daemon (host daemon-name)
  "Ensure Emacs daemon DAEMON-NAME is running on HOST. Start if needed."
  (unless (abz--remote-daemon-running-p host daemon-name)
    (message "Starting Emacs daemon '%s' on %s..." daemon-name host)
    (abz--remote-ssh-command
     host (format "emacs --daemon=%s" (shell-quote-argument daemon-name)))))

;;;; Display proxy commands

(defun abz--remote-xpra-connect (host daemon-name)
  "Start an XPRA session on HOST and attach to it.
Starts the XPRA server on the remote via SSH using the remote's own
xpra binary to avoid version mismatch issues between local and remote
XPRA installations. Then attaches from the local client."
  (abz--remote-ensure-daemon host daemon-name)
  ;; Start xpra server on the remote using the remote's own binary.
  ;; This avoids the local xpra passing options the remote can't parse.
  (message "Starting XPRA session on %s (daemon '%s')..." host daemon-name)
  (let ((start-buf (format "*xpra-start:%s*" host)))
    (abz--remote-ssh-command
     host (format "xpra start --start='emacsclient -c -s %s'"
                  (shell-quote-argument daemon-name))
     start-buf))
  ;; Attach from local client.
  ;; --ssh=ssh forces the system ssh binary so that ~/.ssh/config,
  ;; ControlMaster sockets, and identity files are used.
  (let ((attach-buf (format "*xpra:%s*" host)))
    (message "Attaching to %s via XPRA (check buffer %s for progress)..." host attach-buf)
    (start-process attach-buf attach-buf "xpra" "attach"
                   (format "ssh://%s" host)
                   "--ssh=ssh")))

(defun abz--remote-waypipe-connect (host daemon-name)
  "Connect to remote Emacs on HOST via waypipe."
  (abz--remote-ensure-daemon host daemon-name)
  (let ((buf (format "*waypipe:%s*" host)))
    (message "Connecting to %s via waypipe (daemon '%s', check buffer %s)..."
             host daemon-name buf)
    (start-process buf buf "waypipe" "ssh" host
                   "emacsclient" "-c" "-s" daemon-name)))

;;;###autoload
(defun abz-remote-emacs (host)
  "Connect to a remote Emacs daemon via the configured display proxy.
Starts the daemon if not running. Checks prerequisites on first use.
With prefix argument, prompt for daemon session name."
  (interactive (list (abz--remote-read-host)))
  (let* ((daemon-name (abz--remote-read-daemon-name host))
         (prereqs (abz--remote-check-prerequisites host)))
    (if (abz--remote-prerequisites-met-p prereqs)
        (progn
          (message "Connecting to %s (daemon '%s', method %s)..."
                   host daemon-name abz-remote-display-method)
          (cl-case abz-remote-display-method
            (xpra (abz--remote-xpra-connect host daemon-name))
            (waypipe (abz--remote-waypipe-connect host daemon-name))))
      (abz--remote-report-missing prereqs))))

;;;###autoload
(defun abz-remote-emacs-dwim ()
  "Connect to remote Emacs, inferring host from current TRAMP buffer.
If the current buffer visits a remote TRAMP file, use that host.
Otherwise, prompt for a host interactively.
With prefix argument, prompt for daemon session name."
  (interactive)
  (let ((host (or (and buffer-file-name
                       (file-remote-p buffer-file-name 'host))
                  (and default-directory
                       (file-remote-p default-directory 'host))
                  (abz--remote-read-host))))
    (abz-remote-emacs host)))

;;;###autoload
(defun abz-remote-emacs-stop ()
  "Stop a remote Emacs daemon.
With prefix argument, prompt for daemon session name."
  (interactive)
  (let* ((host (abz--remote-read-host))
         (daemon-name (abz--remote-read-daemon-name host)))
    (if (abz--remote-daemon-running-p host daemon-name)
        (progn
          (message "Stopping Emacs daemon '%s' on %s..." daemon-name host)
          (abz--remote-ssh-command
           host (format "emacsclient -s %s -e '(kill-emacs)'"
                        (shell-quote-argument daemon-name)))
          (message "Daemon '%s' on %s stopped." daemon-name host))
      (message "No daemon '%s' running on %s." daemon-name host))))

;;;###autoload
(defun abz-remote-find-file ()
  "Open a file on a remote host via TRAMP.
Prompts for a host, then opens `find-file' at the remote home directory."
  (interactive)
  (let* ((host (abz--remote-read-host))
         (default-directory (format "/%s:%s:~/" tramp-default-method host)))
    (call-interactively #'find-file)))

;;;###autoload
(defun abz-remote-shell ()
  "Open a shell on a remote host via TRAMP."
  (interactive)
  (let* ((host (abz--remote-read-host))
         (default-directory (format "/%s:%s:~/" tramp-default-method host)))
    (shell (format "*shell:%s*" host))))

;;;###autoload
(defun abz-remote-status ()
  "Show status of remote sessions and prerequisites."
  (interactive)
  (let ((host (abz--remote-read-host)))
    ;; Clear cached results to force re-check
    (remhash host abz--remote-prereq-cache)
    (let ((prereqs (abz--remote-check-prerequisites host))
          (daemons (abz--remote-list-daemons host)))
      (with-output-to-temp-buffer "*remote-status*"
        (princ (format "Remote status for %s\n" host))
        (princ (format "Display method: %s\n\n" abz-remote-display-method))
        (princ "Prerequisites:\n")
        (dolist (pair prereqs)
          (princ (format "  %s: %s\n"
                         (car pair)
                         (if (eq (cdr pair) t) "OK" (cdr pair)))))
        (princ "\nRunning daemons:\n")
        (if daemons
            (dolist (d daemons)
              (princ (format "  %s\n" d)))
          (princ "  (none)\n"))))))

;;;; Keymap

(use-package abz
  :straight nil
  :bind
  (:map abz-map
        :prefix "C-c r"
        :prefix-map abz-map-remote
        :prefix-docstring "Prefix keymap for remote work"
        ("e" . abz-remote-emacs-dwim)
        ("E" . abz-remote-emacs)
        ("f" . abz-remote-find-file)
        ("q" . abz-remote-emacs-stop)
        ("s" . abz-remote-shell)
        ("i" . abz-remote-status)))

(provide 'abz-remote)

;;; abz-remote.el ends here
