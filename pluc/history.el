;; Save mini buffer history between sessions

;; History file
(setq savehist-file (expand-file-name ".history" user-emacs-directory))
(savehist-mode t)
