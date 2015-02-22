;; Emacs server to use with emacsclient

(require 'server)
(unless (server-running-p)
  (server-start))
