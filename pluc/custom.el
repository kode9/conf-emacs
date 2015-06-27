;; Misc customization, mainly basic display setup.

(setq
 frame-title-format "⸗ %F ⸗ %b (%&) ⸗ %Z ⸗"              ;; Frame title format
 display-time-24hr-format nil
 display-time-day-and-date nil
 ;; mouse-avoidance-mode (quote cat-and-mouse) nil (avoid)
 python-python-command "python3"
 show-trailing-whitespace t
 inhibit-startup-screen t                                ;; Do not show the welcome message
 initial-scratch-message nil
 message-log-max 200                                     ;; Disable off message buffer
 visible-bell nil                                        ;; Get rid of bells
 ring-bell-function 'ignore
 vc-follow-symlinks t                                    ;; Automatically follow symlinks to files under CVS
 uniquify-buffer-name-style 'post-forward-angle-brackets ;; Identify multiple buffers with the same file name
 kill-ring-max 500 ;; Kill-ring capacity
 kill-whole-line t ;; When killing a whole line, also remove the terminating newline
 )

(menu-bar-mode 0)        ;; Remove menu
(tool-bar-mode 0)        ;; Remove toolbar
(scroll-bar-mode 0)      ;; Remove scrollbar
(display-time-mode 0)    ;; Hide current time
(size-indication-mode 0) ;; Hide buffer size
(line-number-mode 1)     ;; Display current line
(column-number-mode 1)   ;; Display current column
(display-battery-mode 0) ;; Disable battery mode
(prefer-coding-system 'utf-8) ;; Primary coding system for automatic detection.
(set-language-environment "UTF-8") ;; Default input method

;; Allow undo/redo windows configuration
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Parenthesis mode
(setq show-paren-style 'expression) ;; Show full expression
(setq show-paren-delay 0.01)        ;; Delay before showing
(show-paren-mode 1)                 ;; Highlights parenthesis

;; Emacs server to use with emacsclient
(use-package server
  :config (unless (server-running-p) (server-start)))

;; https://github.com/roman/golden-ratio.el
;;; Automatic resizing of windows using the golden ration and keeping
;;; the one with the focus bigger.
(use-package golden-ratio
  :init
  (setq golden-ratio-auto-scale nil) ;; If not nil, keep frames narrow on wide screens
  (setq split-width-threshold nil) ;; Prevent additional windows creation
  :config
  (golden-ratio-mode 1))
