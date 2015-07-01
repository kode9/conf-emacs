;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 frame-title-format "⸗ %b (%&) ⸗" ;; Frame title format: buffer (status)
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

;; Customization
;;; Took from https://github.com/lunaryorn/.emacs.d
(defconst pluc-custom-file (locate-user-emacs-file "custom.el") "File used to store settings from Customization UI.")
(use-package cus-edit :defer t :config (setq custom-file pluc-custom-file) :init (load pluc-custom-file 'no-error 'no-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup, autosaves and history ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 savehist-file (expand-file-name ".history" user-emacs-directory) ;; History file
 auto-save-default nil ;; Disable autosave for now
 backup-inhibited t ;; Disable backup for now
 )
(savehist-mode nil) ;; save minibuffer history

;; Gets the mouse out of the cursor
(use-package avoid
  :init
  (setq mouse-avoidance-threshold 10) ;; When is it too close
  :config
  (mouse-avoidance-mode 'exile) ;; Exile it to top-right corder, but allow it to come back
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow undo/redo windows configuration ;;
;; Keys: "C-c LEFT" and "C-c RIGHT"	 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'winner-mode) (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server (emacsclient) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package server :config (unless (server-running-p) (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic resizing of windows using the golden ration and keeping ;;
;; the one with the focus bigger.				     ;;
;; https://github.com/roman/golden-ratio.el			     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (setq golden-ratio-auto-scale nil) ;; If not nil, keep frames narrow on wide screens
  (setq split-width-threshold nil) ;; Prevent additional windows creation
  :config
  (golden-ratio-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paradox: better package menu        ;;
;; http://github.com/Malabarba/paradox ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paradox
  :config
  (setq paradox-github-token t) ;; Don't ask for GitHub token
  )

(provide 'pluc-custom)
