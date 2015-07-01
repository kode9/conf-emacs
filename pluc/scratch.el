;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporary snippets. Do not include ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Powerline (custom mode-line)
;;; https://github.com/milkypostman/powerline

;; (defun powerline-pluc-theme ()
;;   "Setup the default mode-line."
;;   (interactive)
;;   (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (mode-line (if active 'mode-line 'mode-line-inactive))
;;                           (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                           (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                           (separator-left (intern (format "powerline-%s-%s"
;;                                                           powerline-default-separator
;;                                                           (car powerline-default-separator-dir))))
;;                           (separator-right (intern (format "powerline-%s-%s"
;;                                                            powerline-default-separator
;;                                                            (cdr powerline-default-separator-dir))))
;;                           (lhs (list (powerline-buffer-id nil 'l)
;;                                      (when (and (boundp 'which-func-mode) which-func-mode)
;;                                        (powerline-raw which-func-format nil 'l))
;;                                      (powerline-raw " ")
;;                                      (funcall separator-left mode-line face1)
;;                                      (when (boundp 'erc-modified-channels-object)
;;                                        (powerline-raw erc-modified-channels-object face1 'l))
;;                                      (funcall separator-left face1 face2)
;;                                      (powerline-raw "%l ϟ %c" face2 'l)
;;                                      (funcall separator-left face2 face1)
;;                                      (powerline-major-mode face1 'l)
;;                                      (powerline-process face1)
;;                                      (powerline-minor-modes face1 'l)
;;                                      (powerline-narrow face1 'l)
;;                                      (powerline-raw " " face1)
;;                                      (funcall separator-left face1 face2)
;;                                      (powerline-vc face2 'r)))
;;                           (rhs (list (powerline-raw global-mode-string face2 'r)
;;                                      (funcall separator-right face2 face1)
;;                                      (funcall separator-right face1 mode-line)
;;                                      (powerline-raw " ")
;;                                      (powerline-raw "%6p" nil 'r)
;;                                      (powerline-hud face2 face1))))
;;                      (concat (powerline-render lhs)
;;                              (powerline-fill face2 (powerline-width rhs))
;;                              (powerline-render rhs)))))))

;; (powerline-pluc-theme)
;; (powerline-default-theme)



;; Highlights current line in compilation within another buffer
;;; Actually it's a bit annoying...
;; (add-hook 'compilation-mode-hook (lambda () (fm-start)))
;; (remove-hook 'compilation-mode-hook (lambda () (fm-start)))



;; Various debug function
;;; http://www.cbrunzema.de/download/ll-debug/ll-debug.el
;; (require 'll-debug)
;; ;;; Comment a region and keep a copy
;; (global-set-key [(control c) (w)] 'll-debug-copy-and-comment-region-or-line)

;; Space around operators
;; (add-to-list 'load-path '"~/.emacs.d/elpa/smart-operator-20051013.1756/")
;; (require 'smart-operator)
;; (defun smart-operator-c-hook ()
;;   (smart-insert-operator-hook)
;;   (local-unset-key (kbd "."))
;;   (local-unset-key (kbd ":"))
;;   (local-unset-key (kbd "!"))
;;   (local-unset-key (kbd "<"))
;;   (local-unset-key (kbd ">"))
;;   (local-unset-key (kbd "/"))
;;   (local-unset-key (kbd "-"))
;;   (local-set-key (kbd "*") 'c-electric-star))
;; (add-hook 'c-mode-common-hook 'smart-operator-c-hook)



;; Close the compilation window if there was no error at all.
;; (setq compilation-exit-message-function
;;       (lambda (status code msg)
;;         ;; If M-x compile exists with a 0
;;         (when (and (eq status 'exit) (zerop code))
;;           ;; then bury the *compilation* buffer, so that C-x b doesn't go there
;;           (bury-buffer "*compilation*")
;;           ;; and return to whatever were looking at before
;;           ;; (replace-buffer-in-windows "*compilation*"))
;;         ;; Always return the anticipated result of compilation-exit-message-function
;;         )))




;; ;; prelude-editing.el
;; (defcustom prelude-indent-sensitive-modes
;;   '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
;;   "Modes for which auto-indenting is suppressed."
;;   :type 'list)

;; (defun indent-region-or-buffer ()
;;   "Indent a region if selected, otherwise the whole buffer."
;;   (interactive)
;;   (unless (member major-mode prelude-indent-sensitive-modes)
;;     (save-excursion
;;       (if (region-active-p)
;;           (progn
;;             (indent-region (region-beginning) (region-end))
;;             (message "Indented selected region."))
;;         (progn
;;           (indent-buffer)
;;           (message "Indented buffer.")))
;;       (whitespace-cleanup))))

;; (global-set-key (kbd "C-c i") 'indent-region-or-buffer)
