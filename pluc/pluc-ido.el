;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido (InteractivelyDoThings): better completion for files and buffer, fuzzy matching etc ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 ido-enable-flex-matching t ;; Fuzzy matching
 ido-use-filename-at-point nil ;; Do not match filename at point
 ido-enable-last-directory-history t ;; Remember latest selected directories
 ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory) ;; ido cache file
 ido-max-work-directory-list 30 ;; Max working directory history
 ido-max-work-file-list 100 ;; Max file history
 ido-confirm-unique-completion nil ;; Do not wait for RET, even with unique completion
 ido-use-virtual-buffers 't ;; Use virtual buffers
 ;;
 confirm-nonexistent-file-or-buffer 'after-completion ;; Ask confirmation for new file/buffer only after completion
 flycheck-completion-system 'ido ;; Use ido for flycheck completion
 )

(ido-ubiquitous-mode 1) ;; ido evberywhere
(ido-vertical-mode 1) ;; Makes ido-mode display vertically

;; Better fuzzy matching
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil) ;; Disable ido faces to see flx highlights.
;; If you don't want to use the flx's highlights you can turn them off like this:
;; (setq flx-ido-use-faces nil)

(ido-mode 1) ;; activer ido
(ido-everywhere 1) ;; Activer ido pour find-files et les buffers

;; ido for meta-x
(smex-initialize)
(global-set-key "\M-x" 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido recently opened files
;; (setq recentf-max-saved-items 50) ;; fixer le nombre d'enregistrements à 50
;; (recentf-mode 1)
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;; find-tag using ido
;; (defun ido-find-tag ()
;;   "Find a tag using ido"
;;   (interactive)
;;   (visit-tags-table-buffer)
;;   (tags-completion-table)
;;   (let (tag-names)
;;     (mapatoms (lambda (x)
;; 		(push (prin1-to-string x t) tag-names))
;; 	      tags-completion-table)
;;     (find-tag (ido-completing-read "Tag: " tag-names))))

;; find-tag-other-window using ido
;; (defun ido-find-tag-other-window ()
;;   "Find a tag using ido"
;;   (interactive)
;;   (visit-tags-table-buffer)
;;   (tags-completion-table)
;;   (let (tag-names)
;;     (mapatoms (lambda (x)
;; 		(push (prin1-to-string x t) tag-names))
;; 	      tags-completion-table)
;;     (find-tag-other-window (ido-completing-read "Tag: " tag-names))))

;; Find files in Tags File
;; (defun ido-find-file-in-tags ()
;;   (interactive)
;;   (save-excursion
;;     (let ((enable-recursive-minibuffers t))
;;       (visit-tags-table-buffer))
;;     (find-file
;;      (expand-file-name
;;       (ido-completing-read
;;        "Project file: " (tags-table-files) nil t)))))

;; (global-set-key [(control c) (d)] 'ido-find-tag)
;; (global-set-key [(control c) (f)] 'ido-find-tag-other-window)
;; (global-set-key [(control c) (g)] 'ido-find-file-in-tags)

(provide 'pluc-ido)
