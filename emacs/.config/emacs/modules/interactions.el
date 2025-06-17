;;; package -- Interactions

;;; Commentary:
;; The package contains all the necessary tools to interact with
;; Emacs, the main are:
;; - helm
;; - evil

;;; Code:

;; Enable Vertico.
(use-package vertico :init (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(if (daemonp)
    (global-set-key (kbd "C-x C-c") 'delete-frame)
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

(defun lz/open-configs ()
  "Funzione per aprire il file di configurazione."
  (interactive)
  (find-file
   (expand-file-name
    (concat user-emacs-directory "init.el"))))

(defun lz/switch-buffers-2-win ()
  "When in split window view switch the buffers."
  (interactive)
  (let* ((other-window (other-window-for-scrolling))
	 (this-window (selected-window))
	 (other-buffer (window-buffer other-window))
	 (this-buffer (window-buffer this-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-buffer this-window other-buffer)))
(global-set-key (kbd "C-x w i") 'lz/switch-buffers-2-win)

(provide 'interactions)
;;; interactions.el ends here
