;;; Evil-config -- vim-style editing for editing buffers, Emacs everywhere else

;;; Commentary:
;; Evil is enabled only in editing buffers (programming and plain text,
;; markdown and org).  All other modes (vterm/terminals, org agenda and
;; calendar, help, dired, magit, ...) come up in the plain Emacs state.

;;; Code

(use-package evil
  :ensure t
  :demand t
  :custom
  (evil-default-state 'emacs)
  (evil-mode-line-format '(after . luser-modeline-major-mode))
  :config
  (dolist (mode '(prog-mode text-mode org-mode markdown-mode))
    (evil-set-initial-state mode 'normal))
  (dolist (mode (append evil-motion-state-modes evil-insert-state-modes))
    (evil-set-initial-state mode 'emacs))
  (define-key evil-normal-state-map (kbd "C-z") 'evil-emacs-state)
  (define-key evil-motion-state-map (kbd "C-z") 'evil-emacs-state)
  (define-key evil-visual-state-map (kbd "C-z") 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "C-z") 'evil-normal-state)
  (evil-mode 1))

(provide 'evil-config)
;;; evil-config.el ends here