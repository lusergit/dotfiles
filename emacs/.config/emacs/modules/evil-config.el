;;; Evil-config -- vim-style editing everywhere (Doom `evil +everywhere')

;;; Commentary:
;; Evil is enabled globally with normal state by default.
;; `evil-collection' provides vim bindings for special modes
;; (dired, help, ibuffer, minibuffer, eshell, ...).
;; `evil-surround' and `evil-commentary' round out Doom defaults.

;;; Code

(use-package evil
  :ensure t
  :demand t
  :init
  ;; Must be set before evil/evil-collection load.
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-want-empty-ex-last-command nil)
  :custom
  (evil-default-state 'normal)
  (evil-mode-line-format '(after . luser-modeline-major-mode))
  :config
  ;; Escape hatch, as in Doom: C-z toggles emacs state.
  (define-key evil-normal-state-map (kbd "C-z") 'evil-emacs-state)
  (define-key evil-motion-state-map (kbd "C-z") 'evil-emacs-state)
  (define-key evil-visual-state-map (kbd "C-z") 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "C-z") 'evil-normal-state)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

(provide 'evil-config)
;;; evil-config.el ends here
