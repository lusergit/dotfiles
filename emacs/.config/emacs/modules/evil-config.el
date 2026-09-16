;;; Evil-config -- vim-style editing everywhere (Doom `evil +everywhere') -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil is enabled globally with normal state by default.
;; `evil-collection' provides vim bindings for special modes
;; (dired, help, ibuffer, minibuffer, eshell, ...).
;; `evil-surround' and `evil-commentary' round out Doom defaults.
;; `evil-mc' provides multicursor editing (Doom `:editor multiple-cursors').

;;; Code:

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
  (evil-mode-line-format '(before . luser-modeline-buffer-remote))
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

(use-package evil-mc
  :ensure t
  :after evil
  :demand t
  :config
  (global-evil-mc-mode 1)
  ;; Doom-style `gz' prefix (cf. Doom
  ;; `modules/config/default/+evil-bindings.el', `:editor multiple-cursors').
  ;; Doom's `+multiple-cursors/evil-mc-toggle-cursors',
  ;; `+multiple-cursors/evil-mc-undo-cursor' and
  ;; `+multiple-cursors/evil-mc-toggle-cursor-here' are Doom wrappers;
  ;; here they map to upstream `evil-mc-pause-cursors',
  ;; `evil-mc-undo-last-added-cursor' and `evil-mc-make-cursor-here'.
  (evil-define-key '(normal visual) 'global
    (kbd "gzd") 'evil-mc-make-and-goto-next-match
    (kbd "gzD") 'evil-mc-make-and-goto-prev-match
    (kbd "gzs") 'evil-mc-skip-and-goto-next-match
    (kbd "gzS") 'evil-mc-skip-and-goto-prev-match
    (kbd "gzc") 'evil-mc-skip-and-goto-next-cursor
    (kbd "gzC") 'evil-mc-skip-and-goto-prev-cursor
    (kbd "gzj") 'evil-mc-make-cursor-move-next-line
    (kbd "gzk") 'evil-mc-make-cursor-move-prev-line
    (kbd "gzm") 'evil-mc-make-all-cursors
    (kbd "gzn") 'evil-mc-make-and-goto-next-cursor
    (kbd "gzN") 'evil-mc-make-and-goto-last-cursor
    (kbd "gzp") 'evil-mc-make-and-goto-prev-cursor
    (kbd "gzP") 'evil-mc-make-and-goto-first-cursor
    (kbd "gzq") 'evil-mc-undo-all-cursors
    (kbd "gzt") 'evil-mc-pause-cursors
    (kbd "gzT") 'evil-mc-resume-cursors
    (kbd "gzu") 'evil-mc-undo-last-added-cursor
    (kbd "gzz") 'evil-mc-make-cursor-here)
  (evil-define-key 'visual 'global
    (kbd "gzI") 'evil-mc-make-cursor-in-visual-selection-beg
    (kbd "gzA") 'evil-mc-make-cursor-in-visual-selection-end))

(provide 'evil-config)
;;; evil-config.el ends here
