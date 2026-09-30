;;; Terminal -- ghostel terminal emulator as default -*- lexical-binding: t; -*-

;;; Commentary:
;; Ghostel (libghostty-vt powered terminal emulator from MELPA) is the
;; default terminal for everything here:
;; - interactive shells via `ghostel' / `ghostel-project'
;; - `eshell' visual commands via `ghostel-eshell-visual-command-mode'
;; - `compile' commands via `ghostel-compile-global-mode'
;; - `comint' (shell, project-shell-command, etc.) via `ghostel-comint-global-mode'
;; Ported from doom/.config/doom/config.el, adapted to `use-package'.
;; External Ghostty app config lives in ghostty/.config/ghostty/config
;; and is complementary.  Fish helpers (`e', `dow', `gst') already exist
;; in fish/.config/fish/config.fish and need no changes.

;;; Code:

;; Forward declarations to keep byte-compilation warning-free
;; (all are autoloaded/defined at runtime).
(defvar project-switch-commands)
(defvar ghostel-eval-cmds)
(defvar ghostel-semi-char-mode-map)
(defvar project-prefix-map)
(declare-function ghostel-send-key "ghostel" (key &optional modifiers))

(use-package consult
  :ensure t)

(use-package ghostel
  :ensure t
  :after (consult)
  :hook (ghostel-mode . luser/ghostel-setup)
  :bind (:map ghostel-semi-char-mode-map
          ("C-s" . consult-line)
          ("C-k" . luser/ghostel-send-C-k-and-kill)
          ("M-p" . luser/ghostel-send-C-p)
          ("M-n" . luser/ghostel-send-C-n)
          :map project-prefix-map
          ("M" . ghostel-project-list-buffers))
  :config
  (defun luser/ghostel-setup ()
    "Buffer setup for ghostel terminals.
Line numbers are noise in a terminal buffer, so turn them off
buffer-locally even when enabled globally."
    (display-line-numbers-mode -1))

  (defun luser/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (defun luser/ghostel-send-C-p ()
    "Send `C-p' to ghostel (eshell-style history-prev)."
    (interactive)
    (ghostel-send-key "p" "ctrl"))

  (defun luser/ghostel-send-C-n ()
    "Send `C-n' to ghostel (eshell-style history-next)."
    (interactive)
    (ghostel-send-key "n" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  ;; Allow `ghostel_cmd magit-status-setup-buffer' (see `gst' in config.fish).
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

;; Extensions shipped with the ghostel package itself: no separate
;; MELPA install, so never `:ensure' them (use-package-always-ensure is t).
(use-package ghostel-eshell
  :ensure nil
  :after ghostel
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package ghostel-compile
  :ensure nil
  :after ghostel
  :hook (after-init . ghostel-compile-global-mode))

(use-package ghostel-comint
  :ensure nil
  :after ghostel
  :hook (after-init . ghostel-comint-global-mode))

(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package consult-ghostel
  :ensure t
  :after (ghostel consult)
  :hook (after-init . consult-ghostel-mode)
  :bind (("C-x m" . consult-ghostel)
         :map project-prefix-map
         ("m" . consult-ghostel-project)
         :map ghostel-semi-char-mode-map
         ("C-c h" . consult-ghostel-history)))

(provide 'terminal)
;;; terminal.el ends here
