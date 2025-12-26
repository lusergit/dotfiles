;;; Init -- emacs init

;;; Commentary:
;; Standard Emacs init file, initializing package.el and then loading
;; modules (elisp code)

;;; Code

;; use-package init
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
	    (not (gnutls-available-p))))
(proto (if no-ssl "https" "http")))
(when no-ssl (warn "No ssl!"))
(add-to-list 'package-archives
       (cons
	"melpa"
	(concat proto "://melpa.org/packages/")) t))
(package-initialize)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

;; better defaults
(add-to-list 'load-path "/home/luser/gitgets/better-defaults")
(require 'better-defaults)

;; themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)

(load-theme 'modus-vivendi)

;; packages
(use-package evil
  :ensure t
  :config
  (evil-default-state 'emacs)
  (evil-set-initial-state 'prog-mode 'normal)
  (setq evil-normal-state-cursor '(box "white")
        evil-insert-state-cursor '(box "yellow")
        evil-visual-state-cursor '(box "magenta")
        evil-replace-state-cursor '(box "forest green")
        evil-operator-pending-state-cursor '(box "orange")
        evil-motion-state-cursor '(box "orange")
        evil-emacs-state-cursor '(box "white")
        evil-undo-system 'undo-redo))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package elixir-ts-mode :ensure t)
;;; init.el ends here
