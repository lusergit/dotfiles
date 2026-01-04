;;; Init -- emacs init

;;; Commentary:
;; Standard Emacs init file, initializing package.el and then loading
;; modules (elisp code)

;;; Code

(add-to-list 'load-path (format "%s%s" user-emacs-directory "modules"))
(require 'config)
(require 'themes)
(require 'modeline)


;; Expand region and multiple cursors
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this))
  :custom
  (mc/always-run-for-all t))


;; Enable Vertico.
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind
  (:map vertico-map ("TAB" . minibuffer-complete)))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))


(use-package marginalia
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package elixir-ts-mode :ensure t)
(put 'narrow-to-region 'disabled nil)
;;; init.el ends here
