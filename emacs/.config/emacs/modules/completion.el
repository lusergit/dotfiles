;;; Completion -- completion and minibuffer packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico, orderless and marginalia.

;;; Code:

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

(provide 'completion)
;;; completion.el ends here