;;; Editing -- text editing helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Expand region.  Multicursor editing lives in `evil-config' via `evil-mc'.

;;; Code:

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'editing)
;;; editing.el ends here