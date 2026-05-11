;;; Editing -- text editing helpers

;;; Commentary:
;; Expand region and multiple cursors.

;;; Code:

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

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'editing)
;;; editing.el ends here