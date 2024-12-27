;;; Git -- custom git configs

;;; Commentary:
;; Basically just setting magit

;;; Code:
(use-package magit
  :ensure t
  :config
  (setq epg-pinentry-mode 'loopback))

(provide 'git-config)
;;; git-config.el ends here
