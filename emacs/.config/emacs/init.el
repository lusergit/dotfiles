;;; Init -- emacs init

;;; Commentary:
;; Standard Emacs init file, initializing package.el and then loading
;; modules (elisp code)

;;; Code:

;; Set custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

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

;; also quelpa
(use-package quelpa :ensure t)

(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

(defgroup lz-group nil
  "Customization group for my custom settings."
  :group 'convenience)

;; MODULES
(add-to-list 'load-path (concat user-emacs-directory "modules/"))
(require 'window-config)
(require 'modeline)
(require 'git-config)
(require 'themes-config)
(require 'org-config)
(require 'site)
(require 'languages)
(require 'snippets-config)
(require 'metalang)
(require 'dired-config)
(require 'vterm-config)

(provide 'init)
;;; init.el ends here
