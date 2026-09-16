;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;;; Commentary:
;; Standard Emacs init file, initializing package.el and then loading
;; modules (elisp code)

;;; Code:

(add-to-list 'load-path (format "%s%s" user-emacs-directory "modules"))
(require 'core)
(require 'themes)
(require 'modeline)
(require 'completion)
(require 'editing)
(require 'programming)
(require 'evil-config)
(require 'vcs)
(require 'leader)
;;; init.el ends here