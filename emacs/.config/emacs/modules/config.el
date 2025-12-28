;;; Config -- emacs config

;;; Commentary:
;; Config of emacs builtin features

;;; Code

;; use-package init

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (display-line-numbers 'relative)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (backup-by-copying t)
  (visible-bell t)
  (load-prefer-newer t)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (apropos-do-all t)
  (mouse-yank-at-point t)
  (require-final-newline t)
  (frame-inhibit-implied-resize t)
  :bind
  (("C-x C-b" . 'switch-to-buffer)
   ("C-x C-c" . 'delete-frame)
   ("M-z" . 'zap-up-to-char)
   ("C-s" . 'isearch-forward-regexp)
   ("C-r" . 'isearch-backward-regexp)
   ("C-M-s" . 'isearch-forward)
   ("C-M-r" . 'isearch-backward))
  :preface
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
  :init
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (save-place-mode 1)
  (show-paren-mode 1)
  (savehist-mode 1)
  (setq-default
   save-interprogram-paste-before-kill t
   completion-ignore-case t))

(provide 'config)
;;; config.el ends here
