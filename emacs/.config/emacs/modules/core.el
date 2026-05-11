;;; Core -- configuration of Emacs builtin features

;;; Commentary:
;; Core settings for Emacs builtin features.

;;; Code:

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (display-line-numbers 'relative)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (backup-by-copying t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (apropos-do-all t)
  (mouse-yank-at-point t)
  (require-final-newline t)
  (treesit-font-lock-level 4)
  :bind
  (("C-x C-b" . 'switch-to-buffer)
   ("C-x C-c" . 'delete-frame)
   ("M-z" . 'zap-up-to-char)
   ("C-s" . 'isearch-forward-regexp)
   ("C-r" . 'isearch-backward-regexp)
   ("C-M-s" . 'isearch-forward)
   ("C-M-r" . 'isearch-backward))
  :init
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))
  :config
  (context-menu-mode 1)
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

(provide 'core)
;;; core.el ends here
