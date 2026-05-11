;;; Programming -- programming language support

;;; Commentary:
;; Eglot (LSP) configuration and language-specific packages.

;;; Code:

;; Common Eglot configuration.
(use-package eglot
  :hook
  (prog-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)
  (eglot-connect-timeout 30)
  (eglot-events-buffer-config '(:size 200000 :format full))
  :bind
  (:map eglot-mode-map
    ("C-c C-a" . eglot-code-actions)
    ("C-c C-r" . eglot-rename)
    ("C-c C-f" . eglot-format)
    ("C-c C-d" . eldoc-doc-buffer))
  :config
  ;; Elixir language server via asdf-installed elixir-ls.
  (add-to-list 'eglot-server-programs
               `((elixir-mode elixir-ts-mode heex-ts-mode)
                 . ,(eglot-alternatives
                     '(("dexter" "lsp")
                       ("elixir-ls"))))))

(use-package elixir-ts-mode :ensure t)

(provide 'programming)
;;; programming.el ends here
