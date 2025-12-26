;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'modus-vivendi
      display-line-numbers-type 'relative
      custom-safe-themes t)

(map! :leader :n :desc "Open like spacemacs" "SPC" #'execute-extended-command)
(map! :leader :n :desc "Switch to next active frame" "f o" #'other-frame)

(after! lsp-mode
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection '("expert" "--stdio"))
                        :activation-fn (lsp-activate-on "elixir")
                        :server-id 'expert
                        :priority -10)))

(setopt treesit-font-lock-level 4)

(after! elixir-ts-mode (add-hook 'elixir-ts-mode-hook #'lsp))

(use-package! treesit-auto :config (global-treesit-auto-mode))
