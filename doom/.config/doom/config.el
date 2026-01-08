;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'modus-vivendi
      doom-font (font-spec :family "Cascadia Code NF" :size 18.0 :weight 'semilight)
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

(use-package! mood-line
  :config (mood-line-mode)
  :custom (mood-line-glyph-alist mood-line-glyphs-ascii))

(use-package! spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :custom-button-width 3
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  (spacious-padding-mode 1))

(use-package! treesit-auto :config (global-treesit-auto-mode))
