;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; quantities
(setq doom-font (font-spec
                 :family "CascadiaCode NF"
                 :size 18.0
                 :weight 'semi-light)
      doom-theme nil                    ; let autodark manage it
      magit-process-finish-apply-ansi-colors t
      display-line-numbers-type 'relative)

(map! :leader
      :n
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)

(map! :leader
      :n
      :desc "Switch to next active frame" "f o" #'other-frame)
(setq custom-safe-themes t)

(use-package! auto-dark
  :config
  (setq auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-mode t))

;; treesit grammars
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")))

(setopt treesit-font-lock-level 4)

(after! elixir-ts-mode
  (add-hook 'elixir-ts-mode-hook #'lsp))

(use-package! treesit-auto
  :config (global-treesit-auto-mode))

(use-package! evil
  :config (evil-set-initial-state 'vterm-mode 'emacs))

(use-package! mood-line
  :config
  (setq mood-line-glyph-alist mood-line-glyphs-unicode)
  (mood-line-mode))

(use-package! spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 10
           :tab-width 4
           :right-divider-width 50
           :scroll-bar-width 8))
  (add-hook 'server-after-make-frame-hook #'spacious-padding-mode))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(after! lsp-mode
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection "expert")
                        :activation-fn (lsp-activate-on "elixir")
                        :server-id 'expert
                        :priority 10)))
