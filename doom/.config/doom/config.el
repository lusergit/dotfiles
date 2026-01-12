;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme nil ;; let autodark manage this
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
                        :priority 10))
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection '("dexter" "lsp"))
                        :activation-fn (lsp-activate-on "elixir")
                        :server-id 'dexter
                        :priority 20)))

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

(use-package! auto-dark
  :defer t
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  :init
  (use-package! modus-themes)
  (setq! custom-safe-themes t)
  (load-theme 'modus-operandi)
  (load-theme 'modus-vivendi)
  (auto-dark-mode))

(use-package! majutsu)
(use-package! terraform-ts-mode)

(map! :after elixir-ts-mode
      :localleader
      :map elixir-ts-mode-map
      :prefix ("i" . "inf-elixir")
      "i" 'inf-elixir
      "p" 'inf-elixir-project
      "l" 'inf-elixir-send-line
      "r" 'inf-elixir-send-region
      "b" 'inf-elixir-send-buffer
      "R" 'inf-elixir-reload-module)

(use-package! fga-mode)

(use-package! ghostel
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . lz/ghostel-send-C-k-and-kill)
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (defun lz/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package! ghostel-eshell
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package! ghostel-compile
  :hook (after-init . ghostel-compile-global-mode))

(use-package! ghostel-comint
  :hook (after-init . ghostel-comint-global-mode))

(use-package! evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(setq evil-insert-state-cursor 'box)
