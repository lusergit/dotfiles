;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; quantities
(setq doom-font (font-spec :family "Cascadia Code PL" :size 18.0 :weight 'semi-light)
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

(after! lsp-mode
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection '("expert" "--stdio"))
                        :activation-fn (lsp-activate-on "elixir")
                        :server-id 'expert
                        :priority -10)))

(setopt treesit-font-lock-level 4)

(after! elixir-ts-mode (add-hook 'elixir-ts-mode-hook #'lsp))

(use-package! treesit-auto :config (global-treesit-auto-mode))
(use-package! mood-line
  :config
  (setq mood-line-format
        (mood-line-defformat
         :left
         (((mood-line-segment-buffer-status) . " ")
          ((mood-line-segment-buffer-name)   . " : ")
          ((mood-line-segment-major-mode) . " "))
         :right
         (((mood-line-segment-cursor-position)    . " ")
          ((when (mood-line-segment-checker) "|") . " ")
          ((mood-line-segment-checker) . " "))))
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

(use-package! just-mode)

(use-package! justl
  :config
  (map! :n "e" 'justl-exec-recipe))
