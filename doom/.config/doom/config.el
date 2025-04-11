;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; quantities
(setq doom-font (font-spec :family "Cascadia Code PL" :size 14.0 :weight 'semi-light)
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

(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elixir-ls"))
                    :major-modes '(elixir-ts-mode)
                    :priority 30
                    :server-id 'elixir-ls)))

(use-package! gleam-ts-mode
  :config
  ;; setup formatter to be used by `SPC c f`
  (after! apheleia
    (setf (alist-get 'gleam-ts-mode apheleia-mode-alist) 'gleam)
    (setf (alist-get 'gleam apheleia-formatters) '("gleam" "format" "--stdin"))))

(use-package! typst-ts-mode
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-menu))

(after! gleam-ts-mode
  (setq treesit-extra-load-path (list (expand-file-name "~/.local/tree-sitter/")))
  (unless (treesit-language-available-p 'gleam)
    ;; hack: change `out-dir' when install language-grammar'
    (let ((orig-treesit--install-language-grammar-1 (symbol-function 'treesit--install-language-grammar-1)))
      (cl-letf (((symbol-function 'treesit--install-language-grammar-1)
                 (lambda (out-dir lang url)
                   (funcall orig-treesit--install-language-grammar-1
                            "~/.local/tree-sitter/" lang url))))
        (gleam-ts-install-grammar)))))

(use-package! kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(setopt treesit-font-lock-level 4)

(after! elixir-ts-mode
  (add-hook 'elixir-ts-mode-hook #'lsp))

(use-package! treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package! evil
  :config
  (evil-set-initial-state 'vterm-mode 'emacs))
