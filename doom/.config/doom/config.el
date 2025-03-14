;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; quantities
(setq doom-font (font-spec :family "Cascadia Code PL" :size 14.0 :weight 'semi-light)
      doom-theme nil                    ; let autodark manage it
      magit-process-finish-apply-ansi-colors t
      display-line-numbers-type 'relative
      scroll-conservatively 101
      scroll-margin 0)

(map! :leader
      :n
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)

(map! :leader
      :n
      :desc "Switch to next active frame" "f o" #'other-frame)
(setq custom-safe-themes t)

(setq auto-dark-themes '((modus-vivendi) (modus-operandi)))
(auto-dark-mode t)

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

(after! treesit
  (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode)))

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

(after! elixir-ts-mode
  (add-hook 'elixir-ts-mode-hook #'lsp))
