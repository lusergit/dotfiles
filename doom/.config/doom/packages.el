;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! mood-line)

(package! auto-dark)

(package! gleam-ts-mode
  :recipe (:host github
           :repo "gleam-lang/gleam-mode"
           :branch "main"

           :files ("gleam-ts-*.el")))

(package! typst-ts-mode
  :recipe (:host codeberg :repo "meow_king/typst-ts-mode"))

(when (package! lsp-bridge
        :recipe (:host github
                 :repo "manateelazycat/lsp-bridge"
                 :branch "master"
                 :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 ;; do not perform byte compilation or native compilation for lsp-bridge
                 :build (:not compile)))
  (package! markdown-mode)
  (package! yasnippet))

(package! jujutsu
  :recipe (:repo "git@github.com:bennyandresen/jujutsu.el.git"))

(package! mood-line)
(package! spacious-padding)
(package! elixir-ts-mode)
(package! ef-themes)
(package! kubernetes)
(package! treesit-auto)
(package! visual-fill-column)
(package! just-mode)
(package! justl :recipe (:host github :repo "psibi/justl.el"))
