;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! mood-line)

(package! auto-dark)

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"))

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

(package! elixir-ts-mode)
(package! ef-themes)
(package! kubernetes)
