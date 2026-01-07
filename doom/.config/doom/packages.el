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

(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :branch "master"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           ;; do not perform byte compilation or native compilation for lsp-bridge
           :build (:not compile)))

(package! spacious-padding)
(package! elixir-ts-mode)
(package! ef-themes)
(package! kubernetes)
(package! treesit-auto)
(package! visual-fill-column)
(package! just-mode)
(package! justl :recipe (:host github :repo "psibi/justl.el"))
(package! rfc-mode)
(package! majutsu :recipe (:host github :repo "0WD0/majutsu"))
(package! modus-themes)
(package! terraform-ts-mode :recipe (:host github :repo "kgrotel/terraform-ts-mode"))
(package! kdl-mode)
(package! inf-elixir)
(package! fga-mode :recipe (:host github :repo "lusergit/fga-mode"))
;; :term ghostel module provides ghostel + evil-ghostel; only the consult
;; extension needs declaring here (same repo/pin as the module).
(package! consult-ghostel
  :recipe (:host github :repo "dakra/ghostel"
           :files ("extensions/consult-ghostel/*.el"))
  :pin "dd72e1f4ae891345a1f76ed98c5cbd71c18e808e")
(package! odin-ts-mode :recipe (:host github :repo "Sampie159/odin-ts-mode"))
(package! ox-typst)
