;;; Vcs -- version control: magit (git), majutsu (jj), diff-hl markers

;;; Commentary:
;; Package declarations for VCS tooling.  Bindings live in leader.el
;; under SPC g (git) and SPC j (jj).

;;; Code

(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t)
  ;; Evil bindings for magit buffers come from evil-collection,
  ;; already initialized in evil-config.el.
  )

(use-package majutsu
  :vc (:url "https://github.com/0WD0/majutsu" :rev :newest)
  :config
  ;; Native Evil integration lives in a separate library that majutsu
  ;; does not autoload; it defers to evil itself, so requiring it here
  ;; is safe (evil is already up, vcs.el loads after evil-config.el).
  (require 'majutsu-evil))

(use-package diff-hl
  :ensure t
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Fringe markers in GUI; automatic margin fallback in terminals.
  ;; Markers work in vc-backed (git) buffers; jj repos are served by
  ;; the majutsu status buffer instead (vc has no jj backend).
  (global-diff-hl-mode 1))

(provide 'vcs)
;;; vcs.el ends here
