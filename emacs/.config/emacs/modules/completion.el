;;; Completion -- completion and minibuffer packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico, orderless and marginalia for the minibuffer.
;; In-buffer completion uses built-in `completion-at-point' (CAPF,
;; fed by Eglot) plus built-in `completion-preview-mode' (Emacs 30+)
;; for auto ghost-text.  `orderless' stays a style, not an engine.

;;; Code:

;; Enable Vertico.
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind
  (:map vertico-map ("TAB" . minibuffer-complete)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Built-in in-buffer completion (Evil-safe: TAB always indents,
;; explicit C-SPC / M-TAB completes, preview auto-shows ghost text).
(use-package emacs
  :custom
  ;; TAB indents only; don't hijack Evil/indent with inline completion.
  (tab-always-indent t)
  ;; Cycle through few candidates instead of popping *Completions*.
  (completion-cycle-threshold 3)
  ;; Keep *Completions* quiet; preview shows the candidate inline.
  (completion-show-help nil)
  :config
  ;; Explicit triggers. M-TAB and C-M-i are built-in defaults;
  ;; C-SPC is a terminal-friendly fallback (shadows set-mark-command
  ;; in prog buffers only; C-u C-SPC / C-@ still pop the mark).
  (define-key prog-mode-map (kbd "C-SPC") #'completion-at-point)
  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-SPC") #'completion-at-point))
  ;; Auto preview when available (Emacs 30.1+). Guarded so older
  ;; Emacsen just get manual C-SPC / M-TAB completion.
  (when (and (>= emacs-major-version 30)
             (require 'completion-preview nil t)
             (fboundp 'global-completion-preview-mode))
    (setq completion-preview-minimum-symbol-length 2
          completion-preview-idle-delay 0.3
          completion-preview-exact-match-only nil)
    (global-completion-preview-mode 1)))

(provide 'completion)
;;; completion.el ends here