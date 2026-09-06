;;; Leader -- Doom-style SPC leader key via general + which-key

;;; Commentary:
;; Single place for all SPC bindings.  Extending is one line:
;;   (luser/leader "g s" 'magit-status :which-key "magit")
;; Prefixes mirror Doom: b buffer, f file, g git (magit), p project,
;; w window, s search, c code (eglot), h help, j jj (majutsu),
;; o open, t toggle, q quit.
;; Commands are Emacs built-ins plus your existing stack
;; (vertico/orderless, eglot, project.el) so nothing else is required.

;;; Code

(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.4)
  (which-key-show-early-on-C-h t)
  :config
  (which-key-mode 1))

(use-package general
  :ensure t
  :demand t
  :after (evil which-key)
  :config
  (general-create-definer luser/leader
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Top level: keep your Doom habits (see doom config.el).
  (luser/leader
    "" '(:ignore t :which-key "leader")
    "SPC" '(execute-extended-command :which-key "M-x"))

  ;; Prefix labels (show up in which-key).
  (luser/leader
    "b" '(:ignore t :which-key "buffer")
    "f" '(:ignore t :which-key "file")
    "g" '(:ignore t :which-key "git")
    "p" '(:ignore t :which-key "project")
    "w" '(:ignore t :which-key "window")
    "s" '(:ignore t :which-key "search")
    "c" '(:ignore t :which-key "code")
    "h" '(:ignore t :which-key "help")
    "j" '(:ignore t :which-key "jj")
    "o" '(:ignore t :which-key "open")
    "t" '(:ignore t :which-key "toggle")
    "q" '(:ignore t :which-key "quit"))

  ;; Buffer.
  (luser/leader
    "b b" '(switch-to-buffer :which-key "switch")
    "b d" '(kill-current-buffer :which-key "kill")
    "b n" '(next-buffer :which-key "next")
    "b p" '(previous-buffer :which-key "prev")
    "b R" '(rename-buffer :which-key "rename")
    "b i" '(ibuffer :which-key "ibuffer"))

  ;; File.
  (luser/leader
    "f f" '(find-file :which-key "find")
    "f s" '(save-buffer :which-key "save")
    "f S" '(save-some-buffers :which-key "save all")
    "f o" '(other-frame :which-key "next frame")
    "f d" '(dired-jump :which-key "dired here"))

  ;; Git (magit, declared in vcs.el).  Letters follow Doom defaults.
  (luser/leader
    "g g" '(magit-status :which-key "status")
    "g G" '(magit-status-here :which-key "status here")
    "g /" '(magit-dispatch :which-key "dispatch")
    "g ." '(magit-file-dispatch :which-key "file dispatch")
    "g b" '(magit-branch-checkout :which-key "checkout branch")
    "g B" '(magit-blame-addition :which-key "blame")
    "g C" '(magit-clone :which-key "clone")
    "g F" '(magit-fetch :which-key "fetch")
    "g L" '(magit-log-buffer-file :which-key "file log")
    "g c" '(magit-commit :which-key "commit")
    "g d" '(magit-diff :which-key "diff")
    "g l" '(magit-log-current :which-key "log")
    "g P" '(magit-push :which-key "push")
    "g p" '(magit-pull :which-key "pull"))

  ;; Project (built-in project.el).
  (luser/leader
    "p p" '(project-switch-project :which-key "switch")
    "p f" '(project-find-file :which-key "find file")
    "p b" '(project-switch-to-buffer :which-key "buffer")
    "p g" '(project-find-regexp :which-key "grep")
    "p k" '(project-kill-buffers :which-key "kill buffers")
    "p !" '(project-shell-command :which-key "shell cmd"))

  ;; Window.
  (luser/leader
    "w w" '(other-window :which-key "other")
    "w d" '(delete-window :which-key "delete")
    "w q" '(evil-quit :which-key "quit window/frame")
    "w o" '(delete-other-windows :which-key "only")
    "w s" '(split-window-below :which-key "split below")
    "w v" '(split-window-right :which-key "split right")
    "w h" '(windmove-left :which-key "left")
    "w j" '(windmove-down :which-key "down")
    "w k" '(windmove-up :which-key "up")
    "w l" '(windmove-right :which-key "right"))

  ;; Search (built-ins; swap for consult when you add it).
  (luser/leader
    "s s" '(isearch-forward-regexp :which-key "isearch regexp")
    "s o" '(occur :which-key "occur")
    "s i" '(imenu :which-key "imenu")
    "s p" '(project-find-regexp :which-key "project grep")
    "s g" '(grep :which-key "grep"))

  ;; Code (eglot, configured in programming.el).
  (luser/leader
    "c a" '(eglot-code-actions :which-key "actions")
    "c r" '(eglot-rename :which-key "rename")
    "c f" '(eglot-format :which-key "format")
    "c d" '(eldoc-doc-buffer :which-key "docs")
    "c g" '(xref-find-definitions :which-key "goto def")
    "c R" '(xref-find-references :which-key "references"))

  ;; Help.
  (luser/leader
    "h f" '(describe-function :which-key "function")
    "h v" '(describe-variable :which-key "variable")
    "h k" '(describe-key :which-key "key")
    "h m" '(describe-mode :which-key "mode")
    "h i" '(info :which-key "info"))

  ;; Jujutsu (majutsu, declared in vcs.el).  Everything else lives
  ;; behind the in-buffer transient (?); these are the entry points.
  (luser/leader
    "j j" '(majutsu :which-key "status")
    "j l" '(majutsu-log :which-key "log"))

  ;; Open.
  (luser/leader
    "o f" '(dired :which-key "dired")
    "o e" '(eshell :which-key "eshell")
    "o c" '(compile :which-key "compile"))

  ;; Toggle.
  (luser/leader
    "t n" '(display-line-numbers-mode :which-key "line numbers")
    "t w" '(whitespace-mode :which-key "whitespace")
    "t t" '(modus-themes-toggle :which-key "theme toggle")
    "t r" '(toggle-truncate-lines :which-key "truncate lines")
    "t f" '(auto-fill-mode :which-key "auto fill"))

  ;; Quit / frame.
  (luser/leader
    "q q" '(save-buffers-kill-emacs :which-key "quit")
    "q r" '(restart-emacs :which-key "restart")
    "q d" '(delete-frame :which-key "delete frame")))

(provide 'leader)
;;; leader.el ends here
