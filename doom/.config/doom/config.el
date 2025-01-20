;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; font
(setq doom-font (font-spec :family "Cascadia Code PL" :size 16.0 :weight 'semi-light))

;; relative line number
(setq display-line-numbers-type 'relative)

;; darkman config
(use-package! darkman
  :custom
  (darkman-themes '(:light modus-operandi
                    :dark modus-vivendi))
  (darkman-switch-themes-silently nil)
  :config
  (when (display-graphic-p)
    (add-hook! 'window-setup-hook (darkman-mode))) ;; after custom.el is loaded
  (when (daemonp)
    (add-hook 'server-after-make-frame-hook #'darkman-mode)
    (advice-add 'darkman-mode :after
                (lambda ()
                  (remove-hook 'server-after-make-frame-hook #'darkman-mode)))))

(map! :leader
      :n
      :desc "Open like spacemacs"
      "SPC" #'execute-extended-command)

(map! :leader
      :n
      :desc "Open like spacemacs"
      "f o" #'other-frame)

;; (setq doom-theme 'modus-vivendi)
