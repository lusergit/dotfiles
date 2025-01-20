;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; quantities
(setq doom-font (font-spec :family "Cascadia Code PL" :size 16.0 :weight 'semi-light)
      doom-theme 'modus-operandi
      display-line-numbers-type 'relative)

(set-face-attribute 'default nil :height 130)

(map! :leader
      :n
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)

(after! doom-ui (mood-line-mode))

(after! doom-ui
  (setq! auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-mode))

(setq scroll-conservatively 101
      scroll-margin 0)

(after! doom-ui (ultra-scroll-mode 1))
