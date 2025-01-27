;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; quantities
(setq doom-font (font-spec :family "Cascadia Code PL" :size 16.0 :weight 'semi-light)
      doom-theme nil ; Let auto dark set the theme
      display-line-numbers-type 'relative)

(set-face-attribute 'default nil :height 130)

(map! :leader
      :n
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)

(setq custom-safe-themes t)

(after! doom-ui
  (setq! auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-mode))

(setq scroll-conservatively 101
      scroll-margin 0)
