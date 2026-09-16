;;; early-init.el --- Early startup configuration -*- lexical-binding: t; -*-

;;; Code:

;;; Native compilation -- log only, no startup popups for upstream warnings
(setq native-comp-async-report-warnings-errors 'silent)

;;; Performance
(setq gc-cons-threshold (* 100 1024 1024))
(defvar lz--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq file-name-handler-alist lz--file-name-handler-alist)))

;;; Package setup -- before Emacs's automatic package-initialize
(require 'package)
(setq package-quickstart t)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "No ssl!"))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
;; Generate the quickstart cache once so later startups activate
;; packages in a single `load' instead of per-package activation.
(unless (file-exists-p package-quickstart-file)
  (package-quickstart-refresh))
(setq use-package-always-ensure t)

;;; Customize file -- load early so options apply before init
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; UI -- disabled before the initial frame is created
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)
(setq visible-bell t)

;;; File loading
(setq load-prefer-newer t)
;;; early-init.el ends here