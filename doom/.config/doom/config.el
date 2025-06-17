;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; quantities
(setq doom-font (font-spec :family "Cascadia Code PL" :size 18.0 :weight 'semi-light)
      doom-theme nil                    ; let autodark manage it
      magit-process-finish-apply-ansi-colors t
      display-line-numbers-type 'relative)

(map! :leader
      :n
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)

(map! :leader
      :n
      :desc "Switch to next active frame" "f o" #'other-frame)
(setq custom-safe-themes t)

(use-package! auto-dark
  :config
  (setq auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-mode t))

;; treesit grammars
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")))

(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elixir-ls"))
                    :major-modes '(elixir-ts-mode)
                    :priority 30
                    :server-id 'elixir-ls)))

;; (use-package! jujutsu)

(use-package! gleam-ts-mode
  :config
  ;; setup formatter to be used by `SPC c f`
  (after! apheleia
    (setf (alist-get 'gleam-ts-mode apheleia-mode-alist) 'gleam)
    (setf (alist-get 'gleam apheleia-formatters) '("gleam" "format" "--stdin"))))

(use-package! typst-ts-mode
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-menu))

(after! gleam-ts-mode
  (setq treesit-extra-load-path (list (expand-file-name "~/.local/tree-sitter/")))
  (unless (treesit-language-available-p 'gleam)
    ;; hack: change `out-dir' when install language-grammar'
    (let ((orig-treesit--install-language-grammar-1 (symbol-function 'treesit--install-language-grammar-1)))
      (cl-letf (((symbol-function 'treesit--install-language-grammar-1)
                 (lambda (out-dir lang url)
                   (funcall orig-treesit--install-language-grammar-1
                            "~/.local/tree-sitter/" lang url))))
        (gleam-ts-install-grammar)))))

(use-package! kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(setopt treesit-font-lock-level 4)

(after! elixir-ts-mode (add-hook 'elixir-ts-mode-hook #'lsp))

(use-package! treesit-auto :config (global-treesit-auto-mode))

(use-package! evil :config (evil-set-initial-state 'vterm-mode 'emacs))

(use-package! mood-line :config (mood-line-mode))

(use-package! spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 10
           :tab-width 4
           :right-divider-width 50
           :scroll-bar-width 8))
  (add-hook 'server-after-make-frame-hook #'spacious-padding-mode))

;;;  Org sheninnigans to convert to HMS format and do calculations
(defun calcFunc-dateDiffToHMS (date1 date2 worktime-per-day)
  "Calculate the difference of DATE1 and DATE2 in HMS form.
Each day counts with WORKTIME-PER-DAY hours."
  (cl-labels ((dateTrunc (date)
                (calcFunc-date (calcFunc-year date)
                               (calcFunc-month date)
                               (calcFunc-day date)))
              (datep (date)
                (and (listp date)
                     (eq (car date) 'date))))
    (if (and (datep date1)
             (datep date2))
        (let* ((business-days (calcFunc-bsub
                               (dateTrunc date1)
                               (dateTrunc date2))))
          (calcFunc-add
           (calcFunc-hms (calcFunc-mul business-days worktime-per-day) 0 0)
           (calcFunc-sub (calcFunc-time date1) (calcFunc-time date2)))
          )
      0)))

(defcustom org-table-filters '(("org-to-hms" . org-time-string-to-calc-hms)
                               ("hms-to-org" . org-calc-hms-to-org-time-string))
  "Alist of filters for org table formulas.
They can be applied for reading the arguments and writing the results.
The `car' of each member is the identifier of the filter
the `cdr' is the function to be called."
  :group 'org-table
  :type '(repeat (cons (string :tag "Identifier of the filter") (symbol "Filter function"))))

(defun org-calc-hms-to-org-time-string (str)
  "Transform calc hms duration to org time string in STR and visa versa."
  (if (string-match "-?\\(?:\\([0-9]+\\)@\\)? *\\([0-9]+\\)' *\\([0-9.eE+-]+\\)\"" str)
      (let ((hour (string-to-number (or (match-string 1 str) "0")))
            (min (string-to-number (match-string 2 str)))
            (sec (string-to-number (match-string 3 str))))
        (format "%d:%02d:%02d" hour min sec))
    str))

(defun org-time-string-to-calc-hms (str)
  "Transform org time string STR into calc hms format."
  (if (string-match "\\(-?\\)\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" str)
      (let ((minus (match-string 1 str))
            (hour (string-to-number (or (match-string 2 str) "0")))
            (min (string-to-number (match-string 3 str)))
            (sec (string-to-number (match-string 4 str))))
        (if minus
            (format "-(%d@ %d' %d\")" hour min sec)
          (format "%d@ %d' %d\"" hour min sec)))
    str))

(defmacro org-table-filter (flags kind)
  "Return filter in FLAGS.
KIND may be \"<\" for input filter and \">\" for output filter.
If there is no filter of the requested kind in FLAGS return `identity'."
  `(if (string-match ,(concat kind "(\\([^)]+\\))") ,flags)
       (prog1
           (or (cdr (assoc-string (match-string 1 ,flags) org-table-filters)) #'identity)
         (setq ,flags (replace-match "" nil nil ,flags)))
     #'identity))

(defun org-table-input-filter (str filter)
  "Apply FILTER to string STR.
STR can also be a list.
In that case apply \\fn to each element of that list."
  (cond
   ((listp str)
    (mapcar (lambda (el)
              (org-table-input-filter el filter))
            str))
   ((stringp str)
    (funcall filter str))
   (t str)))

(defun org-table-eval-formula-filters (oldfun _arg equation &rest _args)
  "Apply filters to the arguments and the result of a table EQUATION.
This is an :override advice for OLDFUN `org-table-eval-formula'."
  (cl-multiple-value-bind
      (eq flags) (split-string equation ";")
    (if (stringp flags)
        (let ((input-filter (org-table-filter flags "<"))
              (output-filter (org-table-filter flags ">")))
          (cl-letf* ((old-justify-field-maybe (symbol-function 'org-table-justify-field-maybe))
                     (old-table-make-reference (symbol-function 'org-table-make-reference))
                     ((symbol-function 'org-table-make-reference)
                      (lambda (elements &rest __args)
                        (apply old-table-make-reference
                               (org-table-input-filter elements input-filter)
                               __args)))
                     ((symbol-function 'org-table-justify-field-maybe)
                      (lambda (&optional new)
                        (funcall old-justify-field-maybe (and (stringp new)
                                                              (funcall output-filter new))))))
            (apply oldfun _arg (concat eq ";" flags) _args)))
      (apply oldfun _arg equation _args))))

(defun org-scratch-buffer ()
  "creates an org mode scratch buffer"
  (interactive)
  (let ((n 0)
        bufname buffer)
    (catch 'done
      (while t
        (setq bufname (concat "*org-scratch" (if (= n 0) "" (int-to-string n)) "*"))
        (setq n (1+ n))
        (when (not (get-buffer bufname))
          (setq buffer (get-buffer-create bufname))
          (with-current-buffer buffer
            (org-mode))
          ;; When called non-interactively, the `t` targets the other window (if it exists).
          (throw 'done (display-buffer buffer t))) ))))

(advice-add 'org-table-eval-formula :around #'org-table-eval-formula-filters)
(setq pgtk-wait-for-event-timeout nil)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
