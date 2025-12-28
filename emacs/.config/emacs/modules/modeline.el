;;; Modeline -- custom modeline settings

;;; Commentary:
;; Set custom modeline information

;;; Code

(defgroup luser-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup luser-modeline-faces nil
  "Faces for my custom modeline."
  :group 'luser-modeline)

;;;; Faces

(defface luser-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
   Modify this face to, for example, add a :box attribute to all
   relevant indicators (combines nicely with my `spacious-padding'
   package).")

(defface luser-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-red-bg
  '((default :inherit (bold luser-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-green-bg
  '((default :inherit (bold luser-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators"
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-yellow-bg
  '((default :inherit (bold luser-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-blue-bg
  '((default :inherit (bold luser-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-magenta-bg
  '((default :inherit (bold luser-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators."
  :group 'luser-modeline-faces)

(defface luser-modeline-indicator-cyan-bg
  '((default :inherit (bold luser-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'luser-modeline-faces)


;;; Buffer remote indicator
(defvar-local luser-modeline-buffer-remote
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'luser-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

;;;; Buffer name and file state
(defun luser-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `luser-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

;;;; TODO: truncate if necessary
(defun luser-modeline--buffer-name ()
  "Return `buffer-name'."
  (buffer-name))

(defun luser-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (luser-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" "R- " name)
      name)))

(defun luser-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `luser-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local luser-modeline-buffer-identification
    '(:eval
      (propertize (luser-modeline-buffer-name)
                  'face (luser-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (luser-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major more indicator

(defun luser-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun luser-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun luser-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `luser-modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local luser-modeline-major-mode
    (list
     (propertize "%[" 'face 'luser-modeline-indicator-red)
     '(:eval
       (concat
        (luser-modeline-major-mode-indicator)
        " "
        (propertize
          (luser-modeline-major-mode-name)
         'mouse-face 'mode-line-highlight
         'help-echo (luser-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'luser-modeline-indicator-red))
  "Mode line construct for displaying major modes.")


;;;; All constructs should be `risky'
(dolist (construct
  '(luser-modeline-buffer-remote
    luser-modeline-buffer-identification
    luser-modeline-major-mode))
  (put construct 'risky-local-variable t))


;; (setq mode-line-format
;;       '("%e"
;;         luser-modeline-buffer-remote
;;         " "
;;         luser-modeline-buffer-identification
;;         "  "
;;         luser-modeline-major-mode
;;         "  "
;;         mode-line-format-right-align))

(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified
                  mode-line-remote mode-line-window-dedicated)
                 display (min-width (6.0)))
                mode-line-frame-identification mode-line-buffer-identification "   "
                mode-line-format-right-align (project-mode-line project-mode-line-format)
                (vc-mode vc-mode) " " mode-line-modes mode-line-misc-info))

(provide 'modeline)
;;; modeline.el ends here
