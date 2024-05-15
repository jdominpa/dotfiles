;;; jdp-modeline.el --- Code for my customm mode line -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Functions and variables for my custom Emacs mode line.

;;; Code:

(defgroup jdp-mode-line nil
  "Custom mode line inspired by the default one."
  :group 'mode-line)

(defgroup jdp-mode-line-faces nil
  "Faces for my custom mode line."
  :group 'jdp-mode-line)

(defcustom jdp-mode-line-string-truncate-length 12
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface jdp-mode-line-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")

(defface jdp-mode-line-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-red-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-green-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-blue-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-cyan-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-gray-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicatovrs with a background."
  :group 'jdp-mode-line-faces)

;;;; Common helper functions

(defun jdp-mode-line--window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

(defun jdp-mode-line--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (if (string-empty-p str)
      str
    (and (jdp-mode-line--window-narrow-p)
         (> (length str) jdp-mode-line-string-truncate-length)
         (not (one-window-p :no-minibuffer)))))

(defun jdp-mode-line-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`jdp-mode-line-string-truncate-length'."
  (if (jdp-mode-line--string-truncate-p str)
      (concat (substring str 0 jdp-mode-line-string-truncate-length) "...")
    str))

(defun jdp-mode-line-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`jdp-mode-line-string-truncate-length' both from its beginning and end."
  (let ((half (floor jdp-mode-line-string-truncate-length 2)))
    (if (jdp-mode-line--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun jdp-mode-line--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun jdp-mode-line-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `jdp-mode-line-string-abbreviate-but-last'."
  (if (jdp-mode-line--string-truncate-p str)
      (mapconcat #'jdp-modeline--first-char (split-string str "[_-]") "-")
    str))

(defun jdp-mode-line-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `jdp-mode-line-string-abbreviate'."
  (if (jdp-mode-line--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'jdp-mode-line--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Keyboard macro indicator

(defvar-local jdp-mode-line-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'jdp-mode-line-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Displayed only on the current window's mode line.")

;;;; Narrow indicator

(defvar-local jdp-mode-line-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'jdp-mode-line-indicator-cyan-bg)))
  "Mode line construct to report the multilingual environment.  Displayed
only on the current window's mode line.")

;;;; Input method

(defvar-local jdp-mode-line-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face 'jdp-mode-line-indicator-green-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Remote file

(defvar-local jdp-mode-line-remote-file
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'jdp-mode-line-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

;;;; Dedicated window

(defvar-local jdp-mode-line-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face 'jdp-mode-line-indicator-gray-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;; Buffer name and modified status

(defun jdp-mode-line-buffer-identification-face ()
  "Return appropriate face or face list for `jdp-mode-line-buffer-identification'."
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

(defun jdp-mode-line--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `jdp-mode-line-string-cut-middle'."
  (when-let ((name (buffer-name)))
    (jdp-mode-line-string-cut-middle name)))

(defun jdp-mode-line-buffer-name ()
  "Return buffer name, with read-only indicator if applicable."
  (let ((name (jdp-mode-line--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun jdp-mode-line-buffer-name-help-echo ()
  "Return `help-echo' value for `jdp-mode-line-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local jdp-mode-line-buffer-identification
    '(:eval
      (propertize (jdp-mode-line-buffer-name)
                  'face (jdp-mode-line-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (jdp-mode-line-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major mode

(defun jdp-mode-line-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun jdp-mode-line-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun jdp-mode-line-major-mode-help-echo ()
  "Return `help-echo' value for `jdp-mode-line-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local jdp-mode-line-major-mode
    (list
     (propertize "%[" 'face 'jdp-mode-line-indicator-red)
     '(:eval
       (concat
        (jdp-mode-line-major-mode-indicator)
        " "
        (propertize
         (jdp-mode-line-string-abbreviate-but-last
          (jdp-mode-line-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (jdp-mode-line-major-mode-help-echo))))
     (propertize "%]" 'face 'jdp-mode-line-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local jdp-mode-line-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun jdp-mode-line--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar jdp-mode-line-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun jdp-mode-line--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun jdp-mode-line--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (jdp-mode-line--vc-help-echo file)
               'local-map jdp-mode-line-vc-map)))

(defun jdp-mode-line--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (jdp-mode-line-string-cut-end
   (jdp-mode-line--vc-text file branch face)))

(defvar jdp-mode-line--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun jdp-mode-line--vc-get-face (key)
  "Get face from KEY in `jdp-mode-line--vc-faces'."
   (alist-get key jdp-mode-line--vc-faces 'up-to-date))

(defun jdp-mode-line--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (jdp-mode-line--vc-get-face (vc-state file backend)))

(defvar-local jdp-mode-line-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  (branch (jdp-mode-line--vc-branch-name file backend))
                  (face (jdp-mode-line--vc-face file backend)))
        (jdp-mode-line--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.  Displayed only on
the current window's mode line.")

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local jdp-mode-line-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Displayed only on the current window's mode line.")

;;;; Miscellanous

(defvar-local jdp-mode-line-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.  Displayed only on
the current window's mode line.")

;;;; Risky local variables
(dolist (construct '(jdp-mode-line-kbd-macro
                     jdp-mode-line-narrow
                     jdp-mode-line-input-method
                     jdp-mode-line-remote-file
                     jdp-mode-line-window-dedicated-status
                     jdp-mode-line-buffer-identification
                     jdp-mode-line-major-mode
                     jdp-mode-line-process
                     jdp-mode-line-vc-branch
                     jdp-mode-line-eglot
                     jdp-mode-line-misc-info))
  (put construct 'risky-local-variable t))

(provide 'jdp-modeline)
;;; jdp-modeline.el ends here
