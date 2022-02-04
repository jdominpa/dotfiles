;;; jdp-fonts.el --- Font configurations -*- lexical-binding: t -*-

;; Author: Joan Domingo Pasarin
;; Maintainer: Joan Domingo Pasarin

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This set of configurations pertains to my font settings, for use in
;; my Emacs setup. Note that this package "requires" Emacs 24.3 or
;; higher, though I only tested it with versions 27 and 28.

;;; Code:

;;; Customisation options
(defgroup jdp-fonts ()
  "Font-related configurations for my Emacs setup."
  :group 'font)

(defcustom jdp-fonts-typeface-sets-alist
  '((laptop 90 "Hack" normal "DejaVu Sans" normal)
    (desktop 130 "Iosevka" light "Roboto" normal)
    (reader 150 "Iosevka" light "Fira Code" normal)
    (presentation 180 "Iosevka" light "Source Sans Pro" normal))
  "Alist of desired typefaces and their particularities.

The list specifies, in this order:

0. Display type of context, used to recognise the association.

1. Font height as an integer that is 10x the point size.

2. The family name (as a string) of the monospaced typeface that
will be assigned to the `default' and `fixed-pitch' faces.

3. The main weight of the monospaced family.

4. The family name of the proportionately spaced typeface that
will be assigned to the `variable-pitch' face.

5. The weight of the proportionately spaced family.

It is assumed that all those typefaces already exist on the
system and we make no effort whatsoever to run relevant tests."
  :group 'jdp-fonts
  :type 'alist)

(defcustom jdp-fonts-monospaced-list
  '("Hack" "DejaVu Sans Mono" "Iosevka" "Source Code Pro"
    "Ubuntu Mono" "Fantasque Sans Mono" "Fira Code" "Monoid")
  "List of typefaces for coding.

It is assumed that those already exist on the system, otherwise
an error will be displayed when trying to set one of them."
  :group 'jdp-fonts
  :type 'list)

(defcustom jdp-fonts-heights-list
  '(100 110 120 130 140 150 160 170 180 190 200)
  "List of font heights for `jdp-fonts-set-font-size-family'."
  :group 'jdp-fonts
  :type 'list)

(defcustom jdp-fonts-line-spacing-alist
  '(("Source Code Pro" . 1)
    ("Ubuntu Mono" . 2))
  "Font families in need of extra line spacing.

The alist defines a font family as a string and the desired
integer to pass to the `line-spacing' variable."
  :group 'jdp-fonts
  :type 'alist)

(defcustom jdp-fonts-laptop-desktop-keys-list '(laptop desktop)
  "Symbols for `jdp-fonts-fonts-per-monitor'.
This is a list whose first item denotes the smallest desirable
entry in `jdp-fonts-typeface-sets-alist' for use on a laptop or
just smaller monitor, while the second points to a larger
display's key in that same alist."
  :group 'jdp-fonts
  :type 'list)

(defcustom jdp-fonts-max-small-resolution-width 1366
  "Maximum width for use in `jdp-fonts-fonts-per-monitor'."
  :group 'jdp-fonts
  :type 'integer)

(defcustom jdp-fonts-bold-weight-alist
  '(("Iosevka Comfy" . semibold)
    ("Fira Code" . semibold)
    ("Source Code Pro" . semibold))
  "Font families in need of a different weight for `bold'.

The alist defines a font family as a string and the desired style
to pass to the `bold' face's weight property."
  :group 'jdp-fonts
  :type 'alist)

;;; Variables

(defvar jdp-fonts-set-typeface-hook nil
  "Hook that is called after setting fonts.")

(defvar jdp-fonts-font-display-hist '()
  "History of inputs for display-related font associations.")

(defvar jdp-fonts-font-family-hist '()
  "History of inputs for font families.")

(defvar jdp-fonts-font-height-hist '()
  "History of inputs for font heights.")

;;; Functions

(defun jdp-fonts--set-face-attribute (face family &optional weight height)
  "Set FACE font to FAMILY, with optional HEIGHT and WEIGHT."
  (let* ((u (if (eq face 'default) 100 1.0))
         (h (or height u))
         (w (or weight 'normal)))
    ;; Read this: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920>
    ;; Hence why the following fails.  Keeping it for posterity...
    ;; (set-face-attribute face nil :family family :weight w :height h)
    (if (eq (face-attribute face :weight) w)
          (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight w 0)
      (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight w 0))
    (internal-set-lisp-face-attribute face :height h 0)))

(defun jdp-fonts--return-nth (choice displays data n)
  "Check if CHOICE maps to DISPLAYS from DATA; return N."
  (if (member choice displays)
      (nth n (assoc choice data))
    (error "'%s' not a member of %s" choice displays)))

(defun jdp-fonts--display-prompt (displays)
  "Prompt for candidate among DISPLAYS."
  (let ((def (nth 1 jdp-fonts-font-display-hist)))
    (completing-read
     "Pick display size: "
     displays nil nil nil 'jdp-fonts-font-display-hist def)))

(defun jdp-fonts-set-fonts (&optional height font-mono font-var weight-mono weight-var)
  "Set default font size using presets.

HEIGHT is the font's height as 10x its point size.  FONT-MONO
should be a monospaced typeface, due to the alignment
requirements of the `fixed-pitch' face.  FONT-VAR could be a
proportionately spaced typeface or even a monospaced one, since
the `variable-pitch' it applies to is not supposed to be
spacing-sensitive.  Both families must be represented as a string
holding the family's name.

WEIGHT-MONO is the weight property of FONT-MONO, while WEIGHT-VAR
is that of FONT-VAR."
  (interactive)
  (if (display-graphic-p)
      (let* ((data jdp-fonts-typeface-sets-alist)
             (displays (mapcar #'car jdp-fonts-typeface-sets-alist))
             (display-strings (mapcar (lambda (x)
                                        (format "%s" (car x)))
                                      jdp-fonts-typeface-sets-alist))
             (prompt (unless height
                       (jdp-fonts--display-prompt display-strings)))
             (choice (or height (intern prompt)))
             (size (or height (jdp-fonts--return-nth choice displays data 1)))
             (mono (or font-mono (jdp-fonts--return-nth choice displays data 2)))
             (weight-m (or weight-mono (jdp-fonts--return-nth choice displays data 3)))
             (var (or font-var (jdp-fonts--return-nth choice displays data 4)))
             (weight-v (or weight-var (jdp-fonts--return-nth choice displays data 5))))
        (jdp-fonts--set-face-attribute 'default mono weight-m size)
        (jdp-fonts--set-face-attribute 'fixed-pitch mono weight-m)
        (jdp-fonts--set-face-attribute 'variable-pitch var weight-v)
        (run-hooks 'jdp-fonts-set-typeface-hook)
        (add-to-history 'jdp-fonts-font-display-hist prompt))
    (error "Not running a graphical Emacs; cannot set fonts")))

(defun jdp-fonts-set-font-size-family ()
  "Set point size and main typeface.
This command is mostly intended for testing typefaces defined in
`jdp-fonts-monospaced-list' at common heights specified in
`jdp-fonts-heights-list'."
  (interactive)
  (if (display-graphic-p)
      (let* ((fonts jdp-fonts-monospaced-list)
             (font (completing-read "Select main font: " fonts nil nil
                                    nil 'jdp-fonts-font-family-hist))
             (nums jdp-fonts-heights-list)
             (sizes (mapcar 'number-to-string nums))
             (size (completing-read "Select or insert number: " sizes nil nil
                                    nil 'jdp-fonts-font-height-hist))
             (var (face-attribute 'variable-pitch :family)))
        (jdp-fonts--set-face-attribute 'default font 'normal (string-to-number size))
        (jdp-fonts--set-face-attribute 'fixed-pitch font)
        (jdp-fonts--set-face-attribute 'variable-pitch var)
        (run-hooks 'jdp-fonts-set-typeface-hook)
        (add-to-history 'jdp-fonts-font-family-hist font)
        (add-to-history 'jdp-fonts-font-height-hist size))
    (error "Not running a graphical Emacs; cannot set fonts")))

(defun jdp-fonts-set-fonts-dwim (&optional arg)
  "Set fonts interactively.
With optional prefix ARG (\\[universal-argument]) call
`jdp-fonts-set-font-size-family' else default to
`jdp-fonts-set-fonts'.

This is just a wrapper around `jdp-fonts-set-fonts' and
`jdp-fonts-set-font-size-family', whose sole purpose is to
economise on dedicated key bindings."
  (interactive "P")
  (if arg
      (jdp-fonts-set-font-size-family)
    (jdp-fonts-set-fonts)))

(defmacro jdp-fonts--font-adjustment (fn doc alist cond1 cond2)
  "Macro for functions that employ `jdp-fonts-set-typeface-hook'.
FN is the name of the resulting function.  DOC is its docstring.
ALIST is an assosiation list of cons cells.  COND1 and COND2 is
the body of an `if' statement's 'if' and 'then' part
respectively."
  `(defun ,fn ()
     ,doc
     (let* ((data ,alist)
            (fonts (mapcar #'car data))
            (font (face-attribute 'default :family))
            (x (cdr (assoc font data))))
       (if (member font fonts)
           ,cond1
         ,cond2))))

(jdp-fonts--font-adjustment
 jdp-fonts-line-spacing
 "Determine desirable `line-spacing', based on font family."
 jdp-fonts-line-spacing-alist
 (setq-default line-spacing x)
 (setq-default line-spacing nil))

;; XXX: This will not work with every theme, but only those that
;; inherit the `bold' face instead of specifying a weight property.
;; The intent is to configure this once and have it propagate wherever
;; a heavier weight is displayed.
(jdp-fonts--font-adjustment
 jdp-fonts-bold-face
 "Determine weight for the `bold' face, based on font family."
 jdp-fonts-bold-weight-alist
 (set-face-attribute 'bold nil :weight x)
 (set-face-attribute 'bold nil :weight 'bold))

(defun jdp-fonts--display-type-for-monitor (&optional smaller larger)
  "Determine typeface specs based on monitor width.
Optional SMALLER and LARGER are two keys that point to entries in
`jdp-fonts-typeface-sets-alist'.  The default uses the relevant
keys from `jdp-fonts-laptop-desktop-keys-list'."
  (let* ((keys jdp-fonts-laptop-desktop-keys-list)
         (face-specs jdp-fonts-typeface-sets-alist)
         (small (or smaller (nth 0 keys)))
         (large (or larger (nth 1 keys)))
         (max-width jdp-fonts-max-small-resolution-width)
         (spec (if (<= (display-pixel-width) max-width)
                   small
                 large)))
    (unless (assoc spec face-specs)
      (error (concat "Key <<%s>> in `jdp-fonts-laptop-desktop-keys-list' "
                     "does not reference anything in "
                     "`jdp-fonts-typeface-sets-alist'")
             spec))
    spec))

(defun jdp-fonts-fonts-per-monitor ()
  "Use font settings based on screen size."
  (when (display-graphic-p)
    (let* ((display (jdp-fonts--display-type-for-monitor))
           (data jdp-fonts-typeface-sets-alist)
           (size (cadr (assoc display data)))
           (mono (nth 2 (assoc display data)))
           (weight-m (nth 3 (assoc display data)))
           (var (nth 4 (assoc display data)))
           (weight-v (nth 5 (assoc display data))))
      (jdp-fonts--set-face-attribute 'default mono weight-m size)
      (jdp-fonts--set-face-attribute 'fixed-pitch mono weight-m)
      (jdp-fonts--set-face-attribute 'variable-pitch var weight-v)
    (run-hooks 'jdp-fonts-set-typeface-hook))))

(provide 'jdp-fonts)
;;; jdp-fonts.el ends here
