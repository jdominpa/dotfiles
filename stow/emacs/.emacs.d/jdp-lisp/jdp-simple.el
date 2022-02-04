;;; jdp-simple.el --- Simple functions for my Emacs configuration -*- lexical-binding: t -*-

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

;; Simple functions used in my personal Emacs configuration.

;;; Code:

(defun jdp-simple-newlines-above-or-below (arg)
  "Insert newlines above or below the current one while keeping the point
in the current one based on the prefix argument ARG as described below.

By default, insert a newline below.  Prefixed with
\\[universal-argument] or \\[negative-argument] insert the
newline above.  Prefixed with a numeric argument ARG, insert ARG
lines below if ARG is positive or insert ARG lines above if ARG
is negative."
  (interactive "*P")
  (let ((arg (cond
               ((not arg)
                1)
               ((listp arg)
                -1)
               (t
                (prefix-numeric-value arg)))))
    (save-excursion
      (if (< arg 0)
          (beginning-of-line)
        (end-of-line))
      (open-line (abs arg)))))

(defmacro jdp-simple-mark (name object &optional docstring)
  "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is the
syntactic construct that the function will act on.  Optional
DOCSTRING describes the resulting function."
  `(defun ,name (&optional arg allow-extend)
     ,docstring
     (interactive "P\np")
     (let ((forward-function (format "forward-%s" ,object)))
       (cond ((and allow-extend
                   (or (and (eq last-command this-command) (mark t))
                       (region-active-p)))
              (setq arg (if arg (prefix-numeric-value arg)
                          (if (< (mark) (point)) -1 1)))
              (set-mark
               (save-excursion
                 (goto-char (mark))
                 (funcall (intern forward-function) arg)
                 (point))))
             (t
              (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                (unless (consp bounds)
                  (user-error "No %s at point" ,object))
                (if (>= (prefix-numeric-value arg) 0)
                    (goto-char (car bounds))
                  (goto-char (cdr bounds)))
                (push-mark
                 (save-excursion
                   (funcall (intern forward-function) (prefix-numeric-value arg))
                   (point))
                 nil t)))))))

(jdp-simple-mark
 jdp-simple-mark-word
 "word"
 "Mark the whole word at point. With optional ARG, mark the
current word and any remaining ARG words away from point.  A
negative argument moves backward. Repeated invocations of this
command mark the next word in the direction originally specified.

This function is a slightly modified version of the built-in
`mark-word' that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use
`jdp-simple-mark-symbol' instead.")

(jdp-simple-mark
 jdp-simple-mark-symbol
 "symbol"
 "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining ARG
symbols away from point.  A negative argument moves backward.
Repeated invocations of this command mark the next symbol in the
direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

(defun jdp-simple-mark-sexp-backward (&optional arg)
  "Mark the previous ARG balanced expression[s].  ARG defaults to 1.
Just a convenient backward-looking `mark-sexp'."
  (interactive "P")
  (if arg
      (mark-sexp (- arg) t)
    (mark-sexp -1 t)))

(defun jdp-simple-mark-construct-dwim (&optional arg)
  "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `jdp-simple-mark-symbol',
`jdp-simple-mark-sexp-backward' and `mark-sexp'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARG objects while counting
the current one (so 3 would be 1+2 more).  A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `jdp-simple-mark-sexp-backward', so be mindful of
where the point is).  Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
  (interactive "P")
  (cond
   ((symbol-at-point)
    (jdp-simple-mark-symbol arg t))
   ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
    (jdp-simple-mark-sexp-backward arg))
   (t
    (mark-sexp arg t))))

(provide 'jdp-simple)
;;; jdp-simple.el ends here
