;;; jdp-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Common commands for my Emacs: <https://protesilaos.com/emacs/dotemacs/>.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup jdp-simple ()
  "Generic utilities for my dotemacs."
  :group 'editing)

(defcustom jdp-simple-scratch-buffer-default-mode 'markdown-mode
  "Default major mode for `jdp-simple-scratch-buffer'."
  :type 'symbol
  :group 'jdp-simple)

;;; Generic setup

;;;; Scratch buffers
;; The idea is based on the `scratch.el' package by Ian Eure:
;; <https://github.com/ieure/scratch-el>.

(defun jdp-simple--scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           when (and (functionp sym)
                     (or (provided-mode-derived-p sym 'text-mode)
                         (provided-mode-derived-p sym 'prog-mode)))
           collect sym))

(defun jdp-simple--scratch-buffer-setup (region &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
         (string (format "Scratch buffer for: %s\n\n" major))
         (text (concat string region))
         (buf (format "*%s scratch*" major)))
    (with-current-buffer (pop-to-buffer buf)
      (funcall major)
      (if (jdp-common-empty-buffer-p)
          ;; We could use `save-restriction' for narrowed buffers, but
          ;; it is overkill.
          (progn
            (insert text)
            (goto-char (point-min))
            (comment-region (point-at-bol) (point-at-eol))
            (goto-char (point-max)))
        (goto-char (point-max))
        (when (jdp-common-line-regexp-p 'non-empty)
          (insert "\n\n"))
        (insert region)))))

(defun jdp-simple-scratch-buffer (&optional arg)
  "Produce a scratch buffer matching the current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `jdp-simple-scratch-buffer-default-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.  Candidates are derivatives of `text-mode' or
`prog-mode'.

If region is active, copy its contents to the new scratch
buffer.

Buffers are named as *MAJOR-MODE scratch*.  If one already exists
for the given MAJOR-MODE, any text is appended to it."
  (interactive "P")
  (let* ((default-mode jdp-simple-scratch-buffer-default-mode)
         (modes (jdp-simple--scratch-list-modes))
         (region (with-current-buffer (current-buffer)
                   (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     "")))
         mode)
    (pcase (prefix-numeric-value arg)
      (16 (progn
            (setq mode (intern (completing-read "Select major mode: " modes nil t)))
            (jdp-simple--scratch-buffer-setup region mode)))
      (4 (jdp-simple--scratch-buffer-setup region default-mode))
      (_ (jdp-simple--scratch-buffer-setup region)))))

;;; Commands

;;;; Commands for marking syntactic constructs

(defmacro jdp-simple-mark (name object &optional docstring)
  "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
  `(defun ,name (&optional arg allow-extend)
     ,docstring
     (interactive "P\np")
     (let ((x (format "%s-%s" "forward" ,object)))
       (cond ((and allow-extend
                   (or (and (eq last-command this-command) (mark t))
                       (region-active-p)))
              (setq arg (if arg (prefix-numeric-value arg)
                          (if (< (mark) (point)) -1 1)))
              (set-mark
               (save-excursion
                 (goto-char (mark))
                 (funcall (intern x) arg)
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
                   (funcall (intern x) (prefix-numeric-value arg))
                   (point)))
                (activate-mark)))))))

(jdp-simple-mark
 jdp-simple-mark-word
 "word"
 "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `jdp-simple-mark-symbol'
instead.")

(jdp-simple-mark
 jdp-simple-mark-symbol
 "symbol"
 "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

(defun jdp-simple-mark-construct-dwim (&optional arg)
  "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `jdp-simple-mark-sexp-backward',
`mark-sexp', and `jdp-simple-mark-symbol'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
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
