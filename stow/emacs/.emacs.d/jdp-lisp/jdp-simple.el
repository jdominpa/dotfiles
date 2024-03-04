;;; jdp-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Common commands for my Emacs.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;; Commands for buffer navigation

(defun jdp-simple-scroll-up-command ()
  "Move half a screen up."
  (interactive)
  (scroll-up-command (/ (window-height) 2))
  (recenter)
  (setq this-command 'scroll-up-command))

(defun jdp-simple-scroll-down-command ()
  "Move half a screen down."
  (interactive)
  (scroll-down-command (/ (window-height) 2))
  (recenter)
  (setq this-command 'scroll-down-command))

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
With optional ARG, mark the current word and any remaining ARGth
words away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next word
in the direction originally specified.")

(jdp-simple-mark
 jdp-simple-mark-line
 "line"
 "Mark the whole line at point.
With optional ARG, mark the current line and any remaining ARGth
lines away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next line
in the direction originally specified.")

(defun jdp-simple-mark-inside-sexp ()
  "Mark the contents inside the current sexp where point is at."
  (interactive)
  (let (beg end)
    (backward-up-list 1 t t)
    (setq beg (1+ (point)))
    (forward-sexp)
    (setq end (1- (point)))
    (goto-char beg)
    (push-mark)
    (goto-char end))
  (activate-mark))

(defun jdp-simple-kill-inside-sexp ()
  "Kill the contents inside the current sexp where point is at."
  (interactive)
  (jdp-simple-mark-inside-sexp)
  (kill-region (mark) (point)))

(defun jdp-simple-unwrap-sexp ()
  "Unwrap the contents inside the current sexp where point is at."
  (interactive)
  (let (end)
    (jdp-simple-mark-inside-sexp)
    (delete-char 1)
    (setq end (1- (point)))
    (goto-char (mark))
    (delete-char -1)
    (set-mark end)))

(defun jdp-simple-unwrap-mark-sexp ()
  "Unwrap and mark the contents inside the current sexp where point is at."
  (interactive)
  (jdp-simple-unwrap-sexp)
  (setq deactivate-mark nil))

(provide 'jdp-simple)
;;; jdp-simple.el ends here
