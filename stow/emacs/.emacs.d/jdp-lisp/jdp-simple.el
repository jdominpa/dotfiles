;;; jdp-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Common commands for my Emacs.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;; Commands for marking syntactic constructs

(defmacro jdp-simple--define-thing-marker (thing forward-thing &rest extra)
  `(defun ,(intern (format "jdp-simple-mark-%s" thing)) (&optional arg allow-extend)
     ,(format "Set mark ARG %ss from point or move mark one %s.
When called from Lisp with ALLOW-EXTEND omitted or nil, mark is set ARG
%ss from point.
With ARG and ALLOW-EXTEND both non-nil (interactively, with prefix
argument), the place to which mark goes is the same place \\[%s]
would move to with the same argument; if the mark is active, it moves
ARG %ss from its current position, otherwise it is set ARG %ss from
point.
When invoked interactively without a prefix argument and no active
region, mark the %s at point.
When invoked interactively without a prefix argument, and region is
active, mark moves one %s away of point (i.e., forward if mark is at or
after point, back if mark is before point), thus extending the region by
one %s.  Since the direction of region extension depends on the relative
position of mark and point, you can change the direction by
\\[exchange-point-and-mark]." thing thing thing forward-thing thing thing thing thing thing)
     (interactive "P\np")
     (cond ((and allow-extend
                 (or (and (eq last-command this-command) (mark t))
                     (region-active-p)))
            (setq arg (if arg (prefix-numeric-value arg)
                        (if (< (mark) (point)) -1 1)))
	        (set-mark
	         (save-excursion
	           (goto-char (mark))
	           (,forward-thing arg)
	           (point))))
	       (t
            ,(plist-get extra :pre)
	        (,forward-thing (prefix-numeric-value arg))
            ,(plist-get extra :post)
            (push-mark nil t t)
            (,forward-thing (- (prefix-numeric-value arg)))))))

(jdp-simple--define-thing-marker word forward-word
                                 :pre (when (and (looking-at "\\>") (> (prefix-numeric-value arg) 0))
                                        (forward-word -1)))

(jdp-simple--define-thing-marker line forward-line
                                 :post (unless (= (preceding-char) ?\n)
                                         (setq arg (1- (prefix-numeric-value arg)))))

(jdp-simple--define-thing-marker sexp forward-sexp)

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
