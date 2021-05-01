;;; jdp-minibuffer.el --- Extensions for the minibuffer -*- lexical-binding: t -*-

;; Author: Joan Domingo Pasarin
;; Maintainer: Joan Domingo Pasarin
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

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

;; Extensions for the minibuffer used in my Emacs configuration

;;; Code:

;;;###autoload
(defun jdp-minibuffer-focus-mini ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(defun jdp-minibuffer--fit-completions-window ()
  "Fit Completions' buffer to its window."
  (fit-window-to-buffer (get-buffer-window "*Completions*")
                        (floor (frame-height) 2) 1))

;;;###autoload
(defun jdp-minibuffer-toggle-completions ()
  "Toggle the presentation of the completions' buffer."
  (interactive)
  (if (get-buffer-window "*Completions*" 0)
      (minibuffer-hide-completions)
    (minibuffer-completion-help)
    (jdp-minibuffer--fit-completions-window)))

;;;###autoload
(defun jdp-minibuffer-keyboard-quit-dwim ()
  "Control the exit behaviour for completions' buffers.

If in a completions' buffer and unless the region is active, run
`abort-recursive-edit'.  Otherwise run `keyboard-quit'.

If the region is active, deactivate it.  A second invocation of
this command is then required to abort the session."
  (interactive)
  (when (derived-mode-p 'completion-list-mode)
    (if (use-region-p)
        (keyboard-quit)
      (abort-recursive-edit))))

(defun jdp-minibuffer--switch-to-completions ()
  "Subroutine for switching to the completions' buffer."
  (unless (get-buffer-window "*Completions*" 0)
    (minibuffer-completion-help))
  (switch-to-completions)
  (jdp-minibuffer--fit-completions-window))

;;;###autoload
(defun jdp-minibuffer-switch-to-completions-top ()
  "Switch to the top of the completions' buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (jdp-minibuffer--switch-to-completions)
  (goto-char (point-min))
  (next-completion 1))

;;;###autoload
(defun jdp-minibuffer-switch-to-completions-bottom ()
  "Switch to the bottom of the completions' buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (jdp-minibuffer--switch-to-completions)
  (goto-char (point-max))
  (next-completion -1)
  (goto-char (point-at-bol))
  (recenter
   (- -1
      (min (max 0 scroll-margin)
           (truncate (/ (window-body-height) 4.0))))
      t))

;;;###autoload
(defun jdp-minibuffer-next-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction it switches to the
minibuffer."
  (interactive "p")
  (if (eobp)
      (jdp-minibuffer-focus-mini)
    (next-completion (or arg 1))))

;;;###autoload
(defun jdp-minibuffer-previous-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction it switches to the
minibuffer."
  (interactive "p")
  (let ((num (if (and (numberp arg) (> arg 0))
		 (* -1 arg)
	       (error "Invalid argument %s" arg))))
    (if (bobp)
        (jdp-minibuffer-focus-mini)
      (next-completion (or num 1)))))

(provide 'jdp-minibuffer)
;;; jdp-minibuffer.el ends here
