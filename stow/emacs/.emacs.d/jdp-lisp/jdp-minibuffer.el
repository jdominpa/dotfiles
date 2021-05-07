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

;; Extensions for the minibuffer used in my Emacs configuration.
;; Almost everything is copied from https://protesilaos.com/dotemacs/.

;;; Code:

(require 'jdp-common)

(defgroup jdp-minibuffer ()
  "Extensions for the minibuffer."
  :group 'minibuffer)

(defcustom jdp-minibuffer-completion-windows-regexp
  "\\*\\(Completions\\|Embark Collect \\(Live\\|Completions\\)\\)"
  "Regexp to match window names with completion candidates.
Used by `jdp-minibuffer--get-completion-window'."
  :group 'jdp-minibuffer
  :type 'string)

(defcustom jdp-minibuffer-remove-shadowed-file-names nil
  "Delete shadowed parts of file names.

For example, if the user types ~/ after a long path name,
everything preceding the ~/ is removed so the interactive
selection process starts again from the user's $HOME.

Only works when variable `file-name-shadow-mode' is non-nil."
  :type 'boolean
  :group 'jdp-minibuffer)

(defcustom jdp-minibuffer-minimum-input 3
  "Live update completions when input is >= N.

Setting this to a value greater than 1 can help reduce the total
number of candidates that are being computed."
  :type 'integer
  :group 'jdp-minibuffer)

(defcustom jdp-minibuffer-live-update-delay 0.3
  "Delay in seconds before updating the Completions' buffer.

Set this to 0 to disable the delay."
  :type 'number
  :group 'jdp-minibuffer)

(defcustom jdp-minibuffer-completion-blocklist nil
  "Commands that do not do live updating of completions.

A less drastic measure is to set `jdp-minibuffer-minimum-input'
to an appropriate value.

The Completions' buffer can still be accessed with commands that
put it in a window (e.g. `jdp-minibuffer-toggle-completions',
`jdp-minibuffer-switch-to-completions-top')."
  :type '(repeat symbol)
  :group 'jdp-minibuffer)

(defcustom jdp-minibuffer-completion-passlist nil
  "Commands that do live updating of completions from the start.

This means that they ignore `jdp-minibuffer-minimum-input' and
the inherent constraint of updating the Completions' buffer only
upon user input.  Furthermore, they also bypass any possible
delay introduced by `jdp-minibuffer-live-update-delay'."
  :type '(repeat symbol)
  :group 'jdp-minibuffer)

;;;; Minibuffer behaviour

;; Thanks to Omar Antolín Camarena for providing the messageless and
;; stealthily.  Source: https://github.com/oantolin/emacs-config.
(defun jdp-minibuffer--messageless (fn &rest args)
  "Set `minibuffer-message-timeout' to 0.
Meant as advice around minibuffer completion FN with ARGS."
  (let ((minibuffer-message-timeout 0))
    (apply fn args)))

(dolist (fn '(exit-minibuffer
              choose-completion
              minibuffer-force-complete
              minibuffer-complete-and-exit
              minibuffer-force-complete-and-exit))
  (advice-add fn :around #'jdp-minibuffer--messageless))

;; Adapted from Omar Antolín Camarena's live-completions library:
;; https://github.com/oantolin/live-completions.
(defun jdp-minibuffer--honor-inhibit-message (fn &rest args)
  "Skip applying FN to ARGS if `inhibit-message' is t.
Meant as `:around' advice for `minibuffer-message', which does
not honor minibuffer message."
  (unless inhibit-message
    (apply fn args)))

(advice-add #'minibuffer-message :around #'jdp-minibuffer--honor-inhibit-message)

;; Copied from icomplete.el
(defun jdp-minibuffer--field-beg ()
  "Determine beginning of completion."
  (if (window-minibuffer-p)
      (minibuffer-prompt-end)
    (nth 0 completion-in-region--data)))

;; Copied from icomplete.el
(defun jdp-minibuffer--field-end ()
  "Determine end of completion."
  (if (window-minibuffer-p)
      (point-max)
    (nth 1 completion-in-region--data)))

;; Copied from icomplete.el
(defun jdp-minibuffer--completion-category ()
  "Return completion category."
  (let* ((beg (jdp-minibuffer--field-beg))
         (md (completion--field-metadata beg)))
    (alist-get 'category (cdr md))))

;; Adapted from icomplete.el
(defun jdp-minibuffer--shadow-filenames (&rest _)
  "Hide shadowed file names."
  (let ((saved-point (point)))
    (when (and
           jdp-minibuffer-remove-shadowed-file-names
           (eq (jdp-minibuffer--completion-category) 'file)
           rfn-eshadow-overlay (overlay-buffer rfn-eshadow-overlay)
           (eq this-command 'self-insert-command)
           (= saved-point (jdp-minibuffer--field-end))
           (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
               (eq ?/ (char-before (- (point) 2)))))
      (delete-region (overlay-start rfn-eshadow-overlay)
                     (overlay-end rfn-eshadow-overlay)))))

(defun jdp-minibuffer--setup-shadow-files ()
  "Set up shadowed file name deletion.
To be assigned to `minibuffer-setup-hook'."
  (add-hook 'after-change-functions #'jdp-minibuffer--shadow-filenames nil t))

(add-hook 'minibuffer-setup-hook #'jdp-minibuffer--setup-shadow-files)

;;;; Basic minibuffer interactions

;;;###autoload
(defun jdp-minibuffer-focus-minibuffer ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(defun jdp-minibuffer--get-completion-window ()
  "Find a live window showing completion candidates."
  (get-window-with-predicate
   (lambda (window)
     (string-match-p
      jdp-minibuffer-completion-windows-regexp
      (format "%s" window)))))

;; Adaptation of `icomplete-fido-backward-updir'.
;;;###autoload
(defun jdp-minibuffer-backward-updir ()
  "Delete char before point or go up a directory.
Must be bound to `minibuffer-local-filename-completion-map'."
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (jdp-minibuffer--completion-category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-delete-char)))

;;;; Minibuffer and Completions' buffer intersection

(defface jdp-minibuffer-hl-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#b0d8ff" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#103265" :foreground "#ffffff")
    (t :inherit (font-lock-string-face elfeed-search-title-face)))
  "Face for current line in the completions' buffer."
  :group 'jdp-minibuffer)

(defface jdp-minibuffer-line-number
  '((default :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#f2eff3" :foreground "#252525")
    (((class color) (min-colors 88) (background dark))
     :background "#151823" :foreground "#dddddd")
    (t :inverse-video t))
  "Face for line numbers in the completions' buffer."
  :group 'jdp-minibuffer)

(defface jdp-minibuffer-line-number-current-line
  '((default :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#8ac7ff" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#142a79" :foreground "#ffffff")
    (t :inverse-video t))
  "Face for current line number in the completions' buffer."
  :group 'jdp-minibuffer)

(autoload 'display-line-numbers-mode "display-line-numbers")
(autoload 'face-remap-remove-relative "face-remap")

;;;###autoload
(defun jdp-minibuffer-display-line-numbers ()
  "Set up line numbers for the completions' buffer.
Add this to `completion-list-mode-hook'."
  (when (derived-mode-p 'completion-list-mode)
    (face-remap-add-relative 'line-number 'jdp-minibuffer-line-number)
    (face-remap-add-relative 'line-number-current-line
                             'jdp-minibuffer-line-number-current-line)
    (display-line-numbers-mode 1)))

;;;###autoload
(defun jdp-minibuffer-hl-line ()
  "Set up line highlighting for the completions' buffer.
Add this to `completion-list-mode-hook'."
  (when (derived-mode-p 'completion-list-mode)
    (face-remap-add-relative 'hl-line 'jdp-minibuffer-hl-line)
    (hl-line-mode 1)))

(defun jdp-minibuffer--clean-completions ()
  "Keep only completion candidates in the Completions."
  (with-current-buffer standard-output
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (delete-region (point-at-bol) (1+ (point-at-eol)))
      (insert (propertize " "
                          'cursor-sensor-functions
                          (list
                           (lambda (_win prev dir)
                             (when (eq dir 'entered)
                               (goto-char prev))))))
      (put-text-property (point-min) (point) 'invisible t))))

(add-hook 'completion-list-mode-hook #'cursor-sensor-mode)
(add-hook 'completion-setup-hook #'jdp-minibuffer--clean-completions)

(defun jdp-minibuffer--fit-completions-window ()
  "Fit Completions' buffer to its window."
  (setq-local window-resize-pixelwise t)
  (fit-window-to-buffer (get-buffer-window "*Completions*")
                        (floor (frame-height) 2) 1))

(defun jdp-minibuffer--input-string ()
  "Return the contents of the minibuffer as a string."
  (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))

(defun jdp-minibuffer--minimum-input ()
  "Test for minimum requisite input for live completions."
  (>= (length (jdp-minibuffer--input-string)) jdp-minibuffer-minimum-input))

;; Adapted from Omar Antolín Camarena's live-completions library:
;; https://github.com/oantolin/live-completions.
(defun jdp-minibuffer--live-completions (&rest _)
  "Update the *Completions* buffer.
Meant to be added to `after-change-functions'."
  (when (minibufferp) ; skip if we've exited already
    (let ((while-no-input-ignore-events '(selection-request)))
      (while-no-input
        (if (jdp-minibuffer--minimum-input)
            (condition-case nil
                (save-match-data
                  (save-excursion
                    (goto-char (point-max))
                    (let ((inhibit-message t)
                          ;; don't ring the bell in `minibuffer-completion-help'
                          ;; when <= 1 completion exists.
                          (ring-bell-function #'ignore))
                      (minibuffer-completion-help)
                      (jdp-minibuffer--fit-completions-window))))
              (quit (abort-recursive-edit)))
          (minibuffer-hide-completions))))))

(defun jdp-minibuffer--live-completions-timer (&rest _)
  "Update Completions with `jdp-minibuffer-live-update-delay'."
  (let ((delay jdp-minibuffer-live-update-delay))
    (when (>= delay 0)
      (run-with-idle-timer delay nil #'jdp-minibuffer--live-completions))))

(defun jdp-minibuffer--setup-completions ()
  "Set up the completions buffer."
  (cond
   ((member this-command jdp-minibuffer-completion-passlist)
    (minibuffer-completion-help)
    (add-hook 'after-change-functions #'jdp-minibuffer--live-completions nil t))
   ((unless (member this-command jdp-minibuffer-completion-blocklist)
    (add-hook 'after-change-functions #'jdp-minibuffer--live-completions-timer nil t)))))

(add-hook 'minibuffer-setup-hook #'jdp-minibuffer--setup-completions)

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
  (if (or (eobp)
          (eq (point-max)
              (save-excursion (forward-line 1) (point))))
      (jdp-minibuffer-focus-minibuffer)
    (next-completion (or arg 1)))
  (setq this-command 'next-line))

;;;###autoload
(defun jdp-minibuffer-previous-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction it switches to the
minibuffer."
  (interactive "p")
  (let ((num (jdp-common-number-negative arg)))
    (if (or (bobp)
            (eq (point) (1+ (point-min)))) ; see hack in `jdp-minibuffer--clean-completions'
        (jdp-minibuffer-focus-minibuffer)
      (next-completion (or num 1)))))

(defun jdp-minibuffer--goto-line (n &optional args)
  "Go to line N in the Completions' with optional ARGS."
  (let ((bounds (count-lines (point-min) (point-max))))
    (if (<= n bounds)
        (progn
          `(,@args)
          (goto-char (point-min))
          (forward-line (1- n))
          (choose-completion))
      (user-error "%d is not within Completions' buffer bounds (%d)" n bounds))))

;;;###autoload
(defun jdp-minibuffer-choose-completion-number (n)
  "Select completion candidate on line number N with prefix arg.

The idea is to pass a prefix numeric argument that refers to a
line number in the Completions' buffer."
  (interactive "p")
  (if current-prefix-arg
      (cond
       ((and (derived-mode-p 'completion-list-mode)
             (active-minibuffer-window))
        (jdp-minibuffer--goto-line n))
       ((and (minibufferp)
             (jdp-minibuffer--get-completion-window))
        (jdp-minibuffer--goto-line n (select-window (jdp-minibuffer--get-completion-window))))
       (t
        (user-error "Only use this inside the minibuffer of the Completions")))
    (user-error "Pass a numeric argument first")))

(provide 'jdp-minibuffer)
;;; jdp-minibuffer.el ends here
