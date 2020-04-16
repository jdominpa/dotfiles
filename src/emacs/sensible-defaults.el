;;; sensible-defaults.el --- Reasonable settings for getting started.

;; Author: Harry R. Schwartz <hello@harryrschwartz.com>
;; Version: 1.0.0
;; URL: https://github.com/hrs/sensible-defaults.el/sensible-defaults.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


(provide 'sensible-defaults)

;; Utility functions:

(defun sensible-defaults/reset-text-size ()
  (interactive)
  (text-scale-set 0))

;; Settings:

(defun sensible-defaults/open-files-from-home-directory ()
  "When opening a file, start searching at the user's home
directory."
  (setq default-directory "~/"))

(defun sensible-defaults/delete-trailing-whitespace ()
  "Call DELETE-TRAILING-WHITESPACE every time a buffer is saved."
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun sensible-defaults/treat-camelcase-as-separate-words ()
  "Treat CamelCaseSubWords as separate words in every programming
mode."
  (add-hook 'prog-mode-hook 'subword-mode))

(defun sensible-defaults/automatically-follow-symlinks ()
  "When opening a file, always follow symlinks."
  (setq vc-follow-symlinks t))

(defun sensible-defaults/make-scripts-executable ()
  "When saving a file that starts with `#!', make it executable."
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(defun sensible-defaults/single-space-after-periods ()
  "Don't assume that sentences should have two spaces after
periods. This ain't a typewriter."
  (setq sentence-end-double-space nil))

(defun sensible-defaults/offer-to-create-parent-directories-on-save ()
  "When saving a file in a directory that doesn't exist, offer
to (recursively) create the file's parent directories."
  (add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t)))))))

(defun sensible-defaults/overwrite-selected-text ()
  "If some text is selected, and you type some text, delete the
selected text and start inserting your typed text."
  (delete-selection-mode t))

(defun sensible-defaults/ensure-that-files-end-with-newline ()
  "If you save a file that doesn't end with a newline,
automatically append one."
  (setq require-final-newline t))

(defun sensible-defaults/confirm-closing-emacs ()
  "Ask if you're sure that you want to close Emacs."
  (setq confirm-kill-emacs 'y-or-n-p))

(defun sensible-defaults/quiet-startup ()
  "Don't present the usual startup message, and clear the scratch
buffer."
  (setq inhibit-startup-message t)
  (setq inhibit-splash-screen t)
  (setq initial-scratch-message nil))

(defun sensible-defaults/make-dired-file-sizes-human-readable ()
  "Add file sizes in human-readable units (KB, MB, etc) to dired
buffers."
  (setq-default dired-listing-switches "-alh"))

(defun sensible-defaults/shorten-yes-or-no ()
  "Don't ask `yes/no?', ask `y/n?'."
  (fset 'yes-or-no-p 'y-or-n-p))

(defun sensible-defaults/always-highlight-code ()
  "Turn on syntax highlighting whenever possible."
  (global-font-lock-mode t))

(defun sensible-defaults/refresh-buffers-when-files-change ()
  "When something changes a file, automatically refresh the
buffer containing that file so they can't get out of sync."
  (global-auto-revert-mode t))

(defun sensible-defaults/show-matching-parens ()
  "Visually indicate matching pairs of parentheses."
  (show-paren-mode t)
  (setq show-paren-delay 0.0))

(defun sensible-defaults/flash-screen-instead-of-ringing-bell ()
  "When you perform a problematic operation, flash the screen
instead of ringing the terminal bell."
  (setq visible-bell t))

(defun sensible-defaults/open-clicked-files-in-same-frame-on-mac ()
  "When you double-click on a file in the Mac Finder open it as a
buffer in the existing Emacs frame, rather than creating a new
frame just for that file."
  (setq ns-pop-up-frames nil))

(defun sensible-defaults/use-all-settings ()
  "Use all of the sensible-defaults settings."
  (sensible-defaults/open-files-from-home-directory)
  (sensible-defaults/delete-trailing-whitespace)
  (sensible-defaults/treat-camelcase-as-separate-words)
  (sensible-defaults/automatically-follow-symlinks)
  (sensible-defaults/make-scripts-executable)
  (sensible-defaults/single-space-after-periods)
  (sensible-defaults/offer-to-create-parent-directories-on-save)
  (sensible-defaults/overwrite-selected-text)
  (sensible-defaults/ensure-that-files-end-with-newline)
  (sensible-defaults/confirm-closing-emacs)
  (sensible-defaults/quiet-startup)
  (sensible-defaults/make-dired-file-sizes-human-readable)
  (sensible-defaults/shorten-yes-or-no)
  (sensible-defaults/always-highlight-code)
  (sensible-defaults/refresh-buffers-when-files-change)
  (sensible-defaults/show-matching-parens)
  (sensible-defaults/flash-screen-instead-of-ringing-bell)
  (sensible-defaults/open-clicked-files-in-same-frame-on-mac))

;; Keybindings:

(defun sensible-defaults/bind-home-and-end-keys ()
  "Make <home> and <end> move point to the beginning and end of
the line, respectively."
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line))

(defun sensible-defaults/bind-keys-to-change-text-size ()
  "Bind C-+ and C-- to increase and decrease text size,
respectively."
  (define-key global-map (kbd "C-)") 'sensible-defaults/reset-text-size)
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C-=") 'text-scale-increase)
  (define-key global-map (kbd "C-_") 'text-scale-decrease)
  (define-key global-map (kbd "C--") 'text-scale-decrease))

(defun sensible-defaults/use-all-keybindings ()
  "Use all of the sensible-defaults keybindings."
  (sensible-defaults/bind-home-and-end-keys)
  (sensible-defaults/bind-keys-to-change-text-size))

(provide 'sensible-defaults)
;;; sensible-defaults.el ends here
