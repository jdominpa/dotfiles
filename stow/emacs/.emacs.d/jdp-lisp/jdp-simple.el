;;; jdp-simple.el --- Common commands for my Emacs configuration -*- lexical-binding: t -*-

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

;; Common commands used in my personal Emacs configuration.

;;; Code:

(defun jdp-simple-multi-line-next (&optional num)
  "Move cursor 15 lines forward.  If numeric argument NUM is
provided, move cursor 15*NUM lines forward."
  (interactive "p")
  (if num
      (forward-line (* 15 num))
    (forward-line 15)))

(defun jdp-simple-multi-line-prev (&optional num)
  "Move cursor 15 lines backward.  If numeric argument NUM is
provided, move cursor 15*NUM lines backward."
  (interactive "p")
  (if num
      (forward-line (* -15 num))
    (forward-line -15)))

(defun jdp-simple-reverse-delete-indentation ()
  "Join the current line to the following line."
  (interactive)
  (delete-indentation t))

(provide 'jdp-simple)
;;; jdp-simple.el ends here
