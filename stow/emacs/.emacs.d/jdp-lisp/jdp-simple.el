;;; jdp-simple.el --- Simple functions for my Emacs configuration -*- lexical-binding: t -*-

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

;; Simple functions used in my personal Emacs configuration.

;;; Code:

(defun jdp-simple-insert-newline-below (arg)
  "Insert a newline below keeping the point in the current line.
With prefix argument ARG, insert that many newlines below."
  (interactive "*p")
  (save-excursion
    (move-end-of-line 1)
    (newline arg)))

(defun jdp-simple-insert-newline-above (arg)
  "Insert newline above keeping the point in the current line.
With prefix argument ARG, insert that many newlines above."
  (interactive "*p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline arg)))

(provide 'jdp-simple)
;;; jdp-simple.el ends here
