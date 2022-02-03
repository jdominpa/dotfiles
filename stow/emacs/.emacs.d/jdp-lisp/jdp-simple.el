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

(defun jdp-simple-insert-newline-above-or-below (arg)
  "Insert newlines above or below the current one while keeping the point
in the current one based on the prefix argument ARG as described below.

By default, insert a newline below.  Prefixed with
\\[universal-argument] or \\[negative-argument] insert the
newline above. Prefixed with a numeric argument ARG, insert ARG
lines below if ARG is positive (a numeric value of 0 is treated
as 1) or insert ARG lines above if ARG is negative."
  (interactive "*P")
  (let ((arg (pcase arg
               ((pred not)
                1)
               ((or (pred listp)
                    (pred (equal #'-)))
                -1)
               ((pred (= 0))
                1)
               (_
                arg))))
    (save-excursion
      (if (< arg 0)
          (beginning-of-line)
        (end-of-line))
      (open-line (abs arg)))))

(provide 'jdp-simple)
;;; jdp-simple.el ends here
