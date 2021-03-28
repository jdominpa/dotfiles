;;; jdp-orderless.el --- Orderless extensions -*- lexical-binding: t -*-

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

;; Extensions to use with the orderless package.

;;; Code:

(defgroup jdp-orderless ()
  "Tweaks for the Orderless completion style."
  :group 'minibuffer)

(defcustom jdp-orderless-default-styles
  '(orderless-flex
    orderless-strict-leading-initialism
    orderless-regexp
    orderless-prefixes
    orderless-literal)
  "List that should be assigned to `orderless-matching-styles'."
  :type 'list
  :group 'jdp-orderless)

(defcustom jdp-orderless-alternative-styles
  '(orderless-literal
    orderless-prefixes
    orderless-strict-leading-initialism
    orderless-regexp)
  "Alternative list for `orderless-matching-styles'.

Unlike `jdp-orderless-default-styles', this variable is intended
for use on a case-by-case basis, with the help of the function
`jdp-orderless-with-styles'."
  :type 'list
  :group 'jdp-orderless)

(defun jdp-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun jdp-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

(defun jdp-orderless-flex-dispatcher (pattern _index _total)
  "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(provide 'jdp-orderless)
;;; jdp-orderless.el ends here
