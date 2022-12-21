;;; jdp-orderless.el --- Extensions for Orderless -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Extensions for Orderless completion style for Emacs.

;;; Style dispatchers

(defun jdp-orderless-literal-dispatcher (pattern _index _total)
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun jdp-orderless-flex-dispatcher (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun jdp-orderless-regexp-dispatcher (pattern _index _total)
  (when (string-suffix-p "," pattern)
    `(orderless-regexp . ,(substring pattern 0 -1))))

(provide 'jdp-orderless)
;;; jdp-orderless.el ends here
