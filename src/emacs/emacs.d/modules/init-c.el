;;; init-c.el --- cc-mode configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for editing C and C++ files.

;;; Code:

(require 'init-programming)

(use-package c-mode
  :ensure nil
  :preface
  (defun jdp/c-mode-common-defaults ()
    (setq c-basic-offset 4
          c-default-style "k&r")
    (c-set-offset 'substatement-open 0))
  :hook (c-mode-common . jdp/c-mode-common-defaults))


(provide 'init-c)
;;; init-c.el ends here
