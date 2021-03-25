;;; init-lisp.el --- Configuration for lisp -*- lexical-binding: t -*-
;;; Commentary:

;; Settings for Lisp and lisp-like languages.  Elisp is configured
;; separately.

;;; Code:

(use-package paredit
  :hook (((lisp-mode emacs-lisp-mode lisp-interaction-mode) . enable-paredit-mode)))


(provide 'init-lisp)
;;; init-lisp.el ends here
