;;; init-lisp.el --- Configuration for lisp -*- lexical-binding: t -*-
;;; Commentary:

;; Settings for Lisp and lisp-like languages.  Elisp is configured
;; separately.

;;; Code:

(use-package paredit
  :diminish
  :preface
  (defun maybe-map-paredit-newline ()
    (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
                (minibufferp))
      (local-set-key (kbd "RET") 'paredit-newline)))
  :hook (((lisp-mode emacs-lisp-mode lisp-interaction-mode) . enable-paredit-mode)
         (paredit-mode . maybe-map-paredit-newline)))


(provide 'init-lisp)
;;; init-lisp.el ends here
