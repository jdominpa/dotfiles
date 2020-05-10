;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
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


(provide 'init-paredit)
;;; init-paredit.el ends here
