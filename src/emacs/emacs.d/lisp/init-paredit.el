;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :diminish
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . enable-paredit-mode)
  :config
  (defun maybe-map-paredit-newline ()
    (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
                (minibufferp))
      (local-set-key (kbd "RET") 'paredit-newline)))

  (add-hook 'paredit-mode-hook 'maybe-map-paredit-newline))


(provide 'init-paredit)
;;; init-paredit.el ends here
