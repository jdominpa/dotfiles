;;; init-programming.el --- prog-mode settings -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for prog-mode and programming related utilities.

;;; Code:

;; Make URL's in comments and strings clickable
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

;; Show the name of the current function definition in the modeline
(require 'which-func)
(add-hook 'prog-mode-hook 'which-function-mode)

;; Font-lock annotations like TODO in source code
(require 'hl-todo)
(add-hook 'prog-mode-hook 'global-hl-todo-mode)

;; Nice colors for delimiters in prog-mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Make comments autofill in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)))

;; On-the-fly syntax checking
(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list))

;; Change modeline color according to flycheck status
(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode))


(provide 'init-programming)
;;; init-programming.el ends here
