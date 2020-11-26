;;; init-lsp.el --- Language server protocol settings -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for lsp-mode.

;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after lsp)


(provide 'init-lsp)
;;; init-lsp.el ends here
