;;; init-c.el --- cc-mode configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for editing C and C++ files.  Clangd or ccls language
;; servers must be installed in the system for lsp-mode to work properly
;; (check https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd and
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-ccls for more information).

;;; Code:

(require 'init-programming)

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . lsp-deferred)
  :config
  (setq-default c-default-style "k&r"))


(provide 'init-c)
;;; init-c.el ends here
