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
  :preface
  (defun jdp/c-mode-common-defaults ()
    (setq c-basic-offset 4
          c-default-style "k&r")
    (c-set-offset 'substatement-open 0))
  :hook ((c-mode-common . jdp/c-mode-common-defaults)
         (c-mode-common . lsp-deferred)))


(provide 'init-c)
;;; init-c.el ends here
