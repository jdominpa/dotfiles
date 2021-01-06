;;; init-haskell.el --- Support for Haskell language -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for Haskell files.  Haskell-language-server must be installed
;; in the system for lsp-mode to work in haskell files (check
;; https://emacs-lsp.github.io/lsp-haskell for more information).

;;; Code:

(require 'init-programming)

(use-package haskell-mode
  :hook (((haskell-mode haskell-literate-mode) . lsp-deferred)
         ((haskell-mode haskell-cabal-mode) . subword-mode)
         (haskell-mode . eldoc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode))
  :mode "\\.ghci\\'"
  :bind (:map haskell-mode-map
              ("C-c h" . hoogle)))

(use-package lsp-haskell
  :after haskell-mode)


(provide 'init-haskell)
;;; init-haskell.el ends here
