;;; init-haskell.el --- Support for Haskell language -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for Haskell files.  Haskell-language-server must be installed
;; in the system for lsp-mode to work in haskell files (check
;; https://emacs-lsp.github.io/lsp-haskell for more information).

;;; Code:

(require 'init-programming)

(use-package haskell-mode
  :init (use-package lsp-haskell)
  :hook (((haskell-mode haskell-cabal-mode) . lsp-mode)
         ((haskell-mode haskell-cabal-mode) . subword-mode)
         (haskell-mode . eldoc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode))
  :mode "\\.ghci\\'"
  :bind (:map haskell-mode-map
              ("C-c h" . hoogle)))

;; Currently disabled in favor of lsp-mode
(use-package dante
  :disabled t
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante
                             '(warning . haskell-hlint)))


(provide 'init-haskell)
;;; init-haskell.el ends here
