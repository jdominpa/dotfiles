;;; init-haskell.el --- Support for Haskell language -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for the Haskell programming language.

;;; Code:

(require 'init-programming)

(use-package haskell-mode
  :hook (((haskell-mode haskell-cabal-mode) . subword-mode)
         (haskell-mode . eldoc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode))
  :mode "\\.ghci\\'"
  :bind (:map haskell-mode-map
              ("C-c h" . hoogle)))

(use-package dante
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante
                             '(warning . haskell-hlint)))


(provide 'init-haskell)
;;; init-haskell.el ends here
