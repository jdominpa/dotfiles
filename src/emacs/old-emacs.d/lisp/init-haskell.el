;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :hook (((haskell-mode haskell-cabal-mode) . subword-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . turn-on-haskell-indentation)
         (haskell-mode . haskell-auto-insert-module-template))
  :mode "\\.ghci\\'"
  :bind (:map haskell-mode-map
              ("C-c h" . hoogle))
  :config
  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'haskell-mode)))

(use-package dante
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante
                             '(warning . haskell-hlint)))


(provide 'init-haskell)
;;; init-haskell.el ends here