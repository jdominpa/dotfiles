;;; init-python.el --- python-mode configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for python files.

;;; Code:

(require 'init-programming)

(setq python-shell-interpreter "python3")

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred))


(provide 'init-python)
;;; init-python.el ends here
