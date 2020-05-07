;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(use-package wgrep
  :bind (:map grep-mode-map
              ("C-c C-q" . wgrep-change-to-wgrep-mode)
              ("w" . wgrep-change-to-wgrep-mode)))

(use-package ag
  :if (executable-find "ag")
  :bind ("C-c s" . ag-project)
  :config
  (use-package wgrep-ag)
  (setq-default ag-highlight-search t))

(use-package rg
  :if (executable-find "rg")
  :bind ("C-c s" . rg-project))


(provide 'init-grep)
;;; init-grep.el ends here
