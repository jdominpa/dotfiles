;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(use-package wgrep
  :bind (("C-c C-q" . wgrep-change-to-wgrep-mode)
         :map grep-mode-map
              ("w" . wgrep-change-to-wgrep-mode)))

(cond
 ((executable-find "ag")
  (use-package ag
    :bind ("C-c s" . ag-project)
    :config
    (use-package wgrep-ag)
    (setq-default ag-highlight-search t)))
 ((executable-find "rg")
  (use-package rg
    :bind ("C-c s" . rg-project))))


(provide 'init-grep)
;;; init-grep.el ends here
