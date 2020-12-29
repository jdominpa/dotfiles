;;; init-latex.el --- Support for LaTeX -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for writing latex documents.

;;; Code:

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . turn-on-auto-fill))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))


(provide 'init-latex)
;;; init-latex.el ends here
