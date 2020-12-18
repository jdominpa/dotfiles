;;; init-latex.el --- Support for LaTeX -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for writing latex documents.

;;; Code:

(use-package tex
  :defer t
  :ensure auctex)

(use-package pdf-tools
  :config (pdf-tools-install))


(provide 'init-latex)
;;; init-latex.el ends here
