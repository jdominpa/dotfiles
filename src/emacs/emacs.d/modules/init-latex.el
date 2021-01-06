;;; init-latex.el --- Support for LaTeX -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for writing latex documents.

;;; Code:

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . turn-on-auto-fill)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))


(provide 'init-latex)
;;; init-latex.el ends here
