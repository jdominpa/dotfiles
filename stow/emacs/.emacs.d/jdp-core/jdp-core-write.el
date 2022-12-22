;;; Org mode
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :custom
  (org-adapt-indentation nil)
  (org-catch-invisible-edits 'show)
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;;; Improved PDF viewing
(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-continuous t)
  :config
  (pdf-loader-install))

;;; LaTeX tools
(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . turn-on-auto-fill)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(provide 'jdp-core-write)
