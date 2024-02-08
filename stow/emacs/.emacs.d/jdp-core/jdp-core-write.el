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
  :defer 3
  :bind (:map pdf-view-mode-map
              ("d" . pdf-view-midnight-minor-mode))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-continuous t)
  (pdf-view-midnight-colors '("#ffffff" . "#000000"))
  :config
  (pdf-loader-install))

;;; LaTeX tools
(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . prettify-symbols-mode))
  :config
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex))

;;; Spellchecking
(use-package jinx
  :ensure t
  :hook ((text-mode . jinx-mode)
         (LaTeX-mode . jinx-mode))
  :bind (:map jinx-mode-map
              ("M-$" . jinx-correct)
              ("C-M-$" . jinx-languages))
  :custom (jinx-languages "en es"))

(provide 'jdp-core-write)
