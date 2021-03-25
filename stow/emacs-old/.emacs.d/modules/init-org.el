;;; init-org.el --- Org mode settings and configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Settings for everything related to org-mode.  This includes org-mode settings
;; and setup for org-agenda.

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :config
  (setq org-ellipsis " ▾")
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp\n")))


(provide 'init-org)
;;; init-org.el ends here
