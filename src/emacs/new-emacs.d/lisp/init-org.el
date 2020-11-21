;;; init-org.el --- Org mode settings and configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure org-plus-contrib
  :init
  (defun jdp/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    (auto-fill-mode 1))
  (defun jdp/org-font-setup ()
    "Set faces for 'org-mode' heading levels."
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.0)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :font "monospace" :weight 'regular :height (cdr face))))
  :hook ((org-mode . jdp/org-mode-setup)
         (org-mode . jdp/org-font-setup))
  :config
  (setq org-ellipsis " ▾")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp\n"))

  (use-package visual-fill-column
    :init
    (defun jdp/org-mode-visual-fill ()
      (setq visual-fill-column-width 100
            visual-fill-column-center-text t)
      (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))
    :hook (org-mode . jdp/org-mode-visual-fill)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(provide 'init-org)
;;; init-org.el ends here
