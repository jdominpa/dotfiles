;;; Version control framework
(use-package vc
  :custom
  ;; Always follow symlinks
  (vc-follow-symlinks t))

;;; Highlight changes on the sidebar
(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-draw-borders nil)
  (global-diff-hl-mode t))

;;; Interactive git front-end
(use-package magit
  :ensure t
  :hook (git-commit-mode . goto-address-mode)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk t))

(provide 'jdp-core-git)
