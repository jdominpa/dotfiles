;;; init-version-control.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for every package related to version control,
;; mainly git and github.

;;; Code:

;; Settings for every version control system
(use-package vc
  :ensure nil
  :bind (:map vc-prefix-map
              ("f" . vc-git-grep))
  :config (setq-default vc-follow-symlinks t))

;; Git packages
(use-package git-blamed)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package magit
  :hook (git-commit-mode . goto-address-mode)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq-default magit-diff-refine-hunk t)
  (when (package-installed-p 'fullframe)
    (fullframe magit-status magit-mode-quit-window)))

(use-package magit-todos
  :after magit)

;; Github packages
(use-package bug-reference-github
  :hook (prog-mode . bug-reference-prog-mode))

(use-package forge)

;; Highlight changes to versioned control files
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode)))


(provide 'init-version-control)
;;; init-version-control.el ends here
