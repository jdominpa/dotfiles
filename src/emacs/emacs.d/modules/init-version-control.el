;;; init-version-control.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for every package related to version control,
;; mainly git and github.

;;; Code:

;; Settings for every version control system
(use-package vc
  :ensure nil
  :init (setq-default vc-follow-symlinks t)
  :bind (:map vc-prefix-map
              ("f" . vc-git-grep)))

;; Git packages
(use-package gitignore-mode :defer t)
(use-package gitconfig-mode :defer t)
(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package magit
  :hook (git-commit-mode . goto-address-mode)
  :bind ("C-x g" . magit-status)
  :config
  (setq-default magit-diff-refine-hunk t)
  (with-eval-after-load 'fullframe
    (fullframe magit-status magit-mode-quit-window)))

(use-package magit-todos
  :after magit
  :hook (magit-status-mode . magit-todos-mode))

;; Github packages
(use-package forge
  :after magit)

;; Highlight changes to versioned control files
(use-package diff-hl
  :demand t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode))


(provide 'init-version-control)
;;; init-version-control.el ends here
