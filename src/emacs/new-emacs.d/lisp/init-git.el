;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(use-package git-blamed)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package magit
  :hook ((git-commit-mode . goto-address-mode)
         (git-commit-mode . evil-insert-state))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq-default magit-diff-refine-hunk t)
  (when (package-installed-p 'fullframe)
    (fullframe magit-status magit-mode-quit-window)))

(use-package magit-todos
  :after magit)



(when *is-a-mac*
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(use-package vc
  :ensure nil
  :bind (:map vc-prefix-map
              ("f" . vc-git-grep)))


(provide 'init-git)
;;; init-git.el ends here
