;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :after evil
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (if (package-installed-p 'counsel)
      (setq projectile-completion-system 'ivy))
  (if (file-directory-p "~/Projects")
      (setq projectile-project-search-path '("~/Projects"))))

(use-package ibuffer-projectile
  :after projectile)


(provide 'init-projectile)
;;; init-projectile.el ends here
