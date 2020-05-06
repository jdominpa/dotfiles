;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :after evil
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map)
         :map evil-normal-state-map
         ("C-p" . projectile-find-file)))

(use-package ibuffer-projectile
  :after projectile)


(provide 'init-projectile)
;;; init-projectile.el ends here
