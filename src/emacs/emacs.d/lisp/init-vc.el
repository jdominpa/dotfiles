;;; init-vc.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Most version control packages are configured separately: see
;; init-git.el, for example.

;;; Code:

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (after-load 'dired
              (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))


(provide 'init-vc)
;;; init-vc.el ends here
