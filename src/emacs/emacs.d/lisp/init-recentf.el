;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq-default recentf-max-saved-items 1000
                recentf-exclude '("/tmp/" "/ssh:")))


(provide 'init-recentf)
;;; init-recentf.el ends here
