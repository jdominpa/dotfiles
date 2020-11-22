;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :config
  (diredfl-global-mode)
  (require 'dired-x))

(use-package dired
  :ensure nil
  :config
  (setq-default dired-dwim-target t)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))


(provide 'init-dired)
;;; init-dired.el ends here
