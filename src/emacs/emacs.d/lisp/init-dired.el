;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :config
  (diredfl-global-mode)
  (require 'dired-x))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

;; (after-load 'dired
;;   (setq dired-recursive-deletes 'top)
;;   (define-key dired-mode-map [mouse-2] 'dired-find-file)
;;   (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))


(provide 'init-dired)
;;; init-dired.el ends here
