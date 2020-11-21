;;; init-linux.el --- Linux specific settings -*- lexical-binding: t -*-
;;; Commentary:

;; Custom settings for GNU Linux systems.

;;; Code:

;; On Linux Emacs doesn't use the shell PATH if it's not started from
;; the shell. This fixes that problem
(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))


(provide 'init-linux)
;;; init-linux.el ends here
