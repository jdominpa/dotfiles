;;; init-shell.el --- sh-mode settings -*- lexical-binding: t -*-
;;; Commentary:

;; Basic configuration for sh-mode.

;;; Code:

;; Make shell script files executable automatically on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(provide 'init-shell)
;;; init-shell.el ends here
