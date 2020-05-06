;;; init-smex.el --- Use smex to improve M-x -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use smex to handle M-x
(use-package smex
  :bind ([remap execute-extended-command] . smex)
  :config
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(provide 'init-smex)
;;; init-smex.el ends here
