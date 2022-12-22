;;; Emacs server
(use-package server
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

;;; Save cursor position
(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-forget-unreadable-files t)
  (save-place-mode t))

;;; Backups
(customize-set-variable 'backup-directory-alist
                        `(("." . ,(locate-user-emacs-file "backup/"))))
(customize-set-variable 'delete-old-versions t)
(customize-set-variable 'create-lockfiles nil)

(provide 'jdp-core-history)
