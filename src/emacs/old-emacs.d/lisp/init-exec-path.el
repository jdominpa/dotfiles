;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
