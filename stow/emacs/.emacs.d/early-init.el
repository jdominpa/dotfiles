;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Install into separate package dirs for each Emacs version to
;; prevent bytecode incompatibility.
(customize-set-variable 'package-user-dir
                        (expand-file-name
                         (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory))

;; Initialise installed packages
(customize-set-variable 'package-enable-at-startup t)

;; Do not resize the frame at this early stage.
(customize-set-variable 'frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(customize-set-variable 'use-dialog-box t)
(customize-set-variable 'use-file-dialog nil)
(customize-set-variable 'inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
