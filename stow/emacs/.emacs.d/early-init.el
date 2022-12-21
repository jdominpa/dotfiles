;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Initialise installed packages
(customize-set-variable 'package-enable-at-startup t)

;; Resize the frame pixelwise
(customize-set-variable 'frame-resize-pixelwise t)

;; Do not resize the frame at this early stage.
(customize-set-variable 'frame-inhibit-implied-resize t)

;; Disable GUI elements
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'use-dialog-box t)
(customize-set-variable 'use-file-dialog nil)
(customize-set-variable 'inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
