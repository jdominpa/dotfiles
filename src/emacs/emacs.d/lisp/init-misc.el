;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'jdp/set-mode-for-new-scripts)

(defun jdp/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))


(provide 'init-misc)
;;; init-misc.el ends here
