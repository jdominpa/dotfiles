;;; init-windows.el --- Windows specific settings -*- lexical-binding: t -*-
;;; Commentary:

;; Custom settings for Windows systems.

;;; Code:

;; Teach Emacs how to interpret various modifier keys
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; Git setup (assuming you've installed Git for Windows)
(when (file-exists-p "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH"))))

;; needed for arc-mode (it allows you to open archives in Emacs)
(if (file-exists-p "C:/Program Files/7-Zip")
    (add-to-list 'exec-path "C:/Program Files/7-Zip")
  (message "7-Zip not found. It's a good idea to install it."))


(provide 'init-windows)
;;; init-windows.el ends here
