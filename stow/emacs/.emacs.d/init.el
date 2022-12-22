;;; init.el --- Personal init file -*- lexical-binding: t -*-

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
            (garbage-collect)))

;; Some basic settings
(setq disabled-command-function nil)
(customize-set-variable 'use-short-answers t)
(customize-set-variable 'initial-buffer-choice t)          ; always start with *scratch* buffer
(customize-set-variable 'blink-cursor-mode nil)

;; Put custom configuration in a separate file
(customize-set-variable 'custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;; Packages and modules

(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(customize-set-variable 'package-archive-priority '(("elpa" . 2)
                                                    ("nongnu" . 1)))

;; "jdp-core" is for all my emacs configuration modules
;; "jdp-lisp" is used for all my custom elisp files
(dolist (path '("jdp-core" "jdp-lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'jdp-core-emacs)
(require 'jdp-core-theme)
(require 'jdp-core-font)
(require 'jdp-core-modeline)
(require 'jdp-core-completion)
(require 'jdp-core-search)
(require 'jdp-core-dired)
(require 'jdp-core-window)
(require 'jdp-core-git)
(require 'jdp-core-shell)
(require 'jdp-core-write)
(require 'jdp-core-langs)
(require 'jdp-core-history)


;;; System settings

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control))

(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (customize-set-variable 'browse-url-generic-program cmd-exe)
      (customize-set-variable 'browse-url-generic-args cmd-args)
      (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
      (setq search-web-default-browser 'browse-url-generic))))

(provide 'init)
;;; init.el ends here
