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

;;; TODO: move this section somewhere else
(use-package so-long
  :custom (global-so-long-mode t))

(customize-set-variable 'save-interprogram-paste-before-kill t)
(customize-set-variable 'mode-require-final-newline 'visit-save)

;;; Applications and utilities

(use-package eshell
  :custom
  (eshell-cd-on-directory t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-directory-name (locate-user-emacs-file "eshell"))
  (eshell-aliases-file (locate-user-emacs-file "eshell/alias"))
  (password-cache t)
  (password-cache-expiry 600))

(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :custom
  (org-adapt-indentation nil)
  (org-catch-invisible-edits 'show)
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-height)
  (pdf-view-continuous t)
  :config
  (pdf-loader-install))

;;; Settings for programming languages

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l R" . eglot-reconnect)
              ("C-c l s" . eglot-shutdown)
              ("C-c l S" . eglot-shutdown-all)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l i" . imenu)
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . flymake-show-buffer-diagnostics)
              ("C-c l D" . flymake-show-project-diagnostics))
  :custom (eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-g s" . consult-eglot-symbols)
              ("M-g M-s" . consult-eglot-symbols)))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package cc-mode
  :bind (:map c-mode-base-map
              ("TAB" . nil))
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode))

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . turn-on-auto-fill)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package electric
  :custom
  (electric-pair-mode t)
  (electric-indent-mode t))

(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-mode t))

(customize-set-variable 'tab-width 4)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'tab-always-indent 'complete)

(use-package flymake
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-wrap-around nil))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode t))

;;; Emacs server and history

(use-package server
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-forget-unreadable-files t)
  (save-place-mode t))

(customize-set-variable 'backup-directory-alist
                        `(("." . ,(locate-user-emacs-file "backup/"))))

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
