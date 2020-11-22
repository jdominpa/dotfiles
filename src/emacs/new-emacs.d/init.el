;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;;; Inicial configuration before loading core and additional modules
(message "[Configuration] Starting custom configuration...")

;; Check Emacs version
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "This Emacs configuration requires version v%s or higher, but you're running v%s" minver emacs-version)))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Add core and modules directories to load-path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Adjust garbage collection thresholds during startup, and thereafter
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                      gc-cons-percentage 0.1)))

;; Personal information
(setq user-full-name "Joan Domingo Pasarin"
      user-mail-address "jdomingopasarin@icloud.com")


;;; Load core stuff
(message "[Configuration] Loading core modules...")

(require 'init-packages)
(require 'init-personal-packages)
(require 'init-ui)
(require 'init-buffers)
(require 'init-editor)


;;; Load system specific settings
(message "[Configuration] Loading system specific settings...")

(cond
 ((eq system-type 'darwin)
  (message "[Configuration] Loading macOS settings...")
  (require 'init-macos))
 ((eq system-type 'gnu/linux)
  (message "[Configuration] Loading Linux settings...")
  (require 'init-linux))
 ((eq system-type 'windows-nt)
  (message "[Configuration] Loading Windows settings...")
  (require 'init-windows))
 ((and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (message "[Configuration] Loading WSL settings...")
  (require 'init-wsl)))


;;; Load additional modules
(message "[Configuration] Loading additional modules...")

;; Vim emulation
(require 'init-evil)

;; Improve narrowing, search and completion
(require 'init-ivy)
(require 'init-company)

;; Org-mode (a legendary productivity tool that deserves its own category)
;; Org-mode helps you keep TODO lists, notes and more.
(require 'init-org)

;; Programming languages support
(require 'init-lsp)
(require 'init-c) ;; TODO
(require 'init-haskell)
(require 'init-emacs-lisp)
(require 'init-lisp)
(require 'init-shell)


(require 'init-gui-frames)
(require 'init-grep)
(require 'init-flycheck)
;;(require 'init-smex)
(require 'init-sessions)
(require 'init-editing-utils)
(require 'init-vc)
(require 'init-git)
(require 'init-github)
(require 'init-markdown)
(require 'init-paredit)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

;; Extra packages which don't require any configuration
(when *is-a-mac*
  (use-package osx-location))
(use-package sudo-edit)

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Store customize UI changes in custom.el file and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
