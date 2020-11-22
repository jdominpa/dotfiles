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
(require 'init-sessions)
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
(require 'init-version-control)
(require 'init-lsp)
(require 'init-c)
(require 'init-emacs-lisp)
(require 'init-haskell)
(require 'init-lisp)
(require 'init-markdown)
(require 'init-shell)


;;; Load customized settings and machine local configuration
;; Configuration made through the customize UI will be stored in custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(message "[Configuration] Loading local and customized settings...")

(require 'init-local nil t)
(when (file-exists-p custom-file)
  (load custom-file))


;;; Allow access from emacsclient
(message "[Configuration] Starting Emacs server...")

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))


(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;;; init.el ends here
