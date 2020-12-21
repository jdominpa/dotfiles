;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;;; Inicial configuration before loading core and additional modules
(message "[Startup] Starting custom configuration...")

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
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(defun jdp/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun jdp/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'jdp/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'jdp/restore-garbage-collection-h)

;; Personal information
(setq user-full-name "Joan Domingo Pasarin"
      user-mail-address "jdomingopasarin@icloud.com")


;;; Load core stuff
(message "[Core] Loading core configuration...")

;; Configure use-package and everything related to package management
(message "[Core] Setting up package management...")
(require 'init-packages)
(require 'init-personal-packages)

;; Load system specific settings. This must be done after package management
;; to ensure we can install packages like exec-path-from-shell
(message "[Core] Loading system specific settings...")
(cond
 ((eq system-type 'darwin)
  (message "[Core] Loading macOS settings...")
  (require 'init-macos))
 ((eq system-type 'gnu/linux)
  (message "[Core] Loading Linux settings...")
  (require 'init-linux))
 ((eq system-type 'windows-nt)
  (message "[Core] Loading Windows settings...")
  (require 'init-windows))
 ((and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (message "[Core] Loading WSL settings...")
  (require 'init-wsl)))

;; Load the rest of core modules
(message "[Core] Loading remaining core modules...")
(require 'init-ui)
(require 'init-buffers)
(require 'init-editor)


;;; Load additional modules
(message "[Modules] Loading additional modules...")

;; Vim emulation
(require 'init-evil)

;; Improve narrowing search and add completion and snippets
(require 'init-ivy)
(require 'init-completion)

;; Org-mode (a legendary productivity tool that deserves its own category)
;; Org-mode helps you keep TODO lists, notes and more.
(require 'init-org)

;; Programming languages support
(message "[Modules] Loading language modules...")
(require 'init-version-control) ;; TODO: check this module
(require 'init-lsp)
(require 'init-c)
(require 'init-emacs-lisp)
(require 'init-haskell)
(require 'init-lisp)
(require 'init-latex)
(require 'init-markdown)
(require 'init-shell)


;;; Load customized settings and machine local configuration
(message "[Configuration] Loading local and customized settings...")

;; Configuration made through the customize UI will be stored in custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load local settings placed in ~/.emacs.d/init-local.el file
(require 'init-local (concat user-emacs-directory "init-local.el") t)


;;; Allow access from emacsclient
(message "[Server] Starting Emacs server...")
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
