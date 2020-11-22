;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq gc-cons-threshold most-positive-fixnum
    gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
        (lambda () (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

;;----------------------------------------------------------------------------
;; Personal information
;;----------------------------------------------------------------------------
(setq user-full-name "Joan Domingo Pasarin"
      user-mail-address "jdomingopasarin@icloud.com")

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package diminish)

(require 'init-evil)

(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-modeline)
(require 'init-flycheck)

(require 'init-recentf)
(require 'init-smex)
(require 'init-ivy)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

;(require 'init-compile)
(require 'init-markdown)
(require 'init-org)
(require 'init-haskell)

(require 'init-paredit)
(require 'init-lisp)

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

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
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
