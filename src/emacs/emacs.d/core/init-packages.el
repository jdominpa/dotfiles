;;; init-packages.el --- Settings for installation of packages -*- lexical-binding: t -*-
;;; Commentary:

;; Sets up package repositories.  This module also installs use-package to use it in the
;; rest of the configuration.

;;; Code:

(require 'package)

;;;; Package setup

;; Install into separate package dirs for each Emacs version to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Standard package repositories. Accesing a package repo over https on Windows can cause
;; problems, so we fall back to http there
(if (eq system-type 'windows-nt)
    (progn
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
  (progn
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Install all packages
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-verbose t)

;; Package to remove minor modes from modeline
(use-package diminish
  :defer t)

;; Package to have certain buffers automatically fullscreen
(use-package fullframe
  :defer t
  :config (fullframe list-packages quit-window))

;; Package to update Emacs' GPG keyring
(use-package gnu-elpa-keyring-update
  :init (setq package-check-signature nil))


(provide 'init-packages)
;;; init-packages.el ends here
