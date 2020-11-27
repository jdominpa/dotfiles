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

;; Package to diminish minor modes
(use-package diminish)

;; Package to have certain buffers automatically fullscreen
(use-package fullframe
  :config
  (fullframe list-packages quit-window))

;; Package to update Emacs' GPG keyring
(use-package gnu-elpa-keyring-update
  :init (let ((package-check-signature nil))))

;; Improve UI in package-menu-mode by widening certain columns
(defun jdp/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun jdp/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (jdp/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (jdp/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'jdp/maybe-widen-package-menu-columns)


(provide 'init-packages)
;;; init-packages.el ends here
