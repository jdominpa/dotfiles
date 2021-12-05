;;; init.el --- Personal init file -*- lexical-binding: t -*-

;; Author: Joan Domingo Pasarin
;; Maintainer: Joan Domingo Pasarin
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file sets up the essentials for loading my init org file. I
;; think literate programming has incredible value when used to write
;; Emacs configuration. It helps explain the thought process behind
;; our decisions and makes sharing the configuration easier.

;;; Code:

(require 'package)

;; Standard package repositories. Accesing a package repo over https
;; on Windows can cause problems, so we fall back to http there.
(if (eq system-type 'windows-nt)
    (progn
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
  (progn
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'vc)
(setq vc-follow-symlinks t) ; My dotfiles are managed with stow symlinks

;; "jdp-lisp" is used for all my custom elisp files; "contrib-lisp" is
;; for third-party code manually handled.
(dolist (path '("jdp-lisp" "contrib-lisp"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)

;; I create an "el" version of my Org configuration file right before
;; closing down Emacs. By doing this I make sure that the latest
;; version of the code is always loaded.
(defvar jdp-emacs-configuration-file "jdp-emacs"
  "Name of the main configuration file without extension.")

(defun jdp-emacs--expand-file-name (extension)
  "Return canonical path to `jdp-emacs-configuration-file' with EXTENSION."
  (expand-file-name
   (concat user-emacs-directory jdp-emacs-configuration-file extension)))

(defun jdp-emacs-load-config ()
  "Load main Emacs configuration, either '.el' or '.org' file."
  (let ((config-el (jdp-emacs--expand-file-name ".el"))
	    (config-org (jdp-emacs--expand-file-name ".org")))
    (if (file-exists-p config-el)
	    (load-file config-el)
      (when (file-exists-p config-org)
	    (require 'org)
	    (org-babel-load-file config-org)))))

;; Load configurations.
(jdp-emacs-load-config)

;; The following is for when Emacs gets closed.
(declare-function org-babel-tangle-file "ob-tangle")

(defun jdp-emacs-build-config ()
  "Create Elisp init file from my Org dotemacs.
Add this function to `kill-emacs-hook`, to use the newest file in
the next session. The idea is to reduce startup time by moving
this process to the end of a session rather than the beginning of
it."
  (let ((config-el (jdp-emacs--expand-file-name ".el"))
	    (config-org (jdp-emacs--expand-file-name ".org")))
    (when (file-exists-p config-org)
      (when (file-exists-p config-el)
	    (delete-file config-el))
      (require 'org)
      (org-babel-tangle-file config-org config-el)
      (byte-compile-file config-el))))

(add-hook 'kill-emacs-hook #'jdp-emacs-build-config)

(provide 'init)
;;; init.el ends here
