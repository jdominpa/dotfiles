;;; init.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Joan Domingo Pasarin
;;
;; Author: Joan Domingo Pasarin <http://github/jdominpa>
;; Maintainer: Joan Domingo Pasarin <jdomingopasarin@icloud.com>
;; Created: d’abril 14, 2020
;; Modified: d’abril 14, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jdominpa/dotfiles/src/emacs/init
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary: My init.el file for GNU Emacs. It starts my literate configuration
;;
;;; Code:

;; Increase startup speed
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Reset GC
(add-hook 'emacs-startup-hook
    (lambda ()
        (setq gc-cons-threshold 16777216 ; 16mb
            gc-cons-percentage 0.1)))

;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation. We'll want to update the package repository and
;; install use-package before loading the literate configuration.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
