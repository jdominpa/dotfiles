;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

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

;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.

;;; Code:

;; Install into separate package dirs for each Emacs version to
;; prevent bytecode incompatibility.
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(defvar package-quickstart)
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box t
      use-file-dialog nil)
(setq inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
