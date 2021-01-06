;;; init-editor.el --- Improvements of the editing settings -*- lexical-binding: t -*-
;;; Commentary:

;; Refine the core editing experience in Emacs.

;;; Code:

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil                    ;; Don't use tabs to indent
              tab-width 4                             ;; but maintain correct appearance
              tab-always-indent 'complete             ;; Smart tab behavior - indent or complete
              require-final-newline t                 ;; Newline at the end of file
              delete-selection-mode t                 ;; Delete selection with a keypress
              save-interprogram-paste-before-kill t   ;; Move clipboard to kill ring before replacing it
              mouse-yank-at-point t)                  ;; Mouse yanks at point instead of at click

;; Store backup and autosave files in the tmp dir
(setq-default backup-directory-alist
              `((".*" . ,temporary-file-directory)))
(setq-default auto-save-file-name-transforms
              `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :ensure nil
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (global-auto-revert-mode))

;; Show matching parens
(use-package paren
  :ensure nil
  :config
  (show-paren-mode))

;; Insert closing parens after opening one
(use-package electric
  :ensure nil
  :config
  (electric-pair-mode)
  (electric-indent-mode))

;; Save recent files
(use-package recentf
  :ensure nil
  :config
  (setq-default recentf-save-file (expand-file-name "recentf" user-emacs-directory)
                recentf-exclude '("/tmp/" "/ssh:"))
  (recentf-mode))

;; Enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Bookmarks
(setq-default bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))

;; Projectile to manage projects
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (if (file-directory-p "~/Projects")
      (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory))
  (projectile-mode))

;; Settings for grep and grep-like tools
(setq-default grep-highlight-matches t
              grep-scroll-output t)

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

(use-package rg
  :if (executable-find "rg")
  ;; TODO: why is ensure-system-package not installed
  ;; :ensure-system-package (rg . ripgrep)
  :bind ("C-c s" . rg-menu))

;; Dired configuration
(use-package dired
  :ensure nil
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default dired-dwim-target t)
  (setq dired-recursive-deletes 'top
        dired-recursive-copies 'top
        dired-dwim-target t))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode)
  ;;(require 'dired-x)    TODO: currently not in use
  )

;; Package to edit files as root user
(use-package sudo-edit
  :defer t)

;; ediff - don't start in another frame
(setq-default ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain)

;; Whitespace-mode configuration
(setq-default show-trailing-whitespace nil)

(defun jdp/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'jdp/show-trailing-whitespace))

(use-package whitespace-cleanup-mode
  :diminish
  :config
  (global-whitespace-cleanup-mode))

;; flyspell-mode does spell-checking on the fly as you type
(use-package flyspell
  :ensure nil
  :preface (require 'ispell)
  :if (executable-find ispell-program-name) ; TODO: check ensure-system-package
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; Highlight escape sequences like \n
(use-package highlight-escape-sequences
  :config
  (hes-mode))


(provide 'init-editor)
;;; init-editor.el ends here
