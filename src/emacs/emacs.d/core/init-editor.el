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

;; Newline behaviour
(bind-key "RET" 'newline-and-indent)

;; Store backup and autosave files in the tmp dir
(setq-default backup-directory-alist
              `((".*" . ,temporary-file-directory)))
(setq-default auto-save-file-name-transforms
              `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Show matching parens
(use-package show-paren-mode
  :ensure nil
  :hook (after-init . show-paren-mode))

;; Insert closing parens after opening one
(use-package electric-pair-mode
  :ensure nil
  :hook ((after-init . electric-pair-mode)
         (after-init . electric-indent-mode)))

;; Save recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq-default recentf-save-file (expand-file-name "recentf" user-emacs-directory)
                recentf-max-saved-items 1000
                recentf-exclude '("/tmp/" "/ssh:")))

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
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (if (file-directory-p "~/Projects")
      (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory)))

(use-package ibuffer-projectile
  :after projectile)

;; Settings for grep and grep-like tools
(setq-default grep-highlight-matches t
              grep-scroll-output t)

(use-package wgrep)
(use-package rg
  :if (executable-find "rg")
  :config (rg-enable-default-bindings))

;; Dired configuration
(use-package diredfl
  :config
  (diredfl-global-mode)
  (require 'dired-x))

(use-package dired
  :ensure nil
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default dired-dwim-target t)
  (setq dired-recursive-deletes 'top
        dired-recursive-copies 'top
        dired-dwim-target t))

;; Package to edit files as root user
(use-package sudo-edit)

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
  :hook (after-init . global-whitespace-cleanup-mode))

;; flyspell-mode does spell-checking on the fly as you type
(require 'ispell)
(use-package flyspell
  :ensure nil
  :if (executable-find ispell-program-name)
  :config (setq ispell-program-name "aspell"))

;; Highlight escape sequences like \n
(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))


(provide 'init-editor)
;;; init-editor.el ends here
