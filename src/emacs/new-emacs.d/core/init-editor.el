;;; init-editor.el --- Improvements of the editing settings -*- lexical-binding: t -*-
;;; Commentary:

;; Refine the core editing experience in Emacs.

;;; Code:

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil   ;; Don't use tabs to indent
              tab-width 8)           ;; but maintain correct appearance

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Newline at the end of file
(setq require-final-newline t)

;; Delete selection with a keypress
(setq-default delete-selection-mode t)

;; Move clipboard to kill ring before replacing it
(setq-default save-interprogram-paste-before-kill t)

;; Mouse yanks at point instead of at click
(setq-default mouse-yank-at-point t)

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

;; Show matching parens and disable blinking-matching-parens
(add-hook 'after-init-hook 'show-paren-mode)
(setq blink-matching-paren nil)

;; Insert closing parens after opening one
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

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

;; Dired configuration
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

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

;; ediff - don't start in another frame
(setq-default ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain)

;; Make shell script files executable automatically on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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

;; Highlight escape sequences like \n
(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

;; List of unicode characters
(use-package list-unicode-display)


(provide 'init-editor)
;;; init-editor.el ends here
