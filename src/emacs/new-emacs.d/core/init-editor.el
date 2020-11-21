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

;; List of unicode characters
(use-package list-unicode-display)


(provide 'init-editor)
;;; init-editor.el ends here
