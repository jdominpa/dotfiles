;;; init-lisp.el --- Emacs lisp settings, and common config for other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jdominpa/headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))


;; Make C-x C-e run 'eval-region if the region is active
(defun jdominpa/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(use-package lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-x C-e" . jdominpa/eval-last-sexp-or-region)
              ("C-c C-e" . pp-eval-expression)))

(use-package ipretty
  :hook (after-init . ipretty-mode))

(defun jdominpa/make-read-only (expression out-buffer-name)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))
(advice-add 'pp-display-expression :after 'jdominpa/make-read-only)


;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------
(use-package auto-compile
  :hook ((after-init . auto-compile-on-save-mode)
         (after-init . auto-compile-on-load-mode)))


;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)


(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))


;; Extras for theme editing
(use-package rainbow-mode
  :diminish
  :preface
  (defun jdominpa/enable-rainbow-mode-if-theme ()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
      (rainbow-mode)))
  :hook ((emacs-lisp-mode . jdominpa/enable-rainbow-mode-if-theme)
         (help-mode . rainbow-mode)))


(use-package flycheck-relint
  :after flycheck
  :config
  (with-eval-after-load 'elisp-mode
    (flycheck-relint-setup)))


(provide 'init-lisp)
;;; init-lisp.el ends here
