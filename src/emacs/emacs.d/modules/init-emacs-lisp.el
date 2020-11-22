;;; init-emacs-lisp.el --- elsip programming settings -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for Elsip programming

;;; Code:

;; Helper functions for lisp-mode
(defun jdp/headerise-elisp ()
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

(defun jdp/make-read-only (expression out-buffer-name)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))
(advice-add 'pp-display-expression :after 'jdp/make-read-only)

;; Prettify output from elisp evaluated expressions
(use-package ipretty
  :hook (after-init . ipretty-mode))

;; Make C-x C-e run 'eval-region if the region is active
(defun jdp/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key [remap eval-expression] 'pp-eval-expression)

;; Elisp mode configuration
(use-package emacs-lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . eldoc-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-x C-e" . jdp/eval-last-sexp-or-region)
              ("C-c C-e" . pp-eval-expression)))

;; Expand macros
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; Automatic byte compilation
(use-package auto-compile
  :hook ((after-init . auto-compile-on-save-mode)
         (after-init . auto-compile-on-load-mode)))


(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here
