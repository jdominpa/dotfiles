;;; init-emacs-lisp.el --- elsip programming settings -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for elisp programming.

;;; Code:

;; Prettify output from elisp evaluated expressions
(use-package ipretty
  :config
  (ipretty-mode))

(use-package emacs-lisp-mode
  :ensure nil
  :preface
  (defun jdp/make-read-only (expression out-buffer-name)
    "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
    (when (get-buffer out-buffer-name)
      (with-current-buffer out-buffer-name
        (view-mode 1))))
  (advice-add 'pp-display-expression :after 'jdp/make-read-only)

  ;; Make C-x C-e run 'eval-region if the region is active
  (defun jdp/eval-last-sexp-or-region (prefix)
    "Eval region from BEG to END if active, otherwise the last sexp."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (eval-region (min (point) (mark)) (max (point) (mark)))
      (pp-eval-last-sexp prefix)))

  :bind (([remap eval-expression] . pp-eval-expression)
         :map emacs-lisp-mode-map
         ("C-x C-e" . jdp/eval-last-sexp-or-region)))

;; Expand macros
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; Automatic byte compilation
(use-package auto-compile
  :config
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode))


(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here
