;;; Emacs native LSP client
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l R" . eglot-reconnect)
              ("C-c l s" . eglot-shutdown)
              ("C-c l S" . eglot-shutdown-all)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l i" . imenu)
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . flymake-show-buffer-diagnostics)
              ("C-c l D" . flymake-show-project-diagnostics))
  :custom (eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-g s" . consult-eglot-symbols)
              ("M-g M-s" . consult-eglot-symbols)))

;;; C/C++ (cc-mode)
(use-package cc-mode
  :bind (:map c-mode-base-map
              ("TAB" . nil))
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

;;; Expand lisp macros
(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;;; Auto fill comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)))

;;; Highlight comment keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;;; Color delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Configure 'electric' behaviour
(use-package electric
  :custom
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-pair-mode t)
  (electric-indent-mode t))

;;; Parentheses
(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-context-when-offscreen 'child-frame)
  (show-paren-mode t))

;;; Tabs and indentation
(customize-set-variable 'tab-width 4)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'tab-always-indent 'complete)

;;; Flymake
(use-package flymake
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-wrap-around nil))

;;; Eldoc
(use-package eldoc
  :custom
  (global-eldoc-mode t))

;;; Handle performance for long lines
(use-package so-long
  :custom
  (global-so-long-mode t))

(provide 'jdp-core-langs)
