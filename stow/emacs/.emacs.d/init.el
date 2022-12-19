;;; init.el --- Personal init file -*- lexical-binding: t -*-

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
            (garbage-collect)))

;; Some basic settings
(fset 'yes-or-no-p 'y-or-n-p)
(setq disabled-command-function nil)
(customize-set-variable 'blink-cursor-mode nil)

;; Put custom configuration in a separate file
(customize-set-variable 'custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;; Packages and modules

(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(customize-set-variable 'package-archive-priority '(("elpa" . 2)
                                                    ("nongnu" . 1)))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; "jdp-core" is for all my emacs configuration modules
;; "jdp-lisp" is used for all my custom elisp files
(dolist (path '("jdp-core" "jdp-lisp"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(require 'jdp-core-emacs)
(require 'jdp-core-theme)
(require 'jdp-core-font)
(require 'jdp-core-modeline)

;;; TODO: move this section somewhere else
(use-package so-long
  :custom (global-so-long-mode t))

(customize-set-variable 'save-interprogram-paste-before-kill t)
(customize-set-variable 'mode-require-final-newline 'visit-save)

;;; Completion

(use-package minibuffer
  :custom
  (completion-styles '(basic orderless))
  (completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                   (project-file (styles . (basic substring partial-completion orderless)))
                                   (kill-ring (styles . (basic substring orderless)))))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (read-answer-short t)
  (resize-mini-windows t)
  (minibuffer-eldef-shorten-default t)
  (file-name-shadow-mode t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  :config
  (setq completion-category-defaults nil
        completion-ignore-case t))

(use-package savehist
  :custom
  (savehist-file (locate-user-emacs-file "savehist"))
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-mode t))

(use-package orderless
  :ensure t
  :demand t
  :bind (:map minibuffer-local-completion-map
              ("?" . nil)
              ("SPC" . nil))
  :custom
  (orderless-matching-styles '(orderless-prefixes
                               orderless-flex
                               orderless-regexp))
  (orderless-style-dispatchers '(orderless-literal-dispatcher
                                 orderless-flex-dispatcher
                                 orderless-regexp-dispatcher))
  :config
  (defun orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  (defun orderless-flex-dispatcher (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun orderless-regexp-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-regexp . ,(substring pattern 0 -1)))))

(use-package marginalia
  :ensure t
  :custom (marginalia-mode t))

(use-package vertico
  :ensure t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-mode t)
  (vertico-reverse-mode t))

(use-package consult
  :ensure t
  :bind (;; C-x bindings
         ("C-x b" . consult-buffer)
         ("C-x M-:" . consult-complex-command)
         ("C-x M-m" . consult-minor-mode-menu)
         ("C-x M-k" . consult-kmacro)
         ;; M-g bindings
         ([remap goto-line] . consult-goto-line)
         ("M-s M-f" . consult-find)
         ("M-s M-g" . consult-grep)
         ("M-s M-i" . consult-imenu)
         ("M-s M-l" . consult-line)
         ("M-s M-m" . consult-mark)
         ("M-s M-s" . consult-outline)
         ("M-s M-y" . consult-yank-pop)
         :map consult-narrow-map
         ("?" . consult-narrow-help))
  :custom
  (register-preview-delay 0.8)
  (consult-preview-key nil)
  (consult-narrow-key ">")
  :config
  (setq register-preview-function #'consult-register-format))

(use-package recentf
  :custom
  (recentf-save-file (locate-user-emacs-file "recentf"))
  (recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip"))
  (recentf-mode t))

(use-package corfu
  :ensure t
  :custom (global-corfu-mode t))

(use-package cape
  :ensure t
  :after corfu
  :init
  (dolist (backend '(cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Search commands

(use-package isearch
  :bind (:map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("M-/" . isearch-complete))
  :custom
  (search-whitespace-regexp ".*?")
  (search-highlight t)
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]")
  (isearch-yank-on-move 'shift)
  :config
  (setq isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil))

;;; Interface settings

(use-package jdp-whitespace
  :bind ([f6] . jdp-whitespace-space-toggle))

(use-package whitespace-cleanup-mode
  :ensure t
  :custom (global-whitespace-cleanup-mode t))

;;; Directory management

(use-package dired
  :hook (dired-mode . hl-line-mode)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-AXhlv --group-directories-first")
  (dired-dwim-target t))

(use-package dired-aux
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t))

;;; Buffer management

(customize-set-variable 'uniquify-buffer-name-style 'forward)
(customize-set-variable 'uniquify-strip-common-suffix t)
(customize-set-variable 'uniquify-after-kill-buffer-p t)

(use-package ibuffer
  :hook ((ibuffer . ibuffer-set-up-preferred-filters)
         (ibuffer . hl-line-mode))
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-movement-cycle nil)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-saved-filter-groups nil)
  (ibuffer-old-time 48))

;;; Window management

(use-package window
  :bind (("C-x _" . balance-windows)
         ("C-x -" . fit-window-to-buffer)
         ("C-x +" . balance-windows-area)
         ("C-x }" . enlarge-window)
         ("C-x {" . shrink-window)
         ("C-x >" . enlarge-window-horizontally) ; override `scroll-right'
         ("C-x <" . shrink-window-horizontally)  ; override `scroll-left'
         :map resize-window-repeat-map
         ("}" . enlarge-window)
         ("{" . shrink-window)
         (">" . enlarge-window-horizontally)
         ("<" . shrink-window-horizontally))
  :custom
  (window-combination-resize t)
  (window-sides-vertical nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `(("\\*\\(Compile-Log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.35)
      (side . bottom)
      (slot . -1)))))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;;; Applications and utilities

(use-package vc
  :custom
  ;; Always follow symlinks
  (vc-follow-symlinks t))

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-draw-borders nil)
  (global-diff-hl-mode t))

(use-package magit
  :ensure t
  :hook (git-commit-mode . goto-address-mode)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk t))

(use-package eshell
  :custom
  (eshell-cd-on-directory t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-directory-name
   (expand-file-name "eshell" user-emacs-directory))
  (eshell-aliases-file
   (expand-file-name "eshell/alias" user-emacs-directory))
  (password-cache t)
  (password-cache-expiry 600))

(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :custom
  (org-adapt-indentation nil)
  (org-catch-invisible-edits 'show)
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-height)
  (pdf-view-continuous t)
  :config
  (pdf-loader-install))

;;; Settings for programming languages

(use-package eglot
  :ensure t
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

(use-package yasnippet
  :ensure t
  :bind (:map yas-keymap
              ("C-v" . yas-next-field)
              ("M-v" . yas-prev-field))
  :custom (yas-global-mode t))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package cc-mode
  :hook (c-mode-common . eglot-ensure)
  :bind (:map c-mode-base-map
              ("TAB" . nil))
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode))

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . turn-on-auto-fill)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package electric
  :custom
  (electric-pair-mode t)
  (electric-indent-mode t))

(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-mode t))

(customize-set-variable 'tab-width 4)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'tab-always-indent 'complete)

(use-package flymake
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-wrap-around nil))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode t))

;;; Emacs server and history

(use-package server
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-forget-unreadable-files t)
  (save-place-mode t))

(customize-set-variable 'backup-directory-alist
                        `(("." . ,(expand-file-name "backup/" user-emacs-directory))))

;;; System settings

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control))

(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (customize-set-variable 'browse-url-generic-program cmd-exe)
      (customize-set-variable 'browse-url-generic-args cmd-args)
      (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
      (setq search-web-default-browser 'browse-url-generic))))

(provide 'init)
;;; init.el ends here
