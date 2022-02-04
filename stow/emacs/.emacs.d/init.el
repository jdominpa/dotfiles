;;; init.el --- Personal init file -*- lexical-binding: t -*-

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
            (garbage-collect)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; "jdp-lisp" is used for all my custom elisp files
;; "contrib-lisp" is for third-party code manually handled.
(dolist (path '("jdp-lisp" "contrib-lisp"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(fset 'yes-or-no-p 'y-or-n-p)
(setq disabled-command-function nil)

;;; General settings

(use-package jdp-simple
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("M-`" . nil)
         ("C-h K" . describe-keymap)
         ;; Commands for lines
         ("C-<return>" . jdp-simple-newlines-above-or-below)
         ("M-SPC" . cycle-spacing)
         ("M-o" . delete-blank-lines)
         ;; Commands for text manipulation
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)        ; "lower" case
         ("M-u" . upcase-dwim)
         ;; Commands for marking objects
         ("M-@" . jdp-simple-mark-word)
         ("C-M-SPC" . jdp-simple-mark-construct-dwim)))

;; Put custom configuration in a separate file
(customize-set-variable 'custom-file
                        (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package modus-themes
  :ensure t
  :bind ([f5] . modus-themes-toggle)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)
  (modus-themes-mode-line '(borderless))
  :init
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(use-package jdp-fonts
  :demand t
  :bind ("C-c f" . jdp-fonts-set-fonts-dwim)
  :custom
  (jdp-fonts-typeface-sets-alist
   '((desktop 140 "Iosevka" normal "Inter" normal)
     (reader 170 "Iosevka" light "FiraGO" normal)
     (presentation 180 "Fira Code" light "FiraGO" normal)))
  (jdp-fonts-monospaced-list
   '("Hack" "DejaVu Sans Mono" "Iosevka" "Source Code Pro"
     "Ubuntu Mono" "Fantasque Sans Mono" "Fira Code" "Monoid"))
  (jdp-fonts-heights-list (number-sequence 100 200 5))
  (jdp-fonts-line-spacing-alist '(("Ubuntu Mono" . 2)))
  (jdp-fonts-laptop-desktop-keys-list '(laptop desktop))
  (jdp-fonts-max-small-resolution-width 1440)
  (jdp-fonts-bold-weight-alist '(("Iosevka" . semibold)
                                 ("Fira Code" . semibold)
                                 ("Source Code Pro" . semibold)))
  ;; This is defined in Emacs' C code, though I feel this is a good
  ;; place to put it.
  (x-underline-at-descent-line t)
  :hook ((jdp-fonts-set-typeface . jdp-fonts-line-spacing)
         (jdp-fonts-set-typeface . jdp-fonts-bold-face)
         ;; See theme section for the following hook
         (modus-themes-after-load-theme . jdp-fonts-bold-face))
  :config
  ;; And this just sets the right font depending on whether my laptop is
  ;; connected to an external monitor or not.
  (jdp-fonts-fonts-per-monitor))

(customize-set-variable 'bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(use-package so-long
  :config
  (global-so-long-mode))

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-timer))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :ensure t
  :custom
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.25)
  :config
  (which-key-mode))

(use-package async
  :ensure t
  :defer t)

;;; Interface settings

(use-package goto-last-change
  :ensure t
  :bind ("C-z" . goto-last-change))

(customize-set-variable 'line-number-mode t)
(customize-set-variable 'column-number-mode t)
(customize-set-variable 'size-indication-mode t)

(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

(customize-set-variable 'mode-line-format
                        '("%e"
                          mode-line-front-space
                          mode-line-mule-info
                          mode-line-client
                          mode-line-modified
                          mode-line-remote
                          mode-line-frame-identification
                          mode-line-buffer-identification
                          "  "
                          mode-line-position
                          " "
                          mode-line-modes
                          "  "
                          (vc-mode vc-mode)
                          "  "
                          mode-line-misc-info
                          mode-line-end-spaces))

(use-package minions
  :ensure t
  :custom
  (minions-mode-line-lighter ";")
  (minions-direct (list 'defining-kbd-macro
                        'flymake-mode))
  :config
  (minions-mode))

(use-package battery
  :custom
  (battery-mode-line-format "[%b%p%%] ")
  (battery-load-low 20)
  (battery-load-critical 10)
  :config
  (display-battery-mode))

(use-package time
  :custom
  (display-time-format "%d-%m-%Y %H:%M")
  (display-time-interval 60)
  (display-time-default-load-average nil)
  :config
  (display-time-mode))

(use-package jdp-whitespace
  :bind ([f6] . jdp-whitespace-space-toggle))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))

(blink-cursor-mode -1)
(cl-flet ((flash-mode-line ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil #'invert-face 'mode-line)))
  (customize-set-variable 'ring-bell-function #'flash-mode-line))

(use-package mouse
  :custom
  (mouse-wheel-scroll-amount
   '(1
     ((shift) . 5)
     ((meta) . 0.5)
     ((control) . text-scale)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t))

(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-conservatively 1000)
(customize-set-variable 'scroll-preserve-screen-position 'always)

(use-package delsel
  :config
  (delete-selection-mode))

(use-package autorevert
  :config
  (auto-revert-mode))

(customize-set-variable 'save-interprogram-paste-before-kill t)

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pules t))

(customize-set-variable 'mode-require-final-newline 'visit-save)

;;; Completion framework and extras

(use-package orderless
  :ensure t
  :demand t
  :bind (:map minibuffer-local-completion-map
              ("?" . nil)
              ("SPC" . nil))
  :config
  (defmacro dispatch: (regexp style)
    (cl-flet ((symcat (a b) (intern (concat a (symbol-name b)))))
      `(defun ,(symcat "dispatch:" style) (pattern _index _total)
         (when (string-match ,regexp pattern)
           (cons ',(symcat "orderless-" style) (match-string 1 pattern))))))
  (cl-flet ((pre/post (str) (format "^%s\\(.*\\)$\\|^\\(?1:.*\\)%s$" str str)))
    (dispatch: (pre/post "=") literal)
    (dispatch: (pre/post "`") regexp)
    (dispatch: (pre/post (if (or minibuffer-completing-file-name
                                 (derived-mode-p 'eshell-mode))
                             "%"
                           "[%.]"))
               initialism))
  (dispatch: "^{\\(.*\\)}$" flex)
  (dispatch: "^\\([^][^\\+*]*[./-][^][\\+*$]*\\)$" prefixes)
  (dispatch: "^!\\(.+\\)$" without-literal)
  :custom
  (orderless-matching-styles 'orderless-regexp)
  (orderless-style-dispatchers '(dispatch:literal
                                 dispatch:regexp
                                 dispatch:initialism
                                 dispatch:flex
                                 dispatch:prefixes
                                 dispatch:without-literal)))

(use-package minibuffer
  :custom
  (completion-styles '(orderless))
  (completion-cycle-threshold nil)
  (completion-show-help nil)
  (completion-auto-help t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (read-answer-short t)
  (resize-mini-windows t)
  (minibuffer-eldef-shorten-default t)
  :config
  (setq completion-category-defaults nil
        completion-ignore-case t)
  (file-name-shadow-mode)
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (vertico-reverse-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package savehist
  :custom
  (savehist-file (locate-user-emacs-file "savehist"))
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode))

(use-package consult
  :ensure t
  :bind (;; C-x bindings
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x M-m" . consult-minor-mode-menu)
         ("C-x M-k" . consult-kmacro)
         ;; M-g bindings
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g M-m" . consult-mark)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         :map isearch-mode-map
         ("M-h" . consult-isearch-history)
         :map consult-narrow-map
         ("?" . consult-narrow-help))
  :custom
  (register-preview-delay 0.8)
  (consult-preview-key nil) ; disable previews
  (consult-narrow-key ">")
  :config
  (setq register-preview-function #'consult-register-format))

(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip"))
  :config
  (recentf-mode))

(use-package corfu
  :ensure t
  :config
  (corfu-global-mode))

(use-package cape
  :ensure t
  :after corfu
  :init
  (dolist (backend '(cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
         ("M-d" . corfu-doc-toggle)
         ("M-n" . corfu-doc-scroll-up)
         ("M-p" . corfu-doc-scroll-down)))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;; Built-in search commands

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
  (lazy-count-suffix-format " (%s/%s)")
  (isearch-yank-on-move 'shift)
  :config
  (setq isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil))

(use-package replace
  :hook (occur-mode . hl-line-mode)
  :bind (("M-s M-o" . multi-occur)
         :map occur-mode-map
         ("t" . toggle-truncate-lines)))

(use-package grep
  :commands (grep grep-find grep-find-toggle-abbreviation))

(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

;;; Directory management

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
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

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)))

;; Part of the `async' package.
(use-package dired-async
  :hook (dired-mode . dired-async-mode))

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
  :demand t
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
  :config
  (global-diff-hl-mode))

(use-package magit
  :ensure t
  :hook (git-commit-mode . goto-address-mode)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk t))

(use-package magit-todos
  :ensure t
  :hook (magit-status-mode . magit-todos-mode))

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

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun jdp-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . jdp-lsp-mode-setup-completion))
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references)))

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ("M-g e" . consult-lsp-diagnostics)
              ("M-g M-e" . consult-lsp-diagnostics)
              ("M-g s" . consult-lsp-symbols)
              ("M-g M-s" . consult-lsp-symbols)))

(use-package yasnippet
  :ensure t
  :bind (:map yas-keymap
              ("<tab>" . nil)
              ("TAB" . nil)
              ("S-<tab>" . nil)
              ("<backtab>" . nil)
              ([remap scroll-up-command] . yas-next-field)
              ([remap scroll-down-command] . yas-prev-field))
  :config
  (yas-global-mode))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package cc-mode
  :hook (c-mode-common . lsp-deferred)
  :bind (:map c-mode-base-map
              ("TAB" . indent-for-tab-command))
  :custom
  (c-default-style "k&r")
  (c-basic-offset 4))

(use-package rustic
  :ensure t
  :custom
  (rustic-format-trigger 'on-save))

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

(customize-set-variable 'python-shell-interpreter "python3")

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package electric
  :config
  (electric-pair-mode)
  (electric-indent-mode))

(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  :config
  (show-paren-mode))

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

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-function
   #'flycheck-display-error-messages-unless-error-list))

(use-package consult-flycheck
  :ensure t
  :after (consult flycheck)
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package eldoc
  :config
  (global-eldoc-mode))

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
  :config
  (save-place-mode))

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
