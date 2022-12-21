;;; Orderless completion style
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
  (orderless-style-dispatchers '(jdp-orderless-literal-dispatcher
                                 jdp-orderless-flex-dispatcher
                                 jdp-orderless-regexp-dispatcher)))

(use-package jdp-orderless)

;;; Completion annotations
(use-package marginalia
  :ensure t
  :custom (marginalia-mode t))

;;; Minibuffer configurations and Vertico
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

(use-package vertico
  :ensure t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-mode t)
  (vertico-reverse-mode t))

;;; Enhanced minibuffer commands
(use-package consult
  :ensure t
  :bind (;; C-x bindings
         ("C-x M-:" . consult-complex-command)
         ("C-x M-m" . consult-minor-mode-menu)
         ("C-x M-k" . consult-kmacro)
         ;; M-g bindings
         ([remap goto-line] . consult-goto-line)
         ("M-s M-b" . consult-buffer)
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

;;; Switch to directories
(use-package consult-dir
  :ensure t
  :bind ("C-x C-d" . consult-dir)
  :custom
  (consult-dir-shadow-filenames nil))

;;; Completion for recent files and directories
(use-package recentf
  :custom
  (recentf-save-file (locate-user-emacs-file "recentf"))
  (recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip"))
  (recentf-mode t))

;;; In-buffer completion
(use-package corfu
  :ensure t
  :custom (global-corfu-mode t))

;;; Completion backends
(use-package cape
  :ensure t
  :after corfu
  :init
  (dolist (backend '(cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

;;; Completion popup icons
(use-package kind-icon
  :ensure t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Snippets
(use-package yasnippet
  :ensure t
  :bind (:map yas-keymap
              ("C-v" . yas-next-field)
              ("M-v" . yas-prev-field))
  :custom (yas-global-mode t))

(provide 'jdp-core-completion)
