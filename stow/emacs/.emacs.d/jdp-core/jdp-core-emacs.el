;;; General settings
(use-package emacs
  :demand t
  :bind (("C-x C-z" . nil)
         ("C-z" . nil)
         ;; Commands for buffers
         ("C-x k" . kill-current-buffer)
         ("C-x K" . kill-buffer)
         ;; Help commands
         ("C-h K" . describe-keymap)
         ("C-h c" . describe-char)
         ;; Commands for lines
         ("M-\\" . cycle-spacing)
         ;; Commands for text manipulation
         ("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim))
  :custom
  (help-window-select t)
  (next-error-recenter '(nil)))

;;; Custom commands
(use-package jdp-simple
  :bind (;; Commands for marking objects
         ("M-@" . jdp-simple-mark-word)
         ("M-L" . jdp-simple-mark-line)
         ("C-M-SPC" . jdp-simple-mark-sexp)
         ("C-M-=" . jdp-simple-mark-inside-sexp)
         ("C-M--" . jdp-simple-kill-inside-sexp)
         ("M-U" . jdp-simple-unwrap-sexp)
         ("M-S" . jdp-simple-unwrap-mark-sexp)))

;;; Track recently visited files and directories
(use-package recentf
  :custom
  (recentf-save-file (locate-user-emacs-file "recentf"))
  (recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip"))
  (recentf-mode t))

;;; Mouse configuration
(use-package mouse
  :custom
  (mouse-wheel-scroll-amount
   '(1
     ((shift) . 5)
     ((meta) . 0.5)
     ((control) . text-scale)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-mode t))

;;; Scrolling behaviour
(custom-set-variables '(scroll-margin 0)
                      '(scroll-conservatively 1)
                      '(scroll-preserve-screen-position 'always))

;;; Repeat mode
(use-package repeat
  :custom
  (repeat-exit-timeout 5)
  (set-mark-command-repeat-pop t)
  (repeat-mode t))

;;; Auto revert mode
(use-package autorevert
  :config (auto-revert-mode))

;;; Delete selection
(use-package delsel
  :custom (delete-selection-mode t))

;;; Avy for navigation within the screen contents
(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-j" . avy-isearch)))

;;; Emacs server
(use-package server
  :config
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

;;; Save cursor position
(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-forget-unreadable-files t)
  (save-place-mode t))

(provide 'jdp-core-emacs)
