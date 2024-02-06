;;; Custom commands
(use-package jdp-simple
  :bind (("C-x C-z" . nil)
         ("C-z" . nil)
         ("C-x k" . kill-current-buffer)
         ("C-x K" . kill-buffer)
         ("C-h K" . describe-keymap)
         ("C-h c" . describe-char)
         ;; Commands for lines
         ("M-o" . delete-blank-lines)   ; alias for C-x C-o
         ("M-SPC" . cycle-spacing)
         ;; Commands for text manipulation
         ("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ;; Commands for marking objects
         ("M-@" . jdp-simple-mark-word)
         ("M-L" . jdp-simple-mark-line)
         ("C-M-=" . jdp-simple-mark-inside-sexp)
         ("C-M--" . jdp-simple-kill-inside-sexp)
         ("M-U" . jdp-simple-unwrap-sexp)
         ("M-S" . jdp-simple-unwrap-mark-sexp)))

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

;;; Always focus help window
(customize-set-variable 'help-window-select t)

;;; Delete selection
(use-package delsel
  :custom (delete-selection-mode t))

;;; Auto revert mode
(use-package autorevert
  :config (auto-revert-mode))

;;; Avy for navigation within the screen contents
(use-package avy
  :ensure t
  :bind ("C-'" . avy-goto-char-timer))

(provide 'jdp-core-emacs)
