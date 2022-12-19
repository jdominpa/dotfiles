;;; Custom commands
(use-package jdp-simple
  :bind (("C-x C-z" . nil)
         ("C-z" . zap-up-to-char)
         ("C-h K" . describe-keymap)
         ("C-h c" . describe-char)
         ("C-c s" . jdp-simple-scratch-buffer)
         ("M-SPC" . cycle-spacing)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("M-@" . jdp-simple-mark-word)
         ("C-M-SPC" . jdp-simple-mark-construct-dwim)))

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
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-conservatively 1)
(customize-set-variable 'scroll-preserve-screen-position 'always)

;;; Delete selection
(use-package delsel
  :custom (delete-selection-mode t))

;;; Auto revert mode
(use-package autorevert
  :config (auto-revert-mode))

;;; Avy for navigation within the screen contents
(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-timer))

;;; NOTE 19-12-2022: currently trying custom selection commands
;; (use-package expand-region
;;   :ensure t
;;   :bind ("C-;" . er/expand-region))

(provide 'jdp-core-emacs)
