;;; Unique buffer names
(custom-set-variables '(uniquify-buffer-name-style 'forward)
                      '(uniquify-strip-common-suffix t)
                      '(uniquify-after-kill-buffer-p t))

;;; Whitespace and line numbers modes
(bind-keys ([f6] . whitespace-mode)
           ([f7] . display-line-numbers-mode)
           ("C-c z" . delete-trailing-whitespace))

;;; Window rules and other tweaks
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
  :bind ("M-o" . ace-window))

(provide 'jdp-core-window)
