;;; Eshell
(use-package eshell
  :custom
  (eshell-cd-on-directory t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-directory-name (locate-user-emacs-file "eshell"))
  (eshell-aliases-file (locate-user-emacs-file "eshell/alias"))
  (password-cache t)
  (password-cache-expiry 300))

(provide 'jdp-core-shell)
