;;; Modus theme
;; NOTE 19-12-2022: modus themes will be refactored in a couple days
(use-package modus-themes
  :ensure t
  :bind ([f5] . modus-themes-toggle)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-region '(bg-only))
  (modus-themes-paren-match '(bold))
  :init
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(provide 'jdp-core-theme)
