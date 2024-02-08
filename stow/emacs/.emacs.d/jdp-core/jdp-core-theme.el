;;; Modus theme
(use-package modus-themes
  :ensure t
  :demand t
  :bind ([f5] . modus-themes-toggle)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  :config
  (load-theme 'modus-vivendi t))

(provide 'jdp-core-theme)
