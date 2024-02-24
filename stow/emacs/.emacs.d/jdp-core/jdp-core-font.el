;;; Font configuration
(use-package fontaine
  :ensure t
  :demand t
  :bind ("C-c f" . fontaine-set-preset)
  :custom
  (fontaine-presets '((regular
                       :default-height 120)
                      (large
                       :default-weight semilight
                       :default-height 160
                       :bold-weight extrabold)
                      (t
                       :default-family "Fira Code"
                       :mode-line-active-family "Fira Code"
                       :mode-line-inactive-family "Fira Code")))
  :custom
  (fontaine-mode t)
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(provide 'jdp-core-font)
