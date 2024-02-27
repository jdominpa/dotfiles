;;; Font configuration
(use-package fontaine
  :ensure t
  :demand t
  :bind ("C-c f" . fontaine-set-preset)
  :custom
  (fontaine-presets '((regular
                       :default-height 130)
                      (large
                       :default-weight semilight
                       :default-height 150
                       :bold-weight extrabold)
                      (t
                       :default-family "Iosevka Comfy"
                       :variable-pitch-family "Iosevka Comfy Motion Duo")))
  :custom
  (fontaine-mode t)
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(provide 'jdp-core-font)
