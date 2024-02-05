;;; Font configuration
(use-package fontaine
  :ensure t
  :demand t
  :bind (("C-c f" . fontaine-set-preset)
         ("C-c F" . fontaine-set-face-font))
  :custom
  (fontaine-presets '((regular
                       :default-height 120)
                      (large
                       :default-weight semilight
                       :default-height 160
                       :bold-weight extrabold)
                      (t
                       :default-family "Fira Code"
                       :default-weight regular
                       :default-height 135
                       :fixed-pitch-height 1.0
                       :fixed-pitch-serif-height 1.0
                       :bold-weight bold
                       :italic-slant italic)))
  :hook ((kill-emacs . fontaine-store-latest-preset)
         (enable-theme-functions . fontaine-apply-current-preset))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(provide 'jdp-core-font)
