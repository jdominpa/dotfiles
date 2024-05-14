;;; Modus theme
(use-package modus-themes
  :ensure t
  :demand t
  :bind ([f5] . modus-themes-toggle)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-slanted-constructs t)
  (modus-themes-completions '((t . (extrabold))))
  (modus-themes-prompts '(extrabold))
  :config
  (load-theme 'modus-vivendi t))

;;; Spacious padding
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :bind ([f8] . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:mode-line-width 3
     :right-divider-width 15))
  (spacious-padding-mode t))

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

(provide 'jdp-core-theme)
