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

;;; Spacious padding
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :bind ([f8] . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:right-divider-width 1))
  (spacious-padding-mode t))

;;; Which-key
(use-package which-key
  :ensure t
  :custom
  (which-key-show-prefix 'bottom)
  (which-key-popup-type 'minibuffer)
  (which-key-preserve-window-configuration t)
  (which-key-idle-delay 0.6)
  (which-key-idle-secondary-delay 0.2)
  (which-key-mode t))

;;; Modeline
(use-package mini-echo
  :ensure t
  :custom
  (mini-echo-default-segments
   '(:long ("time" "meow" "major-mode" "shrink-path" "vcs" "envrc"
            "eglot" "flymake" "mise" "process" "selection-info"
            "narrow" "macro" "profiler")
           :short ("time" "meow" "buffer-name"
                   "flymake" "process" "selection-info"
                   "narrow" "macro" "profiler")))
  (mini-echo-separator " | ")
  (mini-echo-mode t))
(customize-set-variable 'ring-bell-function 'ignore)

;;; Display current time
(use-package time
  :custom
  (display-time-format "%d-%m-%Y, %H:%M")
  (display-time-interval 60)
  (display-time-default-load-average nil)
  (display-time-mode t))

;;; Display battery on laptops
(use-package battery
  :custom
  (battery-mode-line-format "[%b%p%%] ")
  (battery-load-low 20)
  (battery-load-critical 10)
  (display-battery-mode t))

(provide 'jdp-core-ui)
