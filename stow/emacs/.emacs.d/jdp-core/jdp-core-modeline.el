;;; Which-key
(use-package which-key
  :ensure t
  :custom
  (which-key-show-prefix 'bottom)
  (which-key-popup-type 'minibuffer)
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

(setq mode-line-defining-kbd-macro
      (propertize " KMacro" 'face 'mode-line-emphasis))

(use-package jdp-modeline
  :disabled t
  :custom
  (mode-line-position-column-line-format '(" %l,%c"))
  (mode-line-defining-kbd-macro
   (propertize " KMacro" 'face 'mode-line-emphasis))
  (mode-line-format '(;; Left hand side
                      "%e"
                      (:eval (meow-indicator))
                      jdp-mode-line-kbd-macro
                      jdp-mode-line-narrow
                      jdp-mode-line-remote-file
                      jdp-mode-line-window-dedicated-status
                      jdp-mode-line-input-method
                      "  "
                      jdp-mode-line-buffer-identification
                      "  "
                      jdp-mode-line-major-mode
                      jdp-mode-line-process
                      "  "
                      jdp-mode-line-vc-branch
                      "  "
                      jdp-mode-line-eglot
                      "  "
                      jdp-mode-line-flymake
                      ;; Right hand side
                      mode-line-format-right-align
                      jdp-mode-line-misc-info))
  :config
  (with-eval-after-load 'spacious-padding
    (defun jdp-mode-line-spacious-indicators ()
      "Set box attribute to `'jdp-mode-line-indicator-button' if spacious-padding is enabled."
      (if (bound-and-true-p spacious-padding-mode)
          (set-face-attribute 'jdp-mode-line-indicator-button nil :box t)
        (set-face-attribute 'jdp-mode-line-indicator-button nil :box 'unspecified)))

    ;; Run the function at startup and then afterwards whenever
    ;; `spacious-padding-mode' is toggled on/off.
    (jdp-mode-line-spacious-indicators)
    (add-hook 'spacious-padding-mode-hook #'jdp-mode-line-spacious-indicators)))

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

;;; Make bell flash the modeline
(cl-flet ((flash-mode-line ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil #'invert-face 'mode-line)))
  (customize-set-variable 'ring-bell-function #'flash-mode-line))

(provide 'jdp-core-modeline)
