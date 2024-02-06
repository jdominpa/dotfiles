;;; Modeline
(customize-set-variable 'mode-line-position-column-line-format '(" %l,%c"))
(setq mode-line-defining-kbd-macro
      (propertize " KMacro" 'face 'mode-line-emphasis))

(customize-set-variable 'mode-line-format
                        '("%e"
                          mode-line-front-space
                          mode-line-mule-info
                          mode-line-client
                          mode-line-modified
                          mode-line-remote
                          mode-line-frame-identification
                          mode-line-buffer-identification
                          "  "
                          mode-line-position
                          " "
                          mode-line-modes
                          "  "
                          (vc-mode vc-mode)
                          "  "
                          mode-line-misc-info
                          mode-line-end-spaces))

(customize-set-variable 'line-number-mode t)
(customize-set-variable 'column-number-mode t)
(customize-set-variable 'size-indication-mode t)

;;; Hide modeline minor modes
(use-package minions
  :ensure t
  :custom
  (minions-mode-line-lighter ";")
  ;; NOTE: This list is expanded whenever I find modes that should not
  ;; be hidden
  (minions-direct (list 'defining-kbd-macro
                        'flymake-mode))
  (minions-mode t))

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
