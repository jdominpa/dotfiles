;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package color-theme-sanityinc-tomorrow)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(doom-dracula))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(doom-dracula))
  (reapply-themes))

(use-package dimmer
  :init (setq-default dimmer-fraction 0.15)
  :hook (after-init . dimmer-mode)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  ;; Don't dim in terminal windows. Even with 256 colours it can
  ;; lead to poor contrast.  Better would be to vary dimmer-fraction
  ;; according to frame type.
  (defun jdominpa/display-non-graphic-p ()
    (not (display-graphic-p)))
  (push 'jdominpa/display-non-graphic-p dimmer-exclusion-predicates))


(provide 'init-themes)
;;; init-themes.el ends here
