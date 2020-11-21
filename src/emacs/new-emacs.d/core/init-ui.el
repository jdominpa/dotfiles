;;; init-ui.el --- UI customizations and tweaks -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for most of the point and click UI, reduce startup noise,
;; opacity settings, configure smooth scrolling and nice theme and font size.

;;; Code:

;; Remove toolbar, menubar and scrollbar since they aren't needed for anything
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(menu-bar-mode -1)

;; Make mouse command use box dialogs
(setq use-file-dialog nil
      use-dialog-box nil)

;; Disable startup messages
(setq initial-scratch-message nil
      inhibit-startup-screen t)

;; Remove fringes at startup and at every frame created after
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'initial-frame-alist no-border)
  (add-to-list 'default-frame-alist no-border))

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 1000
      scroll-preserve-screen-position 'always)

;; Mode line settings
(setq line-number-mode t
      column-number-mode t
      size-indication-mode t)

;; Show line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; A simple visible bell to replace the constant beeping
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

;; Package to flash point after jumping
(use-package beacon
  :diminish
  :hook (after-init . beacon-mode)
  :config
  (setq-default beacon-size 30))

;; Change default modeline to doom-modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
(use-package all-the-icons
  :after doom-modeline)

;; Set up a nice theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(load-theme 'doom-tomorrow-night t)

;; Set font size
(setq jdp/default-font "monospace-14")

(defun jdp/set-font ()
  "Set the font to `jdp/default-font'. Set that for the current frame, and also make it the default for other, future frames."
  (if (assoc 'font default-frame-alist)
      (setcdr (assoc 'font default-frame-alist) jdp/default-font)
    (add-to-list 'default-frame-alist (cons 'font jdp/default-font))
    (set-frame-font jdp/default-font)))

(jdp/set-font)

;; Settings to adjust the opacity of the frame on the fly
(defun jdp/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(bind-key "[C-M-7]" (lambda () (interactive) (jdp/adjust-opacity nil -2)))
(bind-key "[C-M-8]" (lambda () (interactive) (jdp/adjust-opacity nil 2)))
(bind-key "[C-M-6]" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; Package to dim unfocused buffers
(use-package dimmer
  :init (setq-default dimmer-fraction 0.20)
  :hook (after-init . dimmer-mode)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  ;; Don't dim in terminal windows. Even with 256 colours it can
  ;; lead to poor contrast.  Better would be to vary dimmer-fraction
  ;; according to frame type.
  (defun jdp/display-non-graphic-p ()
    (not (display-graphic-p)))
  (push 'jdp/display-non-graphic-p dimmer-exclusion-predicates))

;; Show available keybindings after we start to type
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.25))


(provide 'init-ui)
;;; init-ui.el ends here
