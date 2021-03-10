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

;; Remove fringes at startup and at every frame created after. Make Emacs start
;; maximized.
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'initial-frame-alist no-border)
  (add-to-list 'default-frame-alist no-border))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 1000
      scroll-preserve-screen-position 'always)

;; Mode line settings
(setq line-number-mode t
      column-number-mode t
      size-indication-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; A simple visible bell to replace the constant beeping
(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

;; Package to flash point after jumping
(use-package beacon
  :diminish
  :config
  (setq-default beacon-size 30)
  (beacon-mode))

;; Set up a nice theme
(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t))

;; Resize font in all buffers at the same size
(use-package default-text-scale
  :config
  (default-text-scale-mode))

;; Font configuration
(when (member "Fira Code" (font-family-list))
  (set-frame-font "Fira Code-13" nil t))

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

(bind-key "M-C-7" (lambda () (interactive) (jdp/adjust-opacity nil -2)))
(bind-key "M-C-8" (lambda () (interactive) (jdp/adjust-opacity nil 2)))
(bind-key "M-C-6" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; Show available keybindings after we start to type
(use-package which-key
  :diminish
  :config
  (setq-default which-key-idle-delay 1.25)
  (which-key-mode))


(provide 'init-ui)
;;; init-ui.el ends here
