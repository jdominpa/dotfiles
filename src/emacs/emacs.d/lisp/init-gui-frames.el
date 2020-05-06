;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun jdominpa/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'jdominpa/maybe-suspend-frame)


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.
(if *is-a-mac*
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (unless (display-graphic-p frame)
                  (set-frame-parameter frame 'menu-bar-lines 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun jdominpa/adjust-opacity (frame incr)
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

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (jdominpa/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (jdominpa/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


(use-package ns-auto-titlebar
  :if *is-a-mac*
  :config (ns-auto-titlebar-mode))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


;; Change global font size easily
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

;; Set default font size
(setq jdominpa/default-font "monospace-13.5")

(defun jdominpa/set-font ()
  "Set the font to `jdominpa/default-font'. Set that for the current frame, and also make it the default for other, future frames."
  (if (assoc 'font default-frame-alist)
      (setcdr (assoc 'font default-frame-alist) jdominpa/default-font)
    (add-to-list 'default-frame-alist (cons 'font jdominpa/default-font))
    (set-frame-font jdominpa/default-font)))

(jdominpa/set-font)


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
