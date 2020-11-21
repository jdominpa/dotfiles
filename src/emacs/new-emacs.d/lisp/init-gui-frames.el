;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun jdp/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(bind-key "C-z" 'jdp/maybe-suspend-frame)


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
(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

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

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (bind-key "M-ƒ" 'toggle-frame-fullscreen))

(bind-key "M-C-8" (lambda () (interactive) (jdp/adjust-opacity nil -2)))
(bind-key "M-C-9" (lambda () (interactive) (jdp/adjust-opacity nil 2)))
(bind-key "M-C-7" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


(when *is-a-mac*
  (use-package ns-auto-titlebar
    :config (ns-auto-titlebar-mode)))

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
(setq jdp/default-font "monospace-13.5")

(defun jdp/set-font ()
  "Set the font to `jdp/default-font'. Set that for the current frame, and also make it the default for other, future frames."
  (if (assoc 'font default-frame-alist)
      (setcdr (assoc 'font default-frame-alist) jdp/default-font)
    (add-to-list 'default-frame-alist (cons 'font jdp/default-font))
    (set-frame-font jdp/default-font)))

(jdp/set-font)


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
