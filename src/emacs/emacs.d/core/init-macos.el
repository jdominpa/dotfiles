;;; init-macos.el --- macOS specific settings -*- lexical-binding: t -*-
;;; Commentary:

;; Custom settings for macOS systems.

;;; Code:

;; On macOS Emacs doesn't use the shell PATH if it's not started from
;; the shell. This fixes that problem
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Set cmd to be meta key and disable option key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (bind-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

;; Make cmd-h hide the frame like it does with other macOS applications
(bind-keys ("M-h" . ns-do-hide-emacs)
           ("M-H" . ns-do-hide-others))

;; Make macOS titlebar match Emacs' theme
(use-package ns-auto-titlebar
  :config (ns-auto-titlebar-mode))

;; Disable cmd-h keybind in magit mode
(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

;; Command-Option-f to toggle fullscreen mode
(when (fboundp 'toggle-frame-fullscreen)
  (bind-key "M-ƒ" 'toggle-frame-fullscreen))

;; Change locate and ls commands
(setq-default locate-command "mdfind")
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; There's no point in hiding the menu bar on macOS
(menu-bar-mode +1)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


(provide 'init-macos)
;;; init-macos.el ends here
