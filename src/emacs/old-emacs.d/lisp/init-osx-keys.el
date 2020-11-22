;;; init-osx-keys.el --- Configure keys specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (bind-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (bind-keys ("M-h" . ns-do-hide-emacs)
             ("M-H" . ns-do-hide-others))
  (use-package nxml-mode
    :ensure nil
    :config
    (unbind-key "M-h" nxml-mode-map)))


(provide 'init-osx-keys)
;;; init-osx-keys.el ends here