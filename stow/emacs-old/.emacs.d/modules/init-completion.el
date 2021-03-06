;;; init-completion.el --- Autocompletion settings -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for autocompletion using company and yasnippets

;;; Code:

(add-to-list 'completion-styles 'initials t)
(bind-key "M-/" 'hippie-expand)

;; Company package for autocompletion
(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete)
              ([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
              ("TAB" . nil)
              ([tab] . nil)
              ("RET" . nil)
              ([return] . nil)
              ("C-SPC" . company-complete-selection))
  :config
  (setq-default company-show-numbers t
                company-idle-delay 0.25
                company-minimum-prefix-length 1
                company-tooltip-align-annotations t
                company-tooltip-flip-when-above t))

;; Code snippets
(use-package yasnippet
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)


(provide 'init-completion)
;;; init-completion.el ends here
