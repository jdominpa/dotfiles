;;; init-completion.el --- Autocompletion settings -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for autocompletion using company and yasnippets

;;; Code:

(add-to-list 'completion-styles 'initials t)

;; Company package for autocompletion
(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete)
              ([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
              ([tab] . company-complete-selection))
  :config
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t
                company-idle-delay 0
                company-minimum-prefix-length 1
                company-tooltip-flip-when-above t))

(use-package company-quickhelp
  :after company
  :hook (global-company-mode . company-quickhelp-mode))

;; Code snippets
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :bind (:map yas-minor-mode-map
              ("C-SPC" . yas-expand))
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (use-package yasnippet-snippets))


(provide 'init-completion)
;;; init-completion.el ends here
