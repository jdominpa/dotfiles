;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for company-mode

;;; Code:

(add-to-list 'completion-styles 'initials t)

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
                company-show-numbers t
                company-idle-delay 0
                company-minimum-prefix-length 1
                company-tooltip-flip-when-above t))

(use-package company-quickhelp
  :after company
  :hook (global-company-mode . company-quickhelp-mode))

(use-package company-box
  :after company
  :hook (global-company-mode . company-box-mode))


(provide 'init-company)
;;; init-company.el ends here
