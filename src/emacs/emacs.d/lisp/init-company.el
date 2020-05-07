;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind (("M--" . company-complete)
         :map company-active-map
         ([tab] . #'company-select-next)
         ([S-tab] . #'company-select-previous))
  :config
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)

  ;; Suspend page-break-lines-mode while company menu is active
  ;; (see https://github.com/company-mode/company-mode/issues/416)
  (after-load 'page-break-lines
              (defvar-local jdominpa/page-break-lines-on-p nil)

              (defun jdominpa/page-break-lines-disable (&rest ignore)
                (when (setq jdominpa/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
                  (page-break-lines-mode -1)))

              (defun jdominpa/page-break-lines-maybe-reenable (&rest ignore)
                (when jdominpa/page-break-lines-on-p
                  (page-break-lines-mode 1)))

              (add-hook 'company-completion-started-hook 'jdominpa/page-break-lines-disable)
              (add-hook 'company-after-completion-hook 'jdominpa/page-break-lines-maybe-reenable)))

(use-package company-quickhelp
  :after company
  :hook (global-company-mode . company-quickhelp-mode))


(provide 'init-company)
;;; init-company.el ends here
