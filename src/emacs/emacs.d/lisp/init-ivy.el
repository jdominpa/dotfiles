;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :bind (:map ivy-mode-map
              ("C-s" . swiper-thing-at-point)
         :map ivy-occur-mode-map
              ("C-c C-q" . ivy-wgrep-change-to-wgrep-mode))
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t
                ivy-re-builders-alist '((swiper . ivy--regex-plus) (t . ivy--regex-fuzzy)))
  (setq-default ivy-initial-inputs-alist
                '((Man-completion-table . "^")
                  (woman . "^"))))

(use-package counsel
  :hook (after-init . counsel-mode)
  :config (setq-default counsel-mode-override-describe-bindings t))


(provide 'init-ivy)
;;; init-ivy.el ends here
