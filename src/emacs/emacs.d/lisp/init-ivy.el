;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :hook ((after-init . ivy-mode)
         (after-init . counsel-mode))
  :bind (:map ivy-mode-map
              ("C-s" . swiper)
              :map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              :map ivy-switch-buffer-map
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-occur-mode-map
              ("C-c C-q" . ivy-wgrep-change-to-wgrep-mode)
              :map counsel-mode-map
              ([remap swiper] . counsel-grep-or-swiper)
              ([remap swiper-backward] . counsel-grep-or-swiper-backward))
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format "(%d/%d) "
                ivy-initial-inputs-alist nil
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t)

  (setq-default counsel-mode-override-describe-bindings t)
  (if (executable-find "rg")
      (bind-key "C-c C-g" 'counsel-rg)))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))


(provide 'init-ivy)
;;; init-ivy.el ends here
