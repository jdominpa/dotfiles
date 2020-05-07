;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :bind (:map ivy-mode-map
              ("C-s" . swiper)
         :map ivy-occur-mode-map
              ("C-c C-q" . ivy-wgrep-change-to-wgrep-mode))
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :diminish
  :hook (after-init . counsel-mode)
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (setq-default ivy-initial-inputs-alist
                '((Man-completion-table . "^")
                  (woman . "^")))
  (cond
   ((executable-find "rg") (global-set-key (kbd "C-c g") 'counsel-rg))
   ((executable-find "ag") (global-set-key (kbd "C-c g") 'counsel-ag))
   ((executable-find "pt") (global-set-key (kbd "C-c g") 'counsel-pt))
   ((executable-find "ack") (global-set-key (kbd "C-c g") 'counsel-ack))))


(provide 'init-ivy)
;;; init-ivy.el ends here
