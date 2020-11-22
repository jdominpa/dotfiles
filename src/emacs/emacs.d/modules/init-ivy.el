;;; init-ivy.el --- Ivy setup -*- lexical-binding: t -*-
;;; Commentary:

;; Ivy-related config.  Ivy is a smart framework for minibuffer
;; completion/filtering/selection (think of ido).  Swiper and counsel
;; are two packages built on top of ivy that provide ivy-powered
;; versions of popular Emacs commands.

;;; Code:

;; Make M-x remember recent commands
(use-package smex
  :disabled t
  :bind ([remap execute-extended-command] . smex)
  :config
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

;; We only need to install counsel since ivy and swiper will be
;; installed as dependencies
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
                ivy-wrap t
                enable-recursive-minibuffers t
                projectile-completion-system 'ivy
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
