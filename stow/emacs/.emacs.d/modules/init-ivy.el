;;; init-ivy.el --- Ivy setup -*- lexical-binding: t -*-
;;; Commentary:

;; Ivy-related config.  Ivy is a smart framework for minibuffer
;; completion/filtering/selection (think of ido).  Swiper and counsel
;; are two packages built on top of ivy that provide ivy-powered
;; versions of popular Emacs commands.

;;; Code:

(use-package amx)

;; We only need to install counsel since ivy and swiper will be
;; installed as dependencies
(use-package counsel
  :diminish ivy-mode counsel-mode
  :demand t
  :bind (:map ivy-mode-map
              ("C-s" . swiper)
         :map counsel-mode-map
              ([remap swiper] . counsel-grep-or-swiper)
              ([remap swiper-backward] . counsel-grep-or-swiper-backward))
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-wrap t
                enable-recursive-minibuffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format "(%d/%d) "
                ivy-initial-inputs-alist nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t)
  (setq-default counsel-mode-override-describe-bindings t)
  (if (executable-find "rg")
      (bind-key "C-c g" 'counsel-rg))
  (ivy-mode)
  (counsel-mode))

(use-package ivy-rich
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))


(provide 'init-ivy)
;;; init-ivy.el ends here
