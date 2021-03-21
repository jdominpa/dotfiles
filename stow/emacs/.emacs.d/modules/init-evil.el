;;; init-evil.el --- Evil mode packages and configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for evil-mode.  This module also installs additional
;; packages to improve evil-mode integration in Emacs.

;;; Code:

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;; Evil-collection tries to make company-mode work like vim
  ;; plugins. Since we'll be making our own company-mode config, we
  ;; disable this option so that it doesn't clash with our settings
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  :after evil
  :diminish evil-snipe-mode evil-snipe-local-mode
  :config
  (evil-snipe-mode)
  (evil-snipe-override-mode))

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC")

  (evilem-define (kbd "SPC s") 'evil-snipe-repeat
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))

  (evilem-define (kbd "SPC S") 'evil-snipe-repeat-reverse
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(provide 'init-evil)
;;; init-evil.el ends here
