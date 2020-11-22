;;; init-evil.el --- Evil mode packages and configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for evil-mode.  This module also installs additional packages
;; to improve evil-mode integration in Emacs.

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :hook (after-init . evil-mode)
  :bind ("M-u" . universal-argument)
  :config
  (setq-default evil-shift-width 2
                evil-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-magit
  :after evil)

(use-package evil-org
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(provide 'init-evil)
;;; init-evil.el ends here
