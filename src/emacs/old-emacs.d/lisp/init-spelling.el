;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ispell)

(use-package flyspell
  :ensure nil
  :if (executable-find ispell-program-name)
  :hook (prog-mode . flyspell-prog-mode)
  :config
  (unbind-key "C-;" flyspell-mode-map)
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))


(provide 'init-spelling)
;;; init-spelling.el ends here
