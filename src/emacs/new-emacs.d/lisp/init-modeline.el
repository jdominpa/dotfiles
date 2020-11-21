;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :after doom-modeline)

(provide 'init-modeline)
;;; init-modeline.el ends here
