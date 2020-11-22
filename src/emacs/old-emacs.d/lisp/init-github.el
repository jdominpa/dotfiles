;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-git)

(use-package bug-reference-github
  :hook (prog-mode . bug-reference-prog-mode))

(use-package forge)


(provide 'init-github)
;;; init-github.el ends here
