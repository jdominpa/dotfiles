;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for editing markdown files.

;;; Code:

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (with-eval-after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))


(provide 'init-markdown)
;;; init-markdown.el ends here
