;;; Isearch and occur
(use-package isearch
  :bind (:map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("M-/" . isearch-complete))
  :custom
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]")
  (isearch-yank-on-move 'shift)
  (isearch-allow-scroll 'unlimited)
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause 'no)
  :config
  (setq isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil))

(use-package replace
  :hook (occur-mode . hl-line-mode))

(provide 'jdp-core-search)
