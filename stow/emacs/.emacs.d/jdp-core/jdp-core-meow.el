;;; Meow setup
(use-package meow
  :ensure t
  :custom
  (meow-replace-state-name-list
   '((normal . "<N>")
     (motion . "<M>")
     (keypad . "<K>")
     (insert . "<I>")
     (beacon . "<B>")))
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("k" . meow-next)
     '("l" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC k/l will run the original command in MOTION state.
     '("k" . "H-k")
     '("l" . "H-l")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     ;; Numeric arguments
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)

     ;; Movement
     '("j" . meow-left)
     '("k" . meow-next)
     '("l" . meow-prev)
     '(";" . meow-right)
     '("b" . meow-back-word)
     '("e" . meow-next-word)
     '("B" . meow-back-symbol)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("t" . meow-till)
     '("X" . meow-goto-line)

     ;; Edit
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     
     ;; kill-region
     '("s" . meow-kill)
     ;; kill-ring-save
     '("y" . meow-save)
     ;; yank
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     ;; replace
     '("r" . meow-replace)
     '("%" . meow-query-replace)
     '("&" . meow-query-replace-regexp)
     
     ;; Selection
     '("h" . meow-reverse)
     '("J" . meow-left-expand)
     '("K" . meow-next-expand)
     '("L" . meow-prev-expand)
     '(":" . meow-right-expand)
     '("x" . meow-line)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("m" . meow-join)
     '("g" . meow-cancel-selection)
     '("z" . meow-pop-selection)

     ;; Grab
     '("G" . meow-grab)
     '("Y" . meow-sync-grab)
     '("R" . meow-swap-grab)
     
     ;; Search
     '("n" . meow-search)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)

     ;; Thing
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)

     ;; Actions
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode t))

(provide 'jdp-core-meow)
