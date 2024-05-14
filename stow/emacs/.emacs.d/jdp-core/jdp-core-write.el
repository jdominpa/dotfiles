;;; General settings for writing prose

(customize-set-variable 'fill-column 80)

;;; Org mode
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :custom
  (org-adapt-indentation nil)
  (org-catch-invisible-edits 'show)
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;;; Improved PDF viewing
(use-package pdf-tools
  :ensure t
  :defer 2
  :bind (:map pdf-view-mode-map
              ("d" . pdf-view-midnight-minor-mode))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-continuous t)
  (pdf-view-midnight-colors '("#ffffff" . "#000000"))
  :config
  (pdf-loader-install))

;;; LaTeX tools
(use-package math-delimiters
  :commands
  math-delimiters-no-dollars
  math-delimiters-insert)

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . prettify-symbols-mode))
  :bind (:map TeX-mode-map
              ("$" . math-delimiters-insert))
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-label-alist
   '(("theorem"     ?T "thm:"  "~\\ref{%s}" t ("theorem")     -3)
     ("lemma"       ?L "lem:"  "~\\ref{%s}" t ("lemma")       -3)
     ("proposition" ?P "prop:" "~\\ref{%s}" t ("proposition") -3)
     ("corollary"   ?C "cor:"  "~\\ref{%s}" t ("corollary")   -3)
     ("remark"      ?R "rem:"  "~\\ref{%s}" t ("remark")      -3)
     ("definition"  ?D "defn:" "~\\ref{%s}" t ("definition")  -3)))
  :config
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (cdlatex-tab . jdp-cdlatex-indent-line)
         (cdlatex-tab . yas-expand)
         (cdlatex-tab . jdp-cdlatex-in-yas-field))
  :custom
  (cdlatex-takeover-dollar nil)
  (cdlatex-sub-super-scripts-outside-math-mode nil)
  :config
  (defun jdp-cdlatex-indent-line ()
    "Indent current line if point is before the first non-whitespace
character of the line."
    (when (or (bolp) (looking-back "^[ \t]+"))
      (LaTeX-indent-line)
      t))

  (with-eval-after-load 'yasnippet
    (defun jdp-cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun jdp-yas-next-field-or-cdlatex nil
      "Jump to the next Yas field correctly with cdlatex active."
      (interactive)
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))

    (bind-keys :map yas-keymap
               ("TAB" . jdp-yas-next-field-or-cdlatex))))

;;; Spellchecking
(use-package jinx
  :ensure t
  :hook ((text-mode . jinx-mode)
         (LaTeX-mode . jinx-mode))
  :bind (:map jinx-mode-map
              ("M-$" . jinx-correct)
              ("C-M-$" . jinx-languages))
  :custom (jinx-languages "en es"))

(provide 'jdp-core-write)
