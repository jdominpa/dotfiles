;;; init-linux.el --- Linux specific settings -*- lexical-binding: t -*-
;;; Commentary:

;; Custom settings for GNU Linux systems.

;;; Code:

;; On Linux Emacs doesn't use the shell PATH if it's not started from
;; the shell. This fixes that problem
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package exwm
  :config
  ;; Turn on `display-time-mode' if you don't use an external bar.
  (setq display-time-default-load-average nil)
  (display-time-mode t)

  ;; Load EXWM.
  (require 'exwm)
  (require 'exwm-config)

  ;; Make workspace 1 be the one where we land at startup
  (add-hook 'exwm-init-hook (lambda ()
                              (exwm-workspace-switch-create 1)))

  ;; Set the initial number of workspaces (they can also be created later).
  (setq exwm-workspace-number 5)

  ;; Change workspace names to the application name
  (add-hook 'exwm-update-class-hook (lambda ()
                                      (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Global keybindings can be defined with `exwm-input-global-keys'.
  ;; Here are a few examples:
  (setq exwm-input-global-keys
        `(;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
                      (interactive)
                      (start-process "" nil "/usr/bin/slock")))))

  ;; C-q will pass the next key to the program focused instead of Emacs
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))

  (exwm-enable))


(provide 'init-linux)
;;; init-linux.el ends here
