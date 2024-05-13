;;; init.el --- Personal init file -*- lexical-binding: t -*-

;; Put custom configuration in a separate file
(customize-set-variable 'custom-file (make-temp-file "emacs-custom-"))

;; Some basic settings
(setq disabled-command-function nil)
(customize-set-variable 'initial-buffer-choice t)          ; always start with *scratch* buffer
(customize-set-variable 'blink-cursor-mode nil)

;; Backups
(custom-set-variables '(make-backup-files nil)
                      '(create-lockfiles nil))


;;; Packages and modules

;; Don't automatically show native compilation warning messages
(when (native-compile-available-p)
  (customize-set-variable 'native-comp-async-report-warnings-errors 'silent))

(require 'package)
(customize-set-variable 'package-archives
                        '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                          ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priority '(("gnu-elpa" . 3)
                                                    ("melpa" . 2)
                                                    ("nongnu" . 1)))

;; "jdp-core" is for all my emacs configuration modules
;; "jdp-lisp" is used for all my custom elisp files
(dolist (path '("jdp-core" "jdp-lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'jdp-core-emacs)
(require 'jdp-core-theme)
(require 'jdp-core-modeline)
(require 'jdp-core-completion)
(require 'jdp-core-search)
(require 'jdp-core-dired)
(require 'jdp-core-window)
(require 'jdp-core-git)
(require 'jdp-core-shell)
(require 'jdp-core-write)
(require 'jdp-core-proglangs)


;;; System settings

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control))

(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (customize-set-variable 'browse-url-generic-program cmd-exe)
      (customize-set-variable 'browse-url-generic-args cmd-args)
      (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
      (setq search-web-default-browser 'browse-url-generic))))

(provide 'init)
;;; init.el ends here
