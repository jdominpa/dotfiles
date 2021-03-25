;;; init-personal-packages.el --- Support elisp manually installed in the personal-packages dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Set load path

(eval-when-compile (require 'cl))
(defun jdp/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(jdp/add-subdirs-to-load-path
 (expand-file-name "personal-packages/" user-emacs-directory))

;;; Utilities for grabbing upstream libs

(defun personal-packages-dir-for (name)
  (expand-file-name (format "personal-packages/%s" name) user-emacs-directory))

(defun personal-packages-library-el-path (name)
  (expand-file-name (format "%s.el" name) (personal-packages-dir-for name)))

(defun download-personal-packages-module (name url)
  (let ((dir (personal-packages-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (personal-packages-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (personal-packages-library-loadable-p name)
    (byte-compile-file (download-personal-packages-module name url))))

(defun personal-packages-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/personal-packages/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (personal-packages-dir-for name)) f))))


(provide 'init-personal-packages)
;;; init-personal-packages.el ends here
