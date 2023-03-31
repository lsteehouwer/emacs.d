;;; Package -- Summary

;;; Commentary:
;; Init file.  It generates a configuration from config.org if it does not exist
;; and then loads it

;;; Code:
(let ((file-name-handler-alist nil)
      (config-file (expand-file-name "config.org" user-emacs-directory))
      (tangled-file (expand-file-name "config.el" user-emacs-directory)))

  (if (file-exists-p tangled-file)
      (load-file tangled-file)
    (require 'org)
    (org-babel-load-file config-file)))

;;; init.el ends here
