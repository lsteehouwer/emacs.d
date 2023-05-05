;;; Package -- Summary

;;; Commentary:
;; Init file. It generates a configuration from config.org if it does not exist
;; and then loads it

;;; Code:
(defun ls/tangle-load (file)
  "Tangle org file FILE and load it."
  (require 'org)
  (org-babel-load-file file))

(let ((file-name-handler-alist nil)
      (config-file (expand-file-name "config.org" user-emacs-directory))
      (tangled-file (expand-file-name "config.el" user-emacs-directory)))

  (if (not (file-exists-p tangled-file))
      (ls/tangle-load config-file)
    (let ((mod-time-config  (nth 5 (file-attributes config-file)))
          (mod-time-tangled (nth 5 (file-attributes tangled-file))))
      (if (time-less-p mod-time-tangled mod-time-config)
          (ls/tangle-load config-file)
        (load-file tangled-file)))))
;;; init.el ends here
