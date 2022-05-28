;; move backup files to tmp directory
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; move autosave files to tmp directory
(make-directory (expand-file-name "tmp/autosave" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/autosave/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/autosave" user-emacs-directory) t)))

(provide 'setup-clutter)
;;; setup-clutter.el ends here
