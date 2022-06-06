(use-package dired
  :ensure nil
  :config
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-la --group-directories-first"))

(provide 'setup-dired)
;;; setup-dired.el ends here
