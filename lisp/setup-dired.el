(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-other-tab dired-other-window dired-other-frame dired-jump-other-window)
  :config
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-la --group-directories-first"))

(provide 'setup-dired)
;;; setup-dired.el ends here
