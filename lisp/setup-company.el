(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-idle-delay 0
                company-minimum-prefix-length 3))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode)
  :config (setq company-posframe-show-indicator nil
                company-posframe-show-params '((alpha . 10))
                company-posframe-quickhelp-show-header nil
                company-posframe-quickhelp-delay nil))

(provide 'setup-company)
;;; setup-company.el ends here.
