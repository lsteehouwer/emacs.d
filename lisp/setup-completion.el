(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
		company-minimum-prefix-length 3)
  (global-company-mode t))

(use-package vertico
  :ensure t
  :config
  (setq read-file-name-completion-ignore-case t
		read-buffer-completion-ignore-case t
		completion-ignore-case t)
  (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :config
  (setq consult-narrow-key "<")
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any))
  :bind (("C-s" . consult-line)))
		 ;; ("/" . consult-line)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
		completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'setup-completion)
