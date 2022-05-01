;; Setup completion styles and frameworks

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
		completion-ignore-case t
		vertico-resize t
		vertico-multiform-categories '((file reverse)
									   (consult-grep buffer)
									   (imenu buffer)))
  (vertico-mode)
  (vertico-multiform-mode))

(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)

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
  :bind (("C-s" . consult-line)
		 ("C-c i" . consult-imenu)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(partial-completion substring flex initials)
		completion-category-overrides '((file (styles partial-completion))
										(consult-location (styles orderless)))))

(provide 'setup-completion)
