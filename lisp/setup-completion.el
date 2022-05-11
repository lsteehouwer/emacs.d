;; Setup completion styles and frameworks

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t
		corfu-cycle t)
  (global-corfu-mode))

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
(define-key vertico-map (kbd "TAB") #'minibuffer-complete)

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
  (setq completion-styles '(basic partial-completion emacs22)
		completion-category-overrides '((file (styles partial-completion))
										(consult-location (styles orderless)))))

(provide 'setup-completion)
