(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-mode t))

(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
		 ("C-h v" . #'helpful-variable)
		 ("C-h k" . #'helpful-key)))

(use-package olivetti
  :ensure  t
  :commands olivetti-mode)

(provide 'setup-misc)
