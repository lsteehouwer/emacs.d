(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  (message "Yas should be active now"))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(provide 'setup-snippets)
