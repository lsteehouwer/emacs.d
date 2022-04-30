;; ruby

(use-package ruby-electric
  :ensure t
  :hook (ruby-mode . ruby-electric-mode))

(use-package yard-mode
  :ensure t
  :hook (ruby-mode . yard-mode))


;; c(++)
(setq c-tab-width 4)
(use-package cc-mode
  :config (setq-default c-basic-offset c-tab-width))

;; haskell
(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :hook ((haskell-mode . lsp-deferred)
		 (haskell-mode . interactive-haskell-mode)))

(provide 'setup-languages)
