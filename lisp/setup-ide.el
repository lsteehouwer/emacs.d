(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode					.	lsp-enable-which-key-integration)
		 (c-mode					.	lsp-deferred)
		 (c++-mode					.	lsp-deferred)
		 (python-mode				.	lsp-deferred)
		 (ruby-mode					.	lsp-deferred)
		 (haskell-mode				.	lsp-deferred)
		 (haskell-interactive-mode	.	lsp-deferred)
		 (dockerfile-mode			.	lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l"
		lsp-clients-clangd-args '("--compile-commands-dir=debug"))
  :config
  (setq lsp-idle-delay 0
		lsp-enable-symbol-highlighting t
		lsp-enable-snippet t))

(use-package lsp-ui
  :after company-box
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-signature-auto-activate nil
		lsp-signature-function 'lsp-signature-posframe
		lsp-signature-render-documentation t
		lsp-ui-doc-enable t
		lsp-ui-doc-delay 0
		lsp-ui-doc-show-with-cursor t
		lsp-ui-doc-show-with-mouse nil
		lsp-ui-doc-position 'at-point
		lsp-ui-doc-max-width 70
		lsp-eldoc-enable-hover nil)
  :bind ((:map evil-normal-state-map
			   ("gd" . 'lsp-ui-peek-find-definitions)
			   ("gr" . 'lsp-ui-peek-find-references))
		 (:map lsp-ui-peek-mode-map
			   ("j" . 'lsp-ui-peek--select-next)
			   ("k" . 'lsp-ui-peek--select-prev))))

(use-package flycheck
  :ensure t)

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-is-never-other-window t
		treemacs-position 'left
		treemacs-width 35)
  :bind (("<f8>" . treemacs)
		 ("S-<f8>" . treemacs-select-window)))

(use-package lsp-treemacs
  :ensure t
  :defer t
  :config (lsp-treemacs-sync-mode t))

(use-package projectile
  :ensure t
  :config (projectile-mode t)
  :bind (:map projectile-mode-map
			  ("C-c p" . projectile-command-map)))

(use-package magit
  :defer t
  :ensure t)

(use-package vterm
  :ensure t
  :bind ("C-S-t" . vterm-other-window)
  :config
  (setq vterm-timer-delay 0))

(provide 'setup-ide)
