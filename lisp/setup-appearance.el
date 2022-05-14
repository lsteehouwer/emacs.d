(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-responsive 'top
		highlight-indent-guides-delay 0
		highlight-indent-guides-method 'character))

(use-package rainbow-delimiters
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)
						  (registers . 10))))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 32
		doom-modeline-workspace-name t
		doom-modeline-icon t)
  (doom-modeline-mode t))

(use-package monokai-theme
  :ensure t
  :defer t)

(use-package modus-themes
  :ensure t
  :defer t)

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 52.37
		calendar-longitude 4.89
		circadian-themes '((:sunrise . modus-operandi)
						   (:sunset . monokai)))
  (circadian-setup))

(use-package all-the-icons
  :ensure t)

(use-package treemacs-all-the-icons
  :after treemacs
  :ensure t
  :config (treemacs-load-theme "all-the-icons"))


(provide 'setup-appearance)
