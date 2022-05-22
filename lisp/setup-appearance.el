;; -*- lexical-binding: t; -*-
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode)

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

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-evil-use-short-tag t
        telephone-line-height 28
        telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat)
  (telephone-line-mode))

(use-package monokai-theme
  :ensure t
  :defer t)

(use-package modus-themes
  :ensure t
  :defer t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 52.37
        calendar-longitude 4.89
        circadian-themes '((:sunrise . leuven)
                           (:sunset . monokai)))
  (circadian-setup))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
