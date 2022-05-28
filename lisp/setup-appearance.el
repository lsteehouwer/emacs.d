;; -*- lexical-binding: t; -*-

(blink-cursor-mode -1)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0
        highlight-indent-guides-method 'character))

(use-package rainbow-delimiters)

(use-package dashboard
  :hook (dashboard-mode . (lambda () (setq-local line-spacing 4)))
  :config
  (setq dashboard-set-navigator t
        dashboard-center-content t
        dashboard-items '((recents  . 10)
                          (projects . 10)))
  (set-face-attribute 'dashboard-items-face nil
                      :family "Roboto"
                      :height 120
                      :weight 'bold)
  (set-face-attribute 'dashboard-heading-face nil
                      :family "Roboto"
                      :height 140
                      :weight 'bold)
  (dashboard-setup-startup-hook))

(use-package telephone-line
  :config
  (setq telephone-line-evil-use-short-tag t
        telephone-line-height 28
        telephone-line-primary-left-separator    'telephone-line-flat
        telephone-line-secondary-left-separator  'telephone-line-flat
        telephone-line-primary-right-separator   'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat)
  (telephone-line-mode))

(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package all-the-icons
  :defer t)

(use-package circadian
  :init
  (setq calendar-latitude 52.37
        calendar-longitude 4.89
        circadian-themes '((:sunrise . doom-gruvbox)
                           (:sunset  . doom-gruvbox)))
  (circadian-setup))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
