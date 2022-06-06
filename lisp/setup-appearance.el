;;; setup-appearance.el --- Setup the appearance of emacs -*- lexical-binding: t; -*-
;;; Commentary:

;; Does what is says on the tin

;;; Code:

(add-hook 'prog-mode-hook
          (lambda () (setq-local indicate-empty-lines t)))

(use-package frame
  :ensure nil
  :config
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode))

(use-package display-line-numbers
  :ensure nil
  :config
  (setq display-line-numbers-widen nil))

(use-package hide-mode-line
  :commands hide-mode-line-mode)

(use-package smooth-scrolling
  :config (smooth-scrolling-mode))

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
                      :height 120
                      :weight 'bold)
  (dashboard-setup-startup-hook))

(use-package telephone-line
  :demand t
  :config
  (setq telephone-line-evil-use-short-tag t
        telephone-line-height 28
        telephone-line-primary-left-separator    'telephone-line-cubed-left
        telephone-line-secondary-left-separator  'telephone-line-flat
        telephone-line-primary-right-separator   'telephone-line-cubed-left
        telephone-line-secondary-right-separator 'telephone-line-flat)
  (telephone-line-mode))

(use-package doom-themes
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
        circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . doom-gruvbox)))
  (circadian-setup))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
