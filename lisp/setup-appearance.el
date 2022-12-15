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
  :hook (prog-mode-hook . display-line-numbers-mode)
  :config
  (setq display-line-numbers-widen nil))

(use-package hide-mode-line
  :commands hide-mode-line-mode)

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
  (setq dashboard-projects-switch-function 'projectile-switch-project-by-name
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
  ;; (telephone-line-defsegment telephone-line-time-segment ()
  ;;   (when (bound-and-true-p 'display-time-mode)
  ;;     (string-trim display-time-string)))
  ;; (add-to-ordered-list 'telephone-line-rhs '(nil telephone-line-time-segment))
  (setq telephone-line-evil-use-short-tag t
        telephone-line-height 28
        telephone-line-primary-left-separator    'telephone-line-flat
        telephone-line-secondary-left-separator  'telephone-line-flat
        telephone-line-primary-right-separator   'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat
        telephone-line-rhs '((nil telephone-line-flycheck-segment telephone-line-misc-info-segment)
                             (accent telephone-line-major-mode-segment)
                             (evil telephone-line-airline-position-segment)))
                             ;; (nil telephone-line-time-segment)))
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
  ;; :after modus-themes
  :init
  (setq modus-themes-fringes 'subtie
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-region 'bg-only
        modus-themes-syntax '(yellow-comments))
  (setq modus-themes-headings
        '((1 . (rainbow 1.0))
          (2 . (rainbow 1.0))
          (t . (rainbow)))
        modus-themes-scale-headings t)
  (setq calendar-latitude 52.37
        calendar-longitude 4.89
        circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(setq scroll-conservatively 101)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
