(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(push '(tool-bar-lines . 0)       default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default delete-by-moving-to-trash t
              fill-column 80)

(setq use-short-answers t
      inhibit-startup-screen t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      scroll-conservatively 101
      scroll-margin 5)

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

(setq create-lockfiles nil
  make-backup-files nil)

(setq auto-save-default t
  auto-save-include-big-deletions t
  auto-save-list-file-prefix (concat user-emacs-directory "autosave/"))

(setq split-width-threshold 160
      split-height-threshold nil)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono Nerd Font"
                    :height 105
                    :weight 'normal)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t
      use-package-compute-statistics t)

(use-package evil
  :after undo-fu
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-backspace-join-lines t
        evil-respect-visual-line-mode t)
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "H" 'evil-beginning-of-visual-line)
  (evil-global-set-key 'motion "L" 'evil-end-of-visual-line)
  (evil-mode t))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(use-package undo-fu)

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-vimish-fold
  :hook (prog-mode . evil-vimish-fold-mode))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config (setq ace-window-display-mode t))

(use-package projectile
  :bind ("C-c p" . projectile-command-map))

(use-package which-key
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-add-column-padding 20
        which-key-idle-delay 0.2)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package vertico
  :bind (:map vertico-map
              (("C-j" . vertico-next)
               ("C-k" . vertico-previous)
               ("C-o" . vertico-next-group)))
  :config
  (setq vertico-count 25
        vertico-resize 'grow-only)
  (vertico-multiform-mode t)
  (vertico-mode t))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-l" . vertico-directory-delete-word)
              ("C-d" . vertico-directory-enter))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-prescient
  :config (vertico-prescient-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package consult
  :after (evil vertico)
  :bind (:map evil-normal-state-map
              ("C-s"   . consult-line)
              ("C-c i" . consult-imenu)
              ("/"     . consult-ripgrep))
  :config
  (setq consult-line-start-from-top t)
  (setq vertico-multiform-categories '((consult-grep buffer))))

(use-package consult-projectile
  :after projectile
  :bind (:map projectile-command-map
              ("h" . consult-projectile)))

(use-package corfu
  :config (setq corfu-auto-delay 0.1
                corfu-auto t
                corfu-cycle nil
                corfu-bar-width 0.4
                corfu-min-width 20
                corfu-left-margin-width 2
                corfu-right-margin-width 2)
  :hook (prog-mode . corfu-mode))

(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox-light t))

(use-package solaire-mode
  :after doom-themes
  :config (solaire-global-mode))

(use-package telephone-line
  :config
  (setq telephone-line-height 25
        telephone-line-evil-use-short-tag t
        telephone-line-primary-left-separator telephone-line-flat
        telephone-line-primary-right-separator telephone-line-flat
        telephone-line-secondary-left-separator telephone-line-flat
        telephone-line-secondary-right-separator telephone-line-flat)
  (telephone-line-mode))

(use-package hide-mode-line
  :defer t)

(use-package treemacs
  :bind (("<f8>" . treemacs)
         ("S-<f8>" . treemacs-select-window))
  :config
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors"
        doom-themes-treemacs-enable-variable-pitch nil))

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode t)

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title nil
        dashboard-center-content t
        dashboard-items '((recents . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode)
  :init
  (set-fringe-style 4))

(setq-default indent-tabs-mode nil
              tab-width 4)

(use-package editorconfig
  :config
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  (editorconfig-mode 1))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3
                display-line-numbers-widen 'grow-only))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook (lambda () (setq-local indicate-empty-lines t)))

(add-hook 'prog-mode-hook
          (lambda () (setq-local show-trailing-whitespace t)))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package column-enforce-mode
  :hook (prog-mode . column-enforce-mode))

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1.0
        flycheck-display-errors-delay 0.2))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package ruby-electric
  :hook (ruby-mode . ruby-electric-mode))

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

(use-package rvm
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

(use-package rspec-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package php-mode
  :defer t)

(use-package visual-fill-column
  :hook (text-mode . visual-fill-column-mode))

(setq-default fill-column 80)
(add-hook 'text-mode-hook #'visual-line-mode)

(use-package org
  :ensure nil
  :hook (org-mode . org-indent-mode)
  :config
  (require 'org-tempo))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉")))

(use-package ox-latex
  :after org
  :ensure nil
  :config
  (setq org-latex-listings 'minted
        org-latex-minted-options '(("linenos" "true")))
  (add-to-list 'org-latex-packages-alist '("" "minted" t ("pdflatex")) t))

(setq dired-auto-revert-buffer t
      dired-listing-switches "-la --group-directories-first")

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package vterm
  :commands (vterm vterm-other-window)
  :bind ("C-S-t" . vterm-other-window)
  :hook (vterm-mode . hide-mode-line-mode)
  :config
  (setq vterm-timer-delay 0
        vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (pdf-loader-install :noquery))
