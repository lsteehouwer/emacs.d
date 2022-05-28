;; web mode
(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)))

;; ruby
(use-package robe
  :hook (ruby-mode . robe-mode))

;; (use-package projectile-rails
;;   :after projectile
;;   :config (projectile-rails-global-mode))

(use-package projectile-rails)
  ;; :after projectile)

;; (define-key projectile-rails-mode-map (kbd "C-c r") 'hydra-projectile-rails/body)

(use-package rvm
  :defer t
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

(use-package ruby-electric
  :hook (ruby-mode . ruby-electric-mode))

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

;; c(++)
(use-package cc-mode
  :init
  (setq c-tab-width 4)
  (setq-default c-basic-offset c-tab-width))

;; meson build system
(use-package meson-mode)

;; haskell
(use-package haskell-mode)

;; (use-package lsp-haskell
;;   :ensure t
;;   :hook ((haskell-mode . lsp-deferred)
;;          (haskell-mode . interactive-haskell-mode)))

;; yaml
(use-package yaml-mode)

;; docker
(use-package dockerfile-mode)

(auto-insert-mode t)
(setq auto-insert-query nil
      auto-insert-directory "~/.emacs.d/auto-insert/"
      auto-insert-alist '((ruby-mode . "ruby.el")))

(provide 'setup-languages)
