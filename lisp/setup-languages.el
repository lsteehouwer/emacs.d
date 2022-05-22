;; ruby
(use-package projectile-rails
  :after projectile
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package rvm
  :ensure t
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

;; (add-hook 'ruby-mode-hook
;;           (lambda () (rvm-activate-corresponding-ruby)))

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

;; meson build system
(use-package meson-mode
  :ensure t)

;; python
(setq python-tab-width 4)
(use-package python
  :config (setq-default python-indent-offset python-tab-width))

;; haskell
(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :hook ((haskell-mode . lsp-deferred)
         (haskell-mode . interactive-haskell-mode)))

;; yaml
(use-package yaml-mode
  :ensure t)

;; docker
(use-package dockerfile-mode
  :ensure t)

(auto-insert-mode t)
(setq auto-insert-query nil
      auto-insert-directory "~/.emacs.d/auto-insert/"
      auto-insert-alist '((ruby-mode . "ruby.el")))

(provide 'setup-languages)
