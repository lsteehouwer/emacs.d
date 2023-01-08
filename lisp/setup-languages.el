;;; setup-lang.el --- Setup packages for different (programming) languages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package prog-mode
  :ensure nil
  :init (setq-default display-line-numbers-width 4
                      display-line-numbers-widen t)
  :hook (prog-mode . (lambda ()
                       (display-line-numbers-mode)
                       (electric-pair-local-mode)
                       (column-enforce-mode)
                       (rainbow-delimiters-mode))))

(use-package column-enforce-mode)

;; web mode
(use-package web-mode
  :mode (("\\.erb\\'"   . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.jsx?\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

;; ruby
(use-package ruby-electric
  :hook (ruby-mode . ruby-electric-mode))

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

(use-package rspec-mode
  :config (setq rspec-use-rvm t)
  :mode "\\.spec\\'"
  :bind ("C-c ." . rspec-find-spec-or-target-other-window))

(use-package rake
  :commands (rake rake-rerun rake-find-tasks))

(use-package rvm
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

(use-package robe
  :hook (ruby-mode . robe-mode))

(use-package haml-mode
  :config (message "loading haml")
  :mode "\\.haml\\'")

(use-package rubocop
  :config (setq rubocop-autocorrect-on-save t
                rubocop-format-on-save t)
  :hook (ruby-mode . rubocop-mode))

(use-package projectile-rails
  :after projectile
  :hook (projectile-mode . projectile-rails-mode)
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

;; c(++)
;; (use-package cc-mode
;;   :ensure nil
;;   :init
;;   (setq c-tab-width 4)
;;   (setq-default c-basic-offset c-tab-width))

;; terraform
(use-package terraform-mode)

;; meson build system
(use-package meson-mode
  :mode "/meson\\(\\.build\\|_options\\.txt\\)\\'")

;; haskell
(use-package haskell-mode
  :mode (("\\.[gh]s\\'" . haskell-mode)
         ("\\.hsc\\'" . haskell-mode)
         ("\\.l[gh]s\\'" . haskell-literate-mode)
         ("\\.hsig\\'" . haskell-mode)
         ("\\.[gh]s\\'" . haskell-mode)
         ("\\.cabal\\'\\|/cabal\\.project\\|/\\.cabal/config\\'" . haskell-cabal-mode)
         ("\\.chs\\'" . haskell-c2hs-mode)
         ("\\.ghci\\'" . ghci-script-mode)
         ("\\.dump-simpl\\'" . ghc-core-mode)
         ("\\.hcr\\'" . ghc-core-mode)))

;; (use-package lsp-haskell
;;   :ensure t
;;   :hook ((haskell-mode . lsp-deferred)
;;          (haskell-mode . interactive-haskell-mode)))

;; yaml
(use-package yaml-mode
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))

;; docker
(use-package dockerfile-mode
  :mode (("\\.dockerfile\\'" . dockerfile-mode)
         ("/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" . dockerfile-mode)))

(auto-insert-mode t)
(setq auto-insert-query nil
      auto-insert-directory "~/.emacs.d/auto-insert/"
      auto-insert-alist '((ruby-mode . "ruby.el")))

(provide 'setup-languages)
;;; setup-languages.el ends here
