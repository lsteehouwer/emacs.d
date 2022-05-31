;; web mode
(setq my-html-indent 2)

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset my-html-indent))

(use-package emmet-mode
  :hook (web-mode . emmet-mode)
  :config
  (evil-define-key 'normal 'emmet-mode-keymap (kbd "C-n") #'emmet-next-edit-point)
  (evil-define-key 'normal 'emmet-mode-keymap (kbd "C-p") #'emmet-prev-edit-point)
  (evil-define-key 'normal 'emmet-mode-keymap (kbd "C-j") #'emmet-next-edit-point)
  (evil-define-key 'normal 'emmet-mode-keymap (kbd "C-k") #'emmet-prev-edit-point)
  (evil-define-key 'insert 'emmet-mode-keymap (kbd "C-n") #'emmet-next-edit-point)
  (evil-define-key 'insert 'emmet-mode-keymap (kbd "C-p") #'emmet-prev-edit-point)
  (evil-define-key 'insert 'emmet-mode-keymap (kbd "C-j") #'emmet-next-edit-point)
  (evil-define-key 'insert 'emmet-mode-keymap (kbd "C-k") #'emmet-prev-edit-point)
  (evil-define-key 'normal 'emmet-mode-keymap (kbd "C-M-j") #'emmet-expand-line)
  (evil-define-key 'insert 'emmet-mode-keymap (kbd "C-M-j") #'emmet-expand-line)
  (setq emmet-indentation my-html-indent
        emmet-move-cursor-between-quotes t))

(use-package haml-mode)

;; ruby
(use-package robe
  :hook (ruby-mode . robe-mode))

(use-package rubocop
  :config (setq rubocop-autocorrect-on-save t
                rubocop-format-on-save t)
  :hook (ruby-mode . rubocop-mode))

(use-package projectile-rails
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)
  (projectile-rails-global-mode))

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

;; markdown
(use-package grip-mode
  :hook (markdown-mode . grip-mode))

(provide 'setup-languages)
;;; setup-languages.el ends here
