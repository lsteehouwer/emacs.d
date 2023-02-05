;; -*- lexical-binding: t -*-

;; Basic settings
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq-default fill-column 80
              display-fill-column-indicator 80)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(blink-cursor-mode -1)

(save-place-mode t)

(defalias #'yes-or-no-p #'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(require 'package)
;; (setq package-quickstart t)
(add-to-list `package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-compute-statistics t)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(require 'setup-gc)
(require 'setup-clutter)
(require 'setup-windowing)
(require 'setup-dired)
(require 'setup-evil)
(require 'setup-company)
(require 'setup-helm)
(require 'setup-ide)
(require 'setup-snippets)
(require 'setup-languages)
(require 'setup-org)
(require 'setup-tex)
(require 'setup-pdf)
(require 'setup-appearance)
(require 'setup-misc)

;; GENERAL SETTINGS
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 110
                    :weight 'bold)
(set-face-attribute 'fixed-pitch nil
                    :family "JetBrains Mono"
                    :height 110
                    :weight 'bold)
(set-face-attribute 'fixed-pitch-serif nil
                    :family "JetBrains Mono"
                    :height 110
                    :weight 'normal)
(set-face-attribute 'variable-pitch nil
                    :family "Liberation Sans"
                    :height 110
                    :weight 'normal)

(setq my-tab-width 4)
(setq-default indent-tabs-mode nil
              tab-width my-tab-width)
