;; -*- lexical-binding: t -*-

(setq initial-major-mode nil)

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

(require 'setup-gc)
(require 'setup-clutter)
(require 'setup-dired)
(require 'setup-evil)
(require 'setup-completion)
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
                    :family "Ubuntu Mono"
                    :height 120
                    :weight 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "Ubuntu Mono"
                    :height 120
                    :weight 'normal)
(set-face-attribute 'variable-pitch nil
                    :family "Ubuntu"
                    :height 110
                    :weight 'normal)

(setq my-tab-width 4)
(setq-default indent-tabs-mode nil
              tab-width my-tab-width)
