;; -*- lexical-binding: t -*-

;; Basic settings
(defun dir-concat (dir file) (concat (file-name-as-directory dir) file))

(push (dir-concat user-emacs-directory "lisp/") load-path)
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

(setq dired-auto-revert-buffer t
	  dired-listing-switches "-la --group-directories-first")

(require 'package)
(add-to-list `package-archives
			 '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(set-fringe-style '(nil . 0))
(set-face-attribute 'fringe nil :background (face-background 'line-number))

(set-face-attribute 'default nil
					:family "JetBrains Mono"
					:height 110
					:weight 'normal)
(set-face-attribute 'fixed-pitch nil
					:family "JetBrains Mono"
					:height 110
					:weight 'normal)

(add-hook 'prog-mode-hook
		  (lambda ()
			(display-line-numbers-mode t)
			(electric-pair-mode t)
			(rainbow-delimiters-mode t)))

(setq my-tab-width 4)
(setq-default indent-tabs-mode t
			  tab-width my-tab-width)
