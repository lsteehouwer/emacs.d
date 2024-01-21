(defvar ls/cache-dir (concat user-emacs-directory ".cache/")
  "Directory used for cached files")

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" ls/cache-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Activate GCMH as soon as possible to reap its benefits as soon as possible
(elpaca gcmh
  (gcmh-mode +1))

;; Install use-package and configure it
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t)
  (setq use-package-always-defer t
        use-package-expand-minimally nil
        use-package-compute-statistics nil))

;; Use general for registering key bindings
(elpaca general
  (general-evil-setup)
  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

(elpaca-wait)

;; (use-package esup)

;; (use-package benchmark-init
;;   :defer nil
;;   :ensure t
;;   :init (benchmark-init/activate)
;;   :hook (after-init-hook . benchmark-init/deactivate))

(use-package emacs
  :elpaca nil
  :init
  ;; Some performance improvements
  (setq initial-scratch-message nil
        auto-mode-case-fold nil
        bidi-inhibit-bpa t
        highlight-nonselected-windows nil
        fast-but-imprecise-scrolling t
        idle-update-delay 1.0
        inhibit-compacting-font-caches t
        redisplay-skip-fontification-on-input t
        read-process-output-max (* 256 1024)
        use-file-dialog nil)
  ;; Tell me when you're collecting garbage so I can keep an eye on it
  (setq garbage-collection-messages t)
  ;; Don't get in my way, Emacs
  (defalias 'yes-or-no-p 'y-or-n-p)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (setq byte-compile-warnings nil
        ring-bell-function #'ignore)
  ;; Disable both backup and lock files
  (setq create-lockfiles nil
        make-backup-files nil)
  ;; Don't clutter whatever directory I'm working in with autosave files
  (setq auto-save-default t
        auto-save-include-big-deletions t
        auto-save-list-file-prefix (concat ls/cache-dir "autosave/"))
  ;; Delete by moving to trash.
  (setq delete-by-moving-to-trash t)
  ;; UTF-8 everywhere please
  (set-language-environment "UTF-8")
  (setq default-input-method nil)
  ;; Smooth scrolling behavior
  (setq scroll-conservatively 101
        scroll-margin 5
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
  ;; Set column width to 80. Archaic but still the de-facto standard
  (setq-default fill-column 80)
  ;; Single space after a sentence end, please.
  (setq-default sentence-end-double-space nil)
  ;; Use spaces not tabs, but if tabs are required show them as 2
  ;; spaces wide
  (setq-default indent-tabs-mode nil
		            tab-width 2)
  ;; Font settings
  (set-face-attribute 'default nil
                      :weight 'normal
                      :width  'normal
                      :slant  'normal
                      :font (font-spec :family "Commit Mono"
                                       :size 10.5
                                       :weight 'normal))
  (set-face-attribute 'variable-pitch nil
                      :weight 'normal
                      :width  'normal
                      :slant  'normal
                      :font (font-spec :family "Liberation Sans"
                                       :size 11))
  ;; Functions for use in hooks (below)
  (defun ls/trim-trailing-newlines ()
    "Remove empty lines at the end of the buffer.

Function lifted from Doom Emacs."
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (delete-blank-lines)))

  (defun ls/indicate-trailing-whitespace ()
    "Indicate trailing whitespace"
    (interactive)
    (setq-local show-trailing-whitespace t))

  (defun ls/indicate-empty-lines ()
    "Place a symbol in the fringe to indicate empty lines at the
bottom of the buffer"
    (interactive)
    (setq-local indicate-empty-lines t))

  :hook ((before-save . ls/trim-trailing-newlines)
         (prog-mode   . ls/indicate-trailing-whitespace)
         (prog-mode   . ls/indicate-empty-lines))
  :general
  (leader-keys
    "<escape>" '(keyboard-escape-quit :which-key t)))

;; Startup tweaks
(use-package startup
  :elpaca nil
  :init
  (setq initial-major-mode 'fundamental-mode
        inhibit-splash-screen t)
  ;; On Emacs versions supporting native compilation, place the compiled files
  ;; in the cache dir
  (when (boundp 'native-comp-eln-load-path)
    (add-to-list 'native-comp-eln-load-path
                 (expand-file-name "eln" ls/cache-dir))))

(use-package simple
  :elpaca nil
  :init (column-number-mode +1))

;; FILES AND PROJECTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package files
  :elpaca nil
  :general
  (leader-keys
    "f"   '(:ignore t :which-key "find")
    "f f" '(find-file :which-key "find file")
    "f F" '(find-file-other-window :which-key "find file other window")
    "f d" '(dired :which-key "find dir")
    "f D" '(dired-other-window :which-key "find dir other window")
    "f e" '(ls/edit-emacs-init :which-key "emacs config"))
  :init
  (defun ls/edit-emacs-init ()
    "Edit emacs configuration"
    (interactive)
    (find-file (concat user-emacs-directory "init.el"))))

;; I never use custom, but in the off chance that I do need it, don't clutter
;; my init.el
(use-package cus-edit
  :elpaca nil
  :init
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package project
  :elpaca nil
  :init
  (setq project-list-file (concat ls/cache-dir "projects")
        project-switch-commands #'project-dired)
  :general
  (leader-keys
    :states 'normal
    "p"          '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p"        '(project-switch-project :which-key "switch project")
    "p b"        '(project-switch-to-buffer :which-key "switch buffer")
    "p f"        '(project-find-file :which-key "find file")
    "p d"        '(project-find-dir :which-key "find dir")
    "p k"        '(project-kill-buffers :which-key "kill all project buffers")))

;; FRAMES AND WINDOWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package frame
  :elpaca nil
  :init
  ;; blinking cursors only distract
  (blink-cursor-mode -1)
  (setq frame-inhibit-implied-resize t)) ;; performance improvement

(use-package window
  :elpaca nil
  :init
  (set-window-fringes nil 16 16 nil t)
  (setq split-width-threshold 160
        split-height-threshold nil))

;; Setup i3-like key bindings. I prefer a master-stack layout like in DWM, but
;; the only package I know of that achieves this, edwina, does not integrate
;; well with other packages that open and close windows. So, i3 it is.
(use-package windmove
  :elpaca nil
  :init
  (cl-defun ls/setup-i3-keys (&key (keymaps 'global) (states 'normal))
    (general-define-key
     :keymaps keymaps
     :states states
     "M-RET"        #'split-window-right
     "M-<return>"   #'split-window-right
     "M-<S-RET>"    #'split-window-below
     "M-<S-return>" #'split-window-below
     "M-h"          #'windmove-left
     "M-j"          #'windmove-down
     "M-k"          #'windmove-up
     "M-l"          #'windmove-right
     "M-Q"          #'delete-window))
  (ls/setup-i3-keys))

;; EDITOR BEHAVIOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vim emulation
(use-package evil
  :demand
  :general
  (general-define-key
    :states 'motion
    "H" #'ls/beginning-of-line-dwim
    "L" #'evil-end-of-visual-line)
  :init
  (defun ls/beginning-of-line-dwim ()
    "Move to the first text character on the line, or when already
there the start of the visual line"
    (interactive)
    (let ((starting-point (point)))
      (evil-first-non-blank-of-visual-line)
      (when (<= starting-point (point))
        (evil-beginning-of-visual-line))))

  (setq evil-want-keybinding nil
        evil-backspace-join-lines t
        evil-respect-visual-line-mode t)
  (when (not (fboundp 'undo-redo))
    (setq evil-undo-system 'undo-fu))
  (evil-mode 1))

(use-package undo-fu
  :unless (fboundp 'undo-redo))

;; Evil bindings in modes not provided by standard evil
(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))

;; Show the number of search candidates in the mini bufer
(use-package evil-anzu
  :after evil
  :demand
  :init (global-anzu-mode))

;; Comment out lines the evil way
(use-package evil-nerd-commenter
  :general
  (general-nvmap
    "C-/" 'evilnc-comment-operator))

;; Snippets
(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

;; Highlight the current line
(use-package hl-line
  :elpaca nil
  :hook (prog-mode . hl-line-mode))

;; Highlight TODO, HACK, FIXME, etc.
(use-package hl-todo
  :init (global-hl-todo-mode))

;; Cleanup trailing whitespace
(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :init
  (defun ls/ws-butler-clean-buffer ()
    "Cleanup all trailing whitespace in a buffer"
    (interactive)
    (ws-butler-clean-region (point-min) (point-max)))
  (setq ws-butler-keep-whitespace-before-point nil))

;; Highlight characters past the fill-column mark
(use-package column-enforce-mode
  :hook (prog-mode . column-enforce-mode))

;; Auto update buffers when other programs modify them
(use-package autorevert
  :elpaca nil
  :init
  (setq auto-revert-verbose nil)
  :hook (after-init . global-auto-revert-mode))

;; Don't slow to a crawl when loading files with very long lines, e.g. minified
;; javascript
(use-package so-long
  :elpaca nil
  :hook (after-init . global-so-long-mode))

;; COMPLETIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A better IComplete vertical
(use-package vertico
  :elpaca (:files ("*.el" "extensions/*.el"))
  :bind (:map vertico-map
              (("C-j" . vertico-next)
               ("C-k" . vertico-previous)
               ("C-o" . vertico-next-group)
               ("C-d" . vertico-directory-enter)
               ("C-l" . vertico-directory-up)))
  :init
  (setq vertico-count 25
        vertico-resize 'grow-only)
  (vertico-multiform-mode +1)
  (vertico-mode +1)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Fuzzy matching
(use-package vertico-prescient
  :after vertico
  :demand
  :init (vertico-prescient-mode))

;; A bunch of handy commands that integrate well with vertico
(use-package consult
  :after (vertico evil)
  :general
  (general-nmap
    "C-c i" #'consult-imenu
    "C-s"   #'consult-ripgrep
    "C-x b" #'consult-buffer)
  (leader-keys
    :states 'normal
    "o b" '(consult-buffer :which-key "open buffer")
    "o B" '(consult-buffer-other-window :which-key "open buffer other window"))
  :init
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)))
  (setq consult-preview-partial-size 0))

;; Quickly switch between all files and open buffers within a project
(use-package consult-project-extra
  :general
  (leader-keys
    :states 'normal
    "SPC" '(consult-project-extra-find :which-key "quick switch")
    "p h" '(consult-project-extra-find :which-key "quick switch")))

;; Add meta information to mini buffer completion candidates
(use-package marginalia
  :after vertico
  :demand
  :init (marginalia-mode))

;; In-buffer completions
(use-package company
  :hook (prog-mode . company-mode)
  :init (setq company-idle-delay 0.05
              company-minimum-prefix-length 3))

;; TOOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Who needs a graphical file explorer?
(use-package dired
  :elpaca nil
  :general
  (leader-keys
    "d" '(:ignore t :which-key "dired")
    "d d" '(dired-jump :which-key "open current dir")
    "d D" '(dired-jump-other-window :which-key "open current dir other window"))
  :init (setq dired-auto-revert-buffer t
              dired-dwim-target t
              dired-listing-switches "-la --group-directories-first"))

;; Spice up dired just a little bit
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; VTERM is probably still the best terminal emulator available in Emacs right
;; now, but eat seems to be making ways. Perhaps in the future I'll give it a
;; shot. In the meantime I've had to pin vterm to a specific version otherwise
;; it will not cooperate with solaire mode.
(use-package vterm
  :hook (vterm-mode . hide-mode-line-mode)
  :elpaca (:pin t :ref "94e2b0b2b4a750e7907dacd5b4c0584900846dd1")
  :init
  (setq vterm-timer-delay 0.0
        vterm-max-scrollback 50000
        vterm-always-compile-module t
        vterm-kill-buffer-on-exit t
        confirm-kill-processes nil)
  :config
  (ls/setup-i3-keys :keymaps 'vterm-mode-map
                    :states '(normal insert)))

(use-package vterm-toggle
  :general
  (leader-keys
    "o t" '(vterm-toggle :which-key "terminal")))

;; FIXME: Some magic so we can build Magit
;; SEE: https://github.com/progfolio/elpaca/issues/216
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq :elpaca `(seq :build ,(+elpaca-seq-build-steps)))

;; Magit is the best git client. Full stop.
(use-package magit
  :init
  (setq transient-levels-file (concat ls/cache-dir  "levels.el")
        transient-values-file (concat ls/cache-dir  "values.el")
        transient-history-file (concat ls/cache-dir "history.el"))
  :general
  (leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log"))
  (general-nmap
    "<escape>" #'transient-quit-one))

(use-package which-key
  :demand
  :config
  (setq which-key-idle-delay 1)
  (which-key-mode))

(use-package helpful
  :general
  (leader-keys
    "h"   '(:ignore t :which-key "help")
    "h f" '(helpful-callable :which-key "callable")
    "h v" '(helpful-variable :which-key "variable")
    "h k" '(helpful-key :which-key "key")
    "h x" '(helpful-command :which-key "command")
    "h d" '(helpful-at-point :which-key "at point")))

;; Replace doc view with pdf tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (pdf-loader-install :noquery))

(use-package editorconfig
  :init
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  (editorconfig-mode +1))

;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These UI elements were already disabled in early-init.el, effectively
;; preventing them from ever showing up. However, Emacs' internal state still
;; says these UI elements are active. By disabling these modes now we rectify
;; this state.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers in programming environments please
(use-package display-line-numbers
  :elpaca nil
  :hook ((prog-mode yaml-mode) . display-line-numbers-mode)
  :init
  (setq display-line-numbers-grow-only t
        display-line-numbers-width-start t))

;; A collection of nice themes
(use-package doom-themes
  :init
  (load-theme 'doom-molokai t))

;; But the modus themes are also very nice
(use-package modus-themes
  ;; Use the remote one over the built-in one since it's more up to date, and
  ;; makes the themes available to older Emacs versions.
  :elpaca (:repo "https://github.com/protesilaos/modus-themes")
  :init
  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)
          (bg-line-number-active bg-hl-line)
          (fg-line-number-inactive fg-dim)
          (bg-line-number-inactive unspecified))))

;; Give "lesser" buffers like *scratch*, terms, and dired a different background
;; to differentiate them from "normal" buffers.
(use-package solaire-mode
  :init
  (solaire-global-mode +1))

;; In some modes it makes no sense to show a mode line. This package provides
;; that capability
(use-package hide-mode-line)

;; Hide all of the various minor modes behind a simple menu in the mode line
(use-package minions
  :commands minions-mode
  :init (minions-mode +1))

;; Some packages need icons. Here they are
(use-package nerd-icons)
(use-package all-the-icons)

;; Pulse the current line upon certain operations, particularly large move
;; operations and window switching
(use-package pulsar
  :demand
  :config
  (setq pulsar-delay 0.05
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta)
  (pulsar-global-mode))

;; LANG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recent versions of Emacs come with tree sitter support built in. This is
;; nice, but I'm not too happy about how much manual action it takes to install
;; language grammars. The function below improves the situation quite a bit, but
;; it's still not quite where I want it to be.
(use-package treesit
  :elpaca nil
  :when (fboundp 'treesit-install-language-grammar)
  :init
  (defun ls/treesit-install-language (lang)
    "Install treesitter language LANG

The built-in treesitter package has the capability of outputting
compiled files to directories other than \"tree-sitter\", but
unfortunately it does not expose this via any configuration
option. I'd rather put up with it for now, than maintaining my
own version of treesitter."
    (require 'treesit)
    (let* ((url    (concat "https://github.com/tree-sitter/tree-sitter-" (symbol-name lang)))
           (record (list lang url)))
      (when (not (member record treesit-language-source-alist))
        (push record treesit-language-source-alist)))
    (when (not (treesit-language-available-p lang))
      (treesit-install-language-grammar lang))))

;; Ruby
(use-package ruby-ts-mode
  :elpaca nil
  :when (fboundp 'treesit-install-language-grammar)
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :config
  (ls/treesit-install-language 'ruby))

(use-package yard-mode
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))

(use-package rubocop
  :hook ((ruby-mode ruby-ts-mode) . rubocop-mode))

(use-package rvm
  :hook ((ruby-mode ruby-ts-mode) . rvm-activate-corresponding-ruby))

(use-package rspec-mode)

(use-package haml-mode)

;; Yaml
(use-package yaml-mode)

;; Coffeescript
(use-package coffee-mode)

;; Docker
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; Terraform
(use-package terraform-mode)

(use-package groovy-mode)

(use-package markdown-mode)

;; Org
(use-package org
  :elpaca nil
  :hook (org-mode . org-indent-mode)
  :config
  (require 'org-tempo))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉")))

(use-package ox-latex
  :elpaca nil
  :config
  (setq org-latex-listings 'minted
        org-latex-minted-options '(("linenos" "true")))
  (add-to-list 'org-latex-packages-alist '("" "minted" t ("pdflatex")) t))
