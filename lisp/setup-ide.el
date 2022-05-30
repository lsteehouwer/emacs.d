(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode                  .   lsp-enable-which-key-integration)
         (c-mode                    .   lsp-deferred)
         (c++-mode                  .   lsp-deferred)
         (python-mode               .   lsp-deferred)
         (haskell-mode              .   lsp-deferred)
         (haskell-interactive-mode  .   lsp-deferred)
         (dockerfile-mode           .   lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-clients-clangd-args '("--compile-commands-dir=debug"))
  :config
  (setq lsp-idle-delay 0
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-signature-auto-activate nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-signature-render-documentation t
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 0
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 70
        lsp-eldoc-enable-hover nil)
  :bind ((:map evil-normal-state-map
               ("gd" . 'lsp-ui-peek-find-definitions)
               ("gr" . 'lsp-ui-peek-find-references))
         (:map lsp-ui-peek-mode-map
               ("j" . 'lsp-ui-peek--select-next)
               ("k" . 'lsp-ui-peek--select-prev))))

(use-package flycheck
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-pos-tip
  :config
  (setq flycheck-pos-tip-max-width 90)
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :config
  (setq treemacs-follow-after-init t
        treemacs-follow-mode -1
        treemacs-is-never-other-window t
        treemacs-user-mode-line-format 'none
        treemacs-position 'left
        treemacs-width 35)
  :bind (("<f8>" . treemacs)
         ("S-<f8>" . treemacs-select-window)))

(use-package treemacs-evil)

(use-package treemacs-projectile
  :after treemacs)

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :defer t
  :config (lsp-treemacs-sync-mode t))

(use-package projectile
  :config (projectile-mode t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package magit
  :commands magit)

(use-package vterm
  :bind ("C-S-t" . vterm-other-window)
  :hook (vterm-mode . hide-mode-line-mode)
  :config
  (setq vterm-timer-delay 0
        vterm-kill-buffer-on-exit t))

(provide 'setup-ide)
;;; setup-ide.el ends here
