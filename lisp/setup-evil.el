(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-minibuffer nil
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "/" 'consult-ripgrep)
  (evil-mode t))

(use-package evil-collection
  :after evil
  :init (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :bind ("C-/" . 'evilnc-comment-or-uncomment-lines))

(use-package ace-window
  :after evil
  :bind ("M-o" . ace-window)
  :config (setq ace-window-display-mode t))

(use-package undo-fu
  :bind (:map evil-normal-state-map
              ("u"   . 'undo-fu-only-undo)
              ("C-r" . 'undo-fu-only-redo)))

(provide 'setup-evil)
;;; setup-evil.el ends here
