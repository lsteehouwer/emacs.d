(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :ensure t
  :init (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :bind ("C-/" . 'evilnc-comment-or-uncomment-lines))

(use-package undo-fu
  :ensure t
  :bind (:map evil-normal-state-map
			  ("u" . 'undo-fu-only-undo)
			  ("C-r" . 'undo-fu-only-redo)))

(provide 'setup-evil)
