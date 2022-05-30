;; dired settings
(setq delete-by-moving-to-trash t)

;; remove trailing whitespace automatically on save
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-add-column-padding 10
        which-key-idle-delay 0.2)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package olivetti
  :commands olivetti-mode)

(use-package hydra)

(provide 'setup-misc)
