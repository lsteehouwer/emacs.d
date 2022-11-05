(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package adaptive-wrap)

;;; setup-text.el ends here
