(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install :noquery))
(add-hook 'pdf-view-mode blink-cursor-mode -1)

(provide 'setup-pdf)
