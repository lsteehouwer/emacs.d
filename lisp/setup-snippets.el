(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-active-extra-mode
             yas-deactive-extra-mode
             yas-maybe-expand-abbrev-key-filter))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(provide 'setup-snippets)
;;; setup-snippets.el ends here
