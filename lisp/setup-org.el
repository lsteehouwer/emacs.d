(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-agenda-files '("~/Documents/.org")
        org-log-done 'time
        org-return-follows-link t
        org-hide-emphasis-markers nil
        org-latex-pdf-process
             '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	           "bibtex %b"
	           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-babel-do-load-languages
   'org-babel-load-languages '((ruby . t)
                               (shell . t)))
  (require 'org-tempo))

(use-package org-ref
  :after org)

(use-package helm-bibtex
  :after org-ref)

(use-package visual-fill-column
  :hook ((org-mode . visual-fill-column-mode)))

(use-package org-superstar
  :hook ((org-mode . org-superstar-mode))
  :config
  (setq org-superstar-headline-bullets-list '("◉")))

(use-package ox-latex
  :after org
  :ensure nil
  :config
  (setq org-latex-listings 'minted
        org-latex-minted-options '(("linenos" "true")))
  (add-to-list 'org-latex-packages-alist '("" "minted" t ("pdflatex")) t))

(provide 'setup-org)
;;; setup-org.el ends here
