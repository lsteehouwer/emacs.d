(require 'ox-beamer)

(use-package org-ref
  :after org
  :ensure t
  :hook org-mode
  :bind (:map org-mode-map
			  ("C-c [" . org-ref-insert-link)))

(provide 'setup-org)
