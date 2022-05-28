;; (require 'ox-beamer)

(use-package org-ref
  :after org
  :hook org-mode
  :bind (:map org-mode-map
              ("C-c [" . org-ref-insert-link)))

(provide 'setup-org)
;;; setup-org.el ends here
