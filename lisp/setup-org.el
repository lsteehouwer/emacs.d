;; (use-package org-superstar)

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . visual-fill-column-mode)
         (org-mode . org-indent-mode))
  :config
  (require 'org-tempo))

;; (require 'ox-beamer)

;; (use-package org-ref
;;   :after org
;;   :hook org-mode
;;   :bind (:map org-mode-map
;;               ("C-c [" . org-ref-insert-link)))

(provide 'setup-org)
;;; setup-org.el ends here
