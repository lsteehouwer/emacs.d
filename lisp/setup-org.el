(use-package org
  :ensure nil
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-agenda-files '("~/org")
        org-log-done 'time
        org-return-follows-link t
        org-hide-emphasis-markers nil)
  (require 'org-tempo))

;; (custom-theme-set-faces
;;  'user
;;  'org-table ((t (:inherit))))

(use-package visual-fill-column
  :hook ((org-mode . visual-fill-column-mode)))

(use-package org-superstar
  :hook ((org-mode . org-superstar-mode))
  :config
  (setq org-superstar-headline-bullets-list '("◉")))

(provide 'setup-org)
;;; setup-org.el ends here
