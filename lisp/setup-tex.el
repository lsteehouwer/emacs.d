(require 'reftex)
(use-package tex
  :defer t
  :ensure auctex
  :init
  (setq-default TeX-master nil)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-auto-save t
        TeX-parse-self t
        TeX-source-correlate-mode t)
  :hook (TeX-mode . (lambda () (turn-on-reftex))))
(provide 'setup-tex)
