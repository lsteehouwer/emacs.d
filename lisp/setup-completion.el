;; Setup completion styles and frameworks

(use-package company
  :ensure t
  :diminish
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  (global-company-mode))

(defvar my-company-point nil)
(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common :after (lambda ()
                        (when (equal my-company-point (point))
                          (yas-expand))))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-doc-enable nil))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              (("DEL" . vertico-directory-delete-char)
               ("TAB" . minibuffer-complete)
               ("C-j" . vertico-next)
               ("C-k" . vertico-previous)))
  :config
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        vertico-resize nil
        vertico-multiform-categories '((file reverse)
                                       (consult-grep buffer)
                                       (imenu buffer))))
(vertico-mode)
(vertico-multiform-mode)

(use-package marginalia
  :after vertico
  :ensure t
  :bind (:map vertico-map (("DEL" . vertico-directory-delete-char)))
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :config
  (setq consult-narrow-key "<")
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any))
  :bind (("C-s" . consult-line)
         ("C-c i" . consult-imenu)))


(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(basic partial-completion emacs22)
        completion-category-overrides '((file (styles partial-completion))
                                        (consult-location (styles orderless)))))

(provide 'setup-completion)
;;; setup-completion.el ends here
