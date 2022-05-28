;; Setup completion styles and frameworks

(savehist-mode)

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1))

;; (defvar my-company-point nil)
;; (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
;; (advice-add 'company-complete-common :after (lambda ()
;;                         (when (equal my-company-point (point))
;;                           (yas-expand))))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-doc-enable nil))

(if (< emacs-major-version 27)
    (progn
      (use-package ivy
        :bind (:map ivy-minibuffer-map
                    (("C-j" . ivy-next-line)
                     ("C-k" . ivy-previous-line)))
        :config (ivy-mode))

      (use-package counsel
        :config (counsel-mode))

      (use-package swiper
        :bind ("C-s" . swiper)))
  (progn
    (use-package vertico
      :bind (:map vertico-map
                  (("DEL" . vertico-directory-delete-char)
                   ("TAB" . minibuffer-complete)
                   ("C-j" . vertico-next)
                   ("C-k" . vertico-previous)))
      :custom
      (read-file-name-completion-ignore-case t)
      (read-buffer-completion-ignore-case t)
      (completion-ignore-case t)
      (vertico-resize nil)
      (vertico-multiform-categories '((file reverse)
                                      (consult-grep buffer)
                                      (imenu buffer)))
      :init
      (vertico-mode t)
      (vertico-multiform-mode t)
      :config
      (evil-define-key 'insert vertico-map (kbd "<escape>") #'keyboard-escape-quit)
      (evil-define-key 'insert vertico-map (kbd "C-k") #'vertico-previous)
      :hook
      (rfn-eshadow-update-overlay . vertico-directory-tidy))

    (use-package marginalia
      :init
      (marginalia-mode))

    (use-package consult
      :config
      (setq consult-narrow-key "<")
      (consult-customize consult-theme :preview-key '(:debounce 0.2 any))
      :bind (("C-s" . consult-line)
             ("C-c i" . consult-imenu)))

    (use-package orderless
      :config
      (setq completion-styles '(basic partial-completion emacs22)
            completion-category-overrides '((file (styles partial-completion))
                                            (consult-location (styles orderless)))))))

(provide 'setup-completion)
;;; setup-completion.el ends here
