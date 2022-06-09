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
                  (("C-j" . vertico-next)
                   ("C-k" . vertico-previous)
                   ("TAB" . minibuffer-complete)
                   ("<escape>" . keyboard-escape-quit)))
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
      (setq enable-recursive-minibuffers t))

    (use-package vertico-directory
      :after vertico
      :ensure nil
      :bind (:map vertico-map
                  ("RET" . vertico-directory-enter)
                  ("DEL" . vertico-directory-delete-char))
      :hook
      (rfn-eshadow-update-overlay . vertico-directory-tidy))

    (use-package vertico-reverse
      :ensure nil
      :bind (:map vertico-reverse-map
                  (("C-j" . vertico-previous)
                   ("C-k" . vertico-next))))

    (use-package marginalia
      :init
      (marginalia-mode)
      :config
      (defmacro pushnew! (place &rest values)
        "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
        (let ((var (make-symbol "result")))
          `(dolist (,var (list ,@values) (with-no-warnings ,place))
             (cl-pushnew ,var ,place :test #'equal))))
      (pushnew! marginalia-command-categories
                '(projectile-find-file              . project-file)
                '(projectile-recentf                . project-file)
                '(projectile-switch-to-buffer       . buffer)
                '(projectile-switch-project         . project-file)
                '(projectile-switch-project-by-name . project-file))
      (setq completion-category-overrides '((file (styles basic flex))
                                            (project-file (styles orderless))
                                            (consult-location (styles basic orderless)))))

    (use-package consult
      :config
      (setq consult-narrow-key "<")
      (consult-customize consult-theme :preview-key '(:debounce 0.2 any))
      :bind (("C-s" . consult-line)
             ("C-c i" . consult-imenu)))

    (use-package orderless)))

    ;;    ;;completion-styles '(orderless partial-completion flex)
    ;;    completion-category-overrides '((file (styles orderless partial-completion flex))
    ;;                                    (consult-location (styles basic orderless)))))))

(provide 'setup-completion)
;;; setup-completion.el ends here
