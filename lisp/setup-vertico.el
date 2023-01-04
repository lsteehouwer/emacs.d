(use-package vertico
  :bind (:map vertico-map
              (("C-j" . vertico-next)
               ("C-k" . vertico-prev)
               ("TAB" . minibuffer-complete)
               ("<escape>" . keyboard-escape-quit)))
  :init
  (vertico-mode t)
  (vertico-multiform-mode t)
  :config
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        enable-recursive-minibuffers t
        vertico-resize nil
        vertico-multiform-categories '((file reverse)
                                       (consult-grep buffer)
                                       (imenu buffer))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-reverse
  :after vertico
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

(use-package orderless)

(provide 'setup-vertico)
