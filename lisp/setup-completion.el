;; Setup completion styles and frameworks

;; (use-package corfu
;;   :ensure t
;;   :bind
;;   (:map corfu-map
;; 		([tab] . corfu-next)
;; 		([backtab] . corfu-previous))
;;   :config
;;   (setq corfu-auto t
;; 		corfu-auto-delay 0
;; 		corfu-auto-prefix 0
;; 		corfu-quit-no-match t
;; 		corfu-cycle t
;; 		corfu-preselect-first t
;; 		corfu-bar-width 2
;; 		corfu-scroll-margin 5
;; 		corfu-left-margin-width 1
;; 		corfu-right-margin-width 1)
;;   (global-corfu-mode))

;; (use-package corfu-doc
;;   :ensure t
;;   :hook (corfu-mode . corfu-doc-mode))

;; (use-package cape
;;   :ensure t)

;; (setq-local completion-at-point-functions
;; 			(mapcar #'cape-company-to-capf
;; 					(list #'company-files #'company-ispell #'company-dabbrev)))

(use-package company
  :ensure t
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
  :after company
  :ensure t
  :config (setq company-box-doc-delay 0
				company-box-doc-enable t
				company-box-doc-frame-parameters '((internal-border-width . 1)))
  :hook (company-mode . company-box-mode))

;; (use-package company-quickhelp
;;   :ensure t
;;   :after company
;;   :config
;;   (setq company-quickhelp-delay 0)
;;   (setq-local blink-cursor-mode -1)
;;   :hook (company-mode . company-quickhelp-mode))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :config
;;   (setq kind-icon-default-face 'corfu-default)
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package vertico
  :ensure t
  :config
  (setq read-file-name-completion-ignore-case t
		read-buffer-completion-ignore-case t
		completion-ignore-case t
		vertico-resize t
		vertico-multiform-categories '((file reverse)
									   (consult-grep buffer)
									   (imenu buffer)))
  (vertico-mode)
  (vertico-multiform-mode))

(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)
(define-key vertico-map (kbd "TAB") #'minibuffer-complete)

(use-package marginalia
  :after vertico
  :ensure t
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
