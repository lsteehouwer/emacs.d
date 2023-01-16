(use-package helm
  :hook (helm-mode . shackle-mode)
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-mini)
         ("C-s"     . helm-occur)
         ("C-c h"   . helm-command-prefix)
         ("C-c i"   . helm-semantic-or-imenu)
         :map helm-map
         ("<tab>"   . helm-execute-persistent-action)
         ("C-i"     . helm-execute-persistent-action)
         ("C-z"     . helm-select-action)
         ("C-j"     . helm-next-line)
         ("C-k"     . helm-previous-line)
         :map helm-find-files-map
         ("C-<backspace>" . helm-find-files-up-one-level))
  :config
  (setq helm-display-header-line nil
        helm-mode-line-string nil
        helm-input-idle-delay 0
        helm-move-to-line-cycle-in-source t
        helm-follow-mode-persistent t
        helm-display-function 'pop-to-buffer)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)))
  (helm-mode 1))

(use-package helm-flx
  :after helm
  :config
  (helm-flx-mode 1))

(use-package helm-ag
  :after helm
  :config
  (setq helm-ag-base-command "rg --no-heading"
        helm-ag-success-exit-status '(0 2))
  (evil-global-set-key 'motion "/" 'helm-do-ag-project-root))

(use-package helm-projectile
  :after helm
  :config (helm-projectile-on))

(use-package helm-themes
  :after helm)

;; (use-package helm-posframe
;;   :config
;;   (setq helm-posframe-size-function 'ls/helm-posframe-get-size
;;         helm-posframe-border-width 3
;;         helm-posframe-poshandler 'posframe-poshandler-frame-top-center
;;         helm-posframe-parameters '((left-fringe . 10)
;;                                    (right-fringe . 10)))
;;   (helm-posframe-enable))

;; (defun ls/helm-posframe-get-size ()
;;   "Custom function to size helm posframe."
;;   (list
;;    :width (or helm-posframe-width (round(* (frame-width) 0.62)))
;;    :height (or helm-posframe-height helm-display-buffer-height)
;;    :min-height (or helm-posframe-min-height
;;                    (let ((height (+ helm-display-buffer-height 1)))
;;                      (min height (or helm-posframe-height height))))
;;    :min-width (or helm-posframe-min-width
;;                   (let ((width (round (* (frame-width) 0.62))))
;;                     (min width (or helm-posframe-width width))))))

(provide 'setup-helm)
;;; setup-helm.el ends here.
