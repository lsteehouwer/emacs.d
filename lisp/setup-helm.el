(use-package helm
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
         :map helm-find-files-map
         ("C-<backspace>" . helm-find-files-up-one-level))
  :config
  (setq helm-input-idle-delay 0
        helm-split-window-inside-p nil
        helm-split-window-default-side 'other
        helm-split-window-other-side-when-one-window 'right
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t)
  (helm-mode 1))

(use-package helm-flx
  :after helm
  :config
  (helm-flx-mode 1))

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
