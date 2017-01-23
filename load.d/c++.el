(use-package irony
  :ensure t
  :defer t)
(use-package company
  :ensure t
  :defer t
  :config (global-company-mode))
(use-package company-irony
  :ensure t
  :defer t
  :config (eval-after-load 'company
            '(add-to-list 'company-backends 'company-irony)))

(use-package flycheck-irony
  :ensure t
  :defer t
  :config (eval-after-load 'flycheck
            '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

