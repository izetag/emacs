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
(use-package rtags
  :ensure t
  :defer t)

(use-package clang-format
  :ensure t
  :defer t)

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
    (lambda ()
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
        ;; Continue to save.
        nil))
    nil
    ;; Buffer local hook.
    t))

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c++-mode-hook 'flycheck-mode)

;; (require 'rtags)
;; (require 'rtags-helm)
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)
;; (setq rtags-use-helm t)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


(c-add-style "ya-style"
             '("k&r"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4),
               (c-continued-statement-offset . +)
               (c-offsets-alist
                (arglist-intro . +)
                (arglist-cont . 0)
                (arglist-cont-nonempty . +)
                (arglist-close . c-lineup-close-paren)
                (inline-open . 0))))
