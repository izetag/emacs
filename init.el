(setq use-package-verbose t)
(setq nsm-settings-file "~/.emacs.local.d/var/nsm-settings.el")
(setq package-user-dir "~/.emacs.local.d/elpa")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

;; increasing boot speed
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
  (add-hook 'after-init-hook
            (lambda () (message "loaded in %s" (emacs-init-time))))

(setq old-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold old-gc-cons-threshold)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

(defconst my-custom-file "~/.emacs.d/custom.el")
(setq custom-file my-custom-file)
(load custom-file)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 14)
   (auto-package-update-maybe))

;; helm
(use-package helm
  :config
  (require 'helm-config)
  (setq helm-mode-fuzzy-match t)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (helm-autoresize-mode t)
  (helm-mode +1)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-r" . helm-recentf))

;; no-littering
(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq vc-handled-backends nil)
(if (display-graphic-p)
    (progn
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (set-scroll-bar-mode 'right)))

(load-theme 'misterioso t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Setting custom font
;; Click [here](https://github.com/hbin/dotfiles-for-emacs) to take a further look.
(set-frame-font "DejaVu Sans Mono:pixelsize=14")

;; If you use Emacs Daemon mode
(add-to-list 'default-frame-alist
               (cons 'font "DejaVu Sans Mono:pixelsize=14"))

(setq tramp-verbose 10)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;; if we're on graphic display
(column-number-mode t)
(if (load "mwheel" t)
    (mwheel-install))

(setq backup-by-copying-when-linked t)
(setq backup-by-copying-when-mismatch t)
(setq load-dirs t)

(setq backup-by-copying-when-linked t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; to allow scaling text without scaling line numbers since they get truncated otherwise
(require 'linum)
(add-hook 'prog-mode-hook #'linum-on)
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

;; auto-package-update
(require 'auto-package-update)
(auto-package-update-maybe)
(auto-package-update-at-time "03:00")
(setq auto-package-update-prompt-before-update t)
(setq auto-package-update-delete-old-versions t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
;; recent list
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; environment variabes from shell on mac
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;(require 'helm-smex)
;(global-set-key [remap execute-extended-command] #'helm-smex)
;(global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)

(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(require 'key-chord)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (key-chord-mode t)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(global-set-key (kbd "C-s-<left>") 'previous-buffer)
(global-set-key (kbd "C-s-<right>") 'next-buffer)
(global-set-key (kbd "C-s-<down>") 'ff-find-other-file)
(global-set-key (kbd "C-s-<up>") 'ff-find-other-file)
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))


(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 120)
(setq delete-trailing-lines nil)
(global-whitespace-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'backward-word 'forward-word arg)
       ;;(paste-to-mark arg)
     )
(global-set-key (kbd "C-c w") (quote copy-word))

(setq create-lockfiles nil)
;; load local config if exists
(let ((local-settings "~/.emacs.local"))
  (if (file-exists-p local-settings)
  (load-file local-settings)))


(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (add-to-list 'projectile-globally-ignored-directories "remote"))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package tramp
  :ensure nil
  :config (progn
            (setq tramp-use-ssh-controlmaster-options nil
                  tramp-verbose 1
                  vc-ignore-dir-regexp (format "%s\\|%s"
                                               vc-ignore-dir-regexp
                                               tramp-file-name-regexp))))
(use-package helm-tramp
  :after (helm tramp)
  :init
  (setq tramp-default-method "ssh")
  :hook (helm-tramp-pre-command-hook . (lambda () (global-aggressive-indent-mode 0)
				                         (projectile-mode 0)
				                         (editorconfig-mode 0)))
  :bind ("C-c s" . helm-tramp))

(defun dwim-backward-kill-word ()
  "DWIM kill characters backward until encountering the beginning of a
word or non-word."
  (interactive)
  (if (thing-at-point 'word) (backward-kill-word 1)
    (let* ((orig-point              (point))
           (orig-line               (line-number-at-pos))
           (backward-word-point     (progn (backward-word) (point)))
           (backward-non-word-point (progn (goto-char orig-point) (backward-non-word) (point)))
           (min-point               (max backward-word-point backward-non-word-point)))

      (if (< (line-number-at-pos min-point) orig-line) (progn (goto-char min-point) (end-of-line) (delete-horizontal-space))
        (delete-region min-point orig-point)
        (goto-char min-point))
      )))

(defun backward-non-word ()
  "Move backward until encountering the beginning of a non-word."
  (interactive)
  (search-backward-regexp "[^a-zA-Z0-9\s\n]")
  (while (looking-at "[^a-zA-Z0-9\s\n]")
    (backward-char))
  (forward-char))
(global-set-key (kbd "M-DEL") 'dwim-backward-kill-word)

(global-set-key (kbd "M-o") 'ace-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(go-mode yasnippet company-lsp lsp-ui editorconfig rtags flycheck-irony company-irony company irony no-littering yaml-mode use-package-ensure-system-package paredit load-dir key-chord idle-highlight-mode helm-smex helm-projectile helm-flx google-c-style exec-path-from-shell better-defaults auto-package-update ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
