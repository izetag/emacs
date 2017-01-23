(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(server-start nil t)
(set-scroll-bar-mode 'right)
(column-number-mode t)
(if (load "mwheel" t)
    (mwheel-install))

(setq backup-by-copying-when-linked t)
(setq backup-by-copying-when-mismatch t)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/"))
  (add-to-list
   'package-archives
   '("marmalade" . "https://marmalade-repo.org/packages/"))
  (package-initialize))

(require 'rtags)
(rtags-diagnostics)

(defvar my-packages '(better-defaults
                      paredit
                      idle-highlight-mode
                      ido-ubiquitous
                      find-file-in-project
                      smex
                      scpaste
                      load-dir
                      yaml-mode
                      projectile
                      flx-ido
                      ido-vertical-mode
                      exec-path-from-shell
                      google-c-style
                      key-chord))
(defconst my-custom-file "~/.emacs.d/custom.el")
(setq custom-file my-custom-file)
(load custom-file t)

;; (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running) 
;; (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

(setq backup-by-copying-when-linked t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; to allow scaling text without scaling line numbers since they get truncated otherwise
(require 'linum)
(add-hook 'prog-mode-hook #'linum-on)
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

;; installing not installed packages
(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(load-theme 'misterioso t)

                                        ; recent list
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; better fuzzy matching
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; supporting projects
(require 'projectile)
(projectile-global-mode)

;; environment variabes from shell on mac
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; smex fuzzy matching of commands
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Setting custom font
;; Click [here](https://github.com/hbin/dotfiles-for-emacs) to take a further look.
(set-frame-font "DejaVu Sans Mono:pixelsize=18")

;; If you use Emacs Daemon mode
(add-to-list 'default-frame-alist
               (cons 'font "DejaVu Sans Mono:pixelsize=18"))

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
(key-chord-define c++-mode-map "df" 'rtags-find-symbol-at-point)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(global-set-key (kbd "C-s-<left>") 'previous-buffer)
(global-set-key (kbd "C-s-<right>") 'next-buffer)
(global-set-key (kbd "C-s-<down>") 'ff-find-other-file)
(global-set-key (kbd "C-s-<up>") 'ff-find-other-file)
(global-set-key (kbd "C-s-R") 'rtags-find-references-at-point)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
(setq rtags-use-helm t)

(require 'flycheck-rtags)

(require 'icicles)
(icy-mode 1)

