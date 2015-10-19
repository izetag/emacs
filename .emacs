(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/tabbar-master")
(require 'google-c-style)
(require 'tabbar)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(require 'frame-cmds)
;; turning off welcome screen
(setq inhibit-startup-message t)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "Russian")
 '(custom-enabled-themes (quote (misterioso)))
 '(show-paren-mode t)
 '(tabbar-background-color nil)
 '(tabbar-mode t nil (tabbar))
 '(tabbar-use-images nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-button-highlight ((t (:inherit nil))))
 '(tabbar-highlight ((t nil))))

(set-language-environment 'UTF-8)
(set-selection-coding-system 'utf-16le-dos)
(set-default-coding-systems 'windows-1251)
(prefer-coding-system 'windows-1251)
(setq default-process-coding-system '(cp866 . cp866))
(set-terminal-coding-system 'cp866)
