;;; UI
(scroll-bar-mode -1)  ; remove scroll bars
(tool-bar-mode -1)  ; remove tool bar
(tooltip-mode -1)  ; disable tooltips
(menu-bar-mode -1)  ; remove menu bar
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;;; Package config
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; Builtins config
; parentheses matching
(show-paren-mode 1)
; enable recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;; Evil
(load "$HOME/.emacs.d/package-config/evil.el")
(load "$HOME/.emacs.d/package-config/doom-themes.el")
(load "$HOME/.emacs.d/package-config/evil-escape.el")

;;; File Tree
(load "$HOME/.emacs.d/package-config/all-the-icons.el")
(load "$HOME/.emacs.d/package-config/treemacs.el")
(load "$HOME/.emacs.d/package-config/treemacs-projectile.el")
(load "$HOME/.emacs.d/package-config/treemacs-evil.el")
(load "$HOME/.emacs.d/package-config/treemacs-magit.el")
(load "$HOME/.emacs.d/package-config/treemacs-icons-dired.el")

;;; Completion
(load "$HOME/.emacs.d/package-config/helm.el")

;;; Window Management
(load "$HOME/.emacs.d/package-config/ace-window.el")

;;; Project Management
(load "$HOME/.emacs.d/package-config/projectile.el")
(load "$HOME/.emacs.d/package-config/helm-projectile.el")

;;; Version Control
(load "$HOME/.emacs.d/package-config/magit.el")

;;; Search
(load "$HOME/.emacs.d/package-config/ag.el")
(load "$HOME/.emacs.d/package-config/helm-ag.el")
(load "$HOME/.emacs.d/package-config/avy.el")

;;; Languages
; Python
;(load "$HOME/.emacs.d/package-config/elpy.el")

;;; Hydra
(load "$HOME/.emacs.d/package-config/hydra.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (general which-key helm doom-themes evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
