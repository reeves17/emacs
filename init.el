;;; UI
(scroll-bar-mode -1)  ; remove scroll bars
(tool-bar-mode -1)  ; remove tool bar
(tooltip-mode -1)  ; disable tooltips
(menu-bar-mode -1)  ; remove menu bar

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
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; Evil
(load "$HOME/.emacs.d/package-config/evil.el")
(load "$HOME/.emacs.d/package-config/doom-themes.el")

;;; Completion
(load "$HOME/.emacs.d/package-config/helm.el")

;;; Languages
; Python
;(load "$HOME/.emacs.d/package-config/elpy.el")

;;; Hydra
(load "$HOME/.emacs.d/package-config/hydra.el")

;; Which Key
;(use-package which-key
;  :ensure t
;  :init
;  (setq which-key-separator " ")
;  (setq which-key-prefix-prefix "+")
;  :config
;  (which-key-mode))

;; Custom keybinding
;(use-package general
;  :ensure t
;  :config (general-define-key
;           :states '(normal visual insert emacs)
;           :prefix "SPC"
;           :non-normal-prefix "M-SPC"
;           ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
;           "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
;           "SPC" '(helm-M-x :which-key "M-x")
;           "pf"  '(helm-find-file :which-key "find files")
;           ;; Buffers
;           "bb"  '(helm-buffers-list :which-key "buffers list")
;           ;; Window
;           "wl"  '(windmove-right :which-key "move right")
;           "wh"  '(windmove-left :which-key "move left")
;           "wk"  '(windmove-up :which-key "move up")
;           "wj"  '(windmove-down :which-key "move bottom")
;           "w/"  '(split-window-right :which-key "split right")
;           "w-"  '(split-window-below :which-key "split bottom")
;           "wx"  '(delete-window :which-key "delete window")
;           ;; Others
;           "at"  '(ansi-term :which-key "open terminal")
;           ))
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
