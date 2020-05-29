(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
  :config
  (helm-autoresize-mode 1)
  ; same value here means it will not resize
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 30)
  )

