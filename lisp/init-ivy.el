(use-package counsel
  :init
  (counsel-mode 1)
  (ivy-mode 1)
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel-projectile
  :init(counsel-projectile-mode 1)
  :config
  (setq ivy-re-builders-alist
  	'((t . ivy--regex-fuzzy))
  	ivy-display-style nil))

(provide 'init-ivy)
