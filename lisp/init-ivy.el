(use-package ivy
  :defer 0.1
  :config (ivy-mode)
  (setq ivy-count-format "(%d/%d) "
	ivy-use-virtual-buffers t))

(use-package ivy-posframe
  :after ivy
  :init (ivy-posframe-enable)
  :config
  (setq ivy-display-function #'ivy-posframe-display-at-frame-center
	ivy-posframe-parameters '((left-fringe . 5) (right-fringe . 5))))

(use-package counsel
  :after ivy
  :init
  (counsel-mode 1)
  (ivy-mode 1)
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (setq ivy-use-selectable-prompt t))

(use-package counsel-projectile
  :after counsel
  :after projectile
  :init (counsel-projectile-mode 1)
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-plus))
	ivy-display-style nil))

(use-package smex)

(provide 'init-ivy)
