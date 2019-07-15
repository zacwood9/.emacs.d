(use-package ivy
  :defer 0.1
  :config (ivy-mode)
  (setq ivy-count-format "(%d/%d) "
	ivy-use-virtual-buffers t
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-use-selectable-prompt t
        ivy-display-style nil))

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
  
(use-package counsel-projectile
  :after counsel
  :after projectile
  :init (counsel-projectile-mode 1)
  :config
  (defun counsel-projectile-switch-project-magit (project)
    "Opens Magit in the project"
    (let ((projectile-switch-project-action 'magit-status))
      (counsel-projectile-switch-project-by-name project)))
  (setq counsel-projectile-switch-project-action #'counsel-projectile-switch-project-magit)
  ;; (add-to-list 'counsel-projectile-switch-project-action '("mg" counsel-projectile-switch-project-magit "open magit"))
  ))

(use-package smex)

(provide 'init-ivy)
