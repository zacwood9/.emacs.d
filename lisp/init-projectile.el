(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t
        projectile-mode-line "Projectile"
        projectile-switch-project-action 'magit-status
        remote-file-name-inhibit-cache nil)
  :config
  (projectile-mode t)
  )

(provide 'init-projectile)
