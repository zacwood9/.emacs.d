(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line "Projectile")
  (setq remote-file-name-inhibit-cache nil))

(provide 'init-projectile)
