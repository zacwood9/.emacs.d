(defun zac/neotree-projectile-root ()
  (interactive)
  (neotree-dir (projectile-project-root)))

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p t") 'zac/neotree-projectile-root))

(provide 'init-projectile)
