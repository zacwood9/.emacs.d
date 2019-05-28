;; packages
(use-package magit
  :defer 0.5
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode))

(provide 'init-magit)
