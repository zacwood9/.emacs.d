(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)))



(provide 'init-dired)
