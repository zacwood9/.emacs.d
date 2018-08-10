(use-package robe
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  :config
  (push 'company-robe company-backends))

(use-package yard-mode
  :init
  (add-hook 'ruby-mode-hook 'yard-mode))

(provide 'init-ruby)
