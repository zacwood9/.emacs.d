(use-package which-key
  :init
  (which-key-mode))

(use-package smartparens
  :init
  (smartparens-global-mode))

(use-package css-eldoc
  :init(css-eldoc-enable))

(use-package docker
  :bind ("C-c d" . docker))

(use-package smart-mode-line
  :init
  (setq sml/theme 'light)
  (add-hook 'after-init-hook 'sml/setup))

(use-package all-the-icons-dired
  :init(add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
    :config
  (all-the-icons-ivy-setup))

(use-package discover
  :init(global-discover-mode 1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  )

(use-package htmlize)
(use-package fireplace)
(use-package xkcd)

(provide 'init-misc)
