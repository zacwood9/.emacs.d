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

(use-package all-the-icons-dired
  :init(add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
    :config
  (all-the-icons-ivy-setup))

(use-package discover
  :init(global-discover-mode 1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))

(provide 'init-misc)
