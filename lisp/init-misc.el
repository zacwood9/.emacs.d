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

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))

(use-package dimmer
  :config (dimmer-mode))

(use-package pdf-tools
  :config
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (setq-default pdf-view-display-size 'fit-page))

(use-package swift-mode)

(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets)

;; (defun cquery//enable ()
;;   (condition-case nil
;;       (lsp-cquery-enable)
;;     (user-error nil)))

;; (use-package cquery
;;   :config
;;   (setq cquery-executable "/usr/local/Cellar/cquery/20180718/bin/cquery")
;;   (add-hook 'c-mode-hook #'cquery//enable)
;;   (add-hook 'c++-mode-hook #'cquery//enable))

(setq python-shell-interpreter "/usr/local/bin/python3")
(setq tramp-default-method "ssh")


(provide 'init-misc)
