(use-package go-mode
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'init-go)
