(use-package rvm
  :defer t
  :config
  (rvm-use-default))

(use-package robe
  :defer t
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (push 'company-robe company-backends))

(use-package yard-mode
  :defer t
  :config
  (add-hook 'enh-ruby-mode-hook 'yard-mode))

(use-package rubocop
  :defer t
  :config
  (add-hook 'enh-ruby-mode-hook 'rubocop-mode))

(use-package projectile-rails
  :defer t
  :config
  (projectile-rails-global-mode))

(use-package enh-ruby-mode
  :defer t
  :config
  (add-to-list
   'auto-mode-alist
   '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))

;; (use-package lsp-ruby
;;   :config(add-hook 'ruby-mode-hook #'lsp-ruby-enable))


;; (require 'lsp-ruby)
;; (add-hook 'ruby-mode-hook #'lsp-ruby-enable)

(provide 'init-ruby)
