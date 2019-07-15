(use-package rvm
  :defer t
  :config
  (rvm-use-default))

(use-package enh-ruby-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))

(use-package yard-mode
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook 'yard-mode))

(use-package rubocop
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook 'rubocop-mode))

(use-package projectile-rails
  :after projectile
  :after enh-ruby-mode
  :config
  (projectile-rails-global-mode))

(use-package robe
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook #'robe-mode)
  (push 'company-robe company-backends))

(use-package ruby-end
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook #'ruby-end-mode))

(use-package rspec-mode
  :ensure t
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode 'rspec-mode)
  (advice-add 'rspec-verify :after
            (lambda ()
              )))



(defun zac/run-spec ()
  (interactive)
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files))))
    (shell-command (format "rspec"))))

;; (use-package lsp-ruby
;;   :config(add-hook 'ruby-mode-hook #'lsp-ruby-enable))


;; (require 'lsp-ruby)
;; (add-hook 'ruby-mode-hook #'lsp-ruby-enable)

(provide 'init-ruby)
