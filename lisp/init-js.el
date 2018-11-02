(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when
		  (string-equal "tsx" (file-name-extension buffer-file-name))
		(tide-setup)
		(prettier-js-mode)
		(flycheck-mode)
		(setq web-mode-enable-auto-quoting nil))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package js2-mode)

(use-package rjsx-mode
  :mode(("\\.js\\'" . rjsx-mode)
	("\\.jsx\\'" . rjsx-mode))
  :init
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'tide-setup))

(use-package tide
  :mode(("\\.ts\\'" . typescript-mode))
  :config
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'tide-mode-hook
	    (lambda ()
	      (flycheck-mode 1)
	      (setq flycheck-check-syntax-automatically '(save mode-enabled))
	      (global-set-key (kbd "C-;") #'zac/insert-comment))))

(use-package prettier-js
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "180"
                           "--tab-width" "4"
                           "--use-tabs" "false"
                           "--jsx-bracket-same-line" "true"
                           )))

(provide 'init-js)
