(use-package rtags
  :config
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))

(use-package company-rtags
  :config
  (add-to-list 'company-backends 'company-rtags))

(use-package ivy-rtags)

(use-package flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-mode 1)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c c") 'compile)))

(use-package irony
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook
	    (lambda ()
	      (define-key irony-mode-map [remap completion-at-point]
		'irony-completion-at-point-async)
	      (define-key irony-mode-map [remap complete-symbol]
		'irony-completion-at-point-async)))
  (setq-default sp-escape-quotes-after-insert nil))

(use-package company-irony
  :config
  (setq company-backends (delete 'company-semantic company-backends))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package cmake-mode)

(use-package cmake-ide
  :config
  (cmake-ide-setup))

(setq c-default-style "k&r")


(provide 'init-c)
