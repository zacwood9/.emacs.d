;; (use-package lsp-mode
;;   :ensure t
;;   :init (setq lsp-inhibit-message t
;;               lsp-eldoc-render-all nil
;;               lsp-highlight-symbol-at-point nil))

;; (use-package company-lsp
;;   :after company
;;   :ensure t
;;   :config
;;   ;; (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
;;   (setq company-lsp-enable-snippet t
;;         company-lsp-cache-candidates t))

;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (setq lsp-ui-sideline-enable t
;;         lsp-ui-sideline-show-symbol t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-sideline-update-mode 'point))

;; (use-package lsp-java
;;   :ensure t
;;   :requires (lsp-ui-flycheck lsp-ui-sideline)
;;   :config
;;   (add-hook 'java-mode-hook  'lsp-java-enable)
;;   (add-hook 'java-mode-hook  'flycheck-mode)
;;   (add-hook 'java-mode-hook  'company-mode)
;;   (add-hook 'java-mode-hook  (lambda () (lsp-ui-flycheck-enable t)))
;;   (add-hook 'java-mode-hook  'lsp-ui-sideline-mode))

;;(use-package eglot)

;; (use-package lsp-mode
;;   :hook (rjsx-mode . lsp))

;; (use-package company-lsp)

;; (use-package lsp-ui
;;   :config
;;   (add-hook 'lsp-ui-mode (lambda () (lsp-ui-mode -1))))

(provide 'init-lsp)
