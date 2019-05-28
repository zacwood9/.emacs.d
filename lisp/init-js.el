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

(use-package js2-mode
  :defer 1)

(use-package tide
  :defer 1
  :mode(("\\.ts\\'" . typescript-mode))
  :config
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'tide-mode-hook
	    (lambda ()
	      (flycheck-mode 1)
	      (setq flycheck-check-syntax-automatically '(save mode-enabled)))))

(use-package prettier-js
  :defer 1)

(use-package rjsx-mode
  :after tide
  :after prettier-js
  :mode(("\\.js\\'" . rjsx-mode)
	("\\.jsx\\'" . rjsx-mode))
  :init
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'tide-setup))

;; (use-package vue-mode
;;   :defer t)

(defun camel-to-kebab (str)
  (let ((case-fold-search nil))
    (apply 'concat (cl-loop for c across str
			    if (char-equal c (upcase c))
			    collect (concat "-" (string (downcase c)))
			    else collect (string c)))))

(defun json-to-args (json)
  (mapcar
   (lambda (obj)
     (list
      (concat "--" (camel-to-kebab (symbol-name (car obj))))
      (if (eq t (cdr obj))
	  "true"
	(if (numberp (cdr obj))
	    (number-to-string (cdr obj))
	  (cdr obj)))))
   json))

(provide 'init-js)
