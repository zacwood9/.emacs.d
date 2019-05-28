(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
           #'TeX-revert-document-buffer)

;; to use pdfview with auctex
(add-hook 'LaTeX-mode-hook 'pdf-tools-install)

;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
       TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c b b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography '("~/School/math290/semester_project/proj.bib")
	ivy-bibtex-default-action #'ivy-bibtex-open-any))

(provide 'init-latex)
