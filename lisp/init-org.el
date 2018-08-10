;; zacwood.me Blog
(setq org-publish-project-alist
      '(("zacwood"
         ;; Path to org files.
         :base-directory "~/iCloud/Developer/zacwood9.github.io/_org"
         :base-extension "org"

         ;; Path to Jekyll Posts
         :publishing-directory "~/iCloud/Developer/zacwood9.github.io/_posts"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         )))

;; add more languages for org-babel
(org-babel-do-load-languages 'org-babel-load-languages '(
							 (sql . t)
							 ))

(provide 'init-org)
