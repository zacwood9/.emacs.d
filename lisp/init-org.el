;; zacwood.me Blog
(setq org-publish-project-alist
      '(("zacwood.me/posts"
         ;; Path to org files.
         :base-directory "~/Developer/zacwood9.github.io/_org/posts"
         :base-extension "org"

         ;; Path to Jekyll Posts
         :publishing-directory "~/Developer/zacwood9.github.io/_posts"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         )))



(defun zac/export-zacwood-md-pages ()
  (interactive)
  (let* ((project-dir "~/Developer/zacwood9.github.io/")
	(pages-dir (concat project-dir "_org/pages/")))
    (dolist (file (directory-files pages-dir))
      (let ((file-path (concat pages-dir file)))
	(if (string-suffix-p ".org" file-path)
	    (with-current-buffer (find-file-noselect file-path)
	      (with-current-buffer (org-md-export-as-markdown)
		(write-file (concat project-dir (string-trim file nil ".org") ".md") nil))))))))

(defun zac/publish-zacwood-me ()
  (interactive)
  (zac/export-zacwood-md-pages)
  (org-publish-project "zacwood.me/posts"))

(defvar zac/blog-post-header)

(setq zac/blog-post-header "#+OPTIONS: toc:nil num:nil\n#+BEGIN_EXPORT html\n---\nlayout: post\ntitle: \nsubtitle: \ntags: \n---\n#+END_EXPORT\n\n")

(defun zac/new-blog-post (name)
  (interactive "sPost name: ")
  (find-file
   (concat "~/Developer/zacwood9.github.io/_org/posts/" (format-time-string "%Y-%m-%d") "-" name ".org"))
  (insert zac/blog-post-header)
  (goto-line 5)
  (move-end-of-line nil))

(global-set-key (kbd "C-c b n") #'zac/new-blog-post)
(global-set-key (kbd "C-c b p") #'zac/publish-zacwood-me)

;; add more languages for org-babel
(org-babel-do-load-languages 'org-babel-load-languages '(
							 (sql . t)
							 ))

(provide 'init-org)
