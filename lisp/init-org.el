;; zacwood.me Blog
(setq org-publish-project-alist
      '(("zacwood.me.posts"
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

(setq org-export-show-temporary-export-buffer nil)


(defun zac/export-zacwood-md-pages ()
  (interactive)
  (let* ((project-dir "~/Developer/zacwood9.github.io/")
	 (pages-dir (concat project-dir "_org/projects/"))
	 (files (directory-files pages-dir)))
    (dolist (file files)
      (message file)
      (let ((file-path (concat pages-dir file)))
	(if (string-suffix-p ".org" file-path)
	    (with-current-buffer (find-file-noselect file-path)
	      (with-current-buffer (org-md-export-as-markdown)
		(write-file (concat project-dir "_projects/" (string-trim file nil ".org") ".md") nil))))))))

(defun zac/publish-zacwood-me ()
  (interactive)
  (zac/export-zacwood-md-pages)
  (org-publish-project "zacwood.me.posts"))


(setq zac/blog-post-header "#+OPTIONS: toc:nil num:nil\n#+BEGIN_EXPORT html\n---\nlayout: post\ntitle: \ntags: \n---\n#+END_EXPORT\n\n\n\n#+BEGIN_EXPORT html\n<!--description-->\n#+END_EXPORT\n")

(defun zac/new-blog-post (name)
  (interactive "sPost name: ")
  (find-file
   (concat "~/Developer/zacwood9.github.io/_org/posts/" (format-time-string "%Y-%m-%d") "-" name ".org"))
  (insert zac/blog-post-header)
  (goto-line 5)
  (move-end-of-line nil))

(global-set-key (kbd "C-c b n") #'zac/new-blog-post)
(global-set-key (kbd "C-c b p") #'zac/publish-zacwood-me)

(add-hook 'org-mode-hook #'visual-line-mode)
(setq org-default-notes-file "~/Developer/notes.org")

(defun zac/open-todos ()
  (interactive)
  (find-file "~/Developer/notes.org"))

(global-set-key (kbd "C-c o t") #'zac/open-todos)

(use-package org-bullets
  :config (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-babel-python-command "python3")
(setq org-agenda-start-on-weekday nil)
(setq org-archive-location "~/Developer/archive.org::")

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(use-package org-pomodoro)

(provide 'init-org)
