;; functions for fast org SQL queries

(defvar zac/sql-engine "postgresql"
  "SQL Engine for use in org-babel.")
(defvar zac/sql-user "zacharywood"
  "User to login as when executing org-babel SQL queries.")
(defvar zac/sql-database "zacharywood"
  "Database to use when executing org-babel SQL queries.")
(defvar zac/sql-host "localhost"
  "Host to connect to when executing org-babel SQL queries.")
(defvar zac/sql-password ""
  "Password to use when connecting for org-babel SQL queries.")

(if (file-exists-p "~/.emacs.d/sql-data.el") (load "~/.emacs.d/sql-data.el"))

(defun zac/insert-sql-block ()
  "Inserts a SQL src block with credentials for use in org buffers."
  (interactive)
  (insert (format "#+begin_src sql :engine %s :dbhost %s :dbuser %s :dbpassword %s :database %s \n\n#+end_src"
		  zac/sql-engine
		  zac/sql-host
		  zac/sql-user
		  zac/sql-password
		  zac/sql-database))
  (previous-line))

(defun zac/insert-elisp-block ()
  "Inserts a emacs lisp src block for use in org buffers."
  (interactive)
  (insert "#+begin_src emacs-lisp\n\n#+end_src")
  (previous-line))

(defun zac/sql-query ()
  "Creates a new Org mode buffer and inserts an sql org-babel block."
  (interactive)
  (switch-to-buffer (generate-new-buffer "SQL"))
  (zac/insert-sql-block)
  (org-mode))

(provide 'zac-sql)
