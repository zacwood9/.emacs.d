;;;; init.el -- my slowly evolving emacs config :)

;;; Package config -- see https://melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Packages

;; Load path for manually installed packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "120"
                           "--tab-width" "4"
                           "--use-tabs" "false"
                           "--jsx-bracket-same-line" "false"
                           "--stylelint-integration" "true"
                           )))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

(use-package js2-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :mode(("\\.js\\'" . rjsx-mode)
	("\\.jsx\\'" . rjsx-mode))
  :init
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'tide-mode))

(use-package tide
  :ensure t
  :mode(("\\.ts\\'" . typescript-mode))
  :init
  (add-hook 'typescript-mode-hook 'tide-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  :config
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package json-mode
  :ensure t)

(use-package robe
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  :config
  (push 'company-robe company-backends))

(use-package yard-mode
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'yard-mode))
  
(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
  :init (load-theme 'doom-one t))

(use-package htmlize
  :ensure t)

(use-package yaml-mode
  :ensure t)

;;; Custom functions

(defun zac/edit-emacs-config ()
  "Opens Emacs configuration file"
  (interactive)
  (find-file user-init-file))

;; variables for use with org-babel sql
(setq zac/sql-engine "postgresql"
      zac/sql-user "zacharywood"
      zac/sql-database "zacharywood")

(defun zac/insert-sql-block ()
  "Inserts a SQL src block with credentials for use in org buffers"
  (interactive)
  (insert (format "#+begin_src sql :engine %s :cmdline \"-U %s -d %s\"\n\n#+end_src" zac/sql-engine zac/sql-user zac/sql-database))
  (previous-line))

(defun zac/insert-elisp-block ()
  "Inserts a SQL src block with credentials for use in org buffers"
  (interactive)
  (insert "#+begin_src emacs-lisp\n\n#+end_src")
  (previous-line))

(defun zac/sql-query ()
  "Creates a new Org mode buffer and inserts an sql org-babel block"
  (interactive)
  (switch-to-buffer (generate-new-buffer "SQL"))
  (zac/insert-sql-block)
  (org-mode))

;; url to use with pretty-print-endpoint
(setq zac/api-url "http://localhost:3001/")

(defun zac/pretty-print-endpoint (url)
  "Prints formatted JSON from a given endpoint in a new buffer"
  (interactive "sEndpoint: ")
  (with-current-buffer (url-retrieve-synchronously (format (concat zac/api-url "%s") url))
    (end-of-buffer)
    (let ((json-string (thing-at-point 'line)))
      (switch-to-buffer
       (generate-new-buffer (format "Pretty Endpoint %s" url)))
      (save-excursion
	(insert json-string)
	(json-pretty-print-buffer))
      (json-mode))))

(global-set-key (kbd "C-c o c") 'zac/edit-emacs-config)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)

;;; Projects

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


;;; Customization

;; add more languages for org-babel
(org-babel-do-load-languages 'org-babel-load-languages '(
							 (sql . t)
							 ))

(scroll-bar-mode 0) ; no scroll bar
(tool-bar-mode 0) ; no tool bar
(menu-bar-mode 0) ; no menu bar
(show-paren-mode 1) ; visualize matching parenthesees
(global-hl-line-mode 1) ; highlight current line
(eldoc-mode 1) ; enable docs in minibuffer
(setq inhibit-startup-screen 1) ; no start screen

;; store all backups in a single directory 
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; y or n instead of yes-or no
(fset 'yes-or-no-p 'y-or-n-p)

;; no annoying bell!
(setq ring-bell-function 'ignore)

;; set font
(set-face-attribute 'default nil
		    :family "Menlo"
		    :height 150)

;; when on mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta) ; set cmd to meta
  (setq mac-option-modifier nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ; configure title bar
  (add-to-list 'default-frame-alist '(ns-appearance . 'nil)))

;; set my init filt to be this file
(setq user-init-file "~/.emacs.d/init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode yard-mode robe which-key use-package tide smartparens rjsx-mode prettier-js magit json-mode htmlize helm-projectile exec-path-from-shell doom-themes company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
