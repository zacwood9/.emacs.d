;;;; init.el -- my slowly evolving emacs config :)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

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
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

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
                           "--single-quote" "false"
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
  (add-hook 'rjsx-mode-hook 'tide-setup))


(use-package tide
  :ensure t
  :mode(("\\.ts\\'" . typescript-mode))
  :init
  (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'tide-mode-hook
	    (lambda ()
	      (flycheck-mode 1)
	      (setq flycheck-check-syntax-automatically '(save mode-enabled)))))

(use-package json-mode
  :ensure t)

(use-package css-eldoc
  :ensure t
  :init(css-eldoc-enable))

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

(use-package docker
  :ensure t
  :init(docker-global-mode))

(use-package gitignore-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

;;; Custom functions

(defun zac/edit-emacs-config ()
  "Opens Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defvar zac/sql-engine "postgresql"
  "SQL Engine for use in org-babel")
(defvar zac/sql-user "zacharywood"
  "User to login as when executing org-babel SQL queries")
(defvar zac/sql-database "zacharywood"
  "Database to use when executing org-babel SQL queries")
(defvar zac/sql-host "localhost"
  "Host to connect to when executing org-babel SQL queries")
(defvar zac/sql-password ""
  "Password to use when connecting for org-babel SQL queries")

(if (file-exists-p "~/.emacs.d/sql-data.el") (load "~/.emacs.d/sql-data.el"))

(defun zac/insert-sql-block ()
  "Inserts a SQL src block with credentials for use in org buffers"
  (interactive)
  (insert (format "#+begin_src sql :engine %s :dbhost %s :dbuser %s :dbpassword %s :database %s \n\n#+end_src"
		  zac/sql-engine
		  zac/sql-host
		  zac/sql-user
		  zac/sql-password
		  zac/sql-database))
  (previous-line))

(defun zac/insert-elisp-block ()
  "Inserts a emacs lisp src block for use in org buffers"
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

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (transpose-lines 1))
    (line-move -2)
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

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
;; (global-linum-mode 1)

;;; see: https://stackoverflow.com/questions/6837511/automatically-disable-a-global-minor-mode-for-a-specific-major-mode
(defun zac/inhibit-global-linum-mode ()
  "Counter-act `global-linum-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))

;;; turn off linum-mode for certain modes
(add-hook 'eshell-mode-hook 'zac/inhibit-global-linum-mode)
(add-hook 'org-mode-hook 'zac/inhibit-global-linum-mode)

(windmove-default-keybindings)
(global-set-key (kbd "M-o") 'other-window)

;; add more languages for org-babel
(org-babel-do-load-languages 'org-babel-load-languages '(
							 (sql . t)
							 ))

(show-paren-mode 1) ; visualize matching parenthesees
(global-hl-line-mode 1) ; highlight current line
(eldoc-mode 1) ; enable docs in minibuffer
(delete-selection-mode 1)

;; store all backups in a single directory 
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; y or n instead of yes-or no
(fset 'yes-or-no-p 'y-or-n-p)

;; no annoying bell!
(setq ring-bell-function 'ignore)

;; set font
(set-face-attribute 'default nil
		    :family "Hack"
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" default)))
 '(fci-rule-color "#4C566A")
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(package-selected-packages
   (quote
    (css-eldoc csv-mode gitignore-mode dockerfile-mode docker markdown-mode emojify htmlize smartparens company-tern json-mode nyan-mode doom-themes prettier-js rjsx-mode js2-mode exec-path-from-shell tide helm-projectile magit which-key projectile helm company use-package)))
 '(vc-annotate-background "#3B4252")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A2BF8A")
    (cons 40 "#bac389")
    (cons 60 "#d3c788")
    (cons 80 "#ECCC87")
    (cons 100 "#e3b57e")
    (cons 120 "#da9e75")
    (cons 140 "#D2876D")
    (cons 160 "#c88982")
    (cons 180 "#be8b98")
    (cons 200 "#B58DAE")
    (cons 220 "#b97e97")
    (cons 240 "#bd6f80")
    (cons 260 "#C16069")
    (cons 280 "#a15b66")
    (cons 300 "#825663")
    (cons 320 "#625160")
    (cons 340 "#4C566A")
    (cons 360 "#4C566A")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
