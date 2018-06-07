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

(use-package json-mode
  :ensure t)

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
  
(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
  :init (load-theme 'doom-one t))

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

;;; Customization

;; add more languages for org-babel
(org-babel-do-load-languages 'org-babel-load-languages '(
							 (sql . t)
							 (shell . t)
							 (python . t)
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
    ("9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" default)))
 '(fci-rule-color "#4C566A")
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(package-selected-packages
   (quote
    (smartparens company-tern json-mode nyan-mode doom-themes prettier-js rjsx-mode js2-mode exec-path-from-shell tide helm-projectile magit which-key projectile helm company use-package)))
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
