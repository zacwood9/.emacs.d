;;; package --- Summary: my slowly evolving Emacs config :)
;;; Commentary:

;;; Code:

;; Load path for manually installed packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

(defvar zac/env-to-copy ())

(require 'init-package)
(require 'init-company)
(require 'init-projectile)
(require 'init-ivy)
(require 'init-magit)
(require 'init-flycheck)
(require 'init-lsp)
(require 'init-dired)
(require 'init-org)
(require 'init-c)
(require 'init-js)
(require 'init-ruby)
(require 'init-go)
(require 'init-cl)
(require 'init-looks)
(require 'init-eshell)
(require 'init-misc)
(use-package dockerfile-mode)
(use-package htmlize)
(use-package fireplace)
(use-package xkcd)
(use-package restclient)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package json-mode)
(use-package gitignore-mode)
(use-package csv-mode)
(require 'zac-sql)
(require 'zac-misc)

(dolist (var zac/env-to-copy)
  (setenv var (shell-command-to-string (concat "echo -n $" var))))

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
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(custom-safe-themes
   (quote
    ("7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "76dc63684249227d64634c8f62326f3d40cdc60039c2064174a7e7a7a88b1587" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "fd2cf9ad9895d93138dd67215702280e0db56e796ee62dea92043eed4b23177c" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "8891c81848a6cf203c7ac816436ea1a859c34038c39e3cf9f48292d8b1c86528" "5c72f78946231d45962c8cc2d054b0a437a9385982576d669c07c8e92afeff64" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(org-agenda-files
   (quote
    ("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/todos.org")))
 '(package-selected-packages
   (quote
    (ox-pandoc eyebrowse darkroom cmake-mode ivy-rtags company-rtags company-irony-c-headers cquery emacs-cquery company-irony dashboard irony yasnippet-snippets yasnippit yasnippet ranger enh-ruby-mode pdf-tools projectile-rails eglot swift-mode dimmer org-bullets rubocop slime rvm lsp-ruby lsp-mode lsp-java web-mode flycheck-gometalinter company-go go-mode restclient atom-dark-theme doom-modeline circadian neotree dired-sidebar latex-preview-pane counsel-dash 2048-game company-box magit-todos xkcd fireplace esh-autosuggest discover dired+ all-the-icons-ivy all-the-icons-dired counsel-projectile counsel ivy smart-mode-line css-eldoc csv-mode gitignore-mode dockerfile-mode docker markdown-mode emojify htmlize smartparens company-tern json-mode nyan-mode doom-themes prettier-js rjsx-mode js2-mode exec-path-from-shell tide helm-projectile magit which-key projectile helm company use-package)))
 '(safe-local-variable-values
   (quote
    ((tab-always-indent . t)
     (swift-basic-offset . 2)
     (swift-syntax-check-fn . swift-project-swift-syntax-check)
     (swift-find-executable-fn . swift-project-executable-find)
     (whitespace-style face lines indentation:space)
     (eval add-hook
	   (quote prog-mode-hook)
	   (lambda nil
	     (whitespace-mode 1))
	   (not :APPEND)
	   :BUFFER-LOCAL)
     (eval let*
	   ((x
	     (dir-locals-find-file default-directory))
	    (this-directory
	     (if
		 (listp x)
		 (car x)
	       (file-name-directory x))))
	   (unless
	       (or
		(featurep
		 (quote swift-project-settings))
		(and
		 (fboundp
		  (quote tramp-tramp-file-p))
		 (tramp-tramp-file-p this-directory)))
	     (add-to-list
	      (quote load-path)
	      (concat this-directory "utils")
	      :append)
	     (let
		 ((swift-project-directory this-directory))
	       (require
		(quote swift-project-settings))))
	   (set
	    (make-local-variable
	     (quote swift-project-directory))
	    this-directory)))))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
