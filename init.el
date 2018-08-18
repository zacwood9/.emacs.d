;;; package --- Summary: my slowly evolving Emacs config :)
;;; Commentary:

;;; Code:

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Load path for manually installed packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-package)
(require 'init-company)
(require 'init-projectile)
(require 'init-ivy)
(require 'init-magit)
(require 'init-flycheck)
(require 'init-lsp)
(require 'init-dired)
(require 'init-org)
(require 'init-js)
(require 'init-ruby)
(require 'init-go)
(require 'init-looks)
(require 'init-misc)
(use-package dockerfile-mode)
(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))
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
    ("76dc63684249227d64634c8f62326f3d40cdc60039c2064174a7e7a7a88b1587" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "fd2cf9ad9895d93138dd67215702280e0db56e796ee62dea92043eed4b23177c" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "8891c81848a6cf203c7ac816436ea1a859c34038c39e3cf9f48292d8b1c86528" "5c72f78946231d45962c8cc2d054b0a437a9385982576d669c07c8e92afeff64" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" default)))
 '(fci-rule-color "#4C566A")
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(package-selected-packages
   (quote
    (lsp-mode lsp-java web-mode flycheck-gometalinter company-go go-mode restclient atom-dark-theme doom-modeline circadian neotree dired-sidebar latex-preview-pane counsel-dash 2048-game company-box magit-todos xkcd fireplace esh-autosuggest discover dired+ all-the-icons-ivy all-the-icons-dired counsel-projectile counsel ivy smart-mode-line css-eldoc csv-mode gitignore-mode dockerfile-mode docker markdown-mode emojify htmlize smartparens company-tern json-mode nyan-mode doom-themes prettier-js rjsx-mode js2-mode exec-path-from-shell tide helm-projectile magit which-key projectile helm company use-package)))
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
