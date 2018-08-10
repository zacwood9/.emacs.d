(use-package doom-themes
    :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
    :init (load-theme 'doom-one-light t))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; set font
(set-face-attribute 'default nil
		    :family "Hack"
		    :height 150)

(provide 'init-looks)
