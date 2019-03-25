(use-package eyebrowse
  :config
  (eyebrowse-mode))

(use-package which-key
  :init
  (which-key-mode))

(use-package smartparens
  :init
  (smartparens-global-mode))

(use-package css-eldoc
  :defer t
  :init(css-eldoc-enable))

(use-package docker
  :defer t
  :bind ("C-c d" . docker))

(use-package all-the-icons-dired
  :init(add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
    :config
  (all-the-icons-ivy-setup))

(use-package discover
  :init
  (global-discover-mode 1))

;; (use-package latex-preview-pane
;;   :config
;;   (latex-preview-pane-enable))

(use-package dimmer
  :config (dimmer-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))

(use-package swift-mode
  :defer t)

(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets)

(use-package restclient
  :defer t
  :config
  (setq restclient-same-buffer-response t))

(setq python-shell-interpreter "/usr/local/bin/python3")
(setq tramp-default-method "ssh")

(use-package selectric-mode)

(use-package try)

(require 'flex)
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))

(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq doom-modeline-python-executable "python3"))

(use-package ag)

(use-package ess
  :config
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer))

(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t) (R . t))))
 '(org-confirm-babel-evaluate nil))

(use-package elfeed
  :config
  (setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "https://swiftbysundell.com/?format=rss"
	"https://xkcd.com/rss.xml"
	"http://feeds.feedburner.com/marginalrevolution/feed"
	"https://www.nationalreview.com/author/jonah-goldberg/rss"
	"https://www.nationalreview.com/author/kevin-d-williamson/rss"
	"https://www.nationalreview.com/author/charles-c-w-cooke/rss"
	"https://ericasadun.com/feed/"
	"https://m.signalvnoise.com/rss"
	"https://lukesmith.xyz/rss.xml"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
	"https://davedelong.com/feed.xml")))

(provide 'init-misc)
