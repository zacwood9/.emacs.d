(use-package eyebrowse
  :defer t
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
  :defer t
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :defer t)

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

;; (use-package elpy
;;   :config
;;   (elpy-enable)
;;   (setq elpy-rpc-python-command "python3")
;;   (setq doom-modeline-python-executable "python3"))

(use-package ag
  :defer t)

(use-package ess
  :defer t
  :config
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer))

(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t) (R . t))))
 '(org-confirm-babel-evaluate nil))

(use-package elfeed
  :defer t
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
	"https://davedelong.com/feed.xml"
	"https://nytrss.zacwood.me/opinion/bret%20stephens")))

(global-set-key (kbd "C-x g") #'magit-status)

(defun zac/new-math290 (file)
  "Makes a new math290 file"
  (interactive "fMake new MATH 290 tex document: ")
  (find-file file)
  (insert-file-contents "~/.emacs.d/templates/math290.tex")
  (goto-char (point-min))
  (re-search-forward "MATH 290 ")
  (save-buffer))

(defun buffer-string* (buffer)
  "Returns the string of the contents of buffer"
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun flatmapcar (func seq)
  "mapcar and remove all nil results"
  (remove nil (mapcar func seq)))

(defun copy-to-osx (text)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(provide 'init-misc)
