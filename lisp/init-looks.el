(use-package doom-themes
  :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
  :init (load-theme 'doom-tomorrow-night t))

;; (use-package doom-modeline
;;   :defer t
;;   :init
;;   (add-hook 'after-init-hook
;; 	    (lambda ()
;; 	      (setq doom-modeline-height 35)
;; 	      (doom-modeline-init)
;; 	      (doom-modeline-mode))))

;; (use-package nyan-mode
;;   :after doom-modeline
;;   :custom
;;   (nyan-cat-face-number 4)
;;   (nyan-animate-nyancat t)
;;   :hook
;;   (doom-modeline-mode . nyan-mode))

;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;   (setq dashboard-banner-logo-title "Welcome, Zac!")
;;   ;; (setq show-week-agenda-p t)
;;   (setq dashboard-items '((agenda . 5))))

(use-package all-the-icons-dired
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; set font
(set-face-attribute 'default nil
		    :family "Hack"
		    :height 175)

(setq linum-format "%3d ")
(setq-default cursor-type 'bar)


(column-number-mode 1)
(setq mode-line-position
            '(;; %p print percent of buffer above top of window, o Top, Bot or All
              ;; (-3 "%p")
              ;; %I print the size of the buffer, with kmG etc
              ;; (size-indication-mode ("/" (-4 "%I")))
              ;; " "
              ;; %l print the current line number
              ;; %c print the current column
              (line-number-mode ("%l" (column-number-mode ":%c")))))

;; (defun shorten-directory (dir max-length)
;;   "Show up to `max-length' characters of a directory name `dir'."
;;   (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;                (output ""))
;;        (when (and path (equal "" (car path)))
;;          (setq path (cdr path)))
;;        (while (and path (< (length output) (- max-length 4)))
;;          (setq output (concat (car path) "/" output))
;;          (setq path (cdr path)))
;;        (when path
;;          (setq output (concat ".../" output)))
;;        output))

;; (defvar mode-line-directory
;;   '(:propertize
;;     (:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 20)) " "))
;;                 face mode-line-directory)
;;   "Formats the current directory.")
;; (put 'mode-line-directory 'risky-local-variable t)

;; (setq-default mode-line-buffer-identification
;;               (propertized-buffer-identification "%b "))

;; (setq-default mode-line-format
;;       '("%e"
;;         mode-line-front-space
;;         ;; mode-line-mule-info -- I'm always on utf-8
;;         mode-line-client
;;         mode-line-modified
;;         ;; mode-line-remote -- no need to indicate this specially
;;         ;; mode-line-frame-identification -- this is for text-mode emacs only
;;         " "
;;         mode-line-directory
;;         mode-line-buffer-identification
;;         " "
;;         mode-line-position
;;         ;;(vc-mode vc-mode)  -- I use magit, not vc-mode
;;         ;(flycheck-mode flycheck-mode-line)
;;         " "
;;         ;mode-line-modes
;;         mode-line-misc-info
;;         mode-line-end-spaces))

(defvar no-modified-modes
  '("Magit" "Ag" "Mocha")
  "List of mode names that should not turn the mode line red when modified.")


(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;mode-line-mule-info
                ;mode-line-client
                ;mode-line-modified
                ;mode-line-remote
                ;mode-line-frame-identification
                                        ;mode-line-buffer-identification
                "  🏞  "
                (:eval (list :propertize (buffer-name)
                             'face
                             (concatenate 'list
                                          '(:weight bold)
                                          (if (and (buffer-modified-p) (not (seq-contains no-modified-modes mode-name)))
                                              '(:foreground "OrangeRed1")))))
                " | "
                mode-line-position

                (:eval (when-let ((branch (magit-get-current-branch))
                                  (temp (buffer-file-name)))
                         (concat " | git: " branch)))
                " | "
                mode-name
                ;mode-line-modes
                mode-line-misc-info
                " | "
                (:eval (calendar-date-string (calendar-current-date)))
                "%n"
                mode-line-end-spaces))


(provide 'init-looks)
