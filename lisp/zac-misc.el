(defconst *is-a-mac* (eq system-type 'darwin))

(defun zac/edit-emacs-config ()
  "Opens Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun zac/new-restclient-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "restclient"))  
  (restclient-mode))

(defun zac/dired-new-eyeframe (dir)
  (interactive "DOpen Directory in new Eyeframe: ")
  (eyebrowse-create-window-config)
  (delete-other-windows)
  (dired dir))

(defun zac/find-file-new-eyeframe (file)
  (interactive "FFind file in new Eyeframe: ")
  (eyebrowse-create-window-config)
  (delete-other-windows)
  (find-file file))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c o c") 'zac/edit-emacs-config)
(global-set-key (kbd "C-c o r") 'zac/new-restclient-buffer)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o s") 'ansi-term)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-w C-d") 'zac/dired-new-eyeframe)
(global-set-key (kbd "C-c C-w C-f") 'zac/find-file-new-eyeframe)

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
  "Remove file connected to current buffer and kills buffer."
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

(defun copy-current-buffer-file ()
  "Copies file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "Copy to: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (copy-file filename new-name 1)
	  (find-file new-name)
          (message "File '%s' successfully copied to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-y") 'copy-current-buffer-file)

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
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key (kbd "<C-S-down>") 'move-line-down)

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

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun zac/insert-js-comment ()
  (interactive)
  (insert "/**\n */")
  (previous-line)
  (web-mode-comment-indent-new-line))

(windmove-default-keybindings)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)

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

;; when on mac
(when *is-a-mac*
  (setq mac-command-modifier 'meta) ; set cmd to meta
  (setq mac-option-modifier nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ; configure title bar
  (add-to-list 'default-frame-alist '(ns-appearance . 'nil))
  (defun zac/open-file ()
    "Opens a file using the macOS \"open\" command."
    (interactive)
    (shell-command (concat "open " (shell-quote-argument (buffer-file-name))))))

(defun zac/print ()
  (interactive)
  (ivy-read
   "Choose printer: "
   (zac//printers)
   :action (lambda (printer)
	     (print printer))))

(defun zac//printers ()
  (delete nil (mapcar
   (lambda (str)
     (let ((split (split-string str " ")))
       (if (equal "printer" (first split))
	   (concat (second split) " " (string-remove-suffix "." (fourth split))))))
   (split-string (shell-command-to-string "/usr/bin/lpstat -p -d") "\n" t))))

(defun zac/list-printers ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "printers"))
  (insert (string-join (zac//printers) "\n"))
  (view-mode))

(defvar zac/downloads-directory (expand-file-name "~/Downloads/"))

(defun zac/move-from-downloads ()
  "Moves a file from ~/Downloads to the current directory"
  (interactive)
  (let ((current-directory default-directory)
	(default-directory zac/downloads-directory))
    (call-interactively 'zac//move-from-downloads-helper)))

(defun zac//move-from-downloads-helper (file)
  (interactive "FChoose file: ")
  (rename-file file (concat current-directory (file-name-nondirectory file))))

(defun read-lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun lines-matching-p (regexp lines)
  (delete nil
	  (mapcar (lambda (line)
		    (if (string-match-p regexp line)
			line))
		  lines)))

(defun zac/copy-file-name ()
  (interactive)
  (shell-command (concat "echo -n '" buffer-file-name "' | pbcopy")))


(provide 'zac-misc)
