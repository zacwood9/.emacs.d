(use-package notmuch
  :defer t
  :config
  ;; setup the mail address and use name
  (setq mail-user-agent 'message-user-agent)
  (setq user-mail-address "zac.wood9@gmail.com"
	user-full-name "Zachary Wood")
  ;; smtp config
  (setq smtpmail-smtp-server "smtp.gmail.com"
	message-send-mail-function 'message-smtpmail-send-it
	notmuch-search-oldest-first nil
	notmuch-show-logo nil)

  ;; report problems with the smtp server
  (setq smtpmail-debug-info t)
  ;; add Cc and Bcc headers to the message buffer
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  ;; postponed message is put in the following draft directory
  (setq message-auto-save-directory "~/mail/draft")
  (setq message-kill-buffer-on-exit t)
  ;; change the directory to store the sent mail
  (setq message-directory "~/mail/"))

(defun notmuch-exec-offlineimap ()
    "execute offlineimap"
    (interactive)
    (set-process-sentinel
     (start-process-shell-command "offlineimap"
                                  "*offlineimap*"
                                  "offlineimap -o")
     '(lambda (process event)
        (notmuch-refresh-all-buffers)
        (let ((w (get-buffer-window "*offlineimap*")))
          (when w
            (with-selected-window w (recenter (window-end))))))))

(defun notmuch-inbox ()
  (interactive)
  (notmuch-tree "tag:inbox"))

(provide 'init-mail)
