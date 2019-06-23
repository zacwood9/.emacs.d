(require 'applescript)

(defmacro iterm (form)
  `(applescript-do (:tell iterm ,form)))

(defun iterm-activate ()
  (interactive)
  (iterm "activate"))

(defun iterm-new-tab (&optional activate?)
  (interactive "p")
  (iterm (:tell "current window" "create tab with default profile"))
  (if activate? (iterm-activate)))

(defun iterm-new-window (&optional activate?)
  (interactive "p")
  (iterm "create window with default profile")
  (if activate? (iterm-activate)))

(defun iterm-run-command (cmd)
  (interactive "MRun command in iTerm: ")
  (iterm (:tell "current session of current window" (format "write text \"%s\"" cmd))))
