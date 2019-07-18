(require 'applescript)

(defconst iterm "application \"iTerm\"")

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

(defun iterm-split (&optional activate?)
  (interactive "p")
  (iterm (:tell "current session of current window" "split vertically with default profile"))
  (if activate? (iterm-activate)))

(defun iterm-run-command (cmd)
  (interactive "MRun command in iTerm: ")
  (iterm (:tell "current session of current window" (format "write text \"%s\"" cmd))))

(defun iterm-set-name (name)
  (interactive "MSet name of current tab: ")
  (iterm (:tell "current session of current window" (format "set name to \"%s\"" name))))

(defun iterm-tab-names ()
  (let* ((str (do-applescript "tell application \"iTerm\"
	set theList to {}
	repeat with t in tabs of current window
		copy name of current session of t to the end of the |theList|
	end repeat
	theList
end tell"))
         (test (subseq str 1 (1- (length str)))))
    (remove-if (lambda (word) (or (string-equal word ", ") (string-equal word ""))) (split-string test "\""))))

(defun iterm-open-dir (dir)
  (interactive "DOpen iTerm tab in directory: ")
  (iterm-new-tab t)
  (iterm-run-command (format "cd %s" dir)))

(provide 'iterm)
