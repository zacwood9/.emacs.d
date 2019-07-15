(defun applescript--eval-form (form)
  "Inserts an Applescript program built from FROM into the current buffer."
  (cond ((listp form)
         (destructuring-bind (first &optional second third) form
           (cond ((eq first :tell)
                  (insert "\ntell ")
                  (insert (eval second))
                  (applescript--eval-form third)
                  (insert "\nend tell"))
                 ((eq first :set)
                  (insert "\nset ")
                  (insert (eval second))
                  (insert " to ")
                  (insert (eval third)))
                 ((listp first)
                  (mapc #'applescript--eval-form form))
                 (t (insert (concat "\n" (eval form)))))))
	((stringp form)
	 (insert (concat "\n" form)))
	((symbolp form) (insert (concat "\n" (symbol-value form))))
	(t (error "invalid form"))))

(defmacro applescript-do (form)
  `(with-temp-buffer
     (applescript--eval-form (quote ,form))
     (do-applescript (buffer-string))))


(provide 'applescript)
