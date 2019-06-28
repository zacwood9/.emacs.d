(defun applescript--eval-form (form)
  (cond ((eq (car-safe form) :tell)
	 (insert "\ntell ")
	 (insert (eval (cadr form)))
	 (applescript--eval-form (car (last form)))
	 (insert "\nend tell"))
	((stringp form)
	 (insert (concat "\n" form)))
	((listp form) (insert (concat "\n" (eval form))))
	((symbolp form) (insert (concat "\n" (symbol-value form))))
	(t (error "invalid form"))))

(defmacro applescript-do (form)
  `(with-temp-buffer
     (applescript--eval-form (quote ,form))
     (do-applescript (buffer-string))))


(provide 'applescript)
