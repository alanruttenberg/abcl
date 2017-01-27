(in-package :sys)

;; adapted from openmcl

(defun maybe-alternative (name &optional (test (constantly t)))
  (delete-duplicates
   (loop
     with symbol = (if (consp name) (second name) name)
     with pname = (symbol-name symbol)
     for package in (list-all-packages)
     for other-package-symbol = (find-symbol pname package)
     for candidate = (and other-package-symbol
                          (neq other-package-symbol symbol)
                          (if (consp name)
			      (list (first name) other-package-symbol)
			      other-package-symbol))
     when (and candidate (funcall test candidate))
       collect candidate)
   :test #'equal))

(defun read-string-from-user (prompt)
  (if (find-package 'swank)
      (funcall (intern "READ-FROM-MINIBUFFER-IN-EMACS" 'swank) prompt)
      (progn
	(write-string *query-io*)
	(eval (read *query-io*)))))

(defun with-global-restarts (fn)
  (flet ((new-value ()
	   (catch :cancel
	     (return-from new-value
	       (list (cell-error-name *debug-condition*)
		     (read-string-from-user
		      (format nil "New value for ~s : " (cell-error-name *debug-condition*))))))
	   (continue *debug-condition*)) ; force error again if cancelled, var still not set.
	 (other-choices (name)
	   (maybe-alternative name (lambda (name) (and (not (keywordp name)) (boundp name))))))
    (restart-case (funcall fn)
      (continue ()
	:test (lambda (c) (typep c 'unbound-variable))
	:report (lambda (s) (format s "Retry getting the value of ~S." (cell-error-name *debug-condition*)))
	(symbol-value (cell-error-name *debug-condition*)))
      (use-homonym (homonym)
	:test (lambda (c) (and (typep c 'unbound-variable) (= (length (other-choices (cell-error-name c))) 1)))
	:report (lambda (s) 
		  (format s "Use the value of ~s this time." (first (other-choices (cell-error-name *debug-condition*)))))
	:interactive (lambda () (use-homonym (car (other-choices (cell-error-name *debug-condition*)))))
	(symbol-value homonym))
      (use-value (value)
	:test (lambda (c) (typep c 'unbound-variable))
	:interactive new-value
	:report (lambda (s)  (format s "Specify a value of ~S to use this time." (cell-error-name *debug-condition*)))
	value)
      (store-value (sym value)
	:test (lambda (c) (typep c 'unbound-variable))
	:interactive new-value 
	:report (lambda (s) (format s "Specify a value of ~S to store and use." (cell-error-name *debug-condition*)))
	(setf (symbol-value sym) value)))))
