(in-package :jss)

(defvar *inhibit-jss-optimization* nil)

;; https://mailman.common-lisp.net/pipermail/armedbear-devel/2016-October/003726.html

(precompiler::define-function-position-lambda-transform jss::invoke-restargs (arglist form args)
  (declare (ignore arglist))
  (unless *inhibit-jss-optimization*
    (precompiler::precompile-function-call 
     `(jss::invoke-restargs-macro
	  ,(second form)
	  ,(car args) (list ,@(cdr args)) ,(fifth form)))))

(defmacro invoke-restargs-macro ( method object args &optional (raw? nil))
  (assert (eq (car args) 'list))
  (setq args (cdr args))
  (if (and (consp object) (eq (car object) 'quote))
      (let ((object (eval object)))
	(let* ((object-as-class
		 (or (ignore-errors (let ((*muffle-warnings* t)) (find-java-class object)))
		     `(find-java-class ',object))))
	  (if raw?
	      `(jstatic-raw ,method ,object-as-class ,@args)
	      `(jstatic ,method ,object-as-class ,@args))))
      (if raw?
	  `(if (symbolp ,object)
	       (jstatic-raw ,method (find-java-class ,object) ,@args)
	       (jcall-raw ,method ,object ,@args))
	  `(if (symbolp ,object)
	       (jstatic ,method (find-java-class ,object) ,@args)
	       (jcall ,method ,object ,@args)))))



;; This one breaks because it can't make-load-form
;; (define-compiler-macro get-java-field
;;     (&whole form class field)
;;   "Compile constant class/field to jfield at compile time"
;;   (cond ((and (constantp class) (constantp field) (ignore-errors (find-java-class (eval class))))
;; 	 (let ((jfield (ignore-errors (get-java-field (eval class) (eval field)))))
;; 	   (if field
;; 	       `(load-time-value ,jfield)
;; 	       form)))
;; 	(t form)))
	 
;; (define-compiler-macro find-java-class 
;;     (&whole form name)
;;   (if (constantp name)
;;       (let ((class (ignore-errors (jclass (maybe-resolve-class-against-imports (eval name))))))
;; 	(if class
;; 	    `(load-time-value ,class)
;; 	    form))
;;       form))


