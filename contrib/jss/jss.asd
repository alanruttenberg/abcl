(in-package :asdf)
(asdf:defsystem :jss
  :author "Alan Ruttenberg, Mark Evenson"
  :version "3.2.0" 
  :description "<> asdf:defsystem <urn:abcl.org/release/1.5.0/contrib/jss#3.2.0>"
  :components ((:module base 
                        :pathname "" :serial t 
                        :components ((:file "packages")
                                     (:file "invoke")
				     (:file "optimize-java-call")
				     (:file "collections")
				     (:file "osgi")
                                     (:file "classpath")
				     (:file "transform-to-field")
                                     (:file "compat")
				     )))
;;  :defsystem-depends-on (:prove-asdf)
;;  :in-order-to ((test-op (test-op jss/tests)))
  )

;; Until prove-asdf works
(let ((where (merge-pathnames "jss-tests.lisp" (load-time-value *load-pathname*))))
  (defun cl-user::test-jss()
    (funcall (intern "QUICKLOAD" 'ql) :prove)
    (funcall (intern "RUN" 'prove) where)))

;; (asdf:defsystem :jss/tests
;;   :depends-on (jss)
;;   :components ((:module tests
;; 		:pathname "" 
;; 		:components ((:test-file "jss-tests"))
;; 		))
;;   :perform (test-op :after (op c)
;;                     (funcall (intern #.(string :run) :prove) c)))



#+nil FIXME
(asdf:defsystem :jss-tests
  :depends-on (jss abcl abcl-test-lisp)
  :components ((:module tests
                        :pathname "" 
                        :components ((:file "tests")))))




   


