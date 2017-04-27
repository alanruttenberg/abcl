(require :asdf)
(in-package :cl-user)

(asdf:defsystem jss
  :author "Alan Ruttenberg, Mark Evenson"
  :long-description "<urn:abcl.org/release/1.5.0/contrib/jss#>"
  :version "3.2.3" 
  :components ((:module base 
                        :pathname "" :serial t 
                        :components ((:file "packages")
                                     (:file "invoke")
                                     (:file "collections")
                                     (:file "classpath")
				     (:file "osgi")
				     (:file "optimize-java-call")
				     (:file "transform-to-field")
                                     (:file "compat"))))
  :perform (asdf:test-op (op c)
                         (asdf:test-system :jss/tests)))


(asdf:defsystem jss/tests
  :defsystem-depends-on (quicklisp-abcl
                         prove-asdf)
  :depends-on (jss
               prove)
  :components ((:module tests
                        :pathname "" 
                        :components ((:test-file "jss-tests"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))

;; Until prove-asdf works
(let ((where (merge-pathnames "jss-tests.lisp" (load-time-value *load-pathname*))))
  (defun cl-user::test-jss()
    (funcall (intern "QUICKLOAD" 'ql) :prove)
    (progv  (list (intern "*DEFAULT-TEST-FUNCTION*" :prove)) (list (function equalp))
      (print (intern "*DEFAULT-TEST-FUNCTION*" :prove))
      (funcall (intern "RUN" 'prove) where))))




