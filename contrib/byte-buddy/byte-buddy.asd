(in-package :asdf)
(asdf:defsystem :byte-buddy
  :author "Alan Ruttenberg, Mark Evenson"
  :version "3.2.2" 
  :description "<> asdf:defsystem <urn:abcl.org/release/1.5.0/contrib/jss#3.2.2>"
  :components 
  ((:mvn "net.bytebuddy/byte-buddy/1.6.4")
   (:mvn "net.bytebuddy/byte-buddy-agent/1.6.5")
   (:file "byte-buddy"))
;;  :defsystem-depends-on (:prove-asdf)
;;  :in-order-to ((test-op (test-op jss/tests)))
  )

;; Until prove-asdf works
(let ((where (merge-pathnames "byte-buddy-tests.lisp" (load-time-value *load-pathname*))))
  (defun cl-user::test-byte-buddy()
    (funcall (intern "QUICKLOAD" 'ql) :prove)
    (funcall (intern "RUN" 'prove) where)))

;; (asdf:defsystem :byte-buddy/tests
;;   :depends-on (byte-buddy)
;;   :components ((:module tests
;; 		:pathname "" 
;; 		:components ((:test-file "byte-buddy-tests"))
;; 		))
;;   :perform (test-op :after (op c)
;;                     (funcall (intern #.(string :run) :prove) c)))







   


