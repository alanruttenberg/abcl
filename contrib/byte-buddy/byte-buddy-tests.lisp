;; stub

(in-package :cl-user)

(defpackage byte-buddy-test
  (:use :cl :cl-user :jss :prove :java))

(in-package :byte-buddy-test)

(plan 1)

;; http://bytebuddy.net/ hello world example
(is 
 (#"toString" 
  (#"newInstance" 
   (java::chain (new 'ByteBuddy)
		("subclass" (java::jclass "java.lang.Object"))
		("method" (#"named" 'ElementMatchers "toString"))
		("intercept" (#"value" 'FixedValue "Hello World!"))
		"make"
		("load" (#"getClassLoader" (find-java-class 'lisp)))
		"getLoaded")))
 "Hello World!")

(finalize)
