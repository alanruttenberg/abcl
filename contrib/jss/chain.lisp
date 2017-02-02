(in-package :jss)

;; new ByteBuddy()
;;   .subclass(Object.class)
;;   .method(ElementMatchers.named("toString"))
;;   .intercept(FixedValue.value("Hello World!"))
;;   .make()
;;   .load(getClass().getClassLoader())
;;   .getLoaded();

;; ->

;; (java::chain (new 'ByteBuddy)
;; 	     ("subclass" (java::jclass "java.lang.Object"))
;; 	     ("method" (#"named" 'ElementMatchers "toString"))
;; 	     ("intercept" (#"value" 'FixedValue "Hello World!"))
;; 	     "make"
;; 	     ("load" (#"getClassLoader" (find-java-class 'lisp)))
;; 	     "getLoaded")

;; something like

;; (#".subclass(Object.class)
;;   .method(ElementMatchers.named("toString"))
;;   .intercept(FixedValue.value("Hello World!"))
;;   .make()
;;   .load(getClass().getClassLoader())
;;   .getLoaded();"
;; (new 'ByteBuddy))

;; Rule: If starts with . then read until ';" so I don't have to quote literal strings.

;; Otherwise:
;; (#"subclass(Object.class)
;;   .method(ElementMatchers.named(\"toString\"))
;;   .intercept(FixedValue.value(\"Hello World!\"))
;;   .make()
;;   .load(getClass().getClassLoader())
;;   .getLoaded();"
;;  (new 'ByteBuddy))


;; Come in if you see () somehwere.
;; If it starts with javachar+()
;; then
;; split at dots
;; elements with () read contents but with comma as separator
;; elements without change to field accessors
;; allow #"new ../abcl

(defun jss-is-chain? (string)
  (or (find #\( string)
      (>= (length (count #\. string)) 2)))

(defvar *chain-readtable* (copy-readtable))
(set-syntax-from-char #\, #\space *chain-readtable* *chain-readtable* )
(set-syntax-from-char #\. #\space *chain-readtable* *chain-readtable* )
(set-macro-character #\, (lambda(char stream) :comma) nil *chain-readtable*)
(set-macro-character #\. (lambda(char stream) :dot) nil *chain-readtable*)
(setf (readtable-case *chain-readtable*) :preserve)

(defun transform-jss-chain (string)
  (let ((*readtable* *chain-readtable*)) 
    (loop for start = 0 then pos
	  for (token pos) = (multiple-value-list (read-from-string string nil :eof :start start))
	  until (eq token :eof)
	  do (print  token))))
