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
;;   .load(!!THIS!!.getClass().getClassLoader())
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

(defun split-by-token (sequence token)
  (loop for this in sequence
	with accum = nil
	if (eq this token) 
	  collect (prog1 (reverse accum) (setq accum nil)) into them
	else 
	  do (push this accum)
	finally 
	   (return (append them (list (reverse accum))))))

(defun read-java-chain-expression (string)
  (let ((*readtable* *chain-readtable*)) 
    (loop for start = 0 then pos
	  for (token pos) = (multiple-value-list (read-from-string string nil :eof :start start))
	  until (eq token :eof)
	  collect  token)))

(defun parse-jss-chain (string &aux takes-argument)
  (when (char= (char string 0) #\.)
    (setq string (subseq string 1))
    (setq takes-argument t))
  (let ((body (parse-read-chain (split-by-token (read-java-chain-expression string) :dot))))
    (if takes-argument
	`(lambda(_) ,@body)
	`(lambda() ,@body))))

(defun parse-read-chain (list)
  (mapcar 'parse-chain-element list))

(defun parse-chain-element(el)
  (if (atom el)
      (if (symbolp el) (intern (string-upcase (string el))) el)
      (cond ((= (length el) 1)
	     (list (intern (string-upcase (car el)))))
	    ((eq (car el) '|new|)
	     `(jnew ,(second el) ,@(mapcar 'parse-chain-element (car (cddr el))) ,@(parse-comma (cdddr el))))
	    ((and (eq (second el) :dot) (symbol (third el)) (consp (fourth el)) ) 
	     `((jstatic ,(string (third el)) (find-java-class ',(first el)) ,(parse-chain-element (car (fourth el))) ,@(parse-comma (cdr (fourth el))))
	       ))
	    ((and (eq (second el) :dot) (symbol (third el)) )
	     (if (equal (third el) '|class|)
		 `((find-java-class ',(first el))
		   ,@(parse-comma (cdddr el)))
		 `((get-java-field (find-java-class ',(first el)) ,(string (third el)))
		   ,@(parse-comma (cdddr el)))))
	    ((member :dot el)
	     `((:chain ,@(mapcan 'parse-chain-element (split-by-token  el :dot)))))
	    ((listp (second el))
	     `((jcall ,(string (first el)) _ ,@(parse-chain-element (second el))) ,@(parse-comma (cddr el)))))))

(defun parse-comma (el)
  (if (null el)
      nil
      (progn
	(assert (eq (car el) :comma) (e))
	(parse (cdr el)))))

#|
(jss::parse-jss-chain ".subclass(Object.class)
   .method(ElementMatchers.named(\"toString\"))
   .intercept(FixedValue.value(string))
   .make()
   .load(getClass().getClassLoader())
   .getLoaded()")

(lambda (_)
  (let ((_ (jcall "subclass" _ (find-java-class '|Object|))))
    ((jcall "method"
	    _
	    (jstatic "named"
		     (find-java-class '|ElementMatchers|)
		     "toString")))
    ((jcall "intercept"
	    _
	    (jstatic "value" (find-java-class '|FixedValue|) string)))
    ((jcall "make" _))
    ((jcall "load"
	    _
	    (:chain (jcall "getClass" _) (jcall "getClassLoader" _))))
    ((jcall "getLoaded" _))))

(lambda (_)
  (let ((_ (jcall "subclass" _ (find-java-class '|Object|))))
    (let ((_ (jcall "method" _ (jstatic "named" (find-java-class '|ElementMatchers|) "toString"))))
      (let ((_  (jcall "intercept" _ (jstatic "value" (find-java-class '|FixedValue|) string))))
	(let ((_ (jcall "make" _)))
	  (let ((_ (jcall "load" _ (let ((__ (jcall "getClass" _))) (jcall "getClassLoader" __)))))
	    (let ((_ (jcall "getLoaded" _)))
	      )))))))


(jss::parse-jss-chain "new ByteBuddy().subclass(Object.class)
   .method(ElementMatchers.named(\"toString\"))
   .intercept(FixedValue.value(string))
   .make()
   .load(getClass().getClassLoader())
   .getLoaded()")

(lambda nil
  (jnew |ByteBuddy|)
  ((jcall "subclass" _ (find-java-class '|Object|)))
  ((jcall "method"
          _
          (jstatic "named"
                   (find-java-class '|ElementMatchers|)
                   "toString")))
  ((jcall "intercept"
          _
          (jstatic "value" (find-java-class '|FixedValue|) string)))
  ((jcall "make" _))
  ((jcall "load"
          _
          (:chain (jcall "getClass" _) (jcall "getClassLoader" _))))
  ((jcall "getLoaded" _)))



LOTS of STUMBLING

(parse '(sym :dot field))
(parse '(bar (sym :dot |class|) :dot (bar nil)))
(parse '(sym :dot class (arg)))
(parse '(sym :dot field :comma sym :dot field))
(parse '(sym (s2 :dot class :comma s3 (s4 ()))))





(parse '(|load| (|getClass| nil :dot |getClassLoader| nil)))

(parse '(|FixedValue| :dot |value| ("Hello World!")))

"new ByteBuddy().subclass(Object.class,t)
   .method(ElementMatchers.named(\"toString\"))
   .intercept(FixedValue.value(\"Hello World!\"))
   .make()
   .load(getClass().getClassLoader())
   .getLoaded()"

read-java-chain-expression ->

(|new| |ByteBuddy| nil :dot
 |subclass| (|Object| :dot |class| :comma |t|) :dot
 |method| (|ElementMatchers| :dot |named| ("toString")) :dot
 |intercept| (|FixedValue| :dot |value| ("Hello World!")) :dot 
 |make| nil :dot
 |load| (|getClass| nil :dot |getClassLoader| nil) :dot 
 |getLoaded| nil)

split-by-token ... :dot

(
(|new| |ByteBuddy| nil) 
(|subclass| (|Object| :dot |class| :comma |t|)) 
(|method| (|ElementMatchers| :dot |named| ("toString"))) 
(|intercept| (|FixedValue| :dot |value| ("Hello World!"))) 
(|make| nil) 
(|load| (|getClass| nil :dot |getClassLoader| nil)) 
(|getLoaded| nil)
)

chain := chain-first [:dot chain-next]* :dot chain-last
chain-first := ("new" expr args) | symbol | call
chain-next := call
chain-last := symbol | function-expr 
call := (symbol args)
args := function-expr [:comma function-expr]*






expr := call | field
args := arg [:comma arg]*
arg := 


args:= chain | 
 
inner := nil | symbol | list
inner (list) := extra arguments

inner:= (static-call | object-call) (:comma static-call)*
inner:= (static-call [:dot inner]+)
static-call := matches (sym :dot sym)
object-call := matches (sym (and (listp _) (eat _ as ))


---

(#"foo.bar(a).field")
(jfield (jstatic bar (jclass foo) a) field)
(#"foo.bar(a).field" arg) -> error

(#"foo.bar(a).field()")
(jcall field (jstatic bar (jclass foo) a))
(#"foo.bar(a).field()" args) -> error

(#".bar(a).field()"  arg)
(jcall field (jcall bar arg a)) if arg value
(jcall field (jstatic bar (jclass arg) a)) if arg symbol
error if no arg

two types of expression:
  one which expects the result of the previous as a first argument.

chain := chain-first [:dot chain-next]* :dot chain-last
chain-first := ("new" class-expr args) | (symbol [arg :comma arg]
call := (symbol args) ; last result is an arg
args := arg-expr [:comma arg-expr]
arg-expr := (symbol :dot symbol) | (symbol (args))

*chain-next := call
chain-last := symbol | call 






(parse-chain chain)
=
  parse-chain-first (first chain)
  parse-chain-next (butlast (cdr chain))
  part-chain-last (car (last chain)))

(parse-chain-first chain-first) 
=
  (cond ((eq (car chain-first) 'new)
	 `(jnew ,(class-expr (second chain)) ,@(parse-args (cddr chain))))
	((symbolp (car chain-first))
	 `(jstatic (jclass (car chain-first)) ,@(parse-args (cdr chain))))
	

parse-class-expr (e)
  (cond ((eq (car e)))

   
---

Chain.
At each step in the chain we are either producing a field value or an object or a class
If a field value the value is the first argument of the next thing
If an object then the object is passed forward and is argument to jcall
if a class then the class if passed forward and is argument to jstatic


(
(|new| |ByteBuddy| nil)  ; a class
(|subclass| (|Object| :dot |class| :comma |t|)) ; jstatic -> object
(|method| (|ElementMatchers| :dot |named| ("toString"))) ; jcall -> object
(|intercept| (|FixedValue| :dot |value| ("Hello World!"))) ; jcall -> object
(|make| nil) 				; jcall -> object
(|load| (|getClass| nil :dot |getClassLoader| nil)) ; jcall -> object
(|getLoaded| nil) jcall -> object
)

(|Object| :dot |class| :comma |t|) -> args = 'object , t
(|ElementMatchers| :dot |named| ("toString")) = arg (jstatic "named" '|ElementMatchers| "toString")


(jcall foo _ (|new| |ByteBuddy| nil) (|new| |ByteBuddy| nil)) -only a chain next eval to object


compare 
(|Object| :dot |class| :comma |t|)
(|ElementMatchers| :dot |named| ("toString"))

match 
    (sym :dot sym) 
    (sym :dot sym (x))
    (sym :dot sym :comma _
    (sym (x) <- this one is ambigious unless there are two elements and no comma
    (sym (x) :comma x
    (sym x ...)
|#
