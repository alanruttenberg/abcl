(in-package :jss)

(defvar *javaparsers* (make-hash-table :test 'equalp))
(defvar *class-to-last-component* (make-hash-table :test 'equalp))

(defclass sharp-quote-expression-reader () ())

(defmacro def-java-read (ast-class class fields &body body)
  (let ((jclass (find-java-class (concatenate 'string "com.github.javaparser.ast.expr." (string ast-class)))))
    `(progn
       (setf (gethash ,jclass *class-to-last-component*) ',ast-class)
       (defmethod ,ast-class ((obj ,class) node) 
	 (let ,(loop for field in fields 
		     for jfield across (#"getDeclaredFields" jclass)
		     collect (list field `#"{node}.{,jfield}"))
	   ,@body)))))

(defmethod ast-to-sexp ((r sharp-quote-expression-reader) node)
  (when (jinstance-of-p  node "java.util.Optional")
    (setq node (#"get" node)))
  (when (or (equal node (load-time-value (#"empty"  'java.util.Optional ))) (null node))
    (return-from ast-to-sexp nil))
  (funcall (gethash (jobject-class node) *class-to-last-component*)  r node))

(defmethod read-java-expression ((r sharp-quote-expression-reader) expression)
  (ast-to-sexp r (#"parseExpression" 'javaparser expression)))

(defun read-sharp-quote-expression (string)
  (read-java-expression (make-instance 'sharp-quote-expression-reader) string))

(defun maybe-class (el)
  (if (and (symbolp el) (upper-case-p (char (string el) 0)))
      `(find-java-class ',el)
      (if (symbolp el)
	  (intern (string-upcase el))
	  el)))

(def-java-read LongLiteralExpr sharp-quote-expression-reader nil
  (read-from-string (#"replaceFirst" (#"getValue" node) "L" "")))

(def-java-read BooleanLiteralExpr sharp-quote-expression-reader (value)
  (if (equal value "true") t nil))

(def-java-read IntegerLiteralExpr sharp-quote-expression-reader nil
 (parse-integer (#"getValue" node)))

(def-java-read DoubleLiteralExpr sharp-quote-expression-reader nil
  (let ((raw (#"getValue" node)))
    (setq raw (#"replaceAll" raw "_" ""))
    (if (#"matches" raw ".*[dD]$")
	(read-from-string (#"replaceFirst" (subseq raw 0 (1- (length raw))) "e" "d"))
	(if (#"matches" raw ".*[fF]$")
	    (read-from-string (subseq raw 0 (1- (length raw))))
	    (read-from-string raw)))))

(def-java-read CharLiteralExpr sharp-quote-expression-reader nil
  (#"getValue" node))

(def-java-read StringLiteralExpr sharp-quote-expression-reader nil
  (#"getValue" node))

(def-java-read NullLiteralExpr sharp-quote-expression-reader nil
  +null+)

(def-java-read ObjectCreationExpr sharp-quote-expression-reader (scope type typeArguments arguments anonymousClassBody)
  (declare (ignore anonymousClassBody typeArguments scope))
  `(new ',(ast-to-sexp obj (#"getName" type)) ,@(mapcar 'ast-to-sexp obj (j2list arguments)))
  )

(def-java-read MethodCallExpr sharp-quote-expression-reader (scope typeArguments name arguments)
  (declare (ignore typeArguments))
  (let* ((scope1 (ast-to-sexp obj scope))
	 (how  (if (and (symbolp scope1) (not (null scope1)) (upper-case-p (char (string scope1) 0))) 
		  'jstatic
		  'jcall)))
    (if (and (symbolp scope1) (not (null scope1)) (upper-case-p (char (string scope1) 0)))
	(setq scope1 `',scope1))
    `(,how ,(#"getIdentifier" name) ,(or scope1 'this) ,@(mapcar 'maybe-class 
								 (mapcar (lambda(el) (ast-to-sexp obj el))
									 (j2list arguments))))
    ))

(def-java-read FieldAccessExpr sharp-quote-expression-reader ()
  (let ((scope (ast-to-sexp obj (#"getScope" node))))
    (if (and (symbolp scope) (upper-case-p (char (string scope) 0)))
	`(get-java-field ',(ast-to-sexp obj (#"getScope" node)) ,(#"getIdentifier" (#"getField" node)) t)
	`(get-java-field ,(maybe-class (ast-to-sexp obj (#"getScope" node))) ,(#"getIdentifier" (#"getField" node)) t))))

(def-java-read ArrayAccessExpr sharp-quote-expression-reader ()
  (let ((index (ast-to-sexp obj (#"getIndex" node))))
    (if (symbolp index) (setq index (intern (string-upcase index))))
    `(aref ,(ast-to-sexp obj (#"getName" node)) ,index)))

(def-java-read ClassExpr sharp-quote-expression-reader (type)
  `(find-java-class ',(ast-to-sexp obj (#"getName" type))))

(def-java-read SimpleName sharp-quote-expression-reader ()
  (let ((symbol (intern (#"getIdentifier" node))))
    symbol))

(def-java-read NameExpr sharp-quote-expression-reader ()
  (ast-to-sexp obj (#"getName" node)))


#|(defun def-java-parse-1 (ast-class fields body)
  (register 
  
			  
(def-java-read MethodReferenceExpr (scope typeArguments identifier)
  )
(def-java-read Name (identifier qualifier)
  )
(def-java-read NameExpr (name)
  )
(def-java-read ObjectCreationExpr (scope type typeArguments arguments anonymousClassBody)
  )
(def-java-read SimpleName (identifier)
  )
(def-java-read SingleMemberAnnotationExpr (memberValue)
  )
(def-java-read StringLiteralExpr (value)
  )
(def-java-read SuperExpr (classExpr)
  )
(def-java-read ThisExpr (classExpr)
  )
(def-java-read TypeExpr (type)
  )
(def-java-read UnaryExpr (expression operator)
  )
(def-java-read VariableDeclarationExpr (modifiers annotations variables)
  )

AssignExpr$Operator: (ASSIGN PLUS MINUS MULTIPLY DIVIDE AND OR XOR REMAINDER LEFT_SHIFT SIGNED_RIGHT_SHIFT UNSIGNED_RIGHT_SHIFT codeRepresentation $VALUES)
BinaryExpr$Operator: (OR AND BINARY_OR BINARY_AND XOR EQUALS NOT_EQUALS LESS GREATER LESS_EQUALS GREATER_EQUALS LEFT_SHIFT SIGNED_RIGHT_SHIFT UNSIGNED_RIGHT_SHIFT PLUS MINUS MULTIPLY DIVIDE REMAINDER codeRepresentation $VALUES)
UnaryExpr$Operator: (PLUS MINUS PREFIX_INCREMENT PREFIX_DECREMENT LOGICAL_COMPLEMENT BITWISE_COMPLEMENT POSTFIX_INCREMENT POSTFIX_DECREMENT codeRepresentation isPostfix $VALUES)

(defun ast-to-sexp (node) 
  (flet ((fields-alist (node)
	   (loop for field across (#"getDeclaredFields" (jobject-class node))
		 collect (list (intern (string-upcase (#"getName" field)) 'keyword) (get-java-field node (#"getName" field) t)))))
    (let ((children (j2list (#"getChildNodes" node))))
      (cond ((null children) (cons node (fields-alist node)))
	    (t (list (cons node (fields-alist node))
		     (mapcar 'ast-to-sexp children)))))))


com.github.javaparser.ast.expr.AnnotationExpr: (name)
com.github.javaparser.ast.expr.ArrayAccessExpr: (name index)
com.github.javaparser.ast.expr.ArrayCreationExpr: (levels elementType initializer)
com.github.javaparser.ast.expr.ArrayInitializerExpr: (values)
com.github.javaparser.ast.expr.AssignExpr: (target value operator)
com.github.javaparser.ast.expr.BinaryExpr: (left right operator)
com.github.javaparser.ast.expr.BooleanLiteralExpr: (value)
com.github.javaparser.ast.expr.CastExpr: (type expression)
com.github.javaparser.ast.expr.CharLiteralExpr: nil
com.github.javaparser.ast.expr.ClassExpr: (type)
com.github.javaparser.ast.expr.ConditionalExpr: (condition thenExpr elseExpr)
com.github.javaparser.ast.expr.DoubleLiteralExpr: nil
com.github.javaparser.ast.expr.EnclosedExpr: (inner)
com.github.javaparser.ast.expr.Expression: nil
com.github.javaparser.ast.expr.FieldAccessExpr: (scope typeArguments name)
com.github.javaparser.ast.expr.InstanceOfExpr: (expression type)
com.github.javaparser.ast.expr.IntegerLiteralExpr: nil
com.github.javaparser.ast.expr.LambdaExpr: (parameters isEnclosingParameters body)
com.github.javaparser.ast.expr.LiteralExpr: nil
com.github.javaparser.ast.expr.LongLiteralExpr: nil
com.github.javaparser.ast.expr.MarkerAnnotationExpr: nil
com.github.javaparser.ast.expr.MemberValuePair: (name value)
com.github.javaparser.ast.expr.MethodCallExpr: (scope typeArguments name arguments)
com.github.javaparser.ast.expr.MethodReferenceExpr: (scope typeArguments identifier)
com.github.javaparser.ast.expr.Name: (identifier qualifier)
com.github.javaparser.ast.expr.NameExpr: (name)
com.github.javaparser.ast.expr.NormalAnnotationExpr: (pairs)
com.github.javaparser.ast.expr.NullLiteralExpr: nil
com.github.javaparser.ast.expr.ObjectCreationExpr: (scope type typeArguments arguments anonymousClassBody)
com.github.javaparser.ast.expr.SimpleName: (identifier)
com.github.javaparser.ast.expr.SingleMemberAnnotationExpr: (memberValue)o
com.github.javaparser.ast.expr.StringLiteralExpr: (value)
com.github.javaparser.ast.expr.SuperExpr: (classExpr)
com.github.javaparser.ast.expr.ThisExpr: (classExpr)
com.github.javaparser.ast.expr.TypeExpr: (type)
com.github.javaparser.ast.expr.UnaryExpr: (expression operator)
com.github.javaparser.ast.expr.VariableDeclarationExpr: (modifiers annotations variables)
com.github.javaparser.ast.ImportDeclaration: (name isStatic isAsterisk)

com.github.javaparser.ast.expr.

(def-java-read ArrayAccessExpr (name index)
  `(aref ,name ,index))
(def-java-read ArrayCreationExpr (levels elementType initializer)
  )
(def-java-read ArrayInitializerExpr (values)
  )
(def-java-read AssignExpr (target value operator)
  )
(def-java-read BinaryExpr (left right operator)
  )
(def-java-read CastExpr (type expression)
  )

(def-java-read ClassExpr (type)
  )
(def-java-read ConditionalExpr (condition thenExpr elseExpr)
  )

(def-java-read EnclosedExpr (inner)
  )
(def-java-read Expression nil
  )
(def-java-read FieldAccessExpr (scope typeArguments name)
  )
(def-java-read InstanceOfExpr (expression type)
  )

(def-java-read LiteralExpr nil
  )

(def-java-read LambdaExpr (parameters isEnclosingParameters body)
  )
(def-java-read MarkerAnnotationExpr nil
  )
(def-java-read MemberValuePair (name value)
  )

;;

|#
