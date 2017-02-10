(in-package :jss)

(defvar *javaparsers* (make-hash-table :test 'equalp))

(defmacro def-javaparse (ast-class fields &body body)
  (let ((class (find-java-class (concatenate 'string "com.github.javaparser.ast.expr." (string ast-class))))
	(fname (intern (format nil "PARSE-~a" (string-upcase (string ast-class))))))
    `(setf (gethash ,class *javaparsers*)
	   (flet ((,fname (node)
		    (let ,(loop for field in fields 
				for jfield across (#"getDeclaredFields" class)
				collect (list field `#"{node}.{,jfield}"))
		      ,@body)))
	     #',fname))))

(def-javaparse LongLiteralExpr nil
  (#"getValue" node))

(def-javaparse BooleanLiteralExpr (value)
  (#"getValue" node))
(def-javaparse IntegerLiteralExpr nil
  (#"getValue" node))
(def-javaparse DoubleLiteralExpr nil
  (#"getValue" node))
(def-javaparse CharLiteralExpr nil
  (#"getValue" node))
(def-javaparse StringLiteralExpr nil
  (#"getValue" node))
(def-javaparse NullLiteralExpr nil
  +null+)

(def-javaparse ObjectCreationExpr (scope type typeArguments arguments anonymousClassBody)
  (declare (ignore anonymousClassBody typeArguments scope))
  `(new ',(ast-to-sexp (#"getName" type)) ,@(mapcar 'ast-to-sexp (j2list arguments)))
  )
    
(defun ast-to-sexp (node)
  (when (jinstance-of-p  node "java.util.Optional")
    (setq node (#"get" node)))
  (when (or (equal node (load-time-value (#"empty"  'java.util.Optional ))) (null node))
    (return-from ast-to-sexp nil))
  (funcall (gethash (jobject-class node) *javaparsers*) node))

(defun read-java-expression (expression)
  (ast-to-sexp (#"parseExpression" 'javaparser expression)))

(defun maybe-class (el)
  (if (and (symbolp el) (upper-case-p (char (string el) 0)))
      `(find-java-class ',el)
      (if (symbolp el)
	  (intern (string-upcase el))
	  el)))

(def-javaparse MethodCallExpr (scope typeArguments name arguments)
  (declare (ignore typeArguments))
  (let* ((scope1 (ast-to-sexp scope))
	 (how  (if (and (symbolp scope1) (not (null scope1)) (upper-case-p (char (string scope1) 0))) 
		  'jstatic
		  'jcall)))
    (if (and (symbolp scope1) (not (null scope1)) (upper-case-p (char (string scope1) 0)))
	(setq scope1 `',scope1))
    `(,how ,(#"getIdentifier" name) ,(or scope1 'this) ,@(mapcar 'maybe-class (mapcar 'ast-to-sexp (j2list arguments))))
    ))

(def-javaparse FieldAccessExpr ()
  (let ((scope (ast-to-sexp (#"getScope" node))))
    (if (and (symbolp scope) (upper-case-p (char (string scope) 0)))
	`(get-java-field ',(ast-to-sexp (#"getScope" node)) ,(maybe-class (ast-to-sexp (#"getField" node))))
	`(get-java-field ,(maybe-class (ast-to-sexp (#"getScope" node))) ,(maybe-class (ast-to-sexp (#"getField" node)))))))

(def-javaparse ClassExpr (type)
  `(find-java-class ',(ast-to-sexp (#"getName" type))))

(def-javaparse SimpleName ()
  (let ((symbol (intern (#"getIdentifier" node))))
    symbol))

(def-javaparse NameExpr ()
  (ast-to-sexp (#"getName" node)))



#|(defun def-java-parse-1 (ast-class fields body)
  (register 
  
			  
(def-javaparse MethodReferenceExpr (scope typeArguments identifier)
  )
(def-javaparse Name (identifier qualifier)
  )
(def-javaparse NameExpr (name)
  )
(def-javaparse ObjectCreationExpr (scope type typeArguments arguments anonymousClassBody)
  )
(def-javaparse SimpleName (identifier)
  )
(def-javaparse SingleMemberAnnotationExpr (memberValue)
  )
(def-javaparse StringLiteralExpr (value)
  )
(def-javaparse SuperExpr (classExpr)
  )
(def-javaparse ThisExpr (classExpr)
  )
(def-javaparse TypeExpr (type)
  )
(def-javaparse UnaryExpr (expression operator)
  )
(def-javaparse VariableDeclarationExpr (modifiers annotations variables)
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
com.github.javaparser.ast.expr.SingleMemberAnnotationExpr: (memberValue)
com.github.javaparser.ast.expr.StringLiteralExpr: (value)
com.github.javaparser.ast.expr.SuperExpr: (classExpr)
com.github.javaparser.ast.expr.ThisExpr: (classExpr)
com.github.javaparser.ast.expr.TypeExpr: (type)
com.github.javaparser.ast.expr.UnaryExpr: (expression operator)
com.github.javaparser.ast.expr.VariableDeclarationExpr: (modifiers annotations variables)
com.github.javaparser.ast.ImportDeclaration: (name isStatic isAsterisk)

com.github.javaparser.ast.expr.

(def-javaparse ArrayAccessExpr (name index)
  `(aref ,name ,index))
(def-javaparse ArrayCreationExpr (levels elementType initializer)
  )
(def-javaparse ArrayInitializerExpr (values)
  )
(def-javaparse AssignExpr (target value operator)
  )
(def-javaparse BinaryExpr (left right operator)
  )
(def-javaparse CastExpr (type expression)
  )

(def-javaparse ClassExpr (type)
  )
(def-javaparse ConditionalExpr (condition thenExpr elseExpr)
  )

(def-javaparse EnclosedExpr (inner)
  )
(def-javaparse Expression nil
  )
(def-javaparse FieldAccessExpr (scope typeArguments name)
  )
(def-javaparse InstanceOfExpr (expression type)
  )

(def-javaparse LiteralExpr nil
  )

(def-javaparse LambdaExpr (parameters isEnclosingParameters body)
  )
(def-javaparse MarkerAnnotationExpr nil
  )
(def-javaparse MemberValuePair (name value)
  )

;;

|#
