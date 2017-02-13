(defun binding-chain (x &optional so-far)
	   (if (null so-far)
	       (binding-chain x (list x))
	       (if #"{x}.next"
		   (binding-chain #"{x}.next" (cons #"{x}.next" so-far))
		   so-far)))

(defun outermost-binding (x)
  (car (binding-chain x)))

(defun add-to-outermost-binding (vars symbol)
  (set-java-field (outermost-binding vars) "next" (new-binding symbol 'bogus) t))

(defun new-binding (var value)
  (jnew (elt (#"getDeclaredConstructors" (find-java-class 'lisp.binding)) 0) var value +null+))
