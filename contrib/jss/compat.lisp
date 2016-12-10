(in-package :jss)

(defparameter *cl-user-compatibility* nil
  "Whether backwards compatibility with JSS's use of CL-USER has been enabled.")

(defun ensure-compatibility ()
  "Ensure backwards compatibility with JSS's use of CL-USER."
  (require 'abcl-asdf)
  (loop :for symbol :in '("ADD-DIRECTORY-JARS-TO-CLASS-PATH"
                          "NEED-TO-ADD-DIRECTORY-JAR?")
        :do 
          (unintern (intern symbol "CL-USER") :cl-user)
        :do
          (import (intern symbol "ABCL-ASDF") :cl-user))
  (let ((dont-export '(*cl-user-compatibility* add-to-classpath)))
    (loop :for symbol :being :each :external-symbol :in :jss 
       :when (not (find symbol dont-export))
         :do 
           (unintern symbol :cl-user)
         :and :do
           (shadowing-import symbol :cl-user)))
  (setf *cl-user-compatibility* t))

;;; Because we're the last file in the ASDF system at the moment
(provide 'jss)


    
