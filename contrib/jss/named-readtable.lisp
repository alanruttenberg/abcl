(in-package :editor-hints.named-readtables)

(defreadtable :jss
  (:merge :standard)
  (:dispatch-macro-char #\# #\" 'jss::read-invoke))

(defreadtable :lsw
  (:merge :jss)
  (:macro-char  #\! 'cl-user::read-uri t))

  
