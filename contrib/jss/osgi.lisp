(in-package :jss)

;; This is the start of an extension of JSS to be able to use OSGI
;; Bundles.  Currently one can take advantage of the class hiding
;; aspect - with the visible packages listed in a JAR manifest
;; "Exported-Packages", those classes can be accessed while the other
;; can't

;; General use:
;; (add-bundle jar-file)
;; (find-java-class <some class exported from your bundle>)
;; Do stuff

;; The current implementation assumes you aren't aiming to have
;; multiple version of the same class exported from different bundles
;; in that find-java-class will try to complain when a class name is
;; ambiguous, but once a class is found it will continue to be found,
;; even if another version of the class becomes available in another
;; bundle. For finer control use:
;; (find-bundle-class bundle classname)
;; Class name can be abbreviated as with find-java-class

;; bundle arguments can either be a string result of (#"getSymbolicName" bundle) or a bundle object
;; loaded bundles are in an association list in *loaded-osgi-bundles*, with each element being 
;; (name object class-lookup-hash)

(defvar *osgi-framework* nil)

(defstruct osgi-bundle
  name
  bundle
  index
  source)
   
(defun ensure-osgi-initialized ()
  (unless *osgi-framework*
    (let* ((ffs (#"load" 'ServiceLoader (find-java-class 'org.osgi.framework.launch.FrameworkFactory)))
	   (factory (#"next" (#"iterator" ffs)))
	   (framework (#"newFramework" factory +null+)))
      (#"start" framework)
      (setq *osgi-framework* framework))))

(defun universal-to-bundle-time (universal-time)
  (* 1000 (- universal-time (encode-universal-time 0 0 0 1 1 1970 0))))

(defun add-bundle (jar)
  (ensure-osgi-initialized)
  (let* ((bundle-context (#"getBundleContext" *osgi-framework*))
	 (location (concatenate 'string "file://" (namestring (truename jar))))
	 (bundle (find location (#"getBundles" bundle-context) :key #"getLocation" :test 'equalp)))

    (when (or (not bundle)
	      (< (#"getLastModified" bundle) 
		 (universal-to-bundle-time (file-write-date jar))))
      (when bundle
	(warn "reinstalling bundle ~a" jar)
	(#"uninstall" bundle))
      (setq bundle (#"installBundle" bundle-context (concatenate 'string "file:" (namestring (truename jar))))))

    (#"start" bundle)
    (let* ((index (index-class-names (bundle-exports-from-manifest (jar-manifest jar) bundle)))
	   ;(struct (make-osgi-bundle :name (#"getSymbolicName" bundle) :bundle bundle :index index :source jar))
	   )
      (remove (#"getSymbolicName" bundle) *loaded-osgi-bundles* :test 'equalp :key 'car)
      (push (list (#"getSymbolicName" bundle) bundle index) *loaded-osgi-bundles*)

      bundle)))

(defun bundle-headers (bundle)
  (loop with headers = (#"getHeaders" bundle)
	for key in (j2list (#"keys" headers))
	collect (list key (#"get" headers key) (#"get" headers key))))

;; Not useful yet
(defun bundle-capabilities (bundle)
  (let ((bundleWiring (#"adapt" bundle (find-java-class 'BundleWiring))))
    (loop with i = (#"iterator" (#"getCapabilities" bundlewiring +null+)) 
	  while (#"hasNext" i)
	  for cap = (#"next" i) 
	  for namespace = (#"getNamespace" cap)
	  for es = (#"entrySet" (#"getAttributes" cap)) 
	  collect (list* namespace cap (mapcar #"getValue" (set-to-list es))))))

;; read the jar manifest and split into key value pairs

(defun jar-manifest (jar)
  (let* ((jar (new 'jarfile (namestring (truename jar))))
	 (entry(#"getEntry" jar "META-INF/MANIFEST.MF"))
	 (stream (new 'lisp.stream 'system::stream
		      (new 'BufferedReader
			   (new 'InputStreamReader (#"getInputStream" jar entry))))))
    (let ((string 
	    (with-output-to-string (s) 
	      (loop for line = (read-line stream nil :eof)
		    until (eq line :eof)
		    do (princ line s) (terpri s)))))
      (let ((props (split-at-char (#"replaceAll" string "(|\\n) " "") #\linefeed)))
	(mapcan (lambda(el) (all-matches el "^(.*?):\\s*(.*)" 1 2))
		props)))))

;; This is ugly but will do until there's a better way
;; The exported packages are listed in the jar manifest on the key "Export-Package"
;; Lines are separated by #\return with continuation lines starting with a space.
;; Once that is fixed, the format is a concatenation of entries like the below

;; package;key="...","..";key2="",
;; package2,
;; package3;..,

;; i.e. for each package there are some optional key values pairs
;; which we're not going to attend to now,

;; Step 1: Since there are "," inside the string we take this apart by
;; first emptying the strings, then splitting by ",", then tossing
;; anything past a ";"

;; Step 2: There may or may not be subpackages. Since we're going to
;; match on the prefix, we throw away everything but the prefix. This
;; is done by first sorting, then taking an element and comparing it
;; to subsequent ones. When the first start the other we toss the other.

;; Step 3: The bundlewiring interface lets one iterate over all
;; 'resources', which are like entries in a jar, some of which are
;; class files. We only want the exported class files, so we only keep
;; those that start with our prefix (.->/ to make it a path)

;; Step 4: Extract the class name from the path (keep ".class" at the end)

;; Learned about bundle wiring at
;; http://stackoverflow.com/questions/22688997/how-i-can-get-list-of-all-classes-from-given-bundle

;; Spun my wheels a while looking for a cleaner way to do this, but
;; its confusing. This should do for now.

(defun bundle-exports-from-manifest (props bundle)
  (let ((entry (second (find "Export-Package" props :test 'equalp :key 'car)))
	(bundleWiring (#"adapt" bundle (find-java-class 'BundleWiring))))
    (loop for package-prefix
	    in 
	    (loop with candidates = (sort (mapcar (lambda(el) (#"replaceAll" el ";.*$" "")) (split-at-char (#"replaceAll" entry "(\\\".*?\\\")" "") #\,))			 
					  'string-lessp)
		  for first = (pop candidates)
		  until (null candidates)
		  do (loop for next = (car candidates)
			   while (and next (eql 0 (search first next))) do (pop candidates))
		  collect first)
	    for path = (substitute #\/ #\. package-prefix) 
	  append
	  (loop for entry in (set-to-list (#"listResources" bundlewiring (concatenate 'string "/" path)
					       "*.class" (jfield (find-java-class 'BundleWiring) "FINDENTRIES_RECURSE")))
		for url = (#"toString" (#"getEntry" bundle entry))
		collect
		(substitute #\. #\/ (subseq 
		 (#"toString" (#"getEntry" bundle entry))
		 (search path url :test 'char=)))))))


(defun find-bundle-class (bundle classname &aux bundle-entry)
  (cond ((stringp bundle) 
	 (setq bundle-entry (assoc bundle *loaded-osgi-bundles* :test 'equal))
	 (setq bundle (second bundle-entry)))
	((java-object-p bundle)
	 (setq bundle-entry (find bundle *loaded-osgi-bundles* :key 'second))))
  (assert bundle () "No bundle named ~a" bundle)
  ;; we'll allow one bundle to be in the cache. Check if we're the one.
  (or (let ((found (gethash (string classname) *imports-resolved-classes*)))
	(and (consp found) (eq (car found) bundle) (second found)))
      (let ((found (lookup-class-name classname :table (third bundle-entry))))
	(if found
	    (progn 
	      (unless (gethash classname *imports-resolved-classes*)
		(setf (gethash classname *imports-resolved-classes*) (cons bundle found)))
	      (#"loadClass" bundle found))
	    (#"loadClass" bundle (string classname))))))
  

