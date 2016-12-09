(in-package :jss)

;; This is the start of an extension of JSS to be able to use OSGI
;; Bundles.  Currently one can take advantage of the class hiding
;; aspect - with the visible packages listed in a JAR manifest
;; "Exported-Packages", those classes can be accessed while the other
;; can't

;; General use:
;; (add-bundle path-to-jar-file)
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

;; bundle arguments can either be a string result of
;; (#"getSymbolicName" bundle) or a bundle object loaded bundles are
;; in an association list in *loaded-osgi-bundles*, with each element
;; being (name object class-lookup-hash)

;; My primary use is to use a project with dependencies that conflict
;; with the jars I'm using.  Here's an example of how I package that
;; using maven. It is from module-bundle/pom.xml from the project
;; https://github.com/alanruttenberg/pagoda (that project uses maven
;; modules in order to also be able to run the usual packaging)
;;
;; <plugin>
;; boilerplate. The maven-bundle-plugin takes over the package phase from the default assembly plugin
;; 	<groupId>org.apache.felix</groupId>
;; 	<artifactId>maven-bundle-plugin</artifactId>
;; 	<extensions>true</extensions>
;; 	<configuration>
;; 	  <descriptorRefs>
;; This will be the prefix for the jar that is created. It will be prefix-<version>.jar
;; 	    <descriptorRef>pagoda-bundle</descriptorRef>
;; 	  </descriptorRefs>
;; 	  <instructions>
;; These are the packages that I want to be visible
;; 	    <Export-Package>uk.ac.ox.cs.pagoda.*,uk.ac.ox.cs.JRDFox.*</Export-Package>
;; This says to put absolutely every class/jar that they depend on in the created bundle
;; 	    <Embed-Dependency>*;scope=compile</Embed-Dependency>   
;; 	    <Embed-Transitive>true</Embed-Transitive>
;; This avoids having the bundle plugin write dependencies that imply
;; the jars of the dependency are also bundles. If they aren't then
;; you get link errors when trying to install the bundle. (sheesh!)
;; 	    <Import-Package/>
;; 	  </instructions>
;; 	</configuration>
;; </plugin>


(defvar *osgi-framework* nil)

(defun ensure-osgi-initialized ()
  (unless *osgi-framework*
    (when (not (ignore-errors (find-java-class 'org.osgi.framework.launch.FrameworkFactory)))
      (error "You need to put felix.jar in abcl's classpath. It can be downloaded from http://felix.apache.org/downloads.cgi - download the distribution and get it from the bin directory"))
    (let* ((ffs (#"load" 'ServiceLoader (find-java-class 'org.osgi.framework.launch.FrameworkFactory)))
	   (factory (#"next" (#"iterator" ffs)))
	   (framework (#"newFramework" factory +null+)))
      (#"start" framework)
      (setq *osgi-framework* framework))))

;; this: http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs is wrong!
;; Compute the offset using (#"currentTimeMillis" 'system)
(defun universal-to-bundle-time (universal-time)
  "Convert from lisp time to unix time in milliseconds, used by osgi"
  (let ((offset (- (get-universal-time) (floor (#"currentTimeMillis" 'system) 1000))))
    (* 1000 (- universal-time offset))))

;; Beware the cache. Bundles are installed in a cache folder and
;; reinstalling them without clearing that will cause a conflict - an
;; error about duplicates. (Among other things, the installation
;; unpacks the jars in the bundle and arranges the classpath to use
;; them).  While I believe that the cache will be refreshed if the
;; version number on the bundle is changed, it's annoying to do that
;; during development. Instead, if the bundle is already installed
;; (available via #"getBundles") then we compare the modification
;; dates of the installed verison and file-write-date of the jar, and
;; if the jar is newer uninstall the old bundle and install the new
;; one.

;; The cache is called felix-cache and gets put in the working
;; directory, which seems hard to predict if you are working in slime.
;; TBD: Control where the cache is placed and add some utility fn to
;; work with it.

;; Name is used to identify the bundle among the loaded bundles. If
;; not supplied then the "symbolic name" is used, the value of the
;; manifest header "Bundle-SymbolicName".

(defun add-bundle (jar &key name)
  (ensure-osgi-initialized)
  (setq jar (namestring (translate-logical-pathname jar)))
  (let* ((bundle-context (#"getBundleContext" *osgi-framework*))
	 (bundle (find jar (#"getBundles" bundle-context) :key #"getLocation" :test 'search)))
    (cl-user::print-db bundle (and bundle(#"getLastModified" bundle)  (universal-to-bundle-time (file-write-date jar))))
    (when (or (not bundle)
	      (< (#"getLastModified" bundle) 
		 (universal-to-bundle-time (file-write-date jar))))
      (when bundle
	(warn "reinstalling bundle ~a" jar)
	(#"uninstall" bundle))
      (setq bundle (#"installBundle" bundle-context (concatenate 'string "file:" jar))))

    (#"start" bundle)
    (let ((name (or name (#"getSymbolicName" bundle))))
      (let* ((index (index-class-names (bundle-exports bundle))))
	(setq *loaded-osgi-bundles* (remove name *loaded-osgi-bundles* :test 'equalp :key 'car))
	(push (list name bundle index) *loaded-osgi-bundles*)
	bundle))))

(defun bundle-headers (bundle)
  (loop with headers = (#"getHeaders" bundle)
	for key in (j2list (#"keys" headers))
	collect (list key (#"get" headers key) (#"get" headers key))))

(defun bundle-header (bundle key)
  (#"get"  (#"getHeaders" bundle) key))

;; Not useful yet
(defun bundle-capabilities (bundle)
  (let ((bundleWiring (#"adapt" bundle (find-java-class 'BundleWiring))))
    (loop with i = (#"iterator" (#"getCapabilities" bundlewiring +null+)) 
	  while (#"hasNext" i)
	  for cap = (#"next" i) 
	  for namespace = (#"getNamespace" cap)
	  for es = (#"entrySet" (#"getAttributes" cap)) 
	  collect (list* namespace cap (mapcar #"getValue" (set-to-list es))))))

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

(defun bundle-exports (bundle)
  (let ((entry (bundle-header bundle "Export-Package"))
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


;; Like find java class, but looks in a bundle. no-cache means don't
;; look for it like find-java-class and don't cache it for
;; find-java-class. Default currently is to do so, but I might change
;; the default, as it could lead to confusion in the case where both
;; find-java-class and find-bundle-class are used and there are two
;; version of the same class in the environment.

(defun find-bundle-class (bundle classname &key no-cache &aux bundle-entry)
  (cond ((stringp bundle) 
	 (setq bundle-entry (assoc bundle *loaded-osgi-bundles* :test 'equal))
	 (setq bundle (second bundle-entry)))
	((java-object-p bundle)
	 (setq bundle-entry (find bundle *loaded-osgi-bundles* :key 'second))))
  (assert bundle () "No bundle named ~a" bundle)
  ;; we'll allow one bundle to be in the cache. Check if we're the one.
  (or (let ((found (and (not no-cache) (gethash (string classname) *imports-resolved-classes*))))
	(and (consp found) (eq (car found) bundle) (second found)))
      (let ((found (lookup-class-name classname :table (third bundle-entry))))
	(if found
	    (progn 
	      (unless no-cache
		(unless (gethash classname *imports-resolved-classes*)
		  (setf (gethash classname *imports-resolved-classes*) (cons bundle found))))
	      (#"loadClass" bundle found))
	    (#"loadClass" bundle (string classname))))))
  

