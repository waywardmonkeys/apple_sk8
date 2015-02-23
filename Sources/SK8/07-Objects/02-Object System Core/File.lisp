;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


;;; Mac-file-type normally follows aliases.  Now it optionally won't.
;;; This will be important if we want to ask an alias if it's a file or a folder.
(in-package :ccl)
(eval-when (:compile-toplevel)
  (defconstant $fdType	#x20))

;;; from l1-file.lisp
(let ((*warn-if-redefine-kernel* nil))
  
  (defun %mac-file-type-or-creator (offset path &optional dont-resolve)
    (when (directory-pathname-p path) (signal-file-error $err-no-file path))
    (%stack-iopb (pb np)
      (%path-to-iopb path pb :errchk dont-resolve)
      (%get-ostype pb offset)))
  
  (defun mac-file-type (path &optional dont-resolve)
    (%mac-file-type-or-creator $fdType path dont-resolve)))


(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  7-12-94  11:28 am
                  SK8::FINDFILEINPROJECT SK8::IMPORTABLEDATAFORK SK8::IMPORTTYPES SK8::SK8IMPORTABLERESOURCETYPESAVAILABLE
                  )

(defmacro assure-OSTypeString (var)
  `(progn
     (unless (and (stringp ,var) (= (length ,var) 4))
       (error "~a, ~a, must be a four character string or symbol" ',var ,var))
     ,var))

;;;  Files and Streams.
;;;  -- till

;;; Replace instances of this macro with the real thing when
;;; we get type checking.
(defmacro check-sk8-type (var type)
  `(unless (inheritsfrom ,var ,type)
     (error "~a needs to be a ~a" ,var ,type)))

(defun coerce-to-string (thing)
  (when thing
    (when (streamp thing) (setq thing (ccl::stream-filename thing)))
    (when (pathnamep thing) (setq thing (namestring thing)))
    (coerce thing 'string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  DirectoryNickname Objects
;;;  Now the physicalName is kept in the MCL translations table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DirectoryNickname used to be called FileAlias
;;; FileAlias used to be called PathnameAlias.
;;;
;;; name is the logical name.
;;; physicalName is the physical name.
;;; They should both be strings.
(new object :objectname "DirectoryNickname"
     :project sk8
     :properties '(logicalName))


(define-handler writeObject (DirectoryNickname str rereadably)
  (unless (maybeWriteObjectName me str)
    (when rereadably (write-string "the DirectoryNickname " str))
    (writeObject (logicalName me) str t)))

;;; Root print's differently because it's translation isn't simple.
(define-handler print-object :hidden (DirectoryNickname str)
  (if (objectname me)
    (call-next-method)
    (format str "[a DirectoryNickname~@[ \"~a\"~]~@[ -> \"~a\"~]]"
            (ignore-errors (logicalName me)) (ignore-errors (physicalname me)))))

;;; The physicalname is grabbed from the Lisp logical pathname translations table.
(define-handler physicalName (DirectoryNickname)
  (let ((dir (second (car (last (logical-pathname-translations (logicalname me)))))))
    (when dir
      (namestring 
       (make-pathname
        :directory (remove :wild-inferiors (pathname-directory dir)))))))


;;; Setting the logicalName or physicalName works by frobbing the 
;;; Lisp Logical Pathname Translations Table. 
(define-handler (setf physicalname) (value DirectoryNickname)
  (with-slots (logicalname) me
    (and logicalname
         value
         (let* ((pn (sk8-filename-to-mcl-pathname value))
                (pn-dir (pathname-directory pn)))
           (setf (logical-pathname-translations logicalname)
                 (list (list "**;" 
                             (make-pathname 
                              :defaults pn
                              :directory (append (or pn-dir '(:absolute))
                                                 '(:wild-inferiors))))))))))


;;; It's completely inappropriate to have multiple PathnameAliases with the 
;;; same logicaNames.
(define-handler (setf logicalname) (value DirectoryNickname)
  (when (find value (knownchildren DirectoryNickname) :key #'logicalName :test #'string-equal)
    (error "DirectoryNickname ~a alias already exists" value))
  (with-slots (logicalname) me
    (cond 
     ((ccl::logical-host-p logicalname)
      (setf (logical-pathname-translations value) 
            (logical-pathname-translations logicalname))
      (setf (logical-pathname-translations logicalname) nil))
     (t
      (setf (logical-pathname-translations value) nil)))
    (setf logicalname value)))


(define-handler findFileObjectNamed (String &key ((:project InProject)))
  (dolist (child (knownchildren File))
    (and (or (null inProject) 
             (eq (project child) inProject))
         (string-equal (logicalname child) me)
         (return child))))

;;; Like the builtin drive-name function, but returns nil when that number
;;; doesn't exist (as opposed to erroring out).
(defun drive-name-soft (numberr)
  (setq numberr (require-type numberr '(integer #x-8000 #x7fff)))
  (ccl::%stack-iopb (pb np)
    (%put-word pb 0 28)
    (%put-word pb numberr 22)
    (when (zerop (#_HGetVInfo  pb))
      (make-pathname :directory (%get-string np) :defaults nil))))

;;; Get all the volumes.
;;; I know full well this is a kludge, and, as Ross Perot says, if someone 
;;; can suggest a better way to do it I'm all ears.  I can look up a volume 
;;; name from a "drive number" (a small negative integer), but I don't know 
;;; how many drives there are.  So we go through, and if I hit 100 non-drives
;;; I figure we've searched hard enough.
;;; 
;;; Returns a list of pn's.
(defun mounted-volumes ()
  (loop with empties = 0
        for i from -1 above #x-8000
        until (> empties 100)
        as vol = (drive-name-soft i)
        if vol collect vol else do (incf empties)))


;;; Root is the desktop folder.
;;; Under it are a bunch of volumes, each with it's own desktop folder.
;;;
;;; now uses drive-name-soft and mounted-volumes to more reliably grab the main desktop.
(defun reset-root-alias ()
  (setf (logical-pathname-translations "Root")
        (nconc 
         (mapcar #'(lambda (each) 
                     (list (make-pathname :host "Root"
                                          :directory (append (pathname-directory each)
                                                             '(:wild-inferiors)))
                           (make-pathname :directory (append (pathname-directory each)
                                                             '(:wild-inferiors)))))
                 (mounted-volumes))
         (list (list "Root:**;" (make-pathname
                                 :directory (append 
                                             (pathname-directory (drive-name-soft 0))
                                             '("Desktop Folder" :wild-inferiors))))))))

  
;;; Converts from SK8 style filename syntax to MCL style filename syntax.
;;;   type can be:
;;;    :physical  physical pathnames only, 
;;;    :logical   logical pathnames only,
;;;     nil       either type is fine.
;;;   error can be: 
;;;     nil       return nil if not appropriate type
;;;     non-nil   error if not appropriate type.

(defun sk8-filename-to-mcl-namestring (filename &key type error)
  (namestring (sk8-filename-to-mcl-pathname filename :type type :error error)))


;;; Returns a list of File objects representing the mounted volumes on this machine.
;;; The first one is the startup volume.
(define-sk8-function mountedVolumes nil (&key ((:project proj)))
  (loop for vol in (mounted-volumes)
        collecting (new file :ospathname vol :project proj)))

;;; Converts from SK8 style filename syntax to an MCL pathname object.
;;; keyword args as above.
;;;
;;; Note to myself: Originally this tried real hard to make a logical file object
;;; by doing a (make-pathname :defaults pn :host "Root") if it wasn't.
(defun sk8-filename-to-mcl-pathname (filename &key type error)
  (when (pathnamep filename) (setf filename (nameString filename)))
  (let ((found-semicolon-first nil)
        (found-colon nil)
        (filename-length (length filename)))
    (dotimes (i filename-length)
      (case (char filename i)
        (#\; (unless found-colon (setq found-semicolon-first t)))
        (#\: (setq found-colon t))))
    (when found-semicolon-first 
      (setq filename (copy-seq filename)) 
      (dotimes (i filename-length)
        (case (char filename i)
          (#\: (setf (char filename i) #\;))
          (#\; (setf (char filename i) #\:)))))
    (let* ((pn (parse-namestring filename))
           (log-pn (typep pn 'logical-pathname)))
      (cond 
       ((and log-pn (eq type :physical))
        (when error (error "Must be a physical pathname.")))
       ((and (null log-pn) (eq type :logical))
        (when error (error "Must be a logical pathname.")))
       (t pn)))))



;;; Converts from MCL style filename syntax to SK8 style filename syntax.
;;; Uses whatever logical host it can for maximum portability of the resultant pathname
(defun mcl-namestring-to-sk8-filename (mcl-namestring)
  (let* ((pn (parse-namestring (backtranslate-physical-pathname mcl-namestring)))
         (pn-host (pathname-host pn)))
    (when (or (null pn-host) (eq pn-host :unspecific))
      (setq pn (make-pathname :defaults pn :host "Root")))
    (namestring-hacking-logical-syntax pn)))
 
;;; Useful DirectoryNicknamees.
;;; RootDirectoryNickname:   DirectoryNickname for the Desktop Folder "Root" --> "<disk>:Desktop;"
;;; SK8DirectoryNickname:    DirectoryNickname for the SK8 directory. "SK8"  --> "CCL;"


;;; The root directory is special, and we have to lie a little.
(new DirectoryNickname
     :project SK8
     :objectName "RootDirectoryNickname"
     :logicalName "Root"
     :physicalname nil)

(reset-root-alias)

(new DirectoryNickname
     :project SK8
     :objectName "SK8DirectoryNickname"
     :logicalName "SK8"
     :physicalName  (namestring (ccl::startup-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CurrentDirectory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| This should not be defined until System is defined!

;;; Can take a string or file that is a valid directory.
;;; (Should it take the super if given a file file?  Easy to implement,
;;; but does it make things more confusing?)
;;; Internally it's a sk8 pathname object.
(define-handler (setf currentDirectory) (value System)
  (cond ((inheritsfrom value File) nil)
        ((stringp value)
         (setf value (new File 
                          :project (project me)
                          :OSPathname (namestring value))))
        (t 
         (sk8-error PropertyTypeMismatchError
                    :object        value
                    :expectedType  (list String File)
                    :ownerObject   System
                    :propertyName 'currentDirectory
                    )))
  (unless (directoryp-improved (OSPathname value))
    (error "~a is not a valid directory" (name value)))
  (setf (slot-value me 'currentDirectory) value)
  value)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Objects (used to be called Pathnames)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pn is a lisp pathname, logical or physical (for easy pathname operations reasons).
;;; Only the code in this file needs to deal with it directly, so that's why it
;;; has the highly abreviated name.
(new object :objectname "File" 
     :project sk8
     :properties '((osPathname)
                   (appleFileSpecGCHandle)
                   (existed)))

;(setf (private sk8::File :property `appleFileSpecGCHandle) t)

(define-handler localVirtualProperties (File)
  '(isRootDirectory
    name
    fileDirectory
    fileType
    logicalName
    physicalName
    fileDevice
     ))

#|
;;; Since these "act like properties" during initialization (& 'set file' of Stream), but aren't really properties,
;;; they must be declared explicitly
;;;
(SK8-declare-syms :SK8 :public
                  SK8::create
                  SK8::device
                  SK8::directory
                  SK8::name
                  SK8::type
                  SK8::defaultsFrom
                  SK8::direction
                  SK8::ifExists
                  SK8::ifDoesNotExist)

|#

(define-handler initialize (File original isNew initArgs)
  (declare (ignore original isNew))
  (call-next-method)
  (let ((create (initializerArgument initargs 'create))
        (devicee (initializerArgument initargs 'device))
        (directory (initializerArgument initargs 'SK8:directory))
        (name (initializerArgument initargs 'name))
        (type (initializerArgument initargs 'type))
        (defaultsFrom (initializerArgument initargs 'defaultsFrom)))
    (declare (ignore devicee))  ;; for now
    (with-slots (OSPathname) me
      (when (and defaultsFrom (not (inheritsFrom defaultsFrom File)))
        (SK8-error ArgumentTypeMismatchError
                   :handlerName 'initialize :argumentName 'defaultsFrom
                   :object defaultsFrom :expectedType File))
      (let ((pn-1 (cond ((null defaultsFrom) nil)
                        ((inheritsFrom defaultsFrom File)
                         (OSPathname defaultsFrom))
                        ((stringp defaultsFrom)
                         (parse-namestring defaultsFrom))
                        (t 
                         (error "DefaultsFrom should be either a file or string."))))
            (pn-2 (make-pathname :directory directory
                                 :name name
                                 :type type)))
        (if defaultsFrom 
          (setf osPathname (merge-pathnames pn-2 pn-1))
          (setf osPathname pn-2)))
      (when (and create (not (probe-file osPathname)))
        (create-file osPathname)))))


;;; returns 2 values: the handle and a boolean telling whether the file exists
(define-handler appleFileSpecGCHandle (File)
   (with-slots (appleFileSpecGCHandle existed osPathname) me
     (or appleFileSpecGCHandle
         (when osPathname
           (sk8-multival-bind (fsspec exists)
                              (T_FSMakeFSSpecGCForCreating 0 0 osPathname)
             (setq existed exists)
             (setq appleFileSpecGCHandle fsspec))))))

(define-handler (setf appleFileSpecGCHandle) (val File)
  (with-slots (appleFileSpecGCHandle existed osPathname) me
    (setf appleFileSpecGCHandle val)
    (setf osPathname (%path-from-fsspec appleFileSpecGCHandle))))

(define-handler preserve (File)
  (setf (appleFileSpecGCHandle me) nil))


;;; Does this file name the RootDirectory?
(define-handler isRootDirectory (File)
  (when (OsPathname me)
    (root-directory-pathname-p (OSPathname me))))

(defun root-directory-pathname-p (pn)
  (let ((pn-directory (pathname-directory pn))
        (pn-host      (pathname-host pn)))
    (and (null (pathname-name pn))
         (null (pathname-type pn))
         (or (and (string-equal pn-host "Root")
                  (or (null pn-directory)
                      (and (null (cdr pn-directory))
                           (eq (car pn-directory) :absolute))))
             (and (or (eq pn-host :unspecific) (string-equal pn-host "Root"))
                  (null (cdddr pn-directory))
                  (eq (car pn-directory) :absolute)
                  (string-equal (third pn-directory) "Desktop Folder")
                  (string-equal (second pn-directory)
                                (second (pathname-directory (drive-name-soft 0)))))))))


(define-handler writeObject (File str rereadably)
  (unless (maybeWriteObjectName me str)
    ;; Added the "[]" marks to indicate than an anonymous file is not readble
    ;; SK8Script.
    (when rereadably (write-string "[the File " str))
    (writeObject (logicalName me) str t)
    (when rereadably (write-string "]" str))))

;;; Complicated, but looks great.
;;; Special cases for Prototype, Empty, RootDirectory, Directories.
(define-handler print-object :hidden (File str)
  (multiple-value-bind
    (ret errorp)
    (ignore-errors
     (cond
      ((objectname me)
       (call-next-method))
      ((isRootDirectory me)
       (format str "[ROOT Directory File]"))
      (t (with-slots (OSPathname) me
           (let ((name (pathname-name OSPathname))
                 (dir  (pathname-directory OSPathname))
                 (type (pathname-type OSPathname))
                 (host (pathname-host OSPathname)))
             (cond ((and (null name) (null dir) (null type) (null host))
                    (format str "[Empty File]"))
                   ((and (null name) (null type))
                    (format str "[Directory File \"~a\"]"
                            (namestring-hacking-logical-syntax (OSPathname me))))
                   (t 
                    (format str "[File \"~a\"]" 
                            (namestring-hacking-logical-syntax (OSPathname me))))))))))
    (declare (ignore ret))
    (when errorp (format str "[a File]"))))

;;; Mac Files really ignore type, and MCL text parses out the type.
;;; This undoes that.
;;; Force-name causes directories (files without names or types) to use the rightmost 
;;; subdirectory name instead of a null string.
(defun pathname-name&type (pn &optional force-name)
  (let ((name (pathname-name pn))
        (type (pathname-type pn))
        str)
    (cond ((and force-name (not (stringp name)) (not (stringp type)))
           (car (last (pathname-directory pn))))
          (t 
           (setq str (if (stringp type)
                       (concatenate 'string name "." type)
                       (if (stringp name) name "")))
           (if (find #\266 str :test #'char=)
             (remove #\266 str)
             str)))))


;;; Some pathname operations on directories don't agree with the mac convention of specifying a 
;;; directory with a filename.  Ie., they want "Drive:directory:directory:" and get horribly 
;;; confused when you hand them "Drive:directory:directory", even though files and directories
;;; can't have the same names.  By and of itself that's not much of an issue, but the folks
;;; used to using the Mac don't understand this difference.
(defun coerce-pathname-to-directory (path)
  (if (directory-pathname-p path) 
    path
    (make-pathname :defaults path
                   :directory (append (or (pathname-directory path) '(:absolute))
                                      (list (pathname-name&type path)))
                   :name nil
                   :type nil)))


;;; Just like directoryp, but hacks the issue above.
(defun directoryp-improved (path)
  (directoryp (coerce-pathname-to-directory path)))


;;; Like namestring, but swaps the colons and semicolons in a logical pathname.
(defun namestring-hacking-logical-syntax (pn)
  (if (typep pn 'logical-pathname)
    (format nil "~a;~{~a:~}~a" 
            (pathname-host pn) 
            (cdr (pathname-directory pn))
            (pathname-name&type pn))
    (namestring pn)))
  

;;; The Filename, regardless of logical or physical.
(define-handler name (File &key (directory t))
  (with-slots (OSPathname) me
    (when OSPathname 
      (if directory 
        (namestring-hacking-logical-syntax OSPathname)
        (pathname-name&type OSPathname :force-name)))))


;;; this is redefined to make sure that every path is stored in its logical form.
;;; This deals with lots of problematic cases...
;;; also,  reset appleFileSpecGCHandle.

(define-handler (setf osPathname) (newPathname File)
   (setf (slot-value me 'appleFileSpecGCHandle) nil)
   (setf (slot-value me 'ospathname)
         (backtranslate-physical-pn-if-appropriate newPathname)))

;;; Sets the name, regardless of physical or logical.
(define-handler (setf name) (value File)
  (check-type value string)
  (with-slots (OSPathname) me
    (setf OSPathname (make-pathname :name value :defaults OSPathname))))


(define-handler fileDirectory (File)
  (with-slots (OSPathname) me
    (when OSPathname
      (if (typep OSPathname 'logical-pathname)
        (concatenate 'string
                     (pathname-host OSPathname)
                     ":"
                     (directory-namestring OSPathname))
        (directory-namestring OSPathname)))))


(define-handler (setf fileDirectory) (value File)
  (check-type value string)
  (with-slots (OSPathname) me
    (setf OSPathname (make-pathname :directory value :defaults OSPathname)))
  value)


(define-handler fileType (File)
  (when (OSPathname me)
    (let ((type (pathname-type (OSPathname me))))
      (when (stringp type) type))))


(define-handler (setf fileType) (value File)
  (check-type value string)
  (with-slots (OSPathname) me
    (setf OSPathname (make-pathname :type value :defaults OSPathname)))
  value)

(defun pathname-to-logicalName (pn)
  (namestring-hacking-logical-syntax
   (let ((pn-host (pathname-host pn)))
     (cond 
      ((typep pn 'logical-pathname)
       (if (string-equal pn-host "Root")
         (backtranslate-physical-pathname pn)
         pn))
      (t
       (when (or (null pn-host) (eq pn-host :unspecific))
         (setq pn (make-pathname :defaults pn :host "Root")))
       (backtranslate-physical-pathname pn))))))

;;; returns a string or nil.
(define-handler logicalName (File)
  (let ((pn (OSPathname me)))
    (when pn
      (pathname-to-logicalName pn))))

;;; Given a pseudo-physical pathname, find best logical translation.

(define-handler (setf logicalName) (value File)
  (check-type value string)
  (setf (OSPathname me) (sk8-filename-to-mcl-pathname value :type :logical :error t))
  value)
      

(define-handler validLogicalName (String)
  (not (null (sk8-filename-to-mcl-pathname me :type :logical))))


(define-handler physicalname (File &key (directory t))
  (when (OSPathname me)
    (let ((pn (translate-logical-pathname (OSPathname me))))
      (if directory
        (namestring pn)
        (pathname-name&type pn :force-name)))))

(define-handler (setf physicalName) (value File)
  (check-type value string)
  (setf (OSPathname me) (sk8-filename-to-mcl-pathname value :type :physical :error t))
  value)


(define-handler validPhysicalName (String)
  (not (null (sk8-filename-to-mcl-pathname me :type :physical))))


;;; I doubt this is the proper modularity -- the device should really be part of the pathname. 
;;; But until we work out more details this should work okay.
(define-handler fileDevice (File)
  (with-slots (OSPathname) me
    (when OSPathname
      (let  ((dir (pathname-directory OSPathname)))
        (when (eq (first dir) :absolute)
          (find (concatenate 'string (second dir) ":")
                (knownchildren StorageDevice)
                :key #'name
                :test #'string=))))))

(defun backtranslate-physical-pn-if-appropriate (pn)
  (when pn
    (if (and (not (typep pn 'logical-pathname))
             (let ((pn-d-pn (pathname-directory pn))
                   (pn-d-d0 (pathname-directory (drive-name-soft 0))))
               (and (eq (first pn-d-pn) (first pn-d-d0))
                    (string-equal (second pn-d-pn) (second pn-d-d0)))))
      (backtranslate-physical-pathname pn)
      pn)))

;;; Useful Files.
;;;   RootDirectory:   the Desktop Folder
;;;   SK8Directory:    The SK8 directory.

(new File 
     :project SK8 
     :objectname "RootDirectory"
     :logicalname "Root:")

;;; Since SK8 creates portable applications for a living we want to coerce pathnames to 
;;; logical whenever possible.  This performs the backtranslation by finding the best matching 
;;; DirectoryNickname and bopping it into the pathname.
;;; 
;;; This is fundamentally wrong because:
;;;   The user didn't really ask for a logical pathname.
;;;   There are obvious cases where there would be multiple matches.
;;;
;;; We know about these issues, but we're doing it this way anyway.
;;; Alternate approaches will definitely be considered.
;;;
(defun backtranslate-physical-pathname (pn)
  (if (root-directory-pathname-p pn) 
    (ospathname rootdirectory)
    (let* ((ppn (translate-logical-pathname pn))
           (p-dir (pathname-directory ppn))
           (best-host "Root")
           (best-count 0)
           (match-rest-length))
      (mapknowndescendants 
       DirectoryNickName
       #'(lambda (dnn)
           (let* ((dnn-host (logicalname dnn))
                  (dnn-path-dir (pathname-directory
                                 (translate-logical-pathname
                                  (caar (last (logical-pathname-translations dnn-host)))))))
             (multiple-value-bind (count rest-length) (pathname-directory-match dnn-path-dir p-dir)
               (when (and count (> count best-count))
                 (setq best-host dnn-host)
                 (setq best-count count)
                 (setq match-rest-length rest-length))))))
      (make-pathname :host best-host 
                     :directory (if match-rest-length 
                                  (list* :absolute (subseq p-dir (- (length p-dir) match-rest-length)))
                                  p-dir)
                     :defaults pn))))


;;; Helper function for above.
;;; Given two pathname directories (lists), returns an integer describing the 
;;; number of matched directories, or nil if no match.  Second value returned 
;;; is the remaining length of dir2.
;;;
;;; :wild-inferiors count for matching, but don't count for the returned number.
;;; (So that more specific matches will override.  Reasonable?)
(defun pathname-directory-match (dir1 dir2)
  (and (eq :absolute (pop dir1))
       (eq :absolute (pop dir2))
       (let ((count 1) wild1 wild2 cdir1 cdir2 )
         (loop 
           (setq cdir1 (car dir1))
           (setq cdir2 (car dir2))
           (cond ((null dir1)
                  (return (values count (length dir2))))
                 ((eq :wild-inferiors cdir1)
                  (setq wild1 t)
                  (pop dir1))
                 ((eq :wild-inferiors cdir2)
                  (setq wild2 t)
                  (pop dir2))
                 (wild1
                  (cond ((string-equal cdir1 cdir2)
                         (setq wild1 nil) (pop dir1) (pop dir2) (incf count))
                        (t 
                         (pop dir2))))
                 (wild2 
                  (cond ((string-equal cdir1 cdir2)
                         (setq wild2 nil) (pop dir1) (pop dir2) (incf count))
                        (t 
                         (pop dir1))))
                 ((string-equal cdir1 cdir2)
                  (pop dir1) (pop dir2) (incf count))
                 (t 
                  (return nil)))))))

(new File 
     :project SK8
     :objectname "SK8Directory"
     :logicalname "SK8;")

(new File
     :project SK8
     :objectname "SK8ApplicationFile"
     :OSPathname (ccl::startup-pathname))

#| This should not be defined until System is defined!
(setf (currentDirectory System) SK8Directory)
|#

(defun restore-file-environment ()
  (reset-root-alias)
  (setf (physicalName SK8DirectoryNickname) (namestring (ccl::startup-directory)))
  (setf (OSPathname SK8ApplicationFile) (ccl::startup-pathname))
  t)


;;; By decree, Files must have logical names, whether they really are or not.
;;; This should help.
;;;   with-respect-to, when present, needs to be a DirectoryNickname.
(defun logify-pathname (pn &optional with-respect-to)
  (unless with-respect-to (setq with-respect-to RootDirectoryNickname))
  (if (typep pn 'logical-pathname)
    pn
    (make-pathname 
     :host      (logicalname with-respect-to)
     :directory (pathname-directory pn)
     :name      (pathname-name pn)
     :type      (pathname-type pn))))

;;;
;;; Handlers that do things to/with Files physically on disk.
;;;

;;; Returns the contain directory (a File object) of the File.
;;; If the file is a directory, we go one level up.
;;; If the file is the RootDirectory, return false.
(define-handler sk8:directory (File)
  (unless (isRootDirectory me)
    (let* ((pn (OSPathname me))
           (dir  (pathname-directory pn))
           (host (pathname-host pn)))
      ;; case: not a directory
      (cond
       ((or (pathname-name pn) (pathname-type pn))
        (new File
             :project (project me)
             :osPathname (make-pathname :host host
                                :directory dir)))
       ;; case: out of directory levels, go psuedo-physical
       ((null dir) 
        (when host
          (setq dir (butlast 
                     (pathname-directory
                      (translate-logical-pathname pn))))
          (when (= 1 (length dir)) (setq dir nil))
          (new File
               :project (project me)
               :OSPathname (make-pathname :host "Root" 
                                          :directory dir))))
       ;; case: directory 
       (t
        (setq dir (butlast dir))
        (when (= 1 (length dir)) (setq dir nil))         
        (if (and (null dir)
                 (eq host :unspecific))
          rootdirectory
          (new File 
               :project (project me)
               :OSPathname (make-pathname :host host :directory dir))))))))


;;; Ruben's original api sketch included Name, FilePathname, Super and Override
;;; keywords, but I have no idea what he was intending with them.
(define-handler createFile (file)
  (when (create-file (OSPathname me) :if-exists nil)
    me))


;;; Directory listing handlers:
;;;   files
;;;   fileNames
;;;   directories
;;;   directoryNames

;;; Returns the files in a directory (a File object) as a list of File objects.
;;; The directories keyword allows the user to determine whether just files or
;;; files and directories are returned.
(define-handler files (File &key (directories t))
  (let ((proj (project me)))
    (mapcar #'(lambda (path) (new File :project proj :OSPathname path))
            (files-internal (OSPathname me) :filesp directories))))


;;; As above but returns strings instead of File objects.
(define-handler fileNames (File &key (directories t))
  (mapcar #'namestring-hacking-logical-syntax
          (files-internal (OSPathname me) :filesp directories)))


;;; Returns the directories in a directory (File).
(define-handler directories (File)
  (let ((proj (project me)))
    (mapcar #'(lambda (path) (new File :project proj :OSPathname (logify-pathname path)))
            (files-internal (OSPathname me) nil :directoriesp))))


;;; As above but returns strings instead of File objects.
(define-handler directoryNames (File)
  (mapcar #'namestring-hacking-logical-syntax
          (files-internal (OSPathname me) nil :directoriesp)))

;;; Several important bits here:
;;;   If the pn is logical, logical pn's are returned.
;;;   If the pn is root, we have to do something kludgey so that the 
;;;     desktop disk is included in the root.
;;;   If pn is the desktop disk, we have to do something kludgey so 
;;;     that we can do a directory of it.
;;; It's for "internal" use, so it inputs and outputs Lisp pathname objects.
(defun files-internal (pn filesp directoriesp &optional macfiletype)
  (setq pn (coerce-pathname-to-directory pn))
  (when (directoryp pn)
    (flet ((filter (p)
             (and (not (pathname-invisible-p p))
                  (or (null macfiletype)
                      (directory-pathname-p p)
                      (pathname-alias-p p)
                      (string= macfiletype (mac-file-type p))))))
      (if (root-directory-pathname-p pn)
        (nconc (mounted-volumes)
               (mapcar 'rootify-pathname (directory (translate-logical-pathname (pathname "Root:*")) 
                                                    :directories directoriesp
                                                    :files filesp
                                                    :test #'filter)))
        (directory (translate-logical-pathname (make-pathname :defaults pn :name :wild :type :wild))
                   :files               filesp
                   :directories         directoriesp
                   :test                #'filter)))))


;;; Given a pathname, converts anything in:
;;;     #p"MainDrive:Desktop Folder:whatever"
;;; to:
;;;     #4p"Root:whatever"
;;;
(defun rootify-pathname (pn)
  (let* ((drive-0 (drive-name-soft 0))
         (drive-0-dir (pathname-directory drive-0))
         (pn-host (pathname-host pn))
         (pn-dir  (pathname-directory pn)))
    (if (and (eq pn-host :unspecific)
             (eq (first pn-dir) (first drive-0-dir))                ; :absolute
             (string-equal (second pn-dir) (second drive-0-dir))    ; drive-name
             (string-equal (third pn-dir) "Desktop Folder"))        ; "Desktop Folder"
      (make-pathname :defaults pn
                     :host "Root"
                     :directory (cons (car pn-dir)
                                      (copy-list (cdddr pn-dir))))
      pn)))

(define-handler fileExists (File)
  (when (OSPathname me)
    (not (null (probe-file (OSPathname me))))))

;; also let things work whe nwe check a null argument. this simplifies a lot of places FileExists is called
(defmethod fileExists ((me Null) &key)
  (declare (ignore me))
  nil)

;;; Is the 'File' a directory?
(define-handler isDirectory (File)
  (not (null (directoryp-improved (OSPathname me)))))


(define-handler creationdate (File)
  (with-slots (OSPathname) me
    (when OSPathname 
      (new DateTime
           :project (project me)
           :secondssince1904 (ccl::universal-to-mac-time
                              (file-create-date OSPathname))))))


(define-handler modificationdate (File)
  (with-slots (OSPathname) me
    (when OSPathname 
      (new DateTime
           :project (project me)
           :secondssince1904 (ccl::universal-to-mac-time
                              (file-write-date OSPathname))))))


(define-handler sk8::delete (File)
  (let ((myname (probe-file (OSPathname me))))
    (when myname
      (removeres myname)  ;; make sure that resource fork is closed, since delete-file doesn't
      (delete-file myname))))


;;; *** check on this
(define-handler device (File)
  (with-slots (OSPathname) me
    (when OSPathname (device OSPathname))))

;;; openProjectFile -- same as openProject, but you can supply a file name instead of a file object
;;;   filename -- a filename string
;;;
(define-sk8-function openProjectFile nil (filename)
  (let (curprojects
        p)
    (mapknowndescendants project
                         #'(lambda (prj) (push prj curprojects)))
    (setf *load-file-namestring* filename)
    (load filename :verbose nil)
    (setf p
          (block findnewproj
            (mapknowndescendants project
                                 #'(lambda (obj)
                                     (unless (memq obj curprojects)
                                       (return-from findnewproj obj))))))
    (when (and p (is-a p project))
      (sk8::opened p)
      (sk8::openedProject sk8::system p))
    p)) ; MUST RETURN THIS PER API

;;; openProject -- loads a project from its store file
;;;
(define-handler openProject (File &key copyright)
  (declare (ignore copyright))
  (openprojectfile (physicalname me)))

(defmethod open-application-document ((a sk8-application-class) path &optional startup)
  (declare (ignore startup))
  (if (string= (mac-file-type path) #.(string-upcase (pathname-type *.fasl-pathname*)))
    (with-simple-restart (continue "Skip loading finder-selected file.")
      (openProjectFile path)
      t)
    (call-next-method)
    ))

#|
;;; Notes to me

;;; These are probably okay
;;; Bootstrap.  
;;; (Not really, just integrate these when you search for the word "Bootstrap")
;;;  define a handler for  move (File) ...)
;;;  define a handler for copy (File &key newName newDirectory)  ...)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resource File Utilities!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If inProj already has a file with fromFile's pathname, it is returned. Otherwise one is created.

(define-sk8-function findFileInProject nil (fromFile inProj)
  (if (eq inProj (project fromFile))
    ;; If from file is already in inProj just return it.
    fromFile
    ;; No? Mmmm. See if anyone in the project matched this path.
    (let ((fromPn (OSPathname fromFile)))
      (or (block found 
            (mapknowndescendants file 
                                 #'(lambda (c)
                                     (when (eq (OSPathname c) fromPN)
                                       (return-from found c)))
                                 inProj))
          ;; Oh well... Just make one.
          (new file :project inProj :OSPathname fromPN)))))

;;; Returns the Mac file type as an ostype keyword or
;;;   :directory if it's a directory
;;;   :danglingAlias if it's an alias to a deleted file
;;;   nil if the file is nonexistant
(define-handler macFileType (File)
  (with-slots (OSPathname) me
    (cond ((null (probe-file OSPathname))
           ;; either the file doesn't exist or it's a dangling alias.
           (ccl::%stack-iopb (pb np)
             (ccl::%path-to-iopb OSPathname pb nil t)
             (when (ccl::pb-alias-p pb)
               'danglingAlias)))
          ((directoryp-improved OSPathname)
           'directory)
          (t
           (symbol-name (mac-file-type OSPathname))))))


(define-handler (setf appleFileSpecGCHandle) (val File)
  (with-slots (appleFileSpecGCHandle existed osPathname) me
    (when (setf appleFileSpecGCHandle val)
      (setf osPathname (path-from-fsspec val))))
  val)

(defun path-from-fsspec (fsspec)
  (let ((dir-id (rref fsspec :fsspec.parid))
        dirlist)
    (with-returned-pstrs ((vname ""))
      (rlet ((qb :CInfoPBRec 
                 :ioCompletion (ccl:%null-ptr)
                 :ioNamePtr vname
                 :ioVRefNum (rref fsspec :fsspec.vrefnum)))
        (loop
          (rset qb :CInfoPBRec.ioFDirIndex -1)
          (rset qb :CinfoPBRec.ioDirID dir-id)
          (if (eql #$noErr (#_PBGetCatInfo qb))
            (push (%get-string (pref qb :CInfoPBRec.ioNamePtr)) dirlist)
            (return nil))
          (when (= dir-id 2)
            (return (merge-pathnames (rref fsspec :fsspec.name)
                                     (make-pathname :directory (cons :absolute dirlist)))))
          (setf dir-id (pref qb :CinfoPBRec.ioDrParID)))))))



(define-handler (setf macFileType) (val File)
  (assure-ostypestring val)
  (with-slots (OSPathname) me
    (cond ((null (probe-file OSPathname))
           (error "~s doesn't exist" me))
          ((directoryp-improved OSPathname)
           (error "~s is a folder and can't have it's macFileType set." me))
          (t
           (set-mac-file-type OSPathname val)
           val))))
           

(define-handler macFileCreator (File)
  (with-slots (OSPathname) me
    (cond ((null (probe-file OSPathname))
           ;; either the file doesn't exist or it's a dangling alias.
           (ccl::%stack-iopb (pb np)
             (ccl::%path-to-iopb OSPathname pb nil t)
             (when (ccl::pb-alias-p pb)
               'danglingAlias)))
          ((directoryp-improved OSPathname)
           'directory)
          (t
           (symbol-name (mac-file-Creator OSPathname))))))

(define-handler (setf macFileCreator) (val File)
  (assure-ostypestring val)
  (with-slots (OSPathname) me
    (cond ((null (probe-file OSPathname))
           (error "~s doesn't exist" me))
          ((directoryp-improved OSPathname)
           (error "~s is a folder and can't have it's macFileCreator set." me))
          (t
           (set-mac-file-creator OSPathname val)
           val))))

;;; Byte offset for flag bits.
;;; I don't know why this isn't around in this image.
(defconstant ccl::$fdFlags 40)
(defconstant ccl::$file-invisible-bit 14)
(defconstant ccl::$file-alias-bit 15)


;;; Get the flag bits.
(defun pathname-flag-bits (path)
  (ccl::%stack-iopb (pb np)
    (ccl::%path-to-iopb path pb nil :dont-resolve)
    (%get-word pb ccl::$fdFlags)))

(defun pathname-invisible-p (path)
  (logbitp ccl::$file-invisible-bit (pathname-flag-bits path)))

(defun pathname-alias-p (path)
  (logbitp ccl::$file-alias-bit (pathname-flag-bits path)))

;; KLUDGE ALERT!!!
;; There is no good documented way to check if a resource file is already open, but
;; we have to close a file after opening it if and only if it was not already open.
;; This routine uses the documented low-level structure of the list of open resource
;; files as described in Inside Mac, Vol I. The with-open-res-file macro uses this
;; function to make sure it only closes a file it has opened if it wasn't already open.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "RESOURCES"))

;;; like with-open-res-file, but uses a File object as argument and doesn't provide the flexibility of caller specified
;;; refnum-var or :if-not-exist arguments

(defmacro withResourceFile ((thefile) &body body)
  (let ((filename (make-symbol "FILENAME")))
    `(let ((,filename (physicalname ,thefile)))
       (with-open-res-file (nil ,filename)
         ,@body))))

;;; Returns t if the file has a resource fork! Very slow if file is not already open!!!

(define-handler macHasResources (file)
  (or (string= (macFileType me) "PICT")
      (let ((filename (physicalName me)))
        (with-open-res-file (refnum filename :if-does-not-exist nil)
          (when refnum t)))))

;;; Returns a list of all the resource types available.
;;; Assumes the resource fork is valid!

(define-handler macResourceTypesAvailable (file)
  (let ((theType (macFileType me)))
    (cond ((string= theType "PICT") (list "PICT"))
          ((eq thetype 'directory) nil)
          ;; Add other types here!
          (t 
           (withResourceFile (me) 
                             (let ((numTypes (#_count1Types))
                                   result j)
                               (dotimes (i numtypes result)
                                 (setf j (1+ i))
                                 (%stack-block ((theType 4))
                                   (#_get1indtype theType j)
                                   (pushnew (%get-ostype theType) result)))))))))

;;; Returns true if there is anything SK8 can import in the resources.

;;; Returns all the resource types in the resource fork that we know how to translate. 

(define-handler macSK8ImportableResourceTypesAvailable (file)
  (let ((theTypes (macresourceTypesAvailable me))
        result)
    (mapKnownDescendants 
     Translator 
     #'(lambda (obj)
         (when (member (externalType obj) theTypes :test #'string=)
           (pushnew (externalType obj) result :test #'string=))))
    (remove "TEXT" result :test #'string=)))

(defun importableDataFork (theFile)
  (mapknownDescendants
   Translator
   #'(lambda (c) 
       (when (canImport c theFile nil)
         (return-from importableDataFork t)))))

;;; Given a handle returns the name and the id.

(defun GetResourceInfo (resourceHandle)
  (%stack-block ((id 2)
                 (type 4) 
                 (name 256))
    (T_GetResInfo resourceHandle id type name)
    (values (%get-signed-word id)
            (%get-string name))))

;;; Returns the handles of all resources of a given type.

(define-handler macGetResourceHandles (file restype)
  (let ((thehandles nil))
    (withResourceFile (me) 
                      (dotimes (i (#_count1Resources restype) thehandles)
                        (pushnew (T_Get1IndResource restype (1+ i)) thehandles)))))

(define-handler macCountResourcesOfType (file restype)
  (cond ((string= (macFileType me) "PICT") 1)
        (t 
         (withResourceFile (me) 
           (#_count1Resources restype)))))

(define-handler macHasResourceFork (file)
  (let ((filename (physicalName me)))
    (with-open-res-file (refnum filename :if-does-not-exist nil)
      (when refnum t))))

;;; This allows you to only get the resource when it is needed.

(define-handler macGetResourceHandleInfo (file restype)
  (cond ((and (string= (macFileType me) "PICT") (string-equal restype "PICT"))
         (sk8-multivals (list (physicalName me :directory nil)) (list nil)))
        (t 
         ;; Only if there is a resource fork...
         (when (macHasResourceFork me)
           (let (thenames theids oneHandle)
             (withResourceFile (me) 
               (dotimes (i (#_count1Resources restype))
                 (with-load-resources-off
                   (setf oneHandle (T_Get1IndResource restype (1+ i))))
                 (multiple-value-bind (theid thename) (getResourceInfo oneHandle)
                   (if (zerop (length thename))
                     (pushnew theid thenames)
                     (pushnew theName thenames))
                   (pushnew theid theids))))
             (sk8-multivals (reverse thenames) (reverse theids)))))))
  
(define-handler macGetResourceHandleNames (file restype)
  (let (thenames oneHandle)
    (withResourceFile (me) 
      (dotimes (i (#_count1Resources restype))
        (with-load-resources-off
          (setf oneHandle (T_Get1IndResource restype (1+ i))))
        (multiple-value-bind (theid thename) (getResourceInfo oneHandle)
          (if (zerop (length thename))
            (pushnew theid thenames)
            (pushnew theName thenames))))
      thenames)))

(define-handler macGetResourceHandleIds (file restype)
  (let (theids oneHandle)
    (withResourceFile (me)
      (dotimes (i (#_count1Resources restype))
        (with-load-resources-off
          (setf oneHandle (T_get1indresource restype (1+ i))))
        (multiple-value-bind (theid thename) (getResourceInfo oneHandle)
          (declare (ignore theName))
          (pushnew theid theids)))
      theids)))

;;; Returns the total number of valid resources in the file.

(define-handler macTotalSK8ImportableResources (file)
  (let ((totalNum 0))
    (withResourcefile (me) 
      (dolist (aType (macSK8ImportableResourceTypesAvailable me) totalNum)
        (incf totalNum (#_count1Resources aType))))))

;;; Given a restype and a resourceId, returns the handle in a safe form for drawing. Should be used
;;; by machandle.

(define-handler macGetResourceHandleFromIdForDrawing (file restype resId)
  (withResourceFile (me)
    (cond ((string-equal restype "cicn") (T_getCIconGC resid))
          ((string-equal restype "ppat") (T_getPiXPatGC resid))
          ((string-equal restype "crsr") (T_getCCursorGC resid))
          (t (T_get1ResourceGC restype resid)))))

;;; Returns the handle in a safe way to perform resource copy operations. Should be called
;;; by the media browser to do its stuff. 

(define-handler macGetResourceHandleFromId (file restype resId)
  (if (string= (macFileType me) "PICT")
    ;; Making it work for pict files!
    (pictFileToHandle me)
    (withResourceFile (me)
      (T_get1ResourceGC restype resid))))

;;; Given a restype and a name, returns the handle.

(define-handler macGetResourceHandleFromName (file resType resName)
  (withResourcefile (me)
    (let ((theRes (T_Get1NamedResource resType resName)))
      (T_detachResource theRes)
      (setf theRes (handleToGCHandle theRes))
      theRes)))

(define-handler macGetResourceFromTypeAndIndex (file restype index)
  (withResourceFile (me)
    (let ((theRes (T_get1IndResource restype index)))
      (T_detachResource theRes)
      ;; Make it a GCable handle.
      (setf theRes (handleToGCHandle theRes))
      theRes)))

(define-handler macUpdateResourceFile (file)
  (withResourceFile (me)
    (T_UpdateResFile (#_curResFile))))

;;; Returns whether the resource file is already open.

(define-handler macResourceFileOpen (file)
  (unless (string= (macFileType me) "PICT")
    (let ((reslist (openresfilenums))
          curNum)
      (withResourceFile (me)
        (setf curNum (#_curResFile)))
      (if (memq curnum reslist)
        t
        nil))))

;; The MCL copy-file function does not copy the resource fork if it is open, so we need to work around that
;; also, this function works with both pathnames and File objects

(defun copyfile (from to &key (if-exists :error) (fork :both))
  (let ((fromname (namestring (if (inheritsfrom from sk8::file)
                                (sk8::physicalname from)
                                from)))
        (toname (merge-pathnames ;; :type :unspecific to allow copying to a filename with no extension, e.g. (copyfile "foo.bar" "foo1")
                 (namestring (if (inheritsfrom to sk8::file)
                               (sk8::physicalname to)
                               to))
                 (make-pathname :type :unspecific))))
    (unless (eq fork :data) (removeRes fromname))
    (copy-file fromname toname :if-exists if-exists :fork fork)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open-file-internal
;;;   -- used by Streams
;;;
;;; Direction defaults to input.
;;; IfExists defaults to an error when the direction is output or io.
;;; IfDoesNotExist defaults to :error on read streams, :create otherwise
;;;
(defun open-file-internal (streamObj fileObj &optional direction ifExists ifDoesNotExist)
  (check-sk8-type streamObj sk8::Stream)
  (if fileObj (check-sk8-type fileObj File))  ;;can be false...
  (unless direction
    (setq direction (or (direction streamObj) 'input)))
  (if (eq direction 'closed) (setf direction 'input))
  (unless ifDoesNotExist  
    (setq ifDoesNotExist (or (ifDoesNotExist streamObj) (if (eq direction 'input) 'SK8::error 'create)) ))
  (unless ifExists 
    (setq ifExists (or (ifExists streamObj) 
                       (case direction 
                         (input   nil)
                         (io     'overwrite)
                         (output 'SK8::append)))
          ))
  ;; If there's a stream in the str slot already, close it
  (when (str streamObj) (close (str streamObj)))
  (setvalue 'direction streamObj direction)
  (setvalue 'ifDoesNotExist streamObj ifDoesNotExist)
  (setvalue 'ifExists streamObj ifExists)
  ;; Open the file and stick the stream into the str slot
  (setValue 'str streamObj
            (if fileobj
              (open (OSPathname fileObj)
                    :direction (case direction
                                 (input  :input)
                                 (output :output)
                                 (io     :io)
                                 ((nil)  nil)
                                 (t      (error
                                          "Direction can only be 'Input', 'Output' or 'IO'")))
                    :element-type (elementType streamObj)
                    :if-exists (case ifExists
                                 (SK8::error  :error)
                                 (rename      :rename)
                                 (overwrite   :overwrite)
                                 (SK8::append :append)
                                 (supersede   :supersede)
                                 ((nil)       nil)
                                 (t           (error "Illegal value for ifExists")))
                    :if-does-not-exist (case ifDoesNotExist
                                         (SK8::error :error)
                                         (create     :create)
                                         ((nil)      nil)
                                         (t          (error
                                                      "ifDoesNotExist may only be 'error' or 'create'"))))
              nil))
  ;; Finally, link the fileObj to the streamObj
  (setValue 'file streamObj fileObj))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stream Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new Collection :objectname "Stream" 
     :project SK8 
     :properties '(str file direction ifExists ifDoesNotExist))

(define-handler initialize (sk8::Stream original isNew initArgs)
  (declare (ignore original isNew))
  (call-next-method)
  (let ((givenFile (initializerArgument initargs 'file)))
    (when givenFile
      (open-file-internal me givenFile
                          (initializerArgument initargs 'direction)
                          (initializerArgument initargs 'ifExists)
                          (initializerArgument initargs 'ifDoesNotExist)))))

(define-handler (setf file) (value sk8::Stream)
  (open-file-internal me value)
  value)


(define-handler (setf direction) (dir sk8::Stream)
  (setvalue 'direction me dir)
  (unless (memq dir (list 'input 'output 'io 'Closed nil))
    (error
     "Direction can only be 'Input', 'Output', 'Closed' or 'IO'"))
  (when (file me)
    (if (eq dir 'closed)
      (sk8::close me)
      (open-file-internal me (file me))))
  dir)

(define-handler (setf ifExists) (val sk8::Stream)
  (setvalue 'ifExists me val)
  (unless (memq val (list 'overwrite 'sk8::append nil))
    (error
     "Direction can only be 'Overwrite' or 'Append'"))
  (when (file me)
    (open-file-internal me (file me)))
  val)

(define-handler (setf ifDoesNotExist) (val sk8::Stream)
  (setvalue 'ifDoesNotExist me val)
  (unless (memq val (list 'create 'sk8::error nil))
    (error
     "Direction can only be 'error' or 'create'"))
  (when (file me)
    (open-file-internal me (file me)))
  val)

(define-handler initializefromstore (sk8::stream)
  (let (newfile)
    (call-next-method)
    (when (file me)
      (unless (fileexists (file me))
        (ignore-errors
         (message-dialog (format nil "Stream file \"~a\" was not found!" (Name (file me))) :ok-text "Find it!")
         (setf newfile (openfiledialog :project (project me)))
         (when newfile
           (copyRealProperties newfile (file me)))
         ))
      (if (fileexists (file me))
        (open-file-internal me (file me))
        (setf (file me) nil))
      )
    ))

(define-handler propertiesToSaveAsFalse (sk8::stream)
  '(str))

(define-handler collectionLike (sk8::Stream)
  nil)

(define-handler writeStreamItem (sk8::Stream item)
  (stream-tyo (str me) item))

(define-handler readStreamItem (sk8::Stream)
  (stream-tyi (str me)))

(define-handler atStreamEnd (sk8::Stream)
  (stream-eofp (str me)))

(define-handler streamPosition (sk8::Stream)
  (CCL::stream-position (str me)))

(define-handler (setf streamPosition) (newPosition sk8::Stream)
  (CCL::stream-position (str me) newPosition)
  newPosition)

(define-handler streamWriterInfo (sk8::Stream)
  (multiple-value-bind (writerFunction argument) (stream-writer (str me))
    (SK8-multivals writerFunction argument)))

(define-handler streamReaderInfo (sk8::Stream)
  (multiple-value-bind (readerFunction argument) (stream-reader (str me))
    (SK8-multivals readerFunction argument)))


;;; Hooks to integrate MCL streams...
;;;
(defmethod writeStreamItem ((me stream) item &key)
  (stream-tyo me item))
(defmethod readStreamItem ((me stream) &key)
  (stream-tyi me))
(defmethod atStreamEnd ((me stream) &key)
  (stream-eofp me))
(defmethod streamPosition ((me stream) &key)
  (CCL::stream-position me))
(defmethod (setf streamPosition) (newPosition (me stream) &key)
  (CCL::stream-position me newPosition)
  newPosition)
(defmethod streamWriterInfo ((me stream) &key)
  (multiple-value-bind (writerFunction argument) (stream-writer me)
    (SK8-multivals writerFunction argument)))
(defmethod streamReaderInfo ((me stream) &key)
  (multiple-value-bind (readerFunction argument) (stream-reader me)
    (SK8-multivals readerFunction argument)))



;;; These are the currently supported types of streams.
(new Text :otherParents sk8::Stream :objectname "TextStream" :project SK8)
(new sk8::Stream :objectname "ByteStream" :project sk8)


(define-handler print-object :hidden (TextStream str)
  (if (objectname me)
    (call-next-method)
    (let ((strm (str me)))
      (format str "[TextStream  ~a]" (if strm
                                       (namestring (ccl::stream-pathname strm))
                                       "<none>")))))

(define-handler print-object :hidden (ByteStream str)
                (if (objectname me)
                  (call-next-method)
                  (let ((strm (str me)))
                    (format str "[ByteStream  ~a]" (if strm
                                                     (namestring (ccl::stream-pathname strm))
                                                     "<none>")))))

(define-handler elementType (TextStream)
  'character)


(define-handler elementType (ByteStream)
  'unsigned-byte)



;;; Reading from TextStreams
;;; All currently return false for end of file.
;;; (If somebody comes up with a better idea, let me know.)

(define-handler readCharacter (TextStream)
  (read-char (str me) nil))

(define-handler readWord (TextStream)
  (let (wordd each-char)
    (loop
      (setq each-char (read-char (str me) nil :eof))
      (cond ((eq each-char :eof) 
             (return))
            ((member each-char *word-delimiters* :test #'char=)
             (when wordd (return)))
            (t (push each-char wordd))))
    (and wordd (coerce (nreverse wordd) 'string))))

(define-handler readline (TextStream)
  (read-line (str me) nil nil))

;;; Writing to TextStreams.


;;; ...and don't forget to flush.
(define-handler flush (sk8::Stream)
  (flush-volume (pathname (str me)))
  t)


(define-handler writeCharacter (TextStream char)
  (check-type char character)
  (write-char char (str me)))


;;; How should writeWord delimit words?  
;;;   Space before? 
;;;   Space after?  
;;;   Not bother?
;;; Welllll, space before for now.  Hopefully someone can come up 
;;; with something better.
(define-handler writeWord (TextStream wordd)
  (check-type wordd string)
  (with-slots (str) me
    (write-char #\space str)
    (write-string wordd str)))


(define-handler writeLine (TextStream linee)
  (check-type linee string)
  (write-line linee (str me)))


(define-handler writeString (TextStream stringg)
  (check-type stringg string)
  (write-string stringg (str me)))


;;; Path methods to TextStreams
;;; Someday handle inserting text and different sized lengths.
(define-handler nextCharacter (TextStream)
  (readCharacter me))

(define-handler (setf nextCharacter) (value TextStream)
  (writeCharacter me value))

(define-handler nextWord (TextStream)
  (readWord me))

(define-handler (setf nextWord) (value TextStream)
  (writeWord me value))

(define-handler nextLine (TextStream)
  (readLine me))

(define-handler (setf nextLine) (value TextStream)
  (writeLine me value))

;;; Reading and writing with ByteStreams.
(define-handler readByte (ByteStream)
  (read-byte (str me) nil nil))

(define-handler writeByte (ByteStream theByte)
  (check-type theByte (unsigned-byte 8))
  (write-byte theByte (str me)))

(define-handler nextByte (ByteStream)
  (readByte me))

(define-handler (setf nextByte) (value ByteStream)
  (writeByte me value))

#|
;;; The way to close a stream is to dispose it. 
;;; I think it is going to have to have to be the other way round... (Hernan.)

(define-handler dispose :before (Stream)
                (close (str me)))
|#

(define-handler sk8::close (sk8::Stream)
  (close (str me))) 



#|
THESE WERE THINGS THAT RUBEN HAD IN THE OLD FILE THAT MAY MAKE SENSE TO IMPLEMENT...

Old To do

1) Finish handlers stubbs below and getFromUser*
2) add collection protocol for container so that files can be
   moved between folders by changing their container
3) add collection protocol for reading from and writing into
   the contents of the files
4) implemenet new after anonymous objects work
5) implement desktop and other built-in file objects

;;;;;;;;;;;;;;;;;;;
;;; FILE DESCENDANTS
;;;;;;;;;;;;;;;;;;;

(new file :objectName "Trashcan" :project sk8)
(new file :objectName "Desktop" :project sk8)
(new file :objectName "Systemfolder" :project sk8)
(new file :objectName "Sk8Folder" :project sk8)
(new file :objectName "Preferencesfolder" :project sk8)
(new file :objectName "Bootvolume" :project sk8)
|#




#|
	Change History (most recent last):
	2	6/15/93	kleiman	online documentation
	3	6/15/93	kleiman	documentation typo
	9	8/31/93	Brian Roddy	Adding the Tillman code and making it D4 compatible
	10	8/31/93	kleiman	qualified sk8::pathname
	11	9/1/93	kleiman	
	12	9/20/93	hernan	Fixed printObject of pathname to not crash when
				the pathname is sk8:pathname.
	13	9/22/93	kleiman	fix printobject
	14	9/28/93	rod	moved getfromuser and getnewfromuser to a consolidated list
	15	10/22/93	kleiman	locally declared 'handler' not special or constant for the mapState[inReverse]/mapCollection[inReverse] handlers
	16	10/29/93	hernan	Integrating our latest changes to files into this
				file. Note that I do not know what of the old stuff
				is still useful and have therefore not erased any
				text.
	17	10/29/93	hernan	Adding utilities for dealing with resource files.
	18	10/29/93	hernan	Making it work!
	19	11/1/93	kleiman	Making sk8-logical-to-physical-pathname work.
				Created a ccl fileAlias object. Hernan.
	20	11/1/93	hernan	Making this work!!!
	21	11/2/93	kleiman	initialize was :before, now is prime handler
	22	11/3/93	hernan	Adding handler to get a resource for drawing.
	23	11/5/93	kleiman	FindFileObjectNamed
	24	11/5/93	kleiman	symbol scan
	25	11/8/93	kleiman	more nice stuff
	26	11/8/93	hernan	Adding macFileType and making resourceTypesAvailable
				deal with pict files.
	27	11/8/93	hernan	HasResources has to return true when the macfiletype is :pict.
	28	11/10/93	kleiman	restore-file-environment
	29	11/12/93	till	Incorporate a gazillion changes to File objects.
	30	11/12/93	till	typo
	31	11/12/93	kleiman	make sk8::directory handle some funny cases
	32	11/12/93	kleiman	more
	33	11/12/93	kleiman	Hernan want's semicolons and colons.
	34	11/12/93	kleiman	A funny situation involving root
	35	11/12/93	kleiman	more rootness
	36	11/12/93	kleiman	foo
	37	11/12/93	kleiman	more rootness
	38	11/12/93	kleiman	more rootness
	39	11/15/93	kleiman	Multilple levels of logical pathnames
	40	11/15/93	kleiman	fix  a braino
	41	11/15/93	kleiman	other random stuff
	42	11/15/93	kleiman	other random stuff
	43	11/15/93	hernan	Adding Don's fix to fileAlias objects.
	44	11/15/93	hernan	directory? -> isDirectory
	45	11/16/93	rod	
	46	11/19/93	till	Added collection protocol stuff
	47	11/19/93	till	
	48	11/23/93	till	some collection protocol stuff
	49	11/24/93	till	add sk8 to mcl namestring conversion stuff
	50	11/24/93	till	add sk8 to mcl namestring conversion stuff
	51	12/1/93	till	make macFileType handle dangling aliases
	52	12/2/93	till	add :directory keyword for physicalname and logicalname
				flush filename, wholename
	53	12/2/93	till	make file names handle directories correctly
	54	12/2/93	kleiman	Fixing unbalanced parenthesis mishap.
	55	12/3/93	hernan	braino in physicalName
	56	12/8/93	hernan	Making getResourceHandleInfo work correctly 
				with :pict files.
	57	12/10/93	hernan	Adding Sidney's utilities to tell whether a resource
				file is already open.
	58	1/7/94	hernan	Adding color cursors to getResourceHandlefromIdForDrawing.
	60	1/17/94	till	fix mapCollection of streams to check for a proper
				lisp stream existing.
	61	2/11/94	sidney	Unnamed objects that can be gc'd
	62	2/12/94	kleiman	expunge -> delete
	63	2/14/94	sidney	rename children to knownchildren
	64	2/18/94	sidney	define openproject API for File object
				Clean up resource file handling to not leave file open all the time
	66	2/25/94	dy	added new File properties appleFileSpecGCHandle and existed
	67	2/26/94	kleiman	some more pn -> osPathname changes
	68	2/26/94	kleiman	fixed parens in File properties
	69	2/28/94	chip	File now prints itself using the object-literal syntax; streams don't have special printers -- they print like any other object
	70	2/28/94	hernan	Commenting out dispose.
	71	3/1/94	till	tweaked macFileType, added (setf macFileType),  
				macFileCreator, and (setf macFileCreator).
	72	3/1/94	till	typo
	73	3/1/94	till	Default value for system directory (bug 107911).
	74	3/2/94	till	Frob callers of macFileType (hasResources, 
				resourceTypesAvailable, getResourceHandleInfo 
				and getResourceHandleFromID) to use string=
				for comparisons.
	75	3/2/94	sidney	Don't try to read resources from files without resource fork
	76	3/2/94	chip	TextStream now has Text as its secondary parent
	77	3/6/94	chip	print... --> write...
	78	3/6/94	chip	added new Stream handlers
	79	3/6/94	chip	just one more
	80	3/7/94	kleiman	opened event sent to an opened project
	81	3/7/94	Hernan	Fixing resource fetching functions to detach the
				resource before returning since the resource file
				is now closed on the spot.
	82	3/8/94	Hernan	Making sure we catch the result of HandleToGCHandle.
	83	3/9/94	Hernan	Fixing the resource functions to deal with PICT
				files.
	84	3/9/94	chip	obsoleted StreamTypeFilteredCollection
	85	3/11/94	till	Speed up file dialogs, changes to files-internal.
	86	3/13/94	sidney	Aethetic improvement
	87	3/15/94	till	improve logicalname of files with physical names for bug 1129130.
	88	3/16/94	chip	a bit of cleanup to Stream
	89	3/21/94	sidney	changed a comment that was confusing the scanner a bit
	90	3/21/94	till	Flush keywords :output, :input, :io
	91	3/22/94	till	Sped up files-internal.
	92	3/22/94	till	Make backtrace-physical-pathname even better.
	93	3/23/94	Hernan	Fixing getResourceHandleFromIdForDrawing to
				turn the handles into the right gcable things.
	94	3/23/94	Hernan	Fixing some resource handle stuff to not dispose
				handles that it got for temporary use.
	95	3/23/94	Hernan	Adding a function that updates a resource file.
	96	3/24/94	kleiman	openProjectFile called opened twice
	97	3/25/94	kleiman	print-object brought more compatible with final scheme
	98	3/25/94	kleiman	modernized print-object
	99	3/25/94	chip	Stream is NOT collectionLike
	100	3/31/94	till	Fix bug in filedialogs, get other volumes on desktop.
	101	3/31/94	till	Silly braino in (setf file) of stream
	102	4/4/94	till	fix sk8::directory for cases where we're one below root
				and that silly thing in backtranslate-physical-pathname.
	103	4/22/94	till	tweaked logicalname so it handles physical names too.
				bug 1156910
	104	5/4/94	till	"Desktop Folder" to "Desktop", bug 1156910.
	105	5/23/94	till	Typo in nextcharcter
	106	5/23/94	till	Fix reset-root-directory (remnant of the Desktop Folder -> Desktop dealie), bug 1163690
	107	6/9/94	till	Move collection protocol code out to collections;StreamCollections
	108	6/10/94	till	appleFileSpecGCHandle needs a setter.  bug 1161841
	109	6/10/94	till	braino from above.
	110	6/10/94	dy	set appleFileSpecGCHandle to false mustn't mess with osPathname
	111	6/13/94	till	bug 1165210: files-internal does not include
				"invisible" files.
				Added pathname-flag-bits and pathname-invisible-p.
	112	6/15/94	till	D'oh!  Use a more correct Decktop directory 
				in root-directory.
	113	6/17/94	till	Several last minute changes to root-directory-pathname-p, 
				backtranslate-physical-pathname, files-internal, root-directory.
				For bugs 1168117, 1169069, 1169070, 1158048.
	114	6/17/94	till	Bug 116970, avoiding asking aliases too many questions.
	115	6/23/94	till	Backtranslate did something very wrong, bug1170192
	116	6/23/94	sidney	provide a copyfile function that is safe for resource forks
	117	6/23/94	till	Bug 1170089, files no longer does an automatic logicfy-pathname.
	118	6/29/94	till	bug 117089 yet again, I think I've got it this time.
				Modifications to files-internal, root-directory-pathname-p
				root-directory, mounted-volumes, reset-root-alias.
				Added rootify-pathname, drive-name-soft.
	119	7/1/94	till	Bug 1171136, files-internal, when given a macFileType
				argument, needs to allow aliases.
	120	7/1/94	sidney	move drive-soft-name to before it's first use
	121	7/5/94	till	Typo in rootify-pathname
	122	7/8/94	Hernan	New implementation of Translators.
	123	7/8/94	Hernan	newTranslator -> translator (dou!)
	124	7/8/94	Hernan	Sk8ImportableResourceTypesAvailable should not assume the
				internal object of all translators is a resource.
	125	7/11/94	till	Add an eval-when around reset-root-alias.
				(a reference to #4p"Root:" wasn't compiling right).
	126	7/11/94	till	reset-root-alias needs the same treatment.
	127	7/11/94	till	and drive-name-soft
	128	7/12/94	Hernan	Adding a function that given a file and a project, 
				returns a file in the project that points to the 
				same pn as the original file. Makes sure not to
				make another file if one in the project already
				exists.
	129	7/14/94	till	Move root-directory into files-internal, fix the 
				bug where the mac-file-type isn't checked, share some code.  Bug 1173171.
	130	7/15/94	Hernan	Removing "TEXT" from the importable resource
				types available.
	131	7/18/94	sidney	#_openresfile needs a mac-namestring argument, an MCL namestring sometimes breaks
	132	8/1/94	till	Ugly bug is setting the file of a stream.
				Modifications to initialize (file) and (setf file).
	133	8/2/94	till	rootDirectory? to isRootDirectory.
				bug 1178238.  Also but 1167491 (reset-root-alias) 
				and anther while I was in the vicinity.
	134	8/3/94	till	Moved mounted-volumes and drive-name-soft
				to 07-devices for loading order reasons.
	135	8/10/94	till	Fix this last minute bug involving write streams.
				Removed set-initargs (it's already in 10-object 
				and moved the initargs hack there too.
	136	 9/ 1/94	Hernan  	1184298: correctly handling negative resource ids.
	137	9/8/94	till	Bug 1182739, (setf file) for Streams didn't hack the new
				non-keywords just right.
	138	 9/ 9/94	dy      	String comparison of Ostypes should be case
							sensitive.
	139	 9/13/94	till    	Better checking of options for (setf File) of Stream.
	140	10/ 3/94	Hernan  	Resource methods will all start with "mac".
	141	10/ 3/94	Hernan  	hasResources -> macHasResources.
	142	10/ 5/94	chip    	initialize args now accessed with initializerArgument
	143	10/ 5/94	chip    	append "option symbol" --> SK8::append
	144	10/21/94	till    	Bug 1194137, fileType shouldn't be returning :unspecific.  Make it return false instead.
	145	10/24/94	chip    	getting rid of bogus *initargs-during-initialize* global!!!
	146	10/31/94	sidney  	create a SK8ApplicationFile File object to hold the pathname of the application
							use (ccl::startup-directory) instead of (mac-default-directory) which may not always be correct
	147	11/ 1/94	till    	Bug 1191213, make files-internal work when the given directory looks like a file (ie., the last colon is missing).
	148	11/ 2/94	Hernan  	Fixing macTotalSk8ImportableResources to call
							macSK8ImportableResourceTypesAvailable.
	149	11/ 2/94	till    	Better implementation of my the thing I did above.
	150	11/ 3/94	till    	Fixing some of the discrepency between folders and files.  Added coerce-pathname-to-directory,  added directoryp-improved, and updated all callers of directoryp.   This should fix a bunch of random issues.
	151	11/11/94	till    	FileAlias -> DirectoryNickname
	152	11/28/94	dy      	gc macro mods
	153	12/ 7/94	Hernan  	Replacing error with the appropriate sk8-error.
	154	12/19/94	dy      	provide localVirtualProperties of File (wasn't implemented)
	155	 1/17/95	till    	trapwrappers for OpenResFile, UseResFile, GetResInfo
							                                          Get1IndResource, Get1NamedResource, DetachResource
							                                          UpdateResFile
	156	 1/18/95	till    	Substitute RemoveRes for (closeResFile (openResFile)) idiom
	157	 1/19/95	till    	make CreateFile return something reasonable
	158	 2/ 8/95	till    	I'm an idiot... try again. (I am checking this in 
							since Don is not here and I need to mess with this
							file. - Hernan.)
	159	 2/ 8/95	Hernan  	Fixing file handlers to not crash when the thing
							given to them does not have a pathname.
	160	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	161	 2/10/95	sidney  	there was a let+ that should have been a let
	162	 2/16/95	sidney  	when converting mcl pathname to sk8 pathname make relative to sk8 directory if possible
							readable argument names for initialize handler
	163	 2/17/95	Hernan  	In writeObject of file, added the "[]" marks to 
							indicate than an anonymous file is not readble
							SK8Script.
	164	 2/21/95	Hernan  	122242: date -> DateTime.
	165	 2/22/95	dy      	mf::undisposable-object! now takes a boolean instead of 1 or 0
	166	 2/23/95	till    	bug 1215523, add mountedVolumes
	167	 2/27/95	till    	Add flush handler
	168	 3/ 2/95	sidney  	Byte is now being used as an object, can't use it as a local variable
	169	 3/ 2/95	till    	bugs 1202657 and 1175190, frob mac-file-type to
							optionally not follow aliases.
	170	 3/ 7/95	sidney  	getting a C string when field is really an OSTYPE caused sporadic crashes
	171	 3/16/95	rod     	fileSpecGCHandle -> appleFileSpecGCHandle
	172	 3/19/95	sidney  	move startup-pathname into ccl package and build part 1
	173	 3/21/95	till    	bug 1230611, sk8-filename-to-mcl-pathname
							is a bit too eager to make the file logical
	174	 3/22/95	sidney  	define (FileExists nil) = nil to prevent errors in some places where it is called
	175	 4/ 3/95	Hernan  	Defining (setf ospathname) of file to always make sure
							the pathname stored is a logical pathname.
	176	 4/ 4/95	Hernan  	move isRootDirectory after RootDirectory is created
	177	 4/ 4/95	till    	Fix the above.  Added backtranslate-physical-pn-if-appropriate.
	178	 4/ 4/95	till    	Move the prev function a little closer to the top.
	179	 4/14/95	till    	bug 1238472, rewrite backtranslate-physical-pathname to actually work.
	180	 4/19/95	till    	I don't know how, but two (count'm two) typos crept into b-p-pn-if-a.
	2  	 8/17/95	sidney  	sk8 Stream object has to be in different package than CL Stream
	3  	 9/ 6/95	Brian   	Fixing streams' horrendous API.  Now you can 
						dynamically set the file and direction of the
						stream object at times other than just creation.
						Also made saving and loading work.
	4  	11/13/95	sidney  	fix copyfile to work with, e.g., (copyfile "foo.bar" "foo1")
	5  	11/28/95	Hernan  	Adding macHasResourceFork. Also making macGetResource
						HandleInfo go down to the resource level even when the 
						file in question is a PICT file and you know you want a 
						resource of a type other than PICT.
	6  	 2/ 7/96	sidney  	remove wood
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 9/ 3/96	Hernan  	Added some error checking to a few handlers.
	2  	10/17/96	Hernan  	openProjectFile sends the opened event to the project.
	3  	10/21/96	sidney  	handle a dropped file by calling openproject
	4  	11/26/96	Hernan  	Moving assure-ostypestring to File.lisp.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
