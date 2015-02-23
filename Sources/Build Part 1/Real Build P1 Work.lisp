(in-package :cl-user)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;; This is the early preparation to produce a version of MCL that can be used to build SK8 and
;; will also be useful for the developers to use to work with SK8 sources.


;;; This creates an MCL image with patches and with resource changes that are necessary.
;;; For convenience in using the resulting image for development work, it also defines the packages
;;; used in SK8 and loads SourceServer (eventually)

;;; These are the patches that we have only in compiled form. We should determine if any of these
;;; are incorporated in MCL 2.0.1 already, and get the sources for any that we really still need.
;;; Since I don't know what these are and presumably they fix problems in MCL, they are being loaded
;;; before we do anything else

;;;  The only things that are conditionalized on  #+SK8 or #-SK8 are defun_X and its relatives in macros-traps.lisp.
;;; These are macros to allow the trap interface code to be used either in SK8 or in raw MCL.
;;; If these macros are changed, remember that you have to recompile all
;;; files that call the macros!!

(pushnew :SK8 *features*) ; This is the master feature that says we're in SK8

;;;MCL3.0 - Put temporary bug fixes for MCL3.0/4.0 here to load them early and set them apart
(let ((ccl::*warn-if-redefine-kernel* nil) (ccl::*warn-if-redefine* nil))
  #+(and ppc-target ccl-4)(load "ccl:Build Part 1;Patches to base MCL;Patches to MCL3.0;fixnum-conditional-patch")
  )

(def-load-pointers clear-old-menubar-at-startup ()   ;; don't leave the menu garbage hanging around on the variable
  (setf ccl::*old-menubar* nil))

;;;;;;;;;;;;;;;;;;
;;; Logical Pathnames
;;;;;;;;;;;;;;;;;;

;; the rest of the logical directories will be defined ok in terms of the base
(def-logical-directory "sk8;" "ccl;sk8:")

(defvar *sk8-features*
  (list :development t                     ; This is a development-only version; if not development, then a release is generated
        :runtime nil                       ; This specifies that a runtime a.o.t. a full release version should be generated
        :sk8script t                       ; Load SK8Script
        :sk8compiler t                     ; Load SK8script compiler
        :sk8listener t                     ; Load SK8Script listener
        :graphics t                        ; Load SK8 Graphics system
        :sk8-ui t                          ; Load SK8 User Interface
        :kernel t                          ; Load MacFrames II object system kernel
        :utilities t                       ; Load various utilities
        :minimal-version nil               ; Minimal version (means try to get under 8 megabytes) *** very funny... - Ruben
        :minimal-system-handlers nil       ; Minimal information kept on system handlers
        :sk8-version 1.0                   ; This SK8 Version
        :analyzer nil                      ; Load runtime code analyzer
        :communications nil                ; Load SK8 networking subsystem
        :image nil                         ; Automatically (try to) save image after finishing boot
        :save-definitions nil              ; ccl::*save-definitions* gets set to this
        ))

;;; Is this a development version?
(defvar *development-p* nil)
;;; As small system handlers as possible?
(defvar *small-system-handlers-p* nil)

;; name of MCL image that we will use to build SK8 that we are creating here
(defvar *sk8-build-appname* nil)

(setq *paste-with-styles* nil)

(setf (getf *sk8-features* :development)
      (y-or-n-dialog "Do you want to build a developer image (with SourceServer) or a user image?"
                     :yes-text "Developer" :no-text "User"))

(let* ((defaultappname
         (concatenate 'string "MCL " (ccl::lisp-implementation-short-version) " SK8 "
                      (if (getf *sk8-features* :development) "1.1" "REL")))
       (appname (get-string-from-user "Enter a name for the saved image file"
                                      :initial-string defaultappname
                                      :allow-empty-strings t)))
  (when (string= appname "")
    (setf appname defaultappname))
  (setf *sk8-build-appname* (concatenate 'string "ccl:" appname))
  (let ((logfile (concatenate 'string  *sk8-build-appname* " log")))
    (delete-file logfile :if-does-not-exist nil)
    (dribble logfile)))

(dolist (f '(
             "ccl:Build Part 1;Patches to base MCL;resources"  ;; we need this one loaded to decide if we want to compile it!
             ))
  (let ((ccl::*warn-if-redefine-kernel* nil) (ccl::*warn-if-redefine* nil))
    (load f :verbose t)))

(defun reset-sk8-globals ()
  (setq *development-p* (getf *sk8-features* :development)
        *trace-print-length* nil
        *trace-print-level* nil
        *load-verbose* nil
        *save-doc-strings* nil
        *record-source-file* t
        *fasl-save-local-symbols* t  ;; like *save-local-symbols*, must be t for lfuns to be able to point to objects
        *save-local-symbols* t    ; Used by SK8Script debugger, and anything that needs handler lfuns to point to objects
        *save-definitions* (getf *sk8-features* :save-definitions)
        *small-system-handlers-p* (getf *sk8-features* :small-system-handlers)
        *save-fred-window-positions* t
        *verbose-eval-selection* *development-p*)
  )

(reset-sk8-globals)

(let ((ccl::*warn-if-redefine-kernel* nil)
      (ccl::*warn-if-redefine* nil))
  (defun reindex-interfaces ()
    (format t "~%You must use a plain vanilla MCL to run (reindex-interfaces)~%")
    ))

;;;--------------------
;;; The following are from SourceServer Sources

(eval-when (eval compile #-debugged load)
  (defrecord (ckid :handle)
    (checkSum longint)
    (LOC longint)
    (version integer)                   ;  this definition is for VERSION 4
    (readOnly integer)                  ;  0 = readonly   nonzero = readwrite
    (branch byte)
    (modifyReadOnly boolean)            ;  T = modreadonly 
    (unused longint)
    (checkoutTime longint)
    (modDate longint)
    (pida longint)
    (pidb longint)
    (userID integer)
    (fileID integer)
    (revID integer)
    (projectlen byte)))

;;; end stuff from sourceserver source
;;;------------------------------------

(defun copyResourcesBetweenFiles (resList toRefnum)
  "copy resources to specified resource file. resList, is a list (file1 ((resourceName1 id1 (fromid2 . toid2) ...) ...)"
  (dolist (filres resList)
    (with-open-res-file (refvar (car filres))
      (#_updateresfile refvar)  ;; make sure file is in shape for copying
      (dolist (res (cdr filres))
        (let ((restype (car res))
              reshand fromresid toresid)
          (dolist (resid (cdr res))
            (if (listp resid)
              (setf fromresid (car resid) toresid (cdr resid))
              (setf fromresid resid toresid resid))
            (setf resHand (get-resource restype fromresid t))
            (if resHand
              (let ((resourceName nil))
                        ;; get the resource name, if any.
                (if (stringp fromresid)
                  (setf resourceName fromresid)  ;; pass in string as id if that's the name
                  (%stack-block ((id 2)   ;;  This code copied from getResourceInfo in 04-file.lisp
                                 (type 4)
                                 (name 256))
                    (#_GetResInfo resHand id type name)
                    (setf resourceName (%get-string name))
                    (when (zerop (length resourceName))
                      (setf resourceName nil))))
                (detach-resource resHand)
                (using-resource-file toRefnum
                  (delete-resource restype toresid t)
                  (add-resource resHand restype toresid :name resourceName)
                  (write-resource resHand)
                  (release-resource resHand)))
              (warn "No '~a' ~s resource in source file ~a"
                    restype fromresid (car filres)))
            ))))))

(defun copyNewResourcesToSavedMCL (filename)
  (declare (ignore filename))
  (let ((refnum (current-resource-file))
        (reslist   ;; files and resources to copy
         '(("ccl:Build Part 1;Resource Patches;Various resource patches"
            ("BNDL" 128 129)
            ("FREF" 128 129 130 131 132 133 134 135 136 137 138 139)
            ("icl4" 128 129 130 131 132 133 134 135 136 137 138 139)
            ("icl8" 128 129 130 131 132 133 134 135 136 137 138 139)
            ("ICN#" 128 129 130 131 132 133 134 135 136 137 138 139)
            ("ics4" 128 129 130 131 132 133 134 135 136 137 138 139)
            ("ics8" 128 129 130 131 132 133 134 135 136 137 138 139)
            ("ics#" 128 129 130 131 132 133 134 135 136 137 138 139)
            ("LSIZ" 1)
            ("SK8 " 0)
            ("vers" 1 2)))))
    (copyResourcesBetweenFiles reslist refnum)))

;;;-------------------------------------------------
;;; Handle the "recompile everything" stuff.

(defun file-ckid-revID (path)
  (with-open-res-file (nil path :errorp nil)
    (let ((ckid (get-resource "ckid" 128 t)))
      (and ckid (rref ckid ckid.revID)))))

(defvar *give-recompile-reason* nil) ; you can set this to t for more information
(defvar *recompile-required-timestamp-file* "ccl:sk8;Recompile Required Timestamp")
(defvar *recompile-required-timestamp*      (ccl::mac-to-universal-time 0))
(defvar *recompile-required-revID*        0)
(defparameter *full-recompile-wanted* nil) ;; Redefined in sk8-init.lisp

;;; We used to use the file-write-date for timestamping the fasl files to determine
;;; when we need to recompile. The MacOS makes that ambiguous by not dealing with the
;;; notion of time zone. We work around that now by using the raw Mac form of the date,
;;; seeing if it has changed without trying to interpret it as a universal time

(defun get-global-recompile-date ()
  (let ((timestamp-file-exists (probe-file *recompile-required-timestamp-file*))
        (ckid-revID (file-ckid-revID *recompile-required-timestamp-file*))
        (now (get-universal-time)))
    
    (unless timestamp-file-exists
      (break "You must get a copy of the checked-in \"ccl:SK8;Recompile Required Timestamp\" file and start over."))
    
    (unless ckid-revID
      (break "Your copy of \"ccl:SK8;Recompile Required Timestamp\" is bogus; it doesn't have a valid ckid resource.
Get a valid copy of the file and start over."))
    
    (when *full-recompile-wanted*
      ;; Make sure timestamp-file exists and has NOW as its write-time
      (set-file-write-date *recompile-required-timestamp-file* now))
    
    (setf *recompile-required-revID*     ckid-revID)
    (setf *recompile-required-timestamp* (ccl::mac-file-write-date *recompile-required-timestamp-file*))
    )
  
  (values))

;;;-------------------------------------------------
;;; sk8-build-files and related matters

(defrecord (CCLd :handle) ; this is the old version 
  (write-date unsigned-longint)
  (pathname   unsigned-longint) ; really a string
  )

(defrecord (CCLe :handle)
  (version longint) ; the version of this record
  (recompile-required-revID unsigned-longint)
  (write-date               unsigned-longint)
  (pathname                 unsigned-longint) ; really a string
  )

;;; MCL appears to have something called field-descriptor-offset, but it's not documented
(defmacro record-field-offset (accessor)
  (ccl::parse-accessor accessor))

(defun set-source-info-stamp-res (pathname
                                       write-date
                                       filename
                                       recompile-required-revID)
  (let ((resType "CCLe")
        (resID   1))
    (with-open-res-file (nil pathname :if-does-not-exist :create)
      (let ((handle (#_NewHandle (+ (record-field-offset CCLe.pathname)
                                    (length filename)
                                    1 ; for the Pascal string length
                                    ))))
        (setf (href handle (CCLe.version                 )) 1)
        (setf (href handle (CCLe.recompile-required-revID)) recompile-required-revID)
        (setf (href handle (CCLe.write-date              )) write-date)
        (ccl:%put-string      handle filename (record-field-offset CCLe.pathname))
        (ccl:delete-resource         resType resID t)
        (ccl:add-resource     handle resType resID)
        (ccl::write-resource  handle)
        (ccl::detach-resource handle)
        (#_DisposHandle       handle)))))

(defun set-source-info-stamp (sourcefile faslfile recompile-required-revID)
  (let ((in-fullpath  (probe-file sourcefile))
        (out-fullpath (probe-file faslfile)))
    (unless in-fullpath  (error "Can't find source file ~a" sourcefile))
    (unless out-fullpath (error "Can't find output file ~a" faslfile))
    (let ((in-name (file-namestring in-fullpath))
          (in-write-date (ccl::mac-file-write-date in-fullpath)))
      (set-source-info-stamp-res out-fullpath
                                 in-write-date
                                 in-name
                                 recompile-required-revID))))

;;; This is complicated because it accepts a CCLd (old-style) resource.
(defun get-source-info-stamp (faslfile)
  (with-open-res-file (nil faslfile :if-does-not-exist nil)
    (let ((res (ccl:get-resource "CCLe" 1 t)))
      (if (or (null res) (ccl:%null-ptr-p res))
        ;; Try to find an old version of the resource (CCLd)
        (let ((oldRes (ccl:get-resource "CCLd" 1 t)))
          (if (or (null oldRes) (ccl:%null-ptr-p oldRes))
            (values nil nil nil)
            (unwind-protect
              (values (href            oldRes CCLd.write-date)
                      (ccl:%get-string oldRes (record-field-offset CCLd.pathname))
                      nil)
              (#_ReleaseResource oldRes))))
        (unwind-protect
          (values (href            res CCLe.write-date)
                  (ccl:%get-string res (record-field-offset CCLe.pathname))
                  (href            res CCLe.recompile-required-revID))
          (#_ReleaseResource res))))))

;; decide if a target file (binary) is logically out of date with the source file
;; This was made into a separate function so we can use it for building files other than via compilation
(defun reason-for-recompiling (source binary)
  (cond
   ((not (probe-file binary))
    "binary doesn't exist")
   ((< (ccl::mac-file-write-date binary) *recompile-required-timestamp*)
    "binary write time < master write time")
   (t
    (multiple-value-bind (stamped-source-write-date stamped-sourcename stamped-recompile-required-revID)
                         (get-source-info-stamp binary)
      (cond
       ((null stamped-source-write-date)
        "binary has no stamped source write time")
       ((/= stamped-source-write-date (ccl::mac-file-write-date source))
        (format nil "source write time differs from write time stamped stamped in binary (~S ~S)"
                stamped-source-write-date (ccl::mac-file-write-date source)))
       ((cond
         ((not stamped-recompile-required-revID)
          "binary not stamped with recompile revID")
         ((/= stamped-recompile-required-revID *recompile-required-revID*)
          (format nil "revID: binary ~D ­ master ~D" stamped-recompile-required-revID *recompile-required-revID*))
         (t
          nil)))
       ((not (string-equal stamped-sourcename (file-namestring (probe-file source))))
        "source name differs from stamped name")
       (t
        nil))))))

;; If the source file exists, takes care of checking time stamps to see if need to recompile
;; If the source file doesn't exist but the binary does, just load the binary and don't worry about time stamp stuff
(defun sk8-compile-and-load (source binary compile-function)
  (when (probe-file source)
    (let ((reason-for-recompiling (reason-for-recompiling source binary)))
      (when reason-for-recompiling
        (if *give-recompile-reason*
          (format t "~%Compiling   ~a ... (~A)" source reason-for-recompiling)
          (format t "~%Compiling   ~a ..." source))
        (funcall compile-function source :output-file binary)
        (set-source-info-stamp source binary *recompile-required-revID*))))
  (if (probe-file binary)
    (load binary :verbose t)
    (if (probe-file source)
      (error "Can't find binary file ~s after trying to build source file ~s" binary source )
      (error "Can't find source file ~s or binary file ~s during build process" source binary))))

(defun sk8-build-files (&rest filenames)
  "For each file in the list of filenames:
If file type is explicit
   if file exists and needs compiling, compile it (if it is sk8 compiles to filename-sk8.fasl)
   load the fasl file
Otherwise
If filename.sk8  exists and needs compiling, compile it into filename-sk8.fasl.
If filename-sk8.fasl  exists, load filename-sk8.fasl.
If filename.lisp exists and needs compiling, compile it into filename.fasl.
If filename.fasl exists, load filename.fasl"
  (dolist (filename filenames)
    (format t "~%      Build ~a..." filename)
    (let ((ftype (pathname-type filename)))
      (if ftype   ;; explicit type specified in name, handle accordingly
        (progn
          (unless (probe-file filename)
            (error "Source file not found: ~a" filename))
          (cond ((string-equal ftype #.(pathname-type *.fasl-pathname*))
                 (load filename))
                ((string-equal ftype "sk8")
                 (sk8-compile-and-load filename
                                       (make-pathname :defaults filename
                                                      :name (concatenate 'string (pathname-name filename) "-sk8")
                                                      :type #.(pathname-type *.fasl-pathname*))
                                       #'compile-sk8-file))
                (t (sk8-compile-and-load filename
                                         (make-pathname :defaults filename
                                                        :type #.(pathname-type *.fasl-pathname*))
                                         #'compile-file))))
        (let* ((sk8-source (make-pathname :defaults filename :type "sk8"))
               (lispSource (make-pathname :defaults filename :type "lisp"))
               (sk8-source-exists (probe-file sk8-source))
               (lispSource-exists (probe-file lispSource))
               (sk8-binary (make-pathname :defaults (concatenate 'string filename "-sk8")
                                          :type #.(pathname-type *.fasl-pathname*)))
               (sk8-binary-exists (probe-file sk8-binary))
               (lispBinary (make-pathname :defaults filename
                                          :type #.(pathname-type *.fasl-pathname*)))
               (lispBinary-exists (probe-file lispBinary)))
          (when (or sk8-source-exists sk8-binary-exists)
            (sk8-compile-and-load sk8-source sk8-binary #'compile-sk8-file))
          (when (or lispSource-exists lispBinary-exists)
            (sk8-compile-and-load lispSource lispBinary #'compile-file)))))))

;;;-------------------------------------------------

(let ((ccl::*warn-if-redefine-kernel* nil) (ccl::*warn-if-redefine* nil))
  (sk8-build-files "ccl:Build Part 1;Patches to base MCL;utilities" ; DY
                   "ccl:Build Part 1;Patches to base MCL;macptr-termination"
                   "ccl:Build Part 1;Patches to base MCL;MCLcreator-patch"
                   "ccl:Build Part 1;Patches to base MCL;MCLmetapoint"
                   "ccl:Build Part 1;Patches to base MCL;time-utilities"
                   "ccl:Build Part 1;Patches to base MCL;encapsulate" ; DY
                   "ccl:Build Part 1;Patches to base MCL;trace-print" ; DY - must come after encapsulate
                   "ccl:Build Part 1;Patches to base MCL;appleevent-toolkit"
                   "ccl:Build Part 1;Patches to base MCL;ccl-menus"
                   "ccl:Build Part 1;Patches to base MCL;boolean" ; DY
                   "ccl:Build Part 1;Patches to base MCL;hash-set" ; DY
                   "ccl:Build Part 1;Patches to base MCL;resources"  ;; already loaded, but this makes sure file is compiled
                   ))

;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;

(unless (find-package :macframes)
  (defpackage :macframes
    (:use :ccl :common-lisp)
    (:nicknames :mf)))

(unless (find-package :ps)
  (defpackage :ps
    (:use :ccl :common-lisp :macframes)))

;;; SK8 Project Package -- must load after MCLsk8-package.fasl
;;;
;;;  Note: the project-object slot is filled when the SK8 project object is created by 03-project.lisp.
;;;

(defpackage SK8
  (:use)
  (:import-from "COMMON-LISP"  "IN-PACKAGE")   ; fred & the loader get confused without this!
  (:export COMMON-LISP:IN-PACKAGE)   ; fred & the loader get confused without this!
  )

;;; now that sk8 package exists
(defun compile-sk8-file (sourceFile &key output-file)
  (sk8::compileScriptFile sourceFile :outputfile output-file))

(unless (find-package :sk8script)
  (defpackage :sk8script
    (:use :ccl :common-lisp :macframes)
    (:nicknames :ss)))

(defpackage UI
  (:use)
  (:import-from "COMMON-LISP"  "IN-PACKAGE")   ; fred & the loader get confused without this!
  (:export COMMON-LISP:IN-PACKAGE)   ; fred & the loader get confused without this!
  )

(defvar mf::*ui-package* (find-package "UI"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make the development packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage SK8Development
  (:use :ccl :common-lisp :macframes :SK8)
  (:nicknames :SK8Dev))

(defpackage UIDevelopment
  (:use :ccl :common-lisp :macframes :UI)
  (:nicknames :UIDev))

;;;========================================================
;;; Dave's junk.

(export '(with-use-resource-file
           with-open-and-use-resource-file))

(defmacro with-use-resource-file ((resRefNumVar) &body body)
  "UseResFile, do the body, and always put back the previous top resfile"
  `(let ((oldResourceFile (require-trap #_CurResFile)))
     (sk8dev::T_UseResFile ,resRefNumVar)
     (unwind-protect
       (progn ,@body)
       (sk8dev::T_UseResFile oldResourceFile)
       )))

(defmacro with-open-and-use-resource-file ((refNumVar resFile
                                                      &rest rest)
                                           &body body)
  "with-use-resource-file within with-open-resource-file"
  `(with-open-resource-file (,refNumVar ,resFile ,@rest)
     (with-use-resource-file (,refNumVar)
       (progn ,@body))))

;;;========================================================
;;; Sorry for the intrusion. Continue with the good stuff.

(when *development-p*
  (sk8-build-files "SK8;09-Development Utilities:general:general-utils")
  (load "ccl:SourceServer Source;load-SourceServer")
  (setf ccl::*default-menubar* (menubar))
  (setf ccl::*user* nil
        ccl::*user-initials* nil
        ccl::*my-projects* `(("Titan:SK8 on MCL3.0 Project:"
                              "ccl:"
                              )
                             ))
  )
(dribble)
(save-application *sk8-build-appname*
                  :creator "SK8 "
                  :init-file "ccl:init"
                  :size '(25600000 25600000)
                  :resources '(copyNewResourcesToSavedMCL (nil "CCL2" 0))
                  )


#|
	Change History (most recent last):
	2		2/21/94	sidney	File to create an MCL to use for building SK8
	3		2/21/94	sidney	Patch a bug in the Resources library
	4		2/21/94	sidney	Add boyer-moore file search example to developer utilities
	5		2/22/94	hernan	Redefining *default-menubar* to include the
							source server menu.
	6		2/23/94	sidney	Use logical pathname for source files instead of hardcoding the build machine's truename for it
	7		2/25/94	sidney	load development-package-utils (scans for public symbols) in development build
	8		2/25/94	sidney	define preload exporting functions earlier in build
	9		2/25/94	sidney	typo
	10		2/25/94	dy		added  macptr-ptr.lisp to the build list
	11		2/25/94	kleiman	moved load of symbol-case-saving here
	12		2/25/94	kleiman	add a call to sk8dev::initialize-trap-null-ptr-values in reset-sk8-globals
	13		2/26/94	kleiman	added xor.lisp to the load list
	14		2/26/94	kleiman	removed call to initialize trap stuff -dy
	15		3/1/94	sidney	Remove many-classes patch until I can get the source and modify it!!!
	16		3/1/94	sidney	Add MCL libraries to default sourceserver projects
	17		3/1/94	sidney	Setting the user and initials to nil so that you can
							be prompted.
	18		3/2/94	sidney	Fix problem with resource code when resource does not exist
	19		3/2/94	Hernan	Now loading Fred 3.0!
	20		3/4/94	dy		(pushnew :macframes *features*)
	21		3/4/94	dy		:SK8 in *features instead of :macframes
	22		3/4/94	sidney	get logical directories to come up correctly in both lisp and sk8 images
	23		3/5/94	sidney	Remove :SK8 from feature list, pending making sure that things that are turned on by it actually work
	24		3/7/94	sidney	Add gcable classes file to load
	25		3/10/94	yost	reinstate the (pushnew :SK8 *features*)
	26		3/25/94	chip	now loads "wrapper-inspection" patch for development version
	27		3/29/94	sidney	preload stuff should not be only in developer version
							ask first, build later
	28		3/29/94	sidney	change default name for app depending on whether developer or release version
	29		3/29/94	sidney	Move appleEvent support to base MCL
	30		4/1/94	sidney	*fasl-save-local-symbols* must be t for define-handler to work correctly!
	31		4/5/94	sidney	turn on egc by default on startup if mmu support is available
	32		5/1/94	sidney	Radar bug #1157940, made uninformative error message more informative
	33		5/2/94	sidney	whoops. screwed up check for errorp in last fix.
	34		6/29/94	chip	added symbol-case-saving at start of load order
	35		6/29/94	chip	took out old "symbol-case-saving" load
	36		6/29/94	chip	changed restypes to strings (from keywords) since symbols now have arbitrary case
	37		7/6/94	chip	took out the new fangled symbol case-saving mechanism
	38		7/12/94	dy		line up file names in Listener
	39		7/20/94	dy		add pr.lisp
	40		8/4/94	sidney	movw load of mf-kernel;SK8Script-symbols-tools to earlier in build
	41 	 8/22/94	It      	change default mcl save name
	42 	 9/ 2/94	chip    	SK8'ed MCL now has modified compiler policy that doesn't constant-fold any constants holding standard-objects
	43 	 9/ 2/94	It      	fixed typo
	44 	 9/12/94	nw      	Added highlevel-event-patch to load list
	45 	 9/14/94	sidney  	only allow reindex-interfaces to be run in vanilla MCL
	46 	 9/26/94	chip    	made packages SK8Script, SK8Development, UIDevelopment, PS, and Wood use Macframes
	47 	 9/26/94	dy      	add l1-io.lisp to build
	48 	 9/26/94	chip    	oops -- fixed glitch in ":use" clause of defpackage's
	49 	10/ 3/94	dy      	add ccl-menus to the build
	50 	10/13/94	dy      	add "Patches to base MCL;encapsulate.lisp" to the build & #+DYost code for no tail recursion optimization
	51 	10/17/94	sidney  	speedup a screw case in use of hash tables that Wood was getting hit by
	52 	10/19/94	sidney  	compiler policy stuff: use the official MCL functions instead of hacking structures directly,
							change the file compilation policy, not just the interactive one,
							use the existing optimize debug 3 proclamation to give DYost his tail recursion results,
							move all of it so it doesn't effect any of the part 1 build.
	53 	10/28/94	dy      	support for building from .sk8 files
	54 	10/28/94	dy      	fix copyNewResourcesToSavedMCL to copy over small icon masks
	55 	11/14/94	dy      	add apropos fixups
	56 	11/15/94	dy      	 Dave@Yost.COM
	57 	11/16/94	dy      	
	58 	11/16/94	dy      	use new keyword arg on with-resource
	59 	11/16/94	dy      	now does its thing by stamping the revID of the master file in the fasl
	60 	11/16/94	dy      	*give-recompile-reason* sense was reversed
	61 	11/16/94	dy      	unwind-protect in get-source-info-stamp
	62 	11/18/94	sidney  	Always store recompile-required-revID field of CCLe defrecord as a number, never as nil
	63 	12/14/94	dy      	add boolean.lisp to the build
	64 	12/15/94	jol     	fix compile-sk8-file to do the right project thing
	65 	12/16/94	dy      	eval-enqueue compile-sk8-file
	66 	12/16/94	dy      	compile-sk8-file doesn't set the project;  that'll be handled as a default in compileScriptFile
	67 	 1/29/95	sidney  	when deciding to build project files, use the same methods we are using for deciding to compile files
	68 	 2/ 3/95	dy      	Add :save-definitions to *sk8-features*
	69 	 2/ 3/95	dy      	typo
	70 	 2/10/95	sidney  	provide for different from and to resource ids in when we copy resources
	71 	 2/13/95	dy      	remove xor.lisp, when-unwind.lisp, add utilities.lisp, utility-macros.lisp
	72 	 2/20/95	dy      	pr.lisp -> trace-print.lisp
	73 	 2/20/95	dy      	move encapsulate.lisp before trace-print.lisp
	74 	 2/20/95	dy      	remove utility-macros.lisp
	75 	 3/ 2/95	dy      	new with-debugging-features macro
	76 	 3/ 2/95	dy      	Moved with-debugging-features macro to Patches to Base MCL
	77 	 3/19/95	sidney  	new resources for new standalone app creator icons
	78 	 3/22/95	sidney  	increase default minimum memory partition size to 25mb
	79 	 4/24/95	dy      	Make recompile immune to changes in daylight savings time.
	80 	 4/28/95	dy      	recompile-require fix
	1  	 6/ 7/95	sidney  	Move the real work of loading to a separate thread
	2  	 6/ 9/95	sidney  	new default name for sourceserver project for MCL 3.0 version
	3  	 6/ 9/95	sidney  	load a patch to MCL3.0 with a bug fix needed in order to save SK8 application
	4  	 6/ 9/95	dy      	reinstate loading of font-menus.lisp and encapsulate.lisp (prettier tracing)
	5  	 7/10/95	dy      	fix *recompile-required-timestamp-file*
	6  	 8/10/95	dy      	load hash-set.lisp
	7  	 8/14/95	sidney  	work around MCL 3.0 problem by setting *load-verbose* to nil before saving image
	8  	 8/17/95	sidney  	extend the no constant folding policy to also not fold *undefined*
	9  	 1/ 5/96	dy      	change *my-projects* to the path on Epiphany
	10 	 1/10/96	Hernan  	Adding Dave's with-use-resource-file because it requires
						the SK8 package to exist.
	11 	 1/10/96	Hernan  	Using the right path to Epiphany.
	12 	 1/22/96	sidney  	Add error check for source file not found during build
	13 	 1/31/96	sidney  	modify build process to work with skil files and explicit file types
	14 	 2/ 1/96	sidney  	remove kludge to not compile skil files, not needed anymore
	15 	 2/ 2/96	sidney  	create a log file of build part 1. compile the fred patch.
	16 	 4/10/96	sidney  	change a build directory and the default sourceserver project file location
	17 	 4/15/96	sidney  	move :recompile-all out of *sk8-features* and use *full-recompile-wanted* variable instead
	18 	 4/15/96	Hernan  	Removing the load-fred-patch which is now loaded as part
						of the Graphics System.
	19 	 4/16/96	sidney  	move stuff that shouldn't be here out of build part 1
	20 	 6/13/96	sidney  	use proper platform dependent fasl file type
	21 	 6/13/96	sidney  	macptr-termination for MCL 3.0 or 3.9
	22 	 6/25/96	sidney  	increase stack size to work around compiler problem in MCL3.9
	23 	 6/28/96	sidney  	some tweaks to build correctly in newer environment
	24 	 7/ 7/96	sidney  	changes for native PPC build
	25 	 8/ 2/96	Hernan  	Adding the PFSL bundle cicns.
	26 	 8/ 2/96	Hernan  	Cleaning up the bundle resources.
	27 	 8/20/96	Hernan  	Not loading that awful trace stuff.
	28 	 9/ 3/96	Hernan  	Removed loading of some of our macptr-termination patches
						because they are in MCL 4.0
	29 	10/21/96	sidney  	load a compiler patch - To be removed when fixed in MCL 4.0
	30 	10/21/96	sidney  	allow for a build from fasls without sources being available
	31 	10/22/96	sidney  	remove loads and removed commented out loads of totally obsolete stuff
	32 	10/24/96	sidney  	remove compiler patch in anticipation of it being fixed in MCL 4.0
						change default name for build part 1 image to production name "1.1"
	33 	10/24/96	sidney  	remove a workaround for a MCL compiler stack problem that has been fixed in 4.0
	34 	11/14/96	sidney  	put compiler patch back because it hasn't been fixed in MCL 4.0 yet
	35 	 2/27/97	Hernan  	
	36 	 3/ 6/97	sidney  	Remove creation of project-store package which is no longer used
	37 	 3/ 7/97	Hernan  	*record-source-file* is set to t regardless of development-p.
|# ;(do not edit past this line!!)
