(in-package :cl-user)

;;; SK8 © 1994 by Apple Computer, Inc.
;;; The code in this file is copyrighted and considered Confidential and Proprietary by Apple Computer, Inc.
;;; Its possession or use, printed or electronic duplication or any kind of distribution without written consent
;;; by Apple Computer, Inc. shall be prosecuted to the fullest extent of the law.
;;; Apple Computer, Inc. -- Advanced Technology Group

(unless (find-package :graphics-system)
  (defpackage :graphics-system
    (:use :ccl :common-lisp)
    (:nicknames :gs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SK8 LOAD
;;;

;;; About QUEUE-BUILD-FIXUP:
;;;
;;;   This macro is exported to all of our packages.  It's purpose is to allow you to evaluate forms
;;;   after the bulk of the build has finished; it's useful when the form can't be evaluated where
;;;   you'd like it to be.  An example would be defining a particular handler on an object that doesn't
;;;   get loaded until later:
;;;
;;;   (queue-build-fixup
;;;     (define-handler someHandler (SomeObject)
;;;       ...))
;;;

#| Modification History

03-30-93 ruben added MCLwindows-patch & removed textdefinitions & colorbug MCL patches
03-14-93 adam  added queue-build-fixup; changed how save and restore work
02-20-93 ruben added ui compaction to save-sk8
02-02-93 ruben containerCollection.lisp loaded by SK8Script
01-31-93 ruben added load of MCLinconsistentClassMsg.fasl
01-27-93 ruben took out references to macframes.rsrc
01-26-93 ruben new global mf::*ui-package*, ui package no longer uses SK8
01-25-93 ruben create UI package so that it can be used by DISPOSE [project]
01-17-93 ruben took out handler caching at load time
10-05-92 ruben load containerCollection
10-01-92 ruben preserve and retore sk8script
09-17-92 ruben MacFrames VI adjustments
09-12-92 ruben save-sk8 moved to SK8 package
08-26-92 ruben set top listener package to :SK8
08-24-92 ruben added MCLselection-eval MCL patch
08-21-92 ruben :sk8-ui and :store features
07-26-92 ruben eval-enqueue set-default-namespace to SK8 project
05-12-92 ruben d29 conversion
03-30-92 ruben began d29; no daemon loading
03-24-92 ruben load patches to MCL
01-03-92 ruben changed it for MacFrames II (d28)
08-09-91 ruben added kernel-only-p option for Matthew
08-02-91 ruben inheritance selection at restart
08-01-91 ruben mac/lisp memory allocation checks mac percentage only
07-29-91 ruben clears caches on startup
06-18-91 adam  Attached comment-tweaker to the TAB key
05-24-91 ruben :development maintains complete SK8Script/GLisp plist info in image
05-22-91 ruben mac heap memory adjusted at 10% for small SK8 images
05-21-91 ruben method caches cleared before saving application
05-19-91 ruben mf::load-patches and mf::load-preferences called when system boots

|#

;;; Dave Yost likes to debug without tail recursion elimination. 
;;; There is already a facility for that, setting the debug optimization to 3.

#+MaxDebug
(when *development-p*
    (proclaim '(optimize (debug 3))); Enable reasonable backtraces by turning off tail recursion elimination
  ;; (setf (getf *sk8-features* :save-definitions) t)
  ;;  (setf (getf *sk8-features* :save-definitions) t)
  (setf ccl::*save-definitions* t) ; Enable stepping
  (format t "~&MaxDebug feature is active~&"))

(defvar mf::*build-in-progress-p* t)

(ccl::def-ccl-pointers more-masters ()
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  (require-trap #_moremasters)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  #~symbol   saves the case of the given symbol as it reads it.
;;;  E.G.:
;;;       #~FooBar
;;;       #~UI::eatMe
;;;
;;;  NOTE:  If you're qualifying the symbol with a package name, the package name MUST be in UPPERCASE!
;;;

(defun read-sym-with-case (stream ch n)
  (declare (ignore ch n))
  (let ((real-readtable-case (readtable-case *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (unwind-protect (CCL::maybe-save-original-sym-case (read stream t nil t))
      (setf (readtable-case *readtable*) real-readtable-case))))
(set-dispatch-macro-character #\# #\~ 'read-sym-with-case *readtable*)



;;; It's far better to use functions proclaimed inline than to use macros.
;;; But ya gotta tell the compiler to notice.
(new-compiler-policy :open-code-inline t)


;; There's a buggy feature in the sourceserver interface code that causes the mac default directory to be
;; changed when the source is updated from he source control database. In any case, the MCL doc says
;; that the mac default directory can change at any time, so we should force it to the correct value
;; here, just before building, just to be safe. That's why the following line is here.
(set-mac-default-directory (mac-namestring "ccl:"))

(eval-when (eval compile load)
  ;;; Define the XcmdBlock record type for communication to and from XCMDs:
  (defrecord (XCmdBlock :pointer)
    (paramCount  integer)
    (param1      handle)
    (param2      handle)
    (param3      handle)
    (param4      handle)
    (param5      handle)
    (param6      handle)
    (param7      handle)
    (param8      handle)
    (param9      handle)
    (param10     handle)
    (param11     handle)
    (param12     handle)
    (param13     handle)
    (param14     handle)
    (param15     handle)
    (param16     handle)
    (returnValue handle)
    (passFlag    boolean)
    (entryPoint  pointer)
    (request     integer)
    (result      integer)
    (inarg1      pointer) ;; These were 'longint's, but they are sometimes used as pointers;
    (inarg2      pointer) ;; making the field-types 'pointer' stops MCL from fiddling with
    (inarg3      pointer) ;; the high bits.
    (inarg4      pointer)
    (inarg5      pointer)
    (inarg6      pointer)
    (inarg7      pointer)
    (inarg8      pointer)
    (outarg1     pointer)
    (outarg2     pointer)
    (outarg3     pointer)
    (outarg4     pointer)))

;;; DOWNSIZING (only two options currently supported)
;;;

;;(cond ((getf *sk8-features* :minimal-version)
;;       (purge-functions t))
;;      (t (preload-all-functions)
;;         (purge-functions nil)))

;;; Used by XCMDs

(defparameter mf::*params* nil)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load the metering stuff
;;;;;;;;;;;;;;;;;;;;;;;;

#|

(when-debug (:metering-startup)
  (ccl::add-feature :metering))
(when-debug (:metering)
  (load  "ccl;lib:meter.fasl"))
(when-debug (:metering-startup)
  (defun SK8::SK8TopLevelMetered ()
    (init-metering)
    (meter (funcall 'SK8::SK8TopLevel))
    (meter-results)
    )
  )

|#

;;;;;
;; The sk8-preload files declares the symbols that will be used for objects as
;; special constants. That allows us to use the compiler to catch attempts to special
;; bind them, something that otherwise could lead to obscure errors at runtime.
;; We have to load the original WOOD code before all that because we are not changing
;; the WOOD files as they are delivered, and they might not compile after the preload.
;; remove wood (sk8-build-files "store;wood:load-wood")
;; the standard way of loading is to use (wood::load-wood), but the following lets us
;; use the sk8 build method to compile files only when we want to
;; remove wood (dolist (f wood::*wood-files*)
;; remove wood  (sk8-build-files (merge-pathnames f "wood:wood;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Do the preload, which basically consists of exporting all the necessary
;;;  symbols from the SK8 Project's and the UI Project's packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sk8-build-files "SK8;Preload:SK8-preload.lisp")

;;;;;;;;;;;;;
;;; Build utility
;;;;;;;;;;;;;

(defvar %SK8-BUILD-FIXUPS% nil)
;;(defvar %FINAL-SK8-BUILD-FIXUP% nil)
;;; Queue up some code to run after SK8's all loaded up
(defmacro queue-build-fixup (&body body)
  `(push '(progn ,@body) %SK8-BUILD-FIXUPS%))
;;(defmacro CCL::queue-final-build-fixup (&body body)
;;  `(setq %FINAL-SK8-BUILD-FIXUP% '(progn ,@body)))

(export 'queue-build-fixup :cl-user)
(import 'queue-build-fixup :CCL)
(import 'queue-build-fixup :MF)
(import 'queue-build-fixup :SS)
(import 'queue-build-fixup :SK8Dev)
(import 'queue-build-fixup :UIDev)

;;;;;;;;;;;;;;;;;;
;;; Load MacFrames!
;;;;;;;;;;;;;;;;;;

;;; Forward Stub so the build process works right
(unless (fboundp 'sk8dev::update-windows-for-new-depth)
  (defun sk8dev::update-windows-for-new-depth ()))

(sk8-build-files "sk8;02-object system:load object system")

;;;;;;;;;;;;;;;;;;
;;; Load SK8Script!
;;;;;;;;;;;;;;;;;;

(when (or (memq :SK8ScriptRuntime *features-wanted*)
          (memq :SK8Script *features-wanted*))
  (sk8-build-files "sk8;05-SK8Script:Load SK8Script Runtime"))

(when (memq :SK8Script *features-wanted*)
  (sk8-build-files "sk8;05-SK8Script:Load SK8Script"))

;;; Took 04-file out, put it where it really should go, but glisp uses the 
;;; symbol Stream for local variables. Since Stream is defined as an object in 04-file,
;;; We cannot compile the glisp files after 04-file has been loaded unless we change the
;;; name of the local variable 'stream' everywhere it is used there.
;;; Easier to leave 04-file here than to fix that right now.  StreamCollections has to follow.

;; (sk8-build-files "mf-kb;04-file")
;; (sk8-build-files "SK8Script;collections:StreamCollections")

;;; putting a valid File object into the SK8 project object

(let ((file  (sk8::new sk8::File :project sk8::SK8)))
  (setf (sk8::logicalname file) "sk8;SK8 Temporary Files:SK8")
  (setf (sk8::file sk8::sk8) file))

;;; initialize-bogus-project -- creates a new SK8 project file by
;;;           using resources from SK8.rsrc and by
;;;           making it a valid project file

(mf::initialize-bogus-project sk8::sk8)

;;;;;;;;;;;;;;;;;;;
;;; Load 2-D Graphics!
;;;;;;;;;;;;;;;;;;;

(when (memq :graphicsSystem *features-wanted*)
  (sk8-build-files "sk8;03-Graphics System:GraphicsSystem"))

;;;;;;;;;;;;;;;;;;;
;;; Load the Store.
;;;;;;;;;;;;;;;;;;;

;;; The store is really needed also in runtimes. Need to separate
;;; store into runtime and store components. 

(sk8-build-files "sk8;04-Store:Load Store")

(when (eq *features-wanted* *runtime-release-preferences*)
  ;; Not loading the store. Is it a runtime build? Load runtime
  ;; requirements. 
  (sk8-build-files 
   "sk8;05-SK8Script:Store:RAM Store"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and Core Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq :objects *features-wanted*)
  (sk8-build-files "sk8;07-Objects:Load Objects"))
  
;;;;;;;;;;;;;;;;;;;;;;
;;; Load Library Functions
;;;;;;;;;;;;;;;;;;;;;;

(when (memq :functions *features-wanted*)
  (sk8-build-files "sk8;06-Globals:Load Globals"))
    
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Project Builder!
;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq :ui *features-wanted*)
  (sk8-build-files "sk8;08-UI:make"))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Development Utilities.
;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq :developmentUtilities *features-wanted*)
  (sk8-build-files "sk8;09-Development Utilities:Load-development-utils"))

;;(mf::close-registry)

(when (memq :graphicsSystem *features-wanted*)
  (if (memq :ui *features-wanted*)
    (setf (SK8::menuBar SK8::Stage) ui::projectbuildermenubar)
    (progn
      (setf (SK8::visible SK8::simpleMenuBar) nil)
      (setf (SK8::menuBar SK8::Stage) SK8::simpleMenuBar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval all the queued build-fixups (except for the final one)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (fixup %SK8-BUILD-FIXUPS%)
  (eval fixup))
(in-package :cl-user) ; make sure we get back into the right package!
(unintern 'queue-build-fixup)
(unintern 'ccl::queue-final-build-fixup)
(setq %SK8-BUILD-FIXUPS% nil)
(unintern '%SK8-BUILD-FIXUPS%)

#|

;;;;;;;;;;;;;;;;;;;;;;
;;; SK8 Release Clean-ups
;;;;;;;;;;;;;;;;;;;;;;

(unless cl-user::*development-p*
  
  ;; Remove logical directories that don't have FileAliases.
  (setq *logical-directory-alist*
        (remove-if-not #'(lambda (a) (member a (sk8::knownchildren sk8::FileAlias) 
                                             :key #'sk8::logicalName
                                             :test #'string-equal))
                       *logical-directory-alist*
                       :key #'car))
  
  ;; MacFrames clean-up:
  ;; CANNOT clear this one because the release needs the file information
  ;; in order to build standalone apps. 
  ;; ccl::%source-files% (make-hash-table :test #'eq)    ; Clear all source file information
  (setq ccl::*fast-help* nil                                ; Clear help info hash table
        )
  
  ;; 2-D clean-up:
  ()
  
  ;; SK8Script clean-up:
  (when (getf *sk8-features* :sk8script)
    (let ((SK8ScriptReservedWords (GET 'SK8SCRIPT::SK8SCRIPT 'GLISP::RESERVEDWORDS))
          pfunction-p)
      (do-symbols (s :glisp)
        (when (symbol-plist s)
          (setq pfunction-p (get s 'glisp::pfunction))
          (setf (symbol-plist s) ())
          (when pfunction-p (setf (get s 'glisp::pfunction) T))))
      (do-symbols (s :SK8Script)
        (when (symbol-plist s)
          (setq pfunction-p (get s 'glisp::pfunction))
          (setf (symbol-plist s) ())
          (when pfunction-p (setf (get s 'glisp::pfunction) T))))
      (setf (GET 'SK8SCRIPT::SK8SCRIPT 'GLISP::RESERVEDWORDS) SK8ScriptReservedWords)))
  
  ;; User Interface clean-up:
  ()
  
  ;; Development Packages Clean-up:
  (dolist (p mf::*sk8-dev-packages*)
    (unuse-package (car p) (cadr p)))
  
  ;; Collect garbage:
  (Progn (gc) (gc))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; Define some useful stuff
;;;;;;;;;;;;;;;;;;;;;;;

;;; load-sk8script-patches -- loads SK8Script patches in text files inside the
;;;   toplevel folder named "SK8Script Patches"
;;;
(defun load-sk8script-patches ()
  (let ((files (directory "ccl;SK8Script Patches:*.sk8" :files t))
        (old (sk8::cursor sk8::stage)))
    (setf (sk8::cursor sk8::stage) sk8::cursor12oclock)
    (unwind-protect
      (when files
        (sk8::sendToLog "Loading SK8Script patches..." :attention t)
        (dolist (f files)
          (sk8::loadscriptfile (namestring f) :verbose 1))
        (sk8::sendToLog "...Done loading patches."))
      (setf (sk8::cursor sk8::stage) old)))
  t)

|#

(defun update-the-windows-menu ()
  (ccl::update-windows-menu ccl::*windows-menu*))

(defun preserve-only-sk8script ()
  (dispose-record gs::!*rgn-rect-initializer*!)
  (#_disposergn (car gs::!*temp-region-pool*!))
  (sk8::preserve sk8::sk8))

(defun restore-only-sk8script ()
  (setf gs::!*rgn-rect-initializer*! (make-record :rect)
        gs::!*temp-region-pool*! (list (#_NewRgn)))
  (sk8::restore sk8::sk8))

(defvar sk8dev::*sk8-is-now-booting-p*)   ;; bound while sk8 is starting up

(defun sk8dev::SK8-booting-p ()
  (and (boundp 'sk8dev::*sk8-is-now-booting-p*) sk8dev::*sk8-is-now-booting-p*))

;; this is a dangerous function. it acts like an eval-enqueue, but ensures that the form is executed after any other forms that are eval-enqueued after
;; it is called but before it is executed. it is dangereous because if it is called a second time before the first one's form has been processed, neither of
;; the two will ever execute, each trying to be last. This is used in startup-sk8 to ensure that the open of a finder document will not happen until after
;; all other initialization. Don't use this function elsewhere without extreme care!
(defun sk8dev::delayed-eval-enqueue (form)
  (eval-enqueue
   `(funcall (if ccl::*eval-queue*
               #'sk8dev::delayed-eval-enqueue
               #'eval)
             ',form)))

;;; This function is called when the SK8 Runtime is fired up by dropping a document
;;; onto it (a fasl file). The file is loaded as a project and the application is saved.
;;; TO DO:
;;; (1) fix the resulting app's size given its memory usage.
;;; (2) allos the project to specify a creator type and a bundle resource for the app. 

(defun build-app-from-runtime (projectFile)
  ;; [1] Load the project file.
  (sk8::openProjectFile projectFile)
  ;; [2] Build the application.
  (let* ((appName (pathname-name projectFile))
         (appDirectory (pathname-directory projectFile))
         (app-path (make-pathname :directory appDirectory :name (concatenate 'string appName " App"))))
    (setf cl-user::*application-type* :app)
    (sk8::buildStandAlone (format nil "~a" app-path) :uiWindows nil)))

(defun dispatch-opened-to-projects-in-standalone ()
  (dolist (c (sk8::knownDescendants sk8::Project))
    (unless (or (eq c sk8::sk8) (eq c (and (boundp 'sk8::UI) (symbol-value 'sk8::UI))))
      (sk8::opened c))))

(defun focus-listener-package (window)
  (let ((pkg :sk8dev))
    (CCL::set-package pkg)
    (set-fred-package (window-key-handler window) pkg)
    (mini-buffer-update window)))

;;MCL3.0
(defun startup-sk8-2 ()
  (declare (ftype t gs::restore-sk8)) ;; forward declaration
  (egc (ccl::egc-mmu-support-available-p))
  (setf mf::*files-loaded-from-finder* (ccl::finder-parameters))
  
  (without-interrupts
   
   ;; Initializing random seed.
   (setf *random-state* (make-random-state t))
   
   (if (memq :graphicsSystem *features-wanted*)
     (gs::restore-sk8)
     (restore-only-sk8script))
   ;; (setf ccl::*control-for-modified-error-system* nil) ;; it's safe to use sk8 error system now
   (when (memq :xcmds *features*) (defvar mf::*params* (make-record :XCmdBlock)))
   
   (when (and *development-p* (fboundp 'sk8:dev-mode))
     ;; To make us developers happy
     (when *arglist-on-space*
       (when (CCL::open-doc-string-file nil nil)
         (CCL::maybe-load-help-map)))
     
     (unless *top-listener*
       (setq *top-listener* (make-instance ccl::*default-listener-class* :window-show nil)))
     
     (focus-listener-package *top-listener*) 
     
     (setq SS::!*lisp-dev-mode* t)
     (SK8:dev-mode))
   
   ;; The function ccl::startup-finished is an internal MCL function that must be called
   ;; as the last part of initializing the Lisp environment on startup
   ;; At the time this comment is being written, all it does is enable processing of AppleEvents,
   ;; which were turned off before the image was saved
   (ccl::startup-finished)
   
   ;; (load-sk8script-patches)
   
   (let ((action (pop mf::*files-loaded-from-finder*)))
     (case action
       (:open
        (if (eq *application-type* :runtime)
          ;; The runtime has special behaviour here: load the file (a project saved for standalone)
          ;; and save the resulting app, changing its *application-type* to :app.
          (when mf::*files-loaded-from-finder*
            (process-run-function 
             '(:background-p t)
             #'(lambda ()
                 (build-app-from-runtime (car mf::*files-loaded-from-finder*)))))
          (dolist (f mf::*files-loaded-from-finder*)
            (sk8dev::delayed-eval-enqueue `(ccl::open-application-document ,*application* ,f t)))))
       (:print
        (dolist (f mf::*files-loaded-from-finder*)
          (when (eq :text (mac-file-type f))
            (sk8dev::delayed-eval-enqueue `(ccl::print-application-document ,*application* ,f t)))))))
   ;; If what is starting up is an App, send the opened event to all the projects in it.
   (when (eq *application-type* :app) (dispatch-opened-to-projects-in-standalone))
   ))

(defun startup-sk8 ()
  (setf ccl::*cursorhook* #'ccl::cursorhook)  ;; restore to normal
  (set-cursor *arrow-cursor*)  ;; ensure that the cursor will end up as an arrow
  (setf mf::*events-on* t)  ;; and event handling will end up on
  (gs:without-events  ;; but not while we're starting up
    (with-cursor *watch-cursor*
      
      ;; this is needed just to load the patches files
      (def-logical-directory "ccl;" (truename "ccl:"))
      ;; redefine startup-sk8-2 in a patches file to redefine the bulk of the startup process
      (let ((sk8dev::*sk8-is-now-booting-p* t)
            (ccl::*sk8-suspend-resume-enabled* nil))
        (declare (special sk8dev::*sk8-is-now-booting-p* ccl::*sk8-suspend-resume-enabled*))
        
        (unwind-protect
          (let ((ccl::*suppress-compiler-warnings* t)
                (*load-verbose* nil))
            (mf::load-patches))
          (startup-sk8-2))))))

;;; Now clears ALL caches (specializers and hash tables)
;;;
(export 'SK8::buildStandalone :sk8)

(defun make-sk8-pathname (pathnameString)
  (setq pathnameString (require-type pathnameString 'string))
  (sk8::sk8-filename-to-mcl-pathname (if (or (find #\; pathnameString)
                                             (find #\: pathnameString))
                                       pathnameString
                                       (concatenate 'string "sk8;" pathnameString))))

(defun cleanup-non-runtime-stuff-for-buildStandalone ()
  (when (and (boundp 'sk8::ui) (sk8::is-a sk8::ui sk8::project))
    (sk8dev::closeproject sk8::ui)))

(defun cleanup-for-buildStandalone ()
  (mf::clear-all-caches))

;; When building a non-developer version of SK8, remove whatever we can to make it difficult for someone to use SK8 as a free MCL development environment
;; unless they already have a copy of MCL
(defun maybe-kill-MCL-development-mode ()
  (unless *development-p*  ;; if not saving a development environment turn off easy access to MCL listener
    (fmakunbound 'sk8:dev-mode)
    (fmakunbound 'ccl::make-new-listener)
    (fmakunbound 'select-backtrace)
    (set-syntax-from-char #\( #\a)
    ))

;;*** Note that this prevents things like mouse events from screwing things up, but it does not prevent command-. from
;;  aborting things in a totally inconsistent state. That's because only a without-interrupts will do that and that makes the
;;  computer look like it is all frozen up for the long time that build-standalone takes. It's bad enough that we are not using
;;  an animated watch cursor to provide some kind of reassurance to the user that something is happening
(defun SK8::buildStandalone (pathname &key (uiWindows t) (creator "SK8S"))
  ;; we have been seeing crashes when people mouse around in the middle of this,
   ;;  and the without-interrupts seems to be the only ay to prevent it.
  ;; the other code setting *events-on* to nil and so forth is left over from less drastic attempts
  (without-interrupts
   (let (resourcecopier)
     (setf mf::*events-on* nil) ;; force no event interruptions for the entire save: leave it to the startup code to enable this again
     (sk8::withlockedcursor sk8::watchcursor
       (unless uiWindows
         (setf gs:*no-ui-windows* t)
         (cleanup-non-runtime-stuff-for-buildStandalone))
       ;; just in case any code tried to enqueue stuff to be done later, we had
       ;; better run the queue now, the last chance before we preserve and shut down everything
       (loop
         (if *eval-queue*
           (ccl::eval-next-queued-form)
           (return)))
       ;; Remember whether the stage was covered.
       (setf gs::*stage-covered-when-app-saved* (sk8::covered sk8::stage))
       (if (memq :graphicsSystem *features-wanted*)
         (gs::preserve-sk8)
         (preserve-only-sk8script))
       ;; This must be done after objects are 'preserved' so that project files are in their resting state.
       ;;Adjust media objects and set up for transferring resources to the application file
       ;; Only copy resources from project files
       (let ((resalist (sk8dev::create-the-alist-of-resources-to-copy)))
         (defun copyTempResourcesToSavedMCL (filename)
           (declare (ignore filename))
           (let ((refnum (current-resource-file)))
             (copyResourcesBetweenFiles resalist refnum))
           (mapcar #'(lambda(namelist)
                       (sk8dev::removeres (car namelist))  ;; force close of resource fork of project files
                       (delete-file (car namelist))  ;; build standalone uses the swapfiles, not the real project files, so delete them here
                       )
                   resalist)
           ;;; Clean up postload debris
           (ps::delete-all-temporary-files)
           (when (boundp 'mf::*build-in-progress-p*) ;; this is only done when building SK8, but I don't know where else to put it
             (makunbound 'mf::*build-in-progress-p*)
             (makunbound 'cl-user::*boot-dialog*)
             (makunbound 'cl-user::*development-preferences*)
             (makunbound 'cl-user::*full-release-preferences*)
             (makunbound 'cl-user::*runtime-release-preferences*)
             (fmakunbound 'cl-user::sk8-boot-dialog)
             (fmakunbound 'cl-user::boot-sk8))
           (maybe-kill-MCL-development-mode)
           (fmakunbound 'copyTempResourcesToSavedMCL)))
       (setf resourcecopier '(copyTempResourcesToSavedMCL))
       (pushnew #'update-the-windows-menu ccl::*save-exit-functions* :key #'ccl::function-name)
       (cleanup-for-buildStandalone)
       (gc))
     (setf ccl::*cursorhook* *watch-cursor*) ;; force watch cursor to stick - undo this in startup-sk8
     (set-cursor *watch-cursor*) ;; can't be animated for the rest of this, but can be a watch
     (setq SS::!*lisp-dev-mode* nil) ;; app will boot not in dev mode. A patch file can always turn it on
     (save-application (make-sk8-pathname pathname)
                       :creator creator
                       :clear-clos-caches t
                       :resources resourcecopier
                       :toplevel-function #'(lambda ()
                                              (%set-toplevel #'toplevel)
                                              (catch :SK8EventAbort
                                                (startup-sk8))
                                              )
                       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval any pending queued stuff and, finally, the final fixup (resets toplevel loop!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop
  (if *eval-queue*
    (ccl::eval-next-queued-form)
    (return)))

#|
	Change History (most recent last):
	2	6/10/93	kleiman	defined wood package
	3	6/10/93	kleiman	close-registry closes documentation file
	4	7/6/93	chip	changed definitions of SK8 and UI packages (not to use anything) & added definitions for the development packages, SK8Dev and UIDev
	7	7/28/93	chip	added Roo's new SAVE-SK8 version
	8	8/13/93	kleiman	save-sk8 buildStandalone fixes & clears all caches
	9	8/16/93	kleiman	added load for final many-classes-patch
	10	8/19/93	kleiman	install simpleMenubar at load completion
	11	8/27/93	chip	AGAIN, changed definitions of SK8 and UI packages (not to use anything)
	12	8/31/93	kleiman	make-sk8-package no longer takes USES keyword
	13	9/1/93	kleiman	loads 04-file after Collection is defined by SS
	14	9/1/93	kleiman	now loads MCLclass-defs (redefines defclass)
	15	9/22/93	kleiman	does not load MCLclass-defs.fasl
				does not load old preferences
	16	9/29/93	kleiman	*macframes-readtable* obsoleted
	17	10/1/93	kleiman	added 840av patches load
	18	10/4/93	kleiman	*readtable* is the original default MCL readtable
	19	10/20/93	kleiman	added directory and package defs for PS
	20	10/20/93	kleiman	added call to startup-finished (for MCL 2.0.1)
	21	10/25/93	kleiman	added post-load clean up
	22	11/1/93	kleiman	add file object to SK8 object
	23	11/1/93	hernan	
	24	11/12/93	kleiman	moved initialize-bogus-project here
	25	11/12/93	kleiman	Correctly initializing the sk8 project file.
	26	11/15/93	kleiman	Clean out *logical-directory-alist*
	27	11/17/93	kleiman	remove caps-lock-key check for load-patches
	28	11/17/93	chip	added read-eval-patch to Patches to MCL
	29	11/24/93	hernan	Removing call to initialize-all-projects from
				startup-sk8. Restore of project does all the
				work required.
	30	11/24/93	kleiman	unuse SK8 from SK8Dev when doing a release build
	31	11/24/93	chip	startup-sk8's unwind-protect doesn't execute things twice if startup-sk8 is patched!
	32	11/30/93	hernan	Setting the cursor back to the arrow cursor at the
				end of startUp-sk8.
	33	12/1/93	kleiman	loads sk8-export.fasl
	34	12/2/93	kleiman	Set mac-default-directory just before build
	35	12/3/93	hernan	Adding a keyword to save-sk8 that specifies
				whether UI windows are wanted in it.
	36	12/5/93	sidney	Make sure mac-default-directory is correct before starting build
	37	12/6/93	kleiman	release clean-up
	38	12/7/93	kleiman	Use SK8's creator when saving the SK8 image file
	39	12/14/93	sidney	Load developer-utilities even when building non-developer version, so it can define *sk8-dev-packages*, used in non-developer load
	40	12/14/93	kleiman	SK8Script clean-up (for non-development version) makes sure to preserve SK8Script's reserveWords list!
	41	12/16/93	till	Add initial version of fasl builds stuff.
	42	12/17/93	till	faslling
	43	12/17/93	till	faslling
	44	12/21/93	sidney	Changes so files can be compiled
	45	12/22/93	kleiman	Remove call to close-registry, an obsolete function
	46	1/4/94	sidney	Add load of new MCL patch, MCLcreator-patch
	47	1/10/94	sidney	Fix the order of the last processing before end of a build
	48	1/24/94	dy	added Patches to MCL:FASLS:macptr-termination.fasl to the list of patches to load
	49	1/25/94	sidney	file got mangled during last checkin: fixed garbled character
	50	1/26/94	sidney	whoops!
	51	2/3/94	kleiman	loads "ps;project-files-initialize.lisp
	52	2/7/94	dy	Added Patches To MCL:FASLS:when-unwind.fasl
	53	2/11/94	sidney	ps;project-files-initialize.lisp is no more again
	54	2/11/94	sidney	define development packages before loading MCL patches
	55	2/15/94	kleiman	
	56	2/21/94	sidney	move early load stuff to build of the MCL for build image
	57	2/22/94	hernan	Removing inProjects left.
	58	2/25/94	sidney	move load of development-utils into build part 1 file
	59	3/1/94	kleiman	startup doesn't refer to "current project" notion
				loads SK8Script patches after everything's restored
	60	3/2/94	Hernan	Not making transcipts anymore since there is 
				some problem with the new Fred there...
	61	3/6/94	Hernan	Remembering whether the stage was covered
				when the application was saved.
	62	3/13/94	sidney	making more files compiled instead of loaded
	63	3/16/94	sidney	get startup arguments from finder and open files if they say to
	64	4/4/94	kleiman	save-sk8 -> buildStandalone
	65	4/4/94	kleiman	ps::compact-resources arguments were incorrect
	66	4/4/94	kleiman	buildStandalone now accepts SK8-native pathnames only
	67	4/5/94	sidney	enable egc if mmu is enabled
	68	5/12/94	till	Tell the compiler to open code inlined functions.
	69	6/7/94	sidney	RADAR #1160764: Changed mf::compact-sk8 to sk8::forceGarbageCollection
	70	6/9/94	till	Took out that kludge where files get loaded after collections.
	71	6/9/94	till	04-file is back.  damn
	72	6/9/94	till	StreamCollections needs to follow 04-file, in the 
				nonsense mentioned above.
	73	6/9/94	till	Arrrrrghh.
	74	7/4/94	sidney	make sure system is saved with events enabled even if built in the background
	75	7/5/94	kleiman	1171346 MoreMaster calls on SK8 startup
	76	7/7/94	sidney	valid filename for streamcollections
	77	7/8/94	Hernan	added symbol-case-saving reader macro here
	78	7/8/94	chip	correctly qualified the call to maybe-save-original-sym-case in read-sym-with-case
	79	7/19/94	kleiman	1175346 *buildstandalone-dont-save-projects*
	80	8/8/94	Hernan	Removing ancient reference to children.
	81 	 9/30/94	Hernan  	1189838: The menubar is not hidden if SK8 is not
							on the foreground when startup-sk8 runs.
	82 	10/ 6/94	dy      	update-windows-for-new-depth stub so build works
	83 	10/ 6/94	dy      	fix update-windows-for-new-depth stub so build works
	84 	10/ 7/94	chip    	added global, mf::*build-in-progress-p*, that indicates build is happening
	85 	10/28/94	dy      	support for saving log file from build
	86 	11/ 1/94	chip    	fixed verbose arg in loadScriptFile call in load-SK8Script-patches (radar #1184619)
	87 	11/ 2/94	sidney  	define SK8-booting-p, which is true while sk8 is starting up
	88 	11/16/94	chip    	now cleans up the "build-fixups" globals more thoroughly!
	89 	11/16/94	sidney  	move SK8 project file to the new SK8 Resources folder
	90 	11/16/94	till    	Added Patches to MCL:MCLpathname-asterisks.lisp
	91 	11/16/94	till    	minor correction to above
	92 	11/17/94	sidney  	correct ton dillman's dyslexic expression
	93 	11/17/94	sidney  	check for files needed for beta version of sk8 on startup
	94 	12/ 4/94	sidney  	1200474: queued build stuff not being run during the build.
	95 	12/ 6/94	sidney  	removed obsolete keyword and dead code that called undefined function from buildstandalone
	96 	12/12/94	sidney  	remove requirement to have traps.idx at runtime
	97 	12/15/94	jol     	load macWidgets
	98 	 1/16/95	rod     	Making the projectbuildermenubar be the 
							default.
	99 	 1/31/95	sidney  	1180502: move loading of case-sensitive names to end of build from preload, and use strings instead of symbols in file of case information
	100	 2/ 3/95	dy      	Put in a #+DYost form
	101	 2/ 8/95	Hernan  	startup-sk8 blocks our suspend and resume handlers
							from being called while SK8 is coming up.
	102	 2/10/95	sidney  	make buildstandalone move resources from project files to the application file
	103	 2/16/95	sidney  	clear reference to file in project objects when building standalone is merging project files into the app
	104	 2/22/95	sidney  	1221621: no more SK8 Resources folder
	105	 2/22/95	dy      	Code for maximum debugging controlled by #+MaxDebug instead of #+DYost
	106	 3/ 3/95	Hernan  	startup-sk8 no longer hides the menubar.
	107	 3/ 3/95	Hernan  	Calling focus-listener-package to get the listener
							to come up focused on sk8dev.
	108	 3/ 6/95	dy      	Tell'm we're about to read in the symbols
	109	 3/19/95	sidney  	changes to support new creator type for stabdalone apps
	110	 3/21/95	sidney  	use swapfile so we don't modify project file during buildstandalone
	111	 3/22/95	sidney  	add onelevel of eval-enqueue to queuing of finder event so it gets done at the right time
	112	 3/23/95	sidney  	one level of enqueue was not enough. make it indefinite levels
	113	 3/27/95	sidney  	1232713: buildStandalone: protect against events in the middle of saving, allow for no UI to be saved
	114	 3/27/95	sidney  	1232713: add even more protection, didn't quite get it all last time
	115	 3/27/95	sidney  	1232713: add even more
	116	 3/28/95	sidney  	combination of last two fixes breaks the build: have to use non-animated cursor inside without-interrupts because of side-effects of copying resources
	117	 4/ 4/95	sidney  	turn off dev-mode at startup, allow a patches file to turn it on if we want
	118	 4/ 5/95	sidney  	set the error mode at save time so user can abort SK8 during startup
	119	 4/12/95	sidney  	run any enqueued forms before saving an application
	2  	 6/ 8/95	sidney  	MCL 3.0 changes
	3  	 6/12/95	dy      	Add conditional compilation stuff for metering the startup code
	4  	 6/23/95	sidney  	add forward declaration to avoid a compiler warning
	5  	 6/23/95	Hernan  	Adding a line to startup-sk8-2 to generate a new random
							seed each time SK8 comes up.
	6  	 7/28/95	dy      	Comment where we should set :mac-heap-percentage
	7  	 8/ 3/95	dy      	buildStandalone calls cl-user::cleanup-for-buildStandalone, which we can replace with fancy reaper stuff
	8  	 8/16/95	dy      	New cleanup-non-runtime-stuff-for-buildStandalone function
	9  	 9/18/95	sidney  	move load of WOOD original code to before preload of symbols to avoid conflicts
	10 	 9/26/95	Till    	Add Liposuction Mode.
	11 	 9/26/95	Till    	fix awful typo ("undo" did it)
	12 	 9/29/95	Till    	closeProject should be closeproject-for-liposuction
	13 	10/ 9/95	Till    	No, the other one.
	14 	11/18/95	sidney  	startup-finished should be ccl::startup-finished if we want AppleEvents to work, and added comment explaining it
	15 	 1/19/96	sidney  	removing references to old sk8script compiler
	16 	 1/24/96	Hernan  	Now loading the SKIL based SK8Script compiler.
	17 	 2/ 7/96	sidney  	remove wood
	2  	 4/12/96	Hernan  	No need to load Copy Resources since it is loaded already.
	3  	 4/16/96	sidney  	get rid of last of the MCL patches in this file
	4  	 4/16/96	sidney  	logical directories now defined in their load files, not used here
	5  	 4/25/96	Hernan  	Removed all references to liposuction and fixed functions
						to allow automatic generation of SK8 apps by the runtime.
	6  	 4/25/96	Hernan  	Fixing typo in startup-sk8.
	7  	 4/26/96	Hernan  	Added machinery to allow SK8 runtimes to build standalone
						apps by just dropping a fasl file onto them.
	8  	 5/ 3/96	Hernan  	Remembering that the Stage was covered when the app is saved.
	9  	 5/ 9/96	sidney  	changes for new object system
	10 	 7/18/96	sidney  	MCL3.9 requires save-application to be called from a listener, MCL3.0 doesn't allow it
	11 	 9/ 3/96	Hernan  	egc has to be turned off until we figure out the GC problem.
	12 	10/18/96	Hernan  	Making standalones be named "name App".
	13 	10/18/96	Hernan  	When a SK8 saved app starts up, the opened event is sent
						to every project in it (except SK8 and the UI).
	14 	10/18/96	Hernan  	Turning on EGC for all builds.
	15 	10/21/96	sidney  	turning quit behavior back on, delete temporary files when appropriate
	16 	 2/27/97	sidney  	remove ability to easily get to MCL listener from non-developer build
	17 	 3/ 4/97	sidney  	remove workaround of 7/18/96 for bug in MCL3.9 that is fixed in 4.0
						do more to remove ability to easily get to MCL listener from non-developer build
	18 	 3/ 7/97	Hernan  	No longer clearing source file information for release.
|# ;(do not edit past this line!!)
