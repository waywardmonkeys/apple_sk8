;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;; Symbols inserted into the appropriate preload file  3-18-94   3:40 pm
(SK8-declare-syms :SK8 :public ; Updated 10-10-96   3:33 pm
                  SK8::COMPACTPROJECTCODE SK8::COMPILESCRIPTFILE SK8::COMPILESCRIPTS SK8::LOADFILEPHYSICALNAME
                  SK8::LOADSCRIPTFASLFILE SK8::LOADSCRIPTFASLFILES SK8::LOADSCRIPTFILE SK8::LOADSCRIPTS
                  SK8::PUBLISHEDSYMBOLS SK8::PUBLISHSYMBOL SK8::SEARCHSCRIPTS SK8::UNPUBLISHSYMBOL)

;;;;;;;;;;;
;;; MACROS
;;;;;;;;;;;


;;; withProject -- for temporary switch of project context
;;;  project - the project context in which body is evaluated
;;;

(mf::publish-project-symbol 'withProject mf::*SK8-package*)
(defmacro withProject (proj &body body)
  `(let ((*package* (SK8::package ,proj))) ,@body))

(define-system-handler (setf objectName) (objectName project &key force)
  (if force
    (call-next-method)
    (error "You can't change a project's name!")))

;;; numberOfObjects -- returns the number of objects in the project
;;;

(define-system-handler numberOfObjects (Project)
  (hash-table-count (objectTable me)))

;; project objects should not be saved
(define-handler dontSave (Project)
  t)

;;; publishSymbol -- publishes a symbol to subprojects
;;;  symbol - the symbol to publish
;;;
(define-handler publishSymbol (Project sym)
  (ensuretype sym Symbol)
  (mf::publish-project-symbol sym (sk8::package me))
  )

;;; subscribe-project-symbols -- used to subscribe to symbols which project requires from required projects
;;;   project - subscribing project
;;;

(defun subscribe-project-symbols (proj)
  (let* ((from (sk8:requiredProject proj))
         (required (list from))
         (package (sk8::package proj)))
    (loop (unless (and from (setq from (sk8:requiredProject from))) (return nil))
          (push from required))
    (dolist (from required)
      (do-external-symbols (s (sk8::package from))
        (shadowing-import s package)))))

;;; new --  NEW returns a new project object
;;;    objectName - project objectName (a simple string)
;;;    projectFolder - project's folder pathname (default is to create a folder with the project's objectName in the SK8 application folder)
;;;    requiredProject - project object which is used by this project (default is SK8 project)
;;;    keepNamespace - if T, then namespace has already been created
;;;    properties - list of property specs for project
;;;    otherParents - list of additional parents for project
;;;

;;; The OLD keyword is used internally only when we create the UI project.
;;;
(define-handler new (Project &rest props)
  (declare (dynamic-extent props))
  (destructuring-bind (&key objectName properties ((:project InProject)) otherParents filename old fromStore 
                            &allow-other-keys) props
    ;; Check arguments:
    (unless (eq me Project) (error "You cannot specialize children of Project!"))
    (when properties (error "Illegal to add properties to projects!"))
    (when otherParents (error "Illegal to mix in other parents to projects!"))
    (unless objectName (error "Projects objects must have an objectName!"))
    (unless (stringp objectName) (error "Project objectName must be a string (~s)" objectName))
    (unless inProject (error "Project ~a's required project was not provided" objectName))
    (let ((file-pathname (sk8-filename-to-mcl-pathname 
                          (or filename (concatenate 'string "sk8:" objectName)))))
      (unless (< (length (pathname-name file-pathname)) 32) (error "File names can't be longer than 31 chars."))
      (setq filename (mcl-namestring-to-sk8-filename (namestring file-pathname)))
      (and (not old)
           (probe-file file-pathname)
           (error "File ~a already exists!" filename))
      (let* ((requiredPackage (package inProject))
             (pkgname (string-upcase objectName))
             (package (find-package pkgname))
             proj
             succeeded)
        (if package
          (unless (and (boundp 'mf::*build-in-progress-p*) mf::*build-in-progress-p*)
            (error "Project name ~a cannot be used because name is already in use by a package" pkgname))
          (setf package (make-package pkgname :use nil)))
        (unwind-protect
          (let ((*package* package))
            (remf props :filename)
            (remf props :old)
            (remf props :fromStore)
            (setf proj (apply #'call-next-method me props))
            (setf (requiredProject proj) inProject)
            (push proj (requiringProjects inProject))
            (import (sk8::sk8_id proj) requiredPackage)
            (let ((objtab (make-hash-table :test #'eq :weak :value)))
              (push objtab mf::*all-weak-hash-tables*)
              (setf ;; (ccl::package-project package) proj
                    (package proj) package
                    (objectTable proj) objtab))
            (subscribe-project-symbols proj)  ;;;Moving this from just before the (setq succeeded t) to before the new file because we need our project's package to be kosher before we can make anything in it.
            (setf (sk8::file proj) (unless fromStore (new File :project proj :OSPathname file-pathname)))
            (import (sk8::sk8_id proj) (package inProject))
            (unless old   ;; new projects end up with a new swapfile, but no file
              (setf (sk8::swapfile proj) (new File :project proj :logicalname (ps::make-temporary-file proj t)))
              (setf file-pathname (namestring (sk8::physicalname (sk8::swapfile proj))))
              (mf::create-project-file proj file-pathname) ;;; remove wood
              (T_CreateResFile file-pathname))
            ;;;no more condition tables (unless fromStore (SS::!set-up-project-condition-tables proj inProject))
            (cl-user::open-resource-file file-pathname :if-does-not-exist (unless old :error))  ;; old is a misleading name: it won't be created until later
            (setq succeeded t)
            proj)
          (if succeeded
            (when (not old) (CreatedNewProject system proj))
            (if proj
              (closeproject proj)
              (when package (delete-package package)))))))))

;; This is kind of a kludge, but a bunch of places call this function, left over from when
;; we defined a "sk8-package" extension to the package object
(defun ccl::package-project (pkg)
  (let ((name (find-symbol (package-name pkg) pkg)))
    (when (and name (boundp name))
      (symbol-value name))))

;;; opened -- event sent to a project after it has been successfully opened
;;;
(define-handler opened (Project)
  nil)

;;; RESTORE PROJECT -- This method restores a project. This is done in containment order.
;;;                   We only call preserve on objects that are not contained by anything.

(defmacro |WITH PROJECT| ((proj) &body body)
  `(let ((SS::!*targetProject* ,proj))   ; this affects the project in which NEW objects are created
     (withproject ,proj                  ; this affects the project (package) in which symbols are interned (via "... as symbol")
       ,@body)))

(define-system-handler restore (project)
  (declare (special actor gs::*windows-recreated*))
  (let ((thefile (file me)))
    (setf (sk8::swapfile me) nil)
    ;; [1] opening the pheap file.
    (if (and thefile (neq me SK8::SK8) (neq me SK8::UI) (fileExists thefile))
      (ccl:open-resource-file (physicalname thefile) :if-does-not-exist nil)
      ;; this was an error, but now we'll just assume it is not needed and the resources are stored in the application file (error "Project ~a's file not found in ~a!" me thefile)
      (setf (file me) nil thefile nil)
      )
    ;; [2] restoring all objects.
    (mapProjectObjects me
                       #'(lambda (obj)
                           (unless (or (eq obj SK8)
                                       (eq obj Project)
                                       (inheritsFrom obj Project))
                             (restore obj))))
    ;; [3]. Restore QuickTimeMovies that weren't found
    ;; (finalizeQuickTimeMovies #'restore)
    
    ;; [4] Reconstituting the windows.
    (let (theWindows)
      (dolist (windowPair gs::*windows-recreated*)
        (when (eq me (project (car windowPair)))
          (pushnew windowPair theWindows)))
      (gs::restore-project-windows theWindows))
    me))

(defmethod make-object-gcable ((me Object))
  (when (mf::object-class-p me)
    (without-interrupts
     ;; container/contents relationship
     (when (realproperty me 'container)
       (setf (container me) nil))
     (when (realproperty me 'contents)
       (dolist (contained (contents me))
         (when (realproperty contained 'container)   ;; bulletproofing, in case we're destroying everything
           (setf (container contained) nil)))    ;; and the contents have already been zapped
       (setf (slot-value me 'contents) nil))
     ;; parent/child relationship:
     (dolist (parent (parents me))
       (setf (knownchildren parent) (delete me (knownchildren parent))))
     ;; now all the children, not just knownchildren
     (dolist (childclass (class-direct-subclasses (class-of me)))
       (let ((childobj (mf::class-owner childclass)))
         (when (mf::populationdata? childobj)
           (setf childobj (first (ccl::population-data childobj))))
         (when (ccl::standard-instance-p childobj)
           (mf::remove-parent childobj me t))))
     ;; get rid of handler scripts
     ;; (ps::copy-all-pending-handlers)  ;; have to ensure pending stuff is flushed, but at least this is fast if they already are
     (dolist (hand (localhandlers me))
       (mf::delete-handler hand :object me)) ;; all scripts have null qualifier
     ;; get rid of local properties
     (dolist (slotd (class-direct-instance-slots (class-of me)))
       (setf (slot-value me (car slotd)) nil)
       (removeproperty me (car slotd)))
     ;; make this a simple object. I hope that the values of the old properties will get gc'd ok
     ;;(changeparents me (list Object))
     ;;  removes the object from the objecttable
     (mf::unrecord-object me)
     ;; clear the associated constant if it is named
     (let ((current (SK8::SK8_id me)))
       (when (and (mf::named-object? me) (boundp current))
         (let ((ccl::*warn-if-redefine-kernel* nil))
           (makunbound current))))
     ;; this is critical! make sure that the object's class no longer points to this as a valid sk8 object
     
     (let* ((myclass (class-of me))
            (ownobj (mf::class-owner myclass)))
       (when ownobj
         (if (mf::populationdata? ownobj)   ;; if this object shares its class, don't destroy the class
           (let ((owners (delete me (ccl::population-data ownobj))))
             (if owners
               (setf (ccl::population-data ownobj) owners)
               (ccl::%class-remprop myclass :owner))
             (dolist (slotd (remove 'sk8::sk8_flags (class-instance-slots myclass) :key #'car))
               (setf (slot-value me (car slotd)) nil))
             ;; now get the object's class out from the list of children of the parent class, changing its inheritance
             (change-class me (mf::find-unowned-class Object))
             (mf::merge-flags-from-parents me))
           ;; object does not share the class. go ahead and zap the class too
           (progn
             (mf::set-class-owner (class-of me) nil)
             ;; ream out the contents of the object, just for good measure
             ;; (leave in sk8_flags, which seems to be critical for lots of things still to come)
             (dolist (slotd (remove 'sk8::sk8_flags (class-instance-slots myclass) :key #'car))
               (setf (slot-value me (car slotd)) nil))
             ;; now get the object's class out from the list of children of the parent class, changing its inheritance
             (change-class me (mf::find-unowned-class Object))
             (mf::merge-flags-from-parents me)
             (mf::obsolete-class-and-subclasses myClass)
             ;; remove class from the lists of subclasses held by its supers
             (dolist (sup (ccl::%class-local-supers myClass))
               (if (typep sup 'class)          ; might be a symbol from earlier forward ref
                 (let ((subs (ccl::%class-subclasses sup)))
                   (if (listp subs)
                     (setf (ccl::%class-subclasses sup) (nremove myClass subs))
                     (setf (ccl::population-data subs) (nremove myClass (ccl::population-data subs))))))) ; initialize-class rectifies this
             (setf (ccl::%class-local-supers myClass) (list (class-of Object)))
             (CCL::initialize-class myClass t)
             (setf (slot-value me 'sk8::sk8_flags) 0)   ;; make this look unnnamed, etc.
             )))))))

;;; This is redefined in SK8ScriptEditor.lisp. Should be defined for the runtime. 

(define-handler ensureHandlersSaved (Project &key (dialog t))
  (declare (ignore dialog me))
  nil)

;;; closeProjectFiles -- closes just the project's files and deletes the swap files, without bothering with any objects
(defun closeProjectFiles (proj)
  (ensureHandlersSaved proj)
  (ps::copy-all-pending-handlers) ;; make sure everything being edited is flushed to disk before we do anything else
  ;; now take care of swap file, which should end up deleted
  (let ((filename (and (sk8:swapfile proj) (physicalname (sk8:swapfile proj)))))
    (when filename
      (removeRes filename)
      (delete-file filename)))
  (setf (slot-value proj 'sk8::swapfile) nil)
  ;; now take care of project file, which should not end up deleted
  (let ((filename (and (sk8:file proj) (physicalname (sk8:file proj)))))
    (when filename
      (removeRes filename))))

;;; PRESERVE -- Preserves project before SK8's image is saved
;;;

(define-system-handler preserve (project)
  (unless (eq me project)
    (unless (or (eq me sk8::sk8) (and (boundp 'sk8::ui) (eq me sk8::ui)))
      ;; create an expendable file to act as project file
         ;; (so we can update it before building a standalone without modifying the real project file)
      (setf (sk8::file me) (ps::create-swapfile-if-needed me)
            (sk8::swapfile me) nil))
    ;; Closing the project file, flushing any cached data to disk
    ;; closeprojectfiles is undefined in runtimes. Do not call it if the only project is sk8.
    (when (> (length (knownChildren Project)) 1)
      (closeprojectfiles me))
    ;; Preserve all objects in it.
    (mapProjectObjects me #'(lambda (obj) 
                              ; *** (store::updatecursor)
                              (unless (inheritsFrom obj project)
                                (preserve obj))))
    ;; Preserve itself.
    (call-next-method)))

;;; (setf filename) -- sets the filename of the project
;;;   This is the keyword used in NEW project, so it is a valid initarg
;;;   for it.
;;; *** The :filename property should probably be changed to :file
;;;     so that it coincides with the real property File of Project.
;;;     Then, this handler would not be needed.
(define-system-handler (setf filename) (filename project)
  (declare (ignore filename))
  ;;; Don't do anything!!!
  )

;;; closeProject -- closes a project without disturbing the project file
;;;
(define-system-handler closeProject (project)
  ;;; Even when closing a project, unsaved versions should be saved!
  (dismissHandlerEditors me :saving t :dialog t)
  (let ((mf::*closing-project* t))
    (sk8::reset-event-system-globals)
    (let ((requiringProjects (requiringProjects me))
          (requiredProject (requiredProject me))
          (project-package (package me)))
      (unwind-protect   ;; make sure project really ends up closed
        (progn      
          (ps::copy-all-pending-handlers)  ;; get things clean before we really start
          
          ;; 1. Before deleting the objects take all the objects of this project off the stage!!!
          (dolist (topLevelActor (contents stage))
            (when (eq me (project topLevelActor))
              (setf (container topLevelActor) nil))) ; *** Would be nice to be able to lock the stage while this happens...
          ;; And get rid of the menubar installed.
          (when (eq (project (sk8::menubar Stage)) me)
            (setf (sk8::menubar Stage) nil))
          
          (event-dispatch)
          
          (sk8::without-events
            (without-interrupts
             ;; 2. Close all projects that require this project:
             (when requiringProjects (mapc #'(lambda (p) (closeproject p)) requiringProjects))
             
             ;; 2.9 Remove all objects in this project that participate in the eventInterests
             ;;     of the System and the Stage.
             
             (clearProjectInterests System me)
             (clearProjectInterests Stage me)
             (clearProjectInterests SK8Clipboard me)

             ;; 3. Remove project's movies and controllers from lists scanned when handling events:
             (dolist (controller sk8dev::*active-movie-controllers*)
               (when (eq (project controller) me)
                 (setf sk8dev::*active-movie-controllers*
                       (delete controller sk8dev::*active-movie-controllers*))))
             
             (dolist (m sk8dev::*active-movies-without-controllers*)
               (when (eq (project m) me)
                 (setf sk8dev::*active-movies-without-controllers*
                       (delete m sk8dev::*active-movies-without-controllers*)))
               )

             (dolist (m sk8dev::*objects-that-want-LowLevelIdle*)
               (when (eq (project m) me)
                 (setf sk8dev::*objects-that-want-LowLevelIdle*
                       (delete m sk8dev::*objects-that-want-LowLevelIdle*)))
               )

             )))
        (unwind-protect
          (progn
            ;; 4. Close project store, resource file and swap file
            (closeProjectFiles me)
            (setf (sk8::file me) nil)
            ;; 5. Delete the project's objects:
            (mapProjectObjects me #'(lambda(obj) (ignore-errors (make-object-gcable obj))))

            (let ((tabl (objecttable me)))
              (clrhash tabl)
              (setf mf::*all-weak-hash-tables* (delete tabl mf::*all-weak-hash-tables*)
                    (objectTable me) nil))
            ;; 6. Delete property entries:
            (maphash #'(lambda (name entry) (declare (ignore entry))
                        (if (eq (symbol-package name) project-package)
                          (remhash name mf::*property-characteristics*)))
                     mf::*property-characteristics*)
            
            ;; 7. Remove link to required project:
            (when requiredProject  ;; there should always be one, at least SK8, but might as well be safe here
              (setf (requiringProjects requiredProject) (delete me (requiringProjects requiredProject))))
            
            ;; 8. Clear entry in SK8Script condition table:
            ;;;no more condition tables (SS::!dispose-project-condition-tables me)
            
            ;; 9. Obsolete project object itself and its package:
            (setf (objectname me :force t) nil)
            (let ((hash (sk8::function-record-table me)))
              (when hash
                (clrhash hash)
                (setf (sk8::function-record-table me) nil)))
            (let ((hash (sk8::handler-record-table me)))
              (when hash
                (clrhash hash)
                (setf (sk8::handler-record-table me) nil)))
            (let ((hash (sk8::variable-record-table me)))
              (when hash
                (clrhash hash)
                (setf (sk8::variable-record-table me) nil)))
            (let ((hash (sk8::constant-record-table me)))
              (when hash
                (clrhash hash)
                (setf (sk8::constant-record-table me) nil)))
            (make-object-gcable me)  ;; more forceful than simply unnaming it
            ;;(mf::compact-sk8) ; Make obsoleted classes and methods GC'able

            ;;;Ensure consistency of our internal port tables...
            (clean-up-port-table *input-port-hash-table*)
            (clean-up-port-table *output-port-hash-table*)
            
            (labels ((forced-delete-package (pkg)
                       (dolist (usingpkg (package-used-by-list pkg))
                         (forced-delete-package usingpkg))
                       (delete-package pkg)))
              (forced-delete-package project-package)))
          ;; force system to recognize that another project should be current
          (sk8::openedProject sk8::system
                              (or (find-if #'(lambda(obj)
                                               (and (neq obj sk8)
                                                    (or (not (boundp 'ui)) (neq obj ui))))
                                           (reverse (knownchildren project)))
                                  (car (last (knownchildren sk8)))
                                  sk8))
          )))
    t))


(define-handler mapProjectObjects (project function)
  (flet ((mapper (key obj)
           (declare (ignore key))
           (funcall function obj)))
    (declare (dynamic-extent #'mapper))
    (let ((tabl (objectTable me)))
      (when tabl
        (maphash #'mapper tabl)))))

(define-sk8-function mapAllObjects nil (function)
  (let (proj)
    (flet ((mapper (key obj)
             (declare (ignore key))
             (funcall function proj obj)))
      (declare (dynamic-extent #'mapper))
      (dolist (p (knownchildren Project))
        (let ((tabl (objectTable p)))
          (when tabl
            (setq proj p)
            (maphash #'mapper tabl)))))))

;;; checkSK8Version -- called at project load time so that project can ensure that it is
;;;                loading on an appropiate version of SK8. This may be shadowed
;;;                or overriden by users. Currently, this checks whether the loaded
;;;                project's major version corresponds to SK8's.
;;;

(define-system-handler checkSK8Version (project)
  (unless (equalp (princ-to-string mf::*macframes-version*)
                  (version me))
    (cerror "Project ~a's major version ~a is incompatible with this copy of SK8 (major version ~s)"
            (objectname me) (version me) (princ-to-string mf::*macframes-version*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

(define-handler SK8::windows (Project)
  (if (not (eq me project))
    (let ((topLevelActors nil)
          container)
      (mapProjectObjects 
       me
       #'(lambda (obj) 
           (when (inheritsFrom obj actor)
             (setf container (container obj))
             (when (or (null container)
                       (eq stage container)
                       ;(inheritsFrom container ui::selectionTLA)
                       )
               (pushnew obj topLevelActors)))))
      topLevelActors)))

(defun map-all-descendants-in-project (proj parent fcn)
  (let ((parent-class (class-of parent)))
    (unless (or (eq parent-class CCL::*built-in-class-class*) ;; built-in class has no sk8 descendants
                (null (class-direct-subclasses parent-class))) ;; don't bother if there are no children
      (flet ((searcher (obj)
               (when (memq parent-class (cdr (CCL::%inited-class-cpl (class-of obj))))
                 (funcall fcn obj))))
        (declare (dynamic-extent #'searcher))
        (mapProjectObjects proj #'searcher)
        nil))))

;;; Used by menubars, menus, & menuItems of Project (among others)
;;;
(defun all-descendants-in-project (proj parent)
  (let ((descendants nil))
    (flet ((gatherer (obj)
             (push obj descendants)))
      (declare (dynamic-extent #'gatherer))
      (map-all-descendants-in-project proj parent #'gatherer)
      descendants)))

(define-handler menus (Project)
  (all-descendants-in-project me Menu))

(define-handler menuBars (Project)
  (all-descendants-in-project me SK8::MenuBar))

(define-handler menuItems (Project)
  (all-descendants-in-project me MenuItem))

;;; objects -- returns a list with all of the objects in the project
;;;
(define-handler objects (Project)
  (if (not (eq me project))
    (let (result)
      (mapprojectObjects me #'(lambda (x) (push x result)))
      result)))


;; helper function for constants, globals and functions handlers
(defun published-sym-producer (prj inherited private predicate)
  (let ((package (package prj))
        (dont-check-package (or inherited private (eq prj SK8::SK8)))
        (result nil))
    (if private
      (with-package-iterator (nextsym package :internal)
        (loop (multiple-value-bind (sym-p sym) (nextsym)
                (if sym-p
                  (when (and
                         (or dont-check-package
                             (eq (symbol-package sym) package))
                         (funcall predicate sym))
                    (push sym result))
                  (return)))))
      (with-package-iterator (nextsym package :external)
        (loop (multiple-value-bind (sym-p sym) (nextsym)
                (if sym-p
                  (when (and
                         (or dont-check-package
                             (eq (symbol-package sym) package))
                         (funcall predicate sym))
                    (push sym result))
                  (return))))))
    result))

;;; constants -- returns a list (of symbols) with all of the constants in the project
;;;
(define-handler constants (Project &key inherited private)
  (published-sym-producer me inherited private
                          #'(lambda(sym)
                              (and (mf::global-sym-p sym)
                                   (boundp sym)
                                   (constant-symbol-p sym)))))

;;; globals -- returns a list (of symbols) of all globals in project
;;;
(define-handler globals (Project &key inherited private)
  (published-sym-producer me inherited private
                          #'(lambda(sym)
                              (and (mf::global-sym-p sym)
                                   (boundp sym)
                                   (not (constant-symbol-p sym))))))

;;; functions -- returns a list of all functions in project
;;;
(define-handler functions (Project &key private)
  (published-sym-producer me nil private
                          #'(lambda(sym)
                              (and (mf::function-sym-p sym)
                                   (fboundp sym)))))

(define-handler projectHandlers (Project)
  (error "Not implemented")
  )

(define-handler removeConstant (project &key name (store t))
  (mf::delete-variable name me :constant t :register store))

(define-handler removeVariable (Project &key name (store t))
  (mf::delete-variable name me :constant nil :register store))

;;; references -- returns a list of references to this object.
;;;  It consists of the following lists:
;;;     1. list of variables
;;;         Each entry in THIS list is of the form: <symbol>
;;;     2. list of constants
;;;         Each entry in THIS list is of the form: <symbol>
;;;     2. list of properties
;;;         Each entry in THIS list is of the form: (<symbol> <object> )
;;;
;;;   projects - a list of projects in which to check for references (default = object's project)
;;;
(defun deepmemq (el seq)
  (if (listp seq)
    (some #'(lambda (x) (deepmemq el x)) seq)
    (eql el seq)))

(define-system-handler sk8:references (sk8:Object
                                          &key (constants t)
                                          (globals t)
                                          (properties t)
                                          (deepSearch t)
                                          (projects (list (project me))))
  (let (vars cons props res
             (thetest (if deepsearch #'deepmemq #'eql)))
    (when properties
      (dolist (p projects)
        (dolist (i (sk8:objects p)) 
          (setf res (sk8:propertiesWithValue i me :test thetest))
          (dolist (j res)
            (push (list j i) props)))))
    (when globals
      (dolist (p projects)
        (dolist (i (sk8:globals p))
          (when (funcall thetest (symbol-value i) me)
            (push i vars)))))
    (when constants
      (dolist (p projects)
        (dolist (i (sk8:constants p))
          (when (funcall thetest (symbol-value i) me)
            (push i cons)))))
    (sk8-multivals vars cons props)))


;;; EVENTS

(define-system-handler activate (project)
  )

(define-system-handler deActivate (project)
  )

(define-system-handler requiredProjects (project)
  (let ((requiredProject (requiredProject me)))
    (when requiredProject
      (cons requiredProject (requiredProjects requiredProject)))))


;;; saveProject -- Saves a project into its store file
;;;   objectsFromOtherProjectsWarning -- if true, warnings will be displayed on the message box
;;;                                describing objects in foreign projects which are in the saved project's containment hierarchy
;;;   objectsFromOtherProjectsAllowed -- if false, then saving will be aborted whenever foreign objects are found
;;;                                in the saved project's containment hierarchy
;;;

(define-handler saveProject (project &key forStandAlone)
  (generateFASLforProject me :outputfile (file me) 
                          :forStandAlone forStandAlone))

(define-handler saveProjectAs (project fileobj)
  (generateFASLforProject me :outputfile fileobj))

;;; compactProject -- compacts a project's store file
;;;  project - the project to compact
;;;  file - optionally a file into which to put a copy of the project in compacted form
;;;  maxVersions - integer, false or true: maximum number of versions of sources to be preserved
;;;               in the compacted file. (default = 1 version--the active version)
;;;               If false, then no sources will be preserved. If true, then all sources will be preserved.
;;;  media - boolean: whether media should be compacted or not (default is true = compact)
;;;
(define-handler compactProjectCode (Project)
  (ps::compactProjectCode me))

;;; utility functions used by compileScriptFile, adapted from internal compiler code

(in-package :ccl)

;;; from fcomp-file
(defun fcomp-read-forms (forms filename env processing-mode)
  (when *compile-verbose* (format t "~&;Compiling ~S..."))
  (let* ((*fasl-source-file* filename)
         (*fcomp-toplevel-forms* nil)
         (*fasl-eof-forms* nil)
         (*loading-file-source-file* filename)
         (read-package nil))
    (declare (special *fasl-eof-forms* *fcomp-toplevel-forms* *fasl-source-file*))
    (push `(setq *loading-file-source-file* ,*loading-file-source-file*)
          *fcomp-toplevel-forms*)
    (dolist (form forms)
      (unless (eq read-package *package*)
        (fcomp-compile-toplevel-forms env))
      (let ((*reading-for-cfasl*
             (and *fcomp-load-time* cfasl-load-time-eval-sym)))
        (declare (special *reading-for-cfasl*))
        (fcomp-form form env processing-mode))
      (loop (unless (setq form *fasl-eof-forms*) (return nil))
            (setq *fasl-eof-forms* nil)
            (fcomp-form-list form env processing-mode))
      (fcomp-compile-toplevel-forms env))))

(defun fcomp-forms (forms filename env)
  (let* ((*package* *package*)
         (*compiling-file* filename)
         (*loading-file-source-file* filename) ;for def's within eval-when (compile)
         (*nx-compile-time-types* *nx-compile-time-types*)
         (*nx-proclaimed-inline* *nx-proclaimed-inline*)
         (*nx-known-declarations* *nx-known-declarations*)
         (*nx-proclaimed-ignore* *nx-proclaimed-ignore*)
         (*nx-speed* *nx-speed*)
         (*nx-space* *nx-space*)
         (*nx-debug* *nx-debug*)
         (*nx-safety* *nx-safety*)
         (*nx-cspeed* *nx-cspeed*)
         (*fcomp-load-time* t)
         (*fcomp-output-list* nil)
         (*fcomp-indentation* 0)
         (*fcomp-last-compile-print* (cons nil (cons nil nil))))
    (fcomp-read-forms forms filename env :not-compile-time)
    (nreverse *fcomp-output-list*)))

;;;We redefine this so we can add the "if-exists" keyword.  We need to do this so we can
;;;write out a fasl into a preexisting file, in our case a file which has all of the resources
;;;of the project already dumped into it.

(let ((*warn-if-redefine-kernel* nil))
  (defun fasl-dump-file (gnames goffsets forms hash filename &key (if-exists :supersede) )
    (let ((opened? nil)
          (finished? nil))
      (unwind-protect
        (with-open-file (*fasdump-stream* filename :direction :output
                                          :element-type 'base-character ; huh
                                          :if-exists if-exists
                                          :if-does-not-exist :create
                                          :external-format #.(ccl::make-keyword (pathname-type *.fasl-pathname*
                                                                                               :case :common)))
          (setq opened? t)
          (multiple-value-bind (*fasdump-writer* *fasdump-writer-arg*) (stream-writer *fasdump-stream*)
            ;(set-mac-file-type filename :fasl)
            (fasl-set-filepos 0)
            (fasl-out-word 0)             ;Will become the ID word
            (fasl-out-word 1)             ;One block in the file
            (fasl-out-long 12)            ;Block starts at file pos 12
            (fasl-out-long 0)             ;Length will go here
            (fasl-dump-block gnames goffsets forms hash)  ;Write the block
            (let ((pos (fasl-filepos)))
              (fasl-set-filepos 8)        ;Back to length longword
              (fasl-out-long (- pos 12))) ;Write length
            (fasl-set-filepos 0)          ;Seem to have won, make us legal
            (fasl-out-word FASL-FILE-ID)
            (setq finished? t)
            filename))
        (when (and opened? (not finished?))
          (delete-file filename))))))


;;; stolen from %compile-file 
;;;Here we add the overwrite keyword so we can dump a fasl into a file which contains project resources...(see above)
(defun %compile-forms (forms source-file output-file load &key (overwrite t))
  (let* ((*fasl-non-style-warnings-signalled-p* nil)
         (*fasl-warnings-signalled-p* nil))
    (when (and overwrite
               (probe-file output-file)
               (neq (mac-file-type output-file) #.(ccl::make-keyword (pathname-type *.fasl-pathname*
                                                                                    :case :common))))
      (unless (y-or-n-dialog (format nil
                                     "Compile destination ~S is a ~A file!  Overwrite it?"
                                     output-file (mac-file-type output-file)))
        (return-from %compile-forms nil)))
    (let* ((*fasl-deferred-warnings* nil) ; !!! WITH-COMPILATION-UNIT ...
           (*fcomp-warnings-header* nil)
           (*compile-file-pathname* (or source-file :sk8script))
           (*compile-file-truename* (or source-file :sk8script))
           (*package* *package*)
           (*readtable* *readtable*)
           (defenv (new-definition-environment))
           (lexenv (new-lexical-environment defenv)))
      (with-compilation-unit ()
        (rplacd (defenv.type defenv) *outstanding-deferred-warnings*)
        (setq forms (fcomp-forms forms source-file lexenv))
        (setf (deferred-warnings.warnings *outstanding-deferred-warnings*) 
              (append *fasl-deferred-warnings* (deferred-warnings.warnings *outstanding-deferred-warnings*))
              (deferred-warnings.defs *outstanding-deferred-warnings*)
              (append (defenv.defined defenv) (deferred-warnings.defs *outstanding-deferred-warnings*)))
        (when *compile-verbose* (fresh-line)))
      (multiple-value-bind (hash gnames goffsets) (fasl-scan forms)
        (fasl-dump-file gnames goffsets forms hash output-file :if-exists (if overwrite :supersede :append))))
    (when load 
      (format t "~%Loading ~a" output-file)
      (load output-file))
    (values (truename (pathname output-file)) 
            *fasl-warnings-signalled-p* 
            *fasl-non-style-warnings-signalled-p*)))

(in-package :SK8Dev)

;;; compilescriptfile -- compiles a SK8Script source file into a fasl (object format) file
;;; see scriptfile-forms-eval-or-compile comments for explanatiopn of most of the arguments
;;; outputfile -- output file path, defaults to the input file with the type replaced with ".fasl"
;;; load  -- t means load the resulting fasl file after compiling it
(define-sk8-function compileScriptFile nil (myfile &key (verbose 1) ((:project proj) SK8) (inproject SK8) (console SS::!*lisp-dev-mode*) (warnings t) outputfile load)
  (cond ((stringp myfile) (setq myfile (sk8-filename-to-mcl-pathname myfile)))
        ((pathnamep myfile) nil)
        ((inheritsfrom myfile file) (setq myfile (ospathname myfile)))
        (t (error "Invalid scriptFile argument")))
  (let ((pathtype (pathname-type myfile)))
    (when (or (null pathtype) (string-equal pathtype #.(pathname-type *.fasl-pathname*)))
      (setf myfile (make-pathname :defaults myfile :type "sk8"))))
  (setf outputfile  (make-pathname :defaults (or outputfile myfile) :type #.(pathname-type *.fasl-pathname*)))
  (let ((*save-definitions* nil)
        (*save-local-symbols* t)   ;;*** kludge alert! this must be t so there will be a lfun-info for us to save stuff in
        (*fasl-save-local-symbols* t)
        (*save-doc-strings* nil)
        (*record-source-file* nil)  
        (CCL::*SUPPRESS-COMPILER-WARNINGS*    T)
        (*load-file-namestring* myfile)
        lispforms)
    (declare (special *load-file-namestring*))
    (setf lispforms (scriptfile-forms-eval-or-compile myfile verbose proj inproject console warnings nil))
    (funcall (if console #'print #'sendToLog)
             (format nil "Begin compilation ~a to ~a ..." myfile outputfile))
    (with-compilation-unit ()
      (ccl::%compile-forms
       lispforms
       myfile
       outputFile 
       load))
    (funcall (if console #'print #'sendToLog)
             (if load
               (format nil "Finished loading ~a ..." outputfile)
               (format nil "Finished compilation ~a to ~a ..." myfile outputfile)))
    ))

;;; loadscriptfile -- loads a SK8Script source file into a project
;;; see scriptfile-forms-eval-or-compile comments for explanatiopn of the arguments
(define-sk8-function loadScriptFile nil (myfile &key (verbose 1) ((:project proj) nil) (inproject SK8) (console SS::!*lisp-dev-mode*) (warnings t))
  (cond ((stringp myfile) (setq myfile (sk8-filename-to-mcl-pathname myfile)))
        ((pathnamep myfile) nil)
        ((inheritsfrom myfile file) (setq myfile (ospathname myfile)))
        (t (error "Invalid scriptFile argument")))
  (scriptfile-forms-eval-or-compile myfile verbose proj inproject console warnings t))

;;; ScriptFaslFile can be:
;;;   a string     (sk8 syntax file name)
;;;   a pathname   
;;;   a File Object
(define-sk8-function loadScriptFASLFile nil (scriptFaslFile &key (verbose nil) &allow-other-keys)
  (cond ((stringp scriptFaslFile) (setq scriptFaslfile (sk8-filename-to-mcl-pathname scriptFaslFile)))
        ((pathnamep scriptFaslFile) nil)
        ((inheritsfrom scriptFaslFile file) (setq scriptFaslFile (ospathname scriptFaslFile)))
        (t (error "Invalid scriptFaslFile argument")))
  (let ((pathtype (pathname-type scriptFaslFile))
        (*load-file-namestring* scriptFaslFile))  ;; used by loadPhysicalFileName
    (declare (special *load-file-namestring*))
    (when (or (null pathtype) (string-equal pathtype "sk8"))
      (setf scriptFaslFile (make-pathname :defaults scriptFaslFile :type #.(pathname-type *.fasl-pathname*))))
    (load scriptFaslFile :verbose verbose)))

(define-sk8-function loadCompiledScriptFile nil (scriptFaslFile &key (verbose nil) &allow-other-keys)
  (loadScriptFASLFile scriptFaslFile :verbose verbose))

;;; loadscripts -- loads list of SK8Script source files into a project
;;;  files - a list of files to load
;;;  project - Either (1) a project into which the contents of all files should be loaded, or
;;;              (2) a string representing the name of a project that should be created and into which all
;;;                 contents of all the files should be loaded.
;;;  verbose - print warnings and progress into the console box or the listener
;;;          1 = prints name of each file being loaded
;;;          2 = 1 + prints each statement being loaded
;;;          false = prints nothing
;;;  console - if true, prints verbose statements into console (the default), else into listener
;;;  inproject - project's superproject (if project is provided)
;;;  skip - skip loading a file, if possible,  if an error occurred
;;;
;;; Returns list of logical names (string) of files actually loaded
;;;
(define-sk8-function loadScripts nil (files &key ((:project proj) nil) (verbose 1) (inproject sk8) (skip t) (console SS::!*lisp-dev-mode*) (warnings t))
  (let (result)
    (dolist (myfile files)
      (if (loadscriptfile myfile :project proj :verbose verbose :inproject inproject :console console :warnings warnings)
        (push myfile result)
        (or skip
            (return))))
    result))

(define-sk8-function compileScripts nil (files &key ((:project proj) nil) (verbose 1) (inproject sk8) (console SS::!*lisp-dev-mode*) (warnings t) (load t))
  (dolist (myfile files)
    (compilescriptfile myfile :project proj :verbose verbose :inproject inproject :console console :warnings warnings :load load)))

(define-sk8-function loadScriptFASLFiles nil (scriptFaslFiles &key (verbose 1) &allow-other-keys)
  (dolist (fi scriptFaslFiles)
    (loadScriptFASLFile fi :verbose verbose)))

(define-sk8-function loadfilephysicalname nil ()
  (declare (special *load-file-namestring* ccl::*loading-file-sourcefile*))
  (let ((mclname (or (and (boundp '*load-file-namestring*) *load-file-namestring*)
                     (and (boundp 'ccl::*loading-file-sourcefile*) ccl::*loading-file-sourcefile*)
                     (car ccl::*loading-files*))))
    (when mclname
      (namestring (translate-logical-pathname mclname)))))



;;__________________________________________________________________
;;A one function fits all version of building 

(define-sk8-function BuildSK8ScriptFiles nil (filenames &key ((:project proj) sk8))
  (unless (listp filenames) (setf filenames (list filenames)))
  (dolist (curFile filenames)
    (cond ((stringp curFile) (setq curFile (sk8-filename-to-mcl-pathname curFile)))
          ((pathnamep curFile) nil)
          ((inheritsfrom curFile file) (setq curFile (ospathname curFile)))
          (t (error "Invalid BuildSK8ScriptFiles file ~s" curFile)))
    (setf curFile (simpleobjectstring curFile))
    (let* ((sk8-source (make-pathname :defaults curFile :type "sk8"))
           (lispSource (make-pathname :defaults curFile :type "lisp"))
           (sk8-source-exists (probe-file sk8-source))
           (lispSource-exists (probe-file lispSource))
           (sk8-binary (make-pathname :defaults (concatenate 'string curFile "-sk8")
                                      :type #.(pathname-type *.fasl-pathname*)))
           (sk8-binary-exists (probe-file sk8-binary))
           (lispBinary (make-pathname :defaults curFile
                                      :type #.(pathname-type *.fasl-pathname*)))
           (lispBinary-exists (probe-file lispBinary)))
      (when (or sk8-source-exists sk8-binary-exists)
        (cl-user::sk8-compile-and-load sk8-source sk8-binary #'(lambda (x &key output-file) (compileScriptFile x :project proj :outputfile output-file))))
      (when (or lispSource-exists lispBinary-exists)
        (cl-user::sk8-compile-and-load lispSource lispBinary #'compile-file)))))


;;;_________________________________________________________________________________________________________________________________
(define-sk8-function searchScripts nil (searchProject searchtext &key (case nil) (count nil)
                                                         (functions t) (handlers t)
                                                         (active nil))
  (multiple-value-bind (h f) 
                       (ps::searchscripts searchProject searchtext :case case :count count :functions functions :handlers handlers :active active)
    (setf h (mapcar #'(lambda (x) (mf::find-handler (if (fourth x)
                                                      (list 'setf (first x))
                                                      (first x))
                                                    (second x))) h))
    (setf f (mapcar #'(lambda (x) (symbol-function (car x))) f))
    (sk8-multivals h f))
  )


#|
	Change History (most recent last):
	2	5/24/93	Hernan	Since the menus are now actors, some care needs
				to be exercised when restoring them. Restore of
				project is modified for this.
	3	6/11/93	kleiman	online documentation
	4	6/14/93	kleiman	more online documentation
	5	6/15/93	kleiman	documentation cosmetics
	6	6/16/93	rod	Removed the check for ui dangling references 
				as they no longer exist as globals (*currenttool
				and *currentEditor*)
	7	6/17/93	kleiman	dispose [project] ui::*current-tool* and ui::*current-editor* are no longer used by UI
	8	6/25/93	kleiman	initialize [project] -> openProject
	9	6/26/93	kleiman	closeProject
	18	8/31/93	kleiman	checkProjectConsistency added
	19	9/22/93	kleiman	new d4 os
	20	9/28/93	rod	moved getfromuser of project to a consolidated set in the UI;dialogs.
	21	9/29/93	kleiman	*macframes-readtable* obsoleted
	22	9/29/93	kleiman	nodes obsoleted
	23	10/1/93	kleiman	obsolete data struct
	24	10/5/93	kleiman	Restore should get the slot value of the container
				slot (not call the container handler to get it).
	25	10/6/93	hernan	restore for project has to be evaluated at the
				end of the build (after the node-container macro
				is defined).
	26	10/8/93	hernan	PrepareForStage has to be called AFTER the restore
				handler is done!!! Changed restore of project to
				do so.
	27	10/11/93	kleiman	mf::*active-tracks* obsoleted
	28	10/18/93	kleiman	object table no longer uses keywords
	29	10/21/93	kleiman	new for new project store
	30	10/21/93	kleiman	(setf filename) dummy
	31	10/21/93	kleiman	silenced swarning
	32	10/25/93	kleiman	New store changes
	33	11/1/93	kleiman	d4 store changes
	34	11/1/93	kleiman	File object used in Project's file property
	35	11/2/93	kleiman	documentation
	36	11/3/93	kleiman	NEW relativizes pathname
	37	11/5/93	kleiman	register- functions integrated to new store
				in backward-compatible way
	38	11/8/93	kleiman	New uses findFileObjectNamed
	39	11/10/93	kleiman	take home
	40	11/12/93	kleiman	d4 madness
	41	11/12/93	hernan	Fixed new to work with new files.
	42	11/15/93	kleiman	new & dispose work with File object
	43	11/16/93	kleiman	new use "sk8:" as default logical directory
	45	11/19/93	kleiman	took home
	46	11/23/93	chip	loadScripts now reports syntax errors and continues on
	47	11/23/93	kleiman	openProject and openProjectFile moved here
	48	11/23/93	kleiman	openproject allows keywords
	49	11/23/93	kleiman	openproject booting patch
	50	11/24/93	hernan	Adding new preserve and restore of project.
	51	11/24/93	kleiman	functions,variables, and constants
	52	11/24/93	kleiman	typos
	52	11/24/93	kleiman	typo
	53	11/24/93	kleiman	variables do boundp check
	54	11/24/93	kleiman	variables makes boundp check
	55	11/24/93	till	fix filename problem in new project
	56	11/24/93	kleiman	removeVariable and removeConstant
	57	11/24/93	kleiman	objectConstant
	58	11/29/93	kleiman	constants takes objectnames keyword, Functions no longer returns macros
	59	11/29/93	kleiman	closeProject reset-event-system-globals BEFORE dispose
	60	12/1/93	kleiman	take home
	61	12/6/93	kleiman	filename checked in dispose (for aborted loads)
	62	12/21/93	kleiman	take out documentation strings
	64	2/14/94	sidney	rename descendants to knowndescendants, children to knownchildren
	65	2/16/94	chip	updated for new translateScriptXXXX APIs
	66	2/17/94	kleiman	saveProject typo (project -> me); openProject
				API (defined on File)
	67	2/18/94	sidney	move the new File stuff to after File is defined!
	68	2/18/94	dy	changed the name of *movies-are-playing-p* to *active-movies-without-controllers* and added support for the new global *active-movie-controllers* list
	69	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	70	2/21/94	sidney	don't barf on fine-does-not-exist error in open-resource-file when :old t
	71	2/21/94	hernan	windows -> sk8::windows.
	72	2/23/94	kleiman	initialize-all-projects is obsoleted
	73	2/28/94	kleiman	getcurrentproject & setcurrentproject obsoleted
				new compactProject API added
	74	3/1/94	kleiman	new loadscripts API
	75	3/2/94	kleiman	loadscripts more verbose keyword + console kywd
	76	3/2/94	kleiman	object-p x -> (typep x 'object) & define-handler
				for Object from t
	77	3/2/94	kleiman	object-p -> typep
	78	3/3/94	kleiman	more code review: obsoleted projectIcon
	79	3/3/94	Hernan	The great handler argument name renaming of 94!
	80	3/7/94	kleiman	opened event
	81	3/7/94	Brian	compactProject now checks for filename before assuming it has a valid one
	82	3/7/94	sidney	make objecttable weak on value and put in *all-weak-hash-tables*
	83	3/8/94	kleiman	create-store open keyword obsoleted
	84	3/8/94	till	Weak hash table fix.
	85	3/8/94	kleiman	OBSOLETE DISPOSE!
	86	3/10/94	kleiman	functions accepts the name keyword
	87	3/10/94	kleiman	loadScriptFile inproject -> project in NEW call
	88	3/10/94	Brian	closeproject did not unname project object &
				(Setf objectname) for project takes force keyword
	89	3/13/94	sidney	try to make objects in closed project gcable
	90	3/15/94	kleiman	copyProject added
	91	3/16/94	sidney	set the target project in the ui when a new project has been opened
	92	3/16/94	kleiman	closeproject now maps project objects just once
	93	3/18/94	kleiman	globals handler checks for Project
	94	3/18/94	kleiman	obsoleted checkConsistency and checkProjectVersion
	95	3/18/94	kleiman	publish -> publishSymbol
	96	3/18/94	kleiman	saveInfo, getInfo, purgeInfo
	97	3/18/94	kleiman	publishSymbol & unpublishSymbol use store
	98	3/18/94	kleiman	typo in publish-project-symbol
	99	3/18/94	kleiman	saveinfo, getinfo & purgeinfo kept in PS
	100	3/21/94	kleiman	loadscriptfile does not gulp first line
	101	3/24/94	kleiman	NEW checks project file name length
	102	3/26/94	rod	Adding systemEvent for new project.  Only
				modified the new project handler and opened
				project handler.
	103	3/26/94	rod	Whoops, should only send systemEvent when
				the project is not "Old
	104	3/31/94	sidney	Don't need to explicitly GC every time we close a project
	105	4/1/94	kleiman	loadScriptFile improvements (Problem 1128650)
	106	4/1/94	kleiman	problem 1128650
	107	4/3/94	sidney	prevent deletion of scripts from store when closing a project
	108	4/12/94	kleiman	code review: copyProject typo; get rid of ancient code
	109	4/12/94	kleiman	code review: obsoleted DOCUMENTS handler
				FUNCTIONS handler returns list of function objects
	110	4/18/94	chip	updated for new scriptErrorPosition api
	111	4/26/94	kleiman	typo in LOADSCRIPT ERROR line; source file closed on error
	112	4/26/94	kleiman	loadfilephysicalname
	113	5/2/94	kleiman	Problem 1159703: loadscriptfile takes SK8 filename
	114	5/13/94	till	Make menus, menuBars, and menuItems
				(of project) more efficient.
	115	5/17/94	sidney	Add swap file field to project object
	116	5/17/94	sidney	provide function to close project files, used when quitting sk8
	117	6/3/94	nil	bind *project-being-loaded* when loading a script
	118	6/6/94	chip	loadScriptFile deals with commands that are continued on multiple lines
	119	6/9/94	sidney	RADAR #1167046: save projects before preserve, use closeprojectfiles for safe deletion of swap file
	120	6/9/94	sidney	RADAR #1167255: change keyword arg in compactproject to match saveproject
	121	6/12/94	sidney	RADAR #1161350: save project as
	122	6/13/94	sidney	bind *project-being-saved*, *project-being-loaded*, *pheap-being-saved* only in correct places
	123	6/15/94	chip	loadScriptFile now uses evaluateScriptCommand, which provides warnings about local vars in handler definitions (radar 1168600)
	124	6/16/94	sidney	1167409: use project name in temp file
	125	6/17/94	chip	!set-up-project-error-tables --> !set-up-project-condition-tables; !dispose-project-error-tables --> !dispose-project-condition-tables
	126	6/17/94	sidney	inform system that a new project is currently open after closing a project
	127	6/18/94	sidney	closeprojectfiles should get both swap and project files and close resource forks
	128	6/23/94	sidney	lazy creation of swapfiles
	129	7/5/94	chip	loadScriptFile now uses with-open-file instead of open, so file is closed when an error is signalled and handled (radar #1170567)
	130	7/13/94	sidney	1170827: error saving a subproject object
	131	7/14/94	Hernan	Getting rid of hideous systemEvent idea and using
				the eventInterests of the System instead.
	132	7/14/94	Hernan	When closing a project we remove from the
				eventListeners of the System and Stage all the
				objects in the project being closed.
	133	7/18/94	sidney	#_openresfile needs a mac-namestring argument, an MCL namestring sometimes breaks
	134	7/19/94	kleiman	1175346 *buildstandalone-dont-save-projects*
	135	7/20/94	kleiman	1175474 constants
	136	7/20/94	kleiman	1175477 variables
	137	7/20/94	kleiman	1175476 functions
	138	8/5/94	sidney	1175552: unwind better out of failed project close or load
	139	8/5/94	Hernan	1178568: removing a project's menubar when the
				project gets closed.
	140	8/18/94	kleiman	1180502
	141	 9/ 1/94	chip    	fixed globals/constants/functions to heed the symbol markers (radar #1183935)
	142	 9/ 2/94	chip    	functions/globals/constants now has special-case for SK8 project -- allows "shared"-package symbols to be considered non-inherited (radar #1183935)
	143	 9/ 2/94	sidney  	pass force keyword arg to setf objectname of object which now uses it
	144	 9/ 6/94	chip    	removed uses of obsolete SS compiler policy
	145	 9/15/94	chip    	loadScriptFile now uses knownChildren to look for project by name
	146	 9/16/94	chip    	made mapProjectObjects & mapAllObjects more efficient -- part of optimization of "propogatable property values" (radar #1186665)
	147	 9/22/94	sidney  	Functions of project is now very different and in another file
	148	 9/22/94	sidney  	Functions to compile sk8script to fasl and load the fasl files
	149	 9/26/94	kend    	Add "with ... end with" context recognition
	150	 9/29/94	chip    	marked Project as not-simple-instantiable
	151	10/ 4/94	sidney  	add warnings keyword to the newer script compile/load functions
	152	10/ 6/94	It      	new of Project now "uses" the 'filename', 'old', & 'fromStore' args (i.e. doesn't pass them on the the next method)
	153	10/ 6/94	chip    	took out explicit marking of Project as not-simple-instantiable (define-handler on new does it now)
	154	10/12/94	sidney  	clean out stuff that could hold on to objects after a project is closed
	155	10/19/94	sidney  	add with-compilation-unit so warning messages appear with file being compiled
	156	10/24/94	chip    	moved 'FUNCTIONS' here (from 01-object)
	157	10/28/94	sidney  	no longer an error if a saved application has no project file if it isn't needed
							remove bogus queue-build-fixup around definition of restore of Project
	158	10/31/94	sidney  	use ccl::open-resource-file instead of obsolete openResources
							fix problem restoring actors in a project that are on the stage in a standalone app
	159	11/ 2/94	sidney  	call saveproject when building a standalone and let it take care of the details
	160	12/ 4/94	sidney  	1169004: use watch cursor when busy loading scripts
	161	12/ 6/94	sidney  	1203842: bulletproof mapProjectObjects so doesn't crash in erroneous case that objectTable is nil
	162	12/ 9/94	Hernan  	Refering to actor things using mf::node-XXX
							instead of the straight accessors.
	163	12/ 9/94	Hernan  	Need to queue-build-fixup restore of project 
							since it uses the actor macros which are not
							defined yet.
	164	12/15/94	jol     	fixed output from compile-sk8-file
	165	12/16/94	dy      	compileScriptFile proj argument -> default to SK8
	166	 1/17/95	till    	trapwrappers for OpenResFile, CloseResFile, CreateResFile
	167	 1/18/95	till    	Substitute RemoveRes for (closeResFile (openResFile)) idiom
	168	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	169	 2/14/95	sidney  	obsolete the publishedsymbols functions
							remove last vestiges of need for project store file during SK8 sessions
	170	 2/16/95	sidney  	don't ever look for SK8 and UI project files when starting up SK8
	171	 2/21/95	rod     	Changing new project to make sure the package
							is completely assembled before creating the
							new file object for the project.
	172	 3/ 1/95	sidney  	1224819: loadscript didn't work with top level "repeat" form.
	173	 3/ 8/95	sidney  	allow animated cursor to animate during script load
	174	 3/13/95	sidney  	progress indicator and allow background processing during load script
	175	 3/14/95	sidney  	use new progress indicator. don't break when this is used to build the progress indicator.
	176	 3/14/95	sidney  	used wrong forms for accessing lines in progress indicator
	177	 3/15/95	rod     	Fixing loadscript to handle comments after 
							continuation characters.
	178	 3/18/95	sidney  	fix loadPhysicalFileName for load functions other than loadScriptFile
	179	 3/20/95	Hernan  	Making pixelMap behave like a real child of Media.
	180	 3/21/95	sidney  	use swapfile so we don't modify project file during buildstandalone
	181	 3/21/95	sidney  	comments got screwed up in last checkin, so checking in again
	182	 3/21/95	sidney  	project objects should not be saved
	183	 3/23/95	sidney  	add some more unwind-protection around closeproject to ensure project gets completely cleared
	184	 3/24/95	sidney  	messages were not being sent to messagbox log. add elapsed time to press box.
	185	 3/27/95	sidney  	check for no ui project so we can buildstandalone without it
							increase speed of closing a large project
	186	 3/27/95	sidney  	1232253:didn't uppercase packagename when checking if new project's package exists, which broke loading subprojects sometimes
	187	 3/27/95	rod     	For loading and compiling scripts, making the 
							progress dialog not appear when the verbose
							keyword is false.
	188	 3/29/95	sidney  	moved some script parsing code into its own function so it could be used by saveastext to fix a bug there
	189	 3/29/95	dy      	insert (finalizeQuickTimeMovies #'restore) in restore of Project
	190	 3/31/95	Hernan  	1234693: restore of Project should not call enteringStage.
							This should only be done when the restore phase is over 
							and we bring up all the windows.
	191	 4/ 3/95	sidney  	clean up open script editors before closing a project
	192	 4/12/95	sidney  	add verbose=0 to show progress bar without other output in loadscript
	193	 4/24/95	Hernan  	1228501: implementing 2 real layers: windows and windoids.
							Since *oldWindow* no longer exists, closeProject does not
							need to clear it.
	194	 4/24/95	rod     	Making closeProject clear the ports from the
							internal hash tables.
	195	 4/24/95	rod     	Making port table cleanup be a lot more 
							efficient and robust.
	196	 4/25/95	rod     	Fixing YesOrNoDialog and MessageToUser calls to 
							not set the heights so it can be computed 
							manually.
	2  	 6/22/95	sidney  	clear clipboard references when closing project
	3  	 7/ 5/95	sidney  	add support for fasl format save/load of projects
	4  	 7/14/95	sidney  	catch attempt to create new project with name of existing package
	5  	 7/18/95	sidney  	whoops, that is a legal thing to do during build of SK8
	6  	 7/18/95	sidney  	whoops, again. screwed up a conditional in the last change.
	7  	 8/ 2/95	Hernan  	When closing a project, closeProject also has to remove
						all the project's objects that appear in 
						*objects-that-want-lowLevelIdle*.
	8  	 8/17/95	sidney  	add a check in addition to boundp because of a change in the build procedure that initializes special vars
						remove bogus test for constant-p in functions function
	9  	 9/20/95	sidney  	read-line had wrong test for eof, causing looping forever in loadscriptfile sometimes
	10 	11/21/95	Hernan  	allows people to use Lisp block comments).`<          ðql    $Ü    _    Á}©    7–    ¿•Â    ä†ä          ~›I    äl    Ú¦Ô    dŒ    =<    ò‚\    
	11 	12/15/95	sidney  	remove use of condition-tables in projects
	12 	 2/ 7/96	sidney  	remove wood
	13 	 2/ 8/96	sidney  	translateScriptCommandOrExpr subsumes older separate functions
	14 	 2/15/96	Brian   	Adding searchScripts function here.
	2  	 4/25/96	Hernan  	Added an argument to saveProject to allow projects to be 
						saved for stand alone building.
	3  	 5/ 7/96	sidney  	removed some dependencies on object system code by moving some definitions to this file
	4  	 5/10/96	Hernan  	published-sym-producer needs to return the result that it
						has so laboriously accumulated.
	5  	 5/23/96	sidney  	use the compile time MCL's file type for fasl, not hardcoded :fasl
	7  	 7/ 7/96	sidney  	changes for native PPC build
	8  	 7/18/96	sidney  	force recompile because of macro change in without-events
	9  	 9/ 3/96	Hernan  	Defined stuff needed for the runtime.
	2  	10/10/96	sidney  	searchscripts was ignoring whether handler was setter or not
	3  	10/10/96	Hernan  	Replacing compactProject with compactProjectCode, a
						function that removes every old version of every handler
						and function saved with the project.
	4  	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	5  	11/26/96	Hernan  	Removing copyProject which does nothing anyway and
						who nobody calls.
	6  	12/13/96	Brian   	Fixing loadscriptfaslfile to not be so verbose...
	7  	 1/ 6/97	Brian Roddy	Adding BuildSk8ScriptFiles.
	8  	 1/30/97	sidney  	package of new project should not Use the lisp default use packages!
	9  	 2/11/97	Brian Roddy	
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
