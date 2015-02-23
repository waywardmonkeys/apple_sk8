;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  8-28-95   3:46 pm
                  SK8::CREATENEWOBJECTFROMSTORE SK8::CREATENEWPROJECTFORLOAD SK8::CREATENEWVARIABLEFROMSTORE
                  SK8::MAKEINSTANCEFROMSTORE SK8::SETVALUEFROMSTORE)

;;; This file contains store stuff that is required by the runtime.

(define-sk8-var *load-file-namestring* :initial-value nil)

;;This function does all the checking for making a new project.
;;it checks for superprojects, if the project already exists, etc.

(define-sk8-function CreateNewProjectForLoad nil (projectname projectfilename swapfile requiringprojects requiredproject)
  (declare (ignore requiringprojects))
  ;; Open required project if it is not already open:
  (when (and requiredproject (not (boundp requiredproject)))
    (message-dialog (format nil "Project ~a requires project ~a to be loaded first. Please help me find ~-1*~a's project file."
                            projectname requiredproject))
    (when (or (eq :cancel (catch :cancel
                            ;; *** Should call SK8's open project dialog instead of MCL's:
                            (setq requiredproject (openProjectFile
                                                   (choose-file-dialog :mac-file-type :wood :button-string "Select")))))
              (null requiredproject) ;; bail out if we still don't have this set to something
              )
      (return-from CreateNewProjectForLoad nil)))
  
  (when (symbolp requiredproject) (setf requiredproject (symbol-value requiredproject)))
  ;; MUST RETURN PROJECT OBJECT!!!
  (let* ((nameAsSym (find-symbol (string-upcase projectname) (sk8::package requiredproject)))
         (proj (and nameAsSym (boundp nameAsSym) (symbol-value nameAsSym))))  ;; was (when nameAsSym (mf::find-object nameAsSym requiredproject))
    (cond ((null proj)
           ;;*** some older code checked for (find-package (string-upcase name)) Do we want to warn if the package exists already?
           (sk8::new sk8::project :fromStore t :project requiredproject
                     :objectname projectname :old t :filename projectfilename :swapfile swapfile))
          ((sk8::inheritsfrom proj sk8::project)
           (sk8::sk8-error sk8::generalProgrammaticError :strings '("Project " " is already loaded") :objects (list proj))
           proj  ;; I don't think that sk8-error continues to here, but just in case, return the proj if it does
           )
          (t ;; object already exists and it isn't a project
           (sk8::sk8-error sk8::generalProgrammaticError
                           :strings '("Cannot load the project " " because object " " with same name already exists")
                           :objects (list projectname proj))
           nil))))

;;; It isn't nice that we are using these global variables. They have to be cleared before loading a project. Now that we
;;; are using the same methods for saving/loading as text as we do in the binary store we can no longer count on being
;;; able to bind them inside a let in the openproject function. Here is a temporary hack, a function that can be called at the
;;; beginning and end of the script to clear these. Grumble.

(define-sk8-function sk8::clearSystemStateForProjectLoad nil ()
  (declare (special sk8::*movies-to-initialize*
                    sk8::*actors-contained-in-other-projects*
                    sk8::*stage-menubar*
                    sk8::*toplevel-actors*))
  (setf sk8::*movies-to-initialize* nil
        sk8::*actors-contained-in-other-projects* nil
        sk8::*stage-menubar* nil
        sk8::*toplevel-actors* nil))

;;;__________________________________________________________________________________________________________________
;;;__________________________________________________________________________________________________________________
;;;The functions to do all the work of making objects.  Note these are all wrapped to look for errors...
;;;__________________________________________________________________________________________________________________
;;;__________________________________________________________________________________________________________________

;;;This macro expands to check for errors and if errors exist, the errorcode argument is executed.
(defmacro withStoreErrorWrap (errorCode &body body)
  `(multiple-value-bind (val1 err?) 
                        (ignore-errors 
                         (progn
                           ,@body)
                         )
     (when err?
       (let ((outstring (make-string-output-stream)))
         (writeobject err? outstring nil)
         ,errorCode
         (sendtolog (concatenate 'string "The error was: " 
                                 (get-output-stream-string outstring)))))
     val1
     ))

;;This wraps the creation of constants and variables in error checking code that 
;;makes sure the symbol isn't bound, etc.
;;N.B.  this is useful for the bug in the object system which sometimes marks
;; object names as global constants of the project.
(define-sk8-function CreateNewVariableFromStore nil (thename value const? proj)
  (when (and (symbolp thename) (boundp thename) const?)
    (sendtolog (format nil "Encountered problem while loading.  Cannot create the constant ~a with the value ~a, because that constant already exists.  The constant will remain unchanged."
                       thename value))
    (return-from CreateNewVariableFromStore nil))
  
  (withStoreErrorWrap 
    (sendtolog (format nil "Encountered surprise problem while trying to create the ~a ~a with the value ~a.  Ignoring problem and continuing."
                       (if const? "constant" "variable") thename value))
    (if const?
      (sk8dev::define-sk8-constant* thename value :project proj :register t)
      (sk8dev::define-sk8-var* thename :initial-value value :project proj :register t))
    )
  )

;;Simple error checking for property setting.
(define-handler SetValueFromStore (object prop value versionInfo)
  (declare (ignore versionInfo))
  
  (unless (property me prop)
    (sendtolog (format nil "Encountered problem while loading.  Cannot set the ~a of ~a to ~a, because that property does not exist.  This setting will be ignored and thus will be lost."
                       prop me value))
    (return-from SetValueFromStore nil))
  
  (withStoreErrorWrap 
    (sendtolog (format nil "Encountered surprise problem while trying to set the ~a of ~a to ~a.  Ignoring problem and continuing."
                       prop me value))
    (setvalue prop me value))
  
  )

;;This makes sure all news have project arguments.  It checks if the specified
;;objectname is still valid and wraps object creation with the appropriate error checking.

;; (define-handler CreateNewObjectFromStore (object &rest initArgs)

(defmethod CreateNewObjectFromStore ((me t) &rest initArgs)
  (let ((objname (getf initargs :objectname)) res)
    (unless (getf initargs :project) 
      (push (targetproject ui) initargs)
      (push :project initargs)
      )
    (when objname
      (loop
        (setf res (withStoreErrorWrap 
                    (let ((newname  (getanswerfromuser (format nil "The specified objectname \"~a\" is not usable.  Please specify another name.  Note that you may need to fix handlers that refer to this object."
                                                               objname)
                                                       :width 350
                                                       :height 100
                                                       :system t)))
                      (setf (getf initargs :objectname) newname))
                    (mf::ensure-usable-objectname objname (package (getf initargs :project)))))
        (when res (return))
        (setf objname (getf initargs :objectname)))
      )
    (withStoreErrorWrap 
      (sendtolog (format nil "Encountered surprise problem while trying to create a new ~a with objectname ~a.  This object will not be created.  Attempting to continue."
                         me objname))
      (apply #'new me initargs))))

;;----------------------------------------------------------------------------------
;;These too are a last resort of internal instances of classes we don't know about.  it
;;simply tries a make-instance of the class...

(define-sk8-function MakeInstanceFromStore nil (cl &rest initArgs)
  (withStoreErrorWrap 
    (sendtolog (format nil "Encountered surprise problem while trying to make an instance of the internal type ~a.  This instance will not be created.  Attempting to continue."
                       cl))
    (apply #'ccl::make-instance cl initargs)))

;; what about update-function and help-spec?
(define-handler sk8::makeSK8MenuItem (menuItem ownermenu itemnumber checkedp cmdkey txt enbld style color-list)
  (let ((OSPointer (make-instance 'gs:*SK8-menu-item*)))
    (setf (OSPointer me) OSPointer)
    (setf (mf::my-frame OSPointer) me)
    (when ownermenu
      (let ((myowner (macMenu ownermenu)))
        (setf (slot-value OSPointer 'ccl::owner) (macMenu ownermenu)
              (nth itemnumber (slot-value myowner 'ccl::item-list)) OSPointer)))
    ;; copy some properties...
    (setf (checkMark me) checkedp)
    (setf (commandKey me) cmdkey)
    (setf (text me) txt)
    (setf (enabled me) enbld)
    (set-menu-item-style OSPointer style)
    (setf (slot-value OSPointer 'ccl::color-list) color-list)
    OSPointer))

;; what about ccl::color-list, ccl::update-function, help-spec, ccl::style, menu-id, menu-handle ??
;; they don't seem to be used in the internal menu object
(define-handler sk8::makeSK8Menu (Menu ownermenu itemnumber theText numitems)
  (let ((theMenu (make-instance 'gs:*SK8-menu*)))
    (setf (slot-value theMenu 'ccl::item-list) (make-list numitems)) ;; to be filled in as menu items are created
    (setf (macMenu me) theMenu)
    (setf (mf::my-frame theMenu) me)
    (when ownermenu
      (let ((myowner (macMenu ownermenu)))
        (setf (slot-value theMenu 'ccl::owner) (macMenu ownermenu)
              (nth itemnumber (slot-value myowner 'ccl::item-list)) theMenu)))
    (if (string= theText "")
      (set-menu-title (macMenu me) "Untitled")
      (set-menu-title (macMenu me) theText))
    theMenu))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
