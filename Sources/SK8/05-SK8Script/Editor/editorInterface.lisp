;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)


(define-sk8-var scriptEditorPrototype :initial-value ScriptEditorWindow)

;*** SHOULD THIS ALSO ALLOW JUST A HANDLER/FUNCTION OBJECT?!
(define-sk8-function findHandlerEditor nil (handlerProject handlerName handlerObject handlerQualifier)
  (block nil
    (flet ((matcher (w)
             (setq w (slot-value w 'gs:my-actor-object))
             (when (SK8::is-a w ScriptEditorWindow)
               (let ((id (inputHandlerID w)))
                 (declare (list id))
                 (when (and (eq (pop id) handlerProject)
                            (eq (pop id) handlerName)
                            (eq (pop id) handlerObject)
                            (eq (car id) handlerQualifier))
                   (return w))))))
      (declare (dynamic-extent #'matcher))
      ;; #####*** THIS CAN/SHOULD(?) MAP OVER ALL FIELDS IN THE *ScriptEditTexts-onStage-&-showingActive* LIST!!!
      (map-windows #'matcher :class 'gs:*SK8-window* :include-windoids t))))


(define-sk8-function warnIfInSuperProject nil (handlerProject handlerName &optional handlerObject handlerQualifier)
  (let* ((lfun (ccl::closure-function (!newgetLfun handlerProject handlerName handlerObject handlerQualifier)))
         (method-already-there? lfun)
         (currently-a-generic? lfun)
         (new-is-a-generic? handlerObject)
         lfunProj)
    (unless lfun
      (setq lfun (ccl::closure-function (!newgetLfun handlerProject handlerName nil nil))
            currently-a-generic? (typep lfun 'standard-generic-function)))
    (when (and lfun (neq (setq lfunProj (!exactLfunInfo lfun)) handlerProject))
      (when (or (and currently-a-generic? (not new-is-a-generic?))        ; new simple function would bash the existing generic
                (and (not currently-a-generic?) new-is-a-generic?)        ; new generic would bash the existing simple function
                (and new-is-a-generic? method-already-there?)             ; new method would bash the existing method
                (and (not new-is-a-generic?) (not currently-a-generic?))) ; new simple function would bash the existing simple function
        (unless
          (with-simple-restart (abort nil)
            (YesOrNoDialog (format nil "Warning: defining the handler ~s in project ~a will override project ~a’s ~
                                        (since ~a is a sub-project of ~a and the handler is public in ~a)!  Go ahead anyway?"
                                   (handlerIDstring handlerName handlerObject handlerQualifier)
                                   (objectName handlerProject) (objectName lfunProj)
                                   (objectName handlerProject) (objectName lfunProj) (objectName lfunProj))
                           :width 420))
          ;; If they didn't click the Yes button, then we can't define the given handler, so return false
          (return-from warnIfInSuperProject nil)))))
  ;; If we get here, it's ok to define the given handler, so return true
  t)

(define-sk8-function editHandler nil (handlerProject &optional handlerName handlerObject handlerQualifier version editorPrototype boundsrect)
  (ps::copy-all-pending-handlers)
  (setq handlerName (!nicefy-handlerName handlerName))
  (when (warnIfInSuperProject handlerProject handlerName handlerObject handlerQualifier)
    ;; Try to find a window that's already showing the given handler:
    (let ((wind (findHandlerEditor handlerProject handlerName handlerObject handlerQualifier))
          (successful nil)
          theEditorField)
      (cond
       ;; The handler's already in some window; select it!
       (wind
        (bringUp wind)
        (when version (setf (versionDisplayed (editor wind)) version))
        wind)
       ;; There's no window; maybe make a new one
       (t
        (let* ((h (if handlerObject
                    (mf::find-local-handler handlerName handlerObject :qualifier handlerQualifier)
                    (fboundp handlerName)))
               (hasSource? (and h (if handlerObject
                                    (or (PS::lfunHasSources (method-function h))
                                        (mf::accessor-method-p h))
                                    (PS::lfunHasSources h)))))
          (when (or (null h)
                    hasSource?
                    (yesOrNoDialog (format nil "The handler ~a is currently defined but it's script is not available.  ~
                                                Would you like to redefine it from scratch?"
                                           (simpleObjectString h :project handlerProject))
                                   :cancel nil))
            (withCursor watchCursor
              (setq wind (recycled (or editorPrototype scriptEditorPrototype ScriptEditorWindow) :in SK8))
              (unwind-protect
                (progn
                  (resetLayout wind)
                  (if boundsrect
                    (setf (boundsrect wind) boundsrect)
                    (bestSize wind))
                  (setf theEditorField (editor wind)
                        (dirty theEditorField) nil
                        (slot-value theEditorField 'versionDisplayed) nil
                        (slot-value theEditorField 'inputHandlerID) nil
                        (inputHandlerID theEditorField) (list* handlerProject handlerName
                                                               (when handlerObject (list handlerObject handlerQualifier))))
                  (bringUp wind)
                  (setq successful t))
                (unless successful
                  (discard wind)))
              wind))))))))

(define-sk8-function editHandlerObject nil (h &key (editorPrototype nil))
  (let ((lfun (if (typep h 'method) (method-function h) h)))
    (multiple-value-bind (hproj hname hobj hqual) (!exactLfunInfo lfun)
      (unless hproj
        (when hobj (setq hproj (if (is-a hobj Project) hobj (project hobj)))))
      (when hproj
        (editHandler hproj hname hobj hqual (PS::lfunVersion lfun) editorPrototype)))))

;; hook error processing to sk8script editor/debuger for sk8script functions

(defun generate-backtrace-list (cur-frm)
  (ignore-errors
   (let ((frm cur-frm)
         stackList
         currentLineNum
         currentID)
     (loop
       (if frm
         (let ((inner-lfun (ccl::cfp-lfun frm ccl::*current-stack-group*)))
           (when inner-lfun
             (setf currentID nil)
             (setf currentLineNum nil)
             (let* ((method (ccl::lfun-name inner-lfun))
                    (closure-lfun (or 
                                   (when (typep method 'ccl::standard-method) ;; sk8 handlers set lfun-name like this
                                     (method-function method))
                                   (when (symbolp method)
                                     (or (get method 'mf::sk8-function)
                                         (find-symbol (symbol-name method) (find-package :sk8)))
                                     )
                                   )))
               (when (and closure-lfun (ps::lfunHasSources inner-lfun))
                 ;; get the lineno variable out of the closure
                 (let* ((lineNumGetter (getf (ccl::%lfun-info inner-lfun) :location-getter)) 
                        (lineNum (when (functionp lineNumGetter) (funcall lineNumGetter)))
                        (id (handlerobjecttohandlerid method)))
                   (unless (fixnump lineNum)
                     (setf lineNum 0))  ;; prevent errors in case got a bogus value
                   (setf currentLineNum lineNum)
                   (setf currentID id)
                   ))
               (when closure-lfun (setf stacklist (nconc stacklist (list (list inner-lfun currentID currentLineNum)))))
               )
             
             ))
         (return nil))
       (setf frm (ccl::parent-frame frm ccl::*current-stack-group*)))
     stackList
     )
   ))

(defun what-is-this-code? (cndition error-pointer)
  (ignore-errors
   (when
     (let ((backtraceList (generate-backtrace-list error-pointer))
           id linenum)
       (when (dolist (i backtraceList)
               (when (and (ps::lfunHasSources (pop i))
                          (setf id (pop i)))
                 (setf linenum (pop i))
                 (return t)))
         (let ((message (with-output-to-string (s) (ccl::report-condition cndition s)))
               (reservedProcess? (or (eql *current-process* CCL::*initial-process*)
                                     (eql *current-process* GS::*SK8-System-process*)
                                     (string-equal (process-name *current-process*) "Listener")
                                     ))
               entry)
           (add-to-break-alist id *current-process* lineNum nil)
           (ignore-errors ;; because we were running into strange problems if an error happened here
            (bringUpHandlerWithRuntimeProblem id
                                              lineNum
                                              :error
                                              message
                                              '(:abort)
                                              backtraceList)
            )
           ;; This is where we stop executing...
           ;;we check that we never arrest the core lisp 
           (if reservedProcess?
             nil ;;(break "~%~%Breaking because the current process is reserved!~%Stepping, Going and Aborting will not work.~%The Expression Watcher will work.~%Type (continue) or (abort) in this listener~%to continue.~%")
             (process-enable-arrest-reason *current-process* :halt))
           ;;Below is where we start again...
           (setf entry (get-from-break-alist id))
           (remove-from-break-alist id)
           (unless reservedProcess?
             (case (fourth entry) 
               (:step #+ignore (pushnew :step lines))
               (:go  
                (resetEditorWithID ID)
                nil)
               (:abort 
                (resetEditorWithID ID)
                (abort))
               (t (error "Illegal debug command ~s" (fourth entry))))
             )
           )
         ))
     t)))


;;___________________________________________________________________________________
;;Making some of the errors print out more nicely...
;;
(let ((*warn-if-redefine-kernel* nil))
  (defmethod ccl::print-object ((method-function method-function) stream)
    (let ((method (ccl::%method-function-method method-function)))
      (if (typep method 'standard-method)
        (format stream "\<~a of ~a\>"
                (ccl::%method-name (ccl::%method-function-method method-function)) 
                (let* ((specializers (ccl::%method-specializers (ccl::%method-function-method method-function)))
                       (class (and (listp specializers) (first specializers))))
                  (cond
                   ((null class)
                    "Unknown")
                   ((consp class) 
                    class)
                   (t
                    (or (class-name class) class)))))
        ;;ORIGNAL CODE
        ;;(ccl::print-method (ccl::%method-function-method method-function)
        ;;                   stream
        ;;                   (ccl::%class-name (class-of method-function)))
        (call-next-method))))
  
  (defmethod ccl::print-object ((clc compiled-lexical-closure) stream)
    (let ((clos-fun (ccl::closure-function clc)))
      (if (and (typep clos-fun 'method-function)
               (typep (ccl::%method-function-method clos-fun) 'standard-method))
        (ccl::print-object clos-fun stream)
        (call-next-method)
        )))
  )

;;___________________________________________________________________________________
;; MCL calls application-error in %error after it tries to signal an MCL condition, if signal returns without handling the error
;; If we first try to signal a corresponding SK8 condition, we can use this to hook the SK8 condition
;; system into the MCL one

(let ((*warn-if-redefine* nil))
  (defmethod ccl::application-error ((a ccl::sk8-application-class) cndition error-pointer)
    (let ((SK8cndition (condition-to-sk8-condition cndition)))
      (SK8-signal SK8cndition))
    (multiple-value-bind (ret err) (what-is-this-code? cndition error-pointer)
      (if (and ret
               ;;These next two things maybe should be handled as
               ;;there own seperate cases.  Listener may not be needed
               ;;initial process maybe should just report the error and ignore it...-BJR
               (not (or (eql *current-process* CCL::*initial-process*)
                        (eql *current-process* GS::*SK8-System-process*)
                        (string-equal (process-name *current-process*) "Listener")
                        )))
        (abort)
        (flet ((errstr (cndt)
                 (let ((*print-circle* *error-print-circle*)
                       (*print-array* nil)
                       (*print-escape* t)
                       (*print-gensym* t)
                       (*print-length* nil)  ; ?
                       (*print-level* nil)   ; ?
                       (*print-lines* nil)
                       (*print-miser-width* nil)
                       (*print-readably* nil)
                       (*print-right-margin* nil)
                       (*signal-printing-errors* nil))
                   (with-output-to-string (s) (ccl::report-condition cndt s)))))
          (if sk8script::!*lisp-dev-mode*
            (progn
              (when err
                (format t "~%While processing error~%~a~% encountered error ~%~a~%" 
                        (errstr cndition)
                        "" ;(with-output-to-string (s) (ccl::report-condition err s) s)
                        ))
              (ccl::break-loop-handle-error cndition error-pointer))
            (progn
              (messageToUser (errstr cndition))
              (abort))))))))
    
#|
	Change History (most recent last):
	1	11/8/93	chip	new file
	2	11/8/93	chip	
	3	11/8/93	chip	
	4	11/8/93	chip	now calls update-script-running-mode with a mode arg (that embodies the tracing mode)
	5	11/10/93	chip	fixed editHandler to set the inputHandlerID instead of setting the versionDisplayed
	6	11/12/93	chip	
	7	11/17/93	chip	added the fcnInEditor? function
	8	11/19/93	chip	editHandler now ensures the editor isn't "dirty" when it first comes up (since it may be "recycling" a used one)
	9	11/23/93	chip	
	10	11/30/93	chip	editHandler now conses a shorter list for the inputHandlerID if it's for a function (not a handler)
	11	12/13/93	chip	editHandler now uses withCursor (instead of with-cursor) for the watch (& only does it when opening a new window); it also resets the basic size/layout of the window (in case it's recycled)
	12	12/13/93	chip	pulled "layout resetting" functionality out of editHandler and into its own handler 'resetLayout'
	13	1/7/94	sidney	big mf/ss merge
	14	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15	3/1/94	rod	Made EditHandler take an option Editor Prototype
	16	3/1/94	rod	Sourceserver glitch
	17	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	18	3/14/94	rod	
	19	3/23/94	chip	fixed editHandlerObject to work with accessor-methods
	20	3/23/94	chip	editHandler now warns if handler exists but no sources; editHandlerObject now behaves just like editHandler
	21	3/29/94	rod	Having new handler editors open relative to 
				existing open editors.
	22	3/29/94	rod	Oh, and make sure that the handler editors are
				opened on a visible monitor.
	23	4/1/94	rod	Now having a global variable to specify the editor prototype.
	24	4/4/94	kleiman	edithandler calls copy-all-pending-handlers
	25	4/12/94	Hernan	Avoiding use of contents when not necessary.
	26	4/28/94	chip	made the "no script available" warning message in editHandler more to Roo's liking; took positioning code out of editHandler (since the editor window's bestSize handler does it!)
	27	6/27/94	rod	Removing cancel from the dialog.
	28	7/15/94	rod	Adding an option to edithandler to specify the
				editor's boundsrect.
	29 	 4/24/95	rod     	Fixing Dialog message for a handler without
							a script.
	30 	 4/25/95	rod     	Fixing YesOrNoDialog and MessageToUser calls to 
							not set the heights so it can be computed 
							manually.
	2  	 8/17/95	sidney  	boundp check is not enough anymore
	3  	 1/19/96	sidney  	removing/rewriting references to old sk8script compiler
	4  	 1/24/96	Hernan  	Commenting out functions that have no callers.
	5  	 2/ 9/96	sidney  	lots of loose ends in defining handlers and functions
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 9/30/96	sidney  	call sk8script debugger if in an error in a sk8script function or handler
	5  	10/10/96	Brian   	Fixing what-is-this-code? to insure the inner-lfun
						is not nil.
	6  	10/11/96	Brian   	Fixing what-is-this-code? to check loop starting
						at the first "frm" rather than the parent of frm.
	7  	10/17/96	sidney  	hook in SK8 condition system to the error processing
	8  	11/12/96	sidney  	add some error checks and ignore-errors to prevent errors that caused crashes during error handling
	9  	11/12/96	Brian   	Changed to use "location-getter" code.   location-getter is defined in skil compiler.lisp in the debug-wrap code.
	10 	11/12/96	Brian   	Adding comment to application-error.
	11 	11/14/96	Brian   	Breaking up what-is-this-code? into piece
						that generates stack backtrace list and one
						which dispatches errors.  Also making it
						handle functions as well as handlers.
	12 	11/14/96	sidney  	don't go into sk8 debugger if error is in sk8 system process
						removed some unused variables and simplified some code
						do different things for internal error depending on whether we are in dev mode
						use sk8 dialog box toodisplay error messages
	13 	11/25/96	Brian   	
	14 	 2/20/97	Brian Roddy	Added new print objects for methods to make 
						them print sk8 methods nicely.  Also added
						flag setting to application error so it will not
						generate an error while printing the error.
	15 	 2/27/97	Hernan  	
	16 	 3/ 7/97	sidney  	define simple error handling earlier and redefine it here if we load this into the build (not in runtime build)
|# ;(do not edit past this line!!)
