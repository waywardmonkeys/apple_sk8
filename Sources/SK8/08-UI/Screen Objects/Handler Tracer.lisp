;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :uidevelopment)

(SK8-declare-syms :UI :public ; Updated  5-04-94   1:20 pm
                  UI::HANDLERTRACERSHOWCALLERSCB UI::HTHANDLERPILE
                  UI::HANDLERTRACERCLEARBPBUTTON UI::HANDLERTRACER)

;;; _______________________________ 
;;; Tracing functionality. 
;;; _______________________________ 

(defun print-args-nicely (argList)
  (let ((result "")
        curArg)
    (loop 
      (unless argList (return))
      (setf curArg (pop argList))
      (if (keywordp curArg)
        (setf result (concatenate 'string result 
                                  (format nil "with ~a ~a " curArg (pop argList))))
        (setf result (concatenate 'string result (objectString curArg) " ")))
      )
    result))

(defun print-return-vals-nicely (vals)
  (objectString (car vals)))
    
(defun objectString-HANDLER (HandlerOrFun)
  (typecase HandlerorFun
    (method
     (format nil "[~a of ~a]" (name HandlerOrFun) (objectString (object HandlerOrFun))))
    (method-function
     (setf handlerOrFun (ccl::%method-function-method HandlerOrFun))
     (format nil "[~a of ~a]" (name HandlerOrFun) (objectString (object HandlerOrFun))))
    (function
     (format nil "[the function ~a]" (name handlerOrFun)))))

(defparameter startTraceToLogForm
  '(addItem (picker mbtext) 
    (format nil "~aCalling ~a with arguments: ~a." 
            (make-string (max 0 (1- *trace-level*)) :initial-element #\tab)
            (if (symbolp sk8dev::function)
              sk8dev::function
              (objectString-HANDLER sk8dev::function))
            (print-args-nicely sk8dev::args))
    :itemType 'result))

(defparameter endTraceToLogForm
  '(addItem (picker mbtext) 
    (format nil "~a~a returned: ~a." 
            (make-string (max 0 (1- *trace-level*)) :initial-element #\tab)
            (if (symbolp sk8dev::function)
              sk8dev::function
              (objectString-HANDLER sk8dev::function))
            (print-return-vals-nicely sk8dev::vals))
    :itemType 'result))

;;; A method on a sk8 object or a public function (a function in SK8 package?)

(defun valid-sk8-func-thing (thing)
  (typecase thing
    (method-function (mf::object-class-p (mf::get-handler-object-from-lfun thing)))
    (standard-method
     (mf::object-class-p (mf::get-handler-object-from-lfun (method-function thing))))
    (function
     (mf::function-sym-p (name thing)))))
      
(defparameter logCallersForm 
  `(when (valid-sk8-func-thing sk8dev::stackFun)
     (addItem (picker mbtext) 
              (format nil "  -> ~a" (objectString-HANDLER sk8dev::stackFun))
              :itemType 'result)))

(defun traced? (handlerOrFun)
  (member (name handlerOrFun) (trace) :key #'name :test #'string-equal))

(defparameter *log-all-callers?* nil)

(defun trace! (handlerOrFun)
  (let ((name (name handlerOrFun)))
    (if (typep handlerOrFun 'method)
      (eval `(tr (,name
                  :callers (when *log-all-callers?* ,logCallersForm)
                  :before ,startTraceToLogForm
                  :after ,endTraceToLogForm
                  :object ,(object handlerOrFun))))
      (eval `(tr (,name
                  :callers (when *log-all-callers?* ,logCallersForm)
                  :before ,startTraceToLogForm
                  :after ,endTraceToLogForm
                  :object ,(object handlerOrFun)))))))

(defun untrace! (handlerOrFun)
  (eval `(untr ,handlerOrFun)))
  

;;; _______________________________ 
;;; The window. 
;;; _______________________________ 

(new UISimpleWindow :objectname "HandlerTracer" :project ui)

(define-handler clearReferences (HandlerTracer &key ((:objects theobjects)))
  (if theobjects
    (withactorlocked (me)
      (setf (items (picker HTHandlerPile)) (delete-if #'(lambda (x) (memq (object x) theobjects)) 
                                                      (items (picker HTHandlerPile)))))
    (setf (items (picker HTHandlerPile)) nil)))

(define-handler SetUpForProject (HandlerTracer &key ((:project theproject)))
  (setf (items (picker HTHandlerPile)) (delete-if-not #'(lambda (x) (memq (project x) (okprojects theproject))) 
                                                  (items (picker HTHandlerPile)))))

(setf (text HandlerTracer) "Handler Tracer")
(setf (sk8::menubar HandlerTracer) nil)
(setf (resizer HandlerTracer) t)
(setf (zoombox HandlerTracer) t)

(new uitextlist :objectname "HTHandlerPile" :project ui)
(setf (pickerprototype HTHandlerPile) objectpilepicker)
(setf (container HTHandlerPile) HandlerTracer)
(setf (title HTHandlerPile) "Handler Pile")
(tagpart HandlerTracer HTHandlerPile 'Pile)

(define-handler (setf OutputObjects) (theval (picker HTHandlerPile))
  (call-next-method))

(setf (editing (picker HTHandlerPile)) t)

(define-handler draggingMouseEnter ((picker HTHandlerPile) actorDragged)
  (when (or (eq actordragged Handlerdatarect)
            (and (eq actordragged objectdatarect)
                 (is-a (object ObjectDataRect) handler)))
    (call-next-method)))

(define-handler draggingMouseWithin ((picker HTHandlerPile) actorDragged)
  (when (or (eq actordragged Handlerdatarect)
            (and (eq actordragged objectdatarect)
                 (is-a (object ObjectDataRect) handler)))
    (call-next-method)))

(define-handler dropped ((picker HTHandlerPile) actorDragged)
  (when (or (eq actordragged Handlerdatarect)
            (and (eq actordragged objectdatarect)
                 (is-a (object ObjectDataRect) handler)))
    (call-next-method)))

;;;These three are kludges because the handler trace cant get a nil...*****
(define-handler mousedown ((picker HTHandlerPile))
  (when (items me) 
    (call-next-method)))

(define-handler (setf items) (itemList (picker HTHandlerPile))
  (call-next-method)
  (dolist (c itemList)
    (unless (traced? c)
      (trace! c))))

(define-handler keydown ((picker HTHandlerPile) thechar)
  (when (and (items me) (selecteditems me)) 
    (if (eq thechar #\delete)
      (let ((sels (selecteditems me)))
        (dolist (i sels)
          (untrace! i))
        (setf (items me) (delete-if #'(lambda (x) (memq x sels)) (items me))))
      (call-next-method))))

(define-handler keyup ((picker HTHandlerPile) thechar)
  (when (items me) 
    (call-next-method)
  ))

(define-handler doubleclick ((picker HTHandlerPile))
  (when (and (items me) (selecteditems me)) 
    (EditHandlerObjectDialog (car (selecteditems me))))
  )


(define-handler createtextdisplayitem ((picker HTHandlerPile) theItem)
  (objectString-HANDLER theItem)
  )

(define-handler enteringStage (HandlerTracer)
  (setf (keytarget me) (picker (Pile me)))
  (setf (items (picker (pile me))) (trace)))


;; The checkbox. 

(new uiCheckBox :objectname "HandlerTracerShowCallersCB" :project ui)
(setf (text HandlerTracerShowCallersCB) "Show calling chain")
(tagpart HandlerTracer HandlerTracerShowCallersCB 'checkbox)
(setf (container HandlerTracerShowCallersCB) HandlerTracer)
(setf (checked HandlerTracerShowCallersCB) nil)

(define-handler (setf checked) (newValue HandlerTracerShowCallersCB)
  (call-next-method)
  (if newValue
    (setf *log-all-callers?* t)
    (setf *log-all-callers?* nil)))

;; The breakpoint deactivator button...

(new RoundRect :objectName "HandlerTracerClearBPButton" :project ui)
(let ((c HandlerTracerClearBPButton))
  (setf (autoHighlight c) t
        (roundedNess c) '(8 8)
        (boundsRect c) '(7 100 191 120)
        (text c) "Clear All Breakpoints"
        (textSize c) 9
        (textFont c) EspySansFont
        (container c) HandlerTracer))
(tagPart HandlerTracer HandlerTracerClearBPButton 'clearBPButton)

(define-handler click (HandlerTracerClearBPButton)
  (when (autoHighlight me)
    (withCursor WatchCursor
      (dolist (h (selectedItems (pile (container me))))
        (clearAllBreakpoints (sk8dev::handlerObjectToHandlerId h)))
      (update me :forceInactive t))))

(define-handler update (HandlerTracerClearBPButton &key forceInactive)
  (unless forceInactive
    (dolist (h (selectedItems (pile (container me))))
      (when (containsActiveBreakpoints (sk8dev::handlerObjectToHandlerId h))
        (withActorLocked (me)
          (setf (text me) "Clear All Breakpoints"
                (textColor me) Black
                (autoHighlight me) t))
        (return-from update))))
  (withActorLocked (me)
    (setf (text me) "No Active Breakpoints"
          (textColor me) Gray
          (autoHighlight me) nil)))

(define-handler resized (HandlerTracer)
  (let (hsize vsize)
    (declare (special hsize vsize))
    (sk8-multival-setf (hsize vsize) (size me))
    (withActorLocked (me)
      (call-next-method)
      (setBoundsRect (Pile me) 6 (+ *windowTop* 10) (- hsize 10) (- vsize 40))
      (setBoundsRect (clearBPButton me) 15 (- vSize 35) (- hsize 30) (- vSize 15))
      (setBoundsRect (checkbox me) (- hsize 120) 31 (- hsize 20) 45))
    ))

(setboundsrect HandlerTracer 100 25 475 225)

(setf (minimumsize HandlerTracer) '(250 185))

(setf (actor PBMenuHandlerTracer) HandlerTracer)

#|
	Change History (most recent last):
	1	11/29/93	rod	
	2	11/29/93	rod	
	2	11/29/93	rod	
	3	11/30/93	rod	
	4	11/30/93	rod	
	5	12/2/93	rod	
	6	12/3/93	rod	
	7	12/3/93	rod	
	8	12/17/93	till	#.'s be gone: (setf OutputObjects), draggedOverEnter, beingdraggedover, dropped
				mousedown, keydown, keyup, doubleclick
	10	2/12/94	kleiman	renaming
	11	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	13	2/22/94	kleiman	window -> sk8::window.
	14	3/3/94	Hernan	The great handler argument name renaming of 94!
	15	3/4/94	kleiman	addparent avoided where possible
	16	3/8/94	rod	
	17	3/9/94	rod	Doing Project Switching and Reference Clearing.
	18	3/26/94	rod	Minimum size stuff.
	19	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	20	4/22/94	Hernan	Made resized do its thing with the actor locked.
	21	4/28/94	rod	
	22	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	23	7/18/94	rod	Adding menuitem stuff here.
	24 	 8/30/94	rod     	Making resized call-next-method
	25 	 9/ 2/94	sidney  	espysansbold -> espysansboldFont.
	26 	 9/ 6/94	rod     	
	27 	10/ 4/94	rod     	Resized changes.
	28 	 4/10/95	rod     	Making removing items from watcher turn off
							their watching status.
	2  	 5/ 9/96	Hernan  	Fixing it for the new compiler API, also removing some
						options. Making it simple and useful, that is...
	3  	 5/10/96	Hernan  	Making the trace better, added callers functionality.
	4  	 5/10/96	Hernan  	Improving the way the callers print.
	5  	 5/13/96	Brian   	Adding declare syms at the top so it will build.
	6  	 9/ 5/96	Brian   	
	7  	11/26/96	Brian   	Fixing createtextdisplayitem and force it to
						recompute the items when entering the stage.
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
