;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  4-20-94   5:47 pm
                  UI::EIDINSETRECT)


;;; _____________________________________________________________________________
;;;
;;; _____________________________________________________________________________

(new uisimplewindow :objectname "ErrorInspectorDialog" :project UI
     :properties '((inspectedLogObject :value nil)))   ;;;used to store the error message we are looking at
(setlocation ErrorInspectorDialog 410 205)
(setSize ErrorInspectorDialog 410 290)
(setf (sk8::menubar ErrorInspectorDialog) nil)
(setf (resizer ErrorInspectorDialog) t)
(setf (zoombox ErrorInspectorDialog) t)

(setf (text ErrorInspectorDialog) "Error Inspector")


(new EditText :project ui :container ErrorInspectorDialog 
                    :objectName "ErrorInspectorDialogDisplayBox"
                    :location '(100 100)
                    :size '(260 18))
(setFrameSize ErrorInspectorDialogDisplayBox 0 0)

(tagpart ErrorInspectorDialog ErrorInspectorDialogDisplayBox 'displayer)
(locktext ErrorInspectorDialogDisplayBox)

(new sifinsetrect :objectname "EIDInsetRect" :project ui)
(setf (container EIDInsetRect) ErrorInspectorDialog)
(tagpart ErrorInspectorDialog EIDInsetRect 'InsetRect)
(sendtoback EIDInsetRect)

(new uitextlist :objectname "ErrorInspectorDialogContextList" :project UI)
(setf (container ErrorInspectorDialogContextList) ErrorInspectorDialog)
(setf (text (titlebar ErrorInspectorDialogContextList)) "Context")
(tagpart ErrorInspectorDialog ErrorInspectorDialogContextList 'ContextList)


(define-handler doubleClick ((picker ErrorInspectorDialogContextList))
  (let* ((context (car (selectedItems me)))
         (theHandler (car context)))
    (EditHandlerObjectDialog theHandler)))

(define-handler createTextDisplayItem ((picker ErrorInspectorDialogContextList) contextInfo)
  (let ((name (pop contextInfo))
        (lineNum (first contextInfo)))
    (format nil "~a~@[ line ~a~]"
            (cond
             ((null name) "<Anonymous>")
             ((symbolp name) (name name))
             (t (objectString name)))
            (unless (eql 0 lineNum) lineNum))))

(define-handler selectionCompleted ((picker ErrorInspectorDialogContextList))
  (UpdateGlobalAndLocalLists (sk8::window me))
  (call-next-method))


(new uitextlist :objectname "ErrorInspectorDialogGlobalsList" :project UI)
(setf (container ErrorInspectorDialogGlobalsList) ErrorInspectorDialog)
(setf (text (titlebar ErrorInspectorDialogGlobalsList)) "Globals")
(tagpart ErrorInspectorDialog ErrorInspectorDialogGlobalsList 'globalsList)

(new uitextlistforcorners :objectname "ErrorInspectorDialogLocalsList" :project UI)
(setf (container ErrorInspectorDialogLocalsList) ErrorInspectorDialog)
(setf (text (titlebar ErrorInspectorDialogLocalsList)) "Locals")
(tagpart ErrorInspectorDialog ErrorInspectorDialogLocalsList 'LocalsList)

(define-handler UpdateGlobalAndLocalLists (ErrorInspectorDialog) 
  (let ((cinfo (car (selectedItems (contextList me)))))
    (setf (items (globalsList me)) (SS::!remove-bogus-var-values (third cinfo)))
    (setf (items (LocalsList me)) (SS::!remove-bogus-var-values (fourth cinfo)))))

(define-handler createTextDisplayItem ((picker ErrorInspectorDialogGlobalsList) varAndVal)
  (concatenate 'string (name (car varAndVal)) ":  " ;;(string #\tab)  *** Why does this make a box??!??
               (objectstring (cdr varAndVal))))
(define-handler createTextDisplayItem ((picker ErrorInspectorDialogLocalsList) varAndVal)
  (concatenate 'string (name (car varAndVal)) ":  " ;;(string #\tab)
               (objectstring (cdr varAndVal))))

(defun var-picker-extendedMouseDown (me)
  (let ((sel (selecteditems me)))
    (when sel
      (setq sel (cdr (first sel))) ; only care about single selection; the "value" is the cdr of the selected item.
      (if (is-a sel handler)
        (progn
          (setf (boundsrect HandlerDataRect :physical t) (itemboundsrect me (car (if (inheritsfrom me picker) 
                                                                                   (selection* me)
                                                                                   (selection me))) :physical t))
          (setf (objects HandlerDataRect) (list (object sel)))
          (setf (handler HandlerDataRect) sel)
          (setf (ComponentFrom HandlerDataRect) me)
          (withcursor standardcursor
            (drag HandlerDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
            )
          t)
        (progn
          (setf (boundsrect ObjectDataRect :physical t) (selectedItemsBoundsRect me))
          (setf (object ObjectDataRect) sel)
          (setf (ComponentFrom ObjectDataRect) me)
          (withcursor standardcursor
            (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
            )
          t)
        ))))

(define-handler extendedMouseDown ((picker ErrorInspectorDialogGlobalsList))
  (var-picker-extendedMouseDown me))
(define-handler extendedMouseDown ((picker ErrorInspectorDialogLocalsList))
  (var-picker-extendedMouseDown me))


(new UIButton :objectname "ErrorInspectorDialogDumpButton" :project UI)
(setf (container ErrorInspectorDialogDumpButton) ErrorInspectorDialog)
(setf (text ErrorInspectorDialogDumpButton) "Clipboard")
(tagpart ErrorInspectorDialog ErrorInspectorDialogDumpButton 'DumpButton)

(define-handler click (ErrorInspectorDialogDumpButton)
  (withCursor watchCursor
    (SS::!copy-errormessage-info-to-clip (inspectedLogObject (container me)))))

(define-handler EnteringStage (ErrorInspectorDialog)
  (call-next-method)
  (let* ((errMsg (inspectedLogObject me))
         (errObj (errorObject errMsg)))
    (setf (text me) (concatenate 'string "Inspector for " (objectName (baseParent errObj))))
    (setf (text (displayer me)) (objectString errObj))
    (setf (items (ContextList me)) (SS::!maybe-nicefy-context-list (context errMsg)))
    (UpdateGlobalAndLocalLists me)))

(define-handler resized (ErrorInspectorDialog)  
  (withActorLocked (me)
    (call-next-method)
    (sk8-multival-bind (hsize vsize) (size me)
      (declare (special hsize vsize))
      (let* ((halfX (round hsize 2))
             (y1 (- vsize *windowBottom*))
             (y2 (+ (round (- y1 110) 2) 110)))
        (setBoundsRect (insetrect me) *windowLeft* (+ 10 *windowTop*) (- hsize *WindowRight*) (+ 80 *windowTop*))
        (setBoundsRect (displayer me) 
                       (+ *windowLeft* (if *MacStyleInterface* 1 4)) 
                       (+ (if *MacStyleInterface* 11 14) *windowTop*)
                       (- hsize *WindowRight* (if *MacStyleInterface* 1 2))
                       (+ (if *MacStyleInterface* 79 78) *windowTop*))
        (setBoundsRect (ContextList me) *windowLeft* (+ 90 *windowTop*) (- halfX 3) y1)
        (setBoundsRect (GlobalsList me) (+ halfX 3) (+ 90 *windowTop*) (- hsize *WindowRight*) (- y2 3))
        (setBoundsRect (localsList me) (+ halfX 3) (+ y2 3) (- hsize *WindowRight*) y1)
        (setBoundsRect (DumpButton me) (- hsize 69) (+ 84 *windowTop*) (- hsize 15) (+ 105 *windowTop*))
        ))))

(resized ErrorInspectorDialog)

(setf (alphabeticalDisplay ErrorInspectorDialogContextList) nil)
(setf (alphabeticalDisplay ErrorInspectorDialogGlobalsList) nil)
(setf (alphabeticalDisplay ErrorInspectorDialogLocalsList) nil)
(setf (minimumsize ErrorInspectorDialog) '(280 230))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new InspectableLogObject :objectName "ErrorLogObject" :project UI
     :properties '(errorObject context command))


(define-handler initialize (ErrorLogObject original isNew initArgs)
  (declare (ignore original isNew))
  (call-next-method)
  ;; *** SHOULD ALSO TRY TO INITIALIZE ITS command PROPERTY, BASED ON THE STATE
  ;;      OF THE MESSAGEBOX AND WHETHER THE doMessageBoxCommand IS IN THIS STACKFRAMESBLOCK!
  (setf (errorObject me) (or (initializerArgument initArgs 'errorObject) currentCondition))
  (setf (context me) (or (initializerArgument initArgs 'context)
                         (SS::!makeStackContextList :stackFramesBlock (context me)
                                                    :vars saveErrorContextVars
                                                    :unexaminables includeHiddenErrorContext))))

(define-handler writeLogObject (ErrorLogObject strm)
  (write-string "Æ ERROR: " strm)
  (writeObject (errorObject me) strm nil))

(define-handler inspectLogObject (ErrorLogObject)
  (withCursor watchCursor
    (let* ((oldDialog (find-if-not #'container (knownchildren ErrorInspectorDialog)))
           (ourDialog (or oldDialog (new ErrorInspectorDialog :project UI))))
      (setf (inspectedLogObject ourDialog) me)
      (EnteringStage ourDialog)
      (resized ourdialog)
      (unless oldDialog (resized ourDialog))
      (setf (container ourDialog) stage))))


;--------------------------------------------------------------------------------
#|
	Change History (most recent last):
	2	6/25/93	Brian Roddy	
	3	6/25/93	Brian Roddy	
	8	8/17/93	kleiman	Removed addedittext call from here and replaced it with a (new call so that it was made in the right project.
	9	9/24/93	chip	doubleClick for the picker of mbtext now does an 'inspectMessage' for any MessageObject
	10	9/28/93	chip	fixed doubleClick in messageBox's picker field
	11	10/22/93	kleiman	handler local -> theHandler (in doubleClick handler)
	12	11/29/93	chip	new logging protocol
	13	11/30/93	rod	
	14	12/3/93	rod	Now doubleclick on a handler calls
				EditHandlerObjectDialog, a nice way to bring
				up a handler or the lisp code if option is down.
	15	12/17/93	till	#.'s be gone: doubleClick, createTextDisplayItem, createTextDisplayItem,createTextDisplayItem
	17	2/14/94	sidney	rename children to knownchildren
	18	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	19	2/21/94	hernan	window -> sk8::window.
	20	2/25/94	hernan	Using symbols instead of keywords for options!!!
	21	2/26/94	rod	hilighter->highlighter
	22	2/28/94	hernan	Avoiding disposing things directly.
	23	3/6/94	chip	print... --> write...; "logObject" renaming
	24	3/21/94	rod	Making a regular window.
	25	3/22/94	rod	
	26	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	27	4/20/94	rod	Some ui layout stuff
	28	4/20/94	Brian	
	29	5/22/94	rod	This is the beginnings of adding a Mac Style to the project builder.
	30	6/3/94	rod	
	31	6/17/94	chip	currentError --> currentCondition
	32	7/8/94	rod	Getting rid of UIColorize.
	33 	10/ 5/94	chip    	initialize args now accessed with initializerArgument
	34 	10/31/94	chip    	made extendedMouseDown in the globals/locals lists just drag the value (instead of the pair) (radar #1189497)
	35 	 1/26/95	Hernan  	Fixing format directive in createTextDisplayItem.
	36 	 2/16/95	sidney  	readable argument names for initialize handler
	2  	12/11/95	Brian   	fixing call to ss package
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
