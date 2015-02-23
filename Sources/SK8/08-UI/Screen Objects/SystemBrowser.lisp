;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


(in-package :uidevelopment)

(SK8-declare-syms :UI :public ; Updated  3-29-94   4:40 pm
                  UI::BACKGROUNDRECT UI::SBDOCLISTCOMP UI::SBHANDLISTCOMP UI::SBPROPLISTCOMP )

(new UISimpleWindow :objectname "SystemBrowser" :project ui)
(setf (sk8::menubar SystemBrowser) t)
(setf (resizer SystemBrowser) t)
(setf (zoombox SystemBrowser) t)


(define-handler clearReferences (SystemBrowser &key ((:objects theobjects)))
  (if theobjects
    (when (some #'(lambda (x) (memq x (outputobjects (Field me)))) theobjects)  ;;if the deleted object is in our objects
      (multiple-value-bind (val1 err?)                                          ;;then try to reevaluate the script in our
                           (ignore-errors (evaluate (field me)))                ;;field.  if it works, fine, we're done.
        (when err?                                                              ;;if we get an error then we just clear the browser.
          (setf (text (textfield (field me))) "")
          (setf (outputObjects (field me)) nil))
        )
      )
    (progn
      (setf (text (textfield (field me))) "")
      (setf (outputObjects (field me)) nil)))
  )

(define-handler SetUpForProject (SystemBrowser &key ((:project theproject)))
  )

(define-handler updateEditor (SystemBrowser &key (propertyname nil) (items nil))
  (when (and propertyname items)
    (if (and (visible (valuetext me)) (eq (inputproperty (picker (valuetext me))) propertyname))
      (keydown (picker (valuetext me)) #\Escape))
    (when (and (eq propertyname (outputproperty SBproplistcomp))
               (some #'(lambda (x) (memq x items)) (outputobjects SBproplistcomp)))
      (setf (outputproperty SBproplistcomp) (outputproperty SBproplistcomp))))
  )

(define-handler activate (SystemBrowser)
  (call-next-method)
  (unlock me :force t))

(setf (text SystemBrowser) "System Browser")

(new uimenubaractor :otherparents BrowserMenubar :objectname "SBMenubar" :project ui)
(setf (container SBMenubar) SystemBrowser)
(tagpart SystemBrowser SBMenubar 'Bar)

(new standardqueryfield :objectname "SBqueryfield" :project ui)
(hide (handle SBqueryfield))
(setf (container SBqueryfield) SystemBrowser)
(tagpart SystemBrowser SBqueryfield 'Field)
(define-handler keyDown ((textfield SBqueryfield) thechar)
  (if (eq theChar #\Tab)
    (setf (keyTarget (sk8::window me)) (picker (ObjectLister (sk8::window me))))
    (call-next-method)))
(setf (items (historymenu SBqueryfield)) (list object "the contents of the stage" actor rectangle renderer eventMode visualEffect menu))


(new uitextlist :objectname "SBobjectList" :project ui)
(setf (container SBobjectList) SystemBrowser)
(setf (title SBobjectList) "Objects")
(tagpart SystemBrowser SBobjectList 'ObjectLister)
(define-handler keyDown ((picker SBobjectList) thechar)
  (if (eq theChar #\Tab)
    (setf (keyTarget (sk8::window me)) (picker (propertyLister (sk8::window me))))
    (call-next-method)))

(new uitextlist :objectname "SBpropertyList" :project ui)
(setf (pickerprototype SBpropertyList) propertypicker)
(setf (title SBpropertyList) "All Properties")
(setf (container SBpropertyList) SystemBrowser)
(setf (view (picker SBpropertyList)) 'all)
(tagpart SystemBrowser SBpropertyList 'propertyLister)
(setf (objectname (picker SBpropertyList)) "SBproplistcomp")

(define-handler keyDown ((picker SBpropertyList) thechar)
  (cond 
   ((or (eq theChar #\Return) (eq theChar #\Enter))
    (when (items me)
      (let* ((theselection (selection* me))
             (rowselected (car theselection)))
        (lock (sk8::window me))
        (EditProperty (inputobjects me)
                      (car (selecteditems me))
                      (itemboundsrect me rowselected :physical t)
                      (sk8::window me))
        )))
   ((eq theChar #\Tab)
    (setf (keyTarget (sk8::window me)) (picker (valueEditor (sk8::window me)))))
   (t
    (call-next-method))))
(define-handler doubleclick ((picker SBpropertyList))
  (keydown me #\return))

(new uitextlist :objectname "SBvalueEditor" :project ui)
(setf (pickerprototype SBvalueEditor) valueEditorpicker)
(setf (title SBvalueEditor) "Value")
(setf (container SBvalueEditor) SystemBrowser)
(tagpart SystemBrowser SBvalueEditor 'valueEditor)
(setf (rowlinessize (picker SBvalueEditor)) 1)

(define-handler keyDown ((picker SBvalueEditor) thechar)
  (cond 
   ((or (eq theChar #\Return) (eq theChar #\Enter))
    (doubleclick me))
   ((eq theChar #\Tab)
    (setf (keyTarget (sk8::window me)) (picker (HandlerLister (sk8::window me)))))
   (t
    (call-next-method))))
(define-handler doubleclick ((picker SBvalueEditor))
  (when (and (inputobjects me) (inputproperty me))
    (if (and (commandkeydown) (= (length (inputobjects me)) 1))
      (editproperty (inputobjects me) (inputproperty me) 
                    (itemboundsrect me (list 1 (cadar (selection me))) :physical t) (sk8::window me))
      (withActorLocked (me)
        (let* ((objed (sk8::window me))
               (valtext (valuetext objed))
               (editor (picker valtext))
               (objs (inputobjects me))
               (pname (inputproperty me))
               (gfubutton (gfubutton valtext))
               (theselection (selection me))
               (rowselected (cadar theselection)))
          (sk8-multival-bind (ll tt rr bb) (itemboundsrect me (list 1 rowselected) :physical t)
            (sk8-multival-bind (lll ttt rrr bbb) (boundsrect valtext :physical t)
              (setf (oldbounds valtext) (list ll tt rr bb))
              (zoomRect ll tt rr bb lll ttt rrr bbb :NumberOfFrames 10
                        :duration 0.001)))
          (setf (inputobjects editor) objs)
          (setf (inputproperty editor) pname)
          (setf (inputobjects gfubutton) objs)
          (setf (inputproperty gfubutton) pname)
          ;;    (itemboundsrect me (list 1 rowselected) :physical t)
          (setf (title valtext) "Editing ")
          (bringtofront valtext)
          (setf (keytarget objed) editor)
          (setselection editor 0 -1)
          (updateEditor objed :propertyname pname :items objs)
          (show valtext)
          )))))

(define-handler (setf InputObjects) (theval (picker SBvalueEditor))
  (progn
    (call-next-method)
    (setf (enabled (reliefbutton (sk8::window me))) (and (items me) (typep (aref (items me) 0 0) 'sk8::object)))))
(define-handler (setf inputProperty) (theval (picker SBvalueEditor))
  (progn
    (call-next-method)
    (setf (enabled (reliefbutton (sk8::window me))) (and (items me) (typep (aref (items me) 0 0) 'sk8::object)))))



(new UIValueText :objectname "SBValueText" :project ui)
(setf (container SBValueText) SystemBrowser)
(tagpart SystemBrowser SBValueText 'ValueText)
(hide SBValueText)
(define-handler NewKeyTarget (SBValueText)
  (picker SBvalueEditor))


#|
(define-handler doubleclick ((picker SBvalueEditor))
  (withactorlocked ((sk8::window me))
    (call-next-method)
    (setf (keyTarget (sk8::window me)) (picker (objectlister (sk8::window me))))
    ))
(new getfromuserbutton :objectname "SBgetfromuserbutton" :project ui)
(setf (container SBgetfromuserbutton) SystemBrowser)
(tagpart SystemBrowser SBgetfromuserbutton 'GFUButton)
(define-handler mousedown (SBgetfromuserbutton)
  (call-next-method)
  (setf (inputproperty (picker (valueEditor (container me)))) 
        (inputproperty (picker (valueEditor (container me))))))
|#

(new uibutton :objectname "SBMoveItButton" :project ui)
(setf (container SBMoveItButton) SystemBrowser)
(setf (text SBMoveItButton) "<-")
(tagpart SystemBrowser SBMoveItButton 'ReliefButton)
(define-handler click (SBMoveItButton)
  (when (enabled me)
    (sk8::setoutputstring (picker (valueEditor (sk8::window me))))
    ))
(define-handler (setf enabled) (theval SBMoveItButton)
  (withactorlocked (me)
    (call-next-method)
    (unless theval
      (setf (texthoffset me) 0)
      (setf (textvoffset me) 0)
      )))
(define-handler (Setf Highlight) (value SBMoveItButton)
  (withactorlocked (me)
    (call-next-method)
    (if value
      (progn
        (setf (texthoffset me) 1)
        (setf (textvoffset me) 1)
        )
      (progn
        (setf (texthoffset me) -1)
        (setf (textvoffset me) -1)
        ))))
(setf (highlight SBMoveItButton) nil)

(new uitextlist :objectname "SBHandlerList" :project ui)
(setf (pickerprototype SBHandlerList) Handlerpicker)
(setf (title SBHandlerList) "Local Handlers")
(setf (container SBHandlerList) SystemBrowser)
(setf (view (picker SBHandlerList)) 'local)
(tagpart SystemBrowser SBHandlerList 'HandlerLister)
(setf (objectname (picker SBHandlerList)) "SBhandlistcomp")
(define-handler keyDown ((picker SBHandlerList) thechar)
  (if (eq theChar #\Tab)
    (setf (keyTarget (sk8::window me)) (textfield (Field (sk8::window me))))
    (call-next-method)))

(new uitextlistforcorners :objectname "SBHandlerViewer" :project ui)
(setf (pickerprototype SBHandlerViewer) HandlerViewerText)
(setf (title SBHandlerViewer) "Script")
(setf (container SBHandlerViewer) SystemBrowser)
(tagpart SystemBrowser SBHandlerViewer 'HandlerViewer)
(setf (objectname (picker SBHandlerViewer)) "SBdoclistcomp")

(setup sbmenubar)
(tagpart SystemBrowser (first (menus SBMenuBar)) 'Omenu)
(tagpart SystemBrowser (second (menus SBMenuBar)) 'pmenu)
(tagpart SystemBrowser (third (menus SBMenuBar)) 'vmenu)
(tagpart SystemBrowser (fourth (menus SBMenuBar)) 'hmenu)
(dolist (i (menus sbmenubar)) (addparent i uimenuactor))

(wireports (findport (field SystemBrowser) 'outputobjects 'output)
           (findport (picker (ObjectLister SystemBrowser)) 'inputObjects 'input))
(wireports (findport (picker (ObjectLister SystemBrowser)) 'outputobjects 'output)
           (findport (picker (propertyLister SystemBrowser)) 'inputObjects 'input))

(wireports (findport (picker (propertyLister SystemBrowser)) 'outputobjects 'output)
           (findport (picker (valueEditor SystemBrowser)) 'inputObjects 'input))
(wireports (findport (picker (propertyLister SystemBrowser)) 'outputproperty 'output)
           (findport (picker (valueEditor SystemBrowser)) 'inputProperty 'input))
(wireports (findport (picker (valueEditor SystemBrowser)) 'outputstring 'output)
           (findport (field SystemBrowser) 'inputstring 'input))

(wireports (findport (picker (ObjectLister SystemBrowser)) 'outputobjects 'output)
           (findport (picker (HandlerLister SystemBrowser)) 'inputObjects 'input))
(wireports (findport (picker (HandlerLister SystemBrowser)) 'outputhandler 'output)
           (findport (picker (HandlerViewer SystemBrowser)) 'inputhandler 'input))
  

(define-handler initialize (SystemBrowser original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (setf (BrowserComponent (omenu me)) (picker (ObjectLister me)))
  (setf (BrowserComponent (pmenu me)) (picker (propertyLister me)))
  (setf (BrowserComponent (vmenu me)) (picker (valueEditor me)))
  (setf (BrowserComponent (hmenu me)) (picker (HandlerLister me))))

(define-handler enteringStage (systembrowser)
  (setf (keytarget me) (textfield (field me))))

(define-handler resized (SystemBrowser)
  (let (hsize vsize)
    (declare (special hsize vsize))
    (sk8-multival-setf (hsize vsize) (size me))
    (let* ((onethird (round (- hsize *WindowLeft* *WindowRight*) 3))
           (twothirds (* 2 onethird))
           (v1 (+ 45 *WindowTop*))
           (v2 (+ v1 (round (- vsize (+ 55 *WindowTop*)) 2)))
           (v3 (+ 5 v2))
           (v4 (- vsize *WindowBottom*))
           (onehalf (round hsize 2))
           )
      (setBoundsRect (Bar me) *BarLeft* *WindowTop* hsize (+ 20 *WindowTop*))
      (setBoundsRect (Field me) 10 (+ 23 *WindowTop*) (- hsize 10) (+ 44 *WindowTop*))
      (setBoundsRect (ObjectLister me) *WindowLeft* v1 (+ onethird 7) v2)
      (setBoundsRect (propertyLister me) (+ onethird 13) v1 (+ twothirds 7) v2)
      (setBoundsRect (valueEditor me) (+ twothirds 13) v1 (- hsize *WindowRight*) v2)
      (setBoundsRect (valueText me) (+ twothirds 13) v1 (- hsize *WindowRight*) v2)
      (setBoundsRect (ReliefButton me) (- hsize 40) (+ v1 2) (- hsize *WindowRight* 4) (+ v1 15))
      (setBoundsRect (HandlerLister me) *WindowLeft* v3 (- onehalf 5) v4)
      (setBoundsRect (HandlerViewer me) (+ onehalf 5) v3 (- hsize *WindowRight*) v4)
      ))
  (call-next-method)
  )
;;;(resized systembrowser)
;;(bringtofront SBgetfromuserbutton)

(define-handler (setf keytarget) (theval SystemBrowser)
  (call-next-method)
  (if (and (visible (valuetext me)) (neq theval (picker (valuetext me))))
    (keydown (picker (valuetext me)) #\escape)))

(setboundsrect SystemBrowser 100 100 500 360)
(setf (minimumsize systembrowser) '(375 220))
;;(setf (container systembrowser) stage)
#|
	Change History (most recent last):
	1	9/20/93	rod	
	3	9/24/93	kleiman	commented out putting it on stage
	8	10/18/93	rod	Fixed my lunacy.  I put prototypes in the browser rather than the "SB" components
	27	12/2/93	rod	Clears browser if an object has been disposed
	32	12/17/93	till	#.'s be gone: keyDown, keyDown, doubleclick, keyDown, (setf InputObjects),  
				SelectionCompleted, keyDown, doubleclick, keyDown,
	35	1/14/94	hernan	
	36	2/18/94	kleiman	
	37	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	39	2/22/94	rod	window -> sk8::window.
	40	2/25/94	hernan	Using symbols instead of keywords for options!!!
	41	3/3/94	Hernan	The great handler argument name renaming of 94!
	43	3/8/94	rod	addparent avoided where possible
	44	3/9/94	rod	Doing Project Switching and Reference Clearing.
	45	3/16/94	rod	
	46	3/26/94	rod	Minimum size stuff.
	47	3/27/94	Brian	Some interface changes.
	48	3/27/94	Brian	
	49	3/27/94	Brian	
	50	3/29/94	rod	
	51	3/29/94	rod	
	54	4/18/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
				534/12/94rod
	55	4/18/94	rod	Fixing query vs. name field
	56	4/20/94	rod	
	57	5/6/94	rod	
	58	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	59	5/31/94	rod	
	60	5/31/94	sidney	It helps not to have extra lines of code left lying around the source file
	61	6/3/94	rod	
	62	6/13/94	rod	1167953: Fixing keyboard shortcuts.
	63	6/15/94	rod	Fixing value list to have row lines.
	64	6/16/94	till	portType keywords begone.
	65	6/23/94	rod	Getting rid of keyword property values.
	66	6/29/94	rod	
	67	7/6/94	rod	Getting rid of UIColorize.
	68 	 9/ 6/94	rod     	
	69 	10/ 7/94	chip    	simplified SystemBrowser's initialize as needed (ports handled automatically now)
	70 	10/20/94	rod     	Fixing inital setting of views.
	71 	10/20/94	rod     	Fixing inital title text for handler and property list
	72 	11/ 2/94	rod     	changing the title text again.
	73 	 2/16/95	sidney  	readable argument names for initialize handler
	74 	 4/ 5/95	rod     	Making clear references be smarter.
	2  	12/ 8/95	Brian   	
	3  	 1/ 9/96	Hernan  	Removing calls to breakpointsVisible.
	2  	 7/ 8/96	Brian   	fixing zoomrects
	3  	10/11/96	Brian Roddy	Removing cross author mixin.
	4  	11/22/96	Brian   	
	5  	11/25/96	Brian   	
	6  	 2/25/97	Brian Roddy	Cleaning up comments.
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
