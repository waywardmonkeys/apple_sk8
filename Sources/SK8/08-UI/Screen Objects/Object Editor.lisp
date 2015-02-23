;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :uidevelopment)

(SK8-declare-syms :UI :public ; Updated  5-27-94   1:16 pm
                  UI::GFUBUTTON UI::OEVALUETEXT UI::OLDBOUNDS UI::WHITEBOX)



(new UISimpleWindow :objectname "ObjectEditor" :project ui
     :properties '(InputObjects))
(setf (minimumsize ObjectEditor) '(170 215))
(setf (sk8::menubar ObjectEditor) t)
(setf (resizer ObjectEditor) t)
(setf (zoombox ObjectEditor) t)

(addInputPort ObjectEditor 'inputObjects :signature Collection)


(defun deepmemq (el seq)
  (if (listp seq)
    (some #'(lambda (x) (deepmemq el x)) seq)
    (eql el seq)))

(define-handler clearReferences (ObjectEditor &key ((:objects theobjects)))
  (if theobjects
    (if (is-a (car theobjects) project)
      (setf (items (historymenu OENameField))
            (delete-if #'(lambda (x) (eq x (car theobjects)))
                       (delete-if #'(lambda (x) (eq (project (if (listp x) (car x) x)) (car theobjects)))   ;;; when lists have project fix ******
                                  (items (historymenu OENameField)))))
      (withActorLocked (me)
        (setf (items (historymenu OENameField)) (delete-if #'(lambda (x) (some #'(lambda (y) (deepmemq y x)) theobjects))
                                                           (items (historymenu OENameField))))
        (when (some #'(lambda (theobject) (memq theobject (outputobjects (nfield me)))) theobjects)
          (setf (inputobjects me) (remove-if #'(lambda (x) (memq x theobjects)) (outputobjects (nfield me)))))))
    (setf (inputobjects me) nil)))

(define-handler SetUpForProject (ObjectEditor &key ((:project theproject)))
  (setf (text me) (concatenate 'string (objectstring theproject :project theproject) " Object Editor"))
  (let ((xx (delete-if-not #'(lambda (x) (memq (project x) (okprojects theproject))) 
                           (outputobjects (nfield me)))))
    (unless (equal xx (outputobjects (nfield me)))
      (setf (inputobjects me) xx))))


(define-handler activate (ObjectEditor)
  (let ((txt (concatenate 'string (objectstring (targetProject UI) :project (targetProject UI)) " Object Editor")))
    (unless (string= txt (text me))
      (setf (text me) txt))
    (call-next-method)
    (updateEditor me)
    (unlock me :force t)
    ))

;;;----------------------------------------------------------------------------------------------
;;;Stuff for updates of this editor...  NOTE THIS IS CALLED BY THE UPDATE CODE IN FILE "Updates and Swatches.lisp"
;;;----------------------------------------------------------------------------------------------

(define-handler updateEditor (objectEditor &key (propertyname nil) (items nil))
  (let* ((objs (outputobjects (picker (psheet me))))
         (objlist (outputobjects (nfield me)))
         (picky (picker (hsheet me)))
         newtext)
    (when (and objlist 
               (or (not items)
                   (intersection items objlist :test #'eq)))
      (if (and (visible (valuetext me)) (eq (inputproperty (picker (valuetext me))) propertyname))
        (withactorlocked (me)
          (setf (inputproperty (picker (valuetext me))) (inputproperty (picker (valuetext me))))
          (selectall (picker (valuetext me)))))
      (cond
       ((or (null propertyname) (eq propertyname 'objectname))
        (setf newtext (if (null objs) 
                        (text (textfield (nfield me))) 
                        (if (every #'objectname objs)
                          (objectstring (if (eql (length objs) 1) 
                                          (car objs) 
                                          objs)
                                        :project (targetproject ui))
                          (querystring (textfield (nfield me))))
                        ))
        (unless (string= (text (textfield (nfield me))) newtext)
          (setf (text (textfield (nfield me))) newtext)
          (dotimes (i (rows picky))
            (setf (aref (imagearray picky) 0 i) 
                  (createTextDisplayItem picky (aref (items picky) 0 i))))
          (forceredraw picky)
          )
        (DoUpdateOnProperty me 'objectname))
       ((or (eq propertyname 'boundsrect) (eq propertyname 'size) (eq propertyname 'location))
        (DoUpdateOnProperty me 'boundsrect)
        (DoUpdateOnProperty me 'size)
        (DoUpdateOnProperty me 'location)
        )
       (t (DoUpdateOnProperty me propertyname))
       )
      (lightforceredraw (picker (psheet me))))))

(define-handler DoUpdateOnProperty (objectEditor propertyname)
  (let* ((psheet (picker (psheet me)))
         (items (items psheet))
         (images (imagearray psheet))
         (rows (rows psheet))
         )
    (dotimes (i rows)
      (when (eq (aref items 0 i) propertyname)
        (setf (aref items 1 i) (CreateSecondItem psheet propertyname))
        (setf (aref images 1 i) (createTextDisplayItem psheet (aref items 1 i)))
        (return)))
    ))

;;;----------------------------------------------------------------------------------------------
;;;The Editor's contents
;;;----------------------------------------------------------------------------------------------

(new StandardInputField :objectname "OENameField" :project ui)
(setf (container OENamefield) ObjectEditor)
(tagpart ObjectEditor OENameField 'NField)
(setf (drawfunction (handle OENameField)) 'ScoredFastTablePicker-draw)
(setf (fillcolor (textfield OENameField)) shadowwhite)
(setf (highlightcolor (textfield OENameField)) uilightcolor)

(define-handler Keydown ((textfield OENameField) thechar)
  (if (eq theChar #\Tab)
    (let ((tla (sk8::window me)))
      (if (visible (psheet tla))
        (setf (keytarget tla) (picker (psheet tla)))
        (setf (keytarget tla) (picker (hsheet tla)))))
    (call-next-method)))

(define-handler update ((historymenu OENameField))
  (call-next-method)
  (dolist (i (menuitems me))
    (unless (memq (project (if (listp (item i))
                             (car (item i))
                             (item i))) (okprojects (targetproject ui)))
      (setf (menu i) nil)))
  )
(define-handler (setf items) (theval (historymenu OENameField))
  (if (eq me (historymenu OENameField))
    (sk8::setValue 'items me theval)
    (progn
      (call-next-method)
      (setf (items (historymenu OENameField)) (slot-value me 'items))
      (setf (slot-value me 'items) nil)
      )))
(define-handler items ((historymenu OENameField))
  (if (eq me (historymenu OENameField))
    (sk8::getValue 'items me)
    (items (historymenu OENameField))))

(new uitextlist :objectname "OEPSheet" :project ui)
(setf (pickerPrototype OEPSheet) propertysheetpicker)
(setf (container OEPSheet) ObjectEditor)
(setf (title OEPSheet) "All Properties")
(setf (editing (picker OEPSheet)) t)
(setf (rowspacing (picker OEPSheet)) 0)
(setframesize (picker OEPSheet) 0 0)
(setf (drawfunction (picker OEPSheet)) 'ScoredFastTablePicker-draw)
(setf (fillcolor (picker OEPSheet)) shadowwhite)
(setf (highlightcolor (picker OEPSheet)) uilightcolor)
(tagpart ObjectEditor OEPSheet 'PSheet)
(define-handler (setf InputObjects) (theval (picker OEPSheet))
  ;;; Here's the property stuff
  (when (visible (container me))
    (call-next-method))
  )


;;; So shadow appears under the splitter.
(define-handler resized (OEPSheet)
  (call-next-method)
  (setboundsrect (insetrect me) 0 0 0 4 :relative t)
  )


(define-handler click ((picker OEPSheet))
  )

(define-handler doubleclick ((picker OEPSheet))
  (when (and (items me) (car (selecteditems me)))
    (if (and (commandkeydown) (= (length (inputobjects me)) 1))
      (editproperty (inputobjects me) (car (selecteditems me)) 
                    (itemboundsrect me (list 1 (cadar (selection me))) :physical t) (sk8::window me))
      (withActorLocked (me)
        (let* ((objed (sk8::window me))
               (valtext (valuetext objed))
               (editor (picker valtext))
               (gfubutton (gfubutton valtext))
               (objs (inputobjects me))
               (pname (car (selecteditems me)))
               (theselection (selection me))
               (rowselected (cadar theselection)))
          (sk8-multival-bind (ll tt rr bb) (itemboundsrect me (list 2 rowselected) :physical t)
            (sk8-multival-bind (lll ttt rrr bbb) (boundsrect valtext :physical t)
              (setf (oldbounds valtext) (list ll tt rr bb))
              (zoomRect ll tt rr bb lll ttt rrr bbb :NumberOfFrames 10
                        :duration 0.001
                        )))
          (setf (inputobjects editor) objs)
          (setf (inputproperty editor) pname)
          (setf (inputobjects gfubutton) objs)
          (setf (inputproperty gfubutton) pname)
          ;;    (itemboundsrect me (list 1 rowselected) :physical t)
          (setf (title valtext) (concatenate 'string  "Values of: " (simpleobjectstring pname)  
                                             ))
          (when (< (height (container me)) 75)
            (setf (proportion (splitter objed)) 
                  (/ (- (+ (top (container me)) 76) (+ *WindowTop* 60))
                     (- (absoluteBottom (splitter objed)) (+ *WindowTop* 60))))
            (resized objed))
          (setf (absoluteTop (Splitter objed)) (+ (top (container me)) 76))
          (bringtofront valtext)
          (setf (keytarget objed) editor)
          (setselection editor 0 -1)
          (updateEditor objed :propertyname pname)
          (show valtext)
          ;;(hide (splitter objed))
          )))))

(define-handler Keydown ((picker OEPSheet) thechar)
  (cond 
   ((or (eq theChar #\Return) (eq theChar #\Enter))
    (doubleclick me))
   ((eq theChar #\Tab)
    (let ((tla (sk8::window me)))
      (if (visible (hsheet tla))
        (setf (keytarget tla) (picker (hsheet tla)))
        (setf (keytarget tla) (textfield (NField tla))))))
   (t (call-next-method))))
  
(new UIValueText :objectname "OEValueText" :project ui)
(setf (container OEValueText) ObjectEditor)
(tagpart ObjectEditor OEValueText 'ValueText)
(hide OEValueText)
(define-handler NewKeyTarget (OEValueText)
  (picker (psheet (sk8::window me))))
(define-handler Keydown ((picker OEValueText) thechar)
  (call-next-method)
  (when (or (eq theChar #\Tab) (eq theChar #\Escape) (eq theChar #\Return) (eq theChar #\Enter))
    (setf (absoluteTop (Splitter (sk8::window me))) (+ *WindowTop* 60))
    ))

(new uitextlistforcorners :objectname "OEHSheet" :project ui)
(setf (pickerPrototype OEHSheet) HandlerSheetpicker)
(hide (titlebar OEHSheet))
(setf (title OEHSheet) "Local Handlers")
(setf (container OEHSheet) ObjectEditor)
(setf (rowspacing (picker OEHSheet)) 0)
(setframesize (picker OEHSheet) 0 0)
(setf (drawfunction (picker OEHSheet)) 'ScoredFastTablePicker-draw)
(setf (fillcolor (picker OEHSheet)) shadowwhite)
(setf (highlightcolor (picker OEHSheet)) uilightcolor)
(tagpart ObjectEditor OEHSheet 'HSheet)

(define-handler (setf title) (theval OEHsheet)
  (let ((mycontainer (container me)))
    (when mycontainer
      (setf (text (splitter mycontainer)) theval))))

(define-handler (setf InputObjects) (theval (picker OEHSheet))
  (when (visible (container me)) 
    (call-next-method)))
(define-handler Keydown ((picker OEHSheet) thechar)
  (if (eq theChar #\Tab)
    (setf (keytarget (sk8::window me)) (textfield (NField (sk8::window me))))
    (call-next-method)))

(new uiMenuBarActor :objectname "OEMenubar" :project ui)
(setf (container OEMenubar) ObjectEditor)
(tagpart ObjectEditor OEMenubar 'Bar)

(new MenuForPropertyPickers :project ui :objectname "OEPropertyMenu" :browsercomponent OEPSheet)
(setf (sk8::menubar OEPropertyMenu) OEMenubar)
(setf (text oePropertymenu) "Properties")
(tagpart ObjectEditor OEPropertyMenu 'Pmenu)

(new menuitem :project ui :objectname "OEPropCP")
(setf (menu OEPropCP) OEPropertyMenu)
(bringtofront OEPropCP)
(sendfarther OEPropCP)
(sendfarther OEPropCP)
(sendfarther OEPropCP)
(define-handler update (OEPropCP)
  (setf (text me) "Edit Property's Attributes")
  (setf (enabled me) (and (inputobjects (browsercomponent (menu me)))
                          (eql (length (inputobjects (browsercomponent (menu me)))) 1))))
(define-handler menuselect (OEPropCP)
  (when (and (items (browsercomponent (menu me))) (lastselection (browsercomponent (menu me))))
    (let* ((theselection (selection (browsercomponent (menu me))))
           (rowselected (cadar theselection)))
      (EditProperty (inputobjects (browsercomponent (menu me)))
                    (car (selecteditems (browsercomponent (menu me))))
                    (itemboundsrect (browsercomponent (menu me)) (list 1 rowselected) :physical t)
                    (sk8::window (browsercomponent (menu me))))
      )))


(new MenuForHandlerPickers :project ui :objectname "OEHandlerMenu" :browsercomponent OEHSheet)
(setf (sk8::menubar OEHandlerMenu) OEMenubar)
(setf (text OEHandlerMenu) "Handlers")
(tagpart ObjectEditor OEHandlerMenu 'Hmenu)

(dolist (i (menus OEMenubar)) (addparent i uimenuactor))

(define-handler (setf InputObjects) (curInput ObjectEditor)
  (if (and (listp curinput) (= (length curinput) 1)) (setf curinput (car curinput)))
  (setf (slot-value me 'InputObjects) curInput)
  (sk8::withLockedCursor animatedClock
      (setf (inputobjects (nfield me)) (if (listp curinput) curinput (list curinput)))
      (unless curinput
        (setf (inputobjects (picker (psheet me))) nil)
        (setf (inputobjects (picker (hsheet me))) nil)
        (setf (outputobjects (picker (psheet me))) nil)
        (setf (outputobjects (picker (hsheet me))) nil)
        (setf (outputhandler (picker (hsheet me))) nil)
        )
      ))


(new uiSplitter :objectname "OESplitter" :project ui
     :properties '((proportion :value 0.67)))
(setf (text OESplitter) "Local Handlers")
(setf (container OESplitter) objectEditor)
(tagpart ObjectEditor OESplitter 'Splitter)
(define-handler drag (OESplitter &key live otherActors dropEvents DraggedOverEvents 
                                  draggingMouseWithinEvents onStage constrainingRect)
  (declare (ignore live otherActors dropEvents DraggedOverEvents 
                   draggingMouseWithinEvents onStage constrainingRect))
  (withActorLocked (me)
    (call-next-method)
    (setf (proportion me) 
          (/ (- (top me) (+ *WindowTop* 60)) (- (absoluteBottom me) (+ *WindowTop* 60))))
    (resized (sk8::window me)))
  )
(define-handler bestsize (OESplitter)
  (let* ((width (width (container me)))
         (v (+ (+ *WindowTop* 60) (* (- (absoluteBottom me) (+ *WindowTop* 60)) (Proportion me)))))
    (setBoundsRect me (- *WindowLeft* (if *MacStyleInterface* 0 1)) 
                   v (- width *WindowLeft* (if *MacStyleInterface* 0 -1)) (+ v 2))
    ))

(define-handler setBoundsRect (OESplitter left top right bottom 
                                             &key physical relative justMoving)
  (let ((objed (container me))
        (abstop (+ *WindowTop* 60))
        (absbot (absoluteBottom me)))
    (unless relative
      (setf top (+ abstop 24 (* 13 (floor (- top abstop 24) 13))))
      (if (< top (+ 36 abstop))
        (progn
          (setf (visible (psheet objed)) nil)
          (setf (keytarget objed) (picker (hsheet objed)))
          (setf top abstop ))
        (unless (visible (psheet objed))
          (setf (visible (psheet objed)) t)
          (setf (inputobjects (picker (hsheet objed))) (outputobjects (nfield objed)))
          ))
      (if (> top (- absbot 36))
        (progn
          (setf (visible (hsheet objed)) nil)
          (setf (keytarget objed) (if (visible (valuetext objed)) (picker (valuetext objed)) (picker (psheet objed))))
          (setf top (- absbot 19) ))
        (unless (visible (hsheet objed))
          (setf (visible (hsheet objed)) t)
          (setf (inputobjects (picker (hsheet objed))) (outputobjects (nfield objed)))
          ))
      )
    (call-next-method me left top right (+ top 19) :physical physical 
                      :relative relative :justmoving justmoving)
    ))


(define-handler setBoundsRect (ObjectEditor left top right bottom 
                                               &key physical relative justMoving)
  (unless (or relative (< (- bottom top) 20))
    (setf bottom (+ top 84 (* 13 (floor (- bottom top 84) 13))))
    )
  (call-next-method me left top right bottom :physical physical 
                    :relative relative :justmoving justmoving)
  )


(define-handler initialize (ObjectEditor original isNew initArgs)
  (declare (ignore isNew initArgs))
  (call-next-method)
  (setf (editing (picker (PSheet me))) t)
  (splitActors (splitter me) 
               (list (psheet me)) (list (hsheet me))
               (list (psheet me) (hsheet me))
               95 500)
  (setf (browsercomponent (pmenu me)) (picker (psheet me)))
  (setf (browsercomponent (hmenu me)) (picker (hsheet me))))


(define-handler resized (ObjectEditor)
  (let (hsize vsize)
    (declare (special hsize vsize))
    (withActorLocked (me)
      (sk8-multival-setf (hsize vsize) (size me))
      (setBoundsRect (Bar me) *BarLeft* *WindowTop* hsize (+ *WindowTop* 20))
      (setBoundsRect (nField me) (+ *WindowLeft* 2) (+ *WindowTop* 25) (- hsize *WindowRight* 2) (+ *WindowTop* 60))
      ;; It seems to be necessary to set these rects twice for the sake
      ;; of the splitter. It is too much work and it slows down the resizing.
      ;; We should figure out a way of letting the splitter know where it 
      ;; should be without doing all this... (Hernan).
      (if (visible (valuetext me))
        (setf (absoluteTop (Splitter me)) (+ (top (psheet me)) 76))
        (setf (absoluteTop (Splitter me)) (+ *WindowTop* 60)))
      (setBoundsRect (PSheet me) *WindowLeft* (+ *WindowTop* 60) (- hsize *WindowRight*) vsize)
      (setBoundsRect (HSheet me) *WindowLeft* 0 (- hsize *WindowRight*) (- vsize 25))
      (setf (absoluteBottom (Splitter me)) (- vSize 20))
      (bestsize (splitter me))
      (setBoundsRect (PSheet me) *WindowLeft* (+ *WindowTop* 60) (- hsize *WindowRight*) (top (splitter me)))
      (setBoundsRect (valuetext me) *WindowLeft* (+ *WindowTop* 60) (- hsize *WindowRight*) (top (splitter me)))
      (setBoundsRect (HSheet me) *WindowLeft* (bottom (splitter me)) (- hsize *WindowRight*) (- vsize *WindowBottom*))
      (call-next-method))
    ))
(setboundsrect ObjectEditor 150 50 372 392)
(resized objecteditor)
(bringtofront (valuetext objecteditor))

#|

(new getfromuserbutton :project ui :objectname "PEGFUButton")
(setf (container PEGFUButton) PropertyEditor)
(tagpart PropertyEditor PEGFUButton 'gfubutton)
(define-handler mousedown (PEGFUButton)
  (call-next-method)
  (when (enabled me)
    (if (state PEChoicesButton)
      (setf (inputproperty PEValueText) (inputproperty PEValueText))
      (keydown (valuetext (container me)) #\escape))))

|#
(define-handler (setf keytarget) (theval ObjectEditor)
  (call-next-method)
  (if (and (visible (valuetext me)) (neq theval (picker (valuetext me))))
    (keydown (picker (valuetext me)) #\escape)))

(define-handler enteringStage (ObjectEditor)
  (setf (text me) (concatenate 'string (objectstring (targetproject ui) :project (targetproject ui)) " Object Editor"))
  (setf (keytarget me) (textfield (nField me))))

(defun editObjects (olist &key (boundsrect '(150 50 372 392)))
  (sk8::withLockedCursor animatedClock
    (unless (listp olist) (setf olist (list olist)))
    (let ((cc (or (car (last (remove-if #'container (knownchildren ObjectEditor))))
                  (new ObjectEditor :project ui))))
      (setf (boundsrect cc) boundsrect)
      (setf (container cc) stage)
      (setf (inputobjects cc) olist)
      cc
      )))

(setf (title OEHSheet) "Local Handlers")

(wirePorts (findport (nfield ObjectEditor) 'outputobjects 'output)
           (findport (picker (HSheet ObjectEditor)) 'inputObjects 'input))

(wirePorts (findport (nfield ObjectEditor) 'outputobjects 'output)
           (findport (picker (PSheet ObjectEditor)) 'inputObjects 'input))

(addInputPort ObjectEditor 'inputObjects :signature Collection)

(new ObjectEditor :ObjectName "InfoWindow" :project ui)

(new ObjectEditor :project ui)
;(new ObjectEditor :project ui :container stage)
;(mapcar #'dispose (knownchildren ObjectEditor))
;(editObjects roundrect)
;(inspect (Splitter (car (knownchildren ObjectEditor))))
;(setf (proportion (Splitter (car (knownchildren ObjectEditor)))) 0.5)
;(setf (container objecteditor) stage)

#|
	Change History (most recent last):
	1	11/15/93	rod	
	14	12/17/93	till	#.'s be gone: Keydown, (setf InputObjects), SelectionCompleted, doubleclick
				Keydown, (setf InputObjects), Keydown
	22	2/12/94	kleiman	renaming
	23	2/14/94	sidney	rename children to knownchildren
  	25	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	27	2/22/94	rod	window -> sk8::window.
	29	2/22/94	rod	Removed GetFromUser Button from here.
				Now on the Property Editor.
	34	3/3/94	Hernan	The great handler argument name renaming of 94!
	35	3/4/94	kleiman	addproperty avoided wherever possible
	38	3/9/94	rod	Doing Project Switching and Reference Clearing.
	45	3/21/94	kleiman	setvalue -> sk8::setValue
	46	3/22/94	rod	kludging it until everything is a project.
	48	3/26/94	rod	Minimum size stuff.
	49	3/26/94	rod	Tuning...
	52	3/30/94	rod	WatchCursor stuff.
	59	4/22/94	Hernan	Make resized lock the actor (just in case).
	62	4/28/94	sidney	Fix initializing of title of OEHSheet contained in a new ObjectEditor
	63	4/28/94	rod	Setting everything right with the titles
	68	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	69	5/26/94	rod	Fixed bug where if an object has location but not
				boundsrect, it gives an error.
	70	5/27/94	rod	
	71	5/31/94	rod	
	72	5/31/94	rod	
	73	6/3/94	rod	
	74	6/10/94	kleiman	Problem 1167276: title becomes"Object Editor"
				(but it should also have a number suffix)
	75	6/13/94	rod	1167953:  Fixing keyboard shortcuts.
	76	6/13/94	rod	
	77	6/15/94	rod	1168599
	78	6/16/94	till	portType keywords begone.
	79	6/17/94	rod	Making sure the history menu does not hold on to
				closed project objects.
	80	6/17/94	rod	Fixing objectstring
	81	6/29/94	rod	
	83	7/13/94	rod	
	84	7/17/94	rod	Speeding up project switching.
	85	7/18/94	rod	
	86	7/18/94	rod	1165082
	87	7/20/94	rod	
	88	8/1/94	rod	
	89 	 8/24/94	rod     	
	90 	 9/ 1/94	Hernan  	
	91 	10/ 7/94	chip    	took port stuff out of ObjectEditor's initialize
	92 	10/11/94	chip    	added ports at toplevel
	93 	10/20/94	rod     	Fixing inital setting of views.
	94 	10/21/94	chip    	Fix of ordering for the build.
	95 	11/ 8/94	rod     	fixing update on property of objectname
	96 	12/ 5/94	rod     	Fixing updates
	97 	 1/25/95	rod     	Making system use deepmemq for checks for
							cleared items.
	98 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	99 	 2/16/95	sidney  	readable argument names for initialize handler
	100	 3/ 2/95	sidney  	deepmemq was in the wrong package
	2  	 8/28/95	Brian   	
	3  	 8/28/95	Brian   	
	4  	 2/14/96	Brian   	fixing macframes references.
	5  	 2/14/96	Brian   	Fixing references to other projects.
	2  	 7/ 8/96	Brian   	fixing zoomrects
	3  	11/22/96	Brian   	
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
