;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :uidevelopment)
#|
UPDATES!!!

Should have an update thing for each button to update the say actors, etc.

Should update targetProject when switching projects...

|#
(SK8-declare-syms :UI :public ; Updated 12-07-95  12:10 pm
                  UI::GETPILE UI::NEWACTION UI::POBOUTCOLOR UI::POBUSERDEFINED UI::POOBJMENU UI::POOBJPICKER
                  UI::POOBJS UI::POOTHERS UI::PROTOTYPES UI::STOREPILE)


;;(define-handler uiEdit (function)
;;  (edithandlerobject me))

(new browsercomponent :objectname "UIObjectDragMixin" :project ui)

(define-handler doubleclick (UIObjectDragMixin)
  (when (selecteditems me)
    (uiedit (mapcar #'value (selecteditems me)))))


;;MCL3.0
(define-handler extendedmousedown (UIObjectDragMixin)
  (call-next-method)
  (let* (theguy
         (sels (selecteditems me))
         (sel (car sels)))
    (if (is-a sel hierarchicalpickeritem) (setf sel (value sel)))
    (when (eql (length sels) 1)
      (sk8-multival-bind (hh vv) (mouseloc stage)
        (setf theguy (actorathvcoordinates hh vv))
        (cond
         ((and (is-a sel renderer) (oktoselect? theguy) (neq theguy stage) (neq (project theguy) ui))
          (cond 
           ((optionkeydown)
            (undoableset 'framecolor theguy sel))
           ((commandkeydown)
            (undoableset 'textcolor theguy sel))
           (t
            (undoableset 'fillcolor theguy sel))))
         ((and (is-a sel actor) 
               (oktoselect? theguy)
               (or (eq theguy stage) (eq (project theguy) (project sel)))
               (or (eq theguy stage)
                   (and (neq theguy sel)
                        (not (containedby theguy sel)))))
          (when (if (eq (container sel) theguy)
                  (yesornodialog (concatenate 'string "Would you like to set the location of " 
                                              (objectstring sel :project (targetproject ui))
                                              " to "
                                              (objectstring (list hh vv) :project (targetproject ui))
                                              "?") :cancel nil)
                  (yesornodialog (concatenate 'string "Would you like to set the container of " 
                                              (objectstring sel :project (targetproject ui))
                                              " to "
                                              (objectstring theguy :project (targetproject ui))
                                              "?") :cancel nil))
            (setf (objectlist UndoableSetLog) (list sel))
            (setf (PropertyName UndoableSetLog) (list 'boundsrect 'container))
            (setf (valuelist UndoableSetLog) (list (list (boundsrect sel)) (list (container sel))))
            (setf (container sel) theguy)
            (setlocation sel hh vv :physical t)
            (updateEditors (SelectedItems selectionHalo) :propertyname 'boundsrect)
            (updateEditors (SelectedItems selectionHalo) :propertyname 'container)
            (select sel)
            ))))))
  t)

(define-handler uiEdit (handler)
  (edithandlerobjectdialog me))

(new uisimpleWindow :objectname "ProjectOverviewer" :project ui
     :properties '(CurrentTool))
(addproperty ProjectOverviewer 'items)
(addproperty ProjectOverviewer 'targetProject)
(setf (actor PBMenuProjectEditor) ProjectOverviewer)

(setf (sk8::menubar ProjectOverviewer) nil)
(setf (resizer ProjectOverviewer) t)
(setf (zoombox ProjectOverviewer) t)


(defun storepile (proj its)
  (when (or (eq proj sk8) (not (inheritsfrom proj project))) (setf proj nil))
  (when proj
    (setf (items ProjectOverviewer) (delete-if #'(lambda (x) (eq (car x) proj)) (items ProjectOverviewer)))
    (push (cons proj its) (items ProjectOverviewer)))
  )
(defun getpile (proj)
  (let ((pile (list nil)))
    (dolist (i (items ProjectOverviewer))
      (if (eq (car i) proj) 
        (setf pile (cdr i))))
    pile
    ))

(define-handler clearReferences (ProjectOverviewer &key ((:objects theobjects)))
  (if theobjects
    (if (is-a (car theobjects) project)
      (setf (items ProjectOverviewer) (delete-if #'(lambda (x) (eq (car x) (car theobjects))) (items ProjectOverviewer)))
      (withActorLocked (me)
        (setf (inputObjects (picker POObjectList)) (delete-if #'(lambda (x) (memq x theobjects)) (mapcar #'value (items (picker POObjectList)))))
        (setf (inputObjects (picker POObjectPile)) (delete-if #'(lambda (x) (memq x theobjects)) (items (picker POObjectPile))))
        ))
    )
  )

(define-handler SetUpForProject (ProjectOverviewer &key ((:project theproject)))
  (setf (targetProject me) theproject))

(define-handler targetProject (ProjectOverviewer)
  (sk8::getValue 'targetProject me))

(define-handler (setf targetProject) (value ProjectOverviewer)
  (unless (and (eq value (targetproject me)) (text me) (not (string= (text me) "")))
    (withactorlocked (me)
      (setf (text me) (if value
                        (concatenate 'string (objectstring value :project value) " Overviewer" )
                        "Project Overviewer"))
      (let ((oldproj (getValue 'targetProject me)))
        (storepile oldproj (items (picker POObjectPile)))
        (setf (inputObjects (picker POObjectList)) nil)
        (setf (inputobjects (picker POObjectPile)) (getpile value))
        (setf (inputproject (picker POConstantSheet)) nil)
        (setf (inputproject (picker POVariableSheet)) nil)
        (setf (inputproject (picker POFunctionSheet)) nil)
        (if (or  (eq value ui) (not (is-a value project))) (setf value nil))
        (setValue 'targetProject me value)
        (unless value
          (when (currenttool me) (hide (picker (currenttool me))))
          (setf (enabled poswatchesshown) nil)
          (setf (currenttool me) nil))
        (if (eq value sk8) 
          (click POBPile)
          (click (or (currentTool me) (prototypes me))))
        ))))

;;;;__________________________________________________________
;;;; OUR FIVE PICKERS!!!
(new UIHOL :objectname "POObjectList" :project ui)
(addparent POObjectList uiTextListforcorners)
(setf (container POObjectList) ProjectOverviewer)
(tagpart ProjectOverviewer POObjectList 'ObjectList)
(setf (alphabeticalDisplay (picker POObjectList)) t)
(setf (selectionstyle (picker POObjectList)) 'discontiguous)
(setf (swatchesshown (picker POObjectList)) nil)
(setf (relation (picker POObjectList)) nil)
(setf (arrowsize (picker POObjectList)) '(0 0))
(setf (iconsize (picker POObjectList)) '(30 30))
(setf (objectname (picker POObjectlist)) "POObjPicker")
(define-handler filterproject (POObjPicker)
  (targetProject (sk8::window me)))

(define-handler draggingMouseEnter (POObjPicker actorDragged)
  (let ((objs (Object ObjectDataRect))
        (okprojs (okprojects (targetproject ui))))
    (unless (listp objs) (setf objs (list objs)))
    (if (and (eq actordragged ObjectDataRect)
             (eq (currenttool (sk8::window me)) (prototypes (sk8::window me)))
             (every #'(lambda (x) (memq (project x) okprojs)) objs)
             (not (some #'prototype objs)))
      (setf (highlight me) t)
      (call-next-method))))

(define-handler dropped (POObjPicker droppee)
  (let ((objs (Object ObjectDataRect))
        (okprojs (okprojects (targetproject ui))))
    (unless (listp objs) (setf objs (list objs)))
    (if (and (eq droppee ObjectDataRect)
             (eq (currenttool (sk8::window me)) (prototypes (sk8::window me)))
             (every #'(lambda (x) (memq (project x) okprojs)) objs)
             (not (some #'prototype objs)))
      (withactorlocked (me)
        (draggingmouseleave me droppee)
        (mapcar #'(lambda (x) (setf (prototype x) t)) objs)
        (bringup (sk8::window me))
        (setf (keytarget (sk8::window me)) me)
        (setupitems (prototypes (sk8::window me)))
        )
      (call-next-method))))

(define-handler Keydown (POObjPicker thechar)
  (if (and (eq theChar #\Delete) (selecteditems me)
           (eq (currenttool (sk8::window me)) (prototypes (sk8::window me))))
    (let ((objs (mapcar #'value (selecteditems me))))
      (when (yesornodialog (concatenate 'string
                                        "Do you wish to remove "
                                        (objectstring (if (eql 1 (length objs))
                                                        (car objs) objs
                                                        )
                                                      :project (targetproject ui)) " from the list of prototypes?") :cancel nil)
        (undoableset 'prototype objs nil)
        (setupitems (prototypes (sk8::window me)))
        ))
    (call-next-method)))
(hide (titlebar POObjectList))

(define-handler (setf inputobjects) (theval POObjPicker)
  (call-next-method)
  (setvalue 'inputobjects me nil))

(addparent POObjPicker UIObjectDragMixin)

(new UIHOL :objectname "POObjectPile" :project ui)
(addparent POObjectPile uiTextListforcorners)
(addparent (picker POObjectPile) mixinforobjectpiles)
(setf (swatchesShown (picker POObjectPile)) nil)
(setf (container POObjectPile) ProjectOverviewer)
(setf (editing (picker POObjectPile)) t)
(setf (alphabeticalDisplay (picker POObjectPile)) nil)
(setf (selectionStyle (picker POObjectPile)) 'discontiguous)
(tagpart ProjectOverviewer POObjectPile 'ObjectPile)
(hide POObjectPile)
(hide (titlebar POObjectPile))
(setf (text (titlebar POObjectPile)) "Free Form (Drop Stuff Here)")
(addparent (picker POObjectPile) UIObjectDragMixin)


;;;We specialize this for speed.  Reuse swatches when appropriate.
(define-handler GenerateItems ((picker POObjectPile) itemlist)
  (let* ((its (items me))
         oldguy
         newlist 
         oldSwatches
         (SwatchesShown (SwatchesShown me))
         (relation (relation me))
         (p (project me)))
    (setf itemlist (mapcar #'(lambda (x) (if (is-a x hierarchicalpickeritem) (value x) x)) itemlist))
    (setf oldSwatches (remove-if #'(lambda (x) (and swatchesshown (memq (value x) itemlist))) its))
    (logoutswatches me (mapcar #'swatch oldSwatches))
    (mapcar #'(lambda (x) (setf (swatch x) nil)) oldSwatches)
    (dolist (i ItemList)
      (setf oldguy (car (remove-if-not #'(lambda (x) (eq (value x) i)) its)))
      (push (if (and oldguy (or (not SwatchesShown) (swatch oldguy)))
              oldguy
              (new hierarchicalpickeritem 
                   :project p 
                   :value i
                   :state (if (and relation (memq relation (properties i)) 
                                   (funcall relation i))
                            'closed 
                            nil)
                   :swatch (if swatchesshown (generateswatch i me) nil)))
            newlist)
      )
    (nreverse newlist)))

(define-handler (setf inputobjects) (theval (picker POObjectPile))
  (call-next-method)
  (setvalue 'inputobjects me nil))

(new uiTextListforcorners :objectname "POVariableSheet" :project ui)
(setf (pickerPrototype POVariableSheet) VariableSheetPicker)
(setf (container POVariableSheet) ProjectOverviewer)
(tagpart ProjectOverviewer POVariableSheet 'VariableSheet)
(hide POVariableSheet)
(hide (titlebar POVariableSheet))
(define-handler keyDown ((picker POVariableSheet) thechar)
  (cond 
   ((or (eq theChar #\Return) (eq thechar #\Enter))
    (doubleclick me))
   (t (call-next-method))))
(define-handler doubleclick ((picker POVariableSheet))
  (when (and (items me) (selecteditems me))
    (withActorLocked (me)
        (let* ((objed (sk8::window me))
               (valtext (valuetext objed))
               (editor (picker valtext))
               (theselection (selection me))
               (rowselected (cadar theselection)))
          (setf (text (picker valtext)) (objectstring (eval (car (selecteditems me))) :project (inputproject me)))
          (sk8-multival-bind (ll tt rr bb) (itemboundsrect me (list 1 rowselected) :physical t)
            (sk8-multival-bind (lll ttt rrr bbb) (boundsrect valtext :physical t)
              (setf (oldbounds valtext) (list ll tt rr bb))
              (zoomRect ll tt rr bb lll ttt rrr bbb :NumberOfFrames 10
                        :duration 0.001)))
          ;;    (itemboundsrect me (list 1 rowselected) :physical t)
          (setf (title valtext) (concatenate 'string "Editing " (Name (car (selecteditems me)))))
          (bringtofront valtext)
          (setf (keytarget objed) editor)
          (setselection editor 0 -1)
          (show valtext)
          ))))

(new UIValueText :objectname "POValueText" :project ui)
(addparent POValueText uitextlistforcorners)
(setf (container POValueText) ProjectOverviewer)
(tagpart ProjectOverviewer POValueText 'ValueText)
(hide POValueText)
(hide (gfubutton POValueText))
(define-handler NewKeyTarget (POValueText)
  (picker (VariableSheet (sk8::window me))))
(define-handler resized (POValueText)
  (call-next-method)
  (sk8-multival-bind (hh vv) (size me)
    (setboundsrect (picker me) 0 0 0 -24 :relative t)
    (setf (fillcolor (whitebox me)) white)
    (setf (fillcolor (BlackLine me)) lightgray)
    (setboundsrect (whitebox me) 5 (- vv 25) (- hh 12) (- vv 1))
    (setboundsrect (BlackLine me) 7 (- vv 24) (- hh 17) (- vv 23))
    (setboundsrect (CancelButton me) (- hh 145) (- vv 20) (- hh 100) (- vv 3))
    (setboundsrect (TryItButton me) (- hh 95) (- vv 20) (- hh 60) (- vv 3))
    (setboundsrect (OKButton me) (- hh 55) (- vv 20) (- hh 20) (- vv 3))
    (setboundsrect (GFUButton me) (- hh 36) 4 (- hh 4) 16)
    ))

(define-handler Keydown ((picker POValueText) thechar)
  (if (or (eq theChar #\Tab) (eq theChar #\Escape) (eq theChar #\Return) (eq theChar #\Enter))
    (progn
      (when (or (eq theChar #\Return) (eq theChar #\Enter))
        (let* ((var (car (selecteditems (picker (VariableSheet (sk8::window me))))))
               (proj (inputProject (picker (VariableSheet (sk8::window me))))))
          (sk8-multival-bind (failure? errors locals globals result)
                             (translateScriptCommandOrExpr proj (text me))
            (declare (ignore errors locals globals))
            (if failure?
              (MessageToUser "Syntax error!")
              (progn
                (setf result (evaluateScriptTranslation result))
                (set var result)
                (setf (inputProject (picker (VariableSheet (sk8::window me)))) proj)
                (call-next-method me #\Tab)
                )))))
      (when (or (eq theChar #\Tab) (eq theChar #\Escape) )
        (call-next-method me #\Tab)))
    (call-next-method)))

(new uiTextListforcorners :objectname "POConstantSheet" :project ui)
(setf (pickerPrototype POConstantSheet) ConstantSheetPicker)
(setf (container POConstantSheet) ProjectOverviewer)
(tagpart ProjectOverviewer POConstantSheet 'ConstantSheet)
(hide POConstantSheet)
(hide (titlebar POConstantSheet))

(new uiTextListforcorners :objectname "POFunctionSheet" :project ui)
(setf (pickerPrototype POFunctionSheet) FunctionSheetPicker)
(setf (container POFunctionSheet) ProjectOverviewer)
(tagpart ProjectOverviewer POFunctionSheet 'FunctionSheet)
(hide POFunctionSheet)
(hide (titlebar POFunctionSheet))


;;;;__________________________________________________________
;;;; OUR Buttons...

;;;first the prototype...

(new rectangle :objectname "ProjectOverviewerButton" :project ui
     :properties '(picker))

(new UIMenuOut :objectname "POBOutColor" :project ui)
(setf (arrowtodraw POBOutColor) nil)

(setf (fillcolor ProjectOverviewerButton) POBOutColor)
(setf (textcolor ProjectOverviewerButton) black)
(setf (textfont ProjectOverviewerButton) espysansboldfont)
(setf (textsize ProjectOverviewerButton) 9)
(setf (autohighlight ProjectOverviewerButton) t)
(setf (textstyle ProjectOverviewerButton) '(plain))
(setf (framesize ProjectOverviewerButton) '(0 0))
(setsize ProjectOverviewerButton 95 17)
(setf (textvoffset ProjectOverviewerButton) -2)

(define-handler (setf highlight) (theval ProjectOverviewerButton)
  (setf (inverts me) nil)
  (call-next-method)
  (lock me)
  (setf (fillcolor me) (if theval white (if (eq me (currentTool (sk8::window me))) ProjectButtonIn POBOutColor)))
  (setf (framesize me) (if theval '(1 1) '(0 0)))
  (setf (textcolor me) (if theval uidarkcolor (if (eq me (currentTool (sk8::window me))) white black)))
  (unlock me)
)

(define-handler newAction (ProjectOverviewerButton)
  (commandkeyevent POMenubar #\N))

(define-handler click (ProjectOverviewerButton)
  (sk8::withlockedcursor animatedclock
    (let ((curTool (currentTool (sk8::window me))))
      (setf (framesize me) '(0 0))
      (if (targetProject (sk8::window me))
        (withcursor watchcursor
          (unless (eq me curtool)
            (withactorlocked (me)
              (setf (fillcolor me) ProjectButtonIn)
              (setf (textcolor me) white)
              (when curTool
                (setf (fillcolor curtool) POBOutColor)
                (setf (textcolor curtool) black)
                )
              (setf (currentTool (sk8::window me)) me)
              (setf (inputObjects (picker POObjectList)) nil)
              (setf (inputproject (picker POConstantSheet)) nil)
              (setf (inputproject (picker POVariableSheet)) nil)
              (setf (inputproject (picker POFunctionSheet)) nil)
              (when curTool
                (hide (picker curTool))
                )
              (show (picker me))
              ))
          (withactorlocked (me)
            ; (setf (title (picker me)) (text me))
            (setf (enabled (reliefbutton (sk8::window me))) t)
            (setupitems me)
            (setf (keytarget (sk8::window me)) (picker (picker me)))
            ))
        (progn
          (setf (fillcolor me) POBOutColor)
          (setf (enabled (reliefbutton (sk8::window me))) nil)
          (when curTool
            (setf (fillcolor curTool) POBOutColor)
            (setf (textcolor curTool) black)
            )))
      )))

(define-handler (setf CurrentTool) (theval ProjectOverviewer)
  (let ((oldtool (currenttool me)))
    (when (and oldtool (null theval))
      (setf (fillcolor oldtool) POBOutColor)
      (setf (textcolor oldtool) black)
      ))
  (setValue 'currenttool me theval)
  )

(define-handler setupitems (ProjectOverviewerButton)
  nil)

;;; Objects Button
(new ProjectOverviewerButton :objectname "POBObjects" :project ui)
(setf (text POBObjects) "All Objects")
(setf (container POBObjects) ProjectOverviewer)
(setf (textstyle POBObjects) '(plain))
(define-handler picker (POBObjects)
  (ObjectList (sk8::window me)))
(define-handler setupitems (POBObjects)
  (setf (enabled poswatchesshown) t)
  (multiple-value-bind (val1 err?) 
                       (ignore-errors (setf (inputobjects (picker (picker me))) 
                                            (delete-if #'(lambda (x) (memq x (uiClearedObjects)))
                                                       (objects (targetProject (sk8::window me))))))
    (declare (ignore val1))
    (if err? 
      (setf (inputobjects (picker (picker me))) 
            (list "Too many items to display..."))))
  )
(define-handler newAction (POBObjects)
  (let* ((proto (or (car (selecteditems (picker (picker me)))) object))
         (theval (getnewfromuser (if (and (is-a (picker (picker me)) hierarchicalpicker)
                                          (is-a proto hierarchicalpickeritem))
                                   (value proto)
                                   proto)
                                 :locked nil :project (targetproject ui))))
    (withactorlocked (me)
      (setupitems me)
      (setf (selecteditems (picker (picker me))) 
            (remove-if-not #'(lambda (x) (eq (value x) theval)) (items (picker (picker me)))))
      )))

;;; Prototypes Button
(new ProjectOverviewerButton :objectname "POBPrototypes" :project ui)
(tagpart ProjectOverviewer POBPrototypes 'Prototypes)
(setf (text POBPrototypes) "Prototypes")
(setf (container POBPrototypes) ProjectOverviewer)
(define-handler picker (POBPrototypes)
  (ObjectList (sk8::window me)))
(define-handler setupitems (POBPrototypes)
  (setf (enabled (reliefbutton (sk8::window me))) t)
  (setf (enabled poswatchesshown) t)
  (setf (inputobjects (picker (picker me))) 
        (delete-if #'(lambda (x) (memq x (uiClearedObjects)))
                   (remove-if-not #'prototype
                                  (objects (targetProject (sk8::window me)))))))

(define-handler newAction (POBPrototypes)
  (let ((theval (getnewfromuser object :locked nil :project (targetproject ui))))
    (withactorlocked (me)
      (setf (prototype theval) t)
      (setupitems me)
      (setf (selecteditems (picker (picker me))) 
            (remove-if-not #'(lambda (x) (eq (value x) theval)) (items (picker (picker me)))))
      )))
(define-handler draggingMouseEnter (POBPrototypes actorDragged)
  (let ((objs (and (eq actordragged ObjectDataRect) (object objectdatarect))))
    (unless (listp objs) (setf objs (list objs)))
    (when (and objs
               (every #'(lambda (x) (memq (project x) (okprojects (targetproject ui)))) objs)
               (every #'(lambda (x) (not (is-a x handler))) objs)
               (not (some #'prototype objs)))
      (setf (highlight me) t))))

(define-handler draggingMouseLeave (POBPrototypes actorDragged)
  (when (highlight me) (setf (highlight me) nil)))

(define-handler dropped (POBPrototypes droppee)
  (when (highlight me)
    (draggingMouseLeave me droppee)
    (click me)
    (dropped (picker (picker me)) droppee)
    )
  )


;;; Media Button
(new ProjectOverviewerButton :objectname "POBMedia" :project ui)
(setf (text POBMedia) "Media")
(setf (container POBMedia) ProjectOverviewer)
(define-handler picker (POBMedia)
  (ObjectList (sk8::window me)))
(define-handler setupitems (POBMedia)
  (setf (enabled poswatchesshown) t)
  (setf (inputobjects (picker (picker me))) 
        (delete-if #'(lambda (x) (memq x (uiClearedObjects)))
                   (remove-if-not #'(lambda (x) (and (inheritsfrom x media)
                                                     (not (inheritsfrom x pixelmap))))
                                  (objects (targetProject (sk8::window me)))))))

(define-handler newAction (POBMedia)
  (withcursor watchcursor
    (bringup MediaBrowser)
  ))

;;; Renderer Button
(new ProjectOverviewerButton :objectname "POBRenderer" :project ui)
(setf (text POBRenderer) "Renderers")
(setf (container POBRenderer) ProjectOverviewer)
(define-handler picker (POBRenderer)
  (ObjectList (sk8::window me)))
(define-handler setupitems (POBRenderer)
  (setf (enabled poswatchesshown) t)
  (setf (inputobjects (picker (picker me))) 
        (delete-if #'(lambda (x) (memq x (uiClearedObjects)))
                   (remove-if-not #'(lambda (x) (and (inheritsfrom x Renderer)
                                                     (not (inheritsfrom x swatchrenderer))
                                                     ))
                                  (objects (targetProject (sk8::window me)))))))
  
(define-handler newAction (POBRenderer)
  (withcursor watchcursor
    (bringup colorpalette)
    ))

;;;------------------------------------------
;;; User Defined Button
(new ProjectOverviewerButton :objectname "POBUserDefined" :project ui)
(setf (text POBUserDefined) "User Query")
(setf (container POBUserDefined) ProjectOverviewer)
(addproperty POBUserDefined 'item)

(define-handler picker (POBUserDefined)
  (ObjectList (sk8::window me)))

(define-handler getitem (POBUserDefined)
  (withcursor standardcursor
    (setf (item me) 
          (getAnswerFromUser "Please enter a SK8Script expression to generate a list of items (e.g. deepcontents of the stage).  These items will be displayed in the Project Overviewer.  Double click on the User Query tab to change your query:"
                             :defaultanswer (if (stringp (item me)) (item me) "")))))

;;; This is called within an ignore errors. If it can, it returns the result of the evaluation.

(defun evaluateScriptExpression (proj theString)
  (sk8-multival-bind (fail? errs locals globals result)
                     (translateScriptCommandOrExpr proj theString)
    (declare (ignore locals globals errs))
    (if fail? 
      nil
      (evaluateScriptTranslation result))))

(define-handler setupitems (POBUserDefined)
  (setf (enabled poswatchesshown) t)
  (unless (item me) (getitem me))
  (if (item me)
    (multiple-value-bind (val1 err?) 
                         (ignore-errors 
                          (setf (inputobjects (picker (picker me))) 
                                (delete-if #'(lambda (x) (or (not (eq (project x) (targetProject (sk8::window me)))) 
                                                             (memq x (uiClearedObjects))))
                                           (evaluatescriptexpression (targetProject (sk8::window me)) (item me))
                                           )
                                ))
      (declare (ignore val1))
      (when err?
        (setf (inputobjects (picker (picker me))) nil)
        (setf (item me) nil)
        (messagetouser "Error while trying to evaluate your query.  The query has been cleared." :beep t)))
    (setf (inputobjects (picker (picker me))) nil))
  )
  
(define-handler doubleClick (POBUserDefined)
  (getitem me)
  (setupitems me))

(define-handler newAction (POBUserDefined)
  (beep))

;;------------------------------------------
;;; Pile Button
(new ProjectOverviewerButton :objectname "POBPile" :project ui)
(setf (text POBPile) "Drop Pile")
(setf (container POBPile) ProjectOverviewer)
(define-handler picker (POBPile)
  (ObjectPile (sk8::window me)))
(define-handler setupitems (POBPile)
  (setf (enabled poswatchesshown) t)
  (setf (enabled (reliefbutton (sk8::window me))) t)
  (setf (inputobjects (picker (picker me)))  
        (delete-if #'(lambda (x) (memq x (uiClearedObjects))) (items (picker (picker me)))))
  )
(define-handler newAction (POBPile)
  (let ((theval (getnewfromuser object :locked nil :project (targetproject ui))))
    (withactorlocked (me)
      (setf (items (picker (picker me))) (cons theval (items (picker (picker me)))))
      (setf (selecteditems (picker (picker me))) 
            (list theval))
      )))
(define-handler draggingMouseEnter (POBPile actorDragged)
  (when (or (eq actordragged Functiondatarect) 
            (eq actordragged Handlerdatarect)
            (eq actordragged ObjectDataRect))
    (setf (highlight me) t)))

(define-handler draggingMouseLeave (POBPile actorDragged)
  (when (highlight me) (setf (highlight me) nil)))

(define-handler dropped (POBPile droppee)
  (when (highlight me)
    (draggingMouseLeave me droppee)
    (click me)
    (dropped (picker (picker me)) droppee)
    )
  )


;;; Functions Button
(new ProjectOverviewerButton :objectname "POBFunctions" :project ui)
(setf (text POBFunctions) "Functions")
(setf (container POBFunctions) ProjectOverviewer)
(setf (textstyle POBFunctions) '(plain))
(define-handler picker (POBFunctions)
  (FunctionSheet (sk8::window me)))
(define-handler setupitems (POBFunctions)
  (setf (enabled poswatchesshown) nil)
  (setf (inputproject (picker (picker me))) (targetProject (sk8::window me))))

;;; Constants Button
(new ProjectOverviewerButton :objectname "POBConstants" :project ui)
(setf (text POBConstants) "Constants")
(setf (textstyle POBConstants) '(plain))
(setf (container POBConstants) ProjectOverviewer)
(define-handler picker (POBConstants)
  (ConstantSheet (sk8::window me)))
(define-handler setupitems (POBConstants)
  (setf (enabled poswatchesshown) nil)
  (setf (inputproject (picker (picker me))) (targetProject (sk8::window me))))

;;; Variables Button
(new ProjectOverviewerButton :objectname "POBVariables" :project ui)
(setf (text POBVariables) "Globals")
(setf (textstyle POBVariables) '(plain))
(setf (container POBVariables) ProjectOverviewer)
(define-handler picker (POBVariables)
  (VariableSheet (sk8::window me)))
(define-handler setupitems (POBVariables)
  (setf (enabled poswatchesshown) nil)
  (setf (inputproject (picker (picker me))) (targetProject (sk8::window me))))
(define-handler newAction (POBVariables)
  (if (eq (keytarget (sk8::window me)) (picker (ValueText (sk8::window me))))
    (keydown (picker (ValueText (sk8::window me))) #\escape))
  (call-next-method)
  )

;;;;__________________________________________________________________________________________

(new uicheckbox :objectname "POSwatchesShown" :project ui)
(setf (text POSwatchesShown) "Show Swatches")
(setf (container POSwatchesShown) ProjectOverviewer)
(setf (checked POSwatchesShown) nil)
(tagpart ProjectOverviewer POSwatchesShown 'checkbox)
(define-handler mousedown (POSwatchesShown)
  (when (enabled me)
    (call-next-method)))
(define-handler check (POSwatchesShown)
  (when (enabled me)
    (call-next-method)
    (let ((p (picker (objectlist (sk8::window me))))
          (p2 (picker (objectpile (sk8::window me))))
          (ct (currenttool (sk8::window me))))
      (sk8::withlockedcursor animatedclock
        (if (checked me)
          (withactorlocked (me)
            (setf (swatchesShown p) t)
            (setf (linespacing p) 2)
            (setf (swatchesShown p2) t)
            (setf (linespacing p2) 2)
            (when ct (setupitems ct)))
          (withactorlocked (me)
            (setf (swatchesShown p) nil)
            (setf (linespacing p) 0)
            (setf (swatchesShown p2) nil)
            (setf (linespacing p2) 0)
            (when ct (setupitems ct))))))))


;;;;__________________________________________________________________________________________
;;;;__________________________________________________________________________________________
;;;;__________________________________________________________________________________________

(new uiMenuBarActor :otherparents browserMenuBar :objectname "POMenubar" :project ui)

(setf (container POMenubar) ProjectOverviewer)
(tagpart ProjectOverviewer POMenubar 'Bar)
(setup POMenubar)
(dolist (i (menus POMenubar)) (addparent i uimenuactor))

;;(dispose (car (menus POMenubar)))
(setf (sk8::menubar (car (menus POMenubar))) nil)

(setf (objectname (car (menus POMenubar))) "POObjMenu") 
(define-handler browsercomponent (POObjMenu)
  (let ((PO (sk8::window (sk8::menubar me))))
    (if (eq (keytarget po) (picker (objectpile PO)))
      (picker (objectpile PO))
      (picker (ObjectList PO)))))
(sendtoback (cadr (menus POMenubar)))
(sendtoback (cadr (menus POMenubar)))
(sendtoback (caddr (menus POMenubar)))
(sendtoback POMenubar)

;;;;__________________________________________________________________________________________

(new uibutton :objectname "PONewButton" :project ui)
(setf (container PONewButton) ProjectOverviewer)
(setf (text PONewButton) "New")
(define-handler click (PONewButton)
  (when (currenttool (sk8::window me))
    (newaction (currenttool (sk8::window me)))))
(tagpart ProjectOverviewer PONewButton 'reliefButton)


;;;;__________________________________________________________________________________________


(new uibiglabel :objectname "POObjs" :project ui)
(setf (container POObjs) ProjectOverviewer)
(setf (text POOBjs) "Objects")
(sendtoback POObjs)

(new uibiglabel :objectname "POOthers" :project ui)
(setf (container POOthers) ProjectOverviewer)
(setf (text POOthers) "Other")
(sendtoback POOthers)

;;;;__________________________________________________________________________________________
;;;;__________________________________________________________________________________________
;;;;__________________________________________________________________________________________
;;;;__________________________________________________________________________________________

(define-handler resized (ProjectOverviewer)
  (let (hsize vsize)
    (sk8-multival-setf (hsize vsize) (size me))
    (withActorLocked (me)
      (setboundsrect (Bar me) 0 vsize hsize (+ vsize 20))
      (let* ((butts (nreverse (delete-if-not #'(lambda (x) (inheritsfrom x projectoverviewerbutton)) 
                                             (contents me))))
             (hspace (+ (+ 10 *windowLeft*) (width projectoverviewerbutton)))
             (x3 (- hspace 1))
             (x4 (- hsize *windowRight*))
             (y1 (+ 25 *windowTop*))
             (y2 (- vsize *windowBottom*)))
        (setBoundsRect POObjs (+ 3 *windowLeft*) (+ 8 *windowTop*) 100 (+ 20 *windowTop*))
        (tile (subseq butts 0 6) :boundsrect (list (+ 10 *windowLeft*) y1 0 0) :columns 1 :vspacing -3 :hspacing 0)
        
        (setBoundsRect POOthers (+ 3 *windowLeft*) (+ 119 *windowTop*) 100 (+ 131 *windowTop*))
        (tile (subseq butts 6 9) :boundsrect (list (+ 10 *windowLeft*) (+ 135 *windowTop*) 0 0) :columns 1 :vspacing -3 :hspacing 0)
        (setBoundsRect (VariableSheet me) x3 y1 x4 y2)
        (setBoundsRect (valueText me) x3 y1 x4 y2)
        (setBoundsRect (ConstantSheet me) x3 y1 x4 y2)
        (setBoundsRect (FunctionSheet me) x3 y1 x4 y2)
        (setBoundsRect (ObjectList me) x3 y1 x4 y2)
        (setBoundsRect (ObjectPile me) x3 y1 x4 y2)
        (setBoundsRect (reliefButton me) (+ 24 *windowLeft*) 231 (+ 78 *windowLeft*) 252)
        (setf (left (checkbox me) :resizing nil) (+ 10 *windowLeft*))
        (setf (top (checkbox me) :resizing nil) 210)
        (call-next-method))
      )))
(setboundsrect ProjectOverviewer 6 216 288 476)
;;;(setf (container ProjectOverviewer) stage)
;;;(setf (targetProject ProjectOverviewer) ui)
;;;(setf (targetProject ProjectOverviewer) nil)
;;;(setf (targetProject ProjectOverviewer) foo)
;;;;   (boundsrect ProjectOverviewer)
;;;;   (resized ProjectOverviewer)

(define-handler (setf keytarget) (theval ProjectOverviewer)
  (call-next-method)
  (if (and (visible (valuetext me)) (neq theval (picker (valuetext me))))
    (keydown (picker (valuetext me)) #\escape)))

(resized ProjectOverviewer)
(define-handler enteringStage (ProjectOverviewer)
  (call-next-method)
  (setf (targetProject me) (targetProject ui)))

(define-handler (setf container) (theval projectoverviewer)
  (call-next-method)
  (unless theval
    (setf (targetProject me) nil)))


(setf (minimumsize ProjectOverviewer) '(275 260))
#|
Change History (most recent last):
1	11/29/93	rod	
5	12/13/93	rod	Added checks for disposed projects.
6	12/17/93	till	#.'s be gone: keydown, keydown, keydown
14	2/12/94	kleiman	renaming
15	2/14/94	sidney	rename children to knownchildren
	19	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	20	2/21/94	hernan	window -> sk8::window.
	23	2/23/94	rod	Relativizing for multiple copies
	25	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	29	2/28/94	hernan	Avoiding disposing things directly.
	31	3/5/94	rod	addproperty avoided wherever possible
	33	3/9/94	rod	Doing Project Switching and Reference Clearing.
	34	3/9/94	rod	Doing Project Switching and Reference Clearing.
	37	3/12/94	sidney	check if currenttool is nil before trying to hide its picker
	39	3/16/94	rod	Fixing various problems...
	40	3/16/94	rod	Fixing various problems...
	42	3/17/94	rod	Drag Multiple.
	43	3/17/94	rod	Various clean up.
	46	3/21/94	kleiman	setvalue -> sk8::setValue
	47	3/21/94	kleiman	newactor for POBObjects missed parenthesis at end during build
	49	3/23/94	rod	Yet more GC work.
	51	3/26/94	rod	Minimum size stuff.
	55	3/29/94	sidney	The SourceServer interface screwed up the comment block at the last checkin
	56	3/29/94	sidney	It did it again!
	62	4/22/94	Hernan	Made resized do its thing with the actor locked.
	63	4/26/94	rod	Redoing drag stuff
	64	4/27/94	rod	Removed kludge as the HierarchicalObjectPicker
				was made more general.
	67	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	69	5/31/94	rod	
	69	5/31/94	rod	
	70	6/3/94	rod	
	71	6/12/94	rod	Making project switching behave.
	72	6/13/94	rod	1167953: Fixing keyboard shortcuts.
	73	6/17/94	rod	
	74	6/17/94	rod	Variables->Globals
	75	6/17/94	rod	
	76	6/17/94	rod	Making sure title always gets set.
	77	6/17/94	rod	Fixing objectstring
	78	6/23/94	rod	
	79	6/23/94	rod	Fixing bug in color dropping of hierachical picker.
	80	6/29/94	rod	Made sure items could be moved between
				buttons.
	82	7/13/94	rod	
	83	7/15/94	rod	
	84	7/15/94	rod	Adding drag and drop of functions, constants, and variables.
	85	7/18/94	rod	Speeding up project switching.
	86	7/21/94	rod	Fixed dragging.
	87	7/21/94	rod	DoubleClick on pile edits...
	88	7/21/94	rod	Fixing linespacing.
	89	7/25/94	rod	Fixing bug when project overviewer is brought up
				without anything loaded.
	90	7/27/94	rod	Adding Swatches to Pile.
	91	7/27/94	rod	
	92	8/2/94	rod	Fixing defaults on pile.
	93	8/8/94	rod	
	94 	 8/30/94	rod     	Making resized call-next-method
	95 	 8/31/94	rod     	
	96 	 8/31/94	rod     	Prototypes bug fixed.
	97 	 8/31/94	rod     	
	98 	 9/ 1/94	Hernan  	espysansbold -> espysansboldfont.
	99 	 9/13/94	rod     	Fixing selectionStyle of pile.
	100	10/ 4/94	rod     	Logging out swatches.  Clearing inputobjects.
	101	10/10/94	rod     	Fixing keyword args...
	102	10/19/94	rod     	Removing libraries.
	103	10/19/94	rod     	Graphic fixes.
	104	10/25/94	rod     	Fixing odd highlighting of buttons when dragged
							over.
	105	11/ 8/94	rod     	Making it so you can't drag handlers to the
							prototype button.
	106	11/11/94	rod     	Fixing bug where the Project Overviewer 
							sometimes appeared with nothing in it.  It was
							an Eval-Enqueue problem.
	107	12/ 3/94	rod     	
	108	12/ 3/94	rod     	Making buttons autohighlight and work on click.
	109	12/ 5/94	rod     	Fixing click of buttons.
	110	12/16/94	rod     	Fixing bug with new button while editing a 
							global's value.
	111	 1/ 4/95	sidney  	remove remaining traces of obsoleted lib functionality
	112	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	113	 4/13/95	rod     	
	114	 4/25/95	rod     	
	3  	 8/28/95	Brian   	
	4  	12/ 8/95	Brian   	Adding user defined button.
	5  	12/11/95	Brian   	fixing call to ss package
	6  	 1/17/96	Hernan  	Folding in the new compiler API.
	7  	 2/14/96	Brian   	fixing population stuff
	2  	 5/ 7/96	Hernan  	evaluateScriptExpression -> translateScriptCommandOrExpr.
	3  	 7/ 8/96	Brian   	fixing zoomrects
	4  	10/14/96	Brian   	Fixing dialog text for user query.
	5  	11/22/96	Brian   	
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
