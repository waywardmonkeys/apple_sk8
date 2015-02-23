;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8development)

(SK8-declare-syms :SK8 :public ; Updated 12-20-94   2:03 pm
                  SK8::ALL SK8::CREATESWATCH SK8::GRAPHIC SK8::INHERITED SK8::LOCAL SK8::PARENTS)

(provide "BROWSERCOMPONENTS")

(require "PROPERTYVALUEDATA" "objects;Browser Components:PropertyValueData")
(require "UNDOABLESETLOG" "objects;Browser Components:UndoableSetLog")
(require "OBJECTDATARECT" "objects;Widgets:ObjectDataRect")
(require "HALO" "objects;Shapes:Halo")
(require "MENUS")
(require "PICKERMENU" "objects;Widgets:PickerMenu")
(require "SCRIPTEDITTEXT" "objects;EditText:ScriptEditText")
(require "ROUNDRECT" "objects;Shapes:RoundRect")
(require "POLYGON" "objects;Shapes:Polygon")
(require "TEXTLIST" "objects;Pickers:TextList")
(require "HIERARCHICALPICKER" "objects;Pickers:HierarchicalPicker")
(require "STYLEDPICKER" "objects;Pickers:StyledPicker")
(require "TABLEPICKER" "objects;Pickers:TablePicker")
(require "EDITORSCRIPTEDITTEXT" "objects;EditText:EditorScriptEditText")
(require "CHECKBOX" "objects;Widgets:CheckBox")
(require "OBJECTSYSTEMDIALOGS" "objects;Dialogs:ObjectSystemDialogs")
(require "BEVELRENDERER" "objects;Effects:BevelRenderer")
(require "GRADIENT" "objects;Effects:Gradient")
(require "TABLEPICKEREDITORADDON" "objects;Pickers:TablePickerEditorAddOn")
(require "SELECTBYROWTABLEPICKERADDON" "objects;Pickers:SelectByRowTPAddOn")
(require "GETNEWOBJECTFROMUSER" "objects;Dialogs:GetNewObjectFromUser")

;;--------------------------------------------------------------------------------------------
;;; BrowserComponent API  -- Mixed in to objects...
;;--------------------------------------------------------------------------------------------

(new Object 
     :objectName "BrowserComponent"
     :prototype t
     :project sk8
     :properties `((TargetProject :value ,sk8)))

(define-handler menuprototype (browsercomponent)
  nil)

(define-sk8-function BrowserHighlight nil (me)
  (if (and me 
           (sk8::window me)
           (inheritsFrom (sk8::window me) BrowserPaper))
    (surroundObject (highlighterHalo (sk8::window me)) me))
  )

(define-sk8-function BrowserUnHighlight nil (me)
  (if (and me
           (sk8::window me)
           (inheritsFrom (sk8::window me) BrowserPaper))
    (unless (keytarget me)
      (moveOffstage (highlighterHalo (sk8::window me)))
      )))

(define-handler activateText (browsercomponent)
  (call-next-method)
  (if (inheritsFrom (container me) textlist)
    (BrowserHighlight (container me))
    (BrowserHighlight me)))
(define-handler deactivateText (browsercomponent)
  (call-next-method)
  (if (inheritsFrom (container me) textlist)
    (BrowserUnHighlight (container me))
    (BrowserUnHighlight me)))

(define-handler resized (browsercomponent)
  (call-next-method)
  (if (eq (KeyTarget (sk8::window me)) me)
    (if (inheritsFrom (container me) textlist)
      (BrowserHighlight (container me))
      (BrowserHighlight me)))
  )


(defvar *Dragging-FrameColor* nil)

(define-handler (setf Highlight) (theval browsercomponent)
  (setf (inverts me) nil)
  (if theval
    (progn
      (setq *Dragging-FrameColor* (framecolor me))
      (setf (framecolor me) cyan))
    (progn      
      (setf (framecolor me) (or *Dragging-FrameColor* black)))
    )
  (call-next-method))

;;--------------------------------------------------------------------------------------------
;;;Objects for dragging and dropping data
;;--------------------------------------------------------------------------------------------

(new rectangle :project SK8 :objectname "PropertyDataRect"
     :properties '(objects PropertyName ComponentFrom))

(pushnew PropertyDataRect *miscelaneous-things-to-be-cleared-by-UndoableSetLog*)

(define-handler clearReferences (PropertyDataRect)
  (setf (Objects me) nil)
  (setf (PropertyName me) nil)
  (setf (ComponentFrom me) nil))

(define-handler drop (PropertyDataRect theactor)
  (declare (ignore-if-unused theActor))
  (call-next-method)
  (when (eventWindow)
    (setf (keytarget (eventWindow)) (keytarget (eventWindow)))))

(define-handler value (PropertyDataRect)
  (let ((pname (propertyname me))
        (objs (objects me))
        val)
    (when (and pname objs)
      (setf val (getpropsvalue pname objs (project (first Objs)) :asastring nil))
      (if (equal val "Objects have different values in this property")
        nil
        val))))

(defun Potential-Descendants (obj)
  (let (result)
    (MACFRAMES::map-all-descendants obj #'(lambda (x) (push x result)))
    result))

(defun DoPropertyDrop (pname Obj)
  (let ((val (value propertydatarect))
        descs)
    (unless (listp obj) (setf obj (list obj)))
    (if (every #'(lambda (x) (memq pname (properties x))) obj)
      (if (every #'(lambda (x) (SetterExistsP pname x)) obj)
        (when (and (UndoableSet pname obj val :confirmation t)
                   (setf descs (apply #'append (mapcar #'potential-descendants obj)))
                   (or (and (= 1 (length descs)) (eq (first descs) (first (objects propertydatarect))))
                       (yesornodialog (concatenate 'string "Do you wish to set the value for all " (objectstring (length descs)) " descendants as well?") :cancel nil)))
          (UndoableSet pname descs val)
          )
        (messagetouser (if (= (length obj) 1)
                         (concatenate 'string 
                                      (objectstring (first obj) :project (project (first obj))) 
                                      " already has the property "
                                      (objectstring pname) 
                                      " and that property cannot be set.")
                         (concatenate 'string 
                                      "Every item in "
                                      (objectstring (first obj) :project (project (first obj))) 
                                      " already has the property "
                                      (objectstring pname) 
                                      " and that property cannot be set for these items.")
                         )
                       :beep t))
      (addpropertydialog (remove-if #'(lambda (x) (memq pname (properties x))) obj) :property pname))))

(define-handler descriptionString (propertydatarect)
  (let ((obj (objects me)))
    (unless (listp obj) (setf obj (list obj)))
    (if (= (length obj) 1)
      (format nil "~a of ~a" 
              (name (propertyname me))
              (objectstring (first obj) :project (project (first obj))))
      (format nil "~a of every item in ~a" 
              (name (propertyname me))
              (objectstring obj :project (project (first obj))))
      )))

;;(descriptionString propertydatarect)
  

(new rectangle :project SK8 :objectname "HandlerDataRect"
     :properties '(Objects Handler ComponentFrom))

(pushnew HandlerDataRect *miscelaneous-things-to-be-cleared-by-UndoableSetLog*)

(define-handler clearReferences (HandlerDataRect)
  (setf (Objects me) nil)
  (setf (Handler me) nil)
  (setf (ComponentFrom me) nil))

(define-handler drop (HandlerDataRect theactor)
  (declare (ignore-if-unused theActor))
  (call-next-method)
  (when (eventWindow)
    (setf (keytarget (eventWindow)) (keytarget (eventWindow)))))

(defun DoHandlerDrop (h obj)
  (unless (listp obj) (setf obj (list obj)))
  (let* ((hname (name h))
         (hasSource? (and h (or (PS::lfunHasSources (method-function h))
                                (MF::accessor-method-p h)))))
    (if hasSource?
      (progn
        (edithandlerobject h)
        (mapc #'(lambda (x) (edithandler (project x) hname x)) obj))
      (messagetouser "This handler could not be moved because it's sources are not available." :beep t))
    ))

(define-handler descriptionString (HandlerDataRect)
  (let ((obj (objects me)))
    (unless (listp obj) (setf obj (list obj)))
    (if (= (length obj) 1)
      (format nil "~a of ~a" 
              (name (Handler me))
              (objectstring (first obj) :project (project (first obj))))
      (format nil "~a of every item in ~a" 
              (name (Handler me))
              (objectstring obj :project (project (first obj))))
      )))

;;(descriptionString HandlerDataRect)


(new rectangle :project SK8 :objectname "FunctionDataRect"
     :properties '(Functions ComponentFrom))

(define-handler clearReferences (FunctionDataRect)
  (setf (Functions me) nil)
  (setf (ComponentFrom me) nil))

(pushnew FunctionDataRect *miscelaneous-things-to-be-cleared-by-UndoableSetLog*)

(define-handler drop (FunctionDataRect theactor)
  (call-next-method)
  (when (eventWindow)
    (setf (keytarget (eventWindow)) (keytarget (eventWindow)))))

(define-handler descriptionString (FunctionDataRect)
  (let ((funs (Functions me))
        (resString ""))
    (unless (listp funs) (setf funs (list funs)))
    (dolist (curfun funs)
      (setf resString
            (concatenate 'string 
                         resString
                         (format nil "~a(~a) " 
                                 (name curfun)
                                 (handlerargumentsstring curfun))))
      )
    resString))

;;(descriptionString FunctionDataRect)


(new rectangle :project SK8 :objectname "GlobalDataRect"
     :properties '(Globals ComponentFrom))

(define-handler clearReferences (GlobalDataRect)
  (setf (Globals me) nil)
  (setf (ComponentFrom me) nil))

(define-handler descriptionString (GlobalDataRect)
  (let ((funs (Globals me))
        (resString ""))
    (unless (listp funs) (setf funs (list funs)))
    (dolist (curfun funs)
      (setf resString
            (concatenate 'string 
                         resString
                         (format nil "~a " 
                                 (name curfun)
                                 )))
      )
    resString))
;;(descriptionString GlobalDataRect)

(pushnew GlobalDataRect *miscelaneous-things-to-be-cleared-by-UndoableSetLog*)

(define-handler value (globalDataRect)
  (let* ((globs (Globals me))
         (glob (if (listp globs) (first globs) globs))
         val)
    (when globs
      (setf val (symbol-value glob))
      val)))

(define-handler drop (globalDataRect theactor)
  (call-next-method)
  (when (eventWindow)
    (setf (keytarget (eventWindow)) (keytarget (eventWindow)))))

(new rectangle :project SK8 :objectname "ConstantDataRect"
     :properties '(Constants ComponentFrom))

(define-handler clearReferences (ConstantDataRect)
  (setf (Constants me) nil)
  (setf (ComponentFrom me) nil))

(define-handler descriptionString (ConstantDataRect)
  (let ((funs (Constants me))
        (resString ""))
    (unless (listp funs) (setf funs (list funs)))
    (dolist (curfun funs)
      (setf resString
            (concatenate 'string 
                         resString
                         (format nil "~a " 
                                 (name curfun)
                                 )))
      )
    resString))

;;(descriptionString ConstantDataRect)

(pushnew ConstantDataRect *miscelaneous-things-to-be-cleared-by-UndoableSetLog*)

(define-handler drop (constantDataRect theactor)
  (call-next-method)
  (when (eventWindow)
    (setf (keytarget (eventWindow)) (keytarget (eventWindow)))))

(define-handler value (constantDataRect)
  (let* ((consts (Constants me))
         (const (if (listp consts) (first consts) consts))
         val)
    (when consts
      (setf val (symbol-value const))
      val)))



;;; _______________________________ 
;;; Add Functionality to Script Edit Text...
;;; _______________________________ 

(defparameter *scriptedittext-droppees* (list ConstantDataRect GlobalDataRect FunctionDataRect HandlerDataRect PropertyDataRect ObjectDataRect))

(define-handler draggingMouseEnter (ScriptEditText actorDragged)
  (if (memq actordragged *scriptedittext-droppees*)
    (setf (highlight me) t)
    (call-next-method)))

(define-handler draggingMouseLeave (ScriptEditText actorDragged)
  (if (memq actordragged *scriptedittext-droppees*)
    (setf (highlight me) nil)
    (call-next-method)))

(define-handler draggingMouseWithin (ScriptEditText actorDragged)
  (if (memq actordragged *scriptedittext-droppees*)
    (let ((location (pointonwhichpart me (h mouse) (v mouse) :part 'sk8::character)))
      (setselection me location (1+ location))
      )
    (call-next-method)))

(define-handler dropped (ScriptEditText droppee)
  (if (memq droppee *scriptedittext-droppees*)
    (let ((location (pointonwhichpart me (h mouse) (v mouse) :part 'sk8::character))
          (theString (descriptionString droppee)))
      (setselection me location location)
      (ed-insert-with-undo (editdata me) theString location)
      (forceredraw me :forceRedraw t)
      )
    (call-next-method)))





;;--------------------------------------------------------------------------------------------
;;;UTILITY FUNCTIONS....
;;--------------------------------------------------------------------------------------------

(defun parentsProperties (obj)
  (let ((ps (parents obj)) (pprops nil))
    (dolist (i ps)
      (setf pprops (nconc (localproperties i) pprops)))
    (setf pprops (delete-duplicates (append (localProperties obj) pprops) :test #'eq))
    pprops))

(defun GraphicProperties (obj)
  (let ((props (list 'BoundsRect 'Size 'Location 'Fillcolor 'FrameSize 'FrameColor 'Origin
                     'AutoHighlight 'Text 'Textsize 'TextColor 'TextFont 'Layer 'Contents 'Container
                     'TextStyle 'TextOffset 'TextLocation 'Scale 'visible)))
    (if (inheritsfrom obj rectangle)  (setf props (append '(WindowTitle windowstyle) props)))
    (if (inheritsfrom obj actor) props nil)))

(defun GetProperties (ObjList DisplayStyle &key (ShowsPrivates nil))
  (let ((props nil)
        (obj (first objlist)))
    (if (not (memq DisplayStyle '(local parents graphic inherited all)))
      (setf DisplayStyle 'local))
    (when ObjList
      (setf props (case DisplayStyle
                    (all (properties obj))
                    (inherited (inheritedProperties obj))
                    (graphic (GraphicProperties obj))
                    (parents (parentsProperties obj))
                    (local (localProperties obj))))
      (when (> (length ObjList) 1)
        (dolist (i (rest ObjList))
          (setf props (nintersection props (case DisplayStyle
                                             (graphic (GraphicProperties i))
                                             (all (properties i))
                                             (inherited (inheritedProperties i))
                                             (parents (parentsProperties i))
                                             (local (localProperties i))))))))
    (unless ShowsPrivates (setf props (delete-if #'(lambda (x) (private obj :property x)) props)))
    props))

(defun parentsHandlers (obj)
  (let ((ps (parents obj)) (phands nil))
    (dolist (i ps)
      (setf phands (nconc (localHandlers i) phands)))
    (setf phands (nconc (localHandlers obj) phands)) ))

(defun GraphicHandlers (obj)
  (if (inheritsfrom obj actor) (localHandlers actor) nil))

(defun GetHandlers (ObjList DisplayStyle &key (ShowsPrivates nil))
  (let ((ourlist nil))
    (when ObjList
      (if (not (memq DisplayStyle '(local parents graphic inherited all)))
        (setf DisplayStyle 'local))
      (case DisplayStyle
        (local (dolist (i ObjList) (setf ourlist (nconc (localHandlers i) ourlist))))
        (inherited (dolist (i ObjList) (setf ourlist (nconc (InheritedHandlers i) ourlist))))
        (graphic (dolist (i ObjList) (setf ourlist (nconc (GraphicHandlers i) ourlist))))
        (parents (dolist (i ObjList) (setf ourlist (nconc (parentsHandlers i) ourlist))))
        (all (dolist (i ObjList) (setf ourlist (nconc (handlers i) ourlist)))))
      (setf ourlist (remove-duplicates ourlist))
      (unless ShowsPrivates (setf ourlist (delete-if #'(lambda (x) (private x)) ourlist)))
      ourlist)))

(defun GetPropsValue (theproperty objs proj &key (AsAString t))
  (let (val (ok t) res error?)
    (multiple-value-bind (val1 err?) 
                         (ignore-errors (setf val (funcall theproperty (first objs))))
      (setf error? err?))
    (when (and (not error?) (> (length objs) 1))
      (dolist (i (rest objs))
        (unless error?
          (multiple-value-bind (val1 err?) (ignore-errors (funcall theproperty i))
            (setf error? err?)
            (unless (equal val val1)
              (setf ok nil))
            ))
        ))
    (if error?
      (progn
        (messagetouser (concatenate 'string 
                                    "Error while getting the value of "
                                    (objectstring theproperty) 
                                    (if (= (length objs) 1)
                                      (concatenate 'string " on " (objectstring (first objs) :project (project (first objs))))
                                      (concatenate 'string " on one of the objects in " (objectstring objs :project (project (first objs)))))
                                    ".") :beep t)
        (setf res "***Error***"))
      (if ok 
        (setf res (if asastring 
                    (if (stringp val) ;;; if it's a string, we get the simpleobjectstring to preserve the niceness, but we add a " on either end to make it rereadable as a string.
                      (concatenate 'string (string #\") 
                                   (simpleobjectstring val :project proj)
                                   (string #\")) 
                      (objectstring val :project proj))
                    val))
        (setf res "Differing Values")))
    res))

(defun GetShortPropsValue (theproperty objs proj)
  (let (res)
    (if (eq theproperty 'knowndescendants)
      (if (> (length objs) 1)
        (if (every #'(lambda (x) (null (knownchildren x))) objs)
          (setf res (objectstring nil))
          (setf res "Differing Values"))
        (let ((c (knownchildren (first objs))))
          (if c
            (setf res (concatenate 'string (objectstring c :project proj) "..."))
            (setf res (objectstring c :project proj))
            )))
      (setf res (GetPropsValue theproperty objs proj :asastring t)))
    res))


;;NEEDS SIDNEYS NEW FUNCTION**************
(defun SetterExistsP (theProperty obj)
  t)

;;--------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------
;;; CORE COMPONENTS
;;--------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------------------
;;; BROWSER PAPER
;;--------------------------------------------------------------------------------------------

(new rectangle :objectname "BrowserPaper" :project SK8 :prototype t)

(addparent BrowserPaper BrowserComponent)

(setf (windowstyle BrowserPaper) 'documentWithZoom)

(new halo :objectname "BrowserPaperHighlighterHalo" :project SK8)
(setf (insetSize BrowserPaperhighlighterHalo) '(2 2))
(setf (fillcolor BrowserPaperhighlighterHalo) yellow)
(setf (container BrowserPaperhighlighterHalo) BrowserPaper)
(tagpart BrowserPaper BrowserPaperhighlighterHalo 'highlighterHalo)

(define-handler resized (BrowserPaper)
  (call-next-method)
  (withactorlocked (me)
    (moveoffstage (highlighterHalo me))
    (when (keytarget me) (activatetext (keytarget me)))
    ))

(resized BrowserPaper)

;;--------------------------------------------------------------------------------------------
;;; BROWSER Menubar
;;--------------------------------------------------------------------------------------------


(new sk8::menubar :objectname "BrowserMenuBar" :project SK8 :prototype t)

(define-handler Setup (browserMenuBar)
  (let (theval)
    (withActorLocked (me)
      ;; (mapcar #'dispose (menus me))
      (dolist (i (menus me))
        (setf (sk8::menubar i) nil))
      (dolist (i (deepcontents (container me)))
        (when (inheritsfrom i browsercomponent)
          (setf theval (menuprototype i))
          (unless (listp theval) (setf theval (list theval)))
          (dolist (j theval)
            (setf (sk8::menubar (new j :project (project me) :BrowserComponent i)) me)
            ))))))

;;--------------------------------------------------------------------------------------------
;;; INPUT COMPONENTS
;;--------------------------------------------------------------------------------------------

;;;---SK8Script Query Item---

(new rectangle :objectname "QueryField" :project SK8
     :prototype t
     :properties '(InputString OutputObjects))
(addparent QueryField BrowserComponent)

(setf (textsize queryfield) 9)
(setf (textfont queryfield) "geneva")

(addInputPort QueryField 'InputString :signature String)
(addOutputPort QueryField 'OutputObjects :signature Collection)


(new scripteditText :objectname "QueryFieldScriptEditText" :project SK8)
(setf (container QueryFieldScriptEditText) QueryField)
(setf (wrapping QueryFieldScriptEditText) 'word)
(setf (textsize QueryFieldScriptEditText) 9)
(tagpart QueryField QueryFieldScriptEditText 'TextField)
(define-handler activateText (QueryFieldScriptEditText)
  (unless (and (down mouse) (eq (actorathvcoordinates (eventh) (eventv)) me))
    (setselection me 0 -1))
  (call-next-method)
  (BrowserHighlight (container me)))
(define-handler activateText (QueryField)
  (setf (keytarget (sk8::window me)) (textfield me)))
(define-handler deactivateText (QueryFieldScriptEditText)
  (let ()
    (call-next-method)
    (BrowserUnHighlight (container me))))
(define-handler targetProject (QueryFieldScriptEditText)
  (targetProject (container me)))

(define-handler evaluate (QueryFieldScriptEditText &key ((:project proj)))
  (sk8-multival-bind (failure? errors locals globals res)
                     (translateScriptCommandOrExpr proj (text me))
    (declare (ignore locals globals))
    (if failure?
      (indicateScriptSyntaxError me errors)
      (scriptInputCompleted me res)))
  )

(define-handler scriptInputCompleted (QueryFieldScriptEditText translation)
  (let* ((temp1 (evaluateScriptTranslation translation))
         (result (if (and (is-a temp1 collection) (not (is-a temp1 aliasedcollection)))
                   (sk8::as temp1 list)
                   (list temp1))))
    (setf (outputobjects (container me)) result)
    (DoBookKeeping (container me))
    (setselection me 0 -1)))
(define-handler keydown (QueryFieldScriptEditText thekey)
  (unless (memq thekey '(#\uparrow #\downarrow #\backarrow #\forwardarrow 
                         #\pageup #\pagedown #\home #\end #\help))
    (when (outputobjects (container me)) (setf (outputobjects (container me)) nil)))
  (call-next-method))
(define-handler sk8::pasteClipboardToSelection (QueryFieldScriptEditText) 
  (when (outputobjects (container me)) (setf (outputobjects (container me)) nil))
  (call-next-method))
(define-handler sk8::cutSelectionToClipboard (QueryFieldScriptEditText) 
  (when (outputobjects (container me)) (setf (outputobjects (container me)) nil))
  (call-next-method))
(define-handler sk8::clearSelection (QueryFieldScriptEditText) 
  (when (outputobjects (container me)) (setf (outputobjects (container me)) nil))
  (call-next-method))

(new PickerMenu :objectname "QueryFieldHistoryMenu" :project SK8)
(setf (container QueryFieldHistoryMenu) QueryField)
(setf (text QueryFieldHistoryMenu) "")
(setf (maximumlength QueryFieldHistoryMenu) 11)
(tagpart QueryField QueryFieldHistoryMenu 'HistoryMenu)
(define-handler CreateTextDisplayItem (QueryFieldHistoryMenu theItem)
  (if (inheritsfrom theItem string) theItem (objectstring theItem :project (targetproject (container me)))))
(define-handler menuselect (QueryFieldHistoryMenu)
  (let ((item (selecteditem me)))
      (setf (items me) (cons item (delete item (items me))))
      (if (stringp item)
        (progn
          (setf (text (textfield (container me))) item)
          (evaluate (textfield (container me))))
        (progn
          (setf (text (textfield (container me))) (objectstring Item :project (targetproject (container me))))
          (setf (outputobjects (container me)) (if (listp Item) 
                                                 Item
                                                 (list Item)))
          )
        )
      (dobookkeeping (container me))
      (setselection (textfield (container me)) 0 -1)
      (setf (selecteditem me) nil)))
(define-handler mousedown (QueryFieldHistoryMenu)
  (setf (keytarget (sk8::window me)) (textfield (container me)))
  (call-next-method)
  )

(define-handler evaluate (QueryField)
  (evaluate (TextField me)))

(define-handler resized (QueryField)
  (call-next-method)
  (if (eq (KeyTarget (sk8::window me)) (textfield me))
    (browserHighlight me))
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (TextField me) 0 0 (- hh 24) vv)
    (setBoundsRect (HistoryMenu me) (- hh 25) 0 hh vv)))

(define-handler (setf InputString) (theval QueryField)
  (sk8dev::withlockedcursor animatedClock
    (sk8::setValue 'InputString me theval)
    (setf (text (TextField me)) theval)
    (setSelection (TextField me) 0 -1)
    (tickeventclock)
    (evaluate (TextField me))))

(define-handler DoBookKeeping (QueryField &key (Warnings nil) (history t))
  (let ((theItems (outputobjects me)))
    (if (not theItems)
      (when Warnings (messageToUser 
                      (format nil "No objects were found in project ~a satisfying the query!"
                              (objectString (targetProject me)))))
      (when history 
        (if (string-equal (objectstring (if (eql 1 (length theItems)) (first theItems) theItems) :project (targetproject me)) (text (textfield me)))
          (progn
            (setf (text (textfield me)) (objectstring (if (eql 1 (length theItems)) (first theItems) theItems) :project (targetproject me)))
            (addItem (historymenu me) (first theItems)))
          (addItem (historymenu me) (text (textfield me))))))))

(define-handler draggingMouseEnter ((textfield queryfield) actorDragged)
  (when (or (eq actorDragged ObjectDataRect) 
            (eq actorDragged Propertydatarect)
            (and (eq actorDragged handlerdatarect) (not (listp (name (handler handlerdatarect))))))
    (setf (highlight (container me)) t)))

(define-handler draggingMouseLeave ((textfield queryfield) actorDragged)
  (when (highlight (container me)) (setf (highlight (container me)) nil)))

(define-handler draggingMouseWithin ((textfield queryfield) actorDragged)
  )

;;;Fix problem with dropping properties of anonymous objects into the object editors field.
(define-handler dropped ((textfield queryfield) droppee)
  (withActorLocked (me)
    (draggingMouseLeave me droppee)
    (cond
     ((eq droppee ObjectDataRect)
      (bringToFront (sk8::window me))
      (setf (keytarget (sk8::window me)) me)
      (setf (text me) (objectstring (object objectdatarect) :project (targetproject (container me))))
      (setselection me 0 -1)
      (setf (outputobjects (container me)) (if (listp (object objectdatarect)) 
                                             (object objectdatarect)
                                             (list (object objectdatarect))))
      (dobookkeeping (container me)))
     ((eq droppee Propertydatarect)
      (let ((val (value propertydatarect)))
        (bringToFront (sk8::window me))
        (setf (keytarget (sk8::window me)) me)
        (if (and (= (length (objects droppee)) 1) (objectname (first (objects droppee))))
          (setf (inputstring (container me)) (concatenate 'string "the " 
                                                          (name (propertyname droppee))
                                                          " of "
                                                          (objectstring (first (objects droppee)) :project (targetproject (container me)))
                                                          ))
          (progn
            (setf (text me) (objectstring (if (listp val)  val (list val)) :project (targetproject (container me))))
            (setf (inputobjects (container me)) (if (listp val)  val (list val)))))
        
        ))
     ((and (eq droppee handlerdatarect) (not (listp (name (handler handlerdatarect)))))   ;;;*** NEED TO CHECK THE HANDLERS ARGUMENTS...
      (bringToFront (sk8::window me))
      (setf (keytarget (sk8::window me)) me)
      (setf (inputstring (container me)) (concatenate 'string "the " 
                                                      (name (name (handler droppee)))
                                                      " of "
                                                      (objectstring (first (objects droppee)) :project (targetproject (container me)))
                                                      ))
      (setselection me 0 -1)
      ))))

(setf (size queryfield) '(200 50))

;;;----------------Now a name field---------------
;;; a query field that changes it's text to what it is evalled to...

(new queryfield :objectname "NameField" :project sk8 :prototype t)

(define-handler deactivateText ((textfield NameField))
  (let ((oo (outputobjects (container me))))
    (setf (text me) (if oo (objectstring (if (= (length oo) 1) (first oo) oo) :project (targetproject (container me))) ""))
    (call-next-method)
    (BrowserUnHighlight (container me))))

(define-handler DoBookKeeping (NameField &key (warnings nil) (history t))
  (call-next-method me :warnings warnings :history nil)
  (let ((val (OutputObjects me)))
    (setf (text (TextField me)) (objectstring (if (eql 1 (length val)) (first val) val) :project (targetproject me)))
    (forceredraw (textfield me)) ;;;*** tell adam 
    (when (and history val)
      (addItem (historymenu me) (if (eql 1 (length val)) (first val) val)))
    val))



;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  GetFromUser Button
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new roundrect :objectname "GetFromUserButton" :project sk8
     :properties '(InputObjects InputProperty (enabled :value t)))
(setf (autoHighlight getfromuserbutton) t)
(setSize getfromuserbutton 27 13)
(setf (text getfromuserbutton) "...")

(new polygon :objectname "GFUButtonArrow" :project sk8)
(setf (fillcolor GFUButtonArrow) black)
(setf (framesize GFUButtonArrow) '(0 0))
(setf (container GFUButtonArrow) GetFromUserButton)
(setf (points GFUButtonArrow) (list 0 0 13 0 6 6))
;;(dolist (i (descendants GetFromUserButton)) (new GFUButtonArrow :project (project i) :container i))
(hide GFUButtonArrow)

(define-handler resized (GetFromUserButton)
  (setf (location (first (contents me)) :physical t) (location me :physical t)))
(resized GetFromUserButton)

(define-handler (setf InputObjects) (theValue GetFromUserButton)
  (sk8::setValue 'InputObjects me theValue)
  (setf (enabled me) (OKToGetValueFromUser theValue (inputproperty me))))

(define-handler (setf InputProperty) (theValue GetFromUserButton)
  (sk8::setValue 'InputProperty me theValue)
  (setf (enabled me) (OKToGetValueFromUser (InputObjects me) theValue))
  (if (enabled me) 
    (if (valuefromuserasamenu (InputObjects me) thevalue)
      (progn
        (setf (text me) "")
        (show (first (contents me)))
        )
      (progn
        (setf (text me) "...")
        (hide (first (contents me)))
        ))))
      
(define-handler (setf enabled) (theValue GetFromUserButton)
  (sk8::setValue 'enabled me theValue)
  (withactorlocked (me)
    (setf (fillcolor me) (if thevalue white lightgray))
    (setf (fillcolor (first (contents me))) (if thevalue black gray))
    (setf (framecolor me) (if thevalue black darkgray))
    (setf (textcolor me) (if thevalue black gray))))

(define-handler Highlight (GetFromUserButton)
  (call-next-method))
(define-handler (setf Highlight) (theValue GetFromUserButton)
  (when (enabled me)
    (call-next-method)))

(define-handler mousedown (Getfromuserbutton)
  (setf (Highlight me) t)
  (unwind-protect
    (if (enabled me) 
      (GetValueFromUser (inputobjects me) (inputproperty me) :relativeactor me))
    (setf (Highlight me) nil))
  )

(define-handler mouseenter (Getfromuserbutton)
  (call-next-method)
  (when (enabled me) 
    (setf (cursor stage) standardcursor)))

(setf (enabled getfromuserbutton) nil)

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  GetObjectField
;;;;---------------------------------------------------------------------------------------------------------------------------

(new rectangle :objectname "GetObjectField" :project SK8
     :prototype t
     :properties '((InputPrototype :inherit)
                   InputObject 
                   OutputObject))
(addparent GetObjectField BrowserComponent)

(setf (textsize GetObjectField) 9)
(setf (textfont GetObjectField) "geneva")

(addInputPort GetObjectField 'InputPrototype :signature Object)
(addInputPort GetObjectField 'InputObject :signature Object)
(addOutputPort GetObjectField 'OutputObject :signature Object)


(new rectangle :objectname "GetObjectFieldTearOffRect" :project SK8) 
(setf (container GetObjectFieldTearOffRect) GetObjectField)
(setf (framesize GetObjectFieldTearOffRect) '(2 2))
(setf (framecolor GetObjectFieldTearOffRect) uirectangleoutbevel)
(setf (fillcolor GetObjectFieldTearOffRect) metal2)
(setf (visible GetObjectFieldTearOffRect) nil)  ;;; set to nil!!!
(tagpart GetObjectField GetObjectFieldTearOffRect 'TearOffRect)
(define-handler mouseDown (GetObjectFieldTearOffRect)
  (let ((sels (OutputObject (container me))))
    (when sels
      (setf (boundsrect ObjectDataRect :physical t) (boundsrect (container me) :physical t))
      (setf (object ObjectDataRect) sels)
      (withcursor standardcursor
        (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
      )))

(new editText :objectname "GetObjectFieldSimpleText" :project SK8)
(setf (textsize GetObjectFieldSimpleText) 9)
(setf (container GetObjectFieldSimpleText) GetObjectField)
(tagpart GetObjectField GetObjectFieldSimpleText 'TextField)
(define-handler EnterInField (GetObjectFieldSimpleText)
  (Evaluate (container me)))
(define-handler ReturnInField (GetObjectFieldSimpleText)
  (Evaluate (container me)))
(define-handler activateText (GetObjectFieldSimpleText)
  (call-next-method)
  (BrowserHighlight (container me)))
(define-handler deactivateText (GetObjectFieldSimpleText)
  (call-next-method)
  (BrowserUnHighlight (container me)))

(new GetFromUserButton :objectname "GetObjectFieldGetFromUserButton" :project SK8)
(setf (container GetObjectFieldGetFromUserButton) GetObjectField)
(setf (enabled GetObjectFieldGetFromUserButton) nil)
(tagpart GetObjectField GetObjectFieldGetFromUserButton 'GetFromUserButton)
(define-handler mousedown (GetObjectFieldGetFromUserButton)
  (when (enabled me)
    (setf (Highlight me) t)
    (unwind-protect
      (let ((theval (getfromuser (InputPrototype (container me)) :project (targetproject (container me)))))
        (setf (Text (textfield (container me))) (objectstring theval :project (targetproject (container me))))
        (evaluate (container me))
        )
      (setf (Highlight me) nil))))

(define-handler evaluate (GetObjectField &key (Warnings nil))
  (sk8dev::withLockedCursor animatedClock
    (let* ((theProject (targetProject me))
           (expr (text (TextField me))))
      (sk8-multival-bind (failure? errors locals globals result)
                         (translateScriptCommandOrExpr theProject expr)
        (declare (ignore errors locals globals))
        (if failure?
          (when Warnings (messageToUser "Syntax error!"))
          (let* ((temp1 (withProject theProject (evaluateScriptTranslation result)))
                 (theItems temp1)  ;;;(filter-by-project temp2 theProject))
                 )
            (unless (inheritsFrom theitems (InputPrototype me)) (setf theitems nil))
            (when (not theItems)
              (when Warnings 
                (messageToUser 
                 (format nil "No objects were found in project ~a satisfying the query!"
                         (objectString theProject))))
              )
            (setf (outputobject me) theitems)
            (setf (text (textfield me)) (if theitems (objectstring theitems :project (targetproject me)) ""))
            ))))
    (setselection (TextField me) 0 -1)))

(define-handler resized (GetObjectField)
  (call-next-method)
  (if (eq (KeyTarget (sk8::window me)) (textfield me))
    (browserHighlight me))
  (sk8-multival-bind (hh vv) (size me)
    (let ((tor (min vv 20)))
      (setBoundsRect (tearoffrect me) 2 2 (- tor 2) (- tor 2))
      (if (visible (tearoffrect me))
        (setBoundsRect (TextField me) tor 0 (- hh 30) vv)
        (setBoundsRect (TextField me) 0 0 (- hh 30) vv))
      (setBoundsRect (GetFromUserButton me) (- hh 28) 2 (- hh 2) (- tor 2)))))

(define-handler (setf InputPrototype) (theval GetObjectField)
  (sk8::setValue 'InputPrototype me theval)
  (setf (enabled (getfromuserbutton me)) theval)
  (unless theval (setf (outputobject me) nil))
  )
(setf (inputprototype getobjectfield) object)
(hide (first (contents (getfromuserbutton getobjectfield))))

(define-handler (setf InputObject) (theval GetObjectField)
  (when (or (not theval) (inheritsfrom theval (inputprototype me)))
    (sk8::setValue 'InputObject me theval)
    (setf (text (TextField me)) (if theval (objectstring theval :project (targetproject me)) ""))
    (setSelection (TextField me) 0 -1)
    (setf (outputobject me) theval)
    (unless theval (setf (outputobject me) nil))
    ))

(define-handler draggingMouseEnter ((textfield GetObjectField) actorDragged)
  (when (or (and (eq actordragged ObjectDataRect) 
                 (inheritsfrom (object ObjectDataRect) (InputPrototype (container me))))
            (and (eq actordragged PropertyDataRect) 
                 (inheritsfrom (value PropertyDataRect) (InputPrototype (container me)))))
    (setf (highlight (container me)) t)))

(define-handler draggingMouseLeave ((textfield GetObjectField) actorDragged)
  (when (highlight (container me)) (setf (highlight (container me)) nil)))

(define-handler dropped (GetObjectField droppee)
  (withActorLocked (me)
    (draggingMouseLeave (textfield me) droppee)
    (cond
     ((and (eq droppee ObjectDataRect) 
           (inheritsfrom (object ObjectDataRect) (InputPrototype me)))
      (setf (keytarget (sk8::window me)) (textfield me))
      (setf (text (textfield me)) (objectstring (object droppee) :project (targetproject me)))
      (setselection (textfield me) 0 -1)
      (evaluate me)
      (bringToFront (sk8::window me))
      )
     ((and (eq droppee PropertyDataRect) 
           (inheritsfrom (value PropertyDataRect) (InputPrototype me)))
      (setf (keytarget (sk8::window me)) (textfield me))
      (setf (text (textfield me)) (objectstring (value PropertyDataRect) :project (targetproject me)))
      (setselection (textfield me) 0 -1)
      (evaluate me)
      (bringToFront (sk8::window me))
      )))
  )

(setf (size GetObjectField) '(200 50))



;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  Here are the pickers for objects...
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new BrowserComponent :objectname "MixinForObjectPickers" :project sk8
     :properties '(InputObjects OutputObjects))

(addInputPort MixinForObjectPickers 'InputObjects :signature Collection)
(addOutputPort MixinForObjectPickers 'OutputObjects :signature Collection)


(define-handler createTextDisplayItem (MixinForObjectPickers theItem)
  (if (and (is-a me hierarchicalpicker) (is-a theItem hierarchicalpickeritem)) (setf theItem (value theItem)))
  (objectstring theItem :project (targetproject me)))

(define-handler (setf inputObjects) (curInput MixinForObjectPickers)
  (sk8::setValue 'inputObjects me curInput)
  (unless curInput (setf (outputobjects me) nil))
  (sk8dev::withLockedCursor animatedClock
    (withActorLocked ((sk8::window me))
      (let ((oldSelection (selectedItems me)))
        (withActorLocked (me)
          (tickeventclock)
          (setf (items me) curInput)
          (if (and oldselection (memq (first oldselection) curInput))
            (setf (selectedItems me) (first oldselection))
            (setf (selectedItems me) (first curInput)))
          (tickeventclock)
          (selectioncompleted me))))))

(define-handler keydown (MixinForObjectPickers theChar)
  (cond
   ((eq theChar #\Delete)
    (let* ((sels (selecteditems me))
           (newsels (mapcar #'(lambda (x) (if (inheritsfrom x hierarchicalpickeritem) (value x) x)) sels))
           (newits (delete-if #'(lambda (x) (memq x newsels))
                              (mapcar #'(lambda (x) (if (inheritsfrom x hierarchicalpickeritem) (value x) x)) 
                                      (items me)))))
      (when sels
        (when (DisposeDialog newsels)
          (setf (inputobjects me) newits)
          ))))
   ((memq thechar '(#\escape #\Help)) 
    (let* ((sel (first (selecteditems me)))
           (id (and sel (if (is-a sel hierarchicalpickeritem) (value sel) sel))))
      (cond
       ((symbolp id)
        nil)
       ((and (listp id) (symbolp (first id)))
        (setf id (first id)))
       ((or (is-a id handler) (is-a id sk8::function))
        (setf id (name id)))
       (t
        (setf id (sk8::sk8_id id))))
      (when (symbolp id)
        (sk8-multival-bind (ll tt rr bb) (itemboundsrect me (if (is-a me picker) (first (selection* me)) (first (selection me)))
                                                         :physical t)
          (PopupBalloonForHelp id (+ ll (/ (- rr ll) 2)) (+ tt (/ (- bb tt) 2)))))))
   (t
    (call-next-method))))

(define-handler DoubleClick (MixinForObjectPickers)
  (let ((theHandler (first (selectedItems me))))
    (when (and theHandler (inheritsfrom theHandler handler))
      (EditHandlerObjectDialog theHandler))))

(define-handler selectioncompleted (MixinForObjectPickers) 
  (let ((sels (selecteditems me)))
    (withactorlocked ((sk8::window me))
      (when sels 
        (if (inheritsfrom (first sels) hierarchicalpickeritem) (setf sels (mapcar #'value sels))))
      (setf (OutputObjects me) sels))))

(new Menu :objectname "MenuForObjectPickers" :project sk8
     :properties '(BrowserComponent))
(setf (text MenuForObjectPickers) "Objects")
(define-handler update (MenuForObjectPickers)
  (let ((theval (and (BrowserComponent me) 
                     (inheritsfrom (BrowserComponent me) MixinForObjectPickers)
                     (selecteditems (BrowserComponent me)))))
    (dolist (i (menuitems me))
      (setf (enabled i) theval)))
  (call-next-method))
(new menuitem :objectname "MenuForObjectPickersNewMenuItem" :menu MenuForObjectPickers :project sk8)
(define-handler menuselect (MenuForObjectPickersNewMenuItem)
  (getNewFromUser object :locked nil :project (targetproject (browsercomponent (menu me)))))
(define-handler update (MenuForObjectPickersNewMenuItem)
  (if (eq (keytarget (sk8::window (browsercomponent (menu me)))) (browsercomponent (menu me))) 
    (setf (commandkey me) #\N)
    (setf (commandkey me) nil))
  (setf (text me) "New Object"))
(new menuitem :objectname "MenuForObjectPickersItemDispose" :menu MenuForObjectPickers :project sk8)
(define-handler menuselect (MenuForObjectPickersItemDispose)
  (setf (keytarget (sk8::window (browsercomponent (menu me)))) (browsercomponent (menu me)))
  (keydown (BrowserComponent (menu me)) #\delete))
(define-handler update (MenuForObjectPickersItemDispose)
  (let* ((allsels (selecteditems (BrowserComponent (menu me))))
         (sels (first allsels)))
    (when sels 
      (if (inheritsfrom sels hierarchicalpickeritem) (setf sels (value sels))))
    (setf (text me)
          (if allsels
            (concatenate 'string "Clear References to " 
                         (if (= (length allsels) 1)
                           (objectstring sels :project (targetproject (BrowserComponent (menu me))))
                           (concatenate 'string (objectstring (length allsels)) " Items")
                           ))
            "Clear References"))))

(define-handler menuprototype (MixinForObjectPickers)
  MenuForObjectPickers)

;;;OJO Should always be kept in synch with extendedMouseDown of valueeditorpicker...
(define-handler ExtendedMouseDown (MixinForObjectPickers)
  (let ((sels (selecteditems me)))
    (when sels
      (if (is-a (first sels) hierarchicalpickeritem) (setf sels (mapcar #'value sels)))
      (if (and (= (length sels) 1) (is-a sels handler))
        (progn
          (setf sels (first sels))
          (setf (boundsrect HandlerDataRect :physical t) (itemboundsrect me (first (if (inheritsfrom me picker) 
                                                                                   (selection* me)
                                                                                   (selection me))) :physical t))
          (setf (objects HandlerDataRect) (list (object sels)))
          (setf (handler HandlerDataRect) sels)
          (setf (ComponentFrom HandlerDataRect) me)
          (withcursor standardcursor
            (drag HandlerDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
            )
          t)
        (progn
          (if (= (length sels) 1) (setf sels (first sels)))
          (setf (boundsrect ObjectDataRect :physical t) (selectedItemsBoundsRect me))
          (setf (object ObjectDataRect) sels)
          (setf (ComponentFrom ObjectDataRect) me)
          (withcursor standardcursor
            (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
            )
          t)
        ))))

(define-handler draggingMouseEnter (MixinForObjectPickers actorDragged)
  (when (and (items me)
             (or (eq actordragged PropertyDataRect)
                 (eq actordragged HandlerDataRect))
             (neq (componentfrom actordragged) me))
    (setf (highlight me) t)))

(define-handler draggingMouseLeave (MixinForObjectPickers actorDragged)
  (when (highlight me) (setf (highlight me) nil)))

(define-handler draggingMouseWithin (MixinForObjectPickers actorDragged)
  (when (and (or (eq actordragged PropertyDataRect)
                 (eq actordragged HandlerDataRect))
             (neq (componentfrom actordragged) me))
    (flashitem me)))

(define-handler dropped (MixinForObjectPickers droppee)
  (withActorLocked (me)
    (draggingMouseLeave me droppee)
    (when (and (items me)
               (or (eq droppee PropertyDataRect)
                   (eq droppee HandlerDataRect))
               (neq (componentfrom droppee) me))
      (let ((theguy (getitemposition me))
            (pname (propertyname propertydatarect))
            (hand (handler HandlerDataRect))
            obj)
        (when theguy
          (setf obj (element me theguy))
          (if (inheritsfrom obj hierarchicalpickeritem) (setf obj (value obj)))
          (bringToFront (sk8::window me))
          (setf (keytarget (sk8::window me)) me)
          (if (eq droppee PropertyDataRect)
            (doPropertyDrop pname obj)
            (DoHandlerDrop hand obj))
          (setf (outputobjects me) (outputobjects me))
          (setf (selecteditems me) (list obj))
          )))
    ))

(new picker :objectname "ObjectPicker" :project sk8)
(addparent ObjectPicker MixinForObjectPickers)

(setf (autotab ObjectPicker) t)
(setf (alphabeticalDisplay ObjectPicker) nil)
(setf (selectionStyle ObjectPicker) 'discontiguous)

(new textlist :objectname "ObjectList" :project sk8 :prototype t)
(setf (pickerprototype ObjectList) ObjectPicker)
(setf (text (titlebar ObjectList)) "Objects")

;;;;------------------------------------------------------------------------------
;;;;  Now the hierarchical picker...

(new hierarchicalpicker :objectname "HierarchicalObjectPicker" :project SK8
     :properties '(filterProject
                   Relation
                   SwatchesShown
                   ))
(addparent HierarchicalObjectPicker MixinForObjectPickers)

(define-handler arrowsize (HierarchicalObjectPicker)
  (if (relation me)
    (getvalue 'arrowsize me)
    '(0 0)))
(define-handler iconsize (HierarchicalObjectPicker)
  (if (SwatchesShown me)
    (getvalue 'iconsize me)
    nil))

(define-handler createSwatch (HierarchicalObjectPicker theItem)
  (declare (ignore theItem))
  nil)

(define-handler GenerateItems (HierarchicalObjectPicker ItemList)
  (let (newlist 
        (SwatchesShown (SwatchesShown me))
        (relation (relation me))
        (p (project me))
        )
    (dolist (i ItemList)
      (push (new hierarchicalpickeritem 
                 :project p 
                 :value i
                 :state (if (and relation (memq relation (properties i)))
                          'closed 
                          nil)
                 :swatch (if swatchesshown (createSwatch me i) nil))
            newlist)
      )
    (setf newlist (nreverse newlist))
    newlist))

(define-handler createArrow (HierarchicalObjectPicker theItem thestring theposition)
  (declare (ignore thestring theposition))
  (case (state theitem)
    (closed   (let* ((filt (filterproject me))
                      (reqprojs (and filt (cons filt (requiredprojects filt)))))
                 (if (delete-if-not #'(lambda (x) (or (not reqprojs) (memq (project x) reqprojs))) (funcall (Relation me) (value TheItem)))
                   FinderClosedArrow (fillcolor me))))
    (sk8::open (let* ((filt (filterproject me))
                    (reqprojs (and filt (cons filt (requiredprojects filt)))))
               (if (delete-if-not #'(lambda (x) (or (not reqprojs) (memq (project x) reqprojs))) (funcall (Relation me) (value TheItem))) 
                 FinderOpenArrow (fillcolor me))))
    (otherwise (and (relation me) (fillcolor me)))))

(define-handler ItemContents (HierarchicalObjectPicker TheItem)
  (when (Relation me)
    (let* ((res (funcall (Relation me) (value TheItem)))
           (filt (filterproject me))
           (reqprojs (and filt (cons filt (requiredprojects filt)))))
      (if filt (setf res (remove-if-not #'(lambda (x) (memq (project x) reqprojs)) res)))
      (generateItems me res))))

(define-handler (setf inputobjects)  (theobjs HierarchicalObjectPicker)
  (let ((its (items me)))
    (call-next-method (generateItems me theobjs) me)
    ;; (mapcar #'(lambda (x) (if (inheritsfrom x hierarchicalpickeritem) (dispose x))) its)
    (sk8::setValue 'inputObjects me theobjs)))

(define-handler (setf Relation) :after (therelation HierarchicalObjectPicker)
                (withactorlocked (me)
                  ;; (mapcar #'dispose (items me))
                  (setf (inputobjects me) (inputobjects me))))

(new MenuForObjectPickers :objectname "MenuForHierarchicalObjectPickers" :project sk8)
(new menuitem :objectname "MHOPSpacer1" 
     :text "-"
     :menu MenuForHierarchicalObjectPickers
     :enabled nil
     :project sk8)
(new menuitem :objectname "MHOPRelation" :project sk8 
     :properties '((Relation :inherit)))
(define-handler update (MHOPRelation)
  (setf (checkmark me) (eq (relation (browsercomponent (menu me))) (relation me))))
(define-handler menuselect (MHOPRelation)
  (setf (relation (browsercomponent (menu me))) (relation me)))
(new MHOPRelation :objectname "MHOPRelationnil" 
     :text "Set Relation to None"
     :menu MenuForHierarchicalObjectPickers
     :relation nil
     :project sk8)
(new MHOPRelation :objectname "MHOPRelationChildren" 
     :text "Set Relation to KnownChildren"
     :menu MenuForHierarchicalObjectPickers
     :relation 'KnownChildren
     :project sk8)
(new MHOPRelation :objectname "MHOPRelationContents" 
     :text "Set Relation to Contents"
     :menu MenuForHierarchicalObjectPickers
     :relation 'Contents
     :project sk8)
(new menuitem :objectname "MHOPSpacer2" 
     :text "-"
     :menu MenuForHierarchicalObjectPickers
     :enabled nil
     :project sk8)
(new menuitem :objectname "MHOPSwatchesShown" :project sk8 :menu MenuForHierarchicalObjectPickers)
(define-handler update (MHOPSwatchesShown)
  (if (SwatchesShown (browsercomponent (menu me)))
    (setf (text me) "Hide Swatches")
    (setf (text me) "Show Swatches")
    ))
(define-handler menuselect (MHOPSwatchesShown)
  (setf (SwatchesShown (browsercomponent (menu me))) 
        (not (SwatchesShown (browsercomponent (menu me))))))

(define-handler menuprototype (HierarchicalObjectPicker)
  MenuForHierarchicalObjectPickers)

                
(new textlist :objectname "HierarchicalObjectList" :project sk8 :prototype t)
(setf (pickerprototype HierarchicalObjectList) HierarchicalObjectPicker)
(setf (text (titlebar HierarchicalObjectList)) "Objects")

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  And a subclass, the object pile...
;;;;---------------------------------------------------------------------------------------------------------------------------

(new MixinForObjectPickers :objectname "MixinForObjectPiles" :project sk8
     :properties '((Editing :value t)))

(setf (editing MixinForObjectPiles) t)

(define-handler draggingMouseEnter (MixinForObjectPiles actorDragged)
  (when (editing me)
    (if (or (eq actordragged Functiondatarect) 
            (eq actordragged Handlerdatarect)
            (eq actordragged ObjectDataRect))
      (setf (highlight me) t)
      (call-next-method))))

(define-handler draggingMouseWithin (MixinForObjectPiles actorDragged)
  (when (editing me)
    (if (or (eq actordragged Functiondatarect) 
            (eq actordragged Handlerdatarect)
            (eq actordragged ObjectDataRect))
      (flashline me)
      (call-next-method))))

(define-handler dropped (MixinForObjectPiles droppee)
  (when (editing me)
    (withActorLocked (me)
      (draggingMouseLeave me droppee)
      (cond
       ((or (eq droppee Functiondatarect) 
            (eq droppee Handlerdatarect)
            (eq droppee ObjectDataRect))
        (let ((theguy (getlineposition me))
              (obj (cond
                    ((eq droppee Handlerdatarect) (handler Handlerdatarect))
                    ((eq droppee ObjectDataRect) (object ObjectDataRect))
                    ((eq droppee Functiondatarect) (functions functionDataRect))
                    ))
              (its (copy-list (items me)))
              NewLayer AlreadyHere)
          (when (is-a me hierarchicalpicker) (setf its (mapcar #'value its)))
          (when theguy
            (bringToFront (sk8::window me))
            (setf (keytarget (sk8::window me)) me)
            (unless (listp obj) (setf obj (list obj)))
            (setf NewLayer (if (inheritsfrom me tablepicker) (second theguy) theguy))
            (dolist (i obj)
              (setf AlreadyHere (position i its :test #'eq))
              (when AlreadyHere
                (setf its (delete i its :test #'eq))
                (if (< AlreadyHere newlayer)
                  (decf newlayer)))
              (setf its (gs:moveToPosition i (cons i its) newlayer)))
            (setf (inputobjects me) its)
            (setf (selecteditems me) obj)
            (selectioncompleted me)
            )))
       (t
        (call-next-method))))))

(define-handler keydown (MixinForObjectPiles theChar)
  (if (eq theChar #\Delete)
    (when (and (items me)
               (editing me)
               (first (selecteditems me)))
      (let ((sels (selecteditems me))
            (xx (max 0 (first (if (inheritsfrom me picker) 
                              (selection* me)
                              (selection me))))))
        (withactorlocked (me)
          (setf (items me) (delete-if #'(lambda (x) (memq x sels)) (items me)))
          (when (and (items me) (is-a me picker))
            (setf xx (min xx (1- (length (items me)))))
            (setf (selection me) (list xx xx))))))
    (call-next-method)))

(define-handler (setf inputObjects) (curInput MixinForObjectPiles)
  (call-next-method (remove-if #'null curinput)
                    me)
  )

(new picker :objectname "ObjectPilePicker" :project sk8)
(addparent ObjectPilePicker MixinForObjectPiles)

(setf (autotab ObjectPilePicker) t)
(setf (editing ObjectPilePicker) t)
(setf (alphabeticalDisplay ObjectPilePicker) nil)
(setf (selectionStyle ObjectPilePicker) 'discontiguous)

(new textlist :objectname "ObjectPile" :project sk8 :prototype t)
(setf (pickerprototype ObjectPile) ObjectPilePicker)
(setf (text (titlebar ObjectPile)) "Object Pile")
(setf (editing (picker objectpile)) t)

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  NOW are the Pickers for Handlers and Properties...
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
(new BrowserComponent :objectname "MixinForPropertyHandlerPickers" :project sk8
     :properties '(InputObjects OutputObjects OutputProperty OutputHandler showsProperties ShowsPrivates
                   showsHandlers (View  :value 'local)))

(setf (showsProperties MixinForPropertyHandlerPickers) t)
(setf (showsHandlers MixinForPropertyHandlerPickers) t)
(setf (View MixinForPropertyHandlerPickers) 'local)

(addInputPort MixinForPropertyHandlerPickers 'InputObjects :signature Collection)
(addOutputPort MixinForPropertyHandlerPickers 'OutputObjects :signature Collection)
(addOutputPort MixinForPropertyHandlerPickers 'OutputProperty :signature Symbol)
(addOutputPort MixinForPropertyHandlerPickers 'OutputHandler :signature Handler)


(define-handler keydown (MixinForPropertyHandlerPickers theChar)
  (cond 
   ((or (eq theChar #\Return) (eq theChar #\Enter))
    (doubleclick me))
   ((memq thechar '(#\escape #\Help)) 
    (let* ((sel (first (selecteditems me)))
           (id (and sel 
                    (if (symbolp sel)
                      sel
                      (name sel)))))
      (when (symbolp id)
        (sk8-multival-bind (ll tt rr bb) (itemboundsrect me (if (is-a me picker) (first (selection* me)) (first (selection me)))
                                                         :physical t)
          (PopupBalloonForHelp id (+ ll (/ (- rr ll) 2)) (+ tt (/ (- bb tt) 2)))))))
   ((eq theChar #\Delete)
    (when (and (inputobjects me) 
               (first (selecteditems me)))
      (if (symbolp (first (selecteditems me)))
        (when (RemovePropertyDialog (first (selecteditems me)) (inputobjects me) )
          (setf (inputobjects me) (inputobjects me)))
        (when (RemoveHandlerDialog (first (selecteditems me)) )
          (setf (inputobjects me) (inputobjects me))))))
   (t
    (call-next-method))))

(define-handler (setf view) :after (theval MixinForPropertyHandlerPickers)
  (setf (inputobjects me) (inputobjects me)))



;;;;;----MENU STUFF----
(new Menu :objectname "MenuForPropertyPickers" :project sk8
     :properties '(BrowserComponent))
(setf (text MenuForPropertyPickers) "Properties")
(new menuitem :objectname "PropertyPickerNewMenuItem" :menu MenuForPropertyPickers :project sk8)
(setf (text PropertyPickerNewMenuItem) "New Property...")
(define-handler menuselect (PropertyPickerNewMenuItem)
  (let* ((pp (BrowserComponent (menu me)))
         thenewthing theitems)
    (withactorlocked (pp)
      (setf thenewthing (AddPropertyDialog (inputobjects pp)))
      (setf theitems (setf (inputobjects pp) (inputobjects pp)))
      (if (memq thenewthing theitems)
        (setf (selectedItems pp) (list thenewthing))
        (if (arrayp theitems)
          (setf (selection pp) (and (> (rows pp) 0) '(0 0)))
          (setf (selectedItems pp) (first theitems))))
      (setf (keytarget (sk8::window pp)) pp))))
(define-handler update (PropertyPickerNewMenuItem)
  (if (eq (keytarget (sk8::window (browsercomponent (menu me)))) (browsercomponent (menu me))) 
    (setf (commandkey me) #\N)
    (setf (commandkey me) nil))
  (setf (enabled me) (inputobjects (BrowserComponent (menu me)))))
(new menuitem :objectname "PropertyPickerItemDispose" :menu MenuForPropertyPickers :project sk8)
(define-handler menuselect (PropertyPickerItemDispose)
  (let ((pp (BrowserComponent (menu me))))
    (setf (keytarget (sk8::window pp)) pp)
    (keydown pp #\delete)))
(define-handler update (PropertyPickerItemDispose)
  (let* ((pp (BrowserComponent (menu me)))
         (theguy (first (selecteditems pp))))
    (setf (enabled me) (and (inputobjects pp) (symbolp theguy)))
    (setf (text me)
          (if (enabled me)
            (concatenate 'string "Remove " (name theguy))
            "Remove Property"))))
(new menuitem :objectname "PropertyPickerShowPrivate" :menu MenuForPropertyPickers :project sk8)
(setf (text PropertyPickerShowPrivate) "Private Properties")
(define-handler menuselect (SK8::PropertyPickerShowPrivate)
  (let ((pp (BrowserComponent (menu me))))
    (setf (keytarget (sk8::window pp)) pp)
    (setf (showsPrivates pp) (not (showsPrivates pp)))
    (setf (inputobjects pp) (inputobjects pp))
    ))
(define-handler update (SK8::PropertyPickerShowPrivate)
  (let ((pp (BrowserComponent (menu me))))
    (setf (checkmark me) (showsPrivates pp))
    ))
(new menuitem :objectname "PropertyPickerPortMenuItem" :menu MenuForPropertyPickers :project sk8)
(define-handler menuselect (PropertyPickerPortMenuItem)
  (let* ((pp (BrowserComponent (menu me)))
         (theguy (first (selecteditems pp)))
         (theobjs (inputobjects pp)))
    (setf (keytarget (sk8::window pp)) pp)
    (addportdialog theguy theobjs)))
(define-handler update (PropertyPickerPortMenuItem)
  (let* ((pp (BrowserComponent (menu me)))
         (theguy (first (selecteditems pp)))
         (theobj (first (inputobjects pp))))
    (setf (enabled me) (and theobj (symbolp theguy) (SetterExistsP theguy theobj)))
    (setf (text me)
          (if (enabled me)
            (concatenate 'string "Add Port to " (name theguy))
            "Add Port"))))
(new menuitem :objectname "PropertyPickerSpacerMenuItem" :menu MenuForPropertyPickers :text "-" :enabled nil :project sk8)
(new menuitem :objectname "PropertyPickerViewTypeMenuItem" :project sk8
     :properties '((ViewType :inherit)))
(define-handler menuselect (PropertyPickerViewTypeMenuItem)
  (let* ((mycomponent (BrowserComponent (menu me)))
        (viewtype (viewtype me))
        (props (showsproperties mycomponent))
        (hands (showshandlers mycomponent)))
    (setf (view mycomponent) viewType)
    (setf (keytarget (sk8::window mycomponent)) mycomponent)
   (when (and (container mycomponent) (is-a (container mycomponent) textlist))
      (setf (title (container mycomponent)) 
            (concatenate 'string (case viewtype
                                   (all "All ")
                                   (inherited "Inherited ")
                                   (parents "Parent's ")
                                   (local "Local ")
                                   (graphic "Graphic "))
                         (cond
                          ((and props hands) "Properties and Handlers")
                          (props "Properties")
                          (hands "Handlers")
                          (t ""))))
      )
    ))
(new PropertyPickerViewTypeMenuItem :menu MenuForPropertyPickers :objectname "PropertyPickerViewTypeMenuItemAll" :viewType 'all :project sk8 :text "Show All")
(new PropertyPickerViewTypeMenuItem :menu MenuForPropertyPickers :objectname "PropertyPickerViewTypeMenuItemInherited" :viewType 'inherited :project sk8 :text "Show Inherited")
(new PropertyPickerViewTypeMenuItem :menu MenuForPropertyPickers :objectname "PropertyPickerViewTypeMenuItemParents" :viewType 'parents :project sk8 :text "Show Parent's")
(new PropertyPickerViewTypeMenuItem :menu MenuForPropertyPickers :objectname "PropertyPickerViewTypeMenuItemLocal" :viewType 'local :project sk8 :text "Show Local")
(new PropertyPickerViewTypeMenuItem :menu MenuForPropertyPickers :objectname "PropertyPickerViewTypeMenuItemGraphic" :viewType 'graphic :project sk8 :text "Show Graphic")
(define-handler update (MenuForPropertyPickers)
  (let ((curstate (view (BrowserComponent me)))
        (theval (and (BrowserComponent me) 
                     (inheritsfrom (BrowserComponent me) MixinForPropertyHandlerPickers))))
    (dolist (i (menuitems me))
      (setf (enabled i) theval)
      (when (inheritsfrom i PropertyPickerViewTypeMenuItem)
        (if (eq curstate (viewtype i))
          (setf (checkmark i) t)
          (setf (checkmark i) nil))
        (setf (enabled i) t))))
  (call-next-method))

(new Menu :objectname "MenuForHandlerPickers" :project sk8
     :properties '(BrowserComponent))
(setf (text MenuForHandlerPickers) "Handlers")
(new menuitem :objectname "HandlerPickerNewMenuItem" :menu MenuForHandlerPickers :project sk8)
(setf (text HandlerPickerNewMenuItem) "New Handler...")
(define-handler menuselect (HandlerPickerNewMenuItem)
  (let* ((pp (BrowserComponent (menu me)))
         thenewthing theitems)
    (withactorlocked (pp)
      (setf thenewthing (EditHandlerDialog (inputobjects pp)))
      (setf theitems (setf (inputobjects pp) (inputobjects pp)))
      (setf (keytarget (sk8::window pp)) pp)
      (if (memq thenewthing theitems)
        (setf (selectedItems pp) (list thenewthing))
        (if (arrayp theitems)
          (setf (selection pp) (and (> (rows pp) 0) '(0 0)))
          (setf (selectedItems pp) (first theitems))))
      (setf (keytarget (sk8::window pp)) pp))))
(define-handler update (HandlerPickerNewMenuItem)
  (if (eq (keytarget (sk8::window (browsercomponent (menu me)))) (browsercomponent (menu me))) 
    (setf (commandkey me) #\N)
    (setf (commandkey me) nil))
  (setf (enabled me) (inputobjects (BrowserComponent (menu me)))))
(new menuitem :objectname "HandlerPickerItemDispose" :menu MenuForHandlerPickers :project sk8)
(define-handler menuselect (HandlerPickerItemDispose)
  (let ((pp (BrowserComponent (menu me))))
    (setf (keytarget (sk8::window pp)) pp)
    (keydown pp #\delete)))
(define-handler update (HandlerPickerItemDispose)
  (let* ((pp (BrowserComponent (menu me)))
         (theguy (first (selecteditems pp))))
    (setf (enabled me) (and (inputobjects pp) (not (symbolp theguy))))
    (setf (text me)
          (if theguy
            (concatenate 'string "Remove " (if (listp (name theguy))
                                             (concatenate 'string "set " (name (second (name theguy))))
                                             (name (name theguy))))
            "Remove Handler"))))
(new menuitem :objectname "HandlerPickerShowPrivate" :menu MenuForHandlerPickers :project sk8)
(setf (text HandlerPickerShowPrivate) "Private Handlers")
(define-handler menuselect (HandlerPickerShowPrivate)
  (let ((pp (BrowserComponent (menu me))))
    (setf (showsPrivates pp) (not (showsPrivates pp)))
    (setf (keytarget (sk8::window pp)) pp)
    (setf (inputobjects pp) (inputobjects pp))
    ))
(define-handler update (HandlerPickerShowPrivate)
  (let ((pp (BrowserComponent (menu me))))
    (setf (checkmark me) (showsPrivates pp)) 
    ))

(new menuitem :objectname "HandlerPickerSpacerMenuItem" :menu MenuForHandlerPickers :text "-" :enabled nil :project sk8)
(new menuitem :objectname "HandlerPickerViewTypeMenuItem" :project sk8
     :properties '((ViewType :inherit)))
(define-handler menuselect (HandlerPickerViewTypeMenuItem)  ;;;;;**** This code is duplicated.  These menus should be consolidated
  (let* ((mycomponent (BrowserComponent (menu me)))
         (viewtype (viewtype me))
         (props (showsproperties mycomponent))
         (hands (showshandlers mycomponent)))
    (setf (view mycomponent) viewType)
    (setf (keytarget (sk8::window mycomponent)) mycomponent)
    (when (and (container mycomponent) (is-a (container mycomponent) textlist))
      (setf (title (container mycomponent)) 
            (concatenate 'string (case viewtype
                                   (all "All ")
                                   (inherited "Inherited ")
                                   (parents "Parent's ")
                                   (local "Local ")
                                   (graphic "Graphic "))
                         (cond
                          ((and props hands) "Properties and Handlers")
                          (props "Properties")
                          (hands "Handlers")
                          (t ""))))
      )
    ))
(new HandlerPickerViewTypeMenuItem :menu MenuForHandlerPickers :objectname "HandlerPickerViewTypeMenuItemAll" :viewType 'all :project sk8 :text "Show All")
(new HandlerPickerViewTypeMenuItem :menu MenuForHandlerPickers :objectname "HandlerPickerViewTypeMenuItemInherited" :viewType 'inherited :project sk8 :text "Show Inherited")
(new HandlerPickerViewTypeMenuItem :menu MenuForHandlerPickers :objectname "HandlerPickerViewTypeMenuItemParents" :viewType 'parents :project sk8 :text "Show Parent's")
(new HandlerPickerViewTypeMenuItem :menu MenuForHandlerPickers :objectname "HandlerPickerViewTypeMenuItemLocal" :viewType 'local :project sk8 :text "Show Local")
(new HandlerPickerViewTypeMenuItem :menu MenuForHandlerPickers :objectname "HandlerPickerViewTypeMenuItemGraphic" :viewType 'graphic :project sk8 :text "Show Graphic")
(define-handler update (MenuForHandlerPickers)
  (let ((curstate (view (BrowserComponent me)))
        (theval (and (BrowserComponent me) 
                     (inheritsfrom (BrowserComponent me) MixinForPropertyHandlerPickers))))
    (dolist (i (menuitems me))
      (setf (enabled i) theval)
      (when (inheritsfrom i HandlerPickerViewTypeMenuItem)
        (if (eq curstate (viewtype i))
          (setf (checkmark i) t)
          (setf (checkmark i) nil))
        (setf (enabled i) t))))
  (call-next-method))

(define-handler menuprototype (MixinForPropertyHandlerPickers)
  (let (theval)
    (if (showsproperties me) (push MenuForPropertyPickers theval))
    (if (showsHandlers me) (push MenuForHandlerPickers theval))
    theval))

(define-handler (setf InputObjects) (theval MixinForPropertyHandlerPickers)
  (sk8dev::withLockedCursor animatedClock
    (withActorLocked ((sk8::window me))
      (let* ((theItems nil)
             (itms (items me))
             (curInput theval)
             (theType (View me))
             (oldSelection (and itms (selectedItems me)))
             (sameAsBefore (equal theval (sk8::getvalue 'inputobjects me)))
             (oldscroll (verticalscroll me))
             )
        (sk8::setValue 'inputObjects me theval)
       (tickeventclock)
        (when (showsProperties me)
          (setf theItems (append theItems (sk8::GetProperties curInput theType :showsprivates (showsprivates me)))))
        (tickeventclock)
        (when (showsHandlers me)
          (setf theItems (append theItems (sk8::GetHandlers curInput theType :showsprivates (showsprivates me)))))
        (setf (outputobjects me) curinput)  ;;;For now objects are shot out first, then props or handlers...
        (withActorLocked (me)
          (tickeventclock)
          (setf (items me) theItems)
          (tickeventclock)
        ;;;; (setf (styles me) (mapcar #'(lambda (x) (createTextItemStyle me x nil nil)) (items me)))  ;;;TOO SLOW!!!!
          (if theItems
            (if (and oldselection (memq (first oldselection) theitems))
              (setf (selectedItems me) (list (first oldselection)))
              (if (arrayp (items me))
                (setf (selection me) (and (> (rows me) 0) '(0 0)))
                (setf (selectedItems me) (first (items me)))))
            ;;(setf (selecteditems me) nil)
            )
          (if (and sameAsBefore oldscroll) (setf (Verticalscroll me) (gs:range 0 oldscroll (if (arrayp (items me)) (rows me) (length (items me))))))
          (tickeventclock)
          (selectioncompleted me))
        theitems))))

(define-handler selectioncompleted (MixinForPropertyHandlerPickers) 
  (withactorlocked ((sk8::window me))
    (let ((val (first (selecteditems me))))
      (if (symbolp val)
        (progn
          (setf (OutputProperty me) val)
          (setf (OutputHandler me) nil))
        (progn
          (setf (OutputProperty me) nil)
          (setf (OutputHandler me) val))))))

(define-handler createTextDisplayItem (MixinForPropertyHandlerPickers theitem)
  (tickeventclock)
  (let ((ost (objectstring theitem :project (targetproject me))))
    (if (symbolp theitem)
      (name theitem)  ;;; in sk8script this would be "theitem as a string"
      ;;;; if we only wanted the method name and not the object as well, this could be:  
      ;;;; (name (method-name handler))
      (delete-if #'(lambda (x) (or (string= x "[") (string= x "]"))) 
                 ost))))

(define-handler createTextItemStyle (MixinForPropertyHandlerPickers targetItem theString position)
  (declare (ignore theString position))
  (let ((obj (first (inputobjects me)))
        thestyle)
    (sk8-multivals nil nil 
                   (progn
                     (if (symbolp targetItem)
                       (progn
                         (setf thestyle (if (SetterExistsP targetItem obj) nil '(italic)))
                         (when (localproperty obj targetItem) (push 'bold thestyle))
                         )
                       (when (memq (object targetItem) (inputobjects me)) (setf thestyle '(bold))))
                     (unless thestyle (setf thestyle '(plain)))
                     thestyle)
                   nil)))

(define-handler DoubleClick (MixinForPropertyHandlerPickers)
  (let ((theHandler (first (selectedItems me))))
    (when (and theHandler (not (symbolp theHandler)))
      (EditHandlerObjectDialog theHandler :obj (first (inputobjects me))))))

(define-handler ExtendedMouseDown (MixinForPropertyHandlerPickers)
  (let ((theitem (first (if (inheritsfrom me picker) 
                        (selection* me)
                        (selection me)))))
    (when theitem
      (if (symbolp (first (selecteditems me)))
        (progn
          (setf (boundsrect PropertyDataRect :physical t) (itemboundsrect me theitem :physical t))
          (setf (objects PropertyDataRect) (inputobjects me))
          (setf (propertyname PropertyDataRect) (first (selecteditems me)))
          (setf (ComponentFrom PropertyDataRect) me)
          (withcursor standardcursor
            (drag PropertyDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
          t)
        (progn
          (setf (boundsrect HandlerDataRect :physical t) (itemboundsrect me theitem :physical t))
          (setf (objects HandlerDataRect) (inputobjects me))
          (setf (handler HandlerDataRect) (first (selecteditems me)))
          (setf (ComponentFrom HandlerDataRect) me)
          (withcursor standardcursor
            (drag HandlerDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
          t)))))

(define-handler draggingMouseEnter (MixinForPropertyHandlerPickers actorDragged)
  (when (and (inputobjects me)
             (or (and (showsproperties me) (eq actordragged PropertyDataRect))
                 (and (showshandlers me) (eq actordragged HandlerDataRect)))
             (neq (componentfrom actordragged) me))
    (setf (highlight me) t)))

(define-handler draggingMouseLeave (MixinForPropertyHandlerPickers actorDragged)
  (when (highlight me) (setf (highlight me) nil)))

(define-handler draggingMouseWithin (MixinForPropertyHandlerPickers actorDragged)
  (when (and (eq actordragged ObjectDataRect) (showsproperties me) (items me))
    (let ((theguy (GetItemPosition me)))
      (if (symbolp (element me theguy))
        (progn
          (unless (highlight me) (setf (highlight me) t))
          (flashitem me))
        (when (highlight me) (setf (highlight me) nil))
        ))))

;;;*** SHOULD HAVE AN INITIAL VALUE SLOT IN ADDPROPERTYDIALOG THAT IS SETTABLE VIA A KEYWORD AND THEN HAVE
;;; DRAG AND DROP OF PROPERTIES SET THIS AUTOMATICALLY...

(define-handler dropped (MixinForPropertyHandlerPickers droppee)
  (withActorLocked (me)
    (draggingMouseLeave me droppee)
    (when (and (inputobjects me)
               (or (and (eq droppee ObjectDataRect) (showsproperties me) (items me))
                   (and (showsproperties me) (eq droppee PropertyDataRect))
                   (and (showshandlers me) (eq droppee HandlerDataRect)))
               (neq (componentfrom droppee) me))
      (cond
       ((eq droppee ObjectDataRect)
        (let ((theguy (getitemposition me))
              (theObjects (inputobjects me))
              (newValue (object ObjectDataRect))
              property)
          (when (and theguy (symbolp (element me theguy)))  ;;; Make sure its a property
            (setf property (element me theguy))
            (bringToFront (sk8::window me))
            (setf (keytarget (sk8::window me)) me)
            (UndoableSet property theObjects newValue :confirmation t)
            (setf (inputobjects me) (inputobjects me))
            (setf (selecteditems me) (list property))
            )))
       ((eq droppee PropertyDataRect)
        (let ((property (propertyname propertydatarect))
              (theObjects (inputobjects me))
              )
          (bringToFront (sk8::window me))
          (setf (keytarget (sk8::window me)) me)
          (DoPropertyDrop property theObjects)
          (setf (inputobjects me) (inputobjects me))
          (setf (selecteditems me) (list property))
          ))
       ((eq droppee HandlerDataRect)
        (let ((hand (handler HandlerDataRect))
              (theObjects (inputobjects me))
              )
          (bringToFront (sk8::window me))
          (setf (keytarget (sk8::window me)) me)
          (DoHandlerDrop hand theObjects)
          (setf (inputobjects me) (inputobjects me))
          (setf (selecteditems me) (list hand))
          ))
       ))))

;;; Now for the Regular picker...
(new styledpicker :objectname "PropertyHandlerPicker" :project sk8)
(addparent PropertyHandlerPicker MixinForPropertyHandlerPickers)

(setf (autotab PropertyHandlerPicker) t)
(setf (alphabeticalDisplay PropertyHandlerPicker) t)
(setf (selectionStyle PropertyHandlerPicker) 'single)

(new PropertyHandlerPicker :objectname "PropertyPicker" :project sk8)
(setf (showshandlers PropertyPicker) nil)
(setf (showsproperties PropertyPicker) t)

(new PropertyHandlerPicker :objectname "HandlerPicker" :project sk8)
(setf (showshandlers HandlerPicker) t)
(setf (showsproperties HandlerPicker) nil)

;; And it's nice instantiations...
(new textlist :objectname "PropertyHandlerList" :project sk8)
(setf (pickerprototype propertyHandlerlist) propertyhandlerpicker)
(setf (text (titlebar PropertyHandlerList)) "Properties and Handlers")
(new textlist :objectname "PropertyList" :project sk8 :prototype t)
(setf (pickerprototype PropertyList) PropertyPicker)
(setf (text (titlebar PropertyList)) "Properties")
(new textlist :objectname "HandlerList" :project sk8 :prototype t)
(setf (pickerprototype HandlerList) HandlerPicker)
(setf (text (titlebar HandlerList)) "Handlers")


;;;;;;;;;;-------------------------------------------------------------------------------------
;;; Now we make a special 2 by n picker from the table picker to serve as a basis for some nice components...
;;;;;;;;;;-------------------------------------------------------------------------------------

(new tablepicker :objectname "TwoByNBrowserPicker" :project sk8 
     :properties '((Editing :inherit :value t) first-strings))
(setf (EditorIncluded TwoByNBrowserPicker) t)
(setf (SelectByRows TwoByNBrowserPicker) t)
(setf (columns TwoByNBrowserPicker) 2)
(setf (columnsrigid TwoByNBrowserPicker) t)
(setf (textsize TwoByNBrowserPicker) 9)
(setf (textfont TwoByNBrowserPicker) "geneva")
(setf (selectionstyle TwoByNBrowserPicker) 'single)

(define-handler FlashItem (TwoByNBrowserPicker)
  (when (items me)
    (let ((theguy (second (GetItemPosition me))))
      (sk8-multival-bind (ll tt rr bb) (itemboundsrect me (list 1 theguy) :physical t)
        (setf ll (- (round ll) 3)
              tt (- (round tt) 3)
              rr (+ (round rr) 3)
              bb (+ (round bb) 3))
        (DrawXORFrame ll tt rr bb :pensize '(3 3))
        (sleep 0.01)
        (DrawXORFrame ll tt rr bb :pensize '(3 3))
        ))))

(define-handler GetItemPosition (TwoByNBrowserPicker)
  (when (items me)
    (sk8-multival-bind (hh vv) (mouseloc stage)
      (let ((theguy (pointonwhichpart me hh vv :part 'index)))
        (when theguy
          (setf (first theguy) 1)
          (setf (second theguy) (gs:range 1 (second theguy) (rows me))))
        (when (not theguy)
          (setf theguy '(1 1)))
        theguy))
    ))

(define-handler ItemBoundsRect (TwoByNBrowserPicker tuple &key (physical nil))
  (let ((row (second tuple))
        (col (first tuple)))
    (if (= col 1)
      (sk8-multival-bind (ll1 tt1 rr1 bb1) 
                         (call-next-method me (list 1 row) :physical physical)
        (sk8-multival-bind (ll2 tt2 rr2 bb2) 
                           (call-next-method me (list 2 row) :physical physical)
          (list ll1 tt1 rr2 bb1)))
      (sk8-multival-bind (ll1 tt1 rr1 bb1) 
                         (call-next-method)
        (list ll1 tt1 rr1 bb1)))
    ))

(define-handler createDisplayItem (TwoByNBrowserPicker theItem)
  (createTextDisplayItem me theItem))

(define-handler keydown (TwoByNBrowserPicker thechar)
  (when (items me)
    (cond
     ((alphanumeric thechar) 
      (process-search-string me thechar))
     (t (moveoffstage (editor me)) (call-next-method)))))

(define-handler findAndSelect (TwoByNBrowserPicker partialString)
  (let ((thepos (position partialString (first-strings me)
                          :test #'protected-subseq-equality)))
    (when thePos
      (withactorlocked (me)
        (setf (selection me) (list 0 (1+ thepos)))
        (alignselection me))
      )))

(setf (framesize (editor TwoByNBrowserPicker)) '(0 0))

(define-handler mousedown (TwoByNBrowserPicker) (moveoffstage (editor me)) (call-next-method))
(define-handler activatetext ((editor TwoByNBrowserPicker)) 
  (withactorlocked (me)
    (when (inheritsfrom (sk8::window me) browserpaper)
      (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical t)
        (decf ll 3)
        (decf tt 3)
        (decf rr 1)
        (decf bb 1)
        (setboundsrect (highlighterHalo (sk8::window me)) ll tt rr bb :physical t)))
    (call-next-method)))
(define-handler deactivatetext ((editor TwoByNBrowserPicker)) 
  (withactorlocked (me)
    (when (inheritsfrom (sk8::window me) browserpaper)
      (moveoffstage (highlighterHalo (sk8::window me))))
    (let ((sc (selecteditems (container me))))
      (moveoffstage me)
      (setf (selecteditems (container me)) nil)  ;;; ***OJO This is here cause forceredraw doesn't work...
      (setf (selecteditems (container me)) sc))
    (call-next-method) ))

(define-handler selectionCompleted (TwoByNBrowserPicker)
  (call-next-method)
  (showselection me))

(define-handler (setf horizontalScroll) (theval TwoByNBrowserPicker)
  (call-next-method 0 me))

(define-handler resized (TwoByNBrowserPicker)
  (let* ((ww (width me))
         (cs (columnspacing me))
         (halfwaythere (round (- ww cs cs 2) 2)))
    (call-next-method)
    (setf (columnwidths me) (list halfwaythere halfwaythere))
    ))

(define-handler (setf items) (theval TwoByNBrowserPicker)
  (let ((everythingOK nil))
    (sk8dev::withLockedCursor animatedClock
      (if (null theval)
        (call-next-method)
        (unwind-protect
          (let* ((thepairs (sort (mapcar #'(lambda (x) (cons (createTextDisplayItem me x) x)) theval) 
                                 #'string-lessp :key #'car))
                 (curstrings (mapcar #'car thepairs))
                 (curitems (mapcar #'cdr thepairs))
                 iarray
                 (dims (list 2 (length thepairs)))
                 theitems
                 (i 0))
            (setupitems me :dimensions dims)
            (setf theitems (items me))
            (SetupSelectionArray me)
            (sk8::setValue 'imageArray me (new array :dimensions dims))
            (setf iarray (imagearray me))
            (setf (first-strings me) curstrings)
            (dolist (item curstrings)
              (setf (aref theitems 0 i) (first curitems))
              (setf (aref theitems 1 i) (CreateSecondItem me (first curitems)))
              (setf (aref iarray 0 i) item)
              (setf (aref iarray 1 i) (aref (items me) 1 i))
              (setf curitems (rest curitems))
              (incf i))
            (SetUpTextColors me :dimensions dims)
            (SetUpTextStyles me :dimensions dims)
            (ComputeRowHeights me)
            (resized me)
            (setf everythingok t))
          (unless EverythingOK (setf (items me) nil)))
        ))))

(resized TwoByNBrowserPicker)
(setf (selectionborder TwoByNBrowserPicker) 0)

(define-handler CreateSecondItem (TwoByNBrowserPicker theitem)
  nil)


;;;;
;;;;;;;;;;-------------------------------------------------------------------------------------
;;; We use the twobynpicker to make a nice editor for properties and handlers...
;;;;;;;;;;-------------------------------------------------------------------------------------
;;;;

(new TwoByNBrowserPicker :objectname "PropertyHandlerSheetPicker" :project sk8)
(addparent PropertyHandlerSheetPicker MixinForPropertyHandlerPickers)

(define-handler createTextDisplayItem (PropertyHandlerSheetPicker theitem)
  (cond 
   ((null theitem) "")
   ((stringp theitem) theitem)
   ((symbolp theitem) (name theitem))
   (t (delete-if #'(lambda (x) (or (string= x "[") (string= x "]"))) 
                 (objectstring theitem :project (targetproject me))) )))

(define-handler CreateSecondItem (PropertyHandlerSheetPicker theitem)
  (if (symbolp theitem)
    (GetShortPropsValue theitem (inputobjects me) (targetproject me))
    (sk8::handlerArgumentsString theitem)))

(define-handler keydown (PropertyHandlerSheetPicker thechar)
  (when (items me)
    (cond
     ((memq theChar (list #\enter #\Return))
      (when (selection me)
        (let* ((theselection (selection me))
               (rowselected (and theselection (cadar theselection)))
               (property (aref (items me) 0 (1- rowselected)))
               (value (aref (items me) 1 (1- rowselected))))
          (when (and (editing me)
                     (symbolp property)
                     (delete-if-not #'(lambda (x) (SetterExistsP property x)) (inputobjects me)))
            (withactorlocked (me)
              (setf (framecolor (editor me)) yellow)
              (DisplayEditor me :indices (list 2 rowselected) :selectedText t :defaulttext value)
              (lightforceredraw me)))
          (when (and (editing me)
                     (not (symbolp property))
                     )
            (call-next-method)))))
     (t (moveoffstage (editor me)) (call-next-method)))))

(define-handler ReturnAction (PropertyHandlerSheetPicker str ind)
  (declare (ignore ind))
  (withActorLocked (me)
    (let* ((theobjects (inputobjects me))
           (property (aref (items me) 0 (1- (cadar (selection me))))))
      (delete-if-not #'(lambda (x) (SetterExistsP property x)) theobjects)
      (if theobjects
        (sk8-multival-bind (failure? errors locals globals result)
                           (translateScriptCommandOrExpr (targetProject me) str)
          (declare (ignore errors locals globals))
          (if failure?
            (MessageToUser "Syntax error!")
            (progn
              (setf result (evaluateScriptTranslation result))
              (UndoableSet property theObjects result)
              (setf (inputobjects me) (inputobjects me)))))
        (MessageToUser (format nil "~a is not a settable property." property)))
      )))

(define-handler SetUpTextStyles (PropertyHandlerSheetPicker &key dimensions)
  (let* ((objs (inputobjects me))
         (obj (first objs))
         (theItems (items me))
         (thestyles (textstyles me))
         thestyle val)
    (unless dimensions 
      (setf dimensions (dimensions me)))
    (setf (slot-value me 'textstyles) (new array :dimensions dimensions))
    (setf thestyles (textstyles me))
    (when (and (> (columns me) 0) (> (rows me) 0))
      (dotimes (r (second dimensions))
        (setf val (aref theitems 0 r))
        (setf thestyle nil)
        (if (symbolp val)
          (progn
            (setf thestyle (if (SetterExistsP (aref theitems 0 r) obj) nil '(italic)))
            (when (localproperty obj val) (push 'bold thestyle))
            )
          (when (memq (object val) objs) (setf thestyle '(bold))))
        (unless thestyle (setf thestyle '(plain)))
        (setf (aref thestyles 0 r) thestyle)
        (setf (aref thestyles 1 r) thestyle))
      )))

(new PropertyHandlerSheetPicker :objectname "PropertySheetPicker" :project sk8)
(setf (showshandlers PropertySheetPicker) nil)
(setf (showsproperties PropertySheetPicker) t)
(setf (view PropertySheetPicker) 'all)
(define-handler resized (PropertySheetPicker)
  (let* ((ww (width me))
         (cs (columnspacing me))
         (theImages (imagearray me))
         (halfwaythere (round (- ww cs cs 2) 2))
         (division halfwaythere)
         (longest 0))
    (call-next-method)
    (dotimes (r (rows me))
      (let ((image (aref theImages 0 r)))
        (setq longest (max longest (+ 2 (first (gs:actor-text-size me :tryText image)))))))
    (if (< longest halfwaythere) (setf division longest))
    (setf (columnwidths me) (list division (- (round (width me)) cs cs  3 division)))
    ))

(new PropertyHandlerSheetPicker :objectname "HandlerSheetPicker" :project sk8)
(setf (showshandlers HandlerSheetPicker) t)
(setf (showsproperties HandlerSheetPicker) nil)

(new textlist :objectname "PropertyHandlerSheet" :project sk8)
(setf (pickerprototype PropertyHandlerSheet) PropertyHandlerSheetPicker)
(setf (text (titlebar PropertyHandlerSheet)) "Properties and Handlers")

(new textlist :objectname "PropertySheet" :project sk8 :prototype t)
(setf (pickerprototype PropertySheet) PropertySheetPicker)
(setf (text (titlebar PropertySheet)) "Properties")


(new textlist :objectname "HandlerSheet" :project sk8 :prototype t)
(setf (pickerprototype HandlerSheet) HandlerSheetPicker)
(setf (text (titlebar HandlerSheet)) "Handlers")

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  Here are for Functions Constants and Variables
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
(new BrowserComponent :objectname "MixinForProjectDataPickers" :project sk8
     :properties '(InputProject OutputFunction OutputVariable OutputConstant
                   showsFunctions
                   showsVariables
                   showsConstants
                   )) ;;showsObjectNames
(setf (showsFunctions MixinForProjectDataPickers) t)
(setf (showsVariables MixinForProjectDataPickers) t)
(setf (showsConstants MixinForProjectDataPickers) t)
;;;;(setf (showsObjectNames MixinForProjectDataPickers) nil)

(addInputPort MixinForProjectDataPickers 'InputProject :signature Project)
(addOutputPort MixinForProjectDataPickers 'OutputFunction :signature SK8::Function)
(addOutputPort MixinForProjectDataPickers 'OutputVariable :signature Symbol)
(addOutputPort MixinForProjectDataPickers 'OutputConstant :signature Symbol)


(define-handler keydown (MixinForProjectDataPickers theChar)
  (cond 
   ((or (eq theChar #\Return) (eq theChar #\Enter))
    (doubleclick me))
   ((eq theChar #\Delete)
    (when (and (inputproject me) (items me))
      (withactorlocked ((sk8::window me))
        (let ((val (first (selecteditems me)))
              (proj (inputproject me)))
          (cond
           ((functionp val)
            (removeFunctionDialog val))
           ((constantp val)
            (RemoveConstantDialog proj :name val))
           (t
            (RemoveVariableDialog proj :name val))
           )
          (setf (inputproject me) proj)))))
   ((memq thechar '(#\escape #\Help)) 
    (let* ((sel (first (selecteditems me)))
           (id (and sel 
                    (if (symbolp sel)
                      sel
                      (name sel)))))
      (when (symbolp id)
        (sk8-multival-bind (ll tt rr bb) (itemboundsrect me (if (is-a me picker) (first (selection* me)) (first (selection me)))
                                                         :physical t)
          (PopupBalloonForHelp id (+ ll (/ (- rr ll) 2)) (+ tt (/ (- bb tt) 2)))))))
   (t 
    (call-next-method))))

(define-handler doubleclick (MixinForProjectDataPickers)
  (when (items me)
    (let ((thefun (first (selecteditems me)))
          (proj (inputproject me)))
      (if (if (functionp thefun)
            (setq thefun (name thefun))
            (fboundp thefun))
        (edithandler proj thefun nil nil nil)))))

(define-handler ExtendedMouseDown (MixinForProjectDataPickers)
  (let ((sels (selecteditems me)))
    (when sels
      (if (is-a (first sels) hierarchicalpickeritem) (setf sels (mapcar #'value sels)))
      (cond 
       ((functionp (first sels))
        (setf (boundsrect FunctionDataRect :physical t) (itemboundsrect me (first (if (inheritsfrom me picker) 
                                                                                  (selection* me)
                                                                                  (selection me))) :physical t))
        (setf (functions FunctionDataRect) (list (first sels)))
        (setf (ComponentFrom FunctionDataRect) me)
        (withcursor standardcursor
          (drag FunctionDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
          )
        t)
       ((constantp (first sels))
        (setf (boundsrect ConstantDataRect :physical t) (itemboundsrect me (first (if (inheritsfrom me picker) 
                                                                                  (selection* me)
                                                                                  (selection me))) :physical t))
        (setf (constants ConstantDataRect) (list (first sels)))
        (setf (ComponentFrom ConstantDataRect) me)
        (withcursor standardcursor
          (drag ConstantDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
          )
        t)
       (t
        (setf (boundsrect GlobalDataRect :physical t) (itemboundsrect me (first (if (inheritsfrom me picker) 
                                                                                (selection* me)
                                                                                (selection me))) :physical t))
        (setf (Globals GlobalDataRect) (list (first sels)))
        (setf (ComponentFrom GlobalDataRect) me)
        (withcursor standardcursor
          (drag GlobalDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
          )
        t)
       ))))

(new Menu :objectname "PDPMenu" :project sk8
     :properties '(BrowserComponent))
(define-handler update (PDPMenu)
  (let ((theval (and (BrowserComponent me) 
                     (inheritsfrom (BrowserComponent me) MixinForProjectDataPickers)
                     (inputProject (BrowserComponent me)))))
    (dolist (i (menuitems me))
      (setf (enabled i) theval)))
  (call-next-method))
(new menuitem :objectname "PDPMenuItemDispose" :project sk8)
(define-handler menuselect (PDPMenuItemDispose)
  (setf (keytarget (sk8::window (BrowserComponent (menu me)))) (BrowserComponent (menu me)))
  (keydown (BrowserComponent (menu me)) #\delete))
(define-handler update (PDPMenuItemDispose)
  (let ((sels (first (selecteditems (BrowserComponent (menu me))))))
    (when sels 
      (if (inheritsfrom sels hierarchicalpickeritem) (setf sels (value sels))))
    (setf (text me)
          (if sels
            (concatenate 'string "Remove " (if (functionp sels) (string (name sels)) (name sels)))
            "Remove"))
    (setf (enabled me) sels)))

(new PDPMenu :objectname "MenuForFunPickers" :project sk8)
(setf (text MenuForFunPickers) "Functions")
(new menuitem :objectname "Newfunctionmi" :menu MenuForFunPickers :project sk8)
(setf (text newfunctionmi) "New Function")
(define-handler update (newfunctionmi)
  (if (eq (keytarget (sk8::window (menu me))) (browsercomponent (menu me)))
    (setf (commandkey me) #\N)
    (setf (commandkey me) nil)))
(define-handler menuselect (newfunctionmi)
  (let ((bc (BrowserComponent (menu me))))
    (AddFunctionDialog (InputProject bc))
    (setf (keytarget (sk8::window bc)) bc)
    (setf (inputproject bc) (inputproject bc))
    ))
(new PDPMenuItemDispose :objectname "FunItemDispose" 
     :menu MenuForFunPickers :project sk8)

(new PDPMenu :objectname "MenuForVarPickers" :project sk8)
(setf (text MenuForVarPickers) "Variables")
(new menuitem :objectname "NewVariablemi" :menu MenuForVarPickers :project sk8)
(setf (text newVariablemi) "New Variable")
(define-handler update (newVariablemi)
  (if (eq (keytarget (sk8::window (menu me))) (browsercomponent (menu me)))
    (setf (commandkey me) #\N)
    (setf (commandkey me) nil)))
(define-handler menuselect (newVariablemi)
  (let* ((bc (BrowserComponent (menu me)))
        (var (AddVariableDialog (InputProject bc))))
    (when var
      (withactorlocked (bc)
        (setf (inputproject bc) (inputproject bc))
        (setf (keytarget (sk8::window bc)) bc)
        (setf (selecteditems bc) (list var))))
    ))
(new PDPMenuItemDispose :objectname "VarItemDispose" 
     :menu MenuForVarPickers :project sk8)

(new PDPMenu :objectname "MenuForConPickers" :project sk8)
(setf (text MenuForConPickers) "Constants")
(new menuitem :objectname "newConstantmi" :menu MenuForConPickers :project sk8)
(setf (text newConstantmi) "New Constant")
(define-handler update (newConstantmi)
  (if (eq (keytarget (sk8::window (menu me))) (browsercomponent (menu me)))
    (setf (commandkey me) #\N)
    (setf (commandkey me) nil)))
(define-handler menuselect (newConstantmi)
  (let* ((bc (BrowserComponent (menu me)))
         (var (AddConstantDialog (InputProject bc))))
    (when var
      (withactorlocked (bc)
        (setf (inputproject bc) (inputproject bc))
        (setf (keytarget (sk8::window bc)) bc)
        (setf (selecteditems bc) (list var))))
    )
  )
(new PDPMenuItemDispose :objectname "ConItemDispose" 
     :menu MenuForConPickers :project sk8)

(define-handler menuprototype (MixinForProjectDataPickers)
  (let ((ourlist nil))
    (if (showsfunctions me) (setf ourlist (cons MenuForFunPickers ourlist)))
    (if (showsvariables me) (setf ourlist (cons MenuForVarPickers ourlist)))
    (if (showsconstants me) (setf ourlist (cons MenuForConPickers ourlist)))
    ourlist))


(define-handler (setf InputProject) (theVal MixinForProjectDataPickers)
  (sk8::setValue 'InputProject me theval)
  (sk8dev::withLockedCursor animatedClock
    (tickeventclock)
    (if theval
      (withActorLocked ((sk8::window me))
        (let* ((theItems nil)
               (itms (items me))
               (curInput theVal)
               (oldSelection (and itms (selectedItems me))))
          (when (showsFunctions me)
            (setf theItems (nconc theItems (mapcar #'symbol-function (functions curInput)))))
          (when (showsVariables me)
            (setf theItems (nconc theItems (globals curInput))))
          (when (showsConstants me)
            (setf theItems (nconc theItems (constants curInput)))) ;:objectnames (showsObjectNames me)
          (tickeventclock)
          (withActorLocked (me)
            (setf (items me) theItems)
            (if theItems
              (if (and oldselection (memq (first oldselection) theitems))
                (setf (selectedItems me) (list (first oldselection)))
                (if (arrayp (items me))
                  (setf (selection me) (and (> (rows me) 0) '(0 0)))
                  (setf (selectedItems me) (first (items me)))))
              )
            (tickeventclock)
            (selectioncompleted me))))
      (progn
        (setf (items me) nil)
        (setf (outputfunction me) nil)
        (setf (outputvariable me) nil)
        (setf (outputconstant me) nil)
        ))))
  
(define-handler selectioncompleted (MixinForProjectDataPickers) 
  (when (and (inputproject me) (items me))
    (withactorlocked ((sk8::window me))
      (let ((val (first (selecteditems me)))
            )
        (cond
         ((functionp val)
          (setf (outputfunction me) val)
          (setf (outputvariable me) nil)
          (setf (outputconstant me) nil)
          )
         ((constantp val)
          (setf (outputfunction me) nil)
          (setf (outputvariable me) nil)
          (setf (outputconstant me) val)
          )
         (t
          (setf (outputfunction me) nil)
          (setf (outputvariable me) val)
          (setf (outputconstant me) nil)
          )
         )))))

(define-handler createTextDisplayItem (MixinForProjectDataPickers theitem)
  (cond 
   ((null theitem) "")
   ((symbolp theitem) (name theitem))
   ((is-a theitem sk8::function) (name (sk8::name theitem)))
   (t (objectstring theitem  :project (inputproject me)))))

(define-handler createTextItemStyle (MixinForProjectDataPickers targetItem theString position)
  (declare (ignore theString position))
  (sk8-multivals nil nil 
                 (if (constantp targetItem) '(italic) '(plain))  
                 nil))

(define-handler draggingMouseEnter (MixinForProjectDataPickers actorDragged)
  (when (and (inputproject me)
             (showsVariables me)
             (items me)
             (or (eq actordragged ObjectDataRect)
                 (eq actordragged PropertyDataRect))
             )
    (setf (highlight me) t)))

(define-handler draggingMouseLeave (MixinForProjectDataPickers actorDragged)
  (when (highlight me) (setf (highlight me) nil)))

(define-handler draggingMouseWithin (MixinForProjectDataPickers actorDragged)
  (when (and (highlight me) (items me))
    (let ((theguy (GetItemPosition me)))
      (if (symbolp (element me theguy))
        (flashitem me)
        ))))

(define-handler dropped (MixinForProjectDataPickers droppee)
  (withActorLocked (me)
    (when (highlight me)
      (draggingMouseLeave me droppee)
      (when (or (eq droppee ObjectDataRect)
                (eq droppee PropertyDataRect))
        (let ((theguy (getitemposition me))
              (newValue (if (eq droppee ObjectDataRect)
                          (object ObjectDataRect)
                          (value  PropertyDataRect)))
              theVar)
          (unless (listp newvalue) (setf newvalue (list newvalue)))
          (if (eql (length newvalue) 1) (setf newvalue (first newvalue)))
          (when (and theguy (symbolp (element me theguy)))  ;;; Make sure its a variable
            (setf theVar (element me theguy))
            (bringToFront (sk8::window me))
            (setf (keytarget (sk8::window me)) me)
            (when (YesOrNoDialog (concatenate 'string "Set " (simpleobjectstring theVar) 
                                              " to " 
                                              (objectstring newValue :project (inputproject me)) "?")
                                 :cancel nil)
              (set theVar newValue))
            (setf (inputproject me) (inputproject me))
            (setf (selecteditems me) (list theVar))
            )))
      )))

(new styledpicker :objectname "ProjectDataPicker" :project sk8)
(addparent ProjectDataPicker MixinForProjectDataPickers)

(setf (autotab ProjectDataPicker) t)
(setf (alphabeticalDisplay ProjectDataPicker) t)
(setf (selectionStyle ProjectDataPicker) 'single)


;;;;;________________________________________________________________________________________________________________
;;;;;  Now we use the 2bynpicker again to make a nice editor for vars constants and functions...
;;;;;________________________________________________________________________________________________________________

(new TwoByNBrowserPicker :objectname "ProjectDataSheetPicker" :project sk8)
(addparent ProjectDataSheetPicker MixinForProjectDataPickers)

(define-handler CreateSecondItem (ProjectDataSheetPicker theitem)
  (cond
   ((functionp theItem)
    (funcall #'handlerArgumentsString theItem))
   ((symbolp theItem)
    (objectstring (symbol-value theitem) :project (inputproject me)))
   (t (objectstring theItem :project (inputproject me)))))

(define-handler keydown (ProjectDataSheetPicker thechar)
  (when (items me)
    (cond
     ((memq theChar (list #\enter #\Return))
      (when (selection me)
        (let* ((theselection (selection me))
               (rowselected (and theselection (cadar theselection)))
               (item (aref (items me) 0 (1- rowselected) ))
               (value (aref (items me) 1 (1- rowselected))))
          (when (and (editing me)
                     (not (constantp item))
                     (not (is-a item sk8::function)))
            (withactorlocked (me)
              (setf (framecolor (editor me)) yellow)
              (DisplayEditor me :indices (list 2 rowselected) :selectedText t :defaulttext value)
              (forceredraw me)))
          (when (and (editing me)
                     (is-a item sk8::function))
            (call-next-method))
          )))
     (t (moveoffstage (editor me)) (call-next-method)))))

(define-handler ReturnAction (ProjectDataSheetPicker str ind)
  (declare (ignore ind))
  (when (selection me)
    (let* ((theselection (selection me))
           (rowselected (and theselection (cadar theselection)))
           (item (aref (items me) 0 (1- rowselected))))
      (sk8-multival-bind (failure? errors locals globals result)
                         (translateScriptCommandOrExpr (inputproject me) str)
        (declare (ignore errors locals globals))
        (if failure?
          (MessageToUser "Syntax error!")
          (progn
            (setf result (evaluateScriptTranslation result))
            (set item result)
            (setf (inputproject me) (inputproject me)))))
      )))

(new textlist :objectname "ProjectDataSheet" :project sk8 :prototype t)
(setf (pickerprototype ProjectDataSheet) ProjectDataSheetPicker)
(setf (text (titlebar ProjectDataSheet)) "Project Stuff")

(new ProjectDataSheetPicker :objectname "FunctionSheetPicker" :project sk8)
(setf (showsFunctions FunctionSheetPicker) t)
(setf (showsVariables FunctionSheetPicker) nil)
(setf (ShowsConstants FunctionSheetPicker) nil)
(new textlist :objectname "FunctionSheet" :project sk8)
(setf (pickerprototype FunctionSheet) FunctionSheetPicker)
(setf (text (titlebar FunctionSheet)) "Functions")

(new ProjectDataSheetPicker :objectname "VariableSheetPicker" :project sk8)
(setf (showsFunctions VariableSheetPicker) nil)
(setf (showsVariables VariableSheetPicker) t)
(setf (ShowsConstants VariableSheetPicker) nil)
(new textlist :objectname "VariableSheet" :project sk8)
(setf (pickerprototype VariableSheet) VariableSheetPicker)
(setf (text (titlebar VariableSheet)) "Variables")

(new ProjectDataSheetPicker :objectname "ConstantSheetPicker" :project sk8)
(setf (showsFunctions ConstantSheetPicker) nil)
(setf (showsVariables ConstantSheetPicker) nil)
(setf (ShowsConstants ConstantSheetPicker) t)
(new textlist :objectname "ConstantSheet" :project sk8)
(setf (pickerprototype ConstantSheet) ConstantSheetPicker)
(setf (text (titlebar ConstantSheet)) "Constants")


;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  HandlerViewer
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new EditorScriptEditText :objectname "HandlerViewerText" :project sk8)
(setf (breakpointsVisible HandlerViewerText) nil)
(addparent HandlerViewerText BrowserComponent)

(setf (textsize HandlerViewerText) 9)  ;;;for consistency...
(setf (autotab HandlerViewerText) nil)
(locktext HandlerViewerText)

(addInputPort HandlerViewerText 'InputHandler :signature Handler)


(define-handler (setf inputhandler) (TheHandler HandlerViewerText)
  (let (res)
    (setf (dirty me) nil)
    (if theHandler
      (progn
        (if (or (not (object thehandler))
                (memq (project (object thehandler)) (list sk8 ui)))
          (setf res nil)
          (setf res (call-next-method theHandler me :error nil)))
        (setf (dirty me) nil)
        (unless res
          (setf (text me) (concatenate 'string 
                                       "Handler Arguments:"
                                       (string newline)
                                       (handlerArgumentsString TheHandler)
                                       (string newline)
                                       (string newline)
                                       "Code Not Available"
                                       ))))
      (progn 
        (setf (text me) "")
        ))
    (updatepartnerScroller me (partnerVScroller me))
    (updatepartnerScroller me (partnerHScroller me))
    (setf (dirty me) nil)
    ))

(new textlist :objectname "HandlerViewer" :project sk8 :prototype t)
(setf (pickerprototype HandlerViewer) HandlerViewerText)
(setf (text (titlebar HandlerViewer)) "Handler Text")




;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  Editor Number 1.  The Simple Text Editor
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------


(new edittext :objectname "ValueText" :project sk8
     :prototype t
     :properties '(InputObjects InputProperty OutputString))
(addparent ValueText BrowserComponent)

(setf (textsize ValueText) 9)  ;;;for consistency...
(setf (autotab ValueText) t)

(addInputPort ValueText 'InputObjects :signature Collection)
(addInputPort ValueText 'InputProperty :signature Symbol)
(addOutputPort ValueText 'OutputString :signature String)


(define-handler (setf inputObjects) (theval ValueText)
  (sk8::setValue 'inputObjects me theval)
  (tickeventclock)
  (unless (and (inputProperty me) theVal) (setf (text me) nil)))

(define-handler (setf inputproperty) (theval ValueText)
  (sk8::setValue 'inputproperty me theval)
  (sk8dev::withlockedcursor animatedClock
    (withactorlocked (me)
      (tickeventclock)
      (let ((objs (inputobjects me)))
        (if (and objs theVal)  ;;;Again we rely on the objects being set to us first and only update when properties are specified
          (progn
            (setf (text me) (GetPropsValue theVal objs (targetproject me)))
            (if (SetterExistsP theval (first objs))
              (progn
                (setf (textstyle me) '(plain))
                (unlocktext me :force t))
              (progn
                (setf (textstyle me) '(italic))
                (locktext me))))
          (setf (text me) "")
          )))))

(define-handler keydown (ValueText thechar)
  (cond  
   ((and (memq thechar (list #\newline #\enter)) (not (optionkeydown)))
    (let* ((property (inputproperty me))
           (theObjects (inputObjects me))
           errstring)
      (delete-if-not #'(lambda (x) (SetterExistsP property x)) theobjects)
      (if theobjects
        (sk8-multival-bind (failure? errors locals globals result)
                           (translateScriptCommandOrExpr (targetProject me) (text me))
          (declare (ignore errors locals globals))
          (if failure?
            (MessageToUser "Syntax error!")
            (progn
              (if (commandkeydown) (setf theobjects (append theobjects (apply #'nconc (mapcar #'potential-descendants theobjects)))))
              (setq result (multiple-value-bind (val1 err?) 
                                                (ignore-errors 
                                                 (setf result (evaluateScriptTranslation result))
                                                 (UndoableSet property theObjects result)
                                                 (dolist (i (deepcontents (sk8::window me)))
                                                   (if (inheritsfrom i propertysheetpicker) (setf (inputObjects i) (inputObjects i))))
                                                 )
                             (when err?
                               (let ((*package* (sk8::package (targetproject ui))))
                                 (setf errstring (with-output-to-string (s) (ccl::report-condition err? s))))
                               (messageTouser errstring :BEEP T)
                               )
                             val1)) 
              )))
        (MessageToUser (format nil "~a is not a settable property." property)))
      (setf (inputproperty me) (inputproperty me))
      (setSelection me 0 -1)
      ))
   ((memq thechar (list #\escape #\tab))
    (setf (inputproperty me) (inputproperty me))
    (call-next-method))
   (t (call-next-method))))


;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  Editor Number 2  Value Editor Picker...  Allows both display and edit of list values...
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------


(new tablepicker :objectname "ValueEditorPicker" :project sk8
     :prototype t
     :properties '(InputObjects InputProperty OutputString EditingList 
                   (Editing :value t)))
(addparent ValueEditorPicker BrowserComponent)

(setf (EditorIncluded ValueEditorPicker) t)
(setf (textsize ValueEditorPicker) 9)
(setf (textfont ValueEditorPicker) "geneva")
(setf (selectionstyle ValueEditorPicker) 'single)
(setf (rowspacing ValueEditorPicker) 2)
(setf (columnspacing ValueEditorPicker) 8)
(setf (columnLinesSize ValueEditorPicker) 0)
(setf (rowLinesSize ValueEditorPicker) 0)

(setf (autotab ValueEditorPicker) t)

(addInputPort ValueEditorPicker 'InputObjects :signature Collection)
(addInputPort ValueEditorPicker 'InputProperty :signature Symbol)
(addOutputPort ValueEditorPicker 'OutputString :signature String)


(new Menu :objectname "MenuForValueEditorPickers" :project sk8
     :properties '(BrowserComponent))
(setf (text MenuForValueEditorPickers) "Values")
(define-handler update (MenuForValueEditorPickers)
  (let ((theval (and (BrowserComponent me) 
                     (inheritsfrom (BrowserComponent me) ValueEditorPicker)
                     (selecteditems (BrowserComponent me))
                     (editinglist (browsercomponent me))
                     (memq 'plain (textstyle (BrowserComponent me))) )))  ;;; IF IT IS AN EDITABLE PROPERTY IT WILL HAVE PLAIN TEXT (this is a fast way to check for this)
    (dolist (i (menuitems me))
      (setf (enabled i) theval)))
  (call-next-method))
(new menuitem :objectname "ValueEditorPickerNewMenuItem" :menu MenuForValueEditorPickers :project sk8)
(define-handler menuselect (ValueEditorPickerNewMenuItem)
  (let ((vep (BrowserComponent (menu me))))
    (if (eq (inputproperty vep) 'knownchildren)
      (getnewfromuser (first (inputobjects vep)) :project (targetproject (browsercomponent (menu me))))
      (when (and (editinglist vep) (selection vep))
        (withactorlocked (vep)
          (let* ((theselection (selection vep))
                 (rowselected (and theselection (cadar theselection)))
                 (theObjects (inputObjects vep))
                 (property (inputproperty vep))
                 newval)
            (setf newval (copy-list (funcall property (first theObjects))))
            (if (zerop (1- rowselected))
              (setf newval (cons nil newval))
              (setf (nthcdr (1- rowselected) newval) (cons nil (nthcdr (1- rowselected) newval))))
            (setf (items vep) newval)
            (setf (selection vep) (list 1 rowselected))
            (setf (keytarget (sk8::window vep)) vep)
            (keydown vep #\return)
            )))
      )))
(define-handler update (ValueEditorPickerNewMenuItem)
  (let ((vep (BrowserComponent (menu me))))
    (if (eq (keytarget (sk8::window vep)) vep) 
      (setf (commandkey me) #\N)
      (setf (commandkey me) nil))
    (cond
     ((null (editing vep))
      (setf (text me) "New"))
     ((eq (inputproperty vep) 'knownchildren)
      (setf (text me) (concatenate 'string "New " (objectstring (first (inputobjects vep)) 
                                                                :project (targetproject (BrowserComponent (menu me)))))))
     ((editinglist vep)
      (setf (text me) "Add Value To List"))
     (t (setf (text me) "New")))))
(new menuitem :objectname "ValueEditorPickerItemDispose" :menu MenuForValueEditorPickers :project sk8)
(define-handler menuselect (ValueEditorPickerItemDispose)
  (setf (keytarget (sk8::window (BrowserComponent (menu me)))) (BrowserComponent (menu me)))
  (keydown (BrowserComponent (menu me)) #\delete))
(define-handler update (ValueEditorPickerItemDispose)
  (let ((vep (BrowserComponent (menu me))))
    (cond
     ((null (editing vep))
      (setf (text me) "Clear References"))
     ((eq (inputproperty vep) 'knownchildren)
      (setf (text me) (concatenate 'string "Clear References to" (objectstring (first (inputobjects vep)) 
                                                                :project (targetproject (BrowserComponent (menu me)))))))
     ((editinglist vep)
      (setf (text me) "Remove Value To List"))
     (t (setf (text me) "Remove")))))

(define-handler menuprototype (ValueEditorPicker)
  MenuForValueEditorPickers)

(define-handler createDisplayItem (ValueEditorPicker theItem)
  (createTextDisplayItem me theItem))

(define-handler (setf inputObjects) (theval ValueEditorPicker)
  (sk8::setValue 'inputObjects me theval)
  (unless (and (inputProperty me) theVal) (setf (text me) nil)))

(define-handler (setf inputproperty) (theval ValueEditorPicker)
  (sk8::setValue 'inputproperty me theval)
  (sk8dev::withlockedcursor animatedClock
    (withActorLocked ((sk8::window me))
      (let ((objs (inputobjects me))
            (pr theval)
            newval
            (cnt 0))
        (tickeventclock)
        (if (and objs pr)  ;;;Again we rely on the objects being set to us first and only update when properties are specified
          (let ((theval (sk8::GetPropsValue pr objs (targetproject me) :asastring nil)))
            (if (or (null theval) (not (listp theval))) 
              (progn
                (setf (editingList me) nil)
                (if (inheritsfrom (container me) textlist) 
                  (setf (text (titlebar (container me))) "Value"))
                (setf theval (list theval)))
              (progn
                (if (inheritsfrom (container me) textlist) 
                  (setf (text (titlebar (container me))) "Value List"))
                (setf (editingList me) t)))
            (tickeventclock)
            (if (delete-if-not #'(lambda (x) (SetterExistsP pr x)) objs)
              (setf (textstyle me) '(plain))
              (setf (textstyle me) '(italic)))
            (setf newval (new array :dimensions (list 1 (length theval)) :project (project me)))
            (dolist (i theval) 
              (setf (aref newval 0 cnt ) i)
              (incf cnt))
            (setf (items me) newval)
            (setf (selection me) (and newval '(0 0)))))
        (unless (and objs pr) (setf (items me) nil))  ;;; blank it if we don't need it
        ))))

(define-handler keydown (ValueEditorPicker thechar)
  (cond
   ((and (editing me) (items me) (selection me) (or (eq theChar #\enter) (eq theChar #\Return)))
    (let* ((theselection (selection me))
           (rowselected (and theselection (cadar theselection)))
           (theObjects (inputObjects me))
           (property (inputproperty me))
           (thevaluetext (aref (imagearray me) 0 (1- rowselected) )))
      (if (eq (inputproperty me) 'knownchildren)
        (messagetouser "Use the menubar or Command-N to add a new child!" :beep t)
        (if (and (delete-if-not #'(lambda (x) (SetterExistsP property x)) theobjects))
          (withactorlocked (me)
            (setf (framecolor (editor me)) yellow)
            (DisplayEditor me :indices (list 1 rowselected) :selectedText t :defaulttext thevaluetext)
            (forceredraw me))
          (messagetouser "This property can not be edited!" :beep t)))))
   ((and (editing me) (items me) (selection me) (eq theChar #\Delete))
    (let* ((theselection (selection me))
           (rowselected (and theselection (cadar theselection)))
           (theObjects (inputObjects me))
           (property (inputproperty me))
           newval
           (thevalue (aref (items me) 0 (1- rowselected))))
      (if (eq (inputproperty me) 'knownchildren)
        (if (DisposeDialog thevalue)
          (setf (inputProperty me) (inputproperty me)))
        (when (and (editing me)
                   (editinglist me)
                   (delete-if-not #'(lambda (x) (SetterExistsP property x)) theobjects))
          (setf newval (funcall property (first theObjects)))
          (if (zerop (1- rowselected))
            (setf newval (rest newval))
            (setf (nthcdr (1- rowselected) newval) (nthcdr rowselected newval)))
          (UndoableSet property theObjects newval)
          (setf (inputProperty me) (inputproperty me))
          ))))
   ((and (items me) (alphanumeric thechar)) 
    (process-search-string me thechar))
   (t (moveoffstage (editor me)) (call-next-method)))
  )

(define-handler construct-list (ValueEditorPicker)
  (let ((r (rows me))
        ourlist)
    (dotimes (i r)
      (push (aref (imagearray me) 0 i) ourlist))
    (nreverse ourlist)))

(define-handler findAndSelect (ValueEditorPicker partialString)
  (let* ((mylist (construct-list me))
         (thepos (position partialString mylist
                          :test #'protected-subseq-equality)))
    (when thePos
      (setf (selection me) (list 0 (1+ thepos)))
      )))

(define-handler mousedown (ValueEditorPicker) (moveoffstage (editor me)) (call-next-method))

(define-handler createTextDisplayItem (ValueEditorPicker theitem)
  (cond 
   ((null theitem) "false")
   ((stringp theitem) theitem)
   ;;;;((and (inheritsfrom theitem handler) (neq theitem handler) ) (name theitem))
   (t (objectstring theitem :project (targetproject me)))))

(define-handler selectionCompleted (ValueEditorPicker)
  (call-next-method)
  (showselection me))

(define-handler resized (ValueEditorPicker)
  (let* ((ww (width me)))
    (call-next-method)
    (setf (columnwidths me) ww)
    ))

(define-handler computecolumnwidths (ValueEditorPicker)
  (let* ((ww (width me)))
    (setf (columnwidths me) ww)
    ))

(define-handler ReturnAction (ValueEditorPicker str ind)
  (declare (ignore str ind))
  (when (selection me)
    (let* ((theselection (selection me))
           (rowselected (and theselection (cadar theselection)))
           (theObjects (inputObjects me))
           (property (inputproperty me))
           (thevaluetext (text (editor me)))
           )
      (delete-if-not #'(lambda (x) (SetterExistsP property x)) theobjects)
      (if theobjects
        (sk8-multival-bind (failure? errors locals globals result)
                           (translateScriptCommandOrExpr (targetProject me) thevaluetext)
          (declare (ignore errors locals globals))
          (if failure?
            (MessageToUser "Syntax error!")
            (progn
              (setf result (evaluateScriptTranslation result))
              (if (editinglist me)
                (let ((curval (funcall property (first theobjects))))
                  (setf (nth (1- rowselected) curval) result)
                  (setf result curval)))
              (UndoableSet property theObjects result)
              )))
        (MessageToUser (format nil "~a is not a settable property." property)))
      (setf (inputProperty me) (inputproperty me))
      )))

(define-handler EscapeAction (ValueEditorPicker)
  (setf (inputProperty me) (inputproperty me))
  )

;;;OJO Should always be kept in synch with extendedMouseDown of mixinforobjectpickers...
(define-handler ExtendedMouseDown (ValueEditorPicker)
  (let ((sels (selecteditems me)))
    (when sels
      (if (is-a (first sels) hierarchicalpickeritem) (setf sels (mapcar #'value sels)))
      (if (and (= (length sels) 1) (is-a sels handler))
        (progn
          (setf sels (first sels))
          (setf (boundsrect HandlerDataRect :physical t) (itemboundsrect me (first (if (inheritsfrom me picker) 
                                                                                   (selection* me)
                                                                                   (selection me))) :physical t))
          (setf (objects HandlerDataRect) (list (object sels)))
          (setf (handler HandlerDataRect) sels)
          (setf (ComponentFrom HandlerDataRect) me)
          (withcursor standardcursor
            (drag HandlerDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
            )
          t)
        (progn
          (if (= (length sels) 1) (setf sels (first sels)))
          (setf (boundsrect ObjectDataRect :physical t) (selectedItemsBoundsRect me))
          (setf (object ObjectDataRect) sels)
          (setf (ComponentFrom ObjectDataRect) me)
          (withcursor standardcursor
            (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
            )
          t)
        ))))


(define-handler draggingMouseEnter (ValueEditorPicker actorDragged)
  (when (and (inputobjects me)
             (inputproperty me)
             (items me)
             (eq actordragged ObjectDataRect)
             (neq (componentfrom actordragged) me))
    (setf (highlight me) t)))

(define-handler draggingMouseLeave (ValueEditorPicker actorDragged)
  (when (highlight me) (setf (highlight me) nil)))

(define-handler draggingMouseWithin (ValueEditorPicker actorDragged)
  (when (and (inputobjects me)
             (inputproperty me)
             (items me)
             (eq actordragged ObjectDataRect)
             (neq (componentfrom actordragged) me))
    (flashitem me)))

(define-handler dropped (ValueEditorPicker droppee)
  (withActorLocked (me)
    (draggingMouseLeave me droppee)
    (when (and (inputobjects me)
               (inputproperty me)
               (items me)
               (eq droppee ObjectDataRect)
               (neq (componentfrom droppee) me))
      (cond
       ((eq droppee ObjectDataRect)
        (let* ((theguy (second (getitemposition me)))
               (theObjects (inputobjects me))
               (property (inputproperty me))
               (newValue (object ObjectDataRect))
               (its (funcall property (first theobjects)))
               )
          (when theguy
            (if (listp its)
              (progn
                (setf its (copy-list its))
                (setf (nth (1- theguy) its) newvalue))
              (setf its newvalue))
            (UndoableSet property theObjects its :confirmation t)
            (bringtofront (sk8::window me))
            (setf (keytarget (sk8::window me)) me)
            (setf (inputobjects me) (inputobjects me))
            )))
       ))))


(resized ValueEditorPicker)
(setf (selectionborder ValueEditorPicker) 0)

(define-handler SetOutputString (ValueEditorPicker)
  (when (and (inputproperty me) (inputobjects me)
             (items me) (typep (aref (items me) 0 0) 'sk8::object))
    (let ((sel (first (selecteditems me))))
      (withActorLocked ((sk8::window me))
        (setf (outputstring me) (concatenate 'string "the " 
                                             (remove-if #'(lambda (x) (char= x #\')) (objectstring (inputproperty me) :project (targetproject me)))
                                             " of "
                                             (objectstring (first (inputobjects me)) :project (targetproject me))))
        (dolist (i (deepcontents (sk8::window me)))
          (when (and (inheritsfrom i objectpicker) 
                     (memq sel (items i)))
            (setf (selecteditems i) sel)
            (selectioncompleted i))
          )))))

(define-handler doubleclick (ValueEditorPicker)
  (SetOutputString me))

(new textlist :objectname "ValueEditor" :project sk8)
(setf (pickerprototype ValueEditor) ValueEditorPicker)
(setf (text (titlebar ValueEditor)) "Value")

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  PropertyControlPanel  Allows complete of the metaproperties of a property
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------


(new browserpaper :objectname "PropertyControlPanel" :project sk8
     :prototype t
     :properties '(InputObject InputProperty))
(setf (fillcolor PropertyControlPanel) graytone90)

(addInputPort PropertyControlPanel 'InputObject :signature Object)
(addInputPort PropertyControlPanel 'InputProperty :signature Symbol)


(new valuetext :objectname "PCPvaluetext" :project sk8 :container PropertyControlPanel)
(setf (textsize PCPvaluetext) 9)
(setf (textfont PCPvaluetext) "geneva")
(tagpart PropertyControlPanel PCPvaluetext 'valuetext)

;;;****THIS SHOULD BE IN 01-object or something...
(define-sk8-function ObjectFrom nil (prop obj)
  (let (theval)
    (dolist (i (cons obj (ancestors obj)))
      (when (assq prop (mf::class-direct-instance-slots (class-of i)))
        (setf theval i)
        (return)))
    theval))

(new checkbox :objectname "PCPInheritableBox" :project sk8 :container PropertyControlPanel :text "Propagating")
(setf (text PCPInheritableBox) "Propagating")
(tagpart PropertyControlPanel PCPInheritableBox 'InheritableCheck)
(define-handler check (PCPInheritableBox)
  (let* ((ch? (checked me))
         (prop (inputproperty (container me)))
         (obj (inputobject (container me)))
         (objFrom (ObjectFrom prop obj)))
    (if (or (memq prop (virtualproperties obj))
            (not (slot-exists-p obj prop))
            (mf::find-handler prop obj))
      (progn
        (messagetouser "This property is virtual!" :beep t)
        (setf (checked me) nil))
      (if (and obj 
                 prop
                 objfrom
                 (or (eq obj objfrom)
                     (yesornodialog (concatenate 'string "This property is inherited from " 
                                                 (objectstring objfrom :project (targetproject (container me)))
                                                 ".  Do you wish to make this property "
                                                 (if (checked me)
                                                   "not propagate?"
                                                   "propagate?")) :cancel nil)))
        (progn
          (call-next-method)
          (if (checked me)
            (makePropertyPropagate obj prop)
            (makePropertyNotPropagate obj prop)))
        (setf (checked me) ch?)))))

(new checkbox :objectname "PCPPrivateBox" :project sk8 :container PropertyControlPanel  :text "Public")
(setf (text PCPPrivateBox) "Private")
(tagpart PropertyControlPanel PCPPrivateBox 'PrivateCheck)
(define-handler check (PCPPrivateBox)
  (let* ((ch? (checked me))
         (prop (inputproperty (container me)))
         (obj (inputobject (container me)))
         (objFrom (ObjectFrom prop obj)))
    (if (memq prop (virtualproperties obj))
      (progn
        (messagetouser "This property is virtual!" :beep t)
        (setf (checked me) nil))
      (if (and obj 
                 prop
                 objfrom
                 (or (eq obj objfrom)
                     (yesornodialog (concatenate 'string "This property is inherited from " 
                                                 (objectstring objfrom :project (targetproject (container me)))
                                                 ".  Do you wish to make this property "
                                                 (if (checked me)
                                                   "public?"
                                                   "private?")) :cancel nil)))
        (progn
          (call-next-method)
          (setf (private objfrom :property prop)
                (checked me))
          )
        (setf (checked me) ch?)))))


(new roundrect :objectname "PCPPropagate" :project sk8 :container PropertyControlPanel)
(setf (text PCPPropagate) "Propagate My Value")
(tagpart PropertyControlPanel PCPPropagate 'PropagateButton)
(define-handler click (PCPPropagate)
  (let ((io (inputobject (container me)))
        (ip (inputproperty (container me))))
    (when (and io ip 
               (yesornodialog "Do you wish to propagate the value of this property to all of its descendants?" :cancel nil))
      (propagateValue ip :from io)
      (play ModalSound)
      t)))

(new roundrect :objectname "PCPReinherit" :project sk8 :container PropertyControlPanel)
(setf (text PCPReinherit) "Reinherit Value")
(tagpart PropertyControlPanel PCPReinherit 'ReinheritButton)
(define-handler click (PCPReinherit)
  (let ((io (inputobject (container me)))
        (ip (inputproperty (container me)))
        prop?
        par
        val)
    (when (and io ip)
      (setf prop? (propagatableValue io ip))
      (unless prop?
        (dolist (i (parents io))
          (when (memq ip (properties i))
            (setf par i)
            (return)))
        (setf val (funcall ip par)))
      (when (yesornodialog (if prop?
                             "Do you wish to have this property dynamically reinherit it's value?"
                             (concatenate 'string "Do you wish to set the value of this property to the value of it's parent, which is "
                                          (objectstring val :project (targetproject (container me)))
                                          "?"))
                           :cancel nil)
        (propagateValue ip :to io)
        (setf (inputproperty (valuetext (container me))) (inputproperty (valuetext (container me))))
        (play ModalSound)
        t))))

(new roundrect :objectname "PCPEditParent" :project sk8 :container PropertyControlPanel)
(setf (text PCPEditParent) "Edit Parent")
(tagpart PropertyControlPanel PCPEditParent 'EditParent)
(define-handler click (PCPEditParent)
  (let ((io (inputobject (container me)))
        (ip (inputproperty (container me)))
        ok)
    (dolist (i (parents io))
      (when (memq ip (properties i))
        (setf (inputobject (container me)) i)
        (setf (inputproperty (container me)) ip)
        (setf ok t)
        (return)))
    (if ok 
      (play ModalSound)
      (messagetouser (concatenate 'string
                                  "None of the parents of " (objectstring io  :project (targetproject (container me))) " have the property "
                                  (simpleobjectstring ip)) :beep t))
    ))

(new roundrect :objectname "PCPGetterButton" :project sk8)
(setf (container PCPGetterButton) PropertyControlPanel)
(tagpart PropertyControlPanel PCPGetterButton 'GetterButton)
(setf (text PCPGetterButton) "Edit Getter...")
(define-handler click (PCPGetterButton)
  (edithandlerdialog (inputobject (container me)) 
                     :handlername (inputproperty (container me)) ))

(new roundrect :objectname "PCPSetterButton" :project sk8)
(setf (container PCPSetterButton) PropertyControlPanel)
(tagpart PropertyControlPanel PCPSetterButton 'SetterButton)
(setf (text PCPSetterButton) "Edit Setter...")
(define-handler click (PCPSetterButton)
  (edithandlerdialog (inputobject (container me)) 
                     :handlername (setter (inputproperty (container me))) ))



(define-handler (setf inputobject) (theval PropertyControlPanel)
  (when (listp theval) (setf theval (first theval)))
  (sk8::setValue 'inputobject me theval)
  (setf (inputobjects (valuetext me)) (list theval))
  )

(define-handler (setf inputproperty) (theval PropertyControlPanel)
  (sk8::setValue 'inputproperty me theval)
  (withActorLocked ((sk8::window me))
    (let ((io (inputobject me)))
      (if (and theval (memq theval (properties io)))
        (progn
          (if (propagatableValue io theval)
            (setf (text (reinheritbutton me)) "Reinherit Value")
            (setf (text (reinheritbutton me)) "Get Parent's Value"))
          (setf (inputproperty (valuetext me)) theval)
          (setf (checked (InheritableCheck me)) (propagatableValue io theval))
          (setf (checked (PrivateCheck me)) (private io :property theval))
          )
        (progn
          (setf (inputproperty (valuetext me)) theval)
          (setf (checked (InheritableCheck me)) nil)
          (setf (checked (PrivateCheck me)) nil)
          )))))

(define-handler resized (PropertyControlPanel)
  (sk8-multival-bind (hsize vsize) (size me)
    (setBoundsrect (GetterButton me) (- hsize 280) 10 (- hsize 180) 30)
    (setBoundsrect (SetterButton me) (- hsize 160) 10 (- hsize 10) 30)
    (setBoundsrect (valuetext me) 5 50  (- hsize 5) 70)
    (setf (left (InheritableCheck me) :resizing nil) 7)
    (setf (top (InheritableCheck me) :resizing nil) 75)
    (setf (left (PrivateCheck me) :resizing nil) (+ 10 (right (inheritablecheck me))))
    (setf (top (PrivateCheck me) :resizing nil) 75)
    (setBoundsrect (EditParent me) (- hsize 280) (- vsize 60) (- hsize 180) (- vsize 40))
    (setBoundsrect (ReinheritButton me) (- hsize 160) (- vsize 60) (- hsize 10) (- vsize 40))
    (setBoundsrect (PropagateButton me) (- hsize 160) (- vsize 30) (- hsize 10) (- vsize 10))
    ))

(setboundsrect propertycontrolpanel 100 100 391 266)

#|
Change History (most recent last):
	2	9/24/93	kleiman	various load problems corrected
	7	10/6/93	rod	Cleaned up handler references with Ruben's new handler functions
	306	 2/16/95	rod     	Fixing getlineposition of tablepicker.
	307	 2/16/95	rod     	clear reference work.
	311	 4/12/95	rod     	Fixing queryfield's dobookkeeping.  it did not
							handle object dropping properly.
	2  	 6/23/95	Hernan  	Brian's patch to dropped of the textField of QueryField.
	3  	 8/ 9/95	Brian   	
	4  	 8/14/95	Brian   	fixing bug in hierachicalobjectpicker.  items are
						reversed.
	5  	12/11/95	Brian   	fixing call to ss package
	6  	 1/ 9/96	Hernan  	Removing calls to set scriptType.
	7  	 1/ 9/96	Hernan  	Removing call to set breakpointsVisible.
	8  	 1/10/96	Hernan  	HandlerViewerText should be an EditorScriptEditText.
	9  	 1/17/96	Hernan  	Folding in the new compiler API.
	10 	 1/19/96	sidney  	removing/rewriting references to old sk8script compiler
	11 	 2/14/96	Brian   	removing search field as it is no longer used.	
	12 	 2/15/96	Brian   	removing references to DataObject.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/16/96	Build   	removing reference to nthItem
	4  	 4/16/96	brian   	removing (nthitem call.
	5  	 4/19/96	Brian   	fixing clear references.
	6  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	7  	 5/ 7/96	Hernan  	Fixing undefined function warnings.
	8  	 5/10/96	Brian   	Cleanup for new object system.
	9  	 5/10/96	Hernan  	(is-a theitem sk8::function) is not a good test for SK8 functionhood. 
						Replacing it with mf::function-sym-p.
	10 	 5/21/96	Brian   	
	11 	10/ 9/96	Hernan  	Changed selectionCompleted of ValueEditorPicker to call
						showSelection with the right arguments.
	12 	10/ 9/96	Hernan  	Calling showselection with the right args.
	13 	10/18/96	Brian   	wrapping evaluation of translation in ignore-errors.
	14 	11/22/96	Brian   	
	15 	11/25/96	Brian   	
	16 	12/17/96	Brian Roddy	adding method to generate description of carried
						object to the dataRects.
	17 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
