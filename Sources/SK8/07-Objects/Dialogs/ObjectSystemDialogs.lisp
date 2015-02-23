;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8dev)

(provide "OBJECTSYSTEMDIALOGS")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "HALO" "objects;Shapes:Halo")
(require "PICKERMENU" "objects;Widgets:PickerMenu")
(require "CHECKBOX" "objects;Widgets:Checkbox")
(require "YESORNODIALOGBOX" "objects;Dialogs:YesOrNoDialogBox")

;;; Requires the compiler interface (calls editHandler).

;;; _______________________________ 
;;; Helpers. 
;;; _______________________________ 

(define-sk8-function EditHandlerDialog nil (obj &key HandlerName Qualifier ((:project inproject))
                                                   )
  (withcursor watchcursor
    (when (listp obj) (setf obj (car obj)))
    #|
    (let ((theName (objectname obj)))
      (Unless theName
        (When (YesOrNoDialog
               (Format Nil "You must name this object before you can create a new handler for it.~
                            ~%Do you wish to name now?")
               :cancel nil)
          (ObjectNameDialog obj)
          (setf theName (objectname obj))))
      (when theName
|#      
    (if HandlerName
      (edithandler (or inproject (project obj)) HandlerName obj Qualifier nil)
      (addhandlerdialog obj))))

;;*** this may not work, but the easiest way to find out is to try it. It used to call edit-definition
;; using the class-name of method-specializers. Since we are using unnamed calsses, I'll simply
;; try passing in the actual classes and see what happens
(define-sk8-function EditHandlerObjectDialog nil (handlerObj &key obj)
  (unless (if (symbolp handlerObj)
            (editHandlerObject (symbol-function handlerObj))
            (editHandlerObject handlerObj))
    (if (optionKeyDown)
      (if (symbolp handlerObj)
        (edit-definition handler 'function)
        (edit-definition (method-name handlerObj) 'method
                         (method-specializers handlerObj)))
      (if (and obj (neq obj (object handlerobj)))
        (if (yesornodialog "Would you like to redefine this handler locally?" :cancel nil)
          (edithandlerdialog obj :handlername (name handlerobj) ))
        ))))

;;; _____________________________________________________________________________
;;;                              AddHandlerDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "AddHandlerDialogBox" :project SK8
     :properties '((creationObjects :value nil)
                   (creationProject :value nil)))
(setlocation AddHandlerDialogBox 333 205)
(setSize AddHandlerDialogBox 400 120)

(new DialogBoxLabel :project SK8 :container AddHandlerDialogBox :location '(65 19) :text "New Handler for")
(new DialogBoxLabel :project SK8 :container AddHandlerDialogBox :location '(83 39) :text "In Project")
(new DialogBoxLabel :project SK8 :container AddHandlerDialogBox :location '(83 63) :text "With Name")
(new DialogBoxDisplayRectangle :project SK8 :size '(260 20) :container AddHandlerDialogBox  :location '(263 18) :objectName "NHDBTheObjectName")
(new DialogBoxDisplayRectangle :project SK8 :size '(260 20) :container AddHandlerDialogBox :location '(263 40) :objectName "NHDBTheProjectName")
(new DialogBoxEditText :project SK8 :container AddHandlerDialogBox :objectName "NHDBTheHandlerNameGetter")
(setboundsrect NHDBTheHandlerNameGetter 133 55 363 73)


(new halo :fillcolor black :project SK8 :objectname "NHDBHighlighter")
(setf (container NHDBHighlighter) AddHandlerDialogBox)
(surroundobject NHDBHighlighter NHDBTheHandlerNameGetter :outset '(1 1))
(define-handler activatetext (NHDBTheHandlerNameGetter)
  (call-next-method)
  (surroundobject NHDBHighlighter me :outset '(1 1)))

(define-handler keyDown (NHDBTheHandlerNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (or (eq theChar #\Enter) (eq theChar #\Return))
      (click NHDBCreateAddHandler)
      (call-next-method))))


#|
(let* ((hands (completehandlername (text me) :any nil 
                                           :onobject (if (listp (creationObjects AddHandlerDialogBox))
                                                       (car (creationObjects AddHandlerDialogBox))
                                                       (creationObjects AddHandlerDialogBox))
                                           :project (creationProject AddHandlerDialogBox)))
               (num (length hands)))
          (if hands
            (if (= 1 num)
              (setf (text me) (car hands))
              (setf (text me) (or (SelectFromCollectionDialog hands :title "Choose a handler name:")
                                    (text me))))
            (ed-beep)))
|#


(new pickermenu :objectname "NHDBEventMenu" :project SK8)
(setf (container NHDBEventMenu) AddHandlerDialogBox)
(setf (text NHDBEventMenu) "")
(define-handler items (NHDBEventMenu)
  (delete-duplicates (sort (append (mapcar #'simpleobjectstring (mapcar #'name (localhandlers (car (creationObjects AddHandlerDialogBox)))))
                                   (if (is-a (car (creationObjects AddHandlerDialogBox)) actor)
                                     (list "activate"
                                           "autoKey" 
                                           "draggingMouseWithin"
                                           "click"
                                           "commandKeyEvent"
                                           "connected"
                                           "deactivate"
                                           "dragged"
                                           "draggingMouseEnter"
                                           "draggingMouseLeave"
                                           "drop"
                                           "dropped"
                                           "doubleClick"
                                           "idle"
                                           "initialize"
                                           "keyDown"
                                           "keyUp"
                                           "mouseDown"
                                           "mouseEnter"
                                           "mouseLeave"
                                           "mouseUp"
                                           "mouseWithin"
                                           "moved"
                                           "resized")
                                     nil))
                           #'string<)
                     :test #'string-equal)
  )
(define-handler menuselect (NHDBEventMenu)
  (withactorLocked (NHDBTheHandlerNameGetter)
    (setf (text NHDBTheHandlerNameGetter) (selecteditem me))
    (setselection NHDBTheHandlerNameGetter 0 -1)
    (click NHDBCreateAddHandler)))
(setf (left NHDBEventMenu :resizing nil) (+ 2 (right NHDBTheHandlerNameGetter)))
(setf (top NHDBEventMenu :resizing nil) (top NHDBTheHandlerNameGetter))

(new DialogBoxCancelButton :objectName "NHDBCancelAddHandler" :project SK8 :container AddHandlerDialogBox)
(define-handler click (NHDBCancelAddHandler)
  (exitmodalstate nil))
(new DialogBoxHighlightedButton :objectname "NHDBCreateAddHandler" :project SK8 
     :container AddHandlerDialogBox :text "Create")

(setboundsrect NHDBCancelAddHandler 220 92 290 110)
(setboundsrect NHDBCreateAddHandler 300 88 378 114)

(defun Break-Into-Words (str)
  (let (theWords)
    (mapWordChunks str nil nil #'(lambda (s e) (push (subseq str s e) theWords)))
    (nreverse theWords)))

(define-handler click (NHDBCreateAddHandler)
  (let* ((name (text NHDBTheHandlerNameGetter))
         (co (creationObjects AddHandlerDialogBox))
         (words (Break-Into-Words name))
         (numwords (length words)))
    (when (> (length name) 0)
      (setf (Highlight me) t)
      (if (or (= numwords 1)
              (and (= numwords 2) (or (string-equal (car words) "set") (string-equal (car words) "with")))
              (and (= numwords 3) (string-equal (cadr words) "for") (string-equal (caddr words) "mods"))
              )
        (setf name (intern-legal-handlerName name (SK8::package (creationProject AddHandlerDialogBox)) nil))
        (setf name nil)
        )
      (cond
       ((memq name (quote (A AFTER ALL AM AN AND ANY ANYTHING APPLY ARE AREN ARENT AS AT BACK BEFORE BEGINNING BEHIND BODY BUT BY BYREFERENCE BYVALUE 
                             CALL CALLIN CALLSPEC CLEANUP SK8::Condition CONSTANT CONTAIN CONTAINED CONTAINS COPY DEFAULTING disposeMem DIV DO DOES DOESN DOESNT DON 
                             DONT EIGHTH ELEVENTH ELSE END ENDS EQUAL EQUALS sk8::error EVERY EVERYTHING EXIT FALSE FIELDS FIFTH FIRST FOR FOREIGN ForeignMemory FOREVER 
                             FOURTH FROM FRONT sk8::function GET GIVEN GLOBAL GREATER I IF IN INHERITED INITIALIZEMEM INOUT INSERT INTO IS ISN ISNT IT ITEM 
                             ITEMTYPE ITS ITSELF LAST LESS LOCAL ME MEMARRAY MemHandle MemPointer MEMRECORD MIDDLE sk8::mod MY MYSELF NAMED ND NEW NEXT NINTH NO
                             NOT numItems Object OF ON ONE OR OUT OVER PASS PRIVATE Project RANGE RD REGA0 REGA1 REGA2 REGA3 REGA4 REGA5 REGD0 REGD1 REGD2 
                             REGD3 REGD4 REGD5 REGD6 REGD7 REMOVE REPEAT Result RETURN RETURNS SAME SECOND SET SEVENTH SIXTH ST START STARTS TENTH TH THAN 
                             THAT THE THEN THIRD THROUGH THRU TIMES TO TRUE TWELFTH TYPE TYPENAME UNLESS UNTIL USING WAIT WHERE WHETHER WHILE WHOSE WITH WITHOUT 
                             )))   ;;;; OJO, this list should be in a global...
        (messageToUser "Sorry, that name is a reserved word in the SK8Script Language and therefore cannot be used as a handler name." :beep t)
        )
       (name
        (let ((qualifier nil)
              (theproj (creationProject AddHandlerDialogBox)))
          (setf (container AddHandlerDialogBox) nil)
          (unless (listp co) (setf co (list co)))
          (dolist (i co)
            (editHandlerdialog i :handlername name :project theproj :qualifier qualifier))
          (setf (Highlight me) nil)
          (exitmodalstate name)))  
       ;;;*** WE ARE RETURNING THE NAME RATHER THAN THE HANDLER OBJECT
       ;;; HOW CAN WE GET THE HANDLER OBJECT????
       (t
        (MessageToUser "Please enter a valid handler name.  Sample valid handler names are \"foo~\", \"set foo\", \"with foo\", and \"foo for mods\".")))
      (setf (Highlight me) nil))
    ))

#| - Problem 1166589
(new DialogBoxLabel :project SK8 :container AddHandlerDialogBox :location '(37 100) :text "Of Type") 

;;; A menu to select the type of handler we want to make. The options are
;;; prime, before and after...

(new menu :objectname "NHDBHandlerTypeMenu" :project SK8
     :text "Prime" :textsize 12 :location '(115 100) :container AddHandlerDialogBox)
(new menuItem :objectname "NHDBHandlerType" :project SK8)
(define-handler menuSelect (NHDBHandlerType)
  (setf (text (menu me)) (text me)))
(new NHDBHandlerType :objectname "NHDBHandlerTypePrime" :project SK8 :text "Prime" :menu NHDBHandlerTypeMenu)
(new NHDBHandlerType :objectname "NHDBHandlerTypeBefore" :project SK8 :text "Before" :menu NHDBHandlerTypeMenu)
(new NHDBHandlerType :objectname "NHDBHandlerTypeAfter" :project SK8 :text "After" :menu NHDBHandlerTypeMenu)
|#

(define-handler EnteringStage (AddHandlerDialogBox)
  (call-next-method)
  (setf (keyTarget  me) NHDBTheHandlerNameGetter)
  (let ((objs (creationObjects me)))
    (setf (text NHDBTheObjectName) (objectString (if (eql 1 (length objs)) (car objs) objs) :project (creationProject me)))
    (setSelection NHDBTheHandlerNameGetter 0 -1)
    ;    (setf (text NHDBHandlerTypeMenu) "Prime")
    (setf (text NHDBTheProjectName) (objectString (creationProject me) :project (creationProject me)))))


(define-sk8-function AddHandlerDialog nil (ObjList)
  (when ObjList
    (let (obj)
      (unless (listp ObjList) (setf ObjList (list ObjList)))
      (setf (creationObjects AddHandlerDialogBox) ObjList)
      (setf obj (car ObjList))
      (setf (creationProject AddHandlerDialogBox)
            (if (inheritsFrom obj project) obj (project obj)))
      (setf (location AddHandlerDialogBox) (mainmonitorcenter))
      (modalDialog AddHandlerDialogBox))))

;;; _____________________________________________________________________________
;;;                              AddPropertyDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "AddPropertyDialogBox" :project SK8
     :properties '((creationObjects :value nil)))
(setlocation AddPropertyDialogBox 333 205)
(setSize AddPropertyDialogBox 387 93)

(new DialogBoxLabel :project SK8 :container AddPropertyDialogBox :location '(50 19) :text "New Property")
(new DialogBoxLabel :project SK8 :container AddPropertyDialogBox :location '(84 42) :text "For")
(new DialogBoxDisplayRectangle :project SK8 :container AddPropertyDialogBox 
     :objectName "NPDBThePropertyName")
(setboundsRect NPDBThePropertyName 105 31 377 50)
(new DialogBoxEditText :project SK8 :container AddPropertyDialogBox 
     :objectName "NPDBThePropertyNameGetter")
(setBoundsRect NPDBThePropertyNameGetter 105 8 377 27)
(new halo :fillcolor black :project SK8 :objectname "NPDBHighlighter")
(setf (container NPDBHighlighter) AddPropertyDialogBox)

(surroundobject NPDBHighlighter NPDBThePropertyNameGetter :outset '(1 1))
(setf (insetsize NPDBHighlighter) '(2 2))
(define-handler activatetext (NPDBThePropertyNameGetter)
  (call-next-method)
  (surroundobject NPDBHighlighter me :outset '(1 1)))

(define-handler keyDown (NPDBThePropertyNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (eq theChar #\Space) 
      (ed-beep)
      (if (or (eq theChar #\Enter) (eq theChar #\Return))
        (click NPDBAddAddProperty)
        (call-next-method)))))

(new checkBox :objectname "NPDBInheritableProperty" :project SK8)
(setf (checked NPDBInheritableProperty) nil)
(setf (text NPDBInheritableProperty) "Propagating")
(setf (container NPDBInheritableProperty) AddPropertyDialogBox)
(setlocation NPDBInheritableProperty 60 74)

(new checkBox :objectname "NPDBPrivateProperty" :project SK8)
(setf (checked NPDBPrivateProperty) nil)
(setf (text NPDBPrivateProperty) "Private")
(setf (container NPDBPrivateProperty) AddPropertyDialogBox)
(setlocation NPDBPrivateProperty 155 74)


(new DialogBoxCancelButton :objectName "NPDBCancelAddProperty" :project SK8)
(setf (container NPDBCancelAddProperty) AddPropertyDialogBox)
(setBoundsRect NPDBCancelAddProperty 217 65 287 83)
(define-handler click (NPDBCancelAddProperty)
  (exitmodalstate nil))

(new DialogBoxHighlightedButton :objectname "NPDBAddAddProperty" :project SK8)
(setf (container NPDBAddAddProperty) AddPropertyDialogBox)
(setf (text NPDBAddAddProperty) "Create")
(setBoundsRect NPDBAddAddProperty 297 61 375 87)

(define-handler click (NPDBAddAddProperty)
  (let* ((theObj (creationObjects AddPropertyDialogBox))
         (str (text NPDBThePropertyNameGetter))
         (symstr (intern-legal-varName str (SK8::package (project (car theObj))) nil))
         haves havenots (oktogo t))
    (unless (listp theobj) (setf theobj (list theobj)))
    (setf (Highlight me) t)
    (if (not symstr)
      (progn
        (setf oktogo nil)
        (MessageToUser "Please enter a valid property name or press cancel to leave the object unchanged" :beep t))
      (progn
        (setf haves (remove-if-not #'(lambda (x) (memq symstr (properties x))) theObj))
        (setf havenots (remove-if #'(lambda (x) (memq symstr (properties x))) theObj))
        (if haves
          (if havenots
            (progn
              (ed-beep)
              (setf oktogo (Yesornodialog (concatenate 'string
                                                       (objectstring (if (eql 1 (length haves)) (car haves) haves) :project (project (car haves)))
                                                       " already" 
                                                       (if (eql 1 (length haves)) " has" " have") " a property " str ".  Add " str " to "
                                                       (objectstring (if (eql 1 (length havenots)) (car havenots) havenots) :project (project (car havenots)))
                                                       "?"))))
            (progn
              (MessageToUser (concatenate 'string
                                          (objectstring (if (eql 1 (length haves)) (car haves) haves)
                                                        :project (project (car haves)))
                                          " already" (if (eql 1 (length haves)) " has" " have") 
                                          " a property " str ".") :beep t)
              (setf oktogo nil))))))
    (when oktogo
      (dolist (i havenots)
        (addProperty i symstr :propagatedValue (checked NPDBInheritableProperty) :private (checked NPDBPrivateProperty)))
      (setf (container AddPropertyDialogBox) nil)
      (setf (Highlight me) nil)
      (exitModalState symstr)))
  (setf (Highlight me) nil))

(define-handler EnteringStage (AddPropertyDialogBox)
  (call-next-method)
  (let ((objs (creationObjects me)))
    (setf (checked NPDBPrivateProperty) nil
          (checked NPDBInheritableProperty) nil)
    (setf (keyTarget  me) NPDBThePropertyNameGetter)
    (setSelection NPDBThePropertyNameGetter 0 -1)
    (setf (text NPDBThePropertyName) (objectString (if (eql 1 (length objs)) (car objs) objs) :project (project (car objs))))))

;;;*** SHOULD HAVE AN INITIAL VALUE SLOT THAT IS SETTABLE VIA A KEYWORD AND THEN HAVE
;;; DRAG AND DROP OF PROPERTIES SET THIS AUTOMATICALLY...

(define-sk8-function AddPropertyDialog nil (ObjList &key (Property nil))
  (when ObjList
    (unless (listp ObjList) (setf ObjList (list ObjList)))
    (setf (creationObjects AddPropertyDialogBox) ObjList)
    (setf (location AddPropertyDialogBox) (mainmonitorcenter))
    (setf (text NPDBThePropertyNameGetter) (if property (simpleobjectstring property) ""))
    (modalDialog AddPropertyDialogBox)))

;;; _____________________________________________________________________________
;;;                               AddFunctionDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "AddFunctionDialogBox" :project SK8
     :properties '((creationObject :value nil)))
(setlocation AddFunctionDialogBox 333 205)
(setSize AddFunctionDialogBox 367 93)

(new DialogBoxLabel :project SK8 :container AddFunctionDialogBox :location '(55 18) :text "New Function")
(new DialogBoxLabel :project SK8 :container AddFunctionDialogBox :location '(60 41) :text "for Project")

(new DialogBoxDisplayRectangle :project SK8 :container AddFunctionDialogBox 
     :objectName "AFDBTheFunctionName")
(setboundsRect AFDBTheFunctionName 105 31 357 51)

(new DialogBoxEditText :project SK8 :container AddFunctionDialogBox 
     :objectName "AFDBTheFunctionNameGetter")
(setBoundsRect AFDBTheFunctionNameGetter 105 8 357 28)
(new halo :fillcolor black :project SK8 :objectname "AFDBHighlighter")
(setf (container AFDBHighlighter) AddFunctionDialogBox)
(surroundobject AFDBHighlighter AFDBTheFunctionNameGetter :outset '(1 1))
(define-handler activatetext (AFDBTheFunctionNameGetter)
  (call-next-method)
  (surroundobject AFDBHighlighter me :outset '(1 1)))

(define-handler keyDown (AFDBTheFunctionNameGetter thechar)
  (if (eq theChar #\Space) 
    (ed-beep)
    (if (or (eq theChar #\Enter) (eq theChar #\Return))
      (click AFDBAddFunctionButton)
      (call-next-method))))

(new DialogBoxCancelButton :objectName "AFDBCancelAddFunction" :project SK8)
(setf (container AFDBCancelAddFunction) AddFunctionDialogBox)
(setBoundsRect AFDBCancelAddFunction 199 64 269 82)
(define-handler click (AFDBCancelAddFunction)
  (exitmodalstate nil))

(new DialogBoxHighlightedButton :objectname "AFDBAddFunctionButton" :project SK8)
(setf (container AFDBAddFunctionButton) AddFunctionDialogBox)
(setf (text AFDBAddFunctionButton) "Create")
(setBoundsRect AFDBAddFunctionButton 279 60 357 86)

(define-handler click (AFDBAddFunctionButton)
  (let ((name (intern-legal-handlerName (text AFDBTheFunctionNameGetter) (SK8::package (creationObject AddFunctionDialogBox)) nil)))
    (setf (Highlight me) t)
    (if name
      (let ((theproj (creationObject AddFunctionDialogBox)))
        (setf (container AddFunctionDialogBox) nil)
        (editHandler theproj name nil nil nil)
        (setf (Highlight me) nil)
        (exitmodalstate))
      (MessageToUser "Please enter a valid function name."))
    (setf (Highlight me) nil)))

(define-handler EnteringStage (AddFunctionDialogBox)
  (call-next-method)
  (setf (keyTarget  me) AFDBTheFunctionNameGetter)
  (setSelection AFDBTheFunctionNameGetter 0 -1)
  (setf (text AFDBTheFunctionName) (objectString (creationObject me) :project (project (creationobject me)))))

(define-handler AddFunctionDialog (project)
  (setf (creationObject AddFunctionDialogBox) me)
  (setf (location AddFunctionDialogBox) (mainmonitorcenter))
  (modalDialog AddFunctionDialogBox))

;;; _____________________________________________________________________________
;;;                         AddVariableDialog and AddConstantDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "AddVariableDialogBox" :project SK8
     :properties '((creationObject :value nil)))
(setlocation AddVariableDialogBox 333 205)
(setSize AddVariableDialogBox 367 137)

(new rectangle :objectName "newVarHeader" :project SK8)

(setf (private newVarHeader) t)

(setf (container newVarHeader) AddVariableDialogBox)
(setf (textSize newVarHeader) 12)
(setf (textColor newVarHeader) black)
(setFramesize newVarHeader 0 0)
(setf (fillcolor newVarHeader) (fillcolor addvariabledialogbox))
(setf (text newVarHeader) "New Variable")
(Setf (textStyle newVarHeader) '(plain))
(Setf (textfont newVarHeader) ChicagoFont)
(setSize newVarHeader 100 20)
(setLocation newVarHeader 53 18)

(new DialogBoxLabel :project SK8 :container AddVariableDialogBox :location '(60 40) :text "With Name")
(new DialogBoxLabel :project SK8 :container AddVariableDialogBox :location '(53 63) :text "Initial Value")
(new DialogBoxLabel :project SK8 :container AddVariableDialogBox :location '(60 86) :text "To Project")

(new DialogBoxDisplayRectangle :project SK8 :container AddVariableDialogBox 
     :objectName "NVDBtheVarProjectName")
(setboundsRect NVDBtheVarProjectName 105 76 355 96)

(new DialogBoxEditText :project SK8 :container AddVariableDialogBox 
     :objectName "NVDBTheVarNameGetter")
(setBoundsRect NVDBTheVarNameGetter 105 30 355 50)

(define-handler keyDown (NVDBTheVarNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (eq theChar #\Space) 
      (ed-beep)
      (if (or (eq theChar #\Enter) (eq theChar #\Return))
        (click NVDBAddNewVar)
        (call-next-method)))))

(new DialogBoxEditText :project SK8 :container AddVariableDialogBox 
     :objectName "NVDBTheVarValueGetter")
(setBoundsRect NVDBTheVarValueGetter 105 53 355 73)

(define-handler keyDown (NVDBTheVarValueGetter thechar)
  (if (or (eq theChar #\Enter) (eq theChar #\Return))
    (click NVDBAddNewVar)
    (call-next-method)))

(new halo :fillcolor black :project SK8 :objectname "NVDBHighlighter")
(setf (container NVDBHighlighter) AddVariableDialogBox)
(surroundobject NVDBHighlighter NVDBTheVarNameGetter :outset '(1 1))
(define-handler activatetext (NVDBTheVarNameGetter)
  (call-next-method)
  (setselection me 0 -1)
  (surroundobject NVDBHighlighter me :outset '(1 1)))
(define-handler activatetext (NVDBTheVarValueGetter)
  (call-next-method)
  (setselection me 0 -1)
  (surroundobject NVDBHighlighter me :outset '(1 1)))
(define-handler deactivatetext (NVDBTheVarNameGetter)
  (call-next-method)
  (moveoffstage NVDBHighlighter))
(define-handler deactivatetext (NVDBTheVarValueGetter)
  (call-next-method)
  (moveoffstage NVDBHighlighter))

(new checkbox :project SK8 :container AddVariableDialogBox  :textfont ChicagoFont :textsize 12
     :objectName "NVDBTheVarPrivateCheck")
(setf (text NVDBTheVarPrivateCheck) "Private")
(setf (top NVDBTheVarPrivateCheck :resizing nil) 110)
(setf (left NVDBTheVarPrivateCheck :resizing nil) 5)

(new DialogBoxCancelButton :objectName "NVDBCancelNewVar" :project SK8)
(setf (container NVDBCancelNewVar) AddVariableDialogBox)
(setBoundsRect NVDBCancelNewVar 195 108 265 126)
(define-handler click (NVDBCancelNewVar)
  (exitmodalstate nil))


(new DialogBoxHighlightedButton :objectname "NVDBAddNewVar" :project SK8)
(setf (container NVDBAddNewVar) AddVariableDialogBox)
(setf (text NVDBAddNewVar) "Create")
(setBoundsRect NVDBAddNewVar 275 104 353 130)

(define-handler click (NVDBAddNewVar)
  (let* ((theProj (creationObject AddVariableDialogBox))
         (varName (text NVDBTheVarNameGetter))
         (symbolVarName (intern-legal-varName varName (SK8::package (creationObject AddVariableDialogBox)) nil)))
    (sk8-multival-bind (failure? errors locals globals result)
                       (translateScriptCommandOrExpr theProj (text NVDBTheVarValueGetter))
      (declare (ignore errors locals globals))
      (setf (Highlight me) t)
      (if (not symbolVarName)
        (MessageToUser "Please enter a valid variable name or press cancel." :beep t)
        (if failure?
          (MessageToUser"Syntax error in the value specification.")
          (let ((theValue (evaluateScriptTranslation result)))
            (if (string= (text newVarHeader) "New Variable")
              (sk8::define-sk8-var* symbolVarName 
                :initial-Value theValue :register t
                :private? (checked NVDBTheVarPrivateCheck)
                :project theProj)
              (sk8::define-sk8-constant*  symbolVarName theValue
                :register t
                :private? (checked NVDBTheVarPrivateCheck)
                :project theProj))
            (setf (container AddVariableDialogBox) nil)
            (setf (Highlight me) nil)
            (exitModalState symbolVarName))))
      (setf (Highlight me) nil))))

(define-handler EnteringStage (AddVariableDialogBox)
  (call-next-method)
  (setf (keyTarget  me) NVDBTheVarNameGetter)
  (setf (checked NVDBTheVarPrivateCheck) nil)
  (setSelection NVDBTheVarNameGetter 0 -1)
  (setf (text NVDBTheVarValueGetter) "false")
  (setf (text NVDBtheVarProjectName) (objectString (creationObject me) :project (project (creationObject me)))))

(define-handler AddVariableDialog (project)
  (setf (text newVarHeader) "New Variable")
  (setf (creationObject AddVariableDialogBox) me)
  (setf (location AddVariableDialogBox) (mainmonitorcenter))
  (modalDialog AddVariableDialogBox))

(define-handler AddConstantDialog (project)
  (setf (text newVarHeader) "New Constant")
  (setf (creationObject AddVariableDialogBox) me)
  (setf (location AddVariableDialogBox) (mainmonitorcenter))
  (modalDialog AddVariableDialogBox))


;;; _____________________________________________________________________________
;;;                             ObjectNameDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "ObjectNameDialogBox" :project SK8
     :properties '((creationObjects :value nil)
                   (creationProject :value nil)))
(setlocation ObjectNameDialogBox 333 205)
(setSize ObjectNameDialogBox 400 135)

(new DialogBoxLabel :project SK8 :container ObjectNameDialogBox :location '(60 20) :text "Give")
(new DialogBoxLabel :project SK8 :container ObjectNameDialogBox :location '(45 43) :text "In Project")
(new DialogBoxLabel :project SK8 :container ObjectNameDialogBox :location '(75 64) :text "The Object Name : " :objectname "ONLabel1")
(new DialogBoxDisplayRectangle :project SK8 :container ObjectNameDialogBox 
     :objectName "ONTheObjectName")
(setBoundsRect ONTheObjectName 88 10 390 30)
(contents objectnamedialogbox)
(new DialogBoxDisplayRectangle :project SK8 :container ObjectNameDialogBox 
     :objectName "ONTheProjectName")
(setboundsRect ONTheProjectName 88 33 390 53)

(new DialogBoxEditText :project SK8 :container ObjectNameDialogBox 
     :objectName "ONTheObjectNameGetter")
(setBoundsRect ONTheObjectNameGetter 88 75 390 95)

(new halo :fillcolor black :project SK8 :objectname "ONHighlighter")
(setf (container ONHighlighter) ObjectNameDialogBox)
(surroundobject ONHighlighter ONTheObjectNameGetter :outset '(1 1))
(define-handler activatetext (ONTheObjectNameGetter)
  (call-next-method)
  (surroundobject ONHighlighter me :outset '(1 1)))

(define-handler keyDown (ONTheObjectNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (eq theChar #\Space) 
      (ed-beep)
      (if (or (eq theChar #\Enter) (eq theChar #\Return))
        (click createObjectNameDialog)
        (call-next-method)))))

(new DialogBoxCancelButton :objectName "CancelObjectNameDialog" :project SK8)
(setf (container CancelObjectNameDialog) ObjectNameDialogBox)
(setBoundsRect CancelObjectNameDialog 152 107 222 125)
(define-handler click (CancelObjectNameDialog)
  (exitmodalstate nil))

(new DialogBoxButton :objectName "UnnameObjectNameDialog" :project SK8)
(setf (container UnnameObjectNameDialog) ObjectNameDialogBox)
(setBoundsRect UnnameObjectNameDialog 232 107 302 125)
(setf (text UnnameObjectNameDialog) "Unname")
(define-handler click (UnnameObjectNameDialog)
  (withactorlocked (me)
    (setf (text ONTheObjectNameGetter) "")
    (click createObjectNameDialog)))

(new DialogBoxHighlightedButton :objectname "createObjectNameDialog" :project SK8)
(setf (container createObjectNameDialog) ObjectNameDialogBox)
(setf (text createObjectNameDialog) "Set")
(setBoundsRect createObjectNameDialog 312 103 390 129)

(define-handler click (createObjectNameDialog)
  (let* ((theObjs (creationObjects ObjectNameDialogBox))
         (str (text ONTheObjectNameGetter))
         (nullstr? (or (not str) (string= str "") (string= str "false")))
         (onlyOne (= (length TheObjs) 1))
         (ok? t)
         symstr)
    (setf (Highlight me) t)
    (unless nullstr?
      (dotimes (i (length theObjs))
        (setf symstr (intern-legal-varName (if onlyOne 
                                             str
                                             (concatenate 'string str (objectstring (1+ i))))
                                           (SK8::package (creationProject ObjectNameDialogBox)) nil))
        (cond
         ((and (boundp symstr) (neq (symbol-value symstr) (nth i theobjs)))
          (MessageToUser "That name is already in use.  Please enter a new name." :beep t)
          (setf ok? nil)
          (return))
         ((not symstr)
          (MessageToUser "Please enter a valid object name or press cancel to leave the ObjectName unchanged." :beep t)
          (setf ok? nil)
          (return)))
        ))
    (when ok? 
      (setf (container ObjectNameDialogBox) nil)
      (setf (Highlight me) nil)
      (setf (objectlist UndoableSetLog) theObjs)
      (setf (PropertyName UndoableSetLog) 'objectname)
      (setf (valuelist UndoableSetLog) (mapcar #'objectname theObjs))
      (dotimes (i (length theObjs))
        (setf (objectname (nth i theobjs)) (cond
                                            (nullstr?
                                             nil)
                                            (onlyOne 
                                             str)
                                            (t
                                             (concatenate 'string str (objectstring (1+ i)))))))
      (sk8::setInform system theObjs 'objectname)
      (exitModalState))
    (setf (Highlight me) nil)
    ))

(define-handler EnteringStage (ObjectNameDialogBox)
  (let ((lenny (length (creationObjects me))))
    (call-next-method)
    (setf (keyTarget  me) ONTheObjectNameGetter)
    (setf (text ONTheObjectNameGetter) (if (and (= 1 lenny) (objectname (car (creationobjects me))))
                                         (objectname (car (creationobjects me)))
                                         ""))
    (setSelection ONTheObjectNameGetter 0 -1)
    (setf (text ONTheObjectName) (if (= 1 lenny)
                                   (objectString (car (creationObjects me)) :project (creationProject me))
                                   (concatenate 'string (objectstring lenny) " Items")))
    (setf (text ONLabel1) (if (= 1 lenny)
                                   "The Object Name : "
                                   "Sequential Object Names Beginning With: "))
    (setf (left ONLabel1 :resizing nil) 5)
    (setf (text ONTheProjectName) (objectString (creationProject me) :project (creationProject me)))))

(define-sk8-function ObjectNameDialog nil (ObjList)
  (when ObjList
    (let (obj)
      (unless (listp ObjList) (setf ObjList (list ObjList)))
      (setf (creationObjects ObjectNameDialogBox) ObjList)
      (setf obj (car objlist))
      (setf (creationProject ObjectNameDialogBox)
            (if (inheritsFrom obj project) obj (project obj)))
      (setf (location ObjectNameDialogBox) (mainmonitorcenter))
      (modalDialog ObjectNameDialogBox))))

;;; _____________________________________________________________________________
;;;                              TagDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "TagDialogBox" :project SK8
     :properties '(creationObjects
                   creationProject
                   taggedobject))
(setlocation TagDialogBox 333 205)
(setSize TagDialogBox 400 127)

(new DialogBoxLabel :project SK8 :container TagDialogBox :objectname "TDBTaglabel" :text "Tag")
(setf (right TDBTaglabel :resizing nil) 60)
(setf (top TDBTaglabel :resizing nil) 10)
(new DialogBoxLabel :project SK8 :container TagDialogBox :objectname "TDBasthelabel" :text "as the")
(setf (right TDBasthelabel :resizing nil) 60)
(setf (top TDBasthelabel :resizing nil) 35)
(new DialogBoxLabel :project SK8 :container TagDialogBox :objectname "TDBoflabel" :text "of")
(setf (right TDBoflabel :resizing nil) 60)
(setf (top TDBoflabel :resizing nil) 60)
(new DialogBoxDisplayRectangle :project SK8 
     :container TagDialogBox  :objectName "TDBTheTaggedObjectName")
(setboundsrect TDBTheTaggedObjectName 65 10 390 30)
(new DialogBoxEditText :project SK8 :container TagDialogBox 
     :objectName "TDBTheTagNameGetter")
(setboundsrect TDBTheTagNameGetter 65 35 390 55)
(new DialogBoxDisplayRectangle :project SK8 
     :container TagDialogBox :objectName "TDBTheTaggeeName")
(setboundsrect TDBTheTaggeeName 65 60 364 80)

(new halo :fillcolor black :project SK8 :objectname "TDBHighlighter")
(setf (container TDBHighlighter) TagDialogBox)
(surroundobject TDBHighlighter TDBTheTagNameGetter :outset '(1 1))
(define-handler activatetext (TDBTheTagNameGetter)
  (call-next-method)
  (surroundobject TDBHighlighter me :outset '(1 1)))

(define-handler keyDown (TDBTheTagNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (or (eq theChar #\Enter) (eq theChar #\Return))
      (click TDBTagItButton)
      (if (eq theChar #\Space)
        (ed-beep)
        (call-next-method)))))


(new pickermenu :objectname "TDBContainerMenu" :project SK8)
(setf (container TDBContainerMenu) TagDialogBox)
(setf (text TDBContainerMenu) "")
(setboundsrect TDBContainerMenu 365 60 390 80)
(define-handler menuselect (TDBContainerMenu)
  (let ((sel (selecteditem me)))
    (withactorLocked (TDBTheTagNameGetter)
      (setf (text TDBTheTaggeeName) (objectstring sel :project (project sel)))
      (setf (taggedobject TagDialogBox) sel)
      (taggedobject TagDialogBox))))
(define-handler items (TDBContainerMenu)
  (containers (creationobjects TagDialogBox))
  )

(new DialogBoxCancelButton :objectName "TDBCancelTagButton" :project SK8 :container TagDialogBox)
(define-handler click (TDBCancelTagButton)
  (exitmodalstate nil))
(new DialogBoxHighlightedButton :objectname "TDBTagItButton" :project SK8 
     :container TagDialogBox :text "Tag")
(setboundsrect TDBCancelTagButton 220 97 290 115)
(setboundsrect TDBTagItButton 300 93 378 119)

(define-handler click (TDBTagItButton)
  (let ((name (text TDBTheTagNameGetter))
        (co (creationObjects TagDialogBox)))
    (when (> (length name) 0)
      (setf (Highlight me) t)
      (setf name (intern-legal-varName name (SK8::package (creationProject TagDialogBox)) nil))
      (if name
        (progn
          (tagpart (taggedobject tagdialogbox) co name)
          (setf (container ObjectNameDialogBox) nil)
          (setf (Highlight me) nil)
          (exitModalState name))      
        (MessageToUser "Please enter a valid property name."))
      (setf (Highlight me) nil))))


(define-handler EnteringStage (TagDialogBox)
  (call-next-method)
  (setf (keyTarget  me) TDBTheTagNameGetter)
  (let ((obj (creationObjects me)))
    (setf (text TDBTheTaggedObjectName) (objectString obj :project (project obj)))
    (setSelection TDBTheTagNameGetter 0 -1)
    (setf (taggedObject me) (container obj))
    (setf (text TDBTheTaggeeName) (objectString (taggedObject me)  :project (project obj)))))

(define-sk8-function TagDialog nil (ObjList)
  (when ObjList
    (if (listp ObjList) (setf ObjList (car ObjList)))
    (if (or (eq (container objlist) stage) (not (container objlist)))
      (messagetouser (concatenate 'string (objectstring objlist) " cannot be tagged as it does not have a container to receive the tag.") :beep t)
      (progn
        (setf (creationObjects TagDialogBox) ObjList)
        (setf (creationproject TagDialogBox) (project ObjList))
        (setf (location TagDialogBox) (mainmonitorcenter))
        (modalDialog TagDialogBox)))))

;;; _____________________________________________________________________________
;;;                              AddPortDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "AddPortDialogBox" :project SK8
     :properties '(inputobjects
                   inputproperty
                   porttype))
(setlocation AddPortDialogBox 333 205)
(setSize AddPortDialogBox 400 127)

(new DialogBoxLabel :project SK8 :container AddPortDialogBox :objectname "APDBAddlabel" :text "Add")
(setf (right APDBAddlabel :resizing nil) 60)
(setf (top APDBAddlabel :resizing nil) 10)
(new DialogBoxLabel :project SK8 :container AddPortDialogBox :objectname "APDBtothelabel" :text "to the")
(setf (right APDBtothelabel :resizing nil) 60)
(setf (top APDBtothelabel :resizing nil) 35)
(new DialogBoxLabel :project SK8 :container AddPortDialogBox :objectname "APDBoflabel" :text "of")
(setf (right APDBoflabel :resizing nil) 60)
(setf (top APDBoflabel :resizing nil) 60)


(new menu :objectname "APDBPortTypeMenu" :project SK8)
(setf (textlocation APDBPortTypeMenu) 'centerleft)
(setf (container APDBPortTypeMenu) AddPortDialogBox)
(new menuitem :objectname "APDBPortTypeMenuItem" :project SK8
     :properties '(porttype))
(define-handler menuselect (APDBPortTypeMenuItem)
  (withactorLocked (AddPortDialogBox)
    (setf (text (menu me)) (objectstring (porttype me)))
    (setf (porttype AddPortDialogBox) (porttype me))
    (setf (left APDBPortTypeMenu :resizing nil) 65)    ))
(define-handler update (APDBPortTypeMenuItem)
  (setf (text me) (objectstring (porttype me)))
)
(new APDBPortTypeMenuItem :project sk8 :menu APDBPortTypeMenu :portType inputPort)
(new APDBPortTypeMenuItem :project sk8 :menu APDBPortTypeMenu :portType outputPort)
;(new APDBPortTypeMenuItem :project sk8 :menu APDBPortTypeMenu :portType inputOutputPort)  ;;;;*****NEED TO ADD THIS FUNCTIONALITY TO THE PORT LAYER!!!!
(setf (top APDBPortTypeMenu :resizing nil) 10)
(setf (left APDBPortTypeMenu :resizing nil) 65)

(new DialogBoxDisplayRectangle :project SK8 :textsize 12 :container AddPortDialogBox  :textstyle '(plain)
     :objectName "APDBPnameRect")
(setboundsrect APDBPnameRect 65 35 390 55)
(new DialogBoxDisplayRectangle :project SK8 
     :container AddPortDialogBox :objectName "APDBOnameRect")
(setboundsrect APDBOnameRect 65 60 390 80)

(define-handler keyDown (AddPortDialogBox thechar)
  (if (or (eq theChar #\Enter) (eq theChar #\Return))
    (click APDBAddItButton)
    (call-next-method)))

(new DialogBoxCancelButton :objectName "APDBCancelButton" :project SK8 :container AddPortDialogBox)
(define-handler click (APDBCancelButton)
  (exitmodalstate nil))
(new DialogBoxHighlightedButton :objectname "APDBAddItButton" :project SK8 
     :container AddPortDialogBox :text "Add")
(setboundsrect APDBCancelButton 220 97 290 115)
(setboundsrect APDBAddItButton 300 93 378 119)

(define-handler click (APDBAddItButton)
  (let ((io (inputobjects AddPortDialogBox))
        (ip (inputproperty AddPortDialogBox))
        (ptype (porttype AddPortDialogBox)))
    (setf (Highlight me) nil)
    (cond 
     ((inheritsfrom ptype inputport) (dolist (i io) (addinputport i ip :prototype ptype)))
     ((inheritsfrom ptype outputPort) (dolist (i io) (addoutputPort i ip :prototype ptype)))
     ((inheritsfrom ptype inputOutputPort) (dolist (i io) (addinputOutputPort i ip :prototype ptype)))
     (t (error "Error:  No port type specified in addportdialog!")))
    (setf (container ObjectNameDialogBox) nil)
    (exitModalState ptype)   
    ))


(define-handler EnteringStage (AddPortDialogBox)
  (call-next-method)
  (setf (keyTarget  me) me)
  (let ((objs (inputobjects me)))
    (setf (text APDBOnameRect) (objectstring (if (eql (length objs) 1) (car objs) objs) :project (project (car objs))))
    (setf (text APDBPnameRect) (objectstring (inputproperty me)))
    (if (not (porttype me)) (setf (porttype me) inputport))
    (setf (text APDBPortTypeMenu) (objectstring (porttype me)))
    (setf (left APDBPortTypeMenu :resizing nil) 65)
    ))


(define-sk8-function AddPortDialog nil (prop objlist)
  (when ObjList
    (unless (listp ObjList) (setf ObjList (list ObjList)))
    (setf (inputobjects AddPortDialogBox) ObjList)
    (setf (inputproperty AddPortDialogBox) prop)
    (setf (location AddPortDialogBox) (mainmonitorcenter))
    (modalDialog AddPortDialogBox)))

;;; _____________________________________________________________________________
;;;                             Dispose Dialog
;;; _____________________________________________________________________________


(defun deepelcheck (el seq)
  (if (listp seq)
    (some #'(lambda (x) (deepelcheck el x)) seq)
    (eql el seq)))

(defun simpleObjectClear (theObj &key (force nil))
  (declare (ignore force))
  (withlockedcursor animatedclock
    (tickeventclock)
    (when (and (is-a theObj connector) (startactor theObj) (endactor theObj))
      (disconnect (startactor theObj) (endactor theObj)))
    (tickeventclock)
    (setf (objectname theobj) nil)
     (when (realproperty theobj 'container)
       (setf (container theobj) nil))
     (dolist (parent (parents theobj))
       (setf (knownchildren parent) (delete theobj (knownchildren parent))))
     (dolist (child (knownchildren theobj))
       (removeparent child theobj))
     (ps::copy-all-pending-handlers)
    (if (boundp 'UndoableSetLog) (clearreferences UndoableSetLog :objects theObj))))

(defun deepObjectClear (theObj &key (force nil))
  ;;;;search props consts and globs
  (let* ((proj (targetProject ui))
         (okprojs (ui::okprojects proj))
         props setter)
    (tickeventclock)
    (mapprojectobjects (targetproject ui)
                       #'(lambda (i)  
                           (tickeventclock)
                           (setf props (propertiesWithValue i theObj :test #'deepelcheck))
                           (dolist (j props)
                             (when (sk8::setterp j i)
                               (setf setter (or (gethash j ccl::%setf-function-names%) 
                                                (gethash j ccl::%setf-methods%)))
                               (unless force
                                 (sendtolog (concatenate 'string "Settting property "
                                                         (name j)
                                                         " of "
                                                         (objectstring i :project (project i)) 
                                                         " to false.")))
                               (multiple-value-bind (val1 err?)
                                                    (ignore-errors (funcall setter nil i))
                                 (declare (ignore val1))
                                 (when err?
                                   (messagetouser (concatenate 'string "Encountered an error while settting the "
                                                               (name j)
                                                               " property of "
                                                               (objectstring i :project (project i)) 
                                                               " to false."
                                                               (string #\newline)
                                                               (string #\newline)
                                                               "This property will still refer to the object."
                                                               "  You may manually set this property to an appropriate value which does not refer to the object.")
                                                  :oktext "Continue"
                                                  :width 300 
                                                  :beep t)
                                   (sendtolog (concatenate 'string "The property "
                                                               (name j)
                                                               " of "
                                                               (objectstring i :project (project i)) 
                                                               " still contains an uncleared reference."
                                                               ))
                                   ))
                               ))))
    (dolist (p okprojs)
      (dolist (i (globals p))
        (when (deepelcheck (symbol-value i) theObj)
          (setf (symbol-value i) nil)
          (unless force
            (sendtolog (concatenate 'string "Settting global "
                                  (name i)
                                  " to false.")))
          )))
    (dolist (p okprojs)
      (dolist (i (Constants p))
        (when (deepelcheck (symbol-value i) theObj)
          (unless force
            (sendtolog (concatenate 'string "Removing constant "
                                    (name i)
                                    ".")))
          (removeconstant p :name i)
          )))
    ))

(define-sk8-function clearAllReferences nil (obj &key (force nil))
  (let ((cnt 0))
    (unless force
      (withlockedcursor animatedclock
        (mf::map-all-descendants obj #'(lambda (x) (declare (ignore x)) (tickeventclock) (incf cnt)))))
    (when (or (= cnt 0) 
              (withcursor standardcursor
                (yesornodialog (concatenate 'string (objectstring obj :project (project obj)) " has " 
                                          (objectstring cnt) 
                                          (if (= cnt 1) 
                                            " descendant.  This"
                                            " descendants.  These")
                                          " will be cleared as well.  Do you wish to continue?")
                             :cancel nil)))
      (withlockedcursor animatedclock
        (mf::map-all-descendants obj #'(lambda (x) (simpleObjectClear x :force force)))
        (simpleObjectClear obj :force force)
        (mf::map-all-descendants obj #'(lambda (x) (deepObjectClear x :force force)))
        (deepObjectClear obj :force force)
        ;;(mf::map-all-descendants obj #'make-object-gcable)
        ;;(make-object-gcable obj)
        ))))

(define-sk8-function DisposeDialog nil (Objects &key
                                                   (textFont ChicagoFont)
                                                   (textsize 12)
                                                   (width 350)
                                                   (height nil)
                                                   (h nil)
                                                   (v nil)
                                                   )
  (unless (listp objects) (setf objects (list objects)))
  (dolist (i (copy-list objects))
    (when (is-a i actor)
      (setf objects (append objects (deepcontents i)))
      ))
  (setf objects (delete-duplicates objects :test #'eq))
  (if (YesOrNoDialog (concatenate 'string
                                  "Do you wish to clear standard references or all references to "
                                  (objectstring (if (eql 1 (length objects))
                                                  (car objects) objects
                                                  )
                                                :project (project (car objects))) "?")
                     :textfont textfont
                     :textsize textsize
                     :width width
                     :height height
                     :h h
                     :v v
                     :yestext "Standard"
                     :notext "All"
                     :cancel t)
    (withcursor watchcursor
      (mapc #'simpleObjectClear objects))
    (when (YesOrNoDialog "Warning: Clearing all references will search through an object's project and set references to that object to false.  This may result in errors later if other objects depend on your object and are not updated manually.  Do you wish to continue?"
                         :width 350
                         :cancel nil)
      (withcursor watchcursor
        (mapc #'clearAllReferences objects)))
    ))


;;; _____________________________________________________________________________
;;;                             Remove Property Dialog
;;; _____________________________________________________________________________

(define-sk8-function RemovePropertyDialog nil (Property Objects
                                                            &key
                                                            (textFont ChicagoFont)
                                                            (textsize 12)
                                                            (width 350)
                                                            (height nil)
                                                            (h nil)
                                                            (v nil)
                                                            (cancel nil))
  (if (some #'(lambda (x) (memq property (virtualproperties x))) objects)
    (progn
      (messagetouser  (concatenate 'string (objectstring property :project (project (car objects)))
                                   " is a virtual property and cannot be removed.") :beep t)
      nil)
    (progn
      (unless (listp objects) (setf objects (list objects)))
      (setf objects (mapcar #'(lambda (x) x) objects)) ;;; ***** This should find out where properties are from... 
      (if (YesOrNoDialog (concatenate 'string
                                      "Do you wish to remove the property "
                                      (objectstring property :project (project (car objects))) " from "
                                      (if (> (length objects) 1)
                                        (objectstring objects :project (project (car objects)))
                                        (objectstring (car objects) :project (project (car objects))))
                                      "?")
                         :textfont textfont
                         :textsize textsize
                         :width width
                         :height height
                         :h h
                         :v v
                         :cancel cancel)
        (progn
          (mapcar #'(lambda (x) (removeproperty x Property)) objects)
          t)
        nil))))

;;; _____________________________________________________________________________
;;;                             Remove Handler Dialog
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; NEEDS TO CHECK IF THE HANDLER IS ON THE OBJECTS, ETC.¬ ***
;;; _____________________________________________________________________________

(define-sk8-function RemoveHandlerDialog nil (thehandler
                                                  &key
                                                  (textFont ChicagoFont)
                                                  (textsize 12)
                                                  (width 350)
                                                  (height nil)
                                                  (h nil)
                                                  (v nil)
                                                  (cancel nil))
  (if (YesOrNoDialog (concatenate 'string
                                    "Do you wish to remove handler "
                                    (objectstring thehandler)  "?")
                       :textfont textfont
                       :textsize textsize
                       :width width
                       :height height
                       :h h
                       :v v
                       :cancel cancel)
    (progn
      (dolist (i (contents stage))
        (if (and (is-a i ScriptEditorWindow) (eq (inputhandler i) thehandler))
          (setf (container i) nil)))
      (RemoveHandler thehandler)
      t)
    nil))

;;; _____________________________________________________________________________
;;;                             RemoveConstantDialog
;;; _____________________________________________________________________________

(define-sk8-function RemoveConstantDialog nil (proj
                                                   &key
                                                   (name nil)
                                                   (textFont ChicagoFont)
                                                   (textsize 12)
                                                   (width 350)
                                                   (height nil)
                                                   (h nil)
                                                   (v nil)
                                                   (cancel nil))
  (when name
    (if (YesOrNoDialog (concatenate 'string
                                    "Do you wish to remove the constant "
                                    (objectstring name :project proj)  "?")
                       :textfont textfont
                       :textsize textsize
                       :width width
                       :height height
                       :h h
                       :v v
                       :cancel cancel)
      (progn
        (RemoveConstant proj :name name)
        t)
      nil)))


;;; _____________________________________________________________________________
;;;                             RemoveVariableDialog
;;; _____________________________________________________________________________

(define-sk8-function RemoveVariableDialog nil (proj
                                                   &key
                                                   (name nil)
                                                   (textFont ChicagoFont)
                                                   (textsize 12)
                                                   (width 350)
                                                   (height nil)
                                                   (h nil)
                                                   (v nil)
                                                   (cancel nil))
  (when name
    (if (YesOrNoDialog (concatenate 'string
                                    "Do you wish to remove the variable "
                                    (objectstring name :project proj)  "?")
                       :textfont textfont
                       :textsize textsize
                       :width width
                       :height height
                       :h h
                       :v v
                       :cancel cancel)
      (progn
        (removeVariable proj :name name)
        t)
      nil)))


;;; _____________________________________________________________________________
;;;                             RemoveFunctionDialog
;;; _____________________________________________________________________________

(define-handler RemoveFunctionDialog (SK8::Function
                                          &key
                                          (textFont ChicagoFont)
                                          (textsize 12)
                                          (width 350)
                                          (height nil)
                                          (h nil)
                                          (v nil)
                                          (cancel nil))
  (when (YesOrNoDialog (concatenate 'string
                                  "Do you wish to remove the function "
                                  (symbol-name (name me))  "?")
                     :textfont textfont
                     :textsize textsize
                     :width width
                     :height height
                     :h h
                     :v v
                     :cancel cancel)
    (RemoveFunction me)
    t))

;;; _____________________________________________________________________________
;;;                             Add Parent Dialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "AddParentDialogBox" :project SK8
     :properties '((creationObjects :value nil)))
(setlocation AddParentDialogBox 333 205)
(setSize AddParentDialogBox 367 93)

(new DialogBoxLabel :project SK8 :container AddParentDialogBox :location '(50 19) :text "Add Parent")
(new DialogBoxLabel :project SK8 :container AddParentDialogBox :location '(84 42) :text "To")
(new DialogBoxDisplayRectangle :project SK8 :container AddParentDialogBox 
     :objectName "AddParDBTheParentName")
(setboundsRect AddParDBTheParentName 105 31 357 50)
(new DialogBoxEditText :project SK8 :container AddParentDialogBox
     :objectName "AddParDBTheParentNameGetter")
(setBoundsRect AddParDBTheParentNameGetter 105 8 357 27)
(new halo :fillcolor black :project SK8 :objectname "AddParDBHighlighter")
(setf (container AddParDBHighlighter) AddParentDialogBox)

(surroundobject AddParDBHighlighter AddParDBTheParentNameGetter :outset '(1 1))
(setf (insetsize AddParDBHighlighter) '(2 2))
(define-handler activatetext (AddParDBTheParentNameGetter)
  (call-next-method)
  (surroundobject AddParDBHighlighter me :outset '(1 1)))

(define-handler keyDown (AddParDBTheParentNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (eq theChar #\Space) 
      (ed-beep)
      (if (or (eq theChar #\Enter) (eq theChar #\Return))
        (click AddParDBDoit)
        (call-next-method)))))

(new DialogBoxCancelButton :objectName "AddParDBCancel" :project SK8)
(setf (container AddParDBCancel) AddParentDialogBox)
(setBoundsRect AddParDBCancel 200 65 270 83)
(define-handler click (AddParDBCancel)
  (exitmodalstate nil))

(new DialogBoxHighlightedButton :objectname "AddParDBDoit" :project SK8)
(setf (container AddParDBDoit) AddParentDialogBox)
(setf (text AddParDBDoit) "Add")
(setBoundsRect AddParDBDoit 280 61 358 87)

(define-handler click (AddParDBDoit)
  (let* ((theObj (creationObjects AddParentDialogBox))
         (str (text AddParDBTheParentNameGetter))
         theValue (oktogo nil) returnvalue)
    (sk8-multival-bind (failure? errors locals globals result)
                       (translateScriptCommandOrExpr (project (car theObj)) str)
      (declare (ignore errors locals globals))
      (setf (Highlight me) t)
      (if (or (not str) (string= str "") failure?)
        (MessageToUser "Please enter a valid Parent name or press cancel to leave the object unchanged" :beep t)
        (progn
          (setf theValue (withproject (project (car theObj)) (evaluateScriptTranslation result)))
          ;;;parent error checking!!!!!!*********
          (setf oktogo t)))
      (when oktogo
        (setf returnValue (mapcar #'(lambda (x) (addparent x theValue)) theObj))
        (setf (container AddParentDialogBox) nil)
        (setf (Highlight me) nil)
        (exitModalState returnValue)))
    (setf (keytarget AddParentDialogBox) AddParDBTheParentNameGetter)
    (setf (Highlight me) nil)))

(define-handler EnteringStage (AddParentDialogBox)
  (call-next-method)
  (let ((objs (creationObjects me)))
    (setf (keyTarget  me) AddParDBTheParentNameGetter)
    (setSelection AddParDBTheParentNameGetter 0 -1)
    (setf (text AddParDBTheParentName) (objectString (if (eql 1 (length objs)) (car objs) objs)
                                                     :project (project (car objs))))))

(define-sk8-function AddParentDialog nil (ObjList Parent)
  (when ObjList
    (unless (listp ObjList) (setf ObjList (list ObjList)))
    (setf (creationObjects AddParentDialogBox) ObjList)
    (setf (location AddParentDialogBox) (mainmonitorcenter))
    (setf (text AddParDBTheParentNameGetter) (if Parent (simpleobjectstring Parent) ""))
    (modalDialog AddParentDialogBox)))

;;; _____________________________________________________________________________
;;;                          Remove Parent Dialog
;;; _____________________________________________________________________________
(define-sk8-function removeParentDialog nil (Objects Parent
                                                        &key
                                                        (textFont ChicagoFont)
                                                        (textsize 12)
                                                        (width 350)
                                                        (height nil)
                                                        (h nil)
                                                        (v nil)
                                                        (cancel nil))
  (unless (listp objects) (setf objects (list objects)))
  ;;;;**** ERROR CHECKING if it is a parent of all of the objects...
  (if (YesOrNoDialog (concatenate 'string
                                  "Do you wish to remove "
                                  (objectstring Parent :project (project (car objects))) " from the parents of "
                                  (if (> (length objects) 1)
                                    (objectstring objects  :project (project (car objects)))
                                    (objectstring (car objects)  :project (project (car objects))))
                                  "?")
                     :textfont textfont
                     :textsize textsize
                     :width width
                     :height height
                     :h h
                     :v v
                     :cancel cancel)
    (progn
      (mapcar #'(lambda (x) (removeparent x parent)) objects)
      t)
    nil))

#|
	Change History (most recent last):
	1	9/28/93	rod	
	2	9/29/93	kleiman	
	3	10/4/93	kleiman	Nothing! Forget it.
	4	10/8/93	rod	
	5	10/11/93	rod	Added Remove Property and Remove Handler Dialogs
	6	10/27/93	rod	
	7	11/1/93	rod	
	8	11/1/93	rod	
	9	11/1/93	rod	
	10	11/1/93	rod	
	12	11/8/93	rod	
	13	11/8/93	rod	
	14	11/8/93	rod	
	15	11/8/93	kleiman	
	16	11/17/93	rod	
	17	11/19/93	rod	
	18	12/2/93	rod	Added RemoveHandler to the removeHandlerDialog
				now that that works!
	19	12/2/93	rod	
	20	12/2/93	rod	
	21	12/2/93	kleiman	removeProperty args changed
	22	12/2/93	kleiman	removeProperty args changed
	23	12/3/93	rod	
	24	12/3/93	rod	
	25	12/10/93	kleiman	
	26	12/17/93	till	#.'s be gone: doubleclick, keyDown
	28	2/12/94	kleiman	name changes
	29	2/18/94	rod	
	31	2/22/94	kleiman	The Final Renaming for Alpha! (yeah, right...)
	32	2/23/94	kleiman	addproperty inheritable -> propagatedValue
	33	2/25/94	rod	
	34	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	35	2/26/94	rod	
	36	2/28/94	rod	Goodbye currentProject
	37	2/28/94	rod	objectstring stuff
	38	2/28/94	hernan	Avoiding calling dispose directly.
	39	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	40	3/7/94	rod	
	41	3/9/94	rod	Doing Project Switching and Reference Clearing.
	42	3/10/94	rod	
	43	3/10/94	rod	
	44	3/10/94	rod	
	45	3/10/94	rod	
	46	3/10/94	rod	
	47	3/10/94	rod	
	48	3/16/94	rod	
	49	3/22/94	rod	
	50	3/25/94	Hernan	Locking the text of the editText used by the 
				messageToUserDialogBox.
	51	3/30/94	rod	
	52	3/30/94	rod	
	53	4/1/94	rod	
	54	4/1/94	rod	Now having a global variable to specify the editor prototype.
	55	4/5/94	kleiman	code review: various objects marked private
	56	4/12/94	kleiman	removeFunctionDialog works with actual Function
				object instead of symbol only
	57	4/13/94	rod	Making sure objectnamedialog clears old name.
	58	4/13/94	rod	Objectnamedialog checks if name already in use.
	59	5/4/94	rod	Locking text of certain edit texts.
	60	5/4/94	rod	Making selectfromcollection picker have a nice
				default size.
	61	5/6/94	rod	
	62	5/6/94	rod	
	63	5/6/94	rod	
	64	5/9/94	rod	
	65	6/3/94	rod	
	66	6/8/94	kleiman	Problem 1166987
	67	6/10/94	kleiman	Problem 1166589: label and menus removed;
				click of NHDBCreateAddHandler and EnteringStage
				of AddHandlerDialogBox modified
	68	6/13/94	rod	1167702:  Fixing TagDialog Menu
	69	6/13/94	rod	1167903:  Added a private checkbox to the
				add variable/constant dialog.
	70	6/16/94	rod	1168903:  Inheritable -> Propagating
	71	6/17/94	rod	
	72	6/23/94	rod	
	73	6/27/94	rod	
	74	6/29/94	chip	case-saving-intern --> intern-symbol/intern-legal-varName/intern-legal-handlerName (for radar #107741)
	75	7/11/94	rod	1173469: fixing allowEmptyStrings option.
	76	7/13/94	rod	1174021
	77	7/14/94	rod	
	78	7/29/94	rod	
	79	8/1/94	rod	Fixing ObjectNameDialog so it can take more 
				than one object at a time.
	80	8/1/94	rod	Fixing bug regarding setting objectnames to false.
	81	8/2/94	rod	Fixing bug with non-activated button in
				get answer from user.
	82	8/2/94	rod	Adding "Unname" button to objectname dialog.
	83 	 8/31/94	rod     	
	84 	 8/31/94	Hernan  	Chicago -> ChicagoFont.
	85 	 8/31/94	Hernan  	Geneva -> GenevaFont
	86 	 9/ 2/94	rod     	
	87 	 9/12/94	rod     	
	88 	 9/12/94	rod     	
	89 	 9/16/94	rod     	
	90 	 9/20/94	rod     	Request to have objectnamer default to 
							objectname if one exists.
	91 	 9/20/94	rod     	
	92 	 9/21/94	rod     	
	93 	 9/28/94	rod     	1180093:  Fixing bug that objects can't have their names set to themselves again.
	94 	11/17/94	rod     	FIxing stupid bug in with handlers.  I can't count 
							to 4.
	95 	11/17/94	rod     	
	96 	11/17/94	rod     	Fixing addhandler to handle "x for mods".
	97 	11/17/94	rod     	removing leftover print statement.
	98 	12/ 3/94	rod     	Removing duplicates from handler menu popup.
	99 	12/ 3/94	rod     	Fixing error message.
	100	12/22/94	rod     	
	101	 1/16/95	rod     	making objectnamedialog send the appropriate
							updates.
	102	 1/18/95	rod     	Adding clear all references
	103	 1/18/95	rod     	
	104	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	105	 2/14/95	rod     	
	106	 2/14/95	rod     	Fixing watch cursors on the dispose dialog.
	107	 3/ 7/95	rod     	
	108	 3/ 8/95	rod     	Cleaning up edittexts and using prototypes.
	109	 3/13/95	sidney  	Cleaning up clearallreferences
	110	 3/23/95	rod     	Prettying up error message for dispose dialog.
	111	 3/23/95	rod     	
	112	 3/24/95	rod     	
	113	 4/ 7/95	rod     	Checking for a reserved word.
	113	 4/ 7/95	rod     	
	113	 4/ 7/95	rod     	
	114	 4/12/95	rod     	
	115	 4/13/95	rod     	adding alphabeticaldisplay option to 
							selectfromcollectiondialog.
	116	 4/17/95	rod     	Adding extra dialog about clear all references
							to make it painfully clear.
	117	 4/25/95	rod     	
	2  	12/11/95	Brian   	fixing call to ss package
	3  	12/18/95	sidney  	add sk8::condition and other overridden symbols to reserved word list
	4  	 1/17/96	Hernan  	Folding in the new compiler API.
	5  	 2/12/96	Brian   	Making the addhandler dialog add the handler when
						a menu item is chosen.
	6  	 2/15/96	Brian   	Adding Sk8 character checking.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 7/ 7/96	sidney  	changes for native PPC build
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
