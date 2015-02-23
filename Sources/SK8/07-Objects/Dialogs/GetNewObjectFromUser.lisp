;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETNEWOBJECTFROMUSER")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "HALO" "objects;Shapes:Halo")
(require "MESSAGETOUSER" "objects;Dialogs:MessageToUserDialogBox")

;;; Requires de compiler interface. 

;;; _____________________________________________________________________________
;;;                             GetNewFromUser(OBJECT)
;;; _____________________________________________________________________________

(new DialogBox :objectname "NewObjectDialogBox" :project SK8
     :properties '((theParent :value nil)
                   (CreationProject :value nil)
                   (Duplicate :value nil)))

(setlocation NewObjectDialogBox 333 205)
(setSize NewObjectDialogBox 390 113)

(new DialogBoxLabel :project SK8 :container NewObjectDialogBox :location '(110 17) :text "New" :objectname "NODBActionLabel")
(new DialogBoxLabel :project SK8 :container NewObjectDialogBox :location '(66 40) :text "With ObjectName")
(new DialogBoxLabel :project SK8 :container NewObjectDialogBox :location '(90 63) :text "In Project")

(new edittext :project SK8 :container NewObjectDialogBox  :textsize 12 :textstyle '(bold)
     :objectName "NODBTheParentName")

(setBoundsRect NODBTheParentName 135 7 370 27)

(define-handler keyDown (NODBTheParentName thechar)
  (when (OkSK8Character thechar) 
    (if (eq theChar #\Space) 
      (ed-beep)
      (if (or (eq theChar #\Enter) (eq theChar #\Return))
        (click NODBAddNewObject)
        (call-next-method)))))

(new edittext :project SK8 :container NewObjectDialogBox  :textsize 12  :textstyle '(bold)
     :objectName "NODBTheObjectNameGetter")

(setboundsRect NODBTheObjectNameGetter 135 30 370 50)

(define-handler keyDown (NODBTheObjectNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (eq theChar #\Space) 
      (ed-beep)
      (if (or (eq theChar #\Enter) (eq theChar #\Return))
        (click NODBAddNewObject)
        (call-next-method)))))

(new edittext :project SK8 :size '(260 18) :container NewObjectDialogBox  :textsize 12 :textstyle '(bold)
     :objectName "NODBTheProjectNameGetter")

(setBoundsRect NODBTheProjectNameGetter 135 53 370 73)

(define-handler keyDown (NODBTheProjectNameGetter thechar)
  (when (OkSK8Character thechar) 
    (if (eq theChar #\Space) 
      (ed-beep)
      (if (or (eq theChar #\Enter) (eq theChar #\Return))
        (click NODBAddNewObject)
        (call-next-method)))))

(bringtofront NODBTheParentName)
(sendtoback NODBTheProjectNameGetter)

(new halo :fillcolor highlighted :project SK8 :objectname "NODBHighlighter")

(setf (container NODBHighlighter) NewObjectDialogBox)
(surroundobject NODBHighlighter NODBTheObjectNameGetter)

(define-handler activatetext (NODBTheParentName)
  (call-next-method)
  (surroundobject NODBHighlighter me))

(define-handler activatetext (NODBTheObjectNameGetter)
  (call-next-method)
  (surroundobject NODBHighlighter me))

(define-handler activatetext (NODBTheProjectNameGetter)
  (call-next-method)
  (surroundobject NODBHighlighter me))

(define-handler deactivatetext (NODBTheParentName)
  (call-next-method)
  (moveoffstage NODBHighlighter))

(define-handler deactivatetext (NODBTheObjectNameGetter)
  (call-next-method)
  (moveoffstage NODBHighlighter))

(define-handler deactivatetext (NODBTheProjectNameGetter)
  (call-next-method)
  (moveoffstage NODBHighlighter))

(new DialogBoxCancelButton :objectName "NODBcancelNewObject" :project SK8)
(setf (container NODBcancelNewObject) NewObjectDialogBox)
(setBoundsRect NODBcancelNewObject 210 85 280 103)

(new DialogBoxHighlightedButton :objectname "NODBAddNewObject" :project SK8)
(setf (container NODBAddNewObject) NewObjectDialogBox)
(setf (text NODBAddNewObject) "Create")
(setBoundsRect NODBAddNewObject 290 81 368 107)

#|
(new DialogBoxButton :objectname "NODBLeaveAnonNewObject" :project SK8)
(setf (container NODBLeaveAnonNewObject) NewObjectDialogBox)
(setf (text NODBLeaveAnonNewObject) "Leave Anonymous")
(setBoundsRect NODBLeaveAnonNewObject 140 85 280 103)
(define-handler click (NODBLeaveAnonNewObject)
  (withactorlocked (NewObjectDialogBox)
    (setf (text NODBTheObjectNameGetter) "")
    (click NODBAddNewObject))
  )
|#

(define-handler click (NODBAddNewObject)
  (let* ((proj (intern-legal-varName (text NODBTheProjectNameGetter) (SK8::package (CreationProject NewObjectDialogBox)) nil))
         (name (text NODBTheObjectNameGetter))
         (symstr (intern-legal-varName (text NODBTheObjectNameGetter) (SK8::package (CreationProject NewObjectDialogBox)) nil))
         (str (text NODBTheParentName))
         newObj)
    (sk8-multival-bind (failure? errors locals globals result)
                       (and proj 
                            (inheritsFrom (symbol-value proj) project)
                            (translateScriptCommandOrExpr (symbol-value proj) str)) 
      (declare (ignore errors locals globals))
      (setf (Highlight me) t)
      (cond
       ((not (and proj (inheritsfrom (symbol-value proj) project)))
        (MessageToUser "Please a valid project for the new object."))
       ((and (not (string= name "")) (boundp symstr))
        (MessageToUser "An object with that name already exists.  Please enter another name or press cancel to leave the object anonymous." :beep t))
       ((or (not str) (string= str "") failure?)
        (MessageToUser "Please enter a valid Parent name or press cancel to leave the object unchanged" :beep t))
       (t
        (setf newobj (if (duplicate NewObjectDialogBox)
                       (sk8::copy (evaluateScriptTranslation result)
                                  :objectname (and (not (string= name "")) name) 
                                  :project (symbol-value proj))
                       (new (evaluateScriptTranslation result) 
                            :objectname (and (not (string= name "")) name) 
                            :project (symbol-value proj))))
        (unless (objectname newobj) (push newobj (knownchildren (baseparent newobj))))
        (setf (container ObjectNameDialogBox) nil)
        (setf (Highlight me) nil)
        (exitModalState newobj)))
      (setf (Highlight me) nil))))

(define-handler EnteringStage (NewObjectDialogBox)
  (call-next-method)
  (setf (text NODBTheParentName) (objectString (theParent NewObjectDialogBox) :project (CreationProject NewObjectDialogBox)))
  (setf (text NODBTheProjectNameGetter) (objectString (CreationProject NewObjectDialogBox) :project (CreationProject NewObjectDialogBox)))
  (setf (text NODBTheObjectNameGetter) nil)
  (if (duplicate me)
    (setf (text NODBActionLabel) "Copy")
    (setf (text NODBActionLabel) "New"))
  (setf (right NODBActionLabel :resizing nil) 125)
  (setf (keyTarget  me) NODBTheObjectNameGetter)
  (setSelection NODBTheObjectNameGetter 0 -1))

(define-handler GetNewFromUser (object &key ((:project InProject) nil) (locked t))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (setf (theParent NewObjectDialogBox) me)
  (setf (CreationProject NewObjectDialogBox) inProject)
  (if locked
    (progn
      (locktext NODBTheParentName)
      (locktext NODBTheProjectNameGetter))
    (progn
      (unlocktext NODBTheParentName :force t)
      (unlocktext NODBTheProjectNameGetter :force t)))
  (setf (duplicate NewObjectDialogBox) nil)
  (setf (location NewObjectDialogBox) (mainmonitorcenter))
  (modalDialog NewObjectDialogBox))

(define-handler GetDuplicateFromUser (object &key ((:project InProject) nil) (locked t))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (setf (theParent NewObjectDialogBox) me)
  (if locked
    (progn
      (locktext NODBTheParentName)
      (locktext NODBTheProjectNameGetter))
    (progn
      (unlocktext NODBTheParentName :force t)
      (unlocktext NODBTheProjectNameGetter :force t)))
  (setf (CreationProject NewObjectDialogBox) inProject)
  (setf (duplicate NewObjectDialogBox) t)
  (setf (location NewObjectDialogBox) (mainmonitorcenter))
  (modalDialog NewObjectDialogBox))

#|
	Change History (most recent last):
	2  	 4/12/96	Hernan  	Fixing method congruency problems.
	3  	 4/22/96	Brian   	
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
