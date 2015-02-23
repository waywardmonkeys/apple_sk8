;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  8-28-95  11:44 am
                  UI::PROPEDITORLABEL UI::TRYITBUTTON UI::UIVALUETEXTTRYIT)


(new PropertyControlPanel :objectname "PropertyEditor" :project ui)
(addparent PropertyEditor uisimplewindow)
(setf (text PropertyEditor) "Property Control Panel")
(setf (container (editParent propertyEditor)) nil)

#|

(define-handler clearReferences (PropertyEditor &key ((:objects theobjects)))
  (when (and (eq (sk8dev::eventWindow) me)
             (inputproperty me))
    (bringtofront (objecteditor me)))
  )

(define-handler SetUpForProject (PropertyEditor &key ((:project theproject)))
  (when (and (eq (sk8dev::eventWindow) me)
             (inputproperty me))
    (bringtofront (objecteditor me))))

|#

(defun EditProperty (Objects Property itemboundsrect TheWindow)
  (withcursor watchcursor
    (withactorlocked (propertyeditor)
      (setf (inputobjects PropertyEditor) Objects)
      (setf (inputProperty PropertyEditor) Property))
    (if (container propertyeditor)
      (bringtofront propertyeditor)
      (progn
        (moveoffstage propertyeditor)
        (setf (container propertyeditor) stage)
        (bringtofront PropertyEditor)
        (setf (location propertyeditor) (location thewindow))))
    ))

(define-handler (setf inputobjects) (theval PropertyEditor)
  (let ((ll (length theval)))
    (setf (inputobject me) (if (= ll 1) (car theval) nil))
    ))

(define-handler (setf inputproperty) (theval PropertyEditor)
  (sk8::setValue 'inputproperty me theval)
  (let ((io (inputobject me)))
    (if theval
      (let ((proppy (propagatableValue io theval)))
        (if proppy
          (setf (text (reinheritbutton me)) "Use Parent's Value")
          (setf (text (reinheritbutton me)) "Get Parent's Value"))
        (setf (checked (InheritableCheck me)) proppy)
        (setf (checked (PrivateCheck me)) (private io :property theval))
        (setf (text PropEditorLabel) (concatenate 'string
                                              (simpleobjectstring theval)
                                              " of "
                                              (objectstring (inputobject me) :project (project (inputobject me)))))
        (resized me)
        )
      (progn
        (setf (checked (InheritableCheck me)) nil)
        (setf (checked (PrivateCheck me)) nil)
        )))
  )

(addparent (ReinheritButton PropertyEditor) uiButton)
(addparent (PropagateButton PropertyEditor) uiButton)
(addparent (SetterButton PropertyEditor) uiButton)
(addparent (GetterButton PropertyEditor) uiButton)
(addparent (PrivateCheck PropertyEditor) uicheckbox)
(addparent (InheritableCheck PropertyEditor) uicheckbox)


(define-handler click ((GetterButton PropertyEditor))
  (call-next-method)
  (setf (container PropertyEditor) nil))
(define-handler click ((SetterButton PropertyEditor))
  (call-next-method)
  (setf (container PropertyEditor) nil))
(define-handler click ((PropagateButton PropertyEditor))
  (when (call-next-method)
    (setf (container PropertyEditor) nil)))

(new uibiglabel :project ui :objectname "PropEditorLabel")
(setf (container PropEditorLabel) PropertyEditor)
(setframesize PropEditorLabel 2 2)
(setf (framecolor PropEditorLabel) framescore)

(define-handler resized (PropertyEditor)
  (withactorlocked (me)
    (let ((top 55))
      (call-next-method)
      (moveoffstage (valuetext me))
      (setBoundsrect PropEditorLabel 10 27 (- (width me) 10) 47)
      (setBoundsrect (ReinheritButton me) 10 top 110 (+ top 22))
      (setBoundsrect (PropagateButton me) 10 (+ top 22 5) 110 (+ top 22 5 22))
      (setBoundsrect (GetterButton me) 120 top 205 (+ top 22))
      (setBoundsrect (SetterButton me) 120 (+ top 22 5) 205 (+ top 22 5 22))
      (setf (left (PrivateCheck me) :resizing nil) 11)
      (setf (top (PrivateCheck me) :resizing nil) (+ top 22 5 22 10))
      (setf (right (InheritableCheck me) :resizing nil) 205)
      (setf (top (InheritableCheck me) :resizing nil) (+ top 22 5 22 10))
      )))
(resized propertyeditor)
(setboundsrect propertyeditor 10 20 225 160)

(hide (highlighterhalo propertyeditor))

;;(editproperty (list oval) 'fillcolor '(0 0 1 1) ui::infowindow)


;;;;-----------------------------------------------------------------------------------------------------------------
;;;;-----------------------------------------------------------------------------------------------------------------
;;;;-----------------------------------------------------------------------------------------------------------------
;;;;-----------------------------------------------------------------------------------------------------------------

(new uitextlist :objectname "UIValueText" :project ui)
(addproperty UIValueText 'oldbounds)
(define-handler NewKeyTarget (UIValueText)
  nil)
(setf (oldbounds UIValueText) '(0 0 1 1))
(setf (pickerPrototype UIValueText) valuetext)
(setf (title UIValueText) "Property Editor")
(setframesize (picker UIValueText) 0 0)
(setf (fillcolor (picker UIValueText)) shadowwhite)
(setf (highlightcolor (picker UIValueText)) uilightcolor)

(define-handler Keydown ((picker UIValueText) thechar)
  (unless (or (eq theChar #\Tab) (eq theChar #\Escape))
    (call-next-method))
  (when (or (eq theChar #\Tab) (eq theChar #\Escape)  
                 (and (eq theChar #\Return)
                      (not (optionkeydown))))
    (withactorlocked ((sk8::window me))
      (hide (container me))
      (sk8-multival-bind (lll ttt rrr bbb) (oldbounds (container me))
        (sk8-multival-bind (ll tt rr bb) (boundsrect (container me) :physical t)
          (zoomRect ll tt rr bb lll ttt rrr bbb :NumberOfFrames 10
                    :duration 0.001)))
      (when (eq (keytarget (sk8::window me)) me)
        (setf (keytarget (sk8::window me)) (newkeytarget (container me)))))
    (setf (inputobjects me) nil)
    (setf (inputobjects (GFUButton (container me))) nil))
  )

(new rectangle :objectname "UIValueTextBox" :project ui)
(setframesize UIValueTextBox 0 0)
(setf (container UIValueTextBox) UIValueText)
(tagpart UIValueText UIValueTextBox 'WhiteBox)
(define-handler (setf fillcolor) :after (theval (picker UIValueText))
                (setf (fillcolor (whitebox (container me))) theval))

(new rectangle :objectname "UIValueTextLine" :project ui)
(setframesize UIValueTextLine 0 0)
(setf (container UIValueTextLine) UIValueText)
(tagpart UIValueText UIValueTextLine 'BlackLine)
(setf (fillcolor UIValueTextLine) lightgray) 

(new uibutton :objectname "UIValueTextCancel" :project ui)
(setf (text UIValueTextCancel) "Cancel")
(setf (container UIValueTextCancel) UIValueText)
(tagpart UIValueText UIValueTextCancel 'CancelButton)
(define-handler click (UIValueTextCancel)
  (withactorlocked (me)
    (keydown (picker (container me)) #\Escape)))

(new uibutton :objectname "UIValueTextTryIt" :project ui)
(setf (text UIValueTextTryIt) "Try")
(setf (container UIValueTextTryIt) UIValueText)
(tagpart UIValueText UIValueTextTryIt 'TryItButton)
(define-handler click (UIValueTextTryIt)
  (withactorlocked (me)
    (keydown (picker (container me)) #\enter)))

(new uibutton :objectname "UIValueTextOK" :project ui)
(setf (text UIValueTextOK) "Set")
(setf (container UIValueTextOK) UIValueText)
(tagpart UIValueText UIValueTextOK 'OKButton)
(define-handler click (UIValueTextOK)
  (withactorlocked (me)
    (keydown (picker (container me)) #\return)))

(new getfromuserbutton :objectname "UIValueTextGFUB" :project ui)
(setf (container UIValueTextGFUB) UIValueText)
(setf (textsize UIValueTextGFUB) 12)
(tagpart UIValueText UIValueTextGFUB 'GFUButton)
(setf (fillcolor UIValueTextGFUB) white)
(addparent UIValueTextGFUB uibutton)
(define-handler (setf enabled) (theValue UIValueTextGFUB)
  (withactorlocked (me)
    (setf (fillcolor (car (contents me))) (if thevalue black gray))
    (setf (textcolor me) (if thevalue black gray))
    (sk8-multival-bind (hh vv) (location me :physical t)
      (if theValue
        (setlocation (car (contents me)) hh (- vv 1) :physical t)
        (setlocation (car (contents me)) hh vv :physical t)))
    )
  (call-next-method))
(define-handler (setf highlight) (theValue UIValueTextGFUB)
  (withactorlocked (me)
    (call-next-method)
    (sk8-multival-bind (hh vv) (location me :physical t)
      (if theValue
        (setlocation (car (contents me)) (+ hh 1) (+ vv 1) :physical t)
        (setlocation (car (contents me)) hh (- vv 1) :physical t)))))
(setf (enabled UIValueTextGFUB) t)
(setf (highlight UIValueTextGFUB) nil)


(define-handler resized (UIValueText)
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



#|
	Change History (most recent last):
	1	11/5/93	rod	
	6	12/17/93	till	#.'s be gone:  draggedOverLeave.
	8	1/14/94	rod	made sure keytarget was set on activate
	9	2/12/94	kleiman	renaming
	10	2/14/94	sidney	rename children to knownchildren
	12	2/22/94	rod	The Final Renaming for Alpha! (yeah, right...)
	13	2/22/94	kleiman	Replaced Everything.
	14	2/25/94	hernan	Using symbols instead of keywords for options!!!
	16	2/28/94	hernan	Avoiding calling dispose directly.
	19	3/4/94	kleiman	addproperty avoided wherever possible
	21	3/9/94	rod	Doing Project Switching and Reference Clearing.
	8	3/21/94	kleiman	setvalue -> sk8::setValue
	17	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	25	6/13/94	rod	1167909: Fixed various problems with the buttons
				and checkboxes on the control panel.
	27	6/13/94	dy	the source control comment block got screwed up and the file would not compile
	28	6/13/94	dy	again!!
	29	7/6/94	rod	Getting rid of UIColorize.
	31 	 9/14/94	rod     	Fixing close box
	2  	 4/16/96	brian   	removing (nthitem call.
	3  	 5/ 7/96	Hernan  	objectEditor??? Removing functions that called this non existant handler.
	4  	 7/ 8/96	Brian   	fixing zoomrects
	5  	11/22/96	Brian   	
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
