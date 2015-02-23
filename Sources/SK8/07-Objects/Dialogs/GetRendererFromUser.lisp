;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETRENDERERFROMUSER")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "COLORPICKER" "objects;Pickers:ColorPicker")
(require "MODALDIALOGMODE" "objects;Dialogs:ModalDialogMode")

;;; _______________________________ 
;;; GetRendererFromUser
;;; _______________________________ 

(new DialogBox :objectname "GetColorDialogBox" :project SK8
     :properties '(filterproject))

(addproperty GetColorDialogBox 'inputobject)

(define-handler (setf inputobject) (theval GetColorDialogBox)
  (withactorlocked (me)
    (setvalue 'inputobject me theval)
    (setf (text getcolormenu) (objectstring theval :project (project theval)))
    (setf (items GetColorColorPicker) (knownchildren theval))
    (setf (left getcolormenu :resizing nil) (+ 10 (right GetColorLabel)))
    ))

(new DialogBoxLabel :project SK8 :container GetColorDialogBox
     :objectName "GetColorLabel")
(setf (text GetColorLabel) "Choose an")

(new DialogBoxDisplayRectangle :project SK8 :container GetColorDialogBox 
     :objectName "GetColorNameDisplayer")

(new colorpicker :project SK8 :container GetColorDialogBox 
     :objectName "GetColorColorPicker")
(setf (fillcolor GetColorColorPicker) (fillcolor GetColorDialogBox))
(setf (LineColor GetColorColorPicker) nil)
(setf (rowLinesSize GetColorColorPicker) 3)
(setf (columnLinesSize GetColorColorPicker) 3)
(setf (highlightcolor GetColorColorPicker) cyan)

(new menu :project SK8 :container GetColorDialogBox 
     :objectName "GetColorMenu")
(setf (textlocation GetColorMenu) 'centerleft)
(setf (texthoffset GetColorMenu) 4)
(new menuitem :project SK8 :objectName "GetColorMenuItem" :properties '(val))

(define-handler update (getcolormenu)
  (let* ((filt (filterproject (container me)))
         (reqprojs (and filt (cons filt (requiredprojects filt)))))
    (dolist (each (menuitems me)) (setf (menu each) nil))
    (mapKnownDescendants
     renderer
     #'(lambda (i)
         (dolist (child (knownchildren i))
           (when (memq (project child) reqprojs)
             (setf (val (new GetColorMenuItem 
                             :project UI
                             :val i
                             :menu me
                             :text (objectstring i  :project filt)))
                   i)
             (return)))))))

(define-handler menuselect (getcolormenuitem)
  (sk8dev::withLockedCursor animatedClock
    (withactorlocked (getcolordialogbox)
      (tickeventclock)
      (setf (inputobject GetColorDialogBox) (val me))
      (tickeventclock)
      (selectioncompleted getcolorcolorpicker)
      )))

(define-handler (setf items) (theval GetColorColorPicker)
  (let* ((filt (filterproject (container me)))
         (reqprojs (and filt (cons filt (requiredprojects filt)))))
    (tickeventclock)
    (if filt (setf theval (remove-if-not #'(lambda (x) (memq (project x) reqprojs)) theval)))
    (tickeventclock)
    (call-next-method theval me)))

(define-handler selectionCompleted (GetColorColorPicker)
  (setf (text GetColorNameDisplayer) (objectstring (car (selecteditems me)))))

(define-handler keyDown (GetColorColorPicker thechar)
  (if (or (eq theChar #\Enter) (eq theChar #\Return))
    (click GetColorOKButton)
    (call-next-method)))

(define-handler doubleclick (GetColorColorPicker)
  (click GetColorOKButton))

(new DialogBoxCancelButton :objectName "GetColorCancelButton" :project SK8)
(setf (container GetColorCancelButton) GetColorDialogBox)

(new DialogBoxHighlightedButton :objectname "GetColorOKButton" :project SK8)
(setf (container GetColorOKButton) GetColorDialogBox)
(setf (text GetColorOKButton) "OK")

(define-handler click (GetColorOKButton)
  (let ((valtoreturn (selecteditems GetColorColorPicker)))
    (sk8-multival-bind (hh vv) (mouseloc stage) 
      (when (neq (container (actorathvcoordinates hh vv)) me)
        (setf (Highlight me) t)
        (sleep 0.05)
        (setf (Highlight me) nil)
        (sleep 0.05)))
    (exitmodalstate valtoreturn)))

(define-handler resized (GetColorDialogBox)
  (withActorLocked (me)
    (call-next-method)
    (sk8-multival-bind (x y) (size me)
      (setf (left GetColorLabel :resizing nil) 7)
      (setf (v GetColorLabel) 17)
      (setf (v getcolormenu) 17)
      (setf (left getcolormenu :resizing nil) (+ 10 (right GetColorLabel)))
      (setBoundsRect GetColorColorPicker 7 30 (- x 7) (- y 66))
      (setBoundsRect GetColorNameDisplayer 7 (- y 62) (- x 7) (- y 40))
      (setBoundsRect GetColorCancelButton (- x 168) (- y 29) (- x 98) (- y 11))
      (setBoundsRect GetColorOKButton (- x 88) (- y 33) (- x 10) (- y 7))
      )))
(setSize GetColorDialogBox 335 260)

(define-handler EnteringStage (GetColorDialogBox)
  (call-next-method)
  (selectioncompleted GetColorColorPicker)
  (setf (keytarget me) GetColorColorPicker)
  )

(define-handler LeavingStage (GetColorDialogBox)
  (unless (eq (project (inputobject GetColorDialogBox)) sk8)
    (setf (inputobject GetColorDialogBox) nil))
  )

(define-handler getFromUser (renderer &key ((:project InProject) nil) 
                                        (multipleValues nil) 
                                        popUpMenu relativeactor)
  (let (theval)
    (setf (location GetColorDialogBox) (mainmonitorcenter))
    (setf (filterproject GetColorDialogBox) inproject)
    (if (eq me renderer)
      (progn 
        (if (inputobject GetColorDialogBox)
          (setf (inputobject GetColorDialogBox) (inputobject GetColorDialogBox))
          (setf (inputobject GetColorDialogBox) rgbcolor))
        (show getcolormenu)
        (setf (text GetColorLabel) "Choose a")
        )
      (progn 
        (setf (inputobject GetColorDialogBox) me)
        (hide getcolormenu)
        (setf (text GetColorLabel) (concatenate 'string "Choose a " (objectstring me)))
        ))
    (resized GetColorDialogBox)
    (setf theval (modalDialog GetColorDialogBox))
    (setf (items GetColorColorPicker) nil)
    (unless multiplevalues (setf theval (car theval)))
    theval))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
