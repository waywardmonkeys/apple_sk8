;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :sk8dev)

(provide "SIMPLEPROPERTYEDITOR")

(require "EDITTEXT" "objects;EditText:EditText")
(require "CHECKBOX" "objects;Widgets:Checkbox")
(require "BROWSERCOMPONENTS" "objects;Browser Components:Browser Components")

;;______________________________________________________________________
;;______________________________________________________________________
;; These are all of the mini-property editors that we will use in 
;; some of our editors...

;;______________________________________________________________________
;;______________________________________________________________________
;; SimplePropertyEditor
;; The basic property editor.  All of the editors descend from this dude.

(new propertyDataRect :objectName "SimplePropertyEditor" :project sk8)

(new rectangle :objectName "PeLabel" :project sk8)

(setf (private peLabel) t)

(setf (fillColor peLabel) gray)
(setf (frameColor peLabel) lightgray)
(setf (textSize peLabel) 10)
(setf (textStyle peLabel) '(plain)) ;; HEW
(setf (textFont peLabel) "Geneva")
(setf (text peLabel) "Label Here")
(setf (container peLabel) SimplePropertyEditor)
(tagpart SimplePropertyEditor peLabel 'label)

(define-handler (Setf labelText) (lvtext SimplePropertyEditor)
  (setf (text (label me)) lvtext))

(define-handler labelText (SimplePropertyEditor)
  (text (label me)))

(define-handler resized (SimplePropertyEditor)
  (setBoundsRect (label me) 1 1 (1- (width me)) 15))


(setf (framesize SimplePropertyEditor) '(1 1))
(setf (framecolor SimplePropertyEditor) black)
(setf (fillcolor (label SimplePropertyEditor)) lightgray)
(setf (textcolor (label SimplePropertyEditor)) black)
(setf (textsize (label SimplePropertyEditor)) 12)
(setf (textlocation (label SimplePropertyEditor)) 'centerleft) ;; HEW
(setf (texthoffset (label SimplePropertyEditor)) 10)


;;______________________________________________________________________
;; WRITEVALUE
;; Makes sure the value is a number and writes it out to the proerty.

(define-handler currentvalue (SimplePropertyEditor)
  nil)

(define-handler writeValue (SimplePropertyEditor)
  (let ((property (PropertyName me))
        (val (currentvalue me)))
    (when (and property (objects me))
      (undoableset property (objects me) val))))


;;______________________________________________________________________
;;______________________________________________________________________
;; speEditText
;; The basic text object that has the correct handlers for working with the editor.

(new editText :objectName "SPEEditText" :project sk8)
(setf (textSize speEditText) 10)
(setf (textFont speEditText) "Geneva")
(setf (text speEditText) "<none>")
(Setf (autoTab speEditText) t)

(define-handler returnInField (speEditText)
  (writeValue (container me))
  (setselection me 0 -1))

(define-handler EnterInField (speEditText)
  (writeValue (container me))
  (setselection me 0 -1))

(define-handler deactivatetext (speEditText)
  (call-next-method)
  (writeValue (container me))
  )

#|  ****** We need updates for these guys!!!!
(define-handler undo (speEditText)
  (undolastset)
  (setf (objects (container me)) (objects (container me)))) ;;; updates here!!
|#
;;______________________________________________________________________
;;______________________________________________________________________
;; 1BOOLEAN
;; The basic boolean editor.

(new SimplePropertyEditor :objectName "BooleanPropertyEditor" :project sk8)
(setBoundsRect BooleanPropertyEditor 10 30 100 46)

(new checkBox :objectName "BooleanPropertyEditorCheck" :project sk8)
(setf (CheckSize BooleanPropertyEditorCheck) 2)
(setf (container BooleanPropertyEditorCheck) BooleanPropertyEditor)
(setf (text BooleanPropertyEditorCheck) nil)
(tagpart BooleanPropertyEditor BooleanPropertyEditorCheck 'checkmark)

(define-handler (setf objects) (theVal BooleanPropertyEditor)
  (sk8::setValue 'objects me theval)
  (setf (checked (checkmark me)) (value me)))

(define-handler currentvalue (SimplePropertyEditor)
  (checked (checkmark me)))

(define-handler check (BooleanPropertyEditorCheck)
  (call-next-method)
  (writeValue (container me)))

(define-handler mouseEnter (BooleanPropertyEditorCheck)
  (setf (cursor stage) CursorPointing))

(define-handler resized (BooleanPropertyEditor)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (checkmark me) 1 1 (- vv 1) (- vv 1))
    (setBoundsRect (label me) (1- vv) 0 hh vv)))

(resized BooleanPropertyEditor)

;;______________________________________________________________________
;;______________________________________________________________________
;; 1NUMBER
;; This is an editor for properties that take a single number.  Examples include
;; various Value properties.

(new SimplePropertyEditor :objectName "NumberPropertyEditor" :project sk8)
(setBoundsRect NumberPropertyEditor 10 30 100 63)

(new speEditText :objectName "NumberPropertyEditorText" :project sk8)
(setf (text NumberPropertyEditorText) "--")
(setf (container NumberPropertyEditorText) NumberPropertyEditor)
(setBoundsRect NumberPropertyEditorText 1 15 89 32)
(tagpart NumberPropertyEditor NumberPropertyEditorText 'Number1)

(define-handler (setf objects) (theVal NumberPropertyEditor)
  (sk8::setValue 'objects me theval)
  (let ((val (value me)))
    (if (listp val) (setf val (car val)))
    (unless (numberp val) (setf val nil))
    (setf (text (Number1 me)) (if val (objectstring (round val)) ""))))

(define-handler currentvalue (NumberPropertyEditor)
  (let ((val (read-from-string (text (Number1 me)) nil nil)))
    (unless (and val (numberp val))
      (setf val nil))
    val))

(define-handler resized (NumberPropertyEditor)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (label me) 1 1 hh 25)
    (setBoundsRect (Number1 me) 1 25 (1- hh) (1- vv))))

(resized NumberPropertyEditor)


;;______________________________________________________________________
;;______________________________________________________________________
;; 2NUMBERs
;; This is an editor for properties that take two numbers.  Examples include
;; frameSize, size, location, arrowSize, etc.)

(new SimplePropertyEditor :objectName "TwoNumberPropertyEditor" :project sk8)

(new speEditText :objectName "pe2NumberText1" :project sk8)
(setf (text pe2NumberText1) "--")
(setf (container pe2NumberText1) TwoNumberPropertyEditor)
(setf (textSize pe2NumberText1) 10)
(tagpart TwoNumberPropertyEditor pe2NumberText1 'Number1)

(new speEditText :objectName "pe2NumberText2" :project sk8)
(setf (text pe2NumberText2) "--")
(setf (container pe2NumberText2) TwoNumberPropertyEditor)
(setf (textSize pe2NumberText2) 10)
(tagpart TwoNumberPropertyEditor pe2NumberText2 'Number2)


(define-handler (setf objects) (theVal TwoNumberPropertyEditor)
  (sk8::setValue 'objects me theval)
  (let ((val (value me)) val1 val2)
    (unless (listp val) (setf val (list nil nil)))
    (setf val1 (car val)
          val2 (cadr val))
    (unless (numberp val1) (setf val1 nil))
    (unless (numberp val2) (setf val2 nil))
    (setf (text (Number1 me)) (if val (objectstring (round val1)) ""))
    (setf (text (Number2 me)) (if val (objectstring (round val2)) ""))))

(define-handler currentvalue (TwoNumberPropertyEditor)
  (let ((val1 (read-from-string (text (Number1 me)) nil nil))
        (val2 (read-from-string (text (Number2 me)) nil nil)))
    (unless (and val1 (numberp val1))
      (setf val1 nil))
    (unless (and val2 (numberp val2))
      (setf val2 nil))
    (list val1 val2)))

(define-handler resized (TwoNumberPropertyEditor)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (label me) 1 1 hh 25)
    (setBoundsRect (number1 me) 1 25 (round (1- hh) 2) (1- vv))
    (setBoundsRect (number2 me) (round (1- hh) 2) 25 (1- hh) (1- vv))))

(resized TwoNumberPropertyEditor)


;;______________________________________________________________________
;;______________________________________________________________________
;; 4NUMBERs
;; This is an editor for properties that take four numbers.  Examples include
;; boundsRect.

(new SimplePropertyEditor :objectName "FourNumberPropertyEditor" :project sk8)
(setBoundsRect FourNumberPropertyEditor 10 100 187 133)

(new speEditText :objectName "pe4NumberText1" :project sk8)
(setf (text pe4NumberText1) "--")
(setf (container pe4NumberText1) FourNumberPropertyEditor)
(setBoundsRect pe4NumberText1 1 15 44 32)
(setf (textSize pe4NumberText1) 10)
(tagpart FourNumberPropertyEditor pe4NumberText1 'Number1)

(new speEditText :objectName "pe4NumberText2" :project sk8)
(setf (text pe4NumberText2) "--")
(setf (container pe4NumberText2) FourNumberPropertyEditor)
(setBoundsRect pe4NumberText2 45 15 88 32)
(setf (textSize pe4NumberText2) 10)
(tagpart FourNumberPropertyEditor pe4NumberText2 'Number2)

(new speEditText :objectName "pe4NumberText3" :project sk8)
(setf (text pe4NumberText3) "--")
(setf (container pe4NumberText3) FourNumberPropertyEditor)
(setBoundsRect pe4NumberText3 89 15 132 32)
(setf (textSize pe4NumberText3) 10)
(tagpart FourNumberPropertyEditor pe4NumberText3 'Number3)

(new speEditText :objectName "pe4NumberText4" :project sk8)
(setf (text pe4NumberText4) "--")
(setf (container pe4NumberText4) FourNumberPropertyEditor)
(setBoundsRect pe4NumberText4 133 15 176 32)
(setf (textSize pe4NumberText4) 10)
(tagpart FourNumberPropertyEditor pe4NumberText4 'Number4)

(define-handler (setf objects) (theVal FourNumberPropertyEditor)
  (sk8::setValue 'objects me theval)
  (let ((val (value me)) val1 val2 val3 val4)
    (unless (listp val) (setf val (list nil nil nil nil)))
    (setf val1 (car val)
          val2 (cadr val)
          val3 (caddr val)
          val4 (cadddr val)
          )
    (unless (numberp val1) (setf val1 nil))
    (unless (numberp val2) (setf val2 nil))
    (unless (numberp val3) (setf val3 nil))
    (unless (numberp val4) (setf val4 nil))
    (setf (text (Number1 me)) (if val (objectstring (round val1)) ""))
    (setf (text (Number2 me)) (if val (objectstring (round val2)) ""))
    (setf (text (Number3 me)) (if val (objectstring (round val3)) ""))
    (setf (text (Number4 me)) (if val (objectstring (round val4)) ""))))

(define-handler currentvalue (FourNumberPropertyEditor)
  (let ((val1 (read-from-string (text (Number1 me)) nil nil))
        (val2 (read-from-string (text (Number2 me)) nil nil))
        (val3 (read-from-string (text (Number3 me)) nil nil))
        (val4 (read-from-string (text (Number4 me)) nil nil)))
    (unless (and val1 (numberp val1))
      (setf val1 nil))
    (unless (and val2 (numberp val2))
      (setf val2 nil))
    (unless (and val3 (numberp val3))
      (setf val3 nil))
    (unless (and val4 (numberp val4))
      (setf val4 nil))
    (list val1 val2 val3 val4)))


(define-handler resized (FourNumberPropertyEditor)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (label me) 1 1 hh 25)
    (setBoundsRect (number1 me) 1 25 (round (1- hh) 4) (1- vv))
    (setBoundsRect (number2 me) (round (1- hh) 4) 25 (round (1- hh) 2) (1- vv))
    (setBoundsRect (number3 me) (round (1- hh) 2) 25 (* 3 (round (1- hh) 4)) (1- vv))
    (setBoundsRect (number4 me) (* 3 (round (1- hh) 4)) 25 (1- hh) (1- vv))))

(resized FourNumberPropertyEditor)

#|
	Change History (most recent last):
	1	3/24/94	rod	
	2	3/24/94	rod	
	3	4/5/94	kleiman	code review: various objects marked private
	4  	 9/ 6/94	rod     	
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
