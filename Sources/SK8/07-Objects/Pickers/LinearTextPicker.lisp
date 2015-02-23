;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "LINEARTEXTPICKER")

(require "EDITTEXT" "objects;EditText:EditText")
(require "SCROLLER" "objects;Widgets:Scroller")

(SK8-declare-syms :SK8 :public ; Updated 11-13-96   3:37 pm
                  SK8::LINEARTEXTPICKER)

;;; ______________________________________________________________
;;; ______________________________________________________________
;;; The linearTextPicker.
;;; ______________________________________________________________
;;; ______________________________________________________________

(new rectangle :objectname "linearTextPicker" :project sk8
     :properties '((items :value nil)
                   (strings :value nil)))

(setSize linearTextPicker 200 40)

(setf (textLocation linearTextPicker) 'topLeft)
(setf (textFont linearTextPicker) "Geneva")
(setf (textSize linearTextPicker) 9)
(setf (textStyle linearTextPicker) '(bold))
(setf (text linearTextPicker) "Title:")

(define-handler resized (linearTextPicker)
  (dovector (c (gs:node-contents me))
    (bestsize c)))

(define-handler minimumSize (linearTextPicker)
  (sk8-multivals 140 30))

(new editText :objectname "linearPickerText" :project sk8)
(setf (container linearPickerText) linearTextPicker)
(setFramesize linearPickerText 0 0)
(setf (textfont linearPickerText) "Geneva")
(setf (textSize linearPickerText) 9)
(setf (textStyle linearPickerText) '(plain))
(tagpart linearTextPicker linearPickerText 'myField)

(setf (wantsIdle linearTextPicker) nil)

(setf (wantsIdle linearPickerText) nil)
(setf (autoTab linearPickerText) nil)
(locktext linearPickerText)

(new scroller :objectname "linearPickerScroller" :project sk8)
(setf (container linearPickerScroller) linearTextPicker)
(tagpart linearTextPicker linearPickerScroller 'myScroller)
(setf (partnervScroller LinearPickerText :affixing nil) LinearPickerScroller)

(define-handler mouseEnter (linearPickerText)
  (setf (cursor stage) cursorPointing))

(define-handler mouseLeave (linearPickerText)
  (setf (cursor stage) standardCursor))

(define-handler mousedown (linearPickerText)
  ;; Only do this stuff when there are items. Avoiding the call to
  ;; hilite-selection avoids the empty line.
  (when (items (container me))
    (let* ((theChar (sk8-multival-bind (h v) (mouseloc stage)
                      (pointOnWhichPart me h v :part 'sk8::character)))
           (theText (text me))
           (start-Pos (find-item-start thetext theChar))
           (end-pos (find-item-end theText start-Pos)))
      (setSelection me start-pos end-pos)
      )))

(define-handler bestsize (linearPickerText)
  (sk8-multival-bind (h v) (size (container me))
    (sk8-multival-bind (textH textV) (actorTextSize (container me))
      (setBoundsRect me textH 2 (- h 16) (- v 2)))))

(define-handler bestsize (linearPickerScroller)
  (sk8-multival-bind (h v) (size (container me))
    (setBoundsRect me (- h 16) 0 h v)))

(bestsize linearPickerText)
(bestsize linearPickerScroller)

(define-handler (setf text) (theText linearTextPicker)
  (withActorLocked (me)
    (call-next-method)
    (bestsize (myField me))))

(define-handler createTextdisplayItem (linearTextPicker theItem)
  (objectString theItem))

(defun build-linear-picker-strings (thePicker theItems)
  (cond ((null theItems) nil)
        (t (cons (createTextdisplayitem thePicker (car theItems))
                 (build-linear-picker-strings thePicker (cdr theItems))))))

;;; Someday will rewrite this to avoid consing like crazy.

(defun make-linear-picker-text (stringList)
  (let ((result ""))
    (dolist (c (butlast stringList))
      (setf result (concatenate 'string result c ", ")))
    (concatenate 'string result (car (last stringList)) #.(string #\Tab)) ; Tab is the end marker
    ))

;;; We will someday add additems and removeitems. For now this method will have to do the trick!

(define-handler (setf items) (theItems linearTextPicker)
  (let ((theStrings (build-linear-picker-strings me theItems))
        (myField (myField me))
        theText)
    (setf (slot-value me 'items) theItems)
    (setf (strings me) theStrings)
    (withActorLocked (me)
      (cond
       (theItems
        (setq theText (make-linear-picker-text theStrings))
        (setf (text myField) theText)
        (setSelection myField 0 (find-item-end theText 0))
        ;; Waiting for Fred's hilite stuff. 
        ;; (hilite-selection myField :outline))
        )
       (t
        (setf (text myField) "")
        ;; (hilite-selection myField nil)
        )))
    theItems))

(defun get-item-from-string (thePicker theString)
  (let ((thepos (position theString (strings thePicker) :test #'string-equal)))
    (when thepos
      (nth thepos (items thePicker)))))

(defun find-item-start (aString aPos)
  (let ((realIndex aPos))
    (when (or (= realIndex (length aString)) (char= (aref aString realIndex) #\,))
      (decf realIndex))
    (dotimes (i realIndex 0)
      (decf realIndex)
      (when (eq (aref aString realIndex) #\,)
        (return-from find-item-start (+ realindex 2))))))

(defun find-item-end (aString aPos)
  (let ((realIndex aPos)
        (stringEnd (length aString)))
    (dotimes (i (- stringEnd aPos) stringEnd)
      (setf realIndex (+ aPos i))
      (when (memq (aref aString realIndex) '(#\, #\Tab)) ; Tab is the end marker
        (return-from find-item-end realindex)))))

(define-handler selectedItems (linearTextPicker)
  (sk8-multival-bind (selStart selEnd) (selection (myField me))
    (let* ((curSelection (subseq (text (myField me)) selStart selEnd))
           (theItem (get-item-from-string me curSelection)))
      theItem)))

;;; This field does not deactivate.

(define-handler deactivateText (linearPickerText)
  )

(define-handler activateText (linearPickerText)
  ;; Waiting for Fred's hilite thingy.
  ;; (hilite-selection me (when (items (container me)) :outline))
  )


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
