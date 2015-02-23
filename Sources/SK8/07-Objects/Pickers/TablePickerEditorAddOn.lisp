;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "TABLEPICKEREDITORADDON")

(require "TABLEPICKER" "objects;Pickers:TablePicker")
(require "EDITTEXT" "objects;EditText:EditText")

;;; ________________________________________________________
;;; ________________________________________________________
;;; The TablePickerEditorAddOn   Mixin
;;; ________________________________________________________
;;; ________________________________________________________
;;; ________________________________________________________
;;; This is a minimal first cut at this...
;;; Should be redone so the user has more control over where the editor shows up.
;;;
;;; Needs a property which says whether or not the rows and columns grow and shrink while typing or not.
;;; Need to deal with mousedown in a more intuitive way.
;;; switching on with the return key isn't good either...
;;; ________________________________________________________
;;; ________________________________________________________

(new object
     :objectName "TablePickerEditorAddOn"
     :project sk8
     :undisposable t
     :properties '((Editor :value nil)))

(new editText
     :objectName "TablePickerEditor"
     :project sk8
     :undisposable t
     :properties '((editItem :value (0 0))))
(setf (autotab tablepickereditor) nil)
(setf (framesize tablepickereditor) '(0 0))

(define-handler initialize (TablePickerEditorAddOn original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (if (inheritsfrom me tablepicker)
    (withactorlocked (me)
      (let ((olded (remove-if-not #'(lambda (x) (inheritsfrom x tablepickereditor)) (contents me))))
        (if olded (setf olded (car olded)))
        (if olded
          (setf (editor me) olded)
          (setf (editor me) (new tablepickereditor :project (project me) :container me)))
        (moveoffstage (editor me))))
    ))

;;; return nil if there was an errror or indeces are off screen...

(define-handler DisplayEditor (TablePickerEditorAddOn &key (Indices nil) (DefaultText nil) (selectedText nil))
  (when indices (setf (editItem (editor me)) indices))  ;;; If the user supplies indeces then make this the edit item
  (when (and defaultText (stringp defaulttext)) (setf (text (editor me)) defaulttext))
  (when selectedText (setSelection (editor me) 0 -1))
  (copytextproperties (editor me) me)
  (let* ((myEditor (editor me))
         (item (editItem myEditor))                       ;;; otherwise use the existing edit item...
         (newbounds nil))
    (when (tuple-p item)
      (setf newbounds (itemboundsrect me item))
      (if newbounds
        (sk8-multival-bind (ll tt rr bb) newbounds
          (incf ll (- (round (columnspacing me) 2) 5))  ;;;  OJO!!! **** this is so text doesn't move.  
          (incf tt (round (rowspacing me) 2))  ;;;; not a good solution, changes if font or size varies!
          (setboundsrect myeditor ll tt rr bb)
          (setf (keytarget (sk8::window me)) myeditor))
        (moveoffstage myeditor)))
    newbounds))

(define-handler ReturnAction (TablePickerEditorAddOn StringValue Indices)
  (setf (sk8-tablePicker-aref (items me) (car indices) (cadr indices)) stringvalue)
  (setf (items me) (items me))
  )

(define-handler EscapeAction (TablePickerEditorAddOn)
  t  )

(define-handler keyDown (TablePickerEditor theChar) 
  (cond
   ((memq theChar (list #\Return #\Enter))
    (making-TablePicker-dirty (container me)
      (ReturnAction (container me) (text me) (edititem me))
      (moveoffstage me)
      (setf (keytarget (sk8::window me)) (container me))))
   ((memq theChar (list #\Escape #\Tab))
    (making-TablePicker-dirty (container me)
      (escapeaction (container me))
      (moveoffstage me)
      (setf (keytarget (sk8::window me)) (container me))))
   (t (call-next-method))))

(define-handler deactivateText (TablePickerEditor) 
  (call-next-method)
  (moveoffstage me))


#|  THIS IS THE THING WHICH WOULD KEEP THE SIZES IN LINE WITH WHAT THE USER IS TYPING... SHOULD INTEGRATE THIS ***
(define-handler keydown (#.(editor MenuBarEditorPicker) thechar)
    (call-next-method)
    (setf (aref (imagearray (container me)) (1- (car (edititem me))) (1- (cadr (edititem me)))) (text me))
    (computecolumnwidths (container me))
    (unless (or (eq thechar #\Return) (eq thechar #\enter))
      (setf (boundsrect me) (itemboundsrect (container me) (edititem me)))
  ))
|#

(define-handler AddedMeAsParent (TablePickerEditorAddOn child oldparents)
  (declare (ignore oldParents))
  (withactorlocked (child)
      (let ((olded (remove-if-not #'(lambda (x) (inheritsfrom x tablepickereditor)) (contents child))))
        (if olded (setf olded (car olded)))
        (if olded
          (setf (editor child) olded)
          (setf (editor child) (new tablepickereditor :project (project child) :container child)))
        (moveoffstage (editor child)))))

;;;;  Give Functionality...

(define-handler (setf EditorIncluded) (theval tablePicker)
  (if theval
    (unless (inheritsfrom me TablePickerEditorAddOn)
      (addParent me TablePickerEditorAddOn) )
    (removeparent me TablePickerEditorAddOn)))

(define-handler EditorIncluded (tablePicker)
  (inheritsfrom me TablePickerEditorAddOn))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
