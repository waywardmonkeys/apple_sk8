;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  3-23-94   2:43 pm
                   SK8::LDITEMLABEL SK8::LDRELATIVETO)



(new dialogbox :objectname "LayoutDialogBox" :Project sk8)
(addproperty LayoutDialogBox 'InputObjects)
(addproperty LayoutDialogBox 'CurrentFunction)
(setf (windowstyle LayoutDialogBox) 'movabledialog)
(setf (windowstyle LayoutDialogBox) 'movabledialog)
(setf (windowtitle LayoutDialogBox) "Layout")
(define-handler resized (LayoutDialogBox)
  (let (hSize vSize)
    (declare (special hSize vSize))
    (sk8-multival-setf (hSize vSize) (size me))
    (mapc #'bestsize (contents me))
    ))

(define-handler (setf inputobjects) (theval LayoutDialogBox)
  (sk8::setValue 'inputobjects me theval)
  (let ((x (selecteditem LDRelativeTo))
        TheObj)
    (dolist (i (items LDRelativeTo))
      (if (eq x i)
        (setf TheObj i)))
    (when (and (not theobj) (= (length theval) 1))
      (setf theobj (cadr (items LDRelativeTo)))
      )
    (setf (selecteditem LDRelativeTo) TheObj)
    (menuselect ldrelativeto)
    )
  (bestsize LDRelativeTo)
  )

;;;============================================================
;;;============================================================
;;;=== Marks for the actors and the relative actor...

(new label :objectname "LDItemLabel" :Project sk8)
(setf (fillcolor LDItemLabel) (fillcolor LayoutDialogBox))
(setf (text LDItemLabel) "Arranging selected objects...")
(setf (container LDItemLabel) LayoutDialogBox)
(define-handler bestsize (LDItemLabel)
  (setf (left me :resizing nil) 10)
  (setf (top me :resizing nil) 10)
  )

(new label :objectname "LDRelativeLabel" :Project sk8)
(setf (fillcolor LDRelativeLabel) (fillcolor LayoutDialogBox))
(setf (text LDRelativeLabel) "Relative to:")
(setf (container LDRelativeLabel) LayoutDialogBox)
(define-handler bestsize (LDRelativeLabel)
  (setf (left me :resizing nil) 20)
  (setf (top me :resizing nil) 45)
  )

(new pickerMenu :objectname "LDRelativeTo" :Project sk8)
(setf (container LDRelativeTo) LayoutDialogBox)
(define-handler bestsize (LDRelativeTo)
  (setf (left me :resizing nil) 25)
  (setf (top me :resizing nil) 65)
  )
(define-handler createTextDisplayItem (LDRelativeTo item)
  (if item
    (objectstring item :project sk8)
    "Items Themselves"))

(define-handler items (LDRelativeTo)
  (nconc (cons nil 
               (delete-duplicates (apply #'append 
                                         (mapcar #'containers (inputobjects LayoutDialogBox)))))
         (list stage)))

(define-handler menuselect (LDRelativeTo)
  (withactorlocked (me)
    (setf (text me) (createTextDisplayitem me (selecteditem me))) 
    (call-next-method)
    (bestsize me)))


;;(resized LayoutDialogBox)
;;;============================================================

(new label :objectname "LDSoLabel" :Project sk8)
(setf (fillcolor LDSoLabel) (fillcolor LayoutDialogBox))
(setf (text LDSoLabel) "So objects are:")
(setf (container LDSoLabel) LayoutDialogBox)
(define-handler bestsize (LDSoLabel)
  (setf (left me :resizing nil) 20)
  (setf (top me :resizing nil) 100)
  )

(new PickerMenu :objectname "LDchooseFunction" :Project sk8)
(setf (container LDchooseFunction) LayoutDialogBox)
(setf (items LDchooseFunction) (list "Aligned" "Distributed" "Tiled"))
(define-handler bestsize (LDchooseFunction)
  (setf (left me :resizing nil) 25)
  (setf (top me :resizing nil) 123)
  )
(define-handler menuselect (LDchooseFunction)
  (withactorlocked (me)
    (setf (text me) (selecteditem me))
    (setf (currentFunction LayoutDialogBox) (text me))
    (call-next-method)
    (bestsize me)))

;;;============================================================
;;;============================================================
;;;=== Cards for each of the functions representing the options... 

(new rectangle :objectname "LayoutDialogBoxCard" :Project sk8)
(setf (fillcolor LayoutDialogBoxCard) transparent)
(setf (framesize LayoutDialogBoxCard) '(2 2))
(setf (framecolor LayoutDialogBoxCard) black)
(setf (mousesensitivity LayoutDialogBoxCard) 'normal)

(define-handler bestsize (LayoutDialogBoxcard)
  (sk8-muLtival-bind (hh vv) (size LayoutDialogBox)
    (setboundsrect me 10 107 (- hh 10) (- vv 50))))

(new NumberPropertyEditor :objectname "LDNumberEditor" :Project sk8)
(setf (fillcolor LDNumberEditor) lightgray)
(setf (fillcolor (label LDNumberEditor)) lightgray)
(setf (textcolor (label LDNumberEditor)) black)
(new checkbox :objectname "LDcheckbox" :Project sk8)
(define-handler resized (LDNumberEditor)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (label me) 1 1 hh (1- vv))
    (setBoundsRect (number1 me) (- hh 46) 5 (- hh 6) (- vv 5))))
;;;;==================Tile Card

(new LayoutDialogBoxCard :objectname "TilingCard" :Project sk8)
(setf (container TilingCard) LayoutDialogBox)

(new LDNumberEditor :objectname "TileRows" :Project sk8)
(setf (container TileRows) TilingCard)
(setf (text (label TileRows)) "Rows")

(new LDNumberEditor :objectname "TileCols" :Project sk8)
(setf (container TileCols) TilingCard)
(setf (text (label TileCols)) "Cols")

(new LDNumberEditor :objectname "TileHspacing" :Project sk8)
(setf (container TileHspacing) TilingCard)
(setf (text (label TileHspacing)) "H Spacing")

(new LDNumberEditor :objectname "TileVspacing" :Project sk8)
(setf (container TileVspacing) TilingCard)
(setf (text (label TileVspacing)) "V Spacing")

(new LDcheckbox :objectname "TileWithResizing" :Project sk8)
(setf (container TileWithResizing) TilingCard)
(setf (text TileWithResizing) "With Resizing")

(let ((c 50)
      (incey 30))
  (dolist (i (list TileRows TileCols TileHspacing TileVspacing))
    (setboundsrect i 20 c 175 (- (+ c incey) 5))
    (incf c incey))
  (setf (top TileWithResizing :resizing nil) c)
  (setf (left TileWithResizing :resizing nil) 20))

(mapcar #'resized (list TileRows TileCols TileHspacing TileVspacing TileWithResizing))
;;;;==================Align Card

(new LayoutDialogBoxCard :objectname "AlignCard" :Project sk8)
(addproperty aligncard 'item)
(setf (container AlignCard) LayoutDialogBox)
(setf (text AlignCard) "")

(new radiobutton :objectname "ALradiobutton" :Project sk8)
(addproperty ALradiobutton 'value)
(setf (textfont ALradiobutton) "Geneva")
(setf (textcolor ALradiobutton) Black)
(setf (textsize ALradiobutton) 10)
(define-handler check (ALradiobutton)
  (setf (item aligncard) (value me))
  (call-next-method)
  )

(new ALradiobutton :objectname "ALRBLeft" :Project sk8)
(setf (container ALRBLeft) AlignCard)
(setf (value ALRBLeft) 'Left)
(setf (text ALRBLeft) "Left")

(new ALradiobutton :objectname "ALRBVCenter" :Project sk8)
(setf (container ALRBVCenter) AlignCard)
(setf (value ALRBVCenter) 'VerticalCenter)
(setf (text ALRBVCenter) "Vertical Center")


(new ALradiobutton :objectname "ALRBRight" :Project sk8)
(setf (container ALRBRight) AlignCard)
(setf (value ALRBRight) 'Right)
(setf (text ALRBRight) "Right")

(new ALradiobutton :objectname "ALRBTop" :Project sk8)
(setf (container ALRBTop) AlignCard)
(setf (value ALRBTop) 'Top)
(setf (text ALRBTop) "Top")

(new ALradiobutton :objectname "ALRBHCenter" :Project sk8)
(setf (container ALRBHCenter) AlignCard)
(setf (value ALRBHCenter) 'HorizontalCenter)
(setf (text ALRBHCenter) "Horizontal Center")

(new ALradiobutton :objectname "ALRBBottom" :Project sk8)
(setf (container ALRBBottom) AlignCard)
(setf (value ALRBBottom) 'Bottom)
(setf (text ALRBBottom) "Bottom")


(let ((c 50)
      (incey 25))
  (dolist (i (list ALRBLeft ALRBVCenter ALRBRight ALRBTop ALRBHCenter ALRBBottom))
    (setboundsrect i 20 c 175 (- (+ c incey) 5))
    (incf c incey))
  )

(check ALRBLeft)

;;;;==================Distribute Card

(new LayoutDialogBoxCard :objectname "DistributeCard" :Project sk8)
(setf (container DistributeCard) LayoutDialogBox)
(addproperty DistributeCard 'item)

(new radiobutton :objectname "DLradiobutton" :Project sk8)
(addproperty DLradiobutton 'value)
(setf (textfont DLradiobutton) "Geneva")
(setf (textcolor DLradiobutton) Black)
(setf (textsize DLradiobutton) 10)
(define-handler check (DLradiobutton)
  (setf (item DistributeCard) (value me))
  (call-next-method)
  )

(new DLradiobutton :objectname "DLHOnly" :Project sk8)
(setf (container DLHOnly) DistributeCard)
(setf (value DLHOnly) 'h)
(setf (text DLHOnly) "Horizontally")

(new DLradiobutton :objectname "DLVOnly" :Project sk8)
(setf (container DLVOnly) DistributeCard)
(setf (value DLVOnly) 'v)
(setf (text DLVOnly) "Vertically")

(new DLradiobutton :objectname "DLBoth" :Project sk8)
(setf (container DLBoth) DistributeCard)
(setf (value DLBoth) nil)
(setf (text DLBoth) "Both")

(new LDcheckbox :objectname "DistWithSize" :Project sk8)
(setf (container DistWithSize) DistributeCard)
(setf (text DistWithSize) "Size Relative")

(let ((c 50)
      (incey 25))
  (dolist (i (list DLHOnly DLVOnly DLBoth))
    (setboundsrect i 20 c 175 (- (+ c incey) 5))
    (incf c incey))
  (setf (top DistWithSize :resizing nil) c)
  (setf (left DistWithSize :resizing nil) 20))

(check DLHOnly)

;;;;==================And Now their use is determined by...

(define-handler (setf currentFunction) (theval LayoutDialogBox)
  (sk8::setValue 'currentFunction me theval)
  (withActorLocked (me)
    (cond 
      ((string= theval "Tiled")
       (show TilingCard)
       (hide aligncard)
       (hide distributecard))
      ((string= theval "Aligned")
       (hide TilingCard)
       (show aligncard)
       (hide distributecard))
      ((string= theval "Distributed")
       (hide TilingCard)
       (hide aligncard)
       (show distributecard)))))

;;;============================================================
;;;============================================================
;;;===The Buttons
;;;(resized LayoutDialogBox)

(new DialogBoxCancelButton :objectname "LDCancel" :Project sk8)
(setf (container LDCancel) LayoutDialogBox)

(define-handler bestsize (LDCancel)
  (sk8-multival-bind (hh vv) (size LayoutDialogBox)
    (setboundsrect me (- hh 168) (- vv 29) (- hh 98) (- vv 11))))


(new DialogBoxHighlightedButton :objectname "LDApplyButton" :Project sk8)
(setf (container LDApplyButton) LayoutDialogBox)
(setf (text LDApplyButton) "OK")

(define-handler bestsize (LDApplyButton)
  (sk8-multival-bind (hh vv) (size LayoutDialogBox)
    (setboundsrect me (- hh 88) (- vv 33) (- hh 10) (- vv 7))))
(define-handler click (LDApplyButton)
  (when (inputobjects LayoutDialogBox)
    (setf (objectlist UndoableSetLog) (inputobjects LayoutDialogBox))
    (setf (PropertyName UndoableSetLog) 'boundsrect)
    (setf (valuelist UndoableSetLog) (mapcar #'boundsrect (inputobjects LayoutDialogBox)))
    ;;;Apply the current Layout Function with options
    (withActorLocked (me)
      (cond
        ((string= (currentfunction LayoutDialogBox) "Tiled")
         (tile (inputobjects LayoutDialogBox) 
               :relativeactor (selecteditem LDRelativeTo)
               :rows (currentvalue TileRows) :columns (currentvalue TileCols)
               :hspacing (currentvalue TileHspacing) :Vspacing (currentvalue TileVspacing) 
               :resizing (checked TileWithResizing)))
        ((string= (currentfunction LayoutDialogBox) "Aligned")
         (align (inputobjects LayoutDialogBox) 
                :relativeactor (selecteditem LDRelativeTo)
                :alignlocation (item aligncard)))
        ((string= (currentfunction LayoutDialogBox) "Distributed")
         (distribute (inputobjects LayoutDialogBox) :sizeRelative (checked DistWithSize)
                     :relativeactor (selecteditem LDRelativeTo)
                     :how (item DistributeCard))
         )
        ))
    (exitmodalstate t)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf (container LayoutDialogBox) nil)
(setf (selecteditem LDchooseFunction) "Aligned")
(menuselect LDchooseFunction)
(setf (boundsrect LayoutDialogBox) '(100 100 360 470))
(setf (inputobjects LayoutDialogBox) nil)
(bringtofront LDchooseFunction)
(sendtoback distributecard)
(sendtoback TilingCard)
(sendtoback aligncard)
;;(setf (inputobjects LayoutDialogBox) uiselection)
;;(setf (container LayoutDialogBox) stage)
(resized LayoutDialogBox)

(define-handler EnteringStage (LayoutDialogBox)
  (call-next-method)
  (unless (selecteditem LDchooseFunction)
    (setf (selecteditem LDchooseFunction) "Aligned")
    (menuselect LDchooseFunction))
  )


(define-handler LeavingStage (LayoutDialogBox)
  ;;;; Clear the relative item for gc purposes.
  ;;;; Decided not to...
  ;;;; (setf (selecteditem LDRelativeTo) nil)
  (setf (inputobjects LayoutDialogBox) nil)
  )

(define-sk8-function LayoutDialog nil (actorList &key (location nil)
                                                   ((label thelabel) "Arranging selected objects..."))
  (setf (location LayoutDialogBox) (or location 
                                       (mainmonitorcenter)))
  (setf (text LDItemLabel) thelabel)
  (setf (inputobjects LayoutDialogBox) actorlist)
  (bestsize LDItemLabel)
  (modaldialog LayoutDialogBox)
  )

#|
	Change History (most recent last):
	1	3/24/94	rod	
	2	3/24/94	rod	
	3	3/27/94	Brian	Added stage as a relative Actor
	4	4/12/94	Hernan	Avoiding use of contents when not necessary.
	5	5/6/94	rod	
	6	6/3/94	rod	
	7	6/13/94	rod	1167890: Fixed default popup alignment.
	8	7/21/94	rod	
	9  	 9/ 2/94	rod     	
	10 	 9/ 2/94	rod     	Fixing cleared currentFunction.
							Handling the case of single inputobject more
							nicely.
	11 	 9/21/94	rod     	
	2  	 2/14/96	Brian   	removing mf::docontents
	3  	 2/15/96	Brian   	removing bogus qualification.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
