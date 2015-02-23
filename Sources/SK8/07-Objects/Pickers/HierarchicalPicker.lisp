;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "HIERARCHICALPICKER")

(require "ICONTEXTPICKER" "objects;Pickers:IconTextPicker")

;;; ________________________________________________________
;;; ________________________________________________________
;;; The Hierarchical Picker
;;; ________________________________________________________
;;; ________________________________________________________

(new IconTextPicker 
     :objectName "HierarchicalPicker"
     :prototype t
     :properties '((Offset :inherit :value 20))
     :project sk8)

(new object 
     :objectName "HierarchicalPickerItem"
     :prototype t
     :properties '(value
                   state
                   (depth :value 0)
                   swatch)
     :project sk8)

(setf (alphabeticalDisplay HierarchicalPicker) nil)
(setf (offset HierarchicalPicker) 15)
(setf (arrowsize HierarchicalPicker) '(16 16))
(setf (depth HierarchicalPickerItem) 0)

(define-handler ItemContents (HierarchicalPicker TheItem)
  (declare (ignore theItem))
  nil)

(define-handler OpenItem (HierarchicalPicker TheItem)
  (let* ((its (items me))
         (newits (copy-list its))
         (pos (1+ (position theitem its)))
         (cs (ItemContents me theitem))
         (dep (1+ (depth theitem)))
         (scr (verticalScroll me))
         (sels (selecteditems me))
         temp)
    (withactorlocked ((sk8::window me))
      (setf (State TheItem) 'sk8::open)
      (when cs ; SPLICE IN NEW CONTENTS...
        (mapcar #'(lambda (x) (setf (depth x) dep)) cs)
        (setf temp (nthcdr pos newits))
        (setf (nthcdr pos newits) cs)
        (setf (nthcdr (length newits) newits) temp))
      (setf (items me) newits)
      (setf (selecteditems me) sels)
      (setf (verticalScroll me) scr)
      )))

(define-handler CloseItem (HierarchicalPicker TheItem)
  (let* ((newits (copy-list (items me)))
         (lenny (length newits))
         (pos1 (position theitem newits))
         (pos2 (1+ pos1))
         (scr (verticalScroll me))
         (sels (selecteditems me))
         newsels
         (dep (depth theitem))
         ;; (theDead nil)
         )
    (withactorlocked ((sk8::window me))  ;;; SELECTIVELY REMOVE ALL "CONTAINED" ITEMS UPON CLOSING (I.E. items whose depth is greater than mine)
      (setf (State TheItem) 'closed)
      (loop 
        (if (or (= pos2 lenny) (<= (depth (nth pos2 newits)) dep)) (return))
        (incf pos2))
      ;; Get the items to be disposed and dispose them!
      ;; (setf theDead (subseq newits (1+ pos1) pos2))
      (setf (nthcdr (1+ pos1) newits) (if (= pos2 lenny) nil (nthcdr pos2 newits)))
      ;; Bring out your dead!
      ;; (dolist (c theDead)
      ;;   (dispose c))
      ;; Set the items.
      (setf (items me) newits)
      (setf newsels (remove-if-not #'(lambda (x) (memq x newits)) sels))
      (if newsels                          ;;;; This if makes it reselect nicely after closing
        (setf (selecteditems me) newsels)
        (setf (selecteditems me) (list theitem)))
      (setf (verticalScroll me) scr))))

(define-handler createArrow (HierarchicalPicker theItem thestring theposition)
  (declare (ignore theString thePosition))
  (case (state theitem)
    (closed FinderClosedArrow)
    (sk8::open FinderOpenArrow)
    (otherwise nil)))

(define-handler createIcon (HierarchicalPicker theItem thestring theposition)
  (declare (ignore theString thePosition))
  (swatch theItem))

(define-handler createOffset (HierarchicalPicker theItem thestring theposition)
  (declare (ignore theString thePosition))
  (* (depth theItem) (Offset me)))

(define-handler createtextdisplayitem (HierarchicalPicker TheItem)
  (call-next-method me (value theitem)))

(define-handler mousedown (HierarchicalPicker)
  (let* ((powp (pointonwhichpart me (eventh) (eventv) :part 'ItemAndPosition))
         (it (and powp 
                  (car powp) 
                  (gs:range 0 (car powp) (length (items me)))))  ;;; *** SHOULD FIX POINTONWHICHPART TO RETURN A VALUE IN RANGE UNLESS TRACKING!!!!
         (loc (and powp (cadr powp)))
         )
    (if (eq loc 'arrow)
      (case (state (nth it (items me)))
        (sk8::open (withcursor watchcursor (CloseItem me (nth it (items me)))))
        (closed (withcursor watchcursor (OpenItem me (nth it (items me)))))
        (otherwise 
         (call-next-method)))
      (call-next-method)))
  )


(define-handler mouseUp (HierarchicalPicker)
  (let* ((powp (pointonwhichpart me (eventh) (eventv) :part 'ItemAndPosition)) ;; HEW
         (loc (and powp (cadr powp)))
         )
    (call-next-method)
    (if (eq loc 'arrow)  ;;; if we are on the arrow, we want no doubleclicking
      (setf (lastSelected me) object))
    ))

(define-handler GenerateItems (HierarchicalPicker ItemList)
  (let ((p (project me)))
    (setf (items me) (mapcar #'(lambda (x) (new HierarchicalPickeritem :project p :value x
                                                :state 'closed)) ItemList))))

(define-handler copySelectionToClipboard (HierarchicalPicker)
  (let ((theItems (selectedItems me)))
    (when theItems
      (addToClipboard (mapcar #'value theItems) sk8Clipboard :copy nil))))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
