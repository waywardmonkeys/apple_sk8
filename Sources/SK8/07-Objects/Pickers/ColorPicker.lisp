;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "COLORPICKER")

(require "TABLEPICKER" "objects;Pickers:TablePicker")

;;; _______________________________ 
;;; ColorPicker.
;;; _______________________________ 

(new tablepicker :objectname "ColorPicker" :project sk8
     :properties '(oldItems))

(setf (private ColorPicker) nil)  ; private api
(setf (prototype colorPicker) t)

(define-handler (setf items) (items ColorPicker)
  (if items 
    (withActorLocked (me)
      (let* (hSwatch vSwatch
                     hSpacing vSpacing
                     rows
                     cols
                     (old-sel (selecteditem me))
                     (numitems (length items))
                     ;; (TableHoffset (TableHoffset me))
                     ;; (TableVoffset (TableVoffset me))
                     (topstart 0)
                     (leftstart 0)
                     hh vv
                     (curlist items) arr
                     )
        (sk8-multival-setf (hh vv) (size me))
        (cond
         ((> hh vv) (let ((theval (/ vv hh)))
                      (setf rows (max (round (sqrt (* theval numitems))) 1))
                      (setf cols (ceiling numitems rows))))
         ((< hh vv) (let ((theval (/ hh vv)))
                      (setf cols (max (round (sqrt (* theval numitems))) 1))
                      (setf rows (ceiling numitems cols))))
         ((= hh vv) 
          (setf cols (max (isqrt numitems) 1))
          (setf rows (ceiling numitems cols))
          ))
        (setf hSpacing (columnSpacing me)
              vSpacing (rowspacing me))
        (setf hh (- hh (* 2 (car (framesize me)))))
        (setf vv (- vv (* 2 (cadr (framesize me)))))
        (let ((HRoom (- hh 6 (* hspacing cols)))  ;;; this 6 and the next are to provide a little buffer around the edges  (always min of 3)
              (VRoom (- vv 6 (* Vspacing rows))))
          (setf hSwatch (truncate HRoom cols))
          (setf vSwatch (truncate VRoom rows))
          (setf topstart (truncate (- vv 1 (* vSwatch rows) (* Vspacing rows)) 2))
          (setf leftstart (truncate (- hh 1 (* hSwatch cols) (* hspacing cols)) 2)))
        (setf arr (new array :project (project me) :dimensions (list cols rows)))
        (sk8::setValue 'items me arr)
        (dotimes (r rows)
          (dotimes (c cols)
            (setf (aref arr c r) (car curlist))
            (setf curlist (cdr curlist))
            (if (not curlist) (return))))
        (setf (columnwidths me) hswatch)
        (setf (rowheights me) vswatch)
        (setf (tablehoffset me) leftstart)
        (setf (tableVoffset me) topstart)
        (setf (oldItems me) items)
        (setupimages me)
        (SetupSelectionArray me :dimensions (list cols rows))
        (if (memq old-sel items) 
          (setf (selecteditem me) old-sel)
          (if items (setf (selection me) '(0 0))))
        ))
    (call-next-method)))

(define-handler (setf horizontalScroll) (theval ColorPicker)
  (declare (ignore theVal))
  (call-next-method 0 me))

(define-handler (setf verticalScroll) (theval ColorPicker)
  (declare (ignore theVal))
  (call-next-method 0 me))

(define-handler resized (ColorPicker)
  (setf (items me) (oldItems me)))

(setf (Highlightselection colorpicker) nil)

(define-handler (setf selection) (items ColorPicker &key (deselecting t) start 
                                           end (off nil) (selectionShown t))
  (declare (ignore-if-unused deselecting start end off selectionShown))
  (when (and (items me) (> (rows me) 0) (> (columns me) 0))
    (let ((bing items))
      (unless (tuple-p bing) (setf bing (car bing)))
      (if bing
        (when (sk8-tablepicker-aref (items me) 
                                    (gs:range 1 (car bing) (columns me)) 
                                    (gs:range 1 (cadr bing) (rows me)))
          (call-next-method))
        (call-next-method)))))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
