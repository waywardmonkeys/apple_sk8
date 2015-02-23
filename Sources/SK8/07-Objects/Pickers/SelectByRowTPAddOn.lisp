;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SELECTBYROWTABLEPICKERADDON")

(require "TABLEPICKER" "objects;Pickers:TablePicker")

;;; ________________________________________________________
;;; ________________________________________________________
;;; The SelectByRowTablePickerAddOn: a SelectByRowTablePickerAddOn that only selects entire rows
;;; ________________________________________________________
;;; ________________________________________________________

;;; Bugs: autoscrolling is funny if the origin is not-zero

(new object
     :objectName "SelectByRowTablePickerAddOn"
     :project sk8
     :undisposable t
     )

(define-handler (setf selectionStyle) (newStyle SelectByRowTablePickerAddOn)
  (case newStyle
    (multiple) ;; HEW
    (single ;; Should Clear all except the first row...
     )
    (otherwise 
     (sk8-error GeneralProgrammaticError
                :strings '("selectionStyle must be either 'single' or 'multiple' for tablePickers"))
     ))
  (sk8::setValue 'selectionStyle me newStyle))

(defun select-row (theSelectByRowTablePickerAddOn whichRows value adding start end)
  (unless adding
    (ClearSelection theSelectByRowTablePickerAddOn))
  (let (;; (style (selectionStyle theSelectByRowTablePickerAddOn))
        (theSelection (selectionArray theSelectByRowTablePickerAddOn))
        (cols (columns theSelectByRowTablePickerAddOn))
        (rows (rows theSelectByRowTablePickerAddOn))
        (item (list nil nil)))
    (flet ((set-row (row)
             (setf (second item) row)
             (dotimes (c cols)
               (setf (first item) (1+ c))
               (set-tuple theSelectByRowTablePickerAddOn item value 'multiple theSelection)))) ;; HEW
      (when (and (numberp start) (> start 0) (numberp end) (> end 0))
        (let ((row-low (gs:range 1 (min (second start) (second end)) rows))
              (row-high (gs:range 1 (max (second start) (second end)) rows)))
          (do ((r row-low (1+ r)))
              ((> r row-high))
            (set-row r))))
      (cond
       ((numberp whichRows)
        (set-row (gs:range 1 whichRows rows)))
       ((listp whichRows)
        (dolist (item whichRows)
          (when (numberp item)
            (set-row (gs:range 1 item rows)))))))))

(defun map-rows (theSelectByRowTablePickerAddOn func return selected)
  (let ((result nil)
        (theSelection (if selected (selectionArray theSelectByRowTablePickerAddOn)))
        (noSelect (not selected))
        (theRows (rows theSelectByRowTablePickerAddOn)))
    (do ((r 1 (1+ r)))
        ((> r theRows))
      (if (or noSelect (sk8-tablePicker-aref theSelection 1 r))
        (let ((val (funcall func r)))
          (if return
            (push val result)))))
    (nreverse result)))

(defun list-matches-list (pattern aList)
  (if (listp pattern)
    (let ((matches t))
      (dolist (p pattern)
        (setq matches nil)
        (loop
          (when (eq p (pop aList))
            (setq matches t)
            (return))
          (unless aList
            (return)))
        (unless matches (return)))
      matches)
    (member pattern aList)))

(defun list-matches-row (pattern mat row)
  (let ((cols (first (array-dimensions mat)))
        (c -1)) ;;; c is going to be pre-incremented
    (setq row (1- row))
    (cond
     ((listp pattern)
      (let ((matches t))
        (decf cols)
        (dolist (p pattern)
          (setq matches nil)
          (loop
            (incf c)
            (when (eq p (aref mat c row))
              (setq matches t)
              (return))
            (unless (< c cols) (return)))
          (unless matches (return)))
        matches))
     (t
      (dotimes (c cols)
        (if (eq pattern (aref mat c row))
          (return-from list-matches-row t)))
      nil))))

(defun select-rows-if (theSelectByRowTablePickerAddOn pred adding multiple)
  (unless adding
    (ClearSelection theSelectByRowTablePickerAddOn))
  (let ((rows (rows theSelectByRowTablePickerAddOn))
        (theItems (items theSelectByRowTablePickerAddOn)))
    (do ((r 1 (1+ r)))
        ((> r rows))
      (when (funcall pred theItems r)
        (select-row theSelectByRowTablePickerAddOn r t multiple nil nil)
        (setf (lastselection theSelectByRowTablePickerAddOn) (list 1 r))
        (unless multiple
          (return-from select-rows-if))))))

;;; THE FOLLOWING THREE HANDLERS ARE THE ONLY THINGS THAT CHANGE!!!

(define-handler (setf selection) (items SelectByRowTablePickerAddOn &key (deselecting t) start end (off nil) (selectionShown t))
  (when (and (items me) (> (rows me) 0) (> (columns me) 0))
    (making-TablePicker-dirty-for-selection
      me
      (select-row me (if (tuple-p items) 
                       (cadr items) 
                       (mapcar #'cadr items))
                  (not off) (and (not deselecting) (eq 'multiple (selectionStyle me))) ;; HEW
                  start end)
      (setf (lastselection me) (if (tuple-p items) items (car items)))
      (when selectionShown (showSelection me)))))

(define-handler (setf selectedItems) (items SelectByRowTablePickerAddOn &key (deselecting t))
  (when (and (items me) (> (rows me) 0) (> (columns me) 0) (listp items))
    (making-TablePicker-dirty-for-selection
      me
      (if items
        (progn
          (if (eq 'multiple (selectionStyle me)) ;; HEW
            (mapc #'(lambda (aList)
                      (select-rows-if me #'(lambda (aMat aRow)
                                             (list-matches-row aList aMat aRow))
                                      (not deselecting) t)) items)
            (select-rows-if me #'(lambda (aMat aRow)
                                   (list-matches-row (first items) aMat aRow))
                            nil nil))
          (showselection me))
        (setf (selection me) nil)))
    (setf (lastselection me) (and (car items) (find-indices me (car items))))
    t))

(define-handler (setf selectedItem) (item SelectByRowTablePickerAddOn &key (deselecting t))
  (when (and (items me) (> (rows me) 0) (> (columns me) 0))
    (making-TablePicker-dirty-for-selection
      me
      (if item
        (progn
          (select-rows-if me #'(lambda (aMat aRow)
                                 (list-matches-row item aMat aRow))
                          (not deselecting) (eq 'multiple (selectionStyle me))) ;; HEW
          (showselection me))
        (setf (selecteditems me) nil))
      (setf (lastselection me) (and item (find-indices me item))))))

;;;;  To Give Functionality...

(define-handler (setf SelectByRows) (theval TablePicker)
  (if theval
    (unless (inheritsfrom me SelectByRowTablePickerAddOn)
      (addParent me SelectByRowTablePickerAddOn))
    (removeparent me SelectByRowTablePickerAddOn)))

(define-handler SelectByRows (TablePicker)
  (inheritsfrom me SelectByRowTablePickerAddOn)
  )

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
