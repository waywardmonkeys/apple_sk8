;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8Development)

(provide "TABLEPICKER")

(require "RECTANGLE")

;;; ________________________________________________________
;;; ________________________________________________________
;;; The TablePicker: a 2D-picker
;;;
;;; BJR 8/30/93 
;;; ________________________________________________________
;;; ________________________________________________________

(new Rectangle
     :objectName "TablePicker"
     :project sk8
     :undisposable t
     :prototype t
     :properties '((items :value nil)
                   (imageArray :value nil)
                   (selectionArray :value nil) ;; Selection is implemented with an array but the user thinks of it as a list
                   (textStyles :value nil)
                   (textColors :value nil)
                   (createTextStyles :value nil)
                   (createTextColors :value nil)
                   (partnerVScroller :value nil)
                   (partnerHScroller :value nil)
                   (lastSelection :value nil)
                   (lastSelected :value nil) ;;;used for doubleclick only!
                   (TableHOffset :value 0)
                   (TableVOffset :value 0)
                   (horizontalScroll :value 0)
                   (verticalScroll :value 0)
                   (columnSpacing :value 10)
                   (rowSpacing :value 8)
                   (minWidth :value 10)
                   (columnWidths :value (10))
                   (minHeight :value 10)
                   (rowHeights :value (10))
                   (columnsRigid :value nil)
                   (rowsRigid :value nil)
                   (rowLinesSize :value 1)
                   (columnLinesSize :value 1)
                   (LineColor :value nil)
                   (selectionStyle :value single)
                   (HighlightSelection :value t)
                   (HighlightColor :value yellow)
                   (selectionBorder :value 2)))

(setf (localCreationRelations TablePicker) '(SK8::[partnerVScroller]
                                             SK8::[partnerHScroller]))

(setf (private TablePicker) nil)  ; in public api

(setf (linecolor tablepicker) black)
(setf (HighlightColor tablepicker) yellow)

(define-handler initialize (TablePicker original isNew initArgs)
  (declare (ignore  isNew initArgs))
  (setf original (originalAncestor me original TablePicker))
  (setf (slot-value me 'items) (if (items original) (sk8::copy (items original)) nil))
  (setf (slot-value me 'textcolors) (if (textcolors original) (sk8::copy (textcolors original)) nil))
  (setf (slot-value me 'textstyles) (if (textstyles original) (sk8::copy (textstyles original)) nil))
  (setf (slot-value me 'SelectionArray) (if (SelectionArray original) (sk8::copy (SelectionArray original)) nil))
  (setf (slot-value me 'ImageArray) (if (ImageArray original) (sk8::copy (ImageArray original)) nil))
  (call-next-method)
  (if (not (columnwidths me)) (setf (ColumnWidths me) (minwidth me))) 
  (if (not (rowheights me)) (setf (rowheights me) (minheight me))) 
  )

;;;----------------------------------------------------------------------------------------------------------------
;;;     MACROS and UTILITY FUNCTIONS...
;;;----------------------------------------------------------------------------------------------------------------


;;; Borrowed directly from picker.lisp
(defmacro making-TablePicker-dirty (theTablePicker &body body)
  (let ((tp (gensym)))
    `(let ((,tp ,theTablePicker))
       (gs:making-self-dirty (,tp t nil nil nil (gs:node-fillRegion ,tp) :fill nil)
         (progn ,@body)))))

(defmacro making-TablePicker-dirty-for-selection (theTablePicker &body body)
  (let ((sels (gensym))
        (currect (gensym))
        (curregn (gensym))
        (totalrgn (gensym))
        (hline (gensym))
        (vline (gensym))
        (tempib (gensym)))
    `(gs:let+ ((,sels (selection ,thetablepicker))
            (,currect (:Rect))
            (,curregn (:region))
            (,totalrgn (:region))
            (,hline (columnLinesSize ,thetablepicker))
            (,vline (rowLinesSize ,thetablepicker))
            (,tempib nil))
       (progn ,@body)
       (#_setEmptyRgn ,totalrgn)
       (dolist (i (append ,sels (selection ,thetablepicker)))
         (setf ,tempIB (itemboundsrect ,theTablePicker i :physical t))
         (when ,tempIB
           (sk8-multival-bind (ll tt rr bb) ,tempIB
             (sk8-multival-bind (lli tti rri bbi) (gs:stage-to-window-rect-coords ,theTablePicker ll tt rr bb)
               (rset ,currect :rect.left (- lli ,hline))
               (rset ,currect :rect.top (- tti ,vline))
               (rset ,currect :rect.right rri)
               (rset ,currect :rect.bottom bbi)
               (#_rectrgn ,curregn ,currect)
               (#_unionrgn ,curregn ,totalrgn ,totalrgn)))))
       (gs:making-self-dirty (,theTablePicker t nil nil nil ,totalrgn nil t)
         ))))

(defmacro making-TablePicker-dirty-for-element (theTablePicker i &body body)
  (let ((currect (gensym))
        (curregn (gensym))
        (hline (gensym))
        (vline (gensym))
        (tempib (gensym)))
    `(gs:let+ ((,currect (:Rect))
            (,curregn (:region))
            (,hline (columnLinesSize ,thetablepicker))
            (,vline (rowLinesSize ,thetablepicker))
            (,tempib nil))
       (progn ,@body)
       (setf ,tempIB (itemboundsrect ,theTablePicker ,i :physical t))
       (when ,tempIB
         (sk8-multival-bind (ll tt rr bb) ,tempIB
           (sk8-multival-bind (lli tti rri bbi) (gs:stage-to-window-rect-coords ,theTablePicker ll tt rr bb)
             (rset ,currect :rect.left (- lli ,hline))
             (rset ,currect :rect.top (- tti ,vline))
             (rset ,currect :rect.right rri)
             (rset ,currect :rect.bottom bbi)
             (#_rectrgn ,curregn ,currect)
             )))
       (gs:making-self-dirty (,theTablePicker t nil nil nil ,curregn nil t)
         ))))

(defmacro sk8-TablePicker-aref (theArray i1 i2)
  `(aref ,theArray (1- ,i1) (1- ,i2)))

(defmacro set-item (theSelection c r value)
  `(setf (sk8-TablePicker-aref ,theSelection ,c ,r) ,value))

(defun tuple-p (tuple)
  (and (listp tuple)
       (numberp (first tuple))
       (numberp (second tuple))
       (null (nthcdr 2 tuple))))

;;; map func left to right, top to bottom
(defun map-dims (cols rows func)
  (do ((c 1 (1+ c)))
      ((> c cols))
    (do ((r 1 (1+ r)))
        ((> r rows))
      (funcall func c r))))

(defun map-items (theTablePicker func return selected)
  (let* ((result nil)
        (theItems (items theTablePicker))
        (theSelection (if selected (selectionArray theTablePicker)))
        (noSelect (not selected))
        (cols (columns theTablePicker))
        (rows (rows theTablePicker))
        )
    (when (and theItems (> rows 0) (> cols 0))
      (map-dims cols rows
                #'(lambda (c r)
                    (if (or noSelect (sk8-TablePicker-aref theSelection c r))
                      (let ((val (funcall func (sk8-TablePicker-aref theItems c r) c r)))
                        (if return
                          (push val result))))))
      (nreverse result))))

#|

;;; This is not called by anyone and it also has congruency problems.

(define-handler mapItems (TablePicker function &key (return t) (selected nil) (setting t))
  (if setting
    (making-TablePicker-dirty
      me
      (let ((theItems (items me)))
        (map-items me #'(lambda (i c r)
                          (setf (sk8-TablePicker-aref theItems c r) (funcall function i c r)))
                   return selected)
        (SetupImages me)
        (recomputeSizes me)))
    (map-items me function return selected)))

|#

(define-handler fillSelection (TablePicker val)
  (making-TablePicker-dirty
    me
    (let ((theItems (items me)))
      (map-items me #'(lambda (i c r)
                        (declare (ignore i))
                        (setf (sk8-TablePicker-aref theItems c r) val))
                 nil t)
      (SetupImages me)
      (recomputeSizes me))))

(defun left-most (the-array)
  (let* ((dims (array-dimensions the-array))
         (cols (first dims))
         (rows (second dims)))
    (dotimes (c cols)
      (dotimes (r rows)
        (when (aref the-array c r) (return-from left-most (1+ c)))))
    nil))

(defun top-most (the-array)
  (let* ((dims (array-dimensions the-array))
         (cols (first dims))
         (rows (second dims)))
    (dotimes (r rows)
      (dotimes (c cols)
        (when (aref the-array c r) (return-from top-most (1+ r)))))
    nil))

(defun right-most (the-array)
  (let* ((dims (array-dimensions the-array))
         (cols (first dims))
         (rows (second dims)))
    (do ((c (1- cols) (1- c)))
        ((< c 0))
      (dotimes (r rows)
        (when (aref the-array c r) (return-from right-most (1+ c)))))
    nil))

(defun bottom-most (the-array)
  (let* ((dims (array-dimensions the-array))
         (cols (first dims))
         (rows (second dims)))
    (do ((r (1- rows) (1- r)))
        ((< r 0))
      (dotimes (c cols)
        (when (aref the-array c r) (return-from bottom-most (1+ r)))))
    nil))

(defun find-indices (theTablePicker theitem) 
  (let (vallist theitems)
    (if (listp theitem)
      (setf theitems theitem)
      (setf theitems (list theitem)))
    (map-items  theTablePicker #'(lambda (i c r)
                                   (if (memq i theitems)
                                     (push (list c r) vallist)))
                nil nil)
    (if (listp theitem)
      vallist
      (car vallist))
    ))

(defun tuple-selected (theTablePicker tuple)
  (and (tuple-p tuple) (sk8-TablePicker-aref (selectionArray theTablePicker) (first tuple) (second tuple))))

(defun max-list-length (lists)
  (let ((result 1))
    (dolist (e lists)
      (if (listp e)
        (setq result (max result (length e)))))
    result))

;;;----------------------------------------------------------------------------------------------------------------
;;;;  Follow the Picker Style of things...
;;;----------------------------------------------------------------------------------------------------------------

(define-handler selectionCompleted (TablePicker)
  )

(define-handler createTextDisplayItem (TablePicker theObject)
  (typecase theObject
    (null "")
    (string theObject)
    (otherwise (objectString theObject))))

(define-handler createDisplayItem (TablePicker theItem)
  (if (inheritsFrom theItem Renderer)
    theItem
    (createTextDisplayItem me theItem)))

(define-handler (setf linespacing) (theval TablePicker)
  (setf (rowspacing me) theval))

(define-handler linespacing (TablePicker)
  (rowspacing me))

;;;----------------------------------------------------------------------------------------------------------------
;;; Compute minimal values for column widths and row heights.  Called when items are set and the cols or rows are not rigid.
;;;----------------------------------------------------------------------------------------------------------------

(define-handler ComputeColumnWidths (TablePicker)
  (unless (and (columnwidths me)
               (= (length (columnwidths me)) (columns me))
               (columnsRigid me))
    (let* ((theImages (imageArray me))
           (columnwidths (columnwidths me))
           (newwlist nil)
           (new-width (minWidth me)))
      (unless (= (length columnwidths) (columns me))
        (setf columnwidths nil)
        (dotimes (i (columns me))
          (setf columnwidths (cons new-width columnwidths))))
      (dotimes (c (columns me))
        (dotimes (r (rows me))
          (let ((image (aref theImages c r)))
            (if (stringp image)
              (setq new-width (max new-width (- (car (gs:actor-text-size me :tryText image)) 6)))
              (setq new-width (max new-width (nth c columnwidths))))))  ;;; preserve the renderers size at minimum...
        (push new-width newwlist)
        (setf new-width (minwidth me))
        )
     (setf (columnwidths me) (reverse newwlist))
      ))
    )

(define-handler ComputeRowHeights (TablePicker) 
  (unless (and (RowHeights me)
               (= (length (RowHeights me)) (rows me)) 
               (rowsRigid me))
    (let ((rowheights (rowheights me))
          (newhlist nil)
          (theImages (imageArray me))
          (new-height (minheight me))
          )
      (unless (= (length rowheights) (rows me))
        (setf rowheights nil)
        (dotimes (i (rows me))
          (setf rowheights (cons new-height rowheights))))
      (dotimes (r (rows me))
        (dotimes (c (columns me))
          (let ((image (aref theImages c r)))
            (if (stringp image)
              (setq new-height (max new-height (cadr (gs:actor-text-size me :tryText image))))
              (setq new-height (max new-height (nth r rowheights))))))  ;;; preserve the renderers size at minimum...
        (push new-height newhlist)
        (setf new-height (minheight me))
        )
      (setf (rowheights me) (reverse newhlist))
      )))

(define-handler recomputeSizes (TablePicker)
  (ComputeColumnWidths me)
  (ComputeRowHeights me))

;;;----------------------------------------------------------------------------------------------------------------
;;; Handlers for width, height and size so the user can do this for items...
;;; note for the accessor functions a :how of partialitems will include partially exposed items in the count...
;;;----------------------------------------------------------------------------------------------------------------

(define-handler width (TablePicker &key how)
  (if (or (eq how 'items) (eq how 'partialItems))
    (let ((col (columns me))
          (columnwidths (columnwidths me))
          (ww (width me))
          (lv (columnspacing me))
          (horizontalScroll (max 0 (horizontalScroll me)))
          (curww 0)
          (thecount 0))
      (if (zerop horizontalscroll) (incf curww (tablehoffset me)))
      (do  ((theIndex horizontalScroll (1+ theIndex)))
           ((>= theIndex col) nil)
        (incf curww (nth theindex columnwidths))
        (incf curww lv)
        (if (>= curww ww) 
          (progn
            (if (eq how 'partialItems)
              (incf thecount))
            (return)))
        (incf thecount))
      (min col thecount))
    (call-next-method)))

(define-handler (setf width) (newWidth TablePicker &key (how nil) (relative nil))
  (let* ((hs (horizontalscroll me))
         (ww (width me :how 'items))
         (colspaces (* ww (columnspacing me)))
         newww
         (cw (columnwidths me)))
    (if (eq how 'items)
      (when (numberp newWidth)
        (setf newww (if relative 
                      (+ colspaces (if (zerop hs) (tablehoffset me) 0)
                         (round (* (- newwidth 0.5) (columnspacing me)))
                         (* newwidth (columnspacing me)) (apply #'+ (subseq cw hs (+ hs ww newwidth))))
                      (+ colspaces (if (zerop hs) (tablehoffset me) 0)
                         (round (* (- newwidth 0.5) (columnspacing me)))
                         (apply #'+ (subseq cw hs (+ hs newwidth))))
                      ))
        (setf (width me) newww))
      (call-next-method))))

(define-handler height (TablePicker &key how)
  (if (or (eq how 'items) (eq how 'partialItems))
    (let ((row (rows me))
          (rowheights (rowheights me))
          (hh (height me))
          (lh (rowspacing me))
          (verticalScroll (max 0 (round (verticalScroll me))))
          (curhh 0)
          (thecount 0))
      (if (zerop verticalScroll) (incf curhh (tablevoffset me)))
      (do  ((theIndex verticalScroll (1+ theIndex)))
           ((>= theIndex row) nil)
        (incf curhh (nth theindex rowheights))
        (incf curhh lh)
        (if (>= curhh hh) 
          (progn
            (if (eq how 'partialItems)
              (incf thecount))
            (return)))
        (incf thecount))
      (min row thecount))
    (call-next-method)))

(define-handler (setf height) (newHeight TablePicker &key how relative) 
  (let* ((vs (round (verticalscroll me)))
         (hh (height me 'items))
         (rowspaces (* hh (rowspacing me)))
         newhh
         (rw (rowheights me)))
    (if (eq how 'items)
      (when (numberp newHeight)
        (setf newhh (if relative 
                      (+ rowspaces (if (zerop vs) (tablevoffset me) 0)
                         (round (* (- newHeight 0.5) (rowSpacing me)))
                         (* newHeight (rowSpacing me)) (apply #'+ (subseq rw vs (+ vs hh newHeight))))
                      (+ rowspaces (if (zerop vs) (tablevoffset me) 0)
                         (round (* (- newHeight 0.5) (rowSpacing me)))
                         (* newHeight (rowSpacing me)) (apply #'+ (subseq rw vs (+ vs newHeight))))
                      ))
        (setf (height me) newhh))
      (call-next-method))))

(define-handler size (TablePicker &key how)
  (if (or (eq how 'items) (eq how 'partialitems))
    (sk8-multivals (width me :how how) (height me :how how))
    (call-next-method)))

(define-handler (setf size) (newSize TablePicker &key how relative)
  (if (eq how 'items)
    (when (tuple-p newSize)
      (let ((cols (first newSize))
            (rows (second newSize)))
        (when (> cols 0)
          (setf (width me :relative relative :how 'items) cols))
        (when (> rows 0)
          (setf (height me :relative relative :how 'items) rows))))
    (call-next-method)))


;;;----------------------------------------------------------------------------------------------------------------
;;;These are all the after methods on our properties to force recomputation and redraws... 
;; What do we do with these bad boys???
;; (minWidth :inherit :value 10)
;; (columnWidths :inherit :value (10))
;; (minHeight :inherit :value 10)
;; (rowHeights :inherit :value (10))
;;;----------------------------------------------------------------------------------------------------------------

(define-handler (setf TableHoffset) :after (val TablePicker)
                (declare (ignore val))
                (forceredraw me))

(define-handler (setf TableVoffset) :after (val TablePicker)
                (declare (ignore val))
                (forceredraw me))

(define-handler (setf rowLinesSize) :after (val TablePicker)
                (declare (ignore val))
                (forceredraw me))
(define-handler (setf columnLinesSize) :after (val TablePicker)
                (declare (ignore val))
                (forceredraw me))

(define-handler (setf HighlightColor) :after (val TablePicker)
                (declare (ignore val))
                (forceredraw me))

(define-handler (setf selectionBorder) :after (val TablePicker)
                (declare (ignore val))
                (forceredraw me))

(define-handler (setf columnsRigid) :after (val TablePicker)
                (unless val
                  (recomputeSizes me)))

(define-handler (setf rowsRigid) :after (val TablePicker)
                (unless val
                  (recomputeSizes me)))

;;;----------------------------------------------------------------------------------------------------------------

(define-handler (setf columnSpacing) (columnSpacing TablePicker &key (relative nil))
  (making-TablePicker-dirty me
    (if relative
      (incf columnSpacing (columnSpacing me)))
    (sk8::setValue 'columnSpacing me columnSpacing)
    ;; (ComputeColumnWidths me)
    (when (partnerHScroller me)
      (update-horizontal-partnerScroller me))))

(define-handler (setf rowSpacing) (spacing TablePicker &key (relative nil))
  (making-TablePicker-dirty me
    (if relative
      (incf spacing (rowSpacing me)))
    (sk8::setValue 'rowSpacing me spacing)
    ;;  (ComputeRowHeights me)
    (when (partnerVScroller me)
      (update-vertical-partnerScroller me))))

(define-handler (setf horizontalScroll) (val TablePicker &key (relative nil)) ;;;FIX for Scrolling
  (when (numberp val)
    (when relative
      (incf val (horizontalScroll me)))
    (let ((cols (columns me)))
      (setq val (gs:range 0 val (max 0 (1- cols)))))
    (let ((h (partnerHScroller me)))
      (when (and h (neq lockedScroller h))
        (setf (currentValue h) val))
      (making-TablePicker-dirty me
        (sk8::setValue 'horizontalScroll me val)))
    ))

(define-handler (setf verticalScroll) (val TablePicker &key (relative nil)) ;;;FIX for Scrolling
  (when (numberp val)
    (when relative
      (incf val (verticalScroll me)))
    (let ((rows (rows me)))
      (setq val (gs:range 0 val (max 0 (1- rows)))))
    (let ((v (partnerVScroller me)))
      (when (and v (neq lockedScroller v))
        (setf (currentValue v) val))
      (making-TablePicker-dirty me
        (sk8::setValue 'verticalScroll me val)))))

;;; Is silently ignoring the right way to disable scaling?
(define-handler (setf scale) (newScale TablePicker)
  (declare (ignore newScale)))

(define-handler element (TablePicker item)
  (if (tuple-p item)
    (sk8-TablePicker-aref (items me) (first item) (second item))))

(define-handler (setf element) (newVal TablePicker item)
  (if (tuple-p item)
    (making-TablePicker-dirty-for-element
      me item
      (let ((col-index (first item))
            (row-index (second item))
            (val (createDisplayItem me newval)))
        (setf (sk8-TablePicker-aref (items me) col-index row-index) newVal)
        (unless (or (stringp val) (is-a val renderer)) 
          (sk8-error GeneralProgrammaticError
                     :strings '("CreateDisplayItem and CreateTextDisplayItem must return a string or a renderer."))
          )
        (setf (sk8-TablePicker-aref (imageArray me) col-index row-index) val)
        (when (createtextcolors me)
          (if (arrayp (textcolors me))
            (when (stringp (sk8-TablePicker-aref (imageArray me) col-index row-index)) 
              (setf (sk8-TablePicker-aref (textcolors me) col-index row-index)
                    (CreateTextColor me (list col-index row-index))))
            (setuptextcolors me)))
        (when (createtextstyles me)
          (if (arrayp (textstyles me))
            (when (stringp (sk8-TablePicker-aref (imageArray me) col-index row-index)) 
              (setf (sk8-TablePicker-aref (textstyles me) col-index row-index)
                    (CreateTextStyle me (list col-index row-index))))
            (setuptextstyles me)))
        (recomputeSizes me)))))

(define-handler (setf text) (v TablePicker &key (item nil))
  (if item
    (setf (element me item) v)
    (call-next-method)))

(define-handler text (TablePicker &key item)
  (cond
   ((tuple-p item) (let ((image (imageArray me))
                         theval)
                     (setf theval (sk8-TablePicker-aref image (first item) (second item)))
                     (if (stringp theval)
                       theval
                       (createtextdisplayitem me theval))))
   ((null item) (call-next-method))))

(define-handler (setf columnWidths) (newWidth TablePicker)
  (let (curlist)
    (cond
     ((numberp newwidth)
      (setf newwidth (round newwidth))
      (making-TablePicker-dirty me
        (dotimes (i (columns me))
          (setf curlist (cons newWidth curlist)))
        (sk8::setValue 'columnWidths me curlist)
        (when (partnerHScroller me)
          (update-horizontal-partnerScroller me))))
     ((null newwidth) (if (> (columns me) 0)
                        (computecolumnwidths me)
                        (sk8::setValue 'columnWidths me newwidth)))
     ((listp newwidth) (sk8::setValue 'columnWidths me newwidth))
     (t (computecolumnwidths me)))
    (forceredraw me)))

(define-handler (setf rowHeights) (newHeight TablePicker)
  (let (curlist)
    (cond
     ((numberp newHeight)
      (setf newHeight (round newHeight))
      (making-TablePicker-dirty me
        (dotimes (i (rows me))
          (setf curlist (cons newheight curlist)))
        (sk8::setValue 'rowHeights me curlist)
        (when (partnerVScroller me)
          (update-vertical-partnerScroller me))))
     ((null newHeight)  (if (> (rows me) 0)
                          (computeRowHeights me)
                          (sk8::setValue 'rowHeights me newHeight)))
     ((listp newHeight) (sk8::setValue 'rowHeights me newHeight))
     (t (computeRowHeights me)))
    (forceredraw me)))


;;;----------------------------------------------------------------------------------------------------------------
;;;  Array stuff
;;;----------------------------------------------------------------------------------------------------------------

(define-handler rows (tablePicker)
  (if (items me)
    (cadr (array-dimensions (items me)))
    0))

(define-handler (setf rows) (newRows TablePicker)
  (SetUpItems me :dimensions (list (columns me) newRows))
  (UpdateTablePicker me))

(define-handler columns (tablePicker)
  (if (items me)
    (car (array-dimensions (items me)))
    0))

(define-handler (setf columns) (newColumns TablePicker)
  (SetUpItems me :dimensions (list newcolumns (rows me)))
  (UpdateTablePicker me))

(define-handler elements (tablePicker)
  (if (items me)
    (let ((ad (array-dimensions (items me))))
      (* (car ad) (cadr ad)))
    0))

(define-handler dimensions (TablePicker)
  (let ((xx (slot-value me 'items)))
    (if (arrayp xx)
      (array-dimensions xx)
      '(0 0))))

(define-handler (setf dimensions) (newDims TablePicker)
  (SetUpItems me :dimensions newDims)  
  (UpdateTablePicker me))

(define-handler (setf lastSelection) (newSel TablePicker)
  (if (tuple-p newsel)
    (progn
      (setf (first newSel) (gs:range 1 (first newSel) (columns me)))
      (setf (second newSel) (gs:range 1 (second newSel) (rows me))))
    (setf newsel nil))
  (sk8::setValue 'lastSelection me newSel))

(define-handler SetupImages (TablePicker)
  (let ((theImages (imageArray me))
        (theItems (items me))
        val)
    (if (arrayp theImages)
      (adjust-array theImages (dimensions me))
      (sk8::setValue 'imageArray me (new array :dimensions (dimensions me))))
    (setf theImages (imageArray me))
    (dotimes (r (rows me))
      (dotimes (c (columns me))
        (if theitems
          (progn
            (setf val (createDisplayItem me (aref theItems c r)))
            (unless (or (stringp val) (is-a val renderer)) 
              (sk8-error GeneralProgrammaticError
                     :strings '("CreateDisplayItem and CreateTextDisplayItem must return a string or a renderer."))))
          (setf val ""))
        (setf (aref theImages c r) val)))
    theImages))

(define-handler SetupSelectionArray (TablePicker &key dimensions)
  (let ((selarray (selectionArray me)))
    (unless dimensions 
      (setf dimensions (dimensions me)))
    (if (arrayp selarray)
      (adjust-array selarray dimensions)
      (sk8::setValue 'selectionArray me (new array :dimensions dimensions)))
    ))

(define-handler SetUpItems (TablePicker &key dimensions)
  (let ((items (getvalue 'items me)))
    (unless dimensions 
      (setf dimensions (dimensions me)))
    (if (arrayp items)
      (adjust-array items dimensions)
      (sk8::setValue 'items me (new array :dimensions dimensions)))
    ))

(define-handler CreateTextColor (TablePicker itemLocation)
  (declare (ignore itemlocation))
  nil)

(define-handler SetUpTextColors (TablePicker &key dimensions)
  (if (createTextColors me)
    (let ((theItems (textcolors me))
          (images (imageArray me)))
      (unless dimensions 
        (setf dimensions (dimensions me)))
      (if (arrayp theItems)
        (adjust-array theItems dimensions)
        (setf (slot-value me 'TextColors) (new array :dimensions dimensions)))
      (setf theitems (textcolors me))
      (dotimes (c (car dimensions))
        (dotimes (r (cadr dimensions))
          (when (stringp (aref images c r)) (setf (aref theitems c r) (CreateTextColor me (list (1+ c) (1+ r)))))))
      )
    (setf (textcolors me) nil)))

(define-handler CreateTextStyle (TablePicker itemLocation)
  (declare (ignore itemlocation))
  nil)

(define-handler SetUpTextStyles (TablePicker &key dimensions)
  (unless dimensions 
    (setf dimensions (dimensions me)))
  (if (and (> (car dimensions) 0) (> (cadr dimensions) 0) (createTextStyles me))
    (let ((theItems (textstyles me))
          (images (imageArray me)))
      (if (arrayp theItems)
        (adjust-array theItems dimensions)
        (setf (slot-value me 'textstyles) (new array :dimensions dimensions)))
      (setf theitems (textstyles me))
      (dotimes (c (car dimensions))
        (dotimes (r (cadr dimensions))
          (when (stringp (aref images c r)) (setf (aref theitems c r) (CreateTextStyle me (list (1+ c) (1+ r)))))))
      )
    (setf (textstyles me) nil)))

(define-handler UpdateTablePicker (TablePicker)
  (making-TablePicker-dirty
    me
    (SetupImages me)
    (SetupSelectionArray me)
    (SetUpTextColors me)
    (SetUpTextStyles me)
    (recomputeSizes me)))

#|
(define-handler items (TablePicker)
  (let ((xx (getvalue 'items me)))
    (if (and (arrayp xx)
             (or (= (car (array-dimensions xx)) 0)
                 (= (cadr (array-dimensions xx)) 0)))
      nil
      xx))
  )
|#

(define-handler (setf items) (newItems TablePicker)
   (withactorlocked (me)
     (let (theItems)
       (typecase newItems
         (null
          (setf (slot-value me 'items) nil))
         (list (let ((cols (max-list-length newItems))
                     (rows (length newItems)))
                 (SetUpItems me :dimensions (list cols rows))  
                 (setf theitems (items me))
                 (do ((r 0 (1+ r))
                      (remaining-rows newItems (cdr remaining-rows))
                      this-row)
                     ((>= r rows))
                   (setq this-row (car remaining-rows))
                   (do ((c 0 (1+ c))
                        (remaining-cols this-row
                                        (if (listp remaining-cols)
                                          (cdr remaining-cols)
                                          remaining-cols))
                        this-col)
                       ((>= c cols))
                     (if (listp remaining-cols)
                       (setq this-col (car remaining-cols))
                       (setq this-col remaining-cols))
                     (setf (aref theItems c r) this-col)))
                 ))
         (array (let ((dims (array-dimensions newItems)))
                  (unless (= 2 (length dims))
                    (sk8-error GeneralProgrammaticError
                               :strings '("Items must be a list or a 2-dimensional array in a TablePicker"))
                    )
                  (sk8::setValue 'items me newItems)))
         (otherwise (sk8-error GeneralProgrammaticError
                               :strings '("Items must be a list or a 2-dimensional array in a TablePicker"))))
       (updateTablePicker me)
       (setf (horizontalscroll me) 0)
       (setf (VerticalScroll me) 0)
       (unless newitems 
         (setf (lastselection me) nil)
         (setf (lastselected me) nil) ;;; clear this to be sure it isn't holding onto objects...
         )
       )
     ))

(define-handler (setf items) :after (newitems tablepicker)
                (declare (ignore newitems))
                (when (items me)
                  (unless (and (equal (array-dimensions (items me)) (array-dimensions (selectionarray me)))
                               (equal (array-dimensions (items me)) (array-dimensions (imagearray me))))
                    (setf (items me) nil)
                    (error "Arrays not in synch.  Tablepicker's items are set to false to prevent further errors.")))
                (when (partnerHScroller me)
                  (update-horizontal-partnerScroller me))
                (when (partnerVScroller me)
                  (update-vertical-partnerScroller me)))

;;;----------------------------------------------------------------------------------------------------------------
;;;  Selection Specific stuff

(define-handler clearSelection (TablePicker &key (redraw t))
  (if redraw
    (making-TablePicker-dirty
      me
      (let ((theSelection (selectionArray me)))
        (map-items me #'(lambda (i c r)
                          (declare (ignore i))
                          (setf (sk8-TablePicker-aref theSelection c r) nil))
                   nil t)))
    (let ((theSelection (selectionArray me)))
      (map-items me #'(lambda (i c r)
                        (declare (ignore i))
                        (setf (sk8-TablePicker-aref theSelection c r) nil))
                 nil t)))
    
    )

(define-handler selectAll (TablePicker)
  (if (memq (selectionStyle me) (list 'contiguous 'discontiguous 'multiple))
    (making-TablePicker-dirty
      me
      (let ((theSelection (selectionArray me)))
        (map-items me #'(lambda (i c r)
                            (declare (ignore i))
                            (setf (sk8-TablePicker-aref theSelection c r) t))
                   nil nil)))))

(define-handler (setf selectionStyle) (newStyle TablePicker)
  (if (memq newstyle (list 'contiguous 'discontiguous)) (setf newstyle 'multiple))
  (case newStyle
    (multiple)
    (single ;; Clear all except the first item
     (making-TablePicker-dirty
       me
       (let ((keeper (lastSelection me))
             (theSelection (selectionArray me)))
         (map-items me #'(lambda (i c r)
                             (declare (ignore i))
                             (unless keeper (setq keeper (list c r)))
                             (unless (and (= c (first keeper)) (= r (second keeper)))
                               (setf (sk8-TablePicker-aref theSelection c r) nil)))
                    nil t))))
    (otherwise (sk8-error GeneralProgrammaticError
                               :strings '("selectionStyle must be either 'single' or 'multiple' for TablePickers"))
              ))
  (sk8::setValue 'selectionStyle me newStyle))

(define-handler (setf selectionArray) (newArray TablePicker)
  (declare (ignore newArray))
  (error "use 'set selection' or 'set selectedItems' to change the selection of a TablePicker"))

(define-handler (setf imageArray) (newArray TablePicker)
  (declare (ignore newArray))
  (error "you may not modify the imageArray of a TablePicker directly"))

(defun single-select (theTablePicker theSelection c r)
  (ClearSelection theTablePicker :redraw nil)
  (setf (sk8-TablePicker-aref theSelection c r) t))

(defun set-tuple (theTablePicker item value style theSelection)
  (let ((first-item (round (first item)))
        (second-item (round (second item))))
    (if (eq style 'multiple)
      (set-item theSelection first-item second-item value)
      (single-select theTablePicker theSelection first-item second-item))))

(defun add-selectedItem (item theTablePicker)
  (let ((theSelection (selectionArray theTablePicker))
        (style (selectionStyle theTablePicker)))
    (map-items theTablePicker #'(lambda (i c r)
                                  (when (eq item i)
                                    (set-item theSelection c r t)
                                    (if (eq style 'single)
                                      (return-from add-selectedItem))))
               nil nil)))

(defun set-selection (theTablePicker items value adding start end)
  (unless adding
    (ClearSelection theTablePicker :redraw nil))
  (let ((style (selectionStyle theTablePicker))
        (theSelection (selectionArray theTablePicker))
        (cols (columns theTablePicker))
        (rows (rows theTablePicker)))
    (when (and (tuple-p start) (tuple-p end))
      (let ((col-low (gs:range 1 (min (first start) (first end)) cols))
            (col-high (gs:range 1 (max (first start) (first end)) cols))
            (row-low (gs:range 1 (min (second start) (second end)) rows))
            (row-high (gs:range 1 (max (second start) (second end)) rows))
            (item (list nil nil)))
        (do ((r row-low (1+ r)))
            ((> r row-high))
          (setf (second item) r)
          (do ((c col-low (1+ c)))
              ((> c col-high))
            (setf (first item) c)
            (set-tuple theTablePicker item value style theSelection)))))
    (cond
     ((tuple-p items)
      (setf (first items) (gs:range 1 (first items) cols))
      (setf (second items) (gs:range 1 (second items) rows))
      (set-tuple theTablePicker items value style theSelection))
     ((listp items)
      (dolist (item items)
        (when (tuple-p item)
          (setf (first item) (gs:range 1 (first item) cols))
          (setf (second item) (gs:range 1 (second item) rows))
          (set-tuple theTablePicker item value style theSelection)))))))

(define-handler selection (TablePicker)
  (map-items me #'(lambda (i c r)
                      (declare (ignore i))
                      (list c r))
             t t))

(define-handler (setf selection) (items TablePicker &key (deselecting t) start end (off nil) (selectionShown t))
  (when (and (items me) (> (rows me) 0) (> (columns me) 0))
    (making-TablePicker-dirty-for-selection
      me
      (set-selection me items (not off) (not deselecting) start end)
      (setf (lastselection me) (if (tuple-p items) items (car items)))
      (when selectionShown (showSelection me))
      ))
  )

(define-handler selectedItems (TablePicker)
  (map-items me #'(lambda (i c r)
                      (declare (ignore c r))
                      i)
             t t))

(define-handler (setf selectedItems) (items TablePicker &key (deselecting t))
  (if (and (items me) (> (rows me) 0) (> (columns me) 0) items)
    (let ((multiple (eq 'multiple (selectionStyle me))))
      (unless (listp items) (setf items (list items)))
      (unless multiple (setf items (list (car items))))
      (making-TablePicker-dirty-for-selection
        me
        (setf (selection me :deselecting deselecting) (find-indices me items) )   
        (setf (lastselection me) (find-indices me (car items))))
      (showselection me)
      t)
    (clearselection me :redraw t)))

;;;---For convenience and for compatibility with some other objects...
(define-handler selectedItem (TablePicker)
  (car (selecteditems me)))

(define-handler (setf selectedItem) (item TablePicker &key (deselecting t))
  (declare (ignore deselecting))
  (if (listp item) (setf item (car item)))
  (setf (selecteditems me) (list item)))

;;;;
;;;; Stuff for showing and aligning the selection...
;;;;
;;;; Recognized values for alignment:
;;;; (alignment-values '(left right top bottom
;;;;                    topleft toght bottomleft bottomright
;;;;                    leftcenter topcenter bottomcenter rightcenter
;;;;                    center))
;;;; All other values are interpreted as nil
;;;; 

(defun show-range (theTablePicker curItem)
  (if (items thetablepicker)
    (cond
     ((tuple-p curItem)
      (sk8-multivals (first curItem) (second curItem) (first curItem) (second curItem)))
     (t
      (let ((theSelection (selectionArray theTablePicker)))
        (sk8-multivals (left-most theSelection) (top-most theSelection)
                       (right-most theSelection) (bottom-most theSelection)))))
    (sk8-multivals nil nil nil nil)))

(defun align-selection (theTablePicker curItem alignment)
  (sk8-multival-bind (show-left show-top show-right show-bottom) (show-range theTablePicker curItem)
    (when show-left
      (let* ((start-hScroll (1+ (round (horizontalScroll theTablePicker))))
             (hScroll start-hScroll)
             (start-vScroll (1+ (round (verticalScroll theTablePicker))))
             (vScroll start-vScroll)
             (visCols (width theTablePicker :how 'items))
             (visRows (height theTablePicker :how 'items))
             (maxhscroll (1+ (max 0 (- (columns theTablePicker) viscols))))
             (maxvscroll (1+ (max 0 (- (rows theTablePicker) visRows))))
             )
        ;;; Figure out horizontal position
        (case alignment
          ((left topleft bottomleft leftcenter)
           (setq hScroll show-left))
          ((right topright bottomright rightcenter)
           (setf hScroll (1+ (- show-right visCols))))
          ((center topcenter bottomcenter)
           (setf hScroll (ceiling (- (+ show-left show-right) visCols) 2))
           )
          (otherwise
           (cond
            ((<= show-right hScroll)
             (setf hScroll show-left))
            ((>= show-left (+ hScroll visCols))
             (setf hScroll (1+ (- show-right visCols)))))))
        ;;; Figure out vertical position
        (case alignment
          ((top topleft topright topcenter)
           (setq vScroll show-top))
          ((bottom bottomleft bottomright bottomcenter)
           (setf vScroll (1+ (- show-bottom visRows))))
          ((center leftcenter rightcenter)
           (setf vScroll (ceiling (- (+ show-top show-bottom) visRows) 2))
           )
          (otherwise
           (cond
            ((<= show-bottom vScroll)
             (setf vScroll show-top))
            ((>= show-top (+ vScroll visRows))
             (setf vScroll (1+ (- show-bottom visRows)))))))
        (setf hscroll (min hscroll maxhscroll))
        (setf vscroll (min vscroll maxvscroll))
        (unless (and (eql hscroll start-hscroll) (eql vscroll start-vscroll))
          (withActorLocked (theTablePicker)
            (setf (horizontalScroll theTablePicker) (1- hScroll))
            (setf (verticalScroll theTablePicker) (1- vScroll))))
        (or (/= hScroll start-hScroll) (/= vScroll start-vScroll))))))

(define-handler showSelection (TablePicker &key curItem)
  (declare (ignore curitem))
  (align-selection me nil nil))

(define-handler alignSelection (TablePicker &key (alignment 'center))
  (align-selection me nil alignment))

;;;----------------------------------------------------------------------------------------------------------------
;;;Drawing and Event Handling...
;;;----------------------------------------------------------------------------------------------------------------

(defun fastTablePicker-draw (theTablePicker thePort draw-rgn)
  (gs:let+ ((flags (gs:node-flags theTablePicker))
            (TableHOffset (TableHOffset theTablePicker))
            (TableVOffset (TableVOffset theTablePicker))
            (columnWidths (columnWidths theTablePicker))
            (rowHeights (rowHeights theTablePicker))
            (columnSpacing (columnSpacing theTablePicker))
            (rowSpacing (rowSpacing theTablePicker))
            (HalfColSpace (round columnSpacing 2))
            (HalfRowSpace (round rowSpacing 2))
            (HighlightSelection (HighlightSelection theTablePicker))
            (cols (columns theTablePicker))
            (rows (rows theTablePicker))
            (startCol (max 0 (round (horizontalScroll theTablePicker))))
            (startRow (max 0 (round (verticalScroll theTablePicker))))
            (theSelection (selectionArray theTablePicker))
            (theImages (imageArray theTablePicker))
            (theTextStyles (textstyles theTablePicker))
            (thetextcolors (textcolors theTablePicker))
            (qd-rect (:rect))
            fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom
            theImage
            (DirtyRect (gs:hrref draw-rgn :region.rgnbbox :storage :pointer))
            (fill-rgn (gs:node-fillRegion theTablePicker))
            (box-rgn (:region))
            (clip-rgn (:region))
            default-text-style
            (default-text-color (sk8::mcl-color (textcolor theTablePicker)))
            (dlc (linecolor theTablePicker))
            (default-line-color (and dlc (neq dlc transparent) (sk8::mcl-color dlc)))
            (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
            (theItems (items theTablePicker))
            (plist (gs:node-properties theTablePicker))
            (hLineSize (rowLinesSize theTablePicker))
            (vLineSize (columnLinesSize theTablePicker))
            (selBorder (selectionBorder theTablePicker)))
    (#_TextFont (fontData (getf plist :text-font ChicagoFont)))
    (setf default-text-style (getf plist :text-style 0))
    (#_TextFace default-text-style)
    (#_TextSize (getf plist :text-size 0))
    (gs:with-clipped-region draw-rgn
      ;; Render the fill and the frame
      (gs:recompute-fill-region theTablePicker flags)
      (gs:recompute-frame-region theTablePicker flags)
      (render (gs:node-fillcolor theTablePicker) theTablePicker draw-rgn thePort) ;;fill-rgn)
      (render (gs:node-frameColor theTablePicker) theTablePicker (gs:node-frameRegion theTablePicker) thePort)
      (with-rgb (rgb default-text-color)
        (#_RGBForeColor rgb))
      (with-rgb (rgb (sk8::mcl-color (gs:node-fillcolor theTablePicker)))
        (#_RGBBackColor rgb))
      
      (setq fill-rect-left (rref fill-rect :rect.left))                       ;;(gs:f.round hOffset)))
      (setq fill-rect-top (rref fill-rect :rect.top))                         ;;(gs:f.round vOffset)))
      (setq fill-rect-right (rref fill-rect :rect.right))                     ;;(gs:f.round hOffset)))
      (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
      (setq fill-rect-bottom (rref fill-rect :rect.bottom))                   ;;(gs:f.round vOffset)))
      (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
      
      (#_PenNormal)
      
      (when theItems
        (if (eql startRow 0) (incf fill-rect-top TableVOffset))
        (if (eql startCol 0) (incf fill-rect-left TableHOffset))
        
        ;; Adjust the bounds
        (let (tmp)
          (setq tmp (+ fill-rect-top (* (- rows startrow) rowspacing) (apply #'+ (nthcdr startRow rowheights))))
          (when (> fill-rect-bottom tmp)
            (setq fill-rect-bottom tmp))
          (setq tmp (+ fill-rect-left (* (- cols startcol) columnspacing) (apply #'+ (nthcdr startCol columnwidths))))
          (when (> fill-rect-right tmp)
            (setq fill-rect-right tmp)))
        
        ;; Draw the top left edge
        (#_sectRgn fill-rgn draw-rgn clip-rgn)
        (unless (and (eql 0 hLineSize) (eql 0 vLineSize))
          (#_PenNormal)
          (#_SetClip clip-rgn)
          ;(#_MoveTo fill-rect-right fill-rect-top)
          ;(#_LineTo fill-rect-left fill-rect-top)
          ;(#_LineTo fill-rect-left fill-rect-bottom)
          
          (when default-line-color
            (with-rgb (rgb default-line-color)
              (#_RGBForeColor rgb))
            
            (unless (eql 0 hLineSize)
              (#_PenSize 1 hLineSize)
              (#_MoveTo fill-rect-left fill-rect-top)
              (#_LineTo fill-rect-right fill-rect-top)
              (do ((cur-Row startRow (1+ cur-Row))
                   (top fill-rect-top bottom)
                   (curhh (nthcdr startRow rowheights) (cdr curhh))
                   (bottom fill-rect-top bottom))
                  ((or (> cur-row rows) 
                       (>= bottom fill-rect-bottom)))
                (setf bottom (+ top rowspacing (car curhh)))
                (when (>= cur-Row 0)
                  (#_MoveTo fill-rect-left bottom)
                  (#_LineTo fill-rect-right bottom))
                ))
            
            (unless (eql 0 vLineSize)
              (#_PenSize vLineSize 1)
              (#_MoveTo fill-rect-left fill-rect-top)
              (#_LineTo fill-rect-left fill-rect-bottom)
              (do ((cur-Col startCol (1+ cur-Col))
                   (left fill-rect-left right)
                   (curvv (nthcdr startcol columnwidths) (cdr curvv))
                   (right fill-rect-left right))
                  ((or (> cur-col cols) (>= right fill-rect-right)))
                (setf right (+ left columnspacing (car curvv)))
                (when (>= cur-Col 0)
                  (#_MoveTo right fill-rect-top)
                  (#_LineTo right fill-rect-bottom))
                )))
          
          (#_PenSize :long #@(1 1)))
        (#_penNormal)
        ;; Draw the cells and the lines.
        (do ((cur-Row startRow (1+ cur-Row))
             (top fill-rect-top bottom)
             (curhh (nthcdr startrow rowheights) (cdr curhh))
             (bottom 0 bottom))
            ((or (>= cur-row rows) (>= top fill-rect-bottom)))
          (setf bottom (+ top RowSpacing (car curhh)))
          (when (>= cur-Row 0)
            (do ((cur-Col startCol (1+ cur-Col))
                 (left fill-rect-left right)
                 (curvv (nthcdr startCol columnwidths) (cdr curvv))
                 (right 0 right))
                ((or (>= cur-col cols) (>= left fill-rect-right)))
              (setf right (+ left columnspacing (car curvv)))
              (when (>= cur-Col 0)
                (setf theImage (aref theImages cur-Col cur-Row ))
                ;; draw-text-not-wrapped only cares about left and top
                (when theImage
                  (if (stringp theImage)
                    (progn
                      (rset qd-rect :rect.left (+ left halfColSpace))
                      (rset qd-rect :rect.top (+ top halfRowSpace))
                      (rset qd-rect :rect.right (- right halfColSpace))
                      (rset qd-rect :rect.bottom (- bottom halfRowSpace))
                      (when (gs:rects-intersect DirtyRect qd-rect)
                        (#_RectRgn box-rgn qd-rect)
                        (#_SectRgn clip-rgn box-rgn box-rgn)
                        (#_SetClip box-rgn)
                        (if (and thetextstyles (aref thetextstyles cur-Col cur-Row)) 
                          (#_TextFace (gs:get-style-number (aref thetextstyles cur-Col cur-Row)))
                          (#_TextFace default-text-style))
                        (if (and thetextcolors (aref thetextcolors cur-Col cur-Row)) 
                          (with-rgb (rgb (sk8::mcl-color (aref thetextcolors cur-Col cur-Row)))
                            (#_RGBForeColor rgb))
                          (with-rgb (rgb default-text-color)
                            (#_RGBForeColor rgb)))
                        (rset qd-rect :rect.right (+ (rref qd-rect :rect.right) 3))
                        (draw-text-not-wrapped theimage qd-rect)
                        ))
                    (progn
                      (rset qd-rect :rect.left (+ left hLineSize))
                      (rset qd-rect :rect.top (+ top vLineSize))
                      (rset qd-rect :rect.right (- right 0))
                      (rset qd-rect :rect.bottom (- bottom 0))
                      (when (gs:rects-intersect DirtyRect qd-rect)
                        (#_RectRgn box-rgn qd-rect)
                        (#_SectRgn clip-rgn box-rgn box-rgn)
                        (#_SetClip box-rgn)
                        (render theImage theTablePicker box-rgn thePort)
                        (with-rgb (rgb (sk8::mcl-color (textColor theTablePicker)))
                          (#_RGBForeColor rgb))
                        (with-rgb (rgb (sk8::mcl-color (gs:node-fillcolor theTablePicker)))
                          (#_RGBBackColor rgb))
                        (#_PenNormal)))))
                (when (aref theSelection cur-Col cur-Row)
                  (rset qd-rect :rect.right right)
                  (rset qd-rect :rect.bottom bottom)
                  (rset qd-rect :rect.left left)
                  (rset qd-rect :rect.top top)
                  (#_RectRgn box-rgn qd-rect)
                  (#_SectRgn clip-rgn box-rgn box-rgn)
                  (#_SetClip draw-rgn)
                  (when HighlightSelection
                    (Set-Pen-For-Hilite)
                    (#_PaintRect qd-rect)
                    (#_PenMode #$srcCopy)
                    (#_PenSize 2 2))
                  (unless (and (eql 0 hLineSize) (eql 0 vLineSize))
                    (gs:let+ ((temp1 (:region))
                              (temp2 (:region))
                              )
                      (rset qd-rect :rect.right (+ right hlinesize))
                      (rset qd-rect :rect.bottom (+ bottom vlinesize ))
                      (#_rectRgn temp1 qd-rect)
                      (#_SetClip clip-rgn)
                      (#_copyRgn temp1 temp2)
                      (#_insetRgn temp2 selBorder selBorder)
                      (#_diffRgn temp1 temp2 temp1)
                      (render (Highlightcolor theTablePicker) theTablePicker temp1 thePort)
                      (with-rgb (rgb default-text-color)
                        (#_RGBForeColor rgb))
                      (with-rgb (rgb (sk8::mcl-color (gs:node-fillcolor theTablePicker)))
                        (#_RGBBackColor rgb))
                      (#_PenNormal)
                      ))
                  )))))
        (#_PenNormal))
      ;; Draw the contents if any!
      (unless (gs:dlist-empty? (gs:node-contents theTablePicker))
        (#_sectRgn fill-rgn draw-rgn clip-rgn)
        (gs:with-clipped-region clip-rgn
          (funcall (gs:node-contentsDrawFunction theTablePicker) theTablePicker thePort clip-rgn fill-rgn t)))
      ;; invert if necessary.
      (when (and (gs:hilited? flags) (gs:inverted? flags))
        (#_InvertRect fill-rect)))))

;;; Setting the TablePicker draw function.

(setf (gs:node-drawFunction TablePicker) 'fastTablePicker-draw)

(define-handler pointOnWhichPart (TablePicker h v &key part tracking)
  (declare (ignore tracking))
  (case part
    ((item index indices)
     (gs:with-temporary-port
       (let ((rowHeights (rowHeights me))
             (columnWidths (columnWidths me))
             (columnspacing (columnspacing me))
             (rowspacing (rowspacing me))
             (hScroll (max 0 (round (horizontalScroll me))))
             (vScroll (max 0 (round (verticalScroll me))))
             hPosition vPosition curcount cursum
             left top bottom right)
         (gs:recompute-fill-region me (gs:node-flags me))
         (let ((fillRect (gs:hrref (gs:node-fillRegion me) :region.rgnbbox :storage :pointer)))
           (setq left (rref fillRect :rect.left))
           (setq top (rref fillRect :rect.top))
           (setq right (rref fillRect :rect.right))
           (setq bottom (rref fillRect :rect.bottom)))
         ;; Change the points to window coords.
         (sk8-multival-bind (wh wv) (gs:stage-to-window-coords me h v)
           (sk8-multival-bind (origin-h origin-v) (origin me)
             (incf wh origin-h)
             (incf wv origin-v))
           (if (zerop hscroll) (incf left (TableHOffset me)))
           (if (zerop vscroll) (incf top (TableVOffset me)))
           (setq hPosition
                 (cond
                  ((> wh right)
                   (+ (ceiling (- wh right) (textsize me)) hscroll (width me :how 'partialitems))
                   )
                  ((< wh left)
                   (- hScroll (ceiling (- left  wh) (textsize me)) ))
                  (t
                   (setq wh (- (gs:f.round wh) left))
                   (setq curcount 0 cursum 0)
                   (+ (or (dolist (i (nthcdr hscroll columnwidths))
                            (incf cursum i)
                            (incf cursum columnspacing)                         
                            (incf curcount)
                            (if (>= cursum wh) (return curcount)))
                          curcount)
                      hScroll))))
           (setq vPosition
                 (cond
                  ((> wv bottom)
                   (+ (ceiling (- wv bottom) (textsize me)) vscroll (height me :how 'partialitems)))
                  ((< wv top)
                   (- vScroll (ceiling (- top  wv) (textsize me)) ))
                  (t
                   (setq wv (- (gs:f.round wv) top))
                   (setq curcount 0 cursum 0)
                   (+ (or (dolist (i (nthcdr vscroll rowheights))
                            (incf cursum i)
                            (incf cursum rowspacing) 
                            (incf curcount)
                            (if (>= cursum wv) (return curcount)))
                          curcount)
                      vScroll))))
           (setf hposition (gs:range 1 hposition (columns me)))
           (setf vPosition (gs:range 1 vPosition (rows me)))
           (if (eq part 'item)
             (sk8-tablepicker-aref (items me) hPosition vPosition)
             (list hPosition vPosition))))))
    (otherwise
     (return-from pointOnWhichPart (call-next-method me h v :part part)))))

(define-handler keyDown (TablePicker theChar)
  (let ((last-sel (lastSelection me)))
    (if (tuple-p last-sel)
      (progn
        (case theChar
          (#\Home
           (setf last-sel '(0 0)))
          (#\End
           (setf last-sel (dimensions me)))
          (#\PageUp
           (decf (second last-sel) (max 1 (height me :how 'items))))
          (#\Page
           (incf (second last-sel) (max 1 (height me :how 'items))))
          (#\backArrow
           (decf (first last-sel)))
          (#\upArrow
           (decf (second last-sel)))
          (#\forwardArrow
           (incf (first last-sel)))
          (#\downArrow
           (incf (second last-sel)))
          (otherwise (call-next-method)))
        (setf (selection me :deselecting (not (xor (shift-key-p) (caps-lock-key-p)))) last-sel)
        )
      (call-next-method))))

(define-handler keyup (TablePicker thechar)
  (declare (ignore thechar))
  (call-next-method)
  (selectioncompleted me))

(define-handler ExtendedMouseDown (tablePicker) 
  nil)

(define-handler mouseDown (TablePicker)
  (sk8-multival-bind (prev-h prev-v) (pointOnWhichPart me (eventH) (eventV) :part 'index)
    (let* ((initial-item (list prev-h prev-v))
           (*event-x* (eventH))
           (*event-y* (eventV))
           (sel-item (list nil nil))
           cur-h cur-v start-h start-v
           (style (selectionStyle me))
           (multi (eq style 'multiple))
           (deselecting (not (and (xor (caps-lock-key-p) (shift-key-p)) multi)))
           (moving nil) ;;;; (option-key-p))  should get rid of this stuff
           (orig (when (not deselecting) (selection me)))
           value
           (theLeft (left me :physical t))
           (theTop (top me :physical t))
           (theRight (right me :physical t))
           (theBottom (bottom me :physical t))
           (ExtendedMousedown t))
      (call-next-method)
      (setf (keytarget (sk8::window me)) me)
      (if (null initial-item)
        (error "Unable to find initial-item in TablePicker's mouseDown"))
      (setq value (not (and (not deselecting) (tuple-selected me initial-item))))
      (unless moving
        (setf (selection me :deselecting deselecting :off (not value)) initial-item))
      
      (dotimes (i 8)
        (sk8-multival-bind (x y) (mouseloc stage)
          (sk8::wait-time-period second 0.05 :events nil)
          (when (or (not (down mouse))
                    (not (equal (pointOnWhichPart me x y :part 'index) initial-item))) 
            (setf ExtendedMousedown nil)
            (return))))
      (when (and extendedMouseDown (aref (items me) (1- prev-h) (1- prev-v)))
        (setf  ExtendedMousedown 
               (extendedMousedown me))) ;;; continue if extended mousedown returns nil
      (unless extendedmousedown  ;;; 
        (loop
          (unless (down mouse) (return))
          (sk8-multival-setf (start-h start-v)
                             (pointOnWhichPart me *event-x* *event-y* :part 'index :tracking t))
          (sk8-multival-setf (*event-x*  *event-y*) (mouseloc stage))
          (sk8-multival-setf (cur-h cur-v)
                             (pointOnWhichPart me *event-x* *event-y* :part 'index :tracking t))
          (if moving
            (let ((h-offset (- prev-h cur-h))
                  (v-offset (- prev-v cur-v)))
              (setf (horizontalScroll me :relative t) h-offset)
              (incf cur-h h-offset)
              (setf (verticalScroll me :relative t) v-offset)
              (incf cur-v v-offset))
            ;; Only call this again if the current item has changed!
            (when (or (/= cur-h start-h) (/= cur-v start-v)
                      (> theLeft *event-x*) (< theRight *event-x*)
                      (> theTop *event-y*) (< theBottom *event-y*))
              (setf (first sel-item) cur-h)
              (setf (second sel-item) cur-v)
              (cond
               ((not deselecting)
                (setf (cursor stage) CursorCrossHair)
                (setf (selection me :deselecting t :selectionShown nil) orig)
                (setf (selection me :deselecting nil :off (not value) :start initial-item :end sel-item :selectionShown nil) nil))
               (t
                (setf (selection me :deselecting t :selectionShown nil) sel-item)))
              (setf (lastSelection me) sel-item)
              (showSelection me :curItem sel-item)))
          (setq prev-h cur-h)
          (setq prev-v cur-v)))
      
      (setf (cursor stage) StandardCursor)
      (selectioncompleted me))))


;;; ________________________________
;;; MouseUp: making tablepicker doubleClick work!
;;; ________________________________
;;;; NOTE!!!
;;;; THIS CODE WAS STOLEN FROM PICKER.LISP.
;;;; UPDATES HERE SHOULD BE MADE THERE AND VICE VERSA!


(define-handler mouseUp (TablePicker)
  ;; Process clicks!
  (when (or gs:*force-click*
            (and (eq (eventActor) gs:*last-mousedown-object*)
                 (< (- (eventTime) gs:*last-mousedown-time*)
                    (#_GetDblTime))
                 (double-click-spacing-p gs:*event-location*
                                         gs:*last-mousedown-location*)))
    (setq gs:*force-click* nil)
    ;; it was a click, for sure
    (when (eq me (eventActor)) (dispatch-click me nil #'(lambda (p)
                                                        (when (eq (car (selectedItems p)) (lastSelected p))
                                                          (doubleClick p)))))
    ;; Propagate up the containment hierarchy!
    (let ((theContainer (container me)))
      (when theContainer
        (mouseup theContainer))))
  ;; Remember the old selection!
  (setf (lastSelected me) (car (selectedItems me))))

;;;----------------------------------------------------------------------------------------------------------------
;;;  Utility Handlers for Users...
;;;----------------------------------------------------------------------------------------------------------------

(define-handler ItemVisible (TablePicker tuple &key (how 'items)) 
  (let* (rr 
         cc 
         (hscroll (round (horizontalscroll me)))
         (vscroll (round (verticalscroll me)))
         (VisCols (width me :how how)) 
         (VisRows (height me :how how))
         )
    (if (listp tuple)
      (sk8-multival-setf (cc rr) tuple)
      (sk8-multival-setf (cc rr) (find-indices me tuple)))
    (when (or (<= rr 0) (<= cc 0) (> rr (rows me)) (> cc (columns me)))
      (sk8-error GeneralProgrammaticError
                               :strings '("Specified indices are not in range.")))
    (and (> cc hscroll) (> rr vscroll) (<= cc (+ hscroll VisCols)) (<= rr (+ vscroll VisRows)))))

(define-handler ItemBoundsRect (TablePicker tuple &key (physical nil))
  (let* (rr cc
            (rs (rowspacing me))
            (cs (columnspacing me))
            (curH (TableHoffset me))
            (curV (TableVoffset me))
            (ColCount (round (horizontalscroll me))) 
            (RowCount (round (verticalscroll me)))
            (cw (columnWidths me))
            (rh (rowHeights me))
            lll ttt rrr bbb
            )
    (if (tuple-p tuple)
      (sk8-multival-setf (cc rr) tuple)
      (progn
        (sk8-multival-setf (cc rr) (find-indices me tuple))
        (incf rr)
        (incf cc)
        ))
    (if (ItemVisible me (list cc rr) :how 'partialitems)
      (progn
        (decf cc)
        (decf rr)
        (setf lll (loop
                    (if (= cc ColCount) (return curH))
                    (incf curH (nth ColCount cw))
                    (incf curH cs)
                    (incf ColCount)
                    ))
        (setf ttt (loop
                    (if (= rr RowCount) (return curV))
                    (incf curV (nth RowCount rh))
                    (incf curV rs)
                    (incf RowCount)
                    ))
        (incf lll (columnlinessize me))
        (incf ttt (rowlinessize me))
        (setf rrr (+ lll cs (nth ColCount cw)))
        (setf bbb (+ ttt rs (nth RowCount rh)))
        (when physical
          (sk8-multival-bind (loff voff rr bb) (fillboundsrect me :physical t)
            (declare (ignore rr bb))
            (incf rrr loff)
            (incf lll loff)
            (incf ttt voff)
            (incf bbb voff)))
        (sk8-multivals lll ttt rrr bbb))
      (sk8-multivals 0 0 0 0))))

;;;----------------------------------------------------------------------------------------------------------------
;;;Scroller stuff.  
;;;----------------------------------------------------------------------------------------------------------------

(define-handler update-horizontal-partnerScroller (TablePicker)
  (let ((theScroller (partnerHScroller me)))
    (when theScroller
      (let ((visCols (width me :how 'partialitems)))
        (setScrollerVals
         theScroller
         :minVal 0 
         :maxVal (- (columns me) (width me :how 'items))
         :curVal (max 0 (horizontalScroll me))
         :scrollStep 1 
         :pageStep visCols
         :thumbview visCols)))))

(define-handler update-vertical-partnerScroller (TablePicker)
  (let ((theScroller (partnerVScroller me)))
    (when theScroller
      (let* ((numrows (rows me))
            (visRows (min numrows (height me :how 'items))))
        (setScrollerVals theScroller
                         :minVal 0
                         :maxVal (- numrows visRows)
                         :curval (max 0 (verticalscroll me))
                         :scrollStep 1
                         :pageStep visRows
                         :thumbview visRows)))))


(define-handler updatePartnerScroller (TablePicker thescroller)
  (if (eq thescroller (partnerVScroller me))
    (update-vertical-partnerScroller me)
    (update-horizontal-partnerScroller me)))



;;; Scrolling stuff
(define-handler partnerScroller (TablePicker orientation)
  (if (eq orientation 'vertical)
    (partnerHScroller me)
    (partnerVScroller me)))

(define-handler (setf partnerScroller) (theScroller TablePicker &key (orientation 'vertical))
  (let ((myScrollerProperty (if (eq orientation 'vertical)  'partnerVScroller 'partnerHScroller))
        (wiredProperty      (if (eq orientation 'vertical)  'verticalScroll   'horizontalScroll)))
    (set-partnerScroller me wiredProperty myScrollerProperty theScroller)))

(define-handler (setf partnerHScroller) (theScroller TablePicker)
  (setf (partnerScroller me :orientation 'horizontal) theScroller))

(define-handler (setf partnerVScroller) (theScroller TablePicker)
  (setf (partnerScroller me :orientation 'vertical) theScroller))

(define-handler resized (TablePicker)
  (when (partnerHScroller me)
    (update-horizontal-partnerScroller me))
  (when (partnerVScroller me)
    (update-vertical-partnerScroller me)))

(define-handler localVirtualProperties (TablePicker)
  (when (eq me TablePicker)
    '(EditorIncluded Dimensions Columns Rows SelectByRows)))

;;; _______________________________ 
;;; From Browser Components...
;;; _______________________________ 

(define-handler FlashLine (tablePicker &key (Orientation 'Vertical))
  (let ((theguy (GetLinePosition me)))
    (when theguy
      (if (eq Orientation 'vertical)
        (sk8-multival-bind (ll tt rr bb) (itemboundsrect me theguy :physical t)
          (setf ll (round ll)
                tt (round tt)
                rr (round rr)
                bb (round bb))
          (drawxorline ll tt rr tt)
          (sleep 0.01)
          (drawxorline ll tt rr tt))
        (sk8-multival-bind (ll tt rr bb) (itemboundsrect me theguy :physical t)
          (setf ll (round ll)
                tt (round tt)
                rr (round rr)
                bb (round bb))
          (decf ll 3)
          (decf bb 1)
          (drawxorline ll tt ll bb)
          (sleep 0.01)
          (drawxorline ll tt ll bb))))))

(define-handler FlashItem (tablePicker)  ;;;Note: specialized in twobynpicker below...
  (when (items me)
    (let ((theguy (GetItemPosition me)))
      (sk8-multival-bind (ll tt rr bb) (itemboundsrect me theguy :physical t)
        (setf ll (- (round ll) 3)
              tt (- (round tt) 3)
              rr (+ (round rr) 3)
              bb (+ (round bb) 3))
        (DrawXORFrame ll tt rr bb :pensize '(3 3))
        (sleep 0.01)
        (DrawXORFrame ll tt rr bb :pensize '(3 3))
        ))))

(define-handler GetLinePosition (tablePicker)
  (if (items me)
    (sk8-multival-bind (hh vv) (mouseloc stage)
      (let ((theguy (pointonwhichpart me hh vv :part 'index)))
        (if theguy
          (progn
            (setf (first theguy) (gs:range 1 (first theguy) (columns me)))
            (setf (second theguy) (gs:range 1 (second theguy) (rows me))))
          (setf theguy (list 1 (rows me))))
        theguy))
    '(1 1)))

(define-handler GetItemPosition (tablePicker)
  (when (items me)
    (sk8-multival-bind (hh vv) (mouseloc stage)
      (let ((theguy (pointonwhichpart me hh vv :part 'index)))
        (when theguy
          (setf (first theguy) (gs:range 1 (first theguy) (columns me)))
          (setf (second theguy) (gs:range 1 (second theguy) (rows me))))
        (when (not theguy)
          (setf theguy (list 1 (1+ (rows me)))))
        theguy))
    ))
  
#|
	Change History (most recent last):
	1	9/20/93	rod	
	3	9/22/93	rod	Fixed bug in setf items which caused the system browser to crash.
	4	9/22/93	rod	
	5	10/1/93	rod	Removed Node References and Added With-fast-slots.
	6	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	7	10/5/93	hernan	Changed making-picker-dirty to evaluate the
				incoming form before calling making-self-dirty.
	8	10/11/93	rod	fixed scrolling bounds
	9	10/22/93	kleiman	locally declared 'handler' not special or constant for the mapState[inReverse]/mapCollection[inReverse] handlers
	16	11/5/93	nil	added two properties columnLinesSize & rowLinesSize, along with setters, and modified the draw function to use those values
	20	11/17/93	kleiman	
	21	11/19/93	rod	added setvalues
	22	11/19/93	kleiman	Made some properties noninheritable
	23	11/19/93	kleiman	
	30	11/23/93	rod	Restore the system font, hopefully this will clear
				up the problem of bashing the system font.
	31	11/24/93	hernan	Now using setScrollerVals to update the scrollers.
	35	11/29/93	hernan	Using with-temporary-ports wherever possible
				to avoid direct references to specific ports.
	38	12/10/93	hernan	WaitTimePeriod is now called without events.
	39	12/21/93	sidney	Changes so files can be compiled
	40	12/22/93	rod	Cleaned up the algorithm of the color picker.  It 
				now uses sqrt rather than isqrt because the result
				is nicer.  This sacrifices speed a little, but only
				about 0.1 seconds per call...
	41	1/10/94	hernan	Fonts just became objects!!!
	43	1/14/94	hernan	Now using ports to talk to scrollers!
	45	1/18/94	rod	Changed draw function to use draw-text-not-wrapped
				now that that function knows how to add a "...
	46	1/18/94	rod	Whoops!!  Now it will work there was a version
				problem.
	47	1/31/94	rod	Make sure to round the vscroll and hscroll since
				the scroller can make them non integers...
	48	2/12/94	kleiman	renaming
	49	2/16/94	rod	Fixed the draw method to highlight more nicely
	50	2/16/94	rod	Now it makes a lot more arrays, but tough, it's
				more robust.  Also fixed drawing of highlight
				border.
	51	2/18/94	rod	Fixed part of the draw method.
	52	2/18/94	sidney	it looks like ss:!waitimeperiod's name is now ss::wait-time-period
	53	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	54	2/21/94	hernan	window -> sk8::window.
	55	2/25/94	hernan	Using symbols instead of keywords for options!!!
	56	2/26/94	rod	
	57	2/26/94	kleiman	
	58	3/3/94	Hernan	The great handler argument name renaming of 94!
	60	3/6/94	rod	old-selection -> lastSelected
	71	4/20/94	rod	Fixing selection of colorpicker
	74	5/4/94	rod	Making tablepickerscroller NOT be a prototype
	76	6/14/94	rod	Fixing bug in the computation of a tablepicker
				scroller's maximum value.
	77	6/16/94	till	portType keywords begone.
	78	6/16/94	rod	Fixing initialization bug introduced by changing
				a keyword to a symbol.
	79	6/23/94	rod	Getting rid of tablePickerScroller.  Adding Page Up,
				Page Down, etc.
	80	6/23/94	rod	Fixing initialize method to be nicer.
	81	6/29/94	rod	
	82	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	83	8/4/94	rod	Fixing that pesky n by 0 bug.
	84	8/4/94	sidney	Removing items thingy
	85	8/22/94	rod	Bug with rowheights and columnwidths
	86	8/22/94	rod	
	87 	 8/30/94	rod     	
	88 	 8/31/94	Hernan  	*scrolling* -> lockedScroller.
	89 	 8/31/94	Hernan  	Chicago -> ChicagoFont.
	90 	 9/ 2/94	rod     	
	91 	 9/12/94	rod     	
	92 	 9/12/94	rod     	Hopefully fixing that pesky array out of bounds
							stuff.
	93 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	94 	10/13/94	chip    	updated (setf partnerScroller) to use set-partnerScroller
	95 	10/18/94	chip    	added partnerscroller creationRelations
	96 	11/ 7/94	rod     	dealing with clearing of lastselected.
	97 	11/ 9/94	rod     	Fixing bug in set selection of colorpicker.
	98 	11/16/94	rod     	Adding error checking to setupimages to make
							sure createTextDisplayItem returns a string
							or a renderer.  Great way to slow this thing down.
	99 	11/16/94	rod     	Fixing setf element as well.
	100	11/30/94	rod     	Fixing set element so it sets up text colors and 
							styles.
	101	12/ 3/94	rod     	error message fixes.
	102	 1/ 9/95	rod     	Fixing extendedmousedown to be called only if 
							on an item..
	103	 1/16/95	rod     	Making these now draw their contents properly.
	104	 2/ 2/95	rod     	Making align-selection to center work nicely.
	105	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	106	 2/16/95	sidney  	readable argument names for initialize handler
	107	 2/20/95	dy      	remove xor defun.  it's now defined in build part 1
	108	 3/ 7/95	rod     	
	109	 3/14/95	rod     	
	110	 3/16/95	rod     	
	111	 3/17/95	rod     	
	2  	 6/23/95	Hernan  	Fixed update-horizontal-partnerScroller to include the first arg.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 4/19/96	Brian   	changing package of wait-time-period
	7  	 2/27/97	Hernan  	Removing selectedItemBoundsRect which is already defined in Picker.lisp
						6  11/15/96Hernan  Initialize cannot assume that the "original" arg will have an
						object of the right type. Calling originalAncestor to deal with it.
|# ;(do not edit past this line!!)
