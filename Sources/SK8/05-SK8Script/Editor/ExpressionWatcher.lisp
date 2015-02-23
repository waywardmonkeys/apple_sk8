;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new Rectangle :objectName "RuntimeExpressionWatcher" :project SK8
     :properties '((dirty :value nil)
                   (hsplitterLocation :value 0.33s0)
                   (vsplitterLocation :value 0.75s0)))

(setf (private RuntimeExpressionWatcher) t)  ; not public api

(defun exprWatcher-associated-handlerEditText (exprWatcher)
  (let ((scriptWind (container exprWatcher)))
    (when scriptWind (editor scriptWind))))

(setBoundsRect RuntimeExpressionWatcher 0 0 100 70)
(define-handler minimumSize (RuntimeExpressionWatcher)
  '(100 70))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new TablePicker :objectName "ExpressionWatcherPicker" :project SK8)

(setf (private ExpressionWatcherPicker) t)  ; not public api

(setf (container ExpressionWatcherPicker) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherPicker 'picker)

(setf (selectionStyle ExpressionWatcherPicker) 'single)
(setf (columns ExpressionWatcherPicker) 2)
(setf (rowSpacing ExpressionWatcherPicker) 0)
(setf (minWidth ExpressionWatcherPicker) 35)
(setf (columnsRigid ExpressionWatcherPicker) t)
(setf (rowsRigid ExpressionWatcherPicker) t)
(setf (wantsMouseWithin ExpressionWatcherPicker) t)
(setf (textSize ExpressionWatcherPicker) 9)
(setf (textFont ExpressionWatcherPicker) "Geneva")
(setf (columnLinesSize ExpressionWatcherPicker) 2)
(setf (selectionBorder ExpressionWatcherPicker) 0)
(setf (horizontalScroll ExpressionWatcherPicker) 0)

(define-handler minimumSize (ExpressionWatcherPicker)
  '(75 25))

(define-handler initialize (ExpressionWatcherPicker original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (setf (rows me) 0) ; to ensure we've got the arrays all set up!
  (insertRow me :item nil))

(define-handler (setf horizontalScroll) (scroll ExpressionWatcherPicker)
  (declare (ignore scroll)))

;;; Since the items always have the same form, we know that the first thing in it
;;; is the string representing it. Nothing to do. 

(defun compute-runtime-expression-string (theitem proj)
  (declare (ignore proj))
  (car theItem))

;;; expr is an item in the picker. The form is {<expressionString>, <expressionForm>}
;;; where the expression form is the right thing produced by registerExpressionForRuntimeUse.

(defun compute-runtime-expression-value-and-string (handlerId expr)
  (if expr
    (let ((val (evaluateExpressionInLocalContext handlerId (second expr))))
      (sk8-multivals (simpleObjectString (car expr) :project (car handlerId)) ;; expr string.
                     val                                                      ;; value of expression.
                     (objectString val :project (car handlerId))))             ;; value string.
    (sk8-multivals "" nil "")))
  
;;; Given a list of expression-info items, insert them into the watcher and then recompute & redisplay the values of all of them

(defun setup-expressionWatcher-items (exprWatcher localVariablesUsed items &key (update t))
  (declare (ignore localVariablesUsed))
  (let* ((proj (targetProject (expressionField exprWatcher)))
         (exprWatcherPicker (picker exprWatcher))
         (numItems (1+ (length items)))
         (itemsArray (getvalue 'items exprWatcherPicker))
         (imagesArray (imageArray exprWatcherPicker))
         (selectionArray (selectionArray exprWatcherPicker))
         (dimensions (list 2 numItems))
         (handlerId (inputHandlerId (editor (sk8::window exprWatcher))))
         (i 0)
         exprString exprVal exprValString)
    (declare (dynamic-extent dimensions))
    (adjust-array itemsArray dimensions :initial-element nil)
    (adjust-array imagesArray dimensions :initial-element nil)
    (adjust-array selectionArray dimensions :initial-element nil)
    (if update
      ;; Compute the values too
      (dolist (expr items)
        (SK8-multival-setf (exprString exprVal exprValString)
                           (compute-runtime-expression-value-and-string handlerId expr))
        (setf (aref itemsArray 0 i) expr
              (aref imagesArray 0 i) exprString
              (aref itemsArray 1 i) exprVal
              (aref imagesArray 1 i) exprValString)
        (incf i))
      ;; Don't compute the values
      (dolist (expr items)
        (setf (aref itemsArray 0 i) expr
              (aref imagesArray 0 i) (compute-runtime-expression-string expr proj)
              (aref itemsArray 1 i) nil
              (aref imagesArray 1 i) "")
        (incf i)))
    ;; Initialize the empty row
    (setf (aref itemsArray 0 i) nil
          (aref imagesArray 0 i) ""
          (aref itemsArray 1 i) nil
          (aref imagesArray 1 i) "")
    (sk8::setValue 'rowHeights exprWatcherPicker (make-list numItems :initial-element 12))
    (forceRedraw exprWatcherPicker)
    (setf (dirty exprWatcher) nil)))

;;; Recompute & redisplay the values of all the expressions in the watcher
;;;
(define-handler update (RuntimeExpressionWatcher &key localsChanged)
  (declare (ignore localsChanged))
  (let* ((exprWatcherPicker (picker me))
         (itemsArray (getvalue 'items exprWatcherPicker))
         (imagesArray (imageArray exprWatcherPicker))
         (handlerId (inputHandlerId (editor (sk8::window me))))
         expr exprVal exprValString)
    (dotimes (i (array-dimension itemsArray 1))
      (setq expr (aref itemsArray 0 i))
      (when expr
        (SK8-multival-setf (expr exprVal exprValString)
                           (compute-runtime-expression-value-and-string handlerId expr))
        (setf (aref itemsArray 1 i) exprVal
              (aref imagesArray 1 i) exprValString)))
    (forceRedraw exprWatcherPicker)))

(define-handler itemsList (ExpressionWatcherPicker)
  (let* ((itemsArray (getvalue 'items me))
         (i (max 0 (1- (array-dimension itemsArray 1))))
         (items nil))
    (loop
      (when (eql 0 i) (return items))
      (push (aref itemsArray 0 (decf i)) items))))

(define-handler selectedText (ExpressionWatcherPicker)
  ;*** COULD BE IMPLEMENTED MUCH MORE EFFICIENTLY!!!
  (text me :item (first (selection me))))

(define-handler selectedRow (ExpressionWatcherPicker)
  ;*** COULD BE IMPLEMENTED MUCH MORE EFFICIENTLY!!!
  (second (first (selection me))))

(define-handler (setf selectedRow) (rowNumber ExpressionWatcherPicker)
  ;*** COULD BE IMPLEMENTED MUCH MORE EFFICIENTLY!!!
  (with-recoverable-list
    (setf (selection me) (list 1 rowNumber)))
  (selectionCompleted me) ; #####*** WHY DOES THIS HAVE TO BE EXPLICITLY CALLED?
  rowNumber)


(define-handler emptyRow (ExpressionWatcherPicker rowNum)
  (eql 0 (length (aref (imageArray me) 0 (1- rowNum)))))

(define-handler emptyRowSelected (ExpressionWatcherPicker)
  (let ((rowNum (selectedRow me)))
    (when rowNum
      (eql 0 (length (aref (imageArray me) 0 (1- rowNum)))))))

(define-handler (setf item) (exprInfo ExpressionWatcherPicker &key rowNumber)
  (decf rowNumber)
  (let* ((exprWatcher (container me))
         (itemsArray (getvalue 'items me))
         (imagesArray (imageArray me))
         (handlerId (inputHandlerId (editor (sk8::window me)))))
    (SK8-multival-bind (exprString exprVal exprValString)
                       (compute-runtime-expression-value-and-string handlerId exprInfo)
      (setf (aref itemsArray 0 rowNumber) exprInfo
            (aref imagesArray 0 rowNumber) exprString
            (aref itemsArray 1 rowNumber) exprVal
            (aref imagesArray 1 rowNumber) exprValString))
    (gs:with-queued-drawing `(forceRedraw ,me))
    (setf (dirty exprWatcher) t)
    exprInfo))

(define-handler insertRow (ExpressionWatcherPicker &key item following)
  (unless following (setq following -1))
  (let* ((itemsArray (getvalue 'items me))
         (imagesArray (imageArray me))
         (selectionArray (selectionArray me))
         (numItems (array-dimension itemsArray 1))
         (dimensions (list 2 (1+ numItems)))
         (i numItems)
         (prev (1- numItems)))
    (declare (dynamic-extent dimensions))
    (unless (<= -1 following (1- numItems)) (error "There is no row number ~a" following))
    (adjust-array itemsArray dimensions :initial-element nil)
    (adjust-array imagesArray dimensions :initial-element nil)
    (adjust-array selectionArray dimensions :initial-element nil)
    (loop
      (when (eql prev following) (return))
      (setf (aref itemsArray 0 i) (aref itemsArray 0 prev)
            (aref itemsArray 1 i) (aref itemsArray 1 prev)
            (aref imagesArray 0 i) (aref imagesArray 0 prev)
            (aref imagesArray 1 i) (aref imagesArray 1 prev)
            (aref selectionArray 0 i) (aref selectionArray 0 prev)
            (aref selectionArray 1 i) (aref selectionArray 1 prev))
      (decf i) (decf prev))
    (setf (aref selectionArray 0 i) nil
          (aref selectionArray 1 i) nil)
    (sk8::setValue 'rowHeights me (cons 12 (rowHeights me)))
    (withActorLocked (me)
      (if item
        (setf (item me :rowNumber (1+ i)) item)
        (forceRedraw me))
      (let ((vscroller (partnerVScroller me)))
        (when vscroller (updatePartnerScroller me vscroller))))))

(define-handler removeRow (ExpressionWatcherPicker &optional rowNumber)
  (unless rowNumber (setq rowNumber (selectedRow me)))
  (when rowNumber
    (decf rowNumber)
    (let* ((itemsArray (getvalue 'items me))
           (imagesArray (imageArray me))
           (selectionArray (selectionArray me))
           (numItems-1 (1- (array-dimension itemsArray 1)))
           (dimensions (list 2 numItems-1))
           (i rowNumber)
           (next (1+ i)))
      (declare (dynamic-extent dimensions))
      (unless (<= 0 rowNumber numItems-1) (error "There is no row number ~a" (1+ rowNumber)))
      (loop
        (when (eql i numItems-1) (return))
        (setf (aref itemsArray 0 i) (aref itemsArray 0 next)
              (aref itemsArray 1 i) (aref itemsArray 1 next)
              (aref imagesArray 0 i) (aref imagesArray 0 next)
              (aref imagesArray 1 i) (aref imagesArray 1 next)
              (aref selectionArray 0 i) (aref selectionArray 0 next)
              (aref selectionArray 1 i) (aref selectionArray 1 next))
        (incf i) (incf next))
      (adjust-array itemsArray dimensions :initial-element nil)
      (adjust-array imagesArray dimensions :initial-element nil)
      (adjust-array selectionArray dimensions :initial-element nil)
      (sk8::setValue 'rowHeights me (cdr (rowHeights me)))
      (withActorLocked (me)
        (unless (selectedRow me) (setf (selectedRow me) rowNumber))
        (forceRedraw me)
        (let ((vscroller (partnerVScroller me)))
          (when vscroller (updatePartnerScroller me vscroller))))
      (setf (dirty (container me)) t))))

(define-handler selectionCompleted (ExpressionWatcherPicker)
  (declare (special *mouseDown-in-exprWatcher-picker* *dragging-from-exprWatcher-picker*))
  (call-next-method)
  (unless (and (eq *mouseDown-in-exprWatcher-picker* me)
               (eq *dragging-from-exprWatcher-picker* me))
    (let* ((itemIndices (first (selection me)))
           (colNum (1- (pop itemIndices)))
           (rowNum (1- (first itemIndices)))
           (imageArray (imageArray me))
           (theText "")
           (theField (expressionField (container me))))
      (when itemIndices
        (when (and (eql 1 colNum) (emptyRowSelected me))
          ;; The "value" field of the empty row has been selected; select the "expression"
          (setf (selectedRow me) (1+ rowNum))
          (return-from selectionCompleted))
        (setq theText (aref imageArray colNum rowNum)))
      (withActorLocked (theField)
        (setf (text theField) theText)
        (setSelection theField 0 t)))))

(defun mouseEvent-in-watcher-divider? (me)
  (gs:with-temporary-port
    (sk8-multival-bind (h v) (gs:stage-to-window-coords me (eventH) (eventV))
      (declare (ignore v))
      (gs:recompute-fill-region me (gs:node-flags me))
      (decf h (rref (gs:node-fillRegion me) :region.rgnBBox.left))
      (sk8-multival-bind (origin-h origin-v) (origin me)
        (declare (ignore origin-v))
        (incf h (round origin-h)))
      (decf h (round (columnSpacing me)))
      (let ((dividerPos (first (columnWidths me)))
            (dividerWidth (columnLinesSize me)))
        (<= (- dividerPos 2) h (+ dividerPos dividerWidth 1))))))

(define-handler mouseWithin (ExpressionWatcherPicker)
  (if (mouseEvent-in-watcher-divider? me)
    (unless (eq (cursor Stage) CursorCrosshair) (setf (cursor Stage) CursorVSplitter))
    (unless (eq (cursor Stage) StandardCursor) (setf (cursor Stage) StandardCursor))))

(defvar *mouseDown-in-exprWatcher-picker* nil)
(defvar *dragging-from-exprWatcher-picker* nil)
(define-handler mouseDown (ExpressionWatcherPicker)
  (cond
   ((mouseEvent-in-watcher-divider? me)
    (mouseDown (hsplitter (container me))))
   (t
    (let ((*mouseDown-in-exprWatcher-picker* me)
          (*dragging-from-exprWatcher-picker* nil))
      (call-next-method)
      (setf (keytarget (sk8::window me)) (container me))))))

(defun best-objectString-for-handlerCall (proj theHandler possibleObjects &key
                                                     targetObject targetEditor property setter)
  (let* ((handlerName (if (symbolp theHandler) theHandler (name theHandler)))
         handlerNameString
         setterForm?)
    (when (listp handlerName)
      (setf setterForm? t)
      (setf handlerName (cadr handlerName))
      )
    (setf handlerNameString (pretty-symbol-name handlerName))
    (unless targetObject
      (when targetEditor
        (let ((handlerObj (third (inputHandlerID targetEditor))))
          (when handlerObj
            (setq targetObject (find handlerObj possibleObjects :test #'(lambda (hObj otherObj)
                                                                          (or (eq otherObj hObj)
                                                                              (SK8::is-a otherObj hObj)))))))))
    (let* ((targetHandler (or (and targetObject (mf::find-applicable-handler handlerName targetObject))
                              theHandler))
           (callArg (if (or targetObject (functionp targethandler)) t (objectString (first possibleObjects) :project proj)))
           (propertyForm? (and targetObject (or property setter setterform?)))
           (argString (handlerArgumentsString targetHandler :call callArg :setterArgument nil
                                              :primaryArgument (not propertyForm?)))
           (emptyArgString? (eql 0 (length argString))))        
      (if propertyForm?
        (concatenate 'string 
                     (if (and setterform? (not setter)) "set " "")
                     "my " handlerNameString (unless emptyArgString? " ") argString
                     (if (or setter setterform?) " to " ""))
        (concatenate 'string 
                     (if (and setterform? (not setter)) "set " "") 
                     handlerNameString " " argString
                     (if (or setter setterform?) " to " "")
                     )))))

;;; Drag & drop stuff...

(define-handler extendedMouseDown (ExpressionWatcherPicker)
  (unless (emptyRowSelected me)
    (let* ((selectionIndices (first (selection me)))
           (expressionColumn? (eql 1 (first selectionIndices)))
           (item (first (selectedItems me))))
      (if expressionColumn?
        (when (listp item) (setq item (first item)))
        (when (eq item (CCL::%UNBOUND-MARKER-8)) (setq item nil)))
      (setf (boundsRect ObjectDataRect :physical t) (itemBoundsRect me selectionIndices :physical t)
            (object ObjectDataRect) item
            (componentFrom ObjectDataRect) me)
      (setq *dragging-from-exprWatcher-picker* me)
      (withCursor StandardCursor
        (drag ObjectDataRect :live nil :onStage t :draggingMouseWithinEvents t :draggedOverEvents t :dropEvents t))
      (setf (componentFrom ObjectDataRect) nil)
      t)))

(define-handler findDropItem (ExpressionWatcherPicker)
  (let* ((targetIndices (getItemPosition me))
         (targetExpressionColumn? (eql 1 (first targetIndices)))
         (validPlace? (or targetExpressionColumn? (not (emptyRow me (second targetIndices))))))
    (when (AND validPlace? TARGETEXPRESSIONCOLUMN?) ; ##### DISALLOWING DROPS IN VALUE COLUMN FOR NOW!!!
      targetIndices)))

(define-handler draggingMouseWithin (ExpressionWatcherPicker actorDragged)
  (let ((targetIndices (findDropItem me)))
    (when targetIndices
      (let ((targetExpressionColumn? (eql 1 (first targetIndices)))
            (item (element me targetIndices)))
        (when targetExpressionColumn?
          (when (listp item) (setq item (first item))))
        (unless (and (eq actorDragged ObjectDataRect)
                     (eq (componentFrom ObjectDataRect) me)
                     (eq (object ObjectDataRect) item))
          (when (or (eq actorDragged ObjectDataRect)
                    (eq actorDragged Propertydatarect)
                    (eq actorDragged handlerdatarect))
            (flashItem me)))))))


(define-handler dropped (ExpressionWatcherPicker droppee)
  (draggingMouseLeave me droppee)
  (let ((targetIndices (findDropItem me)))
    (when targetIndices
      (let ((targetExpressionColumn? (eql 1 (first targetIndices)))
            (item (element me targetIndices))
            (exprField (expressionField (container me)))
            (associatedEditor (exprWatcher-associated-handlerEditText (container me))))
        (when targetExpressionColumn? (setq item (first item)))
        
        (cond
         ((eq droppee ObjectDataRect)
          (bringToFront (sk8::window me))
          (unless (eq item (setq item (object droppee)))
            (setf (selection me) targetIndices
                  (text exprField) (if (stringp item) item (objectString item)))
            (evaluate exprField)))
         
         ((eq droppee PropertyDataRect)
          (bringToFront (sk8::window me))
          (setf (selection me) targetIndices
                (text exprField) (best-objectString-for-handlerCall
                                  (targetProject exprField)
                                  (propertyName droppee)
                                  (objects droppee)
                                  :targetEditor associatedEditor
                                  :property t))
          (evaluate exprField))
         
         ((eq droppee HandlerDataRect)
          (bringToFront (sk8::window me))
          (setf (selection me) targetIndices
                (text exprField) (best-objectString-for-handlerCall
                                  (targetProject exprField)
                                  (handler droppee)
                                  (objects droppee)
                                  :targetEditor associatedEditor))
          (evaluate exprField)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new Rectangle :objectName "ExpressionWatcherHSplitter" :project SK8)

(setf (private ExpressionWatcherHSplitter) t)  ; not public api

(setf (container ExpressionWatcherHSplitter) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherHSplitter 'hsplitter)


(define-handler mouseDown (ExpressionWatcherHSplitter)
  (let* ((exprWatcherPicker (picker (container me)))
         (minColumnWidth (minWidth exprWatcherPicker)))
    (SK8-multival-bind (left top right bottom) (boundsRect exprWatcherPicker :physical t)
      (SK8-multival-bind (fWidth fHeight) (frameSize exprWatcherPicker :physical t)
        (incf left fWidth)
        (decf right fWidth)
        (incf top fHeight)
        (decf bottom fHeight))
      (with-recoverable-list
        (drag me :live nil :constrainingRect (list (+ left minColumnWidth)
                                                     top
                                                     (- right minColumnWidth)
                                                     (1- bottom)))))))

(define-handler moved (ExpressionWatcherHSplitter)
  (let* ((container (container me))
         (theExprTitle (exprTitleLabel container))
         (theValTitle (valTitleLabel container))
         (titleHeight (height theValTitle :physical t))
         (theHSplitterNub (hsplitterNub container))
         (exprWatcherPicker (picker container))
         (widths (columnWidths exprWatcherPicker))
         (totalWidth (SK8-multival-bind (fWidth fHeight) (frameSize exprWatcherPicker :physical t)
                       (declare (ignore fHeight))
                       (- (width exprWatcherPicker :physical t) (* 2 fWidth))))
         newExprColumnWidth)
    (SK8-multival-bind (left top right bottom) (boundsRect exprWatcherPicker)
      (declare (ignore right bottom))
      (SK8-multival-bind (fWidth fHeight) (frameSize exprWatcherPicker) (incf left fWidth) (incf top fHeight))
      (SK8-multival-bind (sLeft sTop sRight sBottom) (boundsRect me)
        (declare (ignore sTop sBottom))
        (setf newExprColumnWidth (round (- sLeft left (columnSpacing exprWatcherPicker)))
              (first widths) newExprColumnWidth
              (second widths) (- totalWidth newExprColumnWidth))
        (withActorLocked (container)
          (setBoundsRect theHSplitterNub (1- sLeft) (- top titleHeight) (1+ sRight) (- top 1))
          (setf (right theExprTitle :resizing t) sLeft)
          (setf (left theValTitle :resizing t) sRight)
          (setf (columnWidths exprWatcherPicker) widths))
        (setf (hsplitterLocation container) (/ sLeft (coerce (width container) 'short-float)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new Rectangle :objectName "ExpressionWatcherHSplitterNub" :project SK8)

(setf (private ExpressionWatcherHSplitterNub) t)  ; not public api

(setf (container ExpressionWatcherHSplitterNub) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherHSplitterNub 'hsplitterNub)

(setf (frameSize ExpressionWatcherHSplitterNub) '(1 0))
(setf (width ExpressionWatcherHSplitterNub) 4)
(setf (fillColor ExpressionWatcherHSplitterNub) Black)
(setf (frameColor ExpressionWatcherHSplitterNub) White)

(define-handler mouseEnter (ExpressionWatcherHSplitterNub)
  (setf (cursor Stage) CursorVSplitter))
(define-handler mouseLeave (ExpressionWatcherHSplitterNub)
  (setf (cursor Stage) StandardCursor))
(define-handler mouseDown (ExpressionWatcherHSplitterNub)
  (mouseDown (hsplitter (container me))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new ScriptEditText :objectName "ExpressionWatcherField" :project SK8)

(setf (private ExpressionWatcherField) t)  ; not public api


(setf (container ExpressionWatcherField) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherField 'expressionField)

(setf (textFont ExpressionWatcherField) "Geneva")
(setf (textStyle ExpressionWatcherField) '(plain))
(setf (textSize ExpressionWatcherField) 9)

(define-handler minimumSize (ExpressionWatcherField)
  '(14 14))


;; #####*** THIS IS TOTALLY BOGUS!  THIS CONNECTION SHOULDN'T BE HARD WIRED LIKE THIS -- BUT IT IS FOR NOW
(define-handler targetProject (ExpressionWatcherField)
  (let ((editorField (exprWatcher-associated-handlerEditText (container me))))
    (if editorField
      (targetProject editorField)
      SK8)))
;; #####*** THIS IS TOTALLY BOGUS!  THIS CONNECTION SHOULDN'T BE HARD WIRED LIKE THIS -- BUT IT IS FOR NOW
(define-handler localVariables (ExpressionWatcherField)
  (let ((editorField (exprWatcher-associated-handlerEditText (container me))))
    (if editorField
      (localVariables editorField)
      nil)))

(define-handler scriptInputCompleted (ExpressionWatcherField translation)
  (let* ((exprWatcherPicker (picker (container me)))
         (itemIndices (first (selection exprWatcherPicker)))
         (handlerId (inputHandlerId (editor (sk8::window me)))))
    (declare (dynamic-extent allLocals))
    (cond
     ;; Either nothing's selected, or an expression cell's selected; put in new expression
     ((or (null itemIndices) (eql 1 (first itemIndices)))
      ;; Turn the translation into callable form.
      (setf translation (registerExpressionForRuntimeUse handlerId (text me)))
      ;; Prepare the data structure used to store in the table.
      (setf translation (list (string-trim *whitespace-charbag* (text me))
                              translation))
      ;; Stick it into the picker
      (withActorLocked (exprWatcherPicker)
        (cond
         ((and itemIndices (not (emptyRowSelected exprWatcherPicker)))
          (setf (item exprWatcherPicker :rowNumber (second itemIndices)) translation)
          (selectionCompleted exprWatcherPicker))
         (t
          (let ((lastRow (rows exprWatcherPicker)))
            (insertRow exprWatcherPicker :item translation :following (- lastRow 2))
            (unless (emptyRowSelected exprWatcherPicker)
              (setf (selectedRow exprWatcherPicker) (1+ lastRow))))))
        (showSelection exprWatcherPicker)))
     
     ;; A value cell's selected; try to set the value of the associated expression
     (t
         ; SET LOCALS!!!  SHOULD DO THIS USING REGISTEREXPRESSION JUNK!!!
      (let ((curvar (aref (items exprWatcherPicker) 0 (1- (second itemIndices))))
            (curProj (first HandlerID))
            var
            translation)
        (when curvar
          (setf curvar (first curvar))
          (if (and (not (string= curvar ""))
                   (= (sk8::length (words curvar)) 1))
            (progn
              (setf translation (translatescriptcommand curProj  curvar))
              (cond
               ((first translation)
                (messageToUser "Unable to set that location." :beep t))
               ((listp (second (fifth translation)))
                (messageToUser "Unable to set that variable." :beep t))
               (t
                (setf var (second (fifth translation)))
                (setf translation (registerExpressionForRuntimeUse HandlerID (text me)))
                (if (and (listp translation) (eq (first translation) 'error))
                  (messageToUser "Can't evaluate that value." :beep t)
                  (progn
                    (multiple-value-bind (res err?) 
                                         (ignore-errors (evaluateExpressionInLocalContext handlerID translation))
                      (if err?
                        (messageToUser "Cannot evaluate that value." :beep t)
                        (progn
                          (setLocalVarInLocalContext handlerId var res)
                          (update (container me))))))))))
            (progn
              (setf translation
                    (registerExpressionForRuntimeUse handlerID 
                                            (concatenate 'string "set " curvar " to " (text me))))
              (if (and (listp translation) (eq (first translation) 'error))
                (messageToUser "Unable to set that location to that value." :beep t)
                (progn
                  (ignore-errors (evaluateExpressionInLocalContext handlerID translation))
                  (update (container me)))
                ))
            )
          
          )))
     )))


(define-handler keyDown (ExpressionWatcherField theKey)
  (let ((thePicker (picker (container me))))
    (cond
     ((eq theKey #\Delete)
      (SK8-multival-bind (s e) (selection me)
        (if (and (eql 0 s) (eql 0 e))
          (unless (emptyRowSelected thePicker) (removeRow thePicker))
          (call-next-method))))
     
     ((eq theKey #\Return)
      (let ((buf (fred-buffer (editData me)))
            (selectedText (selectedText thePicker)))
        (if (and (eql (length selectedText) (buffer-size buf))
                 (buffer-substring-p buf selectedText 0))
          (let ((selectedRow (selectedRow thePicker)))
            (if (shiftKeyDown)
              (unless (eql selectedRow 1) (setf (selectedRow thePicker) (1- selectedRow)))
              (unless (eql selectedRow (rows thePicker)) (setf (selectedRow thePicker) (1+ selectedRow)))))
          (call-next-method))))
     
     (t
      (call-next-method)))))

(define-handler autoKey (ExpressionWatcherField theKey)
  (cond
   ((eq theKey #\Delete)
    (SK8-multival-bind (s e) (selection me)
      (unless (and (eql 0 s) (eql 0 e))
        (keyDown me theKey))))
   (t
    (keyDown me theKey))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new scroller :objectName "ExpressionWatcherScroller" :project SK8)

(setf (private ExpressionWatcherScroller) t)

(setf (container ExpressionWatcherScroller) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherScroller 'scroller)
(setf (partnerVScroller ExpressionWatcherPicker) ExpressionWatcherScroller)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new Rectangle :objectName "ExpressionWatcherResizer" :project SK8)

(setf (private ExpressionWatcherResizer) t)

(setf (container ExpressionWatcherResizer) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherResizer 'resizer)


(setf (frameSize ExpressionWatcherResizer) '(0 0))
(setf (fillColor ExpressionWatcherResizer) ResizeBoxRenderer)
(SK8-multival-bind (width height) (size RuntimeExpressionWatcher)
  (setBoundsRect ExpressionWatcherResizer (- width 16) (- height 16) width height))

(define-handler mouseDown (ExpressionWatcherResizer)
  (resize (sk8::window me) 'bottomRight))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new splitter :objectName "ExpressionWatcherVSplitter" :project SK8)

(setf (private ExpressionWatcherVSplitter) t)

(setf (container ExpressionWatcherVSplitter) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherVSplitter 'vsplitter)

(define-handler mouseEnter (ExpressionWatcherVSplitter)
  (setf (cursor Stage) CursorHSplitter))
(define-handler mouseLeave (ExpressionWatcherVSplitter)
  (setf (cursor Stage) StandardCursor))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new Rectangle :objectName "ExpressionWatcherExprTitle" :project SK8)

(setf (private ExpressionWatcherExprTitle) t)

(setf (text ExpressionWatcherExprTitle) "Expression")
(setf (container ExpressionWatcherExprTitle) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherExprTitle 'exprTitleLabel)

(setf (textFont ExpressionWatcherExprTitle) "Geneva")
(setf (textSize ExpressionWatcherExprTitle) 9)
(setf (textStyle ExpressionWatcherExprTitle) '(bold))

(new Rectangle :objectName "ExpressionWatcherValTitle" :project SK8)

(setf (private ExpressionWatcherValTitle) t)

(setf (text ExpressionWatcherValTitle) "Value")
(setf (container ExpressionWatcherValTitle) RuntimeExpressionWatcher)
(tagPart RuntimeExpressionWatcher ExpressionWatcherValTitle 'valTitleLabel)

(setf (textFont ExpressionWatcherValTitle) "Geneva")
(setf (textSize ExpressionWatcherValTitle) 9)
(setf (textStyle ExpressionWatcherValTitle) '(bold))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sendToBack ExpressionWatcherHSplitter)
(bringToFront ExpressionWatcherHSplitterNub)
(bringToFront ExpressionWatcherVSplitter)
(bringToFront ExpressionWatcherResizer)

;;; Fixing the layers! The vSplitter has to be behind the scroller and the
;;; resize box. This is because the splitter (by definition) spans the width of its container
;;; which makes it easy to use the container's boundsrect for constraining the
;;; drag.

(bringToFront expressionWatcherScroller)
(bringToFront expressionWatcherResizer)

(setf (autoTab RuntimeExpressionWatcher) t)


(define-handler activateText (RuntimeExpressionWatcher)
  (setf (keyTarget (SK8::window me)) (expressionField me)))

(define-handler deactivateText (RuntimeExpressionWatcher)
  (deactivateText (expressionField me)))



(define-handler tabToNextActor (ExpressionWatcherField &key &allow-other-keys)
  (let* ((wind (SK8::window me))
         (ed (when wind (editor wind))))
    (when ed
      (setf (keyTarget wind) ed))))


(define-handler resized (RuntimeExpressionWatcher)
  (SK8-multival-bind (width height) (size me)
    (let* ((titleHeight 17)
           (scrollerWidth 16)
           (growBoxHeight 16)
           (growBoxTop (- height growBoxHeight))
           (scrollerLeft (1+ (- width scrollerWidth)))
           (vsplitterTop (round (* (vsplitterLocation me) height)))
           (hsplitterLeft (round (* (hsplitterLocation me) width))))
      
      ;;; Fix the absolute bottom of the splitter!
      (setf (absoluteBottom (vSplitter me)) height)
      (withActorLocked (me)
        (setBoundsRect (exprTitleLabel me) 0 0 20 titleHeight)
        (setBoundsRect (valTitleLabel me) 20 0 width titleHeight)
        (setBoundsRect (picker me) 0 (1- titleHeight) scrollerLeft (1+ vsplitterTop))
        
        (setBoundsRect (hsplitter me) hsplitterLeft titleHeight (+ 2 hSplitterLeft) vsplitterTop)
        (moved (hsplitter me)) ; *** WHY ISN'T THIS GETTING CALLED AUTOMATICALLY?!
        
        ;; Adam: you might want to preserve the vertical position of everything above the splitter...
        (setBoundsRect (vsplitter me) 0 vsplitterTop width (+ vsplitterTop 2))
        (setBoundsRect (expressionField me) 0 (1+ vsplitterTop) scrollerLeft height)
        (setBoundsRect (scroller me) (1- scrollerLeft) (1- titleHeight) width (1+ growBoxTop))
        (setBoundsRect (resizer me) (1- scrollerLeft) growBoxTop width height)))))

(define-handler bestSize (RuntimeExpressionWatcher)
  (when (visible me)
    (SK8-multival-bind (width height) (size (container me))
      (setBoundsRect me 0 (- height (height me)) (1+ width) (1+ height)))))

(define-handler initialize (RuntimeExpressionWatcher original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (setf (dirty me) nil)
  ;; Initialize the vertical splitter.
  (let ((actorsAtBottom (list (ExpressionField me)))
        (absoluteBottom (height me))
        (absoluteTop 20)
        (actorsAbove (list (picker me) (hsplitter me)))
        (actorsAffected (list (picker me) (hsplitter me) (ExpressionField me))))
    (splitActors (vSplitter me) actorsAbove actorsAtBottom ActorsAffected
                 absoluteTop absoluteBottom)))




#|
	Change History (most recent last):
	1	11/5/93	nil	new file
	14	11/29/93	chip	finished up drag & drop behavior
	3  	 1/19/96	sidney  	removing/rewriting refrences to old sk8script compiler
	4  	 1/22/96	Hernan  	Folding in the new expression watcher API. This should 
						simplify the implementation a lot.
	2  	 4/12/96	Hernan  	Fixing method congruency problems.
	3  	 5/20/96	Brian   	
	4  	 5/23/96	Brian   	
	5  	 5/23/96	sidney  	define best-objectString-for-handlerCall
	7  	12/17/96	Hernan  	Fixing expressionWatcher to not print False when nothing
						has been entered.
	8  	 2/13/97	Brian Roddy	
	9  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
