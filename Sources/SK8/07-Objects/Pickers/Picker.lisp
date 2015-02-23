(in-package :SK8Development)

(provide "PICKER")

(require "RECTANGLE")
(require "ZOOMRECTS" "functions;Zoom Rects")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; By Adam Chipkin and Hernan Epelman-Wang...
;;; Hacked weekly by Brian Roddy


#| Modification History

02-03-93 hernan is porting this to use as a substrate for implementation of the textViewer.
15:40:37  06-09-1992  ADAM  fast-picker no longer flickers selection in listener
15:39:29  06-09-1992  ADAM  Set-items [fast-picker] now does low-level sets properly
05-12-92 ruben d29 conversion
03-29-92 ruben began d29; scroll, selection
03-26-92 ruben gentemp name in NEW

|#

;;; -----------------------------------------------------------------------------------------------
;;; BEGINNING OF MACROS

(defmacro making-picker-dirty (thePicker &body body)
  `(gs:making-self-dirty (,thePicker t nil nil nil (gs:node-fillRegion ,thePicker) :fill nil)
     (progn ,@body)))

;;; END OF MACROS
;;; -----------------------------------------------------------------------------------------------


;;; picker
;;; This object provides the SK8 interface with a very simple minimum 1-d picker object for use in its
;;; interface panels.  This picker has only a fraction of the functionality of the normal SK8 picker, but
;;; should be much faster because of it.
;;;
;;; Gets a list of things for items and uses their objectStrings to print them out in the picker. Gets key events
;;; (for up and down arrow to change the selection).

;;; IMPORTANT NOTE: the selection* is always a sorted list of indeces!!!

(new Rectangle :objectName "Picker" :project sk8
     :properties '((verticalScroll :value  0)                   ;; The first item visible.
                   (items          :value nil)                  ;; The list of items displayed by the picker.
                   (strings        :value nil)                  ;; The list of strings used for display.
                   (selection*      :value  nil)                ;; A number (the position of the selected item).
                   (lineSpacing*    :inherit :value  3)         ;; Number of Pixels between lines...
                   (alphabeticalDisplay  :inherit :value t)     ;; Whether the items are to be shown in ab order.
                   (selectionStyle  :inherit :value single)
                   (HighlightColor :value nil)
                   (partnerVScroller :value nil)
                   (lastSelected  :value nil))                  ;; The last item selected. (see doubleClick!)
     :undisposable t
     :prototype t
     )

(setf (localCreationRelations Picker) '(SK8::[partnerVScroller]))

(setf (HighlightColor picker) Highlighted)
;;;(addproperty picker 'HighlightColor :initialValue Highlighted)

(setf (private Picker) nil)  ; in public api

(setf (private Picker :property 'selection*) t)
(setf (private Picker :property 'lineSpacing*) t)
(setf (private Picker :property 'lastSelected) t)

(setf (textFont picker) "Geneva")
(setf (textSize picker) 9)
(setf (acceptsDrops picker) t)

(define-handler (setf textSize) (size picker)
  (declare (ignore size))
  (making-picker-dirty me
    (call-next-method)))

(define-handler lineSpacing (picker)
  (lineSpacing* me))

(define-handler (setf lineSpacing) (spacing picker)
  (making-picker-dirty me
    (setf (lineSpacing* me) spacing)
    (when (partnervScroller me)
      (updatepartnerScroller me (partnervScroller me)))))

(define-handler (setf selectionStyle) (newStyle picker)
  (cond ((eq newStyle 'single)
         (sk8::setValue 'selectionStyle me newstyle)
         ;; Make sure the selection list has 1 item in it.
         (setf (slot-value me 'selection*) nil))
        ((or (eq newStyle 'contiguous) (eq newStyle 'discontiguous))
         (sk8::setValue 'selectionStyle me newstyle))
        (t (sk8-error PropertyTypeMismatchError
                 :object        newStyle
                 :expectedType  '(single contiguous discontiguous)
                 :ownerObject   Picker
                 :propertyName 'selectionStyle
                 ))))

(define-handler createTextDisplayItem (picker theObject)
  (simpleObjectString theObject))

;;; given a list '((1. a) (2 . b) (3 . c)) returns two lists '(1 2 3) and '(a b c)

(defun separate-the-waters (dualList l1 l2)
  (cond ((null dualList) (values l1 l2))
        (t (multiple-value-bind (ll1 ll2) (separate-the-waters (cdr dualList) l1 l2)
             (values (push (Caar dualList) ll1)
                     (push (cdar dualList) ll2))))))

;;; Returns the items and the strings for the picker to use!

(defun buildPickerText (thepicker itemList)
  (setf itemList (mapcar #'(lambda (anItem) (cons (createTextDisplayItem thepicker anItem) anItem)) itemList))
  (when (alphabeticalDisplay thepicker)
    (setf itemList (sort itemList #'string-lessp :key #'car)))
  ;; get the strings out of it!
  (separate-the-waters itemList nil nil))

(define-handler (setf items) (itemList picker)
  ;; Make sure we have a list!
  ;; (setf itemList (ss::!coerce itemList list))
  (making-picker-dirty me
    (multiple-value-bind (theStrings theItems) (buildPickerText me itemList)
      (setf (slot-value me 'items) theItems)
      (setf (strings me) theStrings)
      (setf (verticalScroll me) 0)
      (if itemList 
        (setf (selection* me) (list 0))
        (progn 
          (setf (selection* me) nil)
          (setf (lastSelected me) nil))))
    (when (partnervScroller me)
      (updatepartnerScroller me (partnervScroller me)))
    itemList))

;;; (SETF SCROLL) -- scrolls the picker so that the first item shown is item # scroll.

(define-handler (setf verticalScroll) (scroll picker &key)
  (unless (= scroll (verticalScroll me))
    (unless (fixnump scroll) (setq scroll (round scroll)))
    (let* ((items (items me))
           (last-item-index (1- (length items))))
      (when items
        (setf scroll (max 0 (min scroll last-item-index)))
        (making-picker-dirty me
          (setf (slot-value me 'verticalScroll) scroll))))))

(define-handler element (picker index)
  (nth index (items me)))

;;; HANDLERS TO DECIDE HOW TO HANDLE SELECTION OF MULTIPLE ITEMS...

(define-handler select-first-item :private (picker &optional adding)
  (if adding
    (case (selectionStyle me)
      (single
       (setf (selection* me) '(0)))
      (contiguous 
       (let ((firstSelected (car (selection* me))))
         (if firstSelected
           (let ((result nil))
             (do ((i (1- firstSelected) (1- i)))
                 ((minusp i) nil)
               (push i result))
             ;; Now add the new items to the selection.
             (setf (selection* me) (nconc result (selection* me)))))))
      (discontiguous
       (pushNew 0 (selection* me))))
    ;; Replacing: easy...
    (setf (selection* me) '(0)))
  (showSelection me))

(define-handler select-last-item :private (picker &optional adding)
  (if adding
    (case (selectionStyle me)
      (single
       (setf (selection* me) (list (1- (length (items me))))))
      (contiguous 
       (let ((lastSelected (car (last (selection* me)))))
         (if lastSelected
           (let ((result nil))
             (do ((i (1- (length (items me))) (1- i)))
                 ((= i lastSelected) nil)
               (push i result))
             ;; Now add the new items to the selection.
             (setf (selection* me) (nconc (selection* me) result))))))
      (discontiguous
       (nconc (selection* me) (list (1- (length (items me)))))))
      ;; Replacing: easy...
      (setf (selection* me) (list (1- (length (items me))))))
  (showSelection me))

(define-handler select-next-item :private (picker &optional adding)
  (if adding
    (case (selectionStyle me)
      (single
       (setf (selection* me) (list (min (1- (length (items me)))
                                          (1+ (car (selection* me)))))))
      ((contiguous discontiguous)
       (let ((lastSelected (car (last (selection* me)))))
         (when (< (1+ lastSelected) (length (items me)))
           (setf (selection* me) (nconc (selection* me) (list (1+ lastSelected))))))))
    ;; Replacing: easy...
    (setf (selection* me) (list (min (1- (length (items me)))
                                       (1+ (car (last (selection* me))))))))
  (showSelection me))

(define-handler select-prev-item :private (picker &optional adding)
  (if adding
    (case (selectionStyle me)
      (single
       (setf (selection* me) (list (max 0 (1- (car (selection* me)))))))
      ((contiguous discontiguous)
       (let ((firstSelected (car (last (selection* me)))))
         (unless (minusp (1- firstSelected))
           (setf (selection* me) (pushnew (1- firstSelected) (selection* me)))))))
      ;; Replacing: easy...
      (setf (selection* me) (list (max 0 (1- (car (selection* me)))))))
  (showSelection me))

(defun compile-selection-list (start end)
  (let ((result nil))
    (do ((i end (1- i)))
        ((< i start) result)
      (push i result))))

(define-handler select-whole-range :private (picker start end &optional adding upwards)
  (if adding
    (case (SelectionStyle me)
      (single
       (setf (selection* me) (list (min (1- (length (items me))) start))))
      (contiguous
       (if (eq adding :notQuite)
         (let* ((oldItems (selection* me))
                (curStart (car oldItems))
                (curEnd (car (last oldItems))))
           (when oldItems
             (cond
              ((and (< start curStart) (> end curEnd))
               (setf (selection* me) nil))
              ((and (< start curStart) (< end curEnd))
               (setf (selection* me) (compile-selection-list (1+ end) curEnd)))
              (t
               (setf (selection* me) (compile-selection-list curStart (1- start)))))))
           (progn 
             (when (selection* me)
               (unless (<= start (car (selection* me))) (setf start (car (selection* me))))
               (unless (>= end (car (last (selection* me)))) (setf end (car (last (selection* me))))))
             ;; OK, we have the two indeces, now select everything between them.
             (setf (selection* me) (compile-selection-list start end)))))
      (discontiguous
       (let ((newItems (compile-selection-list start end))
             (oldItems (selection* me)))
         (if (eq adding :notQuite)
           ;; Remove everything between start and end from the selection.
           (dolist (i newItems)
             (setf oldItems (delete i oldItems)))
           ;; Add everything to the selection.
           (setf oldItems (remove-duplicates (merge 'list oldItems newItems #'<))))
         ;; And set the selection!
         (setf (selection* me) oldItems))))
    ;; Just reset...
    (case (SelectionStyle me)
      (single
       (setf (selection* me) (list (min (1- (length (items me))) start))))
      ((contiguous discontiguous)
       (setf (selection* me) (compile-selection-list start end)))))
  ;; Show the right item in the selection.
  (if upwards
    (showSelection me  :curItem start)
    (showSelection me :curItem end)))

(define-handler compile-item-indeces :private (picker items)
  (let ((theItems (items me)) result pos)
    (dolist (i items nil)
      (setf pos (position i theItems))
      (when pos (pushNew pos result)))
    (sort result #'<)))

;;; This function, that does it all now becomes the inner function.
;;; The interface is written all new.

(defun set-selectedItems (me items &key start end upwards first prev next last (deselecting t))
  (when (items me)
    ;; Nothing to do if there are no items. As long as all the selection functions go through
    ;; there will be no problems. The low level functions assume there are items.
    (let ((adding (if (eq deselecting :notquite)
                    :notquite
                    (not deselecting))))
      (cond (first (select-first-item me adding))
            (last (select-last-item me adding))
            (prev (select-prev-item me adding))
            (next (select-next-item me adding))
            ((and start end) (select-whole-range me start end adding upwards))
            (t
             ;; We got a list of items...
             (unless (listp items) (setf items (list items)))
             (case (selectionStyle me)
               (single
                (setf items (list (car items)))
                (let ((pos (position (car items) (items me))))
                  (if pos
                    (setf (selection* me) (list (min (1- (length (items me))) pos)))
                    (setf (selection* me) nil)))
                (showSelection me))
               ((contiguous discontiguous)
                (if items
                  ;; We find the positions of the items in our collection. Find the start and end and
                  ;; call select-whole-range to do the work.
                  (let ((positions (compile-item-indeces me items)))
                    (if positions
                      (select-whole-range me (car positions) (car (last positions)) adding)
                      (setf (selection* me) nil)))
                  (setf (selection* me) nil)))))))))

;;; Items is a list of items that should be already present in the picker's items.

(define-handler (setf selectedItems) (items picker &key upwards (deselecting t))
  (set-selectedItems me items :upwards upwards :deselecting deselecting))

(define-handler selectedItems (picker &key (indices nil))
  (if indices
    (mapcar #'1+ (selection* me))
    (let ((posList (selection* me))
          (theItems (items me)))
      (mapcar #'(lambda (x) (nth x theItems)) posList))))

;;; With set selection you can specify the items to select by specifying a pair of 
;;; indices (the start and end) or a symbol.

(define-handler (setf selection) (newValue picker &key upwards (deselecting t))
  (cond ((eq newValue 'first) (set-selectedItems me nil :first t :upwards upwards :deselecting deselecting))
        ((eq newValue 'last) (set-selectedItems me nil :last t :upwards upwards :deselecting deselecting))
        ((eq newValue 'previous) (set-selectedItems me nil :prev t :upwards upwards :deselecting deselecting))
        ((eq newValue 'next) (set-selectedItems me nil :next t :upwards upwards :deselecting deselecting))
        (t ; must be a list of 2 indices.
         (set-selectedItems me nil :start (car newValue) :end (cadr newValue) 
                            :upwards upwards :deselecting :deselecting))))

;;; Returns the start and end of the selection.

(define-handler selection (picker)
  (let ((selectedIndices (selection* me)))
    (if selectedIndices
      (sk8-multivals (car selectedIndices) (car (last selectedIndices)))
      (sk8-multivals 0 0))))

;;; Yes, but what selection to show if there are multiple ones? CurItem, when provided, specifies the
;;; item we really want to move to. This is very important in order for picker scrolling while
;;; selecting to work.

(define-handler showSelection (picker &key curItem)
  (let ((theSelection (or curItem (car (Selection* me))))
        (theScroll (verticalScroll me))
        (theSize (size me :how 'items))
        newScroll)
    (making-picker-dirty me
      (when theSelection
        (unless (and (>= theSelection theScroll) (< theSelection (+ theSize theScroll)))
          ;; Now scrolls just one line at a time down.  
          ;; Also this now works if we have a big item which is larger than the size of the picker...
          (Setf newScroll (max 0 (- theSelection (if (< theSelection theScroll) 0 (if (eql thesize 0) 0 (1- thesize))))))
          (setf (verticalScroll me) newScroll))))))

(define-handler SelectAll (picker) 
  (setf (selection me) (list 0 (1- (length (items me))))))


(define-handler keyUp (picker theChar)
  (call-next-method)
  (unless (and (autotab me) (eq thechar #\tab))
    (selectionCompleted me)))

(define-handler keyDown (picker theChar)
  (cond
   ((and (items me) (eq thechar #\Home)) 
    (setf (selection me) 'first))
   ((and (items me) (eq thechar #\End))
    (setf (selection me) 'last))
   ((and (items me) (eq thechar #\PageUp))
    (setf (selection* me) (list (max 0 (- (car (selection* me)) (max 1 (size me :how 'items))))))
    (showselection me))
   ((and (items me) (eq thechar #\Page))
    (setf (selection* me) (list (min (1- (length (items me))) (+ (car (selection* me)) (max 1 (size me :how 'items))))))
    (showselection me))
   ((eq thechar #\upArrow) 
    (when (selectedItems me)
      (set-selectedItems me nil :prev t :deselecting (not (shift-key-p)))))
   ((eq thechar #\downArrow) 
    (when (selectedItems me)
      (set-selectedItems me nil :next t :deselecting (not (shift-key-p)))))
   ((alphanumericp theChar) (process-search-string me theChar))
   (t (call-next-method))))

(define-handler autoKey (picker theChar)
  (case theChar
    (#\upArrow 
     (when (selectedItems me)
       (set-selectedItems me nil :prev t :deselecting (not (shift-key-p)))))
    (#\downArrow 
     (when (selectedItems me)
       (set-selectedItems me nil :next t :deselecting (not (shift-key-p)))))
    (otherwise
     (call-next-method))))

(define-handler get-lineHeight :private (picker)
  (gs:actor-line-height me))

(define-handler size (picker &key (how nil) physical)
  (cond 
   ((eq how 'items) (let* ((oneLine (+ (get-lineHeight me) (lineSpacing me)))
                           (physRect (gs:node-physicalBoundsRect me)))
                      (truncate (- (gs:rect-bottom physRect) (gs:rect-top physRect)) oneLine)))
   ((eq how 'partialitems) (let* ((oneLine (+ (get-lineHeight me) (lineSpacing me)))
                                  (physRect (gs:node-physicalBoundsRect me)))
                             (ceiling (- (gs:rect-bottom physRect) (gs:rect-top physRect)) oneLine)))
   (t (call-next-method me :physical physical))
   ))

(define-handler position-item-visible :private (picker pos)
  (let ((theScroll (verticalScroll me)))
    (and (>= pos theScroll) (< pos (+ (size me :how 'items) theScroll)))))

(define-handler itemVisible (picker item)
  (let ((pos (position item (items me))))
    (when pos 
      (position-item-visible me pos))))


#|  THIS IS THE ORIGINAL NON "..." VERSION...  PERHAPS THIS SHOULD BE AN OPTION
(defun draw-text-not-wrapped (thestring rect)
  (rlet ((fi :fontInfo))
    (#_GetFontInfo fi)
    (let* ((cur-y (rref fi :fontInfo.ascent))
           (line-height (+ cur-y (rref fi :fontInfo.descent) (rref fi :fontInfo.leading)))
           (left-edge (1+ (rref rect :rect.left))))
      (incf cur-y (rref rect :rect.top))
      (#_MoveTo left-edge cur-y)
      (with-cstrs ((cstr thestring))
        (#_DrawText cstr 0 (length thestring)))
      ;;returns new bottom
      (+ (rref rect :rect.top) line-height))))
|#

;;; This draw text checks to see if a line will fit in the alloted space.
;;; if not, it runs a divide and conquer algorithm to find where to cut off the string
;;; and append the "..."  This slows it down only about 5-10% on the average and it 
;;; looks a lot better (like a mac).... -Brian

(defun draw-text-not-wrapped (thestring rect)
  (rlet ((fi :fontInfo))
    (#_GetFontInfo fi)
    (let* ((cur-y (rref fi :fontInfo.ascent))
           (line-height (+ cur-y (rref fi :fontInfo.descent) (rref fi :fontInfo.leading)))
           (left-edge (1+ (rref rect :rect.left)))
           (width (- (rref rect :rect.right) (rref rect :rect.left) 3)))
      (incf cur-y (rref rect :rect.top))
      (#_MoveTo left-edge cur-y)
      (setq thestring (gs:compute-maybe-truncated-text thestring width "É"))
      (with-cstrs ((cstr thestring))
        (#_DrawText cstr 0 (length thestring)))
      ;; Returns new bottom
      (+ (rref rect :rect.top) line-height))))

(defun Set-Pen-For-Hilite ()
  (#_PenMode gs:$hilite)
  )

(defun fastPicker-draw (me thePort draw-Region)
  (gs:let+ ((flags (gs:node-flags me))
            (lineSpace1 (linespacing me))
            (scroll (verticalScroll me))
            (theselection (selection* me))
            (theStrings (strings me))
            (qd-rect (:rect))
            qd-rect-bottom
            item-bottom theString
            (theSize (length theStrings))
            (highlightcolor (highlightcolor me))
            (clip-region (:region))
            (fill (gs:node-fillRegion me)))
    (gs:with-clipped-region draw-region
      ;; Render the fill and the frame
      (gs:recompute-frame-region me flags)
      (render (gs:node-frameColor me) me (gs:node-frameRegion me) thePort)
      (gs:recompute-fill-region me flags)
      (gs:with-composed-clip fill
        (render (gs:node-fillcolor me) me (gs:node-fillRegion me) thePort)
        (when (items me)
          ;; Set up the font stuff.
          (gs:set-text-state me)
          ;; Clip to the fill and set the pen colors.
          (with-rgb (rgb (mcl-color (textColor me)))
            (#_RGBForeColor rgb))
          (with-rgb (rgb (mcl-color (gs:node-fillcolor me)))
            (#_RGBBackColor rgb))
          ;; Set up the rects and variables for the loop.
          (rlet ((fillRect :rect))
            (gs:region-into-rect (gs:node-fillRegion me) fillRect)
            (rset qd-rect :rect.top (rref fillRect :rect.top))
            (rset qd-rect :rect.left (rref fillRect :rect.left))
            (rset qd-rect :rect.bottom (rref fillRect :rect.bottom))
            (rset qd-rect :rect.right (rref fillRect :rect.right)))
          (setq qd-rect-bottom (rref qd-rect :rect.bottom))
          ;; Draw the strings.
          (gs:with-hilitecolor theport highlightcolor
            (do ((theIndex scroll (1+ theIndex)))
                ((= theIndex thesize) nil)
              (setf theString (nth theIndex theStrings))
              ;; Before Drawing: get the textFace to use...
              ;; All set, draw the string!
              (incf (rref qd-rect :rect.top) 1)
              (incf (rref qd-rect :rect.left) 5)
              (setq item-bottom (draw-text-not-wrapped theString qd-rect))
              (decf (rref qd-rect :rect.left) 5)
              (incf item-bottom lineSpace1)
              (when (memq theIndex theSelection)
                (rset qd-rect :rect.bottom item-bottom)
                (Set-Pen-For-Hilite)
                (#_PaintRect qd-rect)
                (#_PenMode #$srcCopy)
                (rset qd-rect :rect.bottom qd-rect-bottom))
              (decf item-bottom 1)
              (when (>= item-bottom qd-rect-bottom) (return))
              (rset qd-rect :rect.top item-bottom))))
        ;; Draw the contents if any!
        (unless (gs:dlist-empty? (gs:node-contents me))
          (#_sectRgn fill draw-region clip-region)
          (gs:with-clipped-region clip-region
            (funcall (gs:node-contentsDrawFunction me) me thePort clip-region fill t))))
      ;; invert if necessary.
      (when (and (gs:hilited? flags) (gs:inverted? flags))
        (#_InvertRect qd-rect)
        ))))

;;; Setting the picker draw function.

(setf (gs:node-drawFunction picker) 'fastPicker-draw)

;;; PointOnWhichPart -- returns the position of the selected item or nil if nothing was clicked on.

(define-handler pointOnWhichPart (picker x y &key part tracking)
  (unless (memq part '(item bounds rect fill frame))
    (SK8-error ArgumentTypeMismatchError
               :handlerName 'pointOnWhichPart :argumentName 'part
               :object part :expectedType '(item ItemAndPosition bounds rect fill frame)))
  (when (neq part 'item) 
    (return-from pointOnWhichPart (call-next-method me x y :part part)))
  (gs:with-temporary-port
    (gs:let+ ((lineSpace1 (linespacing me))
              (lineSpace2 (get-lineHeight me))
              (totalline (+ linespace1 linespace2))
              (allStrings (strings me))
              (scroll (verticalScroll me)) position
              (qd-rect (:rect)))
      (gs:set-text-state me)
      (gs:recompute-fill-region me (gs:node-flags me))
      (rlet ((fillRect :rect))
        (gs:region-into-rect (gs:node-fillRegion me) fillRect)
        (rset qd-rect :rect.top (rref fillRect :rect.top))
        (rset qd-rect :rect.left (rref fillRect :rect.left))
        (rset qd-rect :rect.bottom (rref fillRect :rect.bottom))
        (rset qd-rect :rect.right (rref fillRect :rect.right)))
      ;; Change the points to window coords.
      (sk8-multival-bind (wx wy) (gs:stage-to-window-coords me x y)
        ;; The x does not really matter: reset it to the midoint of the width...
        (when tracking
          (setf wx (gs:f.round (/ (+ (rref qd-rect :rect.left) (rref qd-rect :rect.right)) 2))))
        (if (#_PtInRect (gs:SK8Coords-to-point wx wy) qd-rect)  ;; CHECK THIS!!!
          (progn 
            (setf wy (- (gs:f.round wy) (rref qd-rect rect.top)))
            (setf position (+ (truncate (/ wy totalline)) scroll))
            (if (>= position (length allstrings)) nil position))
          ;; if tracking, check the y position. If above the top, get the item before scroll.
          ;; if not get the scroll + size.
          (if (<= wy (rref qd-rect :rect.top))
            (max 0 (- scroll
                      (ceiling (- (rref qd-rect :rect.top) wy ) (textsize me))))
            (min (1- (length (items me))) 
                 (+ (ceiling (- wy (rref qd-rect :rect.bottom)) (textsize me))
                    scroll
                    (size me :how 'items))))))
      )))

(define-handler ItemBoundsRect (Picker index &key (physical nil))
  (let* ((lineSpace1 (linespacing me))
         (lineSpace2 (get-lineHeight me))
         (item-height (+ lineSpace1 lineSpace2))
         (hh (truncate (car (framesize me))))
         (vv (truncate (cadr (framesize me))))
         (top (- (* item-height index)
                 (* item-height (verticalScroll me)))))
    (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical physical)
      (declare (ignore bb))
      (if physical
        (sk8-multivals (+ ll hh) (+ top tt vv) (- rr hh) (+ top tt item-height))
        (sk8-multivals (+ ll hh) (+ top vv) (- rr hh) (+ top item-height))))
    ))

(define-handler ExtendedMouseDown (picker) 
  nil)

(define-handler mouseDown (picker)
  (call-next-method)
  ;; Make sure the picker is the currentKey.
  (let ((tla (sk8::window me)))
    (when (and tla (neq me (keyTarget tla)))
      (setf (keyTarget tla) me)))
  ;; Get on with it.
  (let ((initial-item (pointOnWhichPart me (eventH) (eventV) :part 'item))
        (oldscroll -1)
        (*event-x* (eventH))
        (*event-y* (eventV))
        new-item
        (ExtendedMousedown t)
        (style (selectionStyle me))
        (deselecting (not (shift-key-p))))
    (when initial-item
      (unless deselecting
        (if (memq initial-item (selection* me))
          (setf deselecting :notQuite)
          (setf deselecting nil)))
      (set-selectedItems me nil :deselecting deselecting :start initial-item :end initial-item))
    ;;  Fixed above line to make it work for single mousedown. (nth initial-Item theItems)))
    (dotimes (i 8)
      (sk8-multival-bind (x y) (mouseloc stage)
        (sk8::wait-time-period second 0.05 :events nil)
        (when (or (not (down mouse))
                  (not (equal (pointOnWhichPart me x y :part 'item) initial-item))) 
          (setf ExtendedMousedown nil)
          (return))))
    (when (and extendedMouseDown initial-item)
      (setf  ExtendedMousedown 
             (extendedMousedown me))) ;;; continue if extended mousedown returns nil
    (unless extendedmousedown  ;;; 
      (loop
        (unless (down mouse) (return))
        (sk8-multival-setf (*event-x*  *event-y*) (mouseloc stage))
        (setq new-item (pointOnWhichPart me *event-x* *event-y* :part 'item :tracking t))
        ;; Only call this again if the current item has changed!
        ;; Now we make do this if either the item changed or if the scroll changed.
        ;; This is because a single item can be so large that the guy can scroll without the item changing.
        (when (and new-item (or (not (= initial-item new-item)) (not (= oldscroll (verticalScroll me)))))
          (setf oldscroll (verticalScroll me))
          (if (and (shift-key-p) (neq style 'single))
            (if (> new-item initial-item)
              (set-selectedItems me nil :start (1+ initial-item) :end new-item :deselecting deselecting)
              (set-selectedItems me nil :start new-item :end (1- initial-item) :upwards t :deselecting deselecting))
            (set-selectedItems me nil :start new-item :end new-item))
          (setf initial-item new-Item)
          )))
    )
  (selectionCompleted me))

(define-handler resized (picker)
  (when (partnervScroller me)
    (updatepartnerscroller me (partnervScroller me))))

(define-handler updatepartnerScroller :private (picker theScroller)
  (let* ((numitems (length (items me)))
         (theSize (min numitems (size me :how 'items))))
    (setScrollerVals theScroller
                     :minval 0
                     :maxval (- numitems theSize)
                     :curval (verticalScroll me)
                     :thumbview theSize
                     :scrollStep 1
                     :pageStep theSize)))

(define-handler partnerScroller (picker orientation)
  (declare (ignore orientation))
  (partnervScroller me))

(define-handler (setf partnerScroller) (theScroller Picker &key)
  (set-partnerScroller me 'verticalScroll 'partnerVScroller theScroller))

(define-handler (setf partnerVScroller) (theScroller picker)
  (setf (partnerScroller me) theScroller))

(define-handler mouseEnter (picker)
  (setf (cursor stage) standardCursor)
  (call-next-method))

(define-handler mouseLeave (picker)
  (setf (cursor stage) standardCursor)
  (call-next-method))

(define-handler addItems (picker itemList)
  (making-picker-dirty me
    (multiple-value-bind (theStrings theItems) (buildPickerText me itemList)
      (setf (slot-value me 'items) (append (items me) theItems))
      (setf (strings me) (append (strings me) theStrings))
      (setf (verticalScroll me) (max 0 (- (length (items me)) (size me :how 'items))))
      (when (partnervScroller me)
        (updatepartnerScroller me (partnervScroller me))))))

;;; ________________________________
;;; Code for picker searchString functionality!
;;; ________________________________

;;; This handler is called when the picker has ended selecting in a fast way...

(define-handler selectionCompleted (picker)
  )

(defun protected-subseq-equality (target sequence)
  (let ((end (length target)))
    (and (<= end (length sequence))
         (string-equal (subseq sequence 0 end) target))))

(defun last-key-time (thePicker)
  (getf (gs:node-properties thePicker) :last-key-time 0))

(defun set-last-key-time (thePicker)
  (setf (getf (gs:node-properties thePicker) :last-key-time)
        (get-internal-real-time)))

(defun pickerTimeIsUp (thePicker)
  (> (- (get-internal-real-time) (last-key-time thePicker)) 1000))

(defun search-string (thePicker)
  (getf (gs:node-properties thePicker) :search-string ""))

(defun add-to-search-string (thePicker theChar)
  (setf (getf (gs:node-properties thePicker) :search-string)
        (concatenate 'string 
                     (getf (gs:node-properties thePicker) :search-string "")
                     (string theChar))))

(defun clear-search-string (thePicker)
  (remf (gs:node-properties thePicker) :search-string))

(defun picker-string-search (thePicker search-string)
  (position search-string (strings thePicker) :test #'protected-subseq-equality))

;;; If the char = newline, we have completed the search string and we look for it. After which we clear the caches.
;;; Else, restart the caches if old and add the new char to the search string.

(define-handler findAndSelect (picker partialString)
  (let ((thepos (position partialString (strings me)
                          :test #'protected-subseq-equality)))
    (when thePos
      (withactorlocked (me)
        (setf (selectedItems me) (list (elt (items me) thePos)))
        ;;;now we center it...
        (let* ((theSize (size me :how 'items))
               (numitems (length (items me)))
               (halfway (- thepos (truncate thesize 2)))
               (maxscroll (- numitems thesize))
               newScroll)
          (Setf newScroll (min maxscroll (max 0 halfway)))
          (setf (verticalScroll me) newScroll)))
        )))

(defun process-search-string (thePicker theChar)
  ;; If the key is the termination key, let the picker know process is finished.
  (if (or (char= theChar #\Newline) (char= theChar #\Enter))
    (selectionCompleted thePicker)
    (progn
      ;; If the caches are old, restart them.
      (when (pickerTimeIsUp thePicker) 
        (clear-search-string thePicker))
      ;; Now set add the character and set the timer.
      (set-last-key-time thePicker)
      (add-to-search-string thePicker theChar)
      (findAndSelect thePicker (search-string thePicker)))))

;;; ________________________________
;;; MouseUp: making picker doubleClick work!
;;; ________________________________

;;; The problem was that doubleclick is determined on an actor basis (two clicks on the same actor
;;; is a doubleclick). In the picker, however, the user only expects doubleclicks to be sent when two
;;; clicks happen on the same selection!!! To intercept this, all pickers need to remember the last thing
;;; selected. If on the second click, the selection remains the same, a valid doubleClick has taken place
;;; and the event is sent. We use the function below for this.
;;;
;;; *** Note, this code is duplicated in TablePicker. Changes made here should be made there as well! -BJR ***


(defun picker-doubleClick-filter (thePicker)
  (when (eq (car (selectedItems thePicker)) (lastSelected thePicker))
    (doubleClick thePicker)))

(define-handler mouseUp (picker)
  ;; Process clicks!
  (when (or gs:*force-click*
            (and (eq (eventActor) gs:*last-mousedown-object*)
                 (< (- (eventTime) gs:*last-mousedown-time*)
                    (#_GetDblTime))
                 (double-click-spacing-p gs:*event-location*
                                         gs:*last-mousedown-location*)))
    (setq gs:*force-click* nil)
    ;; it was a click, for sure
    (when (eq me (eventActor)) (dispatch-click me nil 'picker-doubleClick-filter))
    ;; Propagate up the containment hierarchy!
    (let ((theContainer (container me)))
      (when theContainer
        (mouseup theContainer))))
  ;; Remember the old selection!
  (setf (lastSelected me) (car (selectedItems me))))

(setBoundsRect picker 0 0 50 100)

(define-handler copySelectionToClipboard (picker)
  (let ((theItems (selectedItems me)))
    (when theItems
      (addToClipboard theItems sk8Clipboard :copy nil))))

(define-handler localVirtualProperties (Picker) 
  (when (eq me Picker)
    '(lineSpacing partnerVScroller selectedItems verticalScroll)))

;;; _______________________________ 
;;; Flashing behaviour (from browser components)
;;; _______________________________ 

(define-handler FlashLine (picker)
  (let ((theguy (GetLinePosition me)))
    (sk8-multival-bind (ll tt rr bb) (itemboundsrect me theguy :physical t)
      (setf ll (round ll)
            tt (round tt)
            rr (round rr)
            bb (round bb))
      (drawxorline ll tt rr tt)
      (sleep 0.01)
      (drawxorline ll tt rr tt))))

(define-handler FlashItem (picker)
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

(define-handler GetLinePosition (picker)
  (if (items me)
    (sk8-multival-bind (hh vv) (mouseloc stage)
      (let ((theguy (pointonwhichpart me hh vv :part 'item)))
        (if theguy
          (setf theguy (gs:range 0 theguy (length (items me))))
          (setf theguy (length (items me))))
        theguy))
    0))

(define-handler GetItemPosition (picker)
  (when (items me)
    (sk8-multival-bind (hh vv) (mouseloc stage)
      (let ((theguy (pointonwhichpart me hh vv :part 'item)))
        (when theguy
          (setf theguy (gs:range 0 theguy (1- (length (items me))))))
        (when (not theguy)
          (setf theguy (1- (length (items me)))))
        theguy))
    ))



(define-sk8-function selectedItemsBoundsRect nil (theActor)
  (unless (or (is-a theActor picker) (is-a theActor tablepicker))
    (sk8-error GeneralProgrammaticError
               :strings '("Argument to selectedItemsBoundsRect must be a picker or tablepicker")
               ))
  (let* ((sels (if (inheritsfrom theActor picker) 
                 (selection* theActor)
                 (selection theActor)))
         (firstsel (car sels))
         (lastsel (car (last sels))))
    (sk8-multival-bind (ll tt rr bb) (itemboundsrect theActor firstsel :physical t)
      (sk8-multival-bind (ll1 tt1 rr1 bb1) (itemboundsrect theActor lastsel :physical t)
        (sk8-multivals ll tt rr bb1))))
  )


#|
	Change History (most recent last):
	2	6/11/93	Brian Roddy	Made it a nice size
	3	6/21/93	Hernan	The picker now draws its contents! Also, pickers
				now acceptDrops...
	4	6/24/93	Hernan	Added proper multiple discontiguous selection
				behaviour (the contiguous one is not quite right
				yet though).
	5	6/25/93	Hernan	The Great Renaming of 93.
	11	9/17/93	hernan	Update-partnerScroller now uses setScrollerVals
				to set all the scroller properties. The thumb is then
				only rescaled once.
	12	9/20/93	hernan	Changed a number of things to allow Brian's
				browser components to work. (There are too many
				spiders in this universe. Sigh!...)
	13	9/22/93	hernan	Fixed update-partnerScroller to work correctly
				with the new scroller.
	14	9/29/93	hernan	Adding a keyword to selectedItems to return a 
				list of indices instead of a list of items.
	15	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	16	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	17	10/8/93	hernan	Removed the styled picker and fixed
				showselection and mousedown so they work with
				variable size pickers.
	18	10/8/93	hernan	Removing informative print.
	19	10/11/93	rod	Fixed keyup so that if it's an autotab, it does not send the selectioncompleted
	20	10/13/93	hernan	
	21	10/29/93	rod	
	22	11/1/93	rod	
	23	11/2/93	rod	
	24	11/2/93	rod	Made offset from top and side when drawing. 
				Really we should have a property for this!!! ***
				and we should make sure all the pickers are the
				same.
	25	11/19/93	hernan	Fixed the mousedown method of the picker and
				added error checking on setting the selectionStyle.
	26	11/19/93	hernan	Now using setValue to set the value of SelectionStyle 
				(an inheritable property).
	27	11/22/93	rod	Modified pointonwhichpart, ShowSelection, and
				MouseDown so that we scroll at a speed 
				proportional to the distance we are from the
				bottom of the picker.
	28	11/22/93	rod	Moved all relevant word wrapping functionality 
				to special pickers (with the multilinepicker).
	29	11/22/93	rod	Made minor change to showselection so that
				the proportional scrolling works with multiline
				pickers.
	30	11/22/93	rod	
	31	11/24/93	hernan	Removing from get-lineheight the stuff that sets
				the textfont. This will have to be done somewhere
				else. Also redefined with-temporary-port to use
				the pixmapport. It is probable that the old 
				implementation of this macro was to blame for
				bashing the system's font.
	32	11/29/93	hernan	Using let+ to get the ports.
	33	11/29/93	hernan	Adding cut/paste behaviour.
	34	11/29/93	hernan	Fixing addItems to avoid screwing up the scroll*.
	35	11/30/93	rod	
	35	11/30/93	rod	fixed autokey to call-next-method
	36	12/1/93	hernan	Making get-lineheight restore the text state of
				the system. Still looking for the elusive font
				problem.
	37	12/1/93	hernan	Using actor-line-height to get the text height of
				the picker.
	38	12/1/93	hernan	Moving with-temporary-port to the utilities file.
	39	12/10/93	hernan	WaitTimePeriod is now called without events.
	40	12/17/93	hernan	Getting rid of the scroll* property and making a 
				real (visible) property that can be used to hook
				up ports.
	41	12/21/93	sidney	Changes so files can be compiled
	42	1/10/94	hernan	Fonts just became objects!!!
	43	1/11/94	hernan	self -> me
	44	1/14/94	hernan	Now using ports to talk to scrollers!
	45	1/18/94	rod	Changed draw-text-not-wrapped to append "..."
				on the end of strings that do not fit.  See 
				comments there.
	46	2/12/94	kleiman	name changes
	47	2/18/94	sidney	alphabetical and strings should not be private
	48	2/18/94	sidney	it looks like ss:!waitimeperiod's name is now ss::wait-time-period
	49	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	50	2/21/94	hernan	window -> sk8::window.
	51	2/25/94	hernan	Using symbols instead of keywords for options!!!
	52	3/1/94	brian	fixed quoted symbol to be unquoted in property
				declaration
	53	3/1/94	rod	Fixed PointOnWhichPart Bug
	54	3/3/94	Hernan	The great handler argument name renaming of 94!
	56	3/6/94	Hernan	update-partnerScroller -> udpatePartnerScroller.
	57	3/9/94	Hernan	alphabetical -> alphabeticalDisplay
	58	3/10/94	rod	No longer holding on to the last selected item 
				when items are set to nil.
	59	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	60	3/21/94	kleiman	setvalue -> sk8::setValue
	61	3/22/94	rod	FIxed multiple selection bug.
	62	4/11/94	kleiman	code review
	63	4/12/94	kleiman	code review: took out compiler warnings
				and changed some macros into functions.
				put local macro into eval-when
	64	4/12/94	rod	Adding a selection color.  By default it is just the
				highlightrenderer
	65	4/12/94	rod	Fixing draw function.
	66	4/15/94	Hernan	Removed references to hand cursors.
	67	4/15/94	rod	Fixing HighlightColor for all pickers and edittext...
	68	4/18/94	rod	
	69	4/20/94	Hernan	Replacing calls to get-record-field with calls to
				region-into-rect (which does not cons).
	70	4/26/94	rod	making draw function a little faster.
	71	6/14/94	rod	Fixing bug in the computation of a picker scroller's
				maximum value.
	72	6/16/94	till	portType keywords begone.
	73	6/23/94	rod	
	74	6/29/94	rod	Highlight stuff.
	75	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	76	8/3/94	rod	..."'s are now the elipsis character.
	77	8/22/94	rod	Fixing thorny bug in draw-text-not-wrapped. 
				Missed a boundary condition.
	78 	 9/ 1/94	chip    	draw-text-not-wrapped no longer dies with huge strings (radar #1183704)
	79 	 9/ 2/94	Hernan  	Getting rid of the vScroller property.
	80 	 9/ 9/94	Hernan  	1185305: making createTextDisplayItem use
							simpleObjectString instead of ObjectString.
	81 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	82 	10/ 7/94	Hernan  	1191072: fixing picker's clipping problems.
	83 	10/13/94	chip    	updated (setf partnerScroller) to use set-partnerScroller
	84 	10/18/94	chip    	added partnerscroller creationRelations
	85 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	86 	 1/ 9/95	rod     	Fixing extendedmousedown to be called only if 
							on an item..
	87 	 1/20/95	Hernan  	Splitting the functionality of set selectedItems
							into set selectedItems and set selection.
	88 	 2/ 2/95	rod     	making findandselect scroll to the middle.
	89 	 2/ 6/95	Hernan  	Picker functions should all call set-selectedItems
							to select things. The lower level functions should
							not be called directly because they make 
							dangerous assumptions about the state of the
							picker.
	90 	 3/ 2/95	rod     	making mousedown call-next-method
	91 	 3/ 7/95	rod     	Fixing pageup and pagedown to always scroll
							at least one.
	92 	 3/ 7/95	rod     	typo
	93 	 3/15/95	rod     	Adding error checking to pointonwhichpart.
	2  	 7/24/95	Hernan  	Fixing typo in select-first-item.
	3  	 2/15/96	Brian   	Adding selectAll.
~~	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 4/19/96	Brian   	changing package of wait time period
	6  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
