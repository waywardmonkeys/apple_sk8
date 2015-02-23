;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "MULTILINEPICKER")

(require "PICKER" "objects;Pickers:Picker")

;;; ________________________________________________________
;;; ________________________________________________________
;;; NOW THE MultiLinePicker
;;; ________________________________________________________
;;; ________________________________________________________

(new StyledPicker 
     :objectName "MultiLinePicker"
     :prototype t
     :properties '((DrawSeparator :inherit :value t)
                   (LineInfo :inherit :value nil))
     :project sk8)

(setf (linespacing MultiLinePicker) 8)

(define-handler (setf DrawSeparator) :after (theVal MultiLinePicker)
                (declare (ignore theVal))
                (forceRedraw me))

(define-handler ResetLineInfo :private (MultiLinePicker numtimes)
                (let ((theval nil))
                  (dotimes (i numtimes)
                    (setf theval (cons nil theval)))
                  (setf (lineinfo me) theval)))

(defun skip-whitespace (thestring &optional (i 0))
  (locally (declare (optimize (safety 0) (speed 3) (debug 0)))
    (let ((len (length thestring))
          thechar)
      (decf i)
      ;; Doing this loop inline instead of using the POSITION function makes this function 5x faster
      (loop
        (when (>= (incf i) len) (return nil))
        (setq thechar (aref thestring i))
        ;; Doing this test as an OR instead of a MEMQ makes this function 50% faster
        (when (and (neq thechar #\Space)
                   (neq thechar #\Newline)
                   (neq thechar #\Tab))
          (return i))))))

(defun find-word-break (thestring &optional i)
  (locally (declare (optimize (safety 0) (speed 3) (debug 0)))
    (unless i (setf i 0))
    (let ((len (length thestring))
          thechar)
      (decf i)
      ;; Doing this loop inline instead of using the POSITION function makes this function 5x faster
      (loop
        (when (>= (incf i) len) (return len))
        (setq thechar (aref thestring i))
        ;; Doing this test as an OR instead of a MEMQ makes this function 50% faster
        (when (or (eq thechar #\Space)
                  (eq thechar #\Newline)
                  (eq thechar #\Tab))
          (return i))))))

(defun ParsePickerText (me theString theIndex rect)
  (let ((theval (nth theIndex (LineInfo me))))
    (if theval
      theval
      (rlet ((fi :fontInfo)
             (theport :pointer))
        (#_GetFontInfo fi)
        (#_GetPort theport)
        (let* ((left-edge (1+ (rref rect :rect.left)))
               (line-width (- (rref rect :rect.right) left-edge 4))
               (cur-y (rref fi :fontInfo.ascent))
               (line-height (+ cur-y (rref fi :fontInfo.descent) (rref fi :fontInfo.leading)))
               (bottom (+ (rref rect :rect.bottom) line-height))
               (word-start 0)
               (last-word-start 0)
               (last-word-end 0)
               (cur-width 0)
               last-word
               temp-cur-width
               last-word-width
               temp
               (BreakList nil))
          (incf cur-y (rref rect :rect.top))
          (macrolet ((calc-width (thestring) 
                       `(with-cstrs ((cstr ,thestring)) (#_TextWidth cstr 0 (length ,thestring)))))
            (tagbody
              :NEW-WORD
              (unless word-start (go :EXIT))
              (setq last-word-start word-start
                    last-word-end (find-word-break thestring last-word-start)
                    word-start (skip-whitespace thestring last-word-end))
              :RESTART-WORD
              (setq last-word (subseq thestring last-word-start last-word-end))
              (setq last-word-width (calc-width last-word)
                    temp-cur-width (+ cur-width last-word-width)
                    )
              ;; If we haven't yet overflowed the line-width, process the next word:
              (when (<= temp-cur-width line-width)
                (when (null word-start) (go :FINISH-UP))
                (incf cur-width (calc-width (subseq thestring last-word-start word-start)))  ;;; we choose the start to the beginning of the next word so we can include the width of white space in our calculations
                (go :NEW-WORD))
              (cond
               ;; When we've overflowed, but there are no words ready to spit out, break the current word & spit it out:
               ((eq 0 cur-width)
                (with-cstrs ((cstr last-word))
                  (%stack-block ((onLeftSide 2))
                    (setq temp (#_Pixel2Char cstr (length last-word) 0 line-width onLeftSide))
                    (when (and (eq 0 (%get-word onLeftSide)) (neq 0 temp)) (decf temp))
                    (when (eq 0 temp) (incf temp)))
                  (setf BreakList (cons (+ (or (car breaklist) 0) temp) BreakList)))
                (when (> (incf cur-y line-height) bottom) (go :EXIT))
                (setq last-word-start (+ last-word-start temp))
                (when (eql last-word-start last-word-end)
                  (setq cur-width 0)
                  (go :NEW-WORD)))
               ;; Otherwise, spit out the words that are ready to go:
               (t
                (when (> (incf cur-y line-height) bottom) (go :EXIT))
                (setf BreakList (cons last-word-start BreakList))
                ))
              (setq cur-width 0)
              (when word-start (go :RESTART-WORD))
              :FINISH-UP
              (setq last-word (subseq thestring last-word-start))
              :EXIT
              (setf BreakList (cons last-word-end BreakList))
              ))
          (decf cur-y (rref rect :rect.top))
          (setf (nth theIndex (LineInfo me)) (list cur-y BreakList))
          (sk8-multivals cur-y BreakList))))))

(defun DrawMultiLinePickerText (theString TheBreakList rect)
  (rlet ((fi :fontInfo)
         (theport :pointer))
    (#_GetFontInfo fi)
    (#_GetPort theport)
    (let* ((left-edge (1+ (rref rect :rect.left)))
           (cur-y (rref fi :fontInfo.ascent))
           (line-height (+ cur-y (rref fi :fontInfo.descent) (rref fi :fontInfo.leading)))
           (last-break 0)
           (cutoff (- (rref rect :rect.bottom) line-height))
           (numlines (length theBreakList)) 
           (cnt 1)
           str strlen)
      (incf cur-y (rref rect :rect.top))
      (dolist (i (reverse TheBreakList))
        (#_MoveTo left-edge cur-y)
        (setq str (subseq theString last-break i)
              strlen (- i last-break))
        (when (and (>= cur-y cutoff) (not (= numlines cnt)))
          (setf str (concatenate 'string str "..."))
          (setf strlen (+ 3 strlen))
          )
        (with-cstrs ((cstr str)) (#_DrawText cstr 0 strlen))
        (setf last-break i)
        (when (>= cur-y cutoff)
          (return))
        (incf cur-y line-height)
        (incf cnt)
        ))))

(defun DrawMultiLinePickerItem (self theIndex theString top left right bottom IsSelected 
                                         DrawSeparator SpaceBetweenLines HalfSBL
                                         )
  ;; draw the string...
  (rlet ((fi :fontInfo)
         (newRect :rect))
    (#_GetFontInfo fi)
    (rset newRect :rect.left (+ 5 left)) ;;; +5 because we like a little breathing room...
    (rset newRect :rect.top  0) 
    (rset newRect :rect.right right)
    (rset newRect :rect.bottom  4096)
    (sk8-multival-bind (text-height BreakList) (ParsePickerText self theString theIndex newRect)
      (let* ((newbottom (+ top text-height spacebetweenlines))
             (midpoint (+ halfsbl (truncate text-height 2) top)))
        (rset newRect :rect.left (+ 5 left)) ;;; +5 because we like a little breathing room...
        (rset newRect :rect.top  (- midpoint (truncate text-height 2))) ;;; We center the text
        (rset newRect :rect.right right)
        (rset newRect :rect.bottom bottom)
        (DrawMultiLinePickerText theString BreakList newRect)  ;(concatenate 'string (objectstring BreakList) theString)
        (when IsSelected        
          (rset newRect :rect.left left )
          (rset newRect :rect.top  top) 
          (rset newRect :rect.right right)
          (rset newRect :rect.bottom  newbottom) 
          ;; Now render into it!
          (Set-Pen-For-Hilite)
          (#_PaintRect newRect)
          (#_PenMode #.#$srcCopy)
          )
        (when DrawSeparator
          (#_PenMode #$PatXor)
          (#_PenPat *black-pattern*)
          (#_MoveTo left newbottom)
          (#_lineTo right newbottom)
          (#_PenMode #.#$srcCopy))          
        ;;returns new bottom
        newbottom))))

(defun MultiLinePicker-draw (self thePort draw-Region)
  (gs:let+ ((flags (gs:node-flags self))
            (scroll (verticalScroll self))
            (theselection (selection* self))
            (theStrings (strings self))
            (theItems (items self))
            (highlightcolor (highlightcolor self))
            (DrawSeparator (DrawSeparator self))
            (SpaceBetweenLines (linespacing self))
            (HalfSBL (truncate SpaceBetweenLines 2))
            qd-rect-bottom qd-rect-top qd-rect-left qd-rect-right 
            item-top item-bottom theItem
            (theSize (length theStrings))
            ;; Implementing styles!
            (clip-region (:region))
            (tempRgn (:region)))
    (gs:with-clipped-region draw-region
      ;; Render the fill and the frame
      (gs:recompute-fill-region self flags)
      (gs:recompute-frame-region self flags)
      (render (gs:node-fillcolor self) self (gs:node-fillRegion self) thePort)
      (render (gs:node-frameColor self) self (gs:node-frameRegion self) thePort))
    ;; clip to the fill to draw innards...
    (#_sectRgn (gs:node-fillRegion self) draw-region tempRgn)
    (gs:with-clipped-region tempRgn
      ;; Set up the rects and variables for the loop.
      (let ((fillRect (gs:hrref (gs:node-fillRegion self) :region.rgnbbox :storage :pointer)))
        (setq qd-rect-top (rref fillRect :rect.top)
              qd-rect-bottom (rref fillRect :rect.bottom)
              qd-rect-left (rref fillRect :rect.left)
              qd-rect-right (rref fillRect :rect.right)
              item-top qd-rect-top))
      ;; Draw the strings.
      (when theitems
        (gs:with-hilitecolor theport highlightcolor
          (do ((theIndex scroll (1+ theIndex))
               (curitem (nthcdr scroll theItems) (rest curitem))
               (curstring (nthcdr scroll theStrings) (rest curstring)))
              ((= theIndex thesize) nil)
            (setq theItem (first curitem))
            (SetQuickDrawFontStuff self theItem theindex)
            ;; All set, draw the item
            (setf item-bottom (DrawMultiLinePickerItem self theIndex 
                                                       (first curstring) item-top qd-rect-left qd-rect-right qd-rect-bottom (memq theIndex theSelection)
                                                       DrawSeparator SpaceBetweenLines HalfSBL))
            (setq item-top item-bottom)
            (when (>= item-bottom qd-rect-bottom) (return)))))
      
      ;; Draw the contents if any!
      (unless (gs:dlist-empty? (gs:node-contents self))
        (#_sectRgn (gs:node-fillRegion self) draw-region clip-region)
        (gs:with-clipped-region clip-region
          (funcall (gs:node-contentsDrawFunction self) self thePort clip-region (gs:node-fillRegion self) t)))
      
      ;; invert if necessary.
      (when (and (gs:hilited? flags) (gs:inverted? flags))
        (#_InvertRect (gs:hrref (gs:node-fillRegion self) :region.rgnbbox :storage :pointer))))))

(setf (gs:node-drawFunction MultiLinePicker) 'MultiLinePicker-draw)

(define-handler (setf Items) (theVal MultiLinePicker)
  (unless theval (setf (verticalscroll me) 0))
  (ResetLineInfo me (length theval))
  (call-next-method)
  )

(define-handler showSelection (MultiLinePicker &key curItem)
  (let ((theSelection (or curItem (car (Selection* me))))
        (theScroll (verticalScroll me))
        (theSize (size me :how 'items))
        newScroll)
    (making-picker-dirty me
      (when theSelection
        (unless (and (>= theSelection theScroll) (< theSelection (+ theSize theScroll)))
          (if (< theSelection theScroll) 
            (Setf newScroll (max 0 theSelection))
            (withActorLocked (me)     ;;;;IF WE ARE ONSCREEN, WE POSITION AS FAR DOWN AS POSSIBLE WHERE IT IS STILL FULLY VISIBLE
              (let ((hh (height me))
                    (count 0)
                    (linespacing (linespacing me)))
                (loop  ;;; we loop backwards through the items starting at our selection finding how many we can include in the given height...
                  (if (= 0 (- theSelection count)) (return))
                  
                  ;;;These two lines insure that our lineinfo is ok
                  (setf (verticalScroll me) (- theSelection count))
                  (unless (nth (- theSelection count) (lineinfo me))
                    (special-forceredraw me))
                  ;;; lineheight is stored in the lineinfo + the linespacing...
                  (decf hh (car (nth (- theSelection count) (lineinfo me))))
                  (decf hh linespacing)
                  ;; if we are less than zero then we have gone past the top. time to stop and decrement by 1...
                  (if (< hh 0) (return))
                  (incf count)
                  )
                (decf count)
                (setf count (max 0 count))
                (Setf newScroll (- theSelection count)))))
          (setf (verticalScroll me) newScroll))))))

(define-handler addItems (MultiLinePicker itemList)
  (ResetLineInfo me (+ (length (items me)) (length itemList)))
  (call-next-method))

;;; Because we can have many different sizes (number of items visible at a time) depending on the scroll
;;; we have to update the scroller each time this changes...

(define-handler (setf verticalscroll) (theval multilinepicker)
  (declare (ignore-if-unused theVal))
  (call-next-method)
  (when (partnervScroller me)
    (updatepartnerScroller me (partnervScroller me))))

(define-handler setboundsrect (MultiLinePicker left top right bottom 
                                                  &key physical relative justMoving)
  (declare (ignore left top right bottom physical relative justMoving))
  (ResetLineInfo me (length (items me)))
  (call-next-method))

(defun special-forceredraw (me)
  (gs:let+ ((bogus-rect (:rect))
            (bogus-rgn (:region))
            (thePort (:cgrafport)))
    (with-port thePort
      (rset bogus-rect :rect.top 0)
      (rset bogus-rect :rect.left 0)
      (rset bogus-rect :rect.bottom 0)
      (rset bogus-rect :rect.right 0)
      (#_rectrgn bogus-rgn bogus-rect)
      (MultiLinePicker-draw me thePort bogus-rgn))))

(define-handler pointOnWhichPart (MultiLinePicker x y &key part tracking)
  (when (neq part 'item) 
    (return-from pointOnWhichPart (call-next-method me x y :part part)))
  (let* ((numstrings (length (strings me)))
         (scroll (verticalScroll me))
         (minlines (ceiling (height me) (get-lineheight me)))
         (SpaceBetweenLines (linespacing me))
         midx valueToReturn
         item-bottom)
    (sk8-multival-bind (ll tt rr bb) (fillboundsrect me :physical t)
      (if tracking
        (setf midx (+ (truncate (- rr ll) 2) ll))
        (setf midx x))
      (setf ValueToReturn
            (if (items me)
              (if (and (>= midx ll) (< midx rr) (>= y tt) (< y bb))
                (progn 
                  (setf item-bottom tt)
                  (when (some #'null (subseq (lineinfo me) scroll (min numstrings (+ scroll minlines))))
                    (special-forceredraw me))
                  (do  ((theIndex scroll (1+ theIndex)))
                       ((>= theIndex numstrings) nil)
                    (incf item-bottom (car (nth theindex (lineinfo me))))
                    (incf item-bottom spacebetweenlines)
                    (when (<= y item-bottom) (return theIndex))
                    (when (>= item-bottom bb) (progn (ed-beep) (return)))))
                (if (<= y tt)
                  (max 0
                       (- scroll
                          (ceiling (- tt y) (textsize me))))
                  (min (1- numstrings) 
                       (+ (ceiling (- y bb) (textsize me))
                          scroll
                          (size me :how 'items)))))
              0))
      ValueToReturn
      )))

(define-handler size (MultiLinePicker &key (how nil) physical)
  (if (and (neq how 'items) (neq how 'ItemAndPosition) (neq how 'partialitems))
    (call-next-method me :physical physical)
    (let* ((numstrings (length (strings me)))
           (scroll (verticalScroll me))
           (minlines (ceiling (height me) (get-lineheight me)))
           (SpaceBetweenLines (linespacing me))
           (thecount 0)
           item-bottom)
      (when (strings me)
        (sk8-multival-bind (ll tt rr bb) (fillboundsrect me :physical t)
          (declare (ignore ll rr))
          (setf item-bottom tt)
          (when (some #'null (subseq (lineinfo me) scroll (min numstrings (+ scroll minlines))))
            (special-forceredraw me))
          (do  ((theIndex scroll (1+ theIndex)))
               ((>= theIndex numstrings) theCount)
            (incf item-bottom (car (nth theindex (lineinfo me))))
            (incf item-bottom SpaceBetweenLines)
            (when (>= item-bottom bb) 
              (when (and (eq how 'partialitems) (not (= item-bottom bb)))
                (incf thecount))
              (return))
            (incf theCount))))
      (if (items me)
        (max 1 theCount)
        thecount))))

(define-handler itemBoundsRect (MultiLinePicker index &key physical)
  (cond
   ((not (items me))
    (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical physical)
      (declare (ignore rr bb))
      (sk8-multivals ll tt ll tt)))
   ((< index (verticalScroll me)) 
    (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical physical)
      (declare (ignore bb))
      (sk8-multivals ll tt rr tt)))
   ((and (>= index (+ (verticalScroll me) (size me :how 'partialitems))))
    (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical physical)
      (declare (ignore tt))
      (sk8-multivals ll bb rr bb)))
   (t
    (let* ((scroll (verticalScroll me))
           (SpaceBetweenLines (linespacing me))
           item-bottom item-top)
      (setf item-bottom 0)
      (special-forceredraw me)
      (dotimes (i (1+ (- index scroll)))
        (setf item-top item-bottom)
        (incf item-bottom (car (nth (+ i scroll) (lineinfo me))))
        (incf item-bottom SpaceBetweenLines)
        )
      (sk8-multival-bind (ll tt rr bb) (fillboundsrect me :physical physical)
        (declare (ignore bb))
        (sk8-multivals ll (+ item-top (if physical tt 0)) rr (+ (if physical tt 0) item-bottom))
        )))))


#|
	Change History (most recent last):
~~	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 2/27/97	Hernan  	Fixing method congruency problems.
|# ;(do not edit past this line!!)
