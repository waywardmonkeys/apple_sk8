;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "STYLEDPICKER")

(require "PICKER" "objects;Pickers:Picker")

;;; ________________________________________________________
;;; ________________________________________________________
;;; The styledPicker: a picker with independent styles for each line.
;;; MAYBE WE SHOULD GET RID OF THIS AND JUST HAVE THE MULTILINE PICKER
;;; THIS GUY'S SIZE, ETC. HANDLERS ARE FRIGGIN' SLOW!
;;; ________________________________________________________
;;; ________________________________________________________

(new picker 
     :objectName "StyledPicker"
     :undisposable t
     :prototype t
     :properties '((styles :value nil))
     :project sk8)

(setf (private StyledPicker) nil)   ; in public api
(setf (private StyledPicker :property 'styles) t)

(defun SetQuickDrawFontStuff (me theitem theindex)
  (let* ((properties (gs:node-properties me))
         (default-text-size (gs:f.round (gs:f* (textSize me) (gs:node-yScale me))))
         (default-text-font (getf properties :text-font ChicagoFont))
         (default-text-style (getf properties :text-style 0))
         (default-text-color (sk8::mcl-color (textColor me)))
         (theStyles (styles me))
         (thestring (nth theindex (strings me)))
         theStyle new-size new-font new-style new-color)
    ;; Clip to the fill and set the pen colors.
    (with-rgb (rgb default-text-color)
      (#_RGBForeColor rgb))
    (with-rgb (rgb (sk8::mcl-color (gs:node-fillcolor me)))
      (#_RGBBackColor rgb))
    (if theStyles
      (setf theStyle (nth theIndex theStyles)
            new-font (car theStyle)
            new-size (cadr theStyle)
            new-style (caddr theStyle)
            new-color (cadddr theStyle))
      (sk8-multival-setf (new-font new-size new-style new-color) 
                         (createTextItemStyle me theItem theString (1+ theIndex))))
    (if new-font
      (#_TextFont (fontData new-font))
      (#_TextFont (fontData default-text-font)))
    (if new-style 
      (#_TextFace (gs:get-style-number new-style))
      (#_TextFace default-text-style))
    (if (not new-size)
      (setf new-size default-text-size))
    (#_TextSize new-size)
    (if new-color
      (with-rgb (rgb (sk8::mcl-color new-Color))
        (#_RGBForeColor rgb))
      (with-rgb (rgb default-text-color)
        (#_RGBForeColor rgb)))))

(defun styledPicker-draw (me thePort draw-Region)
  (gs:let+ ((flags (gs:node-flags me))
            (lineSpace1 (linespacing me))
            (scroll (verticalScroll me))
            (theselection (selection* me))
            (theStrings (strings me))
            (highlightcolor (highlightcolor me))
            (theItems (items me))
            (qd-rect (:rect))
            qd-rect-bottom qd-rect-top
            item-top item-bottom theString theItem
            (theSize (length theStrings))
            ;; Implementing styles!
            (clip-region (:region))
            (fill (gs:node-fillRegion me)))
    (gs:with-clipped-region draw-region
      ;; Render the fill and the frame
      (gs:recompute-fill-region me flags)
      (gs:recompute-frame-region me flags)
      (render (gs:node-frameColor me) me (gs:node-frameRegion me) thePort)
      ;; Clip to the fill and set the pen colors.
      ;; Set up the rects and variables for the loop.
      (gs:with-composed-clip fill
        (render (gs:node-fillcolor me) me (gs:node-fillRegion me) thePort)
        (rlet ((fillRect :rect))
          (gs:region-into-rect (gs:node-fillRegion me) fillRect)
          (rset qd-rect :rect.top (rref fillRect :rect.top))
          (rset qd-rect :rect.left (rref fillRect :rect.left))
          (rset qd-rect :rect.bottom (rref fillRect :rect.bottom))
          (rset qd-rect :rect.right (rref fillRect :rect.right)))
        (setq qd-rect-top (rref qd-rect :rect.top)
              qd-rect-bottom (rref qd-rect :rect.bottom)
              item-top qd-rect-top)
        ;; Draw the strings.
        (when theitems
          (gs:with-hilitecolor theport highlightcolor
            (do ((theIndex scroll (1+ theIndex))
                 (curitem (nthcdr scroll theItems) (rest curitem))
                 (curstring (nthcdr scroll theStrings) (rest curstring)))
                ((= theIndex thesize) nil)
              (setf theString (first curstring))
              (setf theItem (first curitem))
              ;; Before Drawing: get the textFace to use...
              (SetQuickDrawFontStuff me theItem theindex)
              ;; All set, draw the string!
              (incf (rref qd-rect :rect.left) 5)
              (setq item-bottom (draw-text-not-wrapped theString qd-rect))
              (decf (rref qd-rect :rect.left) 5)
              (incf item-bottom lineSpace1)
              (when (memq theIndex theSelection)
                (rset qd-rect :rect.top item-top)
                (rset qd-rect :rect.bottom item-bottom)
                (Set-Pen-For-Hilite)
                (#_PaintRect qd-rect)
                (#_PenMode #$srcCopy)
                (rset qd-rect :rect.bottom qd-rect-bottom))
              (setq item-top item-bottom)
              (when (>= item-bottom qd-rect-bottom) (return))
              (rset qd-rect :rect.top item-bottom))))
        ;; Draw the contents if any!
        (unless (gs:dlist-empty? (gs:node-contents me))
          (#_sectRgn fill draw-region clip-region)
          (gs:with-clipped-region clip-region
            (funcall (gs:node-contentsDrawFunction me) me thePort clip-region fill t))))
      ;; invert if necessary.
      (when (and (gs:hilited? flags) (gs:inverted? flags))
        (#_InvertRect qd-rect)))))

(setf (gs:node-drawFunction styledPicker) 'styledPicker-draw)

;;; Returns... {font,size,style,color}

(define-handler createTextItemStyle (styledPicker targetItem theString position)
  (declare (ignore targetItem theString position))
  (sk8-multivals nil nil nil nil))

(define-handler pointOnWhichPart (styledPicker x y &key part tracking)
  (when (neq part 'item) 
    (return-from pointOnWhichPart (call-next-method me x y :part part)))
  (gs:with-temporary-port
    (gs:let+ ((allStrings (strings me))
              (numstrings (length allstrings))
              (theItems (items me))
              (scroll (verticalScroll me))
              (SpaceBetweenLines (linespacing me))
              (qd-rect (:rect))
              qd-rect-bottom  midx valueToReturn
              item-bottom)
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
        (if tracking
          (setf midx (gs:f.round (/ (+ (rref qd-rect :rect.left) (rref qd-rect :rect.right)) 2)))
          (setf midx wx))
        (setf ValueToReturn
              (if (#_PtInRect (gs:SK8Coords-to-point midx wy) qd-rect)  ;; CHECK THIS!!!
                (progn 
                  (setq wy (gs:f.round wy)
                        qd-rect-bottom (rref qd-rect :rect.bottom)
                        item-bottom (rref qd-rect :rect.top))
                  (do  ((theIndex scroll (1+ theIndex))
                        (curitem (nthcdr scroll theItems) (rest curitem)))
                       ((>= theIndex numstrings) nil)
                    (SetQuickDrawFontStuff me (first curitem) theindex)
                    (rlet ((fi :fontInfo))
                      (#_GetFontInfo fi)
                      (incf item-bottom (+ (rref fi :fontInfo.ascent)
                                           (rref fi :fontInfo.descent)
                                           (rref fi :fontInfo.leading)
                                           SpaceBetweenLines)))
                    (when (<= wy item-bottom) (return theIndex))
                    (when (>= item-bottom qd-rect-bottom) (return))
                    (rset qd-rect :rect.top item-bottom))
                  )
                ;; if tracking, check the y position. If above the top, get the item before scroll.
                ;; if not get the scroll + size.
                (if (<= wy (rref qd-rect :rect.top))
                  (max 0 (1- scroll))
                  (min (1- (length (items me))) (+ scroll (max 1 (size me :how 'items)))))))
        
        ValueToReturn
        ))))

;;;----Because each line may be a different size, we have to calculate this by adding up lines...

(define-handler size (styledPicker &key (how nil) physical)
  (if (and (neq how 'items) (neq how 'partialitems))
    (call-next-method me :physical physical :how how)
    (gs:with-temporary-port
      (gs:let+ ((theCount 0)
                (allStrings (strings me))
                (numstrings (length allstrings))
                (theItems (items me))
                (scroll (verticalScroll me))
                (SpaceBetweenLines (linespacing me))
                (qd-rect (:rect))
                qd-rect-bottom
                item-bottom)
        (gs:recompute-fill-region me (gs:node-flags me))
        (let ((fillRect (gs:hrref (gs:node-fillRegion me) :region.rgnbbox :storage :pointer)))
          (rset qd-rect :rect.top (rref fillRect :rect.top))
          (rset qd-rect :rect.left (rref fillRect :rect.left))
          (rset qd-rect :rect.bottom (rref fillRect :rect.bottom))
          (rset qd-rect :rect.right (rref fillRect :rect.right)))
        (setq qd-rect-bottom (rref qd-rect :rect.bottom)
              item-bottom (rref qd-rect :rect.top))
        (do  ((theIndex scroll (1+ theIndex))
              (curitem (nthcdr scroll theItems) (rest curitem)))
             ((>= theIndex numstrings) nil)
          (SetQuickDrawFontStuff me (first curitem) theindex)
          (rlet ((fi :fontInfo))
            (#_GetFontInfo fi)
            (incf item-bottom (+ (rref fi :fontInfo.ascent)
                                 (rref fi :fontInfo.descent)
                                 (rref fi :fontInfo.leading)
                                 SpaceBetweenLines)))
          (when (>= item-bottom qd-rect-bottom) 
            (when (and (eq how 'partialitems) (not (= item-bottom qd-rect-bottom)))
              (incf thecount))
            (return))
          (rset qd-rect :rect.top item-bottom)
          (incf theCount))
        thecount))))

(define-handler itemBoundsRect (styledPicker index &key physical)
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
    (gs:with-temporary-port
      (let* ((theItems (items me))
             (scroll (verticalScroll me))
             (numstrings (+ scroll (size me :how 'partialitems)))
             (hh (truncate (car (framesize me))))
             (vv (truncate (cadr (framesize me))))
             (SpaceBetweenLines (linespacing me))
             item-bottom
             item-top)
        (setq item-bottom 0)
        (do ((theIndex scroll (1+ theIndex))
             (curitem (nthcdr scroll theItems) (rest curitem)))
            ((>= theIndex numstrings) nil)
          (setf item-top item-bottom)
          (SetQuickDrawFontStuff me (first curitem) theindex)
          (rlet ((fi :fontInfo))
            (#_GetFontInfo fi)
            (incf item-bottom (+ (rref fi :fontInfo.ascent)
                                 (rref fi :fontInfo.descent)
                                 (rref fi :fontInfo.leading)
                                 SpaceBetweenLines)))
          (if (eql theIndex index) 
            (return))
          )
        (sk8-multival-bind (ll tt rr bb) (fillboundsrect me :physical physical)
          (declare (ignore bb))
          (if physical
            (sk8-multivals (+ ll hh) (+ item-top tt vv) (- rr hh) (+ item-bottom tt))
            (sk8-multivals (+ ll hh) (+ item-top vv) (- rr hh) item-bottom)))
        )))))


#|
	Change History (most recent last):
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
