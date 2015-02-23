;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "ICONTEXTPICKER")

(require "PICKER" "objects;Pickers:Picker")

;;; ________________________________________________________
;;; ________________________________________________________
;;; The Icon Text Picker
;;; ________________________________________________________
;;; ________________________________________________________

(new picker :objectName "IconTextPicker"
     :properties '((theOffsets :value nil)
                   (arrowSize :value nil)
                   (theArrows :value nil)
                   (iconSize :value nil)
                   (theIcons :value nil))
     ; :undisposable t
     :prototype t
     :project sk8)

(setf (private IconTextPicker :property 'theOffsets) t)
(setf (private IconTextPicker :property 'theArrows) t)
(setf (private IconTextPicker :property 'theIcons) t)
(addproperty IconTextPicker 'textColors)
(addproperty IconTextPicker 'createTextColors)

(define-handler initialize (IconTextPicker original isNew initArgs)
  (declare (ignore isNew initArgs))
  (call-next-method)
  (setf original (originalAncestor me original IconTextPicker))
  (setf (iconsize me) (iconsize original)))

;;; Returns the renderer!

(define-handler createIcon (IconTextPicker theItem theString thePosition)
  (declare (ignore theItem theString thePosition))
  nil)

(define-handler createArrow (IconTextPicker theItem theString thePosition)
  (declare (ignore theItem theString thePosition))
  nil)

(define-handler createOffset (IconTextPicker theItem theString thePosition)
  (declare (ignore theItem theString thePosition))
  0)

(define-handler CreateTextColor (IconTextPicker itemLocation)
  (declare (ignore itemLocation))
  nil)

(defun calc-IconTextPicker-LineHeight (thepicker)
  (let ((the-line-height (get-lineHeight thepicker))
        (arrowheight (or (cadr (arrowsize thepicker)) 0))
        (iconheight (or (cadr (iconSize thepicker)) 0))
        (linespacing (linespacing thepicker))
        )
    (+ linespacing (max the-line-height arrowheight iconheight))))

(defun icon-text-picker-draw (me thePort draw-Region)
  (gs:let+ ((flags (gs:node-flags me))
            (iconSize (iconSize me))
            (arrowsize (arrowsize me))
            (highlightcolor (highlightcolor me))
            (scroll (verticalScroll me))
            (theselection (selection* me))
            (theStrings (strings me))
            (qd-rect (:rect))
            (draw-rect (:rect))
            (draw-rgn (:region))
            LineHeight HalfLineHeight fontascent
            
            (iconHeight (cadr iconSize))
            (HalficonHeight (and iconheight (truncate iconHeight 2)))
            (theIcons (and iconHeight (theIcons me)))
            
            (arrowheight (cadr arrowsize))
            (HalfArrowHeight (and arrowheight (truncate arrowheight 2)))
            (theArrows (and arrowheight (theArrows me)))
            (default-text-color (sk8::mcl-color (textcolor me)))
            (thetextcolors (textcolors me))
            
            (theOffsets (theOffsets me))
            qd-rect-bottom qd-rect-top item-top item-bottom item-left theicon theArrow
            (theSize (length theStrings))
            (clip-region (:region))
            (fill (gs:node-fillRegion me)))
    (gs:with-clipped-region draw-region
      ;; Render the fill and the frame
      (gs:recompute-fill-region me flags)
      (gs:recompute-frame-region me flags)
      (gs:with-composed-clip fill
        (render (gs:node-fillcolor me) me (gs:node-fillRegion me) thePort)
        (when (items me)
          ;; Clip to the fill and set the pen colors.
          (with-rgb (rgb (mcl-color (textColor me)))
            (#_RGBForeColor rgb))
          (with-rgb (rgb (mcl-color (gs:node-fillcolor me)))
            (#_RGBBackColor rgb))
          (setf LineHeight (calc-IconTextPicker-LineHeight me))
          ;; Set up the pen.
          (gs:set-text-state me)          
          (setf HalfLineHeight (truncate LineHeight 2))
          (rlet ((fi :fontInfo))
            (#_GetFontInfo fi)
            (setf fontascent (round (+ (rref fi :fontInfo.ascent)
                                       (rref fi :fontInfo.descent)
                                       (rref fi :fontInfo.leading))
                                    2)))
          
          ;; Set up the rects and variables for the loop.
          (let ((fillRect (gs:hrref (gs:node-fillRegion me) :region.rgnbbox :storage :pointer)))
            (rset qd-rect :rect.top (rref fillRect :rect.top))
            (rset qd-rect :rect.left (rref fillRect :rect.left))
            (rset qd-rect :rect.bottom (rref fillRect :rect.bottom))
            (rset qd-rect :rect.right (rref fillRect :rect.right)))
          (setq qd-rect-top (rref qd-rect :rect.top)
                qd-rect-bottom (rref qd-rect :rect.bottom)
                item-top qd-rect-top)
          ;; Draw the items
          (do ((theIndex scroll (1+ theIndex)))
              ((= theIndex thesize) nil)
            (setf item-left (+ 5 (rref qd-rect :rect.left)))
            (setf item-bottom (+ item-top LineHeight))
            
            (when theArrows
              (setf theArrow (nth theIndex theArrows))
              (when thearrow
                (rset draw-rect :rect.top (+ item-top (- HalfLineHeight HalfArrowHeight)))
                (rset draw-rect :rect.bottom (+ (rref draw-rect :rect.top) ArrowHeight))
                (rset draw-rect :rect.left item-left)
                (rset draw-rect :rect.right (+ item-left (car ArrowSize)))
                (#_rectRgn draw-rgn draw-rect)
                (render thearrow me draw-rgn thePort))
              (incf item-left (car ArrowSize)))
            
            (incf item-left (nth theindex theoffsets))
            
            (when theIcons
              (setf theIcon (nth theIndex theIcons))
              (when theIcon
                (rset draw-rect :rect.top (+ item-top (- HalfLineHeight HalfIconHeight)))
                (rset draw-rect :rect.bottom (+ (rref draw-rect :rect.top) IconHeight))
                (rset draw-rect :rect.left item-left)
                (rset draw-rect :rect.right (+ item-left (car iconSize)))
                (#_rectRgn draw-rgn draw-rect)
                (render theicon me draw-rgn thePort)
                (incf item-left (car iconsize))
                (incf item-left 5)))
            
            (if (and thetextcolors (nth theindex thetextcolors)) 
              (with-rgb (rgb (sk8::mcl-color (nth theindex thetextcolors)))
                (#_RGBForeColor rgb))
              (with-rgb (rgb default-text-color)
                (#_RGBForeColor rgb)))
            (rset draw-rect :rect.top (- (+ item-top halflineheight) fontascent))
            (rset draw-rect :rect.bottom (+ item-top LineHeight))
            (rset draw-rect :rect.left item-left)
            (rset draw-rect :rect.right (rref qd-rect :rect.right))
            (draw-text-not-wrapped (nth theIndex theStrings) draw-rect)            
            
            ;; Hilite if selected.
            (gs:with-hilitecolor theport highlightcolor
              (when (memq theIndex theSelection)
                (rset draw-rect :rect.top item-top)
                (rset draw-rect :rect.bottom (+ item-top lineheight))
                (rset draw-rect :rect.left (rref qd-rect :rect.left))
                (rset draw-rect :rect.right (rref qd-rect :rect.right))
                (Set-Pen-For-Hilite)
                (#_PaintRect draw-rect)
                (#_PenMode #$srcCopy)))
            
            ;; Update the rect.
            (setq item-top item-bottom)
            (when (>= item-bottom qd-rect-bottom) (return))))
        ;; Draw the contents if any!
        (unless (gs:dlist-empty? (gs:node-contents me))
          (#_sectRgn fill draw-region clip-region)
          (gs:with-clipped-region clip-region
            (funcall (gs:node-contentsDrawFunction me) me thePort clip-region fill t))))
      ;; Draw the Frame!
      (render (gs:node-frameColor me) me (gs:node-frameRegion me) thePort)
      ;; invert if necessary.
      (when (and (gs:hilited? flags) (gs:inverted? flags))
        (#_InvertRect qd-rect)))))

(setf (gs:node-drawfunction IconTextPicker) 'icon-text-picker-draw)

(define-handler pointOnWhichPart (IconTextPicker x y &key part tracking)
  (unless (memq part '(item ItemAndPosition bounds rect fill frame))
    (SK8-error ArgumentTypeMismatchError
               :handlerName 'pointOnWhichPart :argumentName 'part
               :object part :expectedType '(item ItemAndPosition bounds rect fill frame)))
  (when (and (neq part 'item) (neq part 'ItemAndPosition)) 
    (return-from pointOnWhichPart (call-next-method me x y :part part)))
  (let (valuetoreturn)
    (gs:with-temporary-port
      (gs:let+ (totalline 
                (allStrings (strings me))
                (scroll (verticalScroll me)) position
                (qd-rect (:rect))
                )
        (setf totalline (calc-IconTextPicker-LineHeight me))
        (gs:recompute-fill-region me (gs:node-flags me))
        (let ((fillRect (gs:hrref (gs:node-fillRegion me) :region.rgnbbox :storage :pointer)))
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
              (setf valueToReturn (if (>= position (length allstrings)) nil position)))
            ;; if tracking, check the y position. If above the top, get the item before scroll.
            ;; if not get the scroll + size.
            (if (<= wy (rref qd-rect :rect.top))
              (setf valueToReturn (max 0
                                       (- scroll
                                          (ceiling (- (rref qd-rect :rect.top) wy ) (textsize me)))))
              (setf valueToReturn (min (1- (length (items me))) 
                                       (+ (ceiling (- wy (rref qd-rect :rect.bottom)) (textsize me))
                                          scroll
                                          (size me :how 'items)))))))))
    (when (and valuetoreturn (eq part 'ItemAndPosition))
      (let* ((wx (- x (left me :physical t)))
             (widtharrow (or (car (arrowsize me)) 0))
             (widthicon (or (car (iconsize me)) 0))
             (curitem (gs:range 0 valuetoreturn (max 0 (1- (length (items me))))))
             (offset (nth curitem (theoffsets me))))
        (cond 
         ((and (arrowsize me) 
               (nth curitem (thearrows me)) 
               (>= wx 5) (<= wx (+ 5 widthArrow)))
          (setf ValueToReturn (list ValueToReturn 'arrow)))
         ((and (iconsize me) 
               (nth curitem (theicons me)) 
               (>= wx (+ 5 widthArrow offset)) (<= wx (+ 5 Offset widthArrow widthIcon)))
          (setf ValueToReturn (list ValueToReturn 'icon)))
         (t
          (setf ValueToReturn (list ValueToReturn 'text))))))
    
    valuetoreturn))

(define-handler size (IconTextPicker &key (how nil) physical)
  (if (neq how 'items)
    (call-next-method me :physical physical)
    (let* (oneLine 
           (physRect (gs:node-physicalBoundsRect me)))
      (setf oneLine (calc-IconTextPicker-LineHeight me))
      (truncate (/ (- (gs:rect-bottom physRect) (gs:rect-top physRect)) oneLine)))))

(define-handler ItemBoundsRect (IconTextPicker index &key (physical nil))
  (let* ((item-height (calc-IconTextPicker-LineHeight me))
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

(define-handler SetupArrows :private (IconTextPicker)
                (when (arrowsize me)
                  (let ((theItems (items me))
                        (theStrings (strings me))
                        (thePos 0))
                    (setf (thearrows me) (mapcar #'(lambda (oneItem oneString)
                                                     (prog1 
                                                       (createArrow me oneItem oneString thePos)
                                                       (incf thePos)))
                                                 theItems theStrings)))))

(define-handler SetupIcons :private (IconTextPicker)
                (when (iconsize me)
                  (let ((theItems (items me))
                        (theStrings (strings me))
                        (thePos 0))
                    (setf (theIcons me) (mapcar #'(lambda (oneItem oneString)
                                                    (prog1 
                                                      (createIcon me oneItem oneString thePos)
                                                      (incf thePos)))
                                                theItems theStrings)))))

(define-handler SetupOffsets :private (IconTextPicker)
                (let ((theItems (items me))
                      (theStrings (strings me))
                      (thePos 0))
                  (setf (theOffsets me) (mapcar #'(lambda (oneItem oneString)
                                                    (prog1 
                                                      (createOffset me oneItem oneString thePos)
                                                      (incf thePos)))
                                                theItems theStrings))))

(define-handler SetUpTextColors (IconTextPicker)
  (if (createTextColors me)
    (let ((theItems (items me))
          (thePos 0))
      (setf (textcolors me) (mapcar #'(lambda (oneItem)
                                        (declare (ignore oneItem))
                                        (prog1 
                                          (createtextcolor me thePos)
                                          (incf thePos)))
                                    theItems)))
    (setf (textcolors me) nil)))

(define-handler (setf Items) (newItems IconTextPicker)
  (declare (ignore-if-unused newItems))
  (withActorLocked (me)
    (Call-next-method)
    (SetupArrows me)
    (SetupIcons me)
    (SetupOffsets me)
    (SetUpTextColors me)
    ))

#|
	Change History (most recent last):
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3  11/15/96Hernan  Initialize cannot assume that the "original" arg will have an
						object of the right type. Calling originalAncestor to deal with it.
|# ;(do not edit past this line!!)
