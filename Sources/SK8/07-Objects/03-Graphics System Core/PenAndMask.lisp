(in-package :sk8dev)
(provide "PENANDMASK")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; __________________
;;; The Mask
;;; __________________

(new Object
     :objectName "Mask"
     :project sk8
     :undisposable t)

(define-handler new (Mask &rest args)
  ;; What do we do with objectname?
  (or (getf args :init-region) (T_NewRgnGC)))

(defun mask-to-qd-region (aMask)
  aMask)

(defun qd-region-to-mask (rgn)
  (new mask :init-region rgn))

;;; Informational.
;;; __________

(define-sk8-function maskBoundsRect nil (aMask)
  (let (ll tt rr bb)
    (rlet ((bounds :rect))
      (gs:region-into-rect aMask bounds)
      (setf ll (rref bounds :rect.left)
            tt (rref bounds :rect.top)
            rr (rref bounds :rect.right)
            bb (rref bounds :rect.bottom)))
    (sk8-multivals ll tt rr bb)))

(define-sk8-function maskLeft nil (aMask)
  (sk8-multival-bind (ll tt rr bb) (maskBoundsRect aMask)
    (declare (ignore tt rr bb))
    ll))

(define-sk8-function maskTop nil (aMask)
  (sk8-multival-bind (ll tt rr bb) (maskBoundsRect aMask)
    (declare (ignore ll rr bb))
    tt))

(define-sk8-function maskRight nil (aMask)
  (sk8-multival-bind (ll tt rr bb) (maskBoundsRect aMask)
    (declare (ignore ll tt bb))
    rr))

(define-sk8-function maskBottom nil (aMask)
  (sk8-multival-bind (ll tt rr bb) (maskBoundsRect aMask)
    (declare (ignore ll tt rr))
    bb))

(define-sk8-function maskEqual nil (maskA maskB)
  (#_equalRgn maskA maskB))

(define-sk8-function maskEmpty nil (aMask)
  (#_emptyRgn aMask))
  

;;; Altering handlers.
;;; _____________

(define-sk8-function rectToMask nil (left top right bottom aMask)
  (let ((region (mask-to-qd-region aMask)))
    (#_SetRectRgn region left top right bottom)))

(define-sk8-function maskCopy nil (src dst)
  (#_CopyRgn (mask-to-qd-region src) (mask-to-qd-region dst)))

(define-sk8-function maskInset nil (theMask h v)
  (#_InsetRgn (mask-to-qd-region theMask) h v))

(define-sk8-function maskDifference nil (srca srcb dest)
  (#_diffRgn (mask-to-qd-region srca) (mask-to-qd-region srcb) 
   (mask-to-qd-region dest)))

(define-sk8-function maskIntersect nil (srca srcb dest)
  (#_sectRgn (mask-to-qd-region srca) (mask-to-qd-region srcb) 
   (mask-to-qd-region dest)))

(define-sk8-function maskUnion nil (srca srcb dest)
  (#_UnionRgn (mask-to-qd-region srca) (mask-to-qd-region srcb) 
   (mask-to-qd-region dest)))

(define-sk8-function currentClipToMask nil (theMask thePaper)
  (declare (ignore thePaper))
  (#_getClip (mask-to-qd-region theMask)))

;;; Region Recording.
;;; _____________

(defvar *making-mask* nil)

(define-sk8-function StartNewMask nil (thePen)
  (declare (ignore thePen))
  (cond
   (*making-mask* 
    (sk8-error GeneralProgrammaticError 
               :strings '("FinishNewMask must be called be for another new mask can be started")))
   (t (#_OpenRgn)
      (setq *making-mask* t))))

(define-sk8-function FinishNewMask nil (dstMask thePen)
  (declare (ignore thePen))
  (cond
   (*making-mask*
    (unless dstMask
      (setq dstMask (new mask)))
    (let ((rgn (mask-to-qd-region dstMask)))
      (#_CloseRgn rgn))
    (setq *making-mask* nil)
    dstMask)
   (t (sk8-error GeneralProgrammaticError
                 :strings '("StartNewMask must be called before FinishNewMask can be called")))))

;;; This is one of those dangerous functions that have no meaning (at least not much!) outside the
;;; draw loop...

(define-sk8-function clipToMask nil (theMask thePaper)
  (declare (ignore thePaper))
  (#_setClip theMask))

;;; WithClippedMask -- sets the clip to the mask and executes the body. Sets *clip* if we are
;;;                within the draw loop.


(defmacro |WITH CLIPPEDMASK| ((theMask thePaper) &body body)
  (declare (ignore thePaper))
  `(gs:with-clipped-region ,theMask
     ,@body))

;;; __________________
;;; The Pen
;;; __________________

(new object
       :objectName "Pen"
       :project sk8
       :undisposable t
       :prototype t
       :properties '((h :value 0)             ;; The position of the pen.
                     (v :value 0)
                     (width :value 1)         ;; The size of the pen.
                     (height :value 1)
                     (down :value t)          ;; Whether the pen is down.
                     (ink :value nil)         ;; The color of the pen.
                     (Mode :value srccopy)   ;; Penmode.
                     (pattern :value nil)     ;; Pen pattern.
                     (textFont :value nil)
                     (textSize :value 12)
                     (textStyle :value nil)
                     ))

(setf (textStyle pen) 0) ;; plain.
(setf (textFont pen) ChicagoFont)  ;; Chicago.
(setf (textSize pen) 12)

(setf (ink pen) black)

;;; How pens work. A variable, *the-pen* holds the current system pen. To use a certain pen,
;;; you make it be the pen of the system. Then anything you do to it sets the pen (in the QD sense).
;;; Setting the pen to nil will restore the QD pen to its initial state.  The safest way to use the
;;; pen is to use the "with pen" form which makes sure the state is left in a correct form when
;;; you are done. The user can make their own pens in which they save state or use the default
;;; pen (the pen object) to do their stuff.
;;;
;;; Note that mode is one of the usual QD things. Pattern is an imageRenderer whose media must
;;; be a colorPattern or a BWPattern.

;;; Low level pen stuff.
;;; _______________

(defvar *the-pen* pen)

(defvar *sk8-pen-modes* 
  `((srcCopy . 0) (srcOr . 1) (srcXor . 2) (srcBic . 3)
    (notSrcCopy . 4) (notSrcOr . 5) (notSrcXor . 6)
    (notSrcBic . 7) (patCopy . 8) (patOr . 9) (patXor . 10)
    (patBic . 11) (notPatCopy . 12) (notPatOr . 13)
    (notPatXor . 14) (notPatBic . 15) (grayishTextOr . 49)
    (blend . 32) (addPin . 33) (addOver . 34)
    (subPin . 35) (addMax . 37) (adMax . 37)
    (subOver . 38) (adMin . 39) (ditherCopy . 64)
    (transparent . 36) (highlight . 50)))


;;; Translates a penmode keyword to its integer equivalent that #_penmode understands.
;;; Returns 0 if nothing is found since an error would be undesirable when trying to set
;;; the pen mode.

(defun mode-arg (modeKeyword)
  (or (cdr (assq modeKeyword *sk8-pen-modes*)) 0))
       
;;; Setting the pen. Assumes this is being done within a call to with-port or
;;; strange things will happen. This should be called from the definition of a
;;; render method or a draw function!

(define-handler install (pen &key thePaper)
  (declare (ignore thePaper))
  ;; Install the pen in our variable.
  (setf *the-pen* me)
  ;; Initialize the system pen.
  (#_MoveTo (h me) (v me))
  (#_PenSize (width me) (height me))
  (if (down me)
    (#_showPen)
    (#_hidePen))
  (#_PenMode (mode-arg (mode me)))
  (with-rgb (theColor (mcl-color (ink me)))
    (#_RGBForeColor theColor))
  ;; Install the pattern. Different thing according to type of media.
  (let ((thePat (pattern me)))
    (when thePat
      (if (inheritsFrom thePat colorPattern)
        (#_penPixPat (loadMedia thePat))
        (#_penPat (loadMedia thePat)))))
  ;; TextFont stuff.
  (#_textFont (fontData (slot-value me 'textFont)))
  (#_textFace (slot-value me 'textStyle))
  (#_textSize (slot-value me 'textSize)))

;;; This handler resets the pen to pen and calls penNormal.

(define-handler deinstall (pen)
  ;; Undo all damage.
  (#_penNormal)
  (#_BackColor #$Whitecolor)
  (gs:restore-text-state)
  ;; Set the variable.
  (setf *the-pen* pen))
 
(define-handler installed (pen)
  (eq me *the-pen*))

(defmacro pen-installed (thePen)
  `(eq ,thePen *the-pen*))

;;; This macro uses your pen to do all sorts of damage.
  
(defmacro withPen (thePen thePaper &body body)
  `(unwind-protect
     (progn
       (install ,thePen :thePaper ,thePaper)
       ,@body)
     (deinstall ,thePen)))

(defmacro |WITH PEN| ((thePen thePaper) &body body)
  `(withPen ,thePen ,thePaper
            ,@body))

;;; Pen Handlers.
;;; __________

(define-handler location (pen)
  (sk8-multivals (h me) (v me)))

(define-handler (setf location) (newLoc pen)
  (cond
   ((inheritsFrom newLoc pointList)
    (setf (slot-value me 'h) (car newLoc))
    (setf (slot-value me 'v) (cadr newLoc))
    (when (pen-installed me)
      (#_moveTo (car newLoc) (cadr newLoc))))
   (t (sk8-error PropertyTypeMismatchError
                 :object        newLoc
                 :expectedType  PointList
                 :ownerObject   Pen
                 :propertyName 'location
                 ))))

(define-handler (setf h) (newH pen)
  (cond
   ((integerp newH)
    (setf (slot-value pen 'h) newH)
    (when (pen-installed me)
      (#_moveTo newH (v me))))
   (t (sk8-error PropertyTypeMismatchError
                 :object        newH
                 :expectedType  Integer
                 :ownerObject   Pen
                 :propertyName 'h
                 ))))

(define-handler (setf v) (newV pen)
  (cond
   ((integerp newV)
    (setf (slot-value pen 'v) newV)
    (when (pen-installed me)
      (#_moveTo (h me) newV)))
   (t (sk8-error PropertyTypeMismatchError
                 :object        newV
                 :expectedType  Integer
                 :ownerObject   Pen
                 :propertyName 'v
                 ))))

(define-handler size (pen)
  (sk8-multivals (width me) (height me)))

(define-handler (setf size) (newSize pen)
  (cond
   ((inheritsFrom newSize PointList)
    (setf (slot-value me 'width) (car newSize))
    (setf (slot-value me 'height) (cadr newSize))
    (when (pen-installed me)
      (#_penSize (car newSize) (cadr newSize))))
   (t (sk8-error PropertyTypeMismatchError
                 :object        newSize
                 :expectedType  PointList
                 :ownerObject   Pen
                 :propertyName 'size
                 ))))

(define-handler (setf width) (newWidth pen)
  (cond
   ((inheritsFrom newWidth PositiveInteger)
    (setf (slot-value pen 'width) newWidth)
    (when (pen-installed me)
      (#_penSize newWidth (height me))))
   (t (sk8-error PropertyTypeMismatchError
                 :object        newWidth
                 :expectedType  PositiveInteger
                 :ownerObject   Pen
                 :propertyName 'width
                 ))))

(define-handler (setf height) (newHeight pen)
  (cond
   ((inheritsFrom newHeight PositiveInteger)
    (setf (slot-value pen 'height) newHeight)
    (when (pen-installed me)
      (#_penSize (width me) newHeight)))
   (t (sk8-error PropertyTypeMismatchError
                 :object        newHeight
                 :expectedType  PositiveInteger
                 :ownerObject   Pen
                 :propertyName 'height
                 ))))

(define-handler (setf ink) (newInk pen)
  (cond
   ((inheritsFrom newInk RGBColor)
    (setf (slot-value me 'ink) newInk)
    (when (pen-installed me)
      (with-rgb (theColor (mcl-color newInk))
        (#_RGBForeColor theColor))))
   (t (sk8-error PropertyTypeMismatchError
                 :object        newInk
                 :expectedType  RGBColor
                 :ownerObject   Pen
                 :propertyName 'ink
                 ))))

(define-handler (setf down) (newDown pen)
  (setf (slot-value me 'down) newDown)
  (when (pen-installed me)
    (if newDown
      (#_showPen)
      (#_hidePen))))

(define-handler (setf mode) (newMode pen)
  (setf (slot-value me 'mode) newmode)
  (when (pen-installed me)
    (#_penMode (mode-arg newMode)))
  newMode)

;;; Takes a font object.

(define-handler (setf textFont) (theFont pen)
  (setf (slot-value me 'textFont) theFont)
  (when (pen-installed me)
    (#_textFont (fontData theFont))
  theFont))

(define-handler (setf textSize) (newSize pen)
  (setf newSize (gs:f.round newSize))
  (setf (slot-value me 'textSize) newSize)
  (when (pen-installed me)
    (#_textSize newSize))
  newSize)

;;; Just like the actor method.

(define-handler textStyle (pen)
  (let ((style (slot-value me 'textstyle))
        (result nil))
    (dolist (num '(64 32 16 8 4 2 1))
      (when (>= style num)
        (push (num-to-style num) result)
        (decf style num)))
    (if (null result)
      '(plain)
      result)))

(define-handler (setf textStyle) (styleList pen)
  (let ((styleNum (gs:get-style-number styleList)))
    (setf (slot-value me 'textStyle) styleNum)
    (when (pen-installed me)
      (#_textFace styleNum))
    styleList))

;;; Ensures that the given pen object and any the underlying graphics
;;; state is reset to a standard consistent state. This should ALWAYS be called
;;; when the user uses the system pen to do his drawing. (Note that when a
;;; custom pen is used, installing and deinstalling it takes care of it.)

(define-handler resetPen (pen)
  ;; Leave system pen in the right shape.
  (#_PenNormal)
  (#_moveTo 0 0)
  (#_penSize 1 1)
  ;; Location.
  (setf (h me) 0)
  (setf (v me) 0)
  ;; Size.
  (setf (width me) 1)
  (setf (height me) 1)
  ;; Down.
  (setf (down me) t)
  ;; Color.
  (setf (ink me) black)
  (#_BackColor #$Whitecolor)
  (#_foreColor #$BlackColor)
  ;; Text stuff.
  (setf (slot-value me 'textFont) ChicagoFont)
  (setf (slot-value me 'textStyle) 0)
  (setf (slot-value me 'textSize) 12)
  (gs:restore-text-state))
  
;;; Moves the pen to h v. If the pen is the current pen it tries to draw.

(define-handler LineTo (pen h v)
  ;; Set the slots.
  (setf (slot-value pen 'h) h)
  (setf (slot-value pen 'v) v)
  (when (pen-installed me)
    (#_lineTo h v)))

(define-sk8-function invertMask nil (theMask)
  (#_invertRgn theMask))

;;; Paints the region with the pen's color.

(define-handler paintMask (pen theMask)
  (with-rgb (theColor (mcl-color (ink me)))
    (#_RGBForeColor theColor))
  (#_PaintRgn (mask-to-qd-region theMask))
  ;; Clean up.
  (#_ForeColor #$BlackColor))

(define-handler frameMask (pen theMask)
  (with-rgb (theColor (mcl-color (ink me)))
    (#_RGBForeColor theColor))
  (#_FrameRgn (mask-to-qd-region theMask))
  ;; Clean up.
  (#_ForeColor #$BlackColor))

(define-handler paintRect (pen theRect)
  (maybe-recover-list theRect)
  (sk8-destructuring-bind (left top right bottom) theRect
    (with-rgb (theColor (mcl-color (ink me)))
      (#_RGBForeColor theColor))
    (rlet ((qdRect :rect :left left :top top :right right :bottom bottom))
      (#_PaintRect qdRect))
    (#_ForeColor #$BlackColor)))

(define-handler frameRect (pen theRect)
  (maybe-recover-list theRect)
  (sk8-destructuring-bind (left top right bottom) theRect
    (with-rgb (theColor (mcl-color (ink me)))
      (#_RGBForeColor theColor))
    (rlet ((qdRect :rect :left left :top top :right right :bottom bottom))
      (#_FrameRect qdRect))
    (#_ForeColor #$BlackColor)))

(define-handler paintOval (pen theRect)
  (maybe-recover-list theRect)
  (sk8-destructuring-bind (left top right bottom) theRect
    (with-rgb (theColor (mcl-color (ink me)))
      (#_RGBForeColor theColor))
    (rlet ((qdRect :rect :left left :top top :right right :bottom bottom))
      (#_PaintOval qdRect))
    (#_ForeColor #$BlackColor)))

(define-handler frameOval (pen theRect)
  (maybe-recover-list theRect)
  (sk8-destructuring-bind (left top right bottom) theRect
    (with-rgb (theColor (mcl-color (ink me)))
      (#_RGBForeColor theColor))
    (rlet ((qdRect :rect :left left :top top :right right :bottom bottom))
      (#_FrameOval qdRect))
    (#_ForeColor #$BlackColor)))

;;; Assumes the pen is in the right location and just draws the string.

(define-handler drawString (pen theString)
  (when (stringp theString)
    (with-cstrs ((cstr thestring))
      (#_DrawText cstr 0 (length thestring)))))

;;; returns the components of a font. 

(define-handler fontInfo (Pen)
  (gs:let+ ((fi (:fontInfo))
         (thePort (:cGrafport)))
    (if (installed me)
      (progn 
        (#_getFontInfo fi)
        (sk8-multivals (rref fi :fontInfo.ascent)
                       (rref fi :fontInfo.descent) 
                       (rref fi :fontInfo.leading)))
      (without-interrupts 
       (with-port thePort
         ;; Set text attributes.
         (#_textFont (slot-value me 'textFont))
         (#_textFace (slot-value me 'textStyle))
         (#_textSize (slot-value me 'textSize))
         (#_getFontInfo fi)
         (sk8-multivals (rref fi :fontInfo.ascent) 
                        (rref fi :fontInfo.descent)
                        (rref fi :fontInfo.leading)))))))

(define-handler stringSize (pen theString)
  (gs:let+ ((fi (:fontInfo))
         (thePort (:cGrafport))
         x y)
    (if (installed me)
      (progn 
        (#_getFontInfo fi)
        (setf y (+ (rref fi :fontInfo.ascent) (rref fi :fontInfo.descent) (rref fi :fontInfo.leading)))
        (with-pstrs ((pstr theString))
          (setf x (#_stringWidth pstr))))
      (without-interrupts 
       (with-port thePort
         ;; Set text attributes.
         (#_textFont (fontData (slot-value me 'textFont)))
         (#_textFace (slot-value me 'textStyle))
         (#_textSize (slot-value me 'textSize))
         (#_getFontInfo fi)
         (setf y (+ (rref fi :fontInfo.ascent) (rref fi :fontInfo.descent) (rref fi :fontInfo.leading)))
         ;; And now the width of the string.
         (with-pstrs ((pstr theString))
           (setf x (#_stringWidth pstr))))))
    (sk8-multivals x y)))
    
#|
	Change History (most recent last):
	1		1/10/94	hernan	New file. Implements as much of the imaging 
							system as we are going to get.
	2		1/10/94	hernan	Fonts just became objects!!!
	3		1/10/94	hernan	Ooops. Fixing left over junk.
	4		1/11/94	hernan	self -> me
	5		1/17/94	hernan	Removing unnecessary setting of the pen when
							framing ovals and others.
	6		2/12/94	kleiman	name changes
	7		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	8		2/25/94	hernan	Using symbols instead of keywords for options!!!
	9		2/28/94	hernan	Now using GCable mac pointers and handles!
	10		3/4/94	kleiman	font-num -> fontData
	11		3/9/94	Hernan	Getting rid of leftover keywords.
	12		4/20/94	Hernan	Replacing calls to get-record-field with calls to
							region-into-rect (which does not cons).
	13		4/25/94	Hernan	*pen-modes* -> *sk8-pen-modes* to avoid 
							conflicts with MCL's version.
	14		4/26/94	Hernan	Adding a function to measure the size of a 
							string.
	15 	 8/23/94	Hernan  	set penmode -> set mode.
	16 	 8/23/94	Hernan  	Texfont should return a font object.
	17 	 8/31/94	Hernan  	Chicago -> ChicagoFont.
	18 	 9/12/94	Hernan  	1180502: capitalizing object names.
	19 	 9/29/94	chip    	marked Mask as not-simple-instantiable
	20 	10/ 3/94	Hernan  	machandle -> loadMedia. Also added paper and pen
							as args to a few methods.
	21 	10/ 6/94	chip    	took out explicit marking of Mask as not-simple-instantiable (define-handler on new does it now)
	22 	11/ 5/94	Hernan  	Adding maskEqual and maskEmpty.
	23 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	24 	 3/28/95	Hernan  	1233252: removing extraneous 10 from stringSize.
	2  	 6/23/95	Hernan  	Install of Pen no longer messes up the fonts.
	3  	 7/17/95	Hernan  	Adding invertMask.
	4  	 7/20/95	Hernan  	Adding maskUnion (which we seem to have forgotten).
	5  	 7/31/95	Hernan  	Added fontInfo.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	5  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	6  	 5/ 1/96	Hernan  	stringSize no longer subtracts gratuitous 10 pixels.
	7  	12/17/96	Hernan  	StringSize of pen needs to get the fontData of the font
						before calling #_textFace.
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
