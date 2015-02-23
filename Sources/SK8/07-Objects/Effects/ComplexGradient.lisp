;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "COMPLEXGRADIENT")

(require "GRADIENT" "objects;Effects:Gradient")

;;______________________________________________________________________
;; COMPLEX GRADIENT

(new Gradient
     :objectName "ComplexGradient"
     :undisposable t
     :prototype t
     :properties '((penMode :value 50)
                   (startOpRed :value 0)
                   (startOpGreen :value 0)
                   (startOpBlue :value 0)
                   (endOpRed :value 0)
                   (endOpGreen :value 0)
                   (endOpBlue :value 0))
     :project sk8)

(define-handler (setf startOpRed) (newValue ComplexGradient)
  (setf (slot-value me 'startOpRed) (gs:f.round newValue)))

(define-handler (setf startOpGreen) (newValue ComplexGradient)
  (setf (slot-value me 'startOpGreen) (gs:f.round newValue)))

(define-handler (setf startOpBlue) (newValue ComplexGradient)
  (setf (slot-value me 'startOpBlue) (gs:f.round newValue)))

(define-handler (setf endOpRed) (newValue ComplexGradient)
  (setf (slot-value me 'endOpRed) (gs:f.round newValue)))

(define-handler (setf endOpGreen) (newValue ComplexGradient)
  (setf (slot-value me 'endOpGreen) (gs:f.round newValue)))

(define-handler (setf endOpBlue) (newValue ComplexGradient)
  (setf (slot-value me 'endOpBlue) (gs:f.round newValue)))

(define-handler translucent (complexGradient)
  (memq (gs:mode-arg (penMode me))
        (list #$Invert #$SrcOr #$srcXor #$srcBic
              #$notSrcOr #$notPatXor #$notPatBic #$blend
              #$addPin #$addOver #$adMin #$AdMax #$subPin
              #$subOver 50)))

(define-handler (setf penmode) (newValue ComplexGradient)
  (when (symbolp newvalue)
    (setf newvalue
          (case newvalue
            (INVERT #$INVERT)
            (SRCOR #$SRCOR)
            (SRCXOR #$SRCXOR)
            (SRCBIC #$SRCBIC)
            (NOTSRCOR #$NOTSRCOR)
            (NOTPATXOR #$NOTPATXOR)
            (NOTPATBIC #$NOTPATBIC)
            (BLEND #$BLEND)
            (ADDPIN #$ADDPIN)
            (ADDOVER #$ADDOVER)
            (ADMIN #$ADMIN)
            (ADMAX #$ADMAX)
            (SUBPIN #$SUBPIN)
            (SUBOVER #$SUBOVER)
            (otherwise 50))))
  (setvalue 'penmode me newvalue))


(define-handler render (complexGradient theActor region thePaper)
  (declare (ignore theActor thePaper))
  (gs:let+ ((trgn (:region))
            (currentClip (:region))
            (curColor (:rgbColor))
            (curOpColor (:rgbColor))
            (qd-rect (:rect)) 
            left top maxx maxy
            sleft stop sright sbottom xOffset yOffset
            redInc greenInc blueInc curRed curGreen curBlue
            redOpInc greenOpInc blueOpInc curOpRed curOpGreen curOpBlue)
    (gs:region-into-rect region qd-rect)
    (setf left (rref qd-rect :rect.left)
          top (rref qd-rect :rect.top)
          maxx (- (rref qd-rect :rect.right) left)
          maxy (- (rref qd-rect :rect.bottom) top))
    (when (not (zerop (* maxx maxy)))
      ;; calc minimum fill area
      (#_getClip currentClip)
      (#_SectRgn currentClip region trgn)  
      (gs:with-clipped-region trgn
        (#_PenMode (gs:mode-arg (penMode me)))
        (#_PenPat *black-pattern*)
        (gs:region-into-rect trgn qd-rect)
        (setq sleft (rref qd-rect :rect.left)
              stop (rref qd-rect :rect.top)
              sright (rref qd-rect :rect.right)
              sbottom (rref qd-rect :rect.bottom)
              xOffset (- sLeft left)
              yOffset (- sTop top))
        ;; which way to fill?
        (case (direction me)
          ;; horizontal fill
          (horizontal
           (setq redInc (round (- (endRed me) (startRed me)) maxx)
                 greenInc (round (- (endGreen me) (startGreen me)) maxx)
                 blueInc (round (- (endBlue me) (startBlue me)) maxx)
                 curRed (round (+ (startRed me) (* redInc xOffset)))
                 curGreen (round (+ (startGreen me) (* greenInc xOffset)))
                 curBlue (round (+ (startBlue me) (* blueInc xOffset)))
                 redOpInc (round (- (endOpRed me) (startOpRed me)) maxx)
                 greenOpInc (round (- (endOpGreen me) (startOpGreen me)) maxx)
                 blueOpInc (round (- (endOpBlue me) (startOpBlue me)) maxx)
                 curOpRed (round (+ (startOpRed me) (* redOpInc xOffset)))
                 curOpGreen (round (+ (startOpGreen me) (* greenOpInc xOffset)))
                 curOpBlue (round (+ (startOpBlue me) (* blueOpInc xOffset))))
           (dotimes (i (- sright sleft))
             (rset curColor :rgbColor.red curRed)
             (rset curColor :rgbColor.green curGreen)
             (rset curColor :rgbColor.blue curBlue)
             (rset curOpColor :rgbColor.red curOpRed)
             (rset curOpColor :rgbColor.green curOpGreen)
             (rset curOpColor :rgbColor.blue curOpBlue)
             (#_RGBForeColor curColor)
             (#_OpColor curOpColor)
             (#_MoveTo (+ sleft i) stop)
             (#_LineTo (+ sleft i) sbottom)
             (setq curRed (+ curRed redInc)
                   curGreen (+ curGreen greenInc)
                   curBlue (+ curBlue blueInc)
                   curOpRed (+ curOpRed redOpInc)
                   curOpGreen (+ curOpGreen greenOpInc)
                   curOpBlue (+ curOpBlue blueOpInc))))
          ;; vertical fill
          (vertical
           (setq redInc (round (- (endRed me) (startRed me)) maxy)
                 greenInc (round (- (endGreen me) (startGreen me)) maxy)
                 blueInc (round (- (endBlue me) (startBlue me)) maxy)
                 curRed (round (+ (startRed me) (* redInc yOffset)))
                 curGreen (round (+ (startGreen me) (* greenInc yOffset)))
                 curBlue (round (+ (startBlue me) (* blueInc yOffset)))
                 redOpInc (round (- (endOpRed me) (startOpRed me)) maxy)
                 greenOpInc (round (- (endOpGreen me) (startOpGreen me)) maxy)
                 blueOpInc (round (- (endOpBlue me) (startOpBlue me)) maxy)
                 curOpRed (round (+ (startOpRed me) (* redOpInc yOffset)))
                 curOpGreen (round (+ (startOpGreen me) (* greenOpInc yOffset)))
                 curOpBlue (round (+ (startOpBlue me) (* blueOpInc yOffset))))
           (dotimes (i (- sbottom stop))
             (rset curColor :rgbColor.red curRed)
             (rset curColor :rgbColor.green curGreen)
             (rset curColor :rgbColor.blue curBlue)
             (rset curOpColor :rgbColor.red curOpRed)
             (rset curOpColor :rgbColor.green curOpGreen)
             (rset curOpColor :rgbColor.blue curOpBlue)
             (#_RGBForeColor curColor)
             (#_OpColor curOpColor)
             (#_MoveTo sLeft (+ sTop i))
             (#_LineTo sRight (+ sTop i))
             (setq curRed (+ curRed redInc)
                   curGreen (+ curGreen greenInc)
                   curBlue (+ curBlue blueInc)
                   curOpRed (+ curOpRed redOpInc)
                   curOpGreen (+ curOpGreen greenOpInc)
                   curOpBlue (+ curOpBlue blueOpInc))))
          ;; shapeBurst
          (shape
           (gs:let+ ((nRgn (:region))
                     (scale (scaleFactor me)))
             (setq curRed (startRed me)
                   curGreen (startGreen me)
                   curBlue (startBlue me)
                   redInc (round (- (endRed me) curRed) (round (max maxx maxy) scale))
                   greenInc (round (- (endGreen me) curGreen) (round (max maxx maxy) scale))
                   blueInc (round (- (endBlue me) curBlue) (round (max maxx maxy) scale))
                   curOpRed (startOpRed me)
                   curOpGreen (startOpGreen me)
                   curOpBlue (startOpBlue me)
                   redOpInc (round (- (endOpRed me)  curOpRed) (round (max maxx maxy) scale))
                   greenOpInc (round (- (endOpGreen me) curOpGreen) (round (max maxx maxy) scale))
                   blueOpInc (round (- (endOpBlue me) curOpBlue) (round (max maxx maxy) scale)))
             (#_CopyRgn region nRgn)
             (dotimes (i (round (max maxx maxy) scale))
               (rset curColor :rgbColor.red curRed)
               (rset curColor :rgbColor.green curGreen)
               (rset curColor :rgbColor.blue curBlue)
               (rset curOpColor :rgbColor.red curOpRed)
               (rset curOpColor :rgbColor.green curOpGreen)
               (rset curOpColor :rgbColor.blue curOpBlue)
               (#_RGBForeColor curColor)
               (#_OpColor curOpColor)
               (#_frameRgn nRgn)
               (#_InsetRgn nRgn 1 1)
               (setq curRed (+ curRed redInc)
                     curGreen (+ curGreen greenInc)
                     curBlue (+ curBlue blueInc)
                     curOpRed (+ curOpRed redOpInc)
                     curOpGreen (+ curOpGreen greenOpInc)
                     curOpBlue (+ curOpBlue blueOpInc)))
             (#_PaintRgn nRgn)))
          ;; oval
          (oval
           (gs:let+ ((nRect (:rect))
                     (scale (scaleFactor me)))
             (setq curRed (startRed me)
                   curGreen (startGreen me)
                   curBlue (startBlue me)
                   redInc (round (- (endRed me) curRed) (round (max maxx maxy) scale))
                   greenInc (round (- (endGreen me) curGreen) (round (max maxx maxy) scale))
                   blueInc (round (- (endBlue me) curBlue) (round (max maxx maxy) scale))
                   curOpRed (startOpRed me)
                   curOpGreen (startOpGreen me)
                   curOpBlue (startOpBlue me)
                   redOpInc (round (- (endOpRed me)  curOpRed) (round (max maxx maxy) scale))
                   greenOpInc (round (- (endOpGreen me) curOpGreen) (round (max maxx maxy) scale))
                   blueOpInc (round (- (endOpBlue me) curOpBlue) (round (max maxx maxy) scale)))
             (copy-record (gs:hrref region :region.rgnbbox :storage :pointer) :rect nRect)
             (dotimes (i (round (max maxx maxy) scale))
               (rset curColor :rgbColor.red curRed)
               (rset curColor :rgbColor.green curGreen)
               (rset curColor :rgbColor.blue curBlue)
               (rset curOpColor :rgbColor.red curOpRed)
               (rset curOpColor :rgbColor.green curOpGreen)
               (rset curOpColor :rgbColor.blue curOpBlue)
               (#_RGBForeColor curColor)
               (#_OpColor curOpColor)
               (#_frameOval nRect)
               (#_InsetRect nRect 1 1)
               (setq curRed (+ curRed redInc)
                     curGreen (+ curGreen greenInc)
                     curBlue (+ curBlue blueInc)
                     curOpRed (+ curOpRed redOpInc)
                     curOpGreen (+ curOpGreen greenOpInc)
                     curOpBlue (+ curOpBlue blueOpInc)))
             (#_PaintOval nRect)))
          ;; rect
          (rect
           (gs:let+ ((nRect (:rect))
                     (scale (scaleFactor me)))
             (setq curRed (startRed me)
                   curGreen (startGreen me)
                   curBlue (startBlue me)
                   redInc (round (- (endRed me) curRed) (round (max maxx maxy) scale))
                   greenInc (round (- (endGreen me) curGreen) (round (max maxx maxy) scale))
                   blueInc (round (- (endBlue me) curBlue) (round (max maxx maxy) scale))
                   curOpRed (startOpRed me)
                   curOpGreen (startOpGreen me)
                   curOpBlue (startOpBlue me)
                   redOpInc (round (- (endOpRed me)  curOpRed) (round (max maxx maxy) scale))
                   greenOpInc (round (- (endOpGreen me) curOpGreen) (round (max maxx maxy) scale))
                   blueOpInc (round (- (endOpBlue me) curOpBlue) (round (max maxx maxy) scale)))
             (copy-record (gs:hrref region :region.rgnbbox :storage :pointer) :rect nRect)
             (dotimes (i (round (max maxx maxy) scale))
               (rset curColor :rgbColor.red curRed)
               (rset curColor :rgbColor.green curGreen)
               (rset curColor :rgbColor.blue curBlue)
               (rset curOpColor :rgbColor.red curOpRed)
               (rset curOpColor :rgbColor.green curOpGreen)
               (rset curOpColor :rgbColor.blue curOpBlue)
               (#_RGBForeColor curColor)
               (#_OpColor curOpColor)
               (#_frameRect nRect)
               (#_InsetRect nRect 1 1)
               (setq curRed (+ curRed redInc)
                     curGreen (+ curGreen greenInc)
                     curBlue (+ curBlue blueInc)
                     curOpRed (+ curOpRed redOpInc)
                     curOpGreen (+ curOpGreen greenOpInc)
                     curOpBlue (+ curOpBlue blueOpInc)))
             (#_PaintRect nRect)))
          (t
           (error "Direction ~a is not valid for rendering ~a." (direction me) me)))
        ;; clean up
        (#_PenNormal)))))

;;; _______________________________ 
;;; ComplexGradients.
;;; _______________________________ 

(new ComplexGradient :objectName "Redblend"
     :STARTRED 65535 :STARTGREEN 0 :STARTBLUE 0
     :ENDRED 65535 :ENDGREEN 0 :ENDBLUE 0
     :STARTOPRED 65535 :STARTOPGREEN 65535 :STARTOPBLUE 65535 
     :ENDOPRED 0 :ENDOPGREEN 0 :ENDOPBLUE 0
     :DIRECTION 'HORIZONTAL :PENMODE 'blend
     :project sk8)

(new ComplexGradient :objectName "Blueblend"
     :STARTRED 0 :STARTGREEN 0 :STARTBLUE 65535
     :ENDRED 0 :ENDGREEN 0 :ENDBLUE 65535
     :STARTOPRED 65535 :STARTOPGREEN 65535 :STARTOPBLUE 65535
     :ENDOPRED 0 :ENDOPGREEN 0 :ENDOPBLUE 0
     :DIRECTION 'VERTICAL :PENMODE 'blend
     :project sk8)

(new ComplexGradient :objectName "Shapestain"
     :STARTRED 65535 :STARTGREEN 0 :STARTBLUE  0
     :ENDRED 0 :ENDGREEN 0 :ENDBLUE 65535 :STARTOPRED 0
     :STARTOPGREEN 0 :STARTOPBLUE 0
     :ENDOPRED 65535 :ENDOPGREEN 65535 :ENDOPBLUE 65535
     :DIRECTION 'SHAPE :PENMODE 'blend :SCALEFACTOR 5
     :project sk8)


#|
	Change History (most recent last):
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	4  	 7/12/96	Brian   	Fixing penmode
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
