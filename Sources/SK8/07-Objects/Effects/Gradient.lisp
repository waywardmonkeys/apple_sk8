;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GRADIENT")

(require "RENDERER")

;;;______________________________________________________________________
;;; GRADIENT

(new Renderer
     :objectName "Gradient"
     :undisposable t
     :prototype t
     :properties '((direction :value horizontal)
                   (scaleFactor :value 3)
                   (startRed :value 0)
                   (startGreen :value 0)
                   (startBlue :value 0)
                   (endRed :value 65535)
                   (endGreen :value 65535)
                   (endBlue :value 65535))
     :project sk8)

(define-handler (setf startRed) (newValue Gradient)
  (setf (slot-value me 'startRed) (gs:f.round newValue)))

(define-handler (setf startGreen) (newValue Gradient)
  (setf (slot-value me 'startGreen) (gs:f.round newValue)))

(define-handler (setf startBlue) (newValue Gradient)
  (setf (slot-value me 'startBlue) (gs:f.round newValue)))

(define-handler (setf endRed) (newValue Gradient)
  (setf (slot-value me 'endRed) (gs:f.round newValue)))

(define-handler (setf endGreen) (newValue Gradient)
  (setf (slot-value me 'endGreen) (gs:f.round newValue)))

(define-handler (setf endBlue) (newValue Gradient)
  (setf (slot-value me 'endBlue) (gs:f.round newValue)))

(define-handler (setf direction) (newValue Gradient)
  (if (memq newValue '(horizontal vertical shape rect oval))
    (setf (slot-value me 'direction) newValue)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  '(horizontal vertical shape rect oval)
               :ownerObject   Gradient
               :propertyName 'direction
               )))

(define-handler stageRelative (Gradient)
  t)

(define-handler render (Gradient theActor region thePaper)
  (declare (ignore theActor thePaper))
  (gs:let+ ((trgn (:region))
            (currentClip (:region))
            (curColor (:rgbColor))
            (qd-rect (:rect))
            left top sleft stop sright sbottom maxx maxy
            redInc greenInc blueInc curRed curGreen curBlue)
    (gs:region-into-rect region qd-rect)
    (setf left (rref qd-rect :rect.left)
          top (rref qd-rect :rect.top)
          sleft left
          stop top
          sright (rref qd-rect :rect.right)
          sbottom (rref qd-rect :rect.bottom)
          maxx (- sright left)
          maxy (- sbottom top))
    (when (not (zerop (* maxx maxy)))
      ;; calc minimum fill area
      (#_getClip currentClip)
      (#_sectRgn currentClip region trgn)
      (gs:with-clipped-region trgn
        ;; which way to fill?
        (case (direction me)
          ;; horizontal fill
          (horizontal
           (setq redInc (round (- (endRed me) (startRed me)) maxx)
                 greenInc (round (- (endGreen me) (startGreen me)) maxx)
                 blueInc (round (- (endBlue me) (startBlue me)) maxx)
                 curRed (round (startRed me))
                 curGreen (round (startGreen me))
                 curBlue (round (startBlue me)))
           (dotimes (i (- sright sleft))
             (rset curColor :rgbColor.red curRed)
             (rset curColor :rgbColor.green curGreen)
             (rset curColor :rgbColor.blue curBlue)
             (#_RGBForeColor curColor)
             (#_MoveTo (+ sleft i) stop)
             (#_LineTo (+ sleft i) sbottom)
             (setq curRed (+ curRed redInc)
                   curGreen (+ curGreen greenInc)
                   curBlue (+ curBlue blueInc))))
          ;; vertical fill
          (vertical
           (setq redInc (round (- (endRed me) (startRed me)) maxy)
                 greenInc (round (- (endGreen me) (startGreen me)) maxy)
                 blueInc (round (- (endBlue me) (startBlue me)) maxy)
                 curRed (round (startRed me))
                 curGreen (round (startGreen me))
                 curBlue (round (startBlue me)))
           (dotimes (i (- sbottom stop))
             (rset curColor :rgbColor.red curRed)
             (rset curColor :rgbColor.green curGreen)
             (rset curColor :rgbColor.blue curBlue)
             (#_RGBForeColor curColor)
             (#_MoveTo sLeft (+ sTop i))
             (#_LineTo sRight (+ sTop i))
             (setq curRed (+ curRed redInc)
                   curGreen (+ curGreen greenInc)
                   curBlue (+ curBlue blueInc))))
          ;; shapeBurst
          (shape
           (gs:let+ ((nRgn (:region))
                     (scale (scaleFactor me)))
             (if (zerop scale) (error "Scale must be greater than zero"))
             (setq curRed (startRed me)
                   curGreen (startGreen me)
                   curBlue (startBlue me)
                   redInc (round (- (endRed me) curRed) (max 1 (round (max maxx maxy) scale)))
                   greenInc (round (- (endGreen me) curGreen) (max 1 (round (max maxx maxy) scale)))
                   blueInc (round (- (endBlue me) curBlue) (max 1 (round (max maxx maxy) scale))))
             (#_CopyRgn region nRgn)
             (dotimes (i (round (max maxx maxy) scale))
               (rset curColor :rgbColor.red curRed)
               (rset curColor :rgbColor.green curGreen)
               (rset curColor :rgbColor.blue curBlue)
               (#_RGBForeColor curColor)
               (#_frameRgn nRgn)
               (#_InsetRgn nRgn 1 1)
               (setq curRed (+ curRed redInc)
                     curGreen (+ curGreen greenInc)
                     curBlue (+ curBlue blueInc)))
             (#_PaintRgn nRgn)))
          ;; oval
          (oval
           (gs:let+ ((nRect (:rect))
                     (scale (scaleFactor me)))
             (if (zerop scale) (error "Scale must be greater than zero"))
             (setq curRed (startRed me)
                   curGreen (startGreen me)
                   curBlue (startBlue me)
                   redInc (round (- (endRed me)  curRed) (max 1 (round (max maxx maxy) scale)))
                   greenInc (round (- (endGreen me) curGreen) (max 1 (round (max maxx maxy) scale)))
                   blueInc (round (- (endBlue me) curBlue) (max 1 (round (max maxx maxy) scale))))
             (copy-record (gs:hrref region :region.rgnbbox :storage :pointer) :rect nRect)
             (dotimes (i (round (max maxx maxy) scale))
               (rset curColor :rgbColor.red curRed)
               (rset curColor :rgbColor.green curGreen)
               (rset curColor :rgbColor.blue curBlue)
               (#_RGBForeColor curColor)
               (#_frameOval nRect)
               (#_InsetRect nRect 1 1)
               (setq curRed (+ curRed redInc)
                     curGreen (+ curGreen greenInc)
                     curBlue (+ curBlue blueInc))
               (#_PaintOval nRect))))
          ;; rect
          (rect
           (gs:let+ ((nRect (:rect))
                     (scale (scaleFactor me)))
             (if (zerop scale) (error "Scale must be greater than zero"))
             (setq curRed (startRed me)
                   curGreen (startGreen me)
                   curBlue (startBlue me)
                   redInc (round (- (endRed me) curRed) (max 1 (round (max maxx maxy) scale)))
                   greenInc (round (- (endGreen me) curGreen) (max 1 (round (max maxx maxy) scale)))
                   blueInc (round (- (endBlue me) curBlue) (max 1 (round (max maxx maxy) scale))))
             (copy-record (gs:hrref region :region.rgnbbox :storage :pointer) :rect nRect)
             (dotimes (i (round (max maxx maxy) scale))
               (rset curColor :rgbColor.red curRed)
               (rset curColor :rgbColor.green curGreen)
               (rset curColor :rgbColor.blue curBlue)
               (#_RGBForeColor curColor)
               (#_frameRect nRect)
               (#_InsetRect nRect 1 1)
               (setq curRed (+ curRed redInc)
                     curGreen (+ curGreen greenInc)
                     curBlue (+ curBlue blueInc))
               (#_PaintRect nRect))))
          (t
           (error "Direction ~a is not valid for rendering ~a." (direction me) me)))
        ))))

;;; _______________________________ 
;;; Gradients!
;;; _______________________________ 

(new Gradient :objectName "Sunset" 
     :STARTRED 20000 :STARTGREEN 0 :STARTBLUE 30000
     :ENDRED 65535 :ENDGREEN 32768 :ENDBLUE 32768
     :DIRECTION 'VERTICAL
     :project sk8)

(new Gradient :objectName "Metal1" 
     :STARTRED 20000 :STARTGREEN 20000 :STARTBLUE 20000
     :ENDRED 60000 :ENDGREEN 60000 :ENDBLUE 60000
     :DIRECTION 'VERTICAL
     :project sk8)

(new Gradient :objectName "Metal2" 
     :STARTRED 60000 :STARTGREEN 60000 :STARTBLUE 60000
     :ENDRED 20000 :ENDGREEN 20000 :ENDBLUE 20000
     :DIRECTION 'VERTICAL
     :project sk8)

(new Gradient :objectName "Starburst"
     :STARTRED 0 :STARTGREEN 0 :STARTBLUE 0
     :ENDRED 65535 :ENDGREEN 0 :ENDBLUE 0
     :DIRECTION 'SHAPE
     :project sk8)

(new Gradient :objectName "Blacktored"
     :STARTRED 0 :STARTGREEN 0 :STARTBLUE 0
     :ENDRED 65535 :ENDGREEN 0 :ENDBLUE 0
     :DIRECTION 'HORIZONTAL
     :project sk8)

(new Gradient :objectName "Bluetunnel"
     :STARTRED 0 :STARTGREEN 32768 :STARTBLUE 65535
     :ENDRED 0 :ENDGREEN 0 :ENDBLUE 16000
     :DIRECTION 'rect
     :project sk8)


#|
	Change History (most recent last):
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
