;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "TBSKETCHRENDERER")

(require "SKETCHRENDERER" "objects;Effects:SketchRenderer")

;;; tbSketchRenderer -- specialized for the treeBrowser: has 1 start point and many end points...

(new SketchRenderer
     :objectName "TBSketchRenderer"
     :undisposable t
     :prototype t
     :properties '((startPoint :value (0 20)))
     :project sk8)

(setf (backgroundRenderer tbSketchRenderer) white)
(setf (foreGroundRenderer tbSketchRenderer) black)

(define-handler render (tbSketchRenderer theActor region thePaper)
  (gs:let+ ((temp-region (:region))
            (currentClip (:region)))
    (#_getClip currentClip)
    (#_sectRgn currentClip region temp-region)
    (gs:with-clipped-region temp-region
      (render (backgroundRenderer me) theActor region thePaper)
      ;; Now draw the lines...
      (#_penNormal)
      (let* ((pensize (pensize me))
             (left (rref (gs:hrref region :region.rgnbbox :storage :pointer) :rect.left))
             (top (rref (gs:hrref region :region.rgnbbox :storage :pointer) :rect.top))
             (startpoint (startPoint me))
             (startX (+ left (car startPoint)))
             (startY (+ top (cadr startPoint))))
        (#_PenSize (car penSize) (cadr penSize))
        (with-rgb (lineColor (mcl-color (foreGroundRenderer me)))
          (#_RGBForeColor lineColor)
          (dolist (quad (linepoints me))
            (#_MoveTo startX startY)
            (#_LineTo (+ left (car quad)) (+ top (cadr quad)))))
        (#_PenNormal)))))


#|
	Change History (most recent last):
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
