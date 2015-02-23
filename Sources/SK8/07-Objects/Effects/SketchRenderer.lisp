;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SKETCHRENDERER")

(require "RENDERER")

;;;______________________________________________________________________
;;; Sketch Renderer (courtesy of Alan Peterson). Draws lines between the specified points.

(new Renderer
     :objectName "SketchRenderer"
     :undisposable t
     :prototype t
     :properties '((linepoints :value ((10 10 30 40)))
                   (pensize :value (1 1))
                   (foreGroundRenderer :value nil)
                   (backgroundRenderer :value nil))
     :project sk8)

(setf (foreGroundRenderer sketchRenderer) black)
(setf (backgroundRenderer sketchRenderer) white)

;;; Make sure the fore renderer is an RGB.

(define-handler (setf foreGroundRenderer) (newColor SketchRenderer)
  (if (inheritsFrom newColor RGBColor)
    (setf (slot-value me 'foreGroundRenderer) newColor)
    (sk8-error PropertyTypeMismatchError
               :object        newColor
               :expectedType  RGBColor
               :ownerObject   SketchRenderer
               :propertyName 'foreGroundRenderer
               )))

(define-handler (setf penSize) (newValue SketchRenderer)
  (if (and (every #'numberp newValue) (= 2 (length newValue)))
    ;; Round and set the slot.
    (progn
      (setf (car newValue) (gs:f.round (car newValue)))
      (setf (cadr newValue) (gs:f.round (cadr newValue)))
      (setf (slot-value me 'penSize) newValue))
    (sk8-error GeneralProgrammaticError
               :strings '("The pensize should be a list of 2 integers."))))

(define-handler render (SketchRenderer theActor region thePaper)
  (gs:with-composed-clip region
    (render (backgroundRenderer me) theActor region thePaper)
    ;; Now draw the lines...
    (#_penNormal)
    (rlet ((r :rect))
      (gs:region-into-rect region r)
      (let ((pensize (pensize me))
            (left (rref r :rect.left))
            (top (rref r :rect.top)))
        (#_PenSize (car penSize) (cadr penSize))
        (with-rgb (lineColor (mcl-color (foreGroundRenderer me)))
          (#_RGBForeColor lineColor)
          (dolist (quad (linepoints me))
            (#_MoveTo (+ left (first quad)) (+ top (second quad)))
            (#_LineTo (+ left (third quad)) (+ top (fourth quad))))
          (#_PenNormal))))))


#|
	Change History (most recent last):
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
