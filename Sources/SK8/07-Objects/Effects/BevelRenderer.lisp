;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "BEVELRENDERER")

(require "RENDERER")

;;____________________________________________________________________
;; BevelRenderer

;; This renderer takes four rgbColors and renders each of them around
;; the object.

(new renderer :objectName "BevelRenderer" :project sk8
     :undisposable t
     :prototype t
     :properties `((leftRenderer :value ,black)
                   (topRenderer :value ,white)
                   (rightRenderer :value ,black)
                   (bottomRenderer :value ,white)
                   (frameRelative :value t)))
  
(define-handler (setf leftRenderer) (newValue BevelRenderer)
  (if (inheritsFrom newValue Renderer)
    (setf (slot-value me 'leftRenderer) newValue)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  Renderer
               :ownerObject   BevelRenderer
               :propertyName 'leftRenderer
               )))

(define-handler (setf topRenderer) (newValue BevelRenderer)
  (if (inheritsFrom newValue Renderer)
    (setf (slot-value me 'topRenderer) newValue)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  Renderer
               :ownerObject   BevelRenderer
               :propertyName 'topRenderer
               )))

(define-handler (setf rightRenderer) (newValue BevelRenderer)
  (if (inheritsFrom newValue Renderer)
    (setf (slot-value me 'rightRenderer) newValue)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  Renderer
               :ownerObject   BevelRenderer
               :propertyName 'rightRenderer
               )))

(define-handler (setf bottomRenderer) (newValue BevelRenderer)
  (if (inheritsFrom newValue Renderer)
    (setf (slot-value me 'bottomRenderer) newValue)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  Renderer
               :ownerObject   BevelRenderer
               :propertyName 'bottomRenderer
               )))

(define-handler render (BevelRenderer theActor region thePaper)
  (gs:let+ ((tempRgn1 (:region))
            (currentClip (:region)))
    (#_getClip currentClip)
    (#_Sectrgn currentClip region tempRgn1)
    (gs:with-clipped-region tempRgn1
      (gs:let+ ((tempRgn (:region))
                (bounds (:rect))
                left right top bottom h v 
                maxNum fv)
        ;; init the vars
        (gs:region-into-rect region bounds)
        (setq left (rref bounds :rect.left)
              top (rref bounds :rect.top)
              Right (rref bounds :rect.right)
              bottom (rref bounds :rect.bottom)
              h (round (+ left right) 2)
              v (round (+ top bottom) 2))
        (if (frameRelative me)
          (progn
            (setq fv (gs:node-frameSize theActor)
                  fv (gs:pt-v fv)
                  maxNum (1+ (round (min (- right left) (- bottom top)) 2)))
            
            ;; left
            (#_OpenRgn)
            (#_MoveTo left top)
            (#_LineTo (min (+ left maxNum) h) (min (+ top maxNum) v))
            (#_LineTo (min (+ left maxNum) h) (max (- bottom maxNum) v))
            (#_LineTo left bottom)
            (#_LineTo left top)
            (#_CloseRgn tempRgn)
            (render (leftRenderer me) theActor tempRgn thePaper)
            ;; top
            (#_OpenRgn)
            (#_MoveTo left top)
            (#_LineTo (min (+ left maxNum) h) (min (+ top maxNum) v))
            (#_LineTo (max (- right maxNum) h) (min (+ top maxNum) v))
            (#_LineTo right top)
            (#_LineTo left top)
            (#_CloseRgn tempRgn)
            (render (topRenderer me) theActor tempRgn thePaper)
            ;; right
            (#_OpenRgn)
            (#_MoveTo right top)
            (#_LineTo (max (- right maxNum) h) (min (+ top maxNum) v))
            (#_LineTo (max (- right maxNum) h) (max (- bottom maxNum) v))
            (#_LineTo right bottom)
            (#_LineTo right top)
            (#_CloseRgn tempRgn)
            (render (rightRenderer me) theActor tempRgn thePaper)
            ;; bottom
            (#_OpenRgn)
            (#_MoveTo left bottom)
            (#_LineTo (min (+ left maxNum) h) (max (- bottom maxNum) v))
            (#_LineTo (max (- right maxNum) h) (max (- bottom maxNum) v))
            (#_LineTo right bottom)
            (#_LineTo left bottom)
            (#_CloseRgn tempRgn)
            (render (bottomRenderer me) theActor tempRgn thePaper)
            )
          (progn
            ;; left
            (#_OpenRgn)
            (#_MoveTo left top)
            (#_LineTo h v)
            (#_LineTo left bottom)
            (#_LineTo left top)
            (#_CloseRgn tempRgn)
            (render (leftRenderer me) theActor tempRgn thePaper)
            ;; top
            (#_OpenRgn)
            (#_MoveTo left top)
            (#_LineTo h v)
            (#_LineTo right top)
            (#_LineTo left top)
            (#_CloseRgn tempRgn)
            (render (topRenderer me) theActor tempRgn thePaper)
            ;; right
            (#_OpenRgn)
            (#_MoveTo right top)
            (#_LineTo h v)
            (#_LineTo right bottom)
            (#_LineTo right top)
            (#_CloseRgn tempRgn)
            (render (rightRenderer me) theActor tempRgn thePaper)
            ;; bottom
            (#_OpenRgn)
            (#_MoveTo left bottom)
            (#_LineTo h v)
            (#_LineTo right bottom)
            (#_LineTo left bottom)
            (#_CloseRgn tempRgn)
            (render (bottomRenderer me) theActor tempRgn thePaper)))))))

(define-handler translucent (bevelrenderer)
  (or (translucent (leftRenderer me))
      (translucent (topRenderer me))
      (translucent (rightRenderer me))
      (translucent (bottomRenderer me))))

;;; _______________________________ 
;;; Instances
;;; _______________________________ 

(new bevelRenderer :objectName "UIRectangleOutBevel" :project sk8)
(setf (leftRenderer uiRectangleOutBevel) gray
      (topRenderer uiRectangleOutBevel) gray
      (bottomRenderer uiRectangleOutBevel) darkGray
      (rightRenderer uiRectangleOutBevel) black)

(new bevelRenderer :objectName "UIRectangleInBevel" :project sk8)
(setf (leftRenderer uiRectangleInBevel) black
      (topRenderer uiRectangleInBevel) darkGray
      (bottomRenderer uiRectangleInBevel) gray
      (rightRenderer uiRectangleInBevel) gray)



#|
	Change History (most recent last):
	2  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
