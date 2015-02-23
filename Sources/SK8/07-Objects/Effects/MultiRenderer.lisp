;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "MULTIRENDERER")

(require "COMPLEXRGBCOLOR" "objects;Effects:ComplexRGBColor")

;;____________________________________________________________________
;; MultiRenderer RENDERER

(new Renderer :objectName "MultiRenderer"
     :undisposable t
     :prototype t
     :properties '((rendererList :value nil))
     :project sk8)

(define-handler (setf rendererList) (newValue MultiRenderer)
  (if (every #'(lambda (x) (inheritsFrom x Renderer)) newValue)
    (setf (slot-value me 'rendererList) newValue)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  "a list of Renderers"
               :ownerObject   MultiRenderer
               :propertyName 'rendererList
               )))

(define-handler render (MultiRenderer theActor region thePaper)
  (mapc #'(lambda (curColor)
            (#_penNormal)
            (render curColor theActor region thePaper))    
        (rendererList me)))

(define-handler translucent (MultiRenderer)
  (every #'translucent (rendererList me)))

;;; _______________________________ 
;;; Instances.
;;; _______________________________ 

(new multirenderer :objectname "MuchLighterWoodFloor" :project sk8)
(setf (rendererlist MuchLighterWoodFloor) (list woodfloor muchlighter))

(new multirenderer :objectname "DarkerFloorTile" :project sk8)
(setf (rendererlist DarkerFloorTile) (list FloorTile Darker))



#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
