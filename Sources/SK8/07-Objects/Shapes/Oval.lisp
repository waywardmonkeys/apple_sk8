;;; SK8 Built-In Oval Actor Definition
;;;

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)

(provide "OVAL")

(require "ACTOR")

#| Modification History

01-22-93 ruben id -> objectName
09-20-92 hernan changing this to work with MF VI
07-26-92 ruben changed to SK8 package
05-12-92 ruben d29 conversion
03-30-92 ruben began d29; swept for "get-" and "Set-"
03-26-92 ruben gentemp name in new
03-25-92 ruben call-next-method
01-19-92 converted to MacFrames II

|#


(new Actor :objectname "Oval" :undisposable t :prototype t :project sk8)
(setf (private Oval) nil) ; PRIVATE API

;;; 1.0
;;; MakeBoundsRegion -- makes the bounds region for the oval. The region is saved in the
;;;                 boundsRegion field which should ALREADY have a region in it! When
;;;                 we are done we label the node's bounds as not dirty.

(define-handler makeBoundsRegion (oval)
  (gs:let+ ((flags (gs:node-flags me))
            (phys-rect (gs:recompute-physicalBoundsRect me))
            (rect (:rect :SK8Rect phys-rect)))
    (#_OpenRgn)    
    (#_FrameOval rect)
    (#_closeRgn (gs:node-BoundsRegion me))
    (gs:boundsDirty! flags 0)))

;;; 1.0
;;; makeFillRegion -- makes the fill region for the oval. The region is saved in the
;;;               fillRegion field which should ALREADY have a region in it! When
;;;               we are done, we label the node's fill as not dirty.

(define-handler makeFillRegion (oval)
  (gs:let+ ((rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me))) 
            xInset yInset)
    (sk8-multival-setf (xInset yInset) (framesize me :physical t))
    (#_InsetRect rect xInset yInset)
    (#_OpenRgn)
    (#_FrameOval rect)
    (#_closeRgn (gs:node-fillRegion me))
    (gs:fillDirty! (gs:node-flags me) 0)))

;;; 1.0
;;; makeFrameRegion -- makes the frame region for the oval. The region is saved in the
;;;                 frameRegion field which should ALREADY have a region in it! When
;;;                 we are done, we label the node's frame as not dirty.
;;; 
;;; Since the only way to compute this region is to compute the bounds region and the
;;; fill region, we might as well save these in the fields for the region. This means that calling this
;;; method alone insures that the whole actor is cached properly. Therefore, if we just call this
;;; method, everything will be clean... (IS THIS TRUE FOR ALL ACTORS?)

(define-handler makeFrameRegion (oval)
  (let ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (gs:recompute-fill-region me flags)
    (#_diffRgn (gs:node-boundsRegion me) 
     (gs:node-fillRegion me)
     (gs:node-frameRegion me))
    (gs:frameDirty! (gs:node-flags me) 0)))

#|
	Change History (most recent last):
	7	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	8	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	9	1/11/94	hernan	self -> me
	10	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	11	4/5/94	kleiman	approved for public API
	12 	 9/28/94	Hernan  	Moving the offseting code out of the region
							calculating methods. Also fixing makeboundsregion
							to stop forcing the fill and frame to be recomputed.
	13 	 3/11/95	dy      	time -> cl:time
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
