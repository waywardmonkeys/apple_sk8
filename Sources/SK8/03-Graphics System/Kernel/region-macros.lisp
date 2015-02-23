(in-package :graphics-system)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; _______________________________________
;;; Recomputing regions and the offseting optimization!
;;; _______________________________________

;;; If the actor uses the offset optimization and offseting can be done,
;;; do it. Otherwise make the region.

(defun update-bounds-region (actor flags)
  (if (and (onlyLocationChanged? flags) (offsetsRegions? flags))
    ;; Do the clever offseting thing.
    (let ((physRect (recompute-physicalBoundsRect actor))
          (bounds (node-BoundsRegion actor)))
      (SK8-multival-bind
        (dx dy) (find-physical-offset physRect bounds)
        (#_offsetRgn bounds dx dy))
      (boundsDirty! flags 0))
    ;; Oh well... just make the region.
    (sk8::makeBoundsRegion actor)))

(defun update-frame-region (actor flags)
  (if (and (onlyLocationChanged? flags) (offsetsRegions? flags))
    ;; Do the clever offseting thing.
    (let ((physRect (recompute-physicalBoundsRect actor))
          (frame (node-frameRegion actor)))
      (SK8-multival-bind
        (dx dy) (find-physical-offset physRect frame)
        (#_offsetRgn frame dx dy))
      (frameDirty! flags 0))
    ;; Oh well... just make the region.
    (sk8::makeFrameRegion actor)))
    
;;; The fill is more complicated. When the region is made, the offset from the
;;; bounds is stored into the offsetBy slot.

(defun update-fill-region (actor flags)
  (let ((bounds (node-boundsRegion actor))
        (fill (node-fillRegion actor))
        (offsetBy (node-offsetBy actor)))
    (if (and (onlyLocationChanged? flags) (offsetsRegions? flags))
      ;; Offset by real offset - values in offsetby.
      (progn
        (recompute-bounds-region actor flags)
        (SK8-multival-bind (dx dy) (find-region-offset bounds fill)
          (decf dx (pt-h offsetBy))
          (decf dy (pt-v offsetBy))
          (#_offsetRgn fill dx dy))
        (fillDirty! flags 0))
      ;; Make the region and then store the offset in the offsetBy slot.
      (progn
        (sk8::makeFillRegion actor)
        (when (offsetsRegions? flags)
          (recompute-bounds-region actor flags)
          ;; Compute the offset by and store it.
          (sk8-multival-bind (xError yError) (find-region-offset bounds fill)
            (setf (pt-h offsetBy) xError
                  (pt-v offsetBy) yError)))))))


#|
	Change History (most recent last):
	1  	 9/28/94	Hernan  	Here are the functions that recompute the actor regions.
	2  	 7/25/96	Hernan  	Fixed an ancient region bug. The frame region needs to be
						computed from the physical boundsRect, not the bounds
						Region since the boundsRegion might already be recomputed.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
