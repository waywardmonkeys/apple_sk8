;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SLOWSTRIPCOPY")

;;; Required by the PaintField and the LoneRangeRenderer. 

(defun slowStripCopy (gWorld bitMap rect rgb)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((left (rref rect :Rect.left))
         (top (rref rect :Rect.top))
         (bottom (rref rect :Rect.bottom))
         (vSize (- bottom top))
         (rowBytes (rref bitmap :bitMap.rowbytes))
         (baseAddr (rref bitMap :bitMap.baseAddr))
         (GPIX (gs:gWorld-pixmap gWorld))
         theByte)
    (declare (integer left top bottom vSize rowBytes theByte))
    (%stack-block ((theColor 6))
      (with-port
        gWorld
        (#_LockPixels gPix)
        (doTimes (y vSize)
          (doTimes (x rowBytes)
            (setq theByte 0)
            (doTimes (i 8)
              ;; get the pixel
              (#_GetCPixel (+ left (* x 8) i) (+ top y) theColor)
              ;; check it
              (if (not (or (/= (%get-word theColor 0) (%get-word rgb 0))
                           (/= (%get-word theColor 2) (%get-word rgb 2))
                           (/= (%get-word theColor 4) (%get-word rgb 4))))
                ;; set the pixel
                (setq theByte (+ theByte (expt 2 (- 7 i))))))
            ;; store the byte
            (%put-byte baseAddr theByte (+ (* rowBytes y) x))))
        (#_UnlockPixels gPix))
      )))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
