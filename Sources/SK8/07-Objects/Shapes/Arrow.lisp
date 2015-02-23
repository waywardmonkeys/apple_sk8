(in-package :sk8dev)

(provide "ARROW")

(require "LINESEGMENT" "objects;Shapes:LineSegment")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; The Arrow is a child of lineSegment. We add a few properties and modify the makeboundsRegion method.
;;; Only the endpoints are cached! Thus, the makeBoundsRegion method will be slow.

(new LineSegment :objectName "Arrow" :project sk8
     :prototype t
     :properties '((arrows :value both) ; Either both or start or end.
                   (arrowSize :value (10 10))) ; Logical size.
        )

(setf (lineSize Arrow) 5)
(setf (gs:node-drawFunction Arrow) 'gs:sk8-simple-draw)

(define-handler localVirtualProperties (arrow)
  (when (eq me Arrow)
    '(startArrow endArrow)))

;;; ________________
;;; Regions.
;;; ________________

;;; This vector is used to return the points.

(defvar *arrow-point-vector* (make-array 20))

;;; Does all the work. Given endpoints, computes all points and returns them.

(defun get-arrow-points (me)
  (let* ((points *arrow-point-vector*)
         (arrowSize (arrowSize me))
         startx starty endx endy len

         width aWidth aHeight x y
         saBaseRefx saBaseRefy
         saBaseStartx saBaseStarty
         saBaseEndx saBaseEndy
         
         eaBaseRefx eaBaseRefy
         eaBaseStartx eaBaseStarty
         eaBaseEndx eaBaseEndy
         
         arrows)

    (sk8-multival-setf (startx starty endx endy) (endpoints me :physical t))
    (sk8-multival-setf (startx starty endx endy) 
                       (gs:stage-to-window-rect-coords me startx starty endx endy))
    (setf len (gs:coords-distance startx starty endx endy))
    (setf (aref points 0) startx
          (aref points 1) starty
          (aref points 10) endx
          (aref points 11) endy)


    (cond
     ;; length is too short to show the arrows.
     ((< len 0.5s0)
      (setq saBaseRefx   0s0    saBaseRefy   0s0
            saBaseStartx 0s0    saBaseStarty 0s0
            saBaseEndx   0s0    saBaseEndy   0s0
            
            eaBaseRefx   0s0    eaBaseRefy   0s0
            eaBaseStartx 0s0    eaBaseStarty 0s0
            eaBaseEndx   0s0    eaBaseEndy   0s0))
     (t
      (setq width (gs:fhalf (lineSize me))
            aWidth (gs:fhalf (car arrowSize))
            aHeight (cadr arrowSize)
            
            x (gs:f- endx startx)
            y (gs:f- endy starty)
            
            saBaseRefx   (gs:f* x (gs:f/ aHeight len))
            saBaseRefy   (gs:f* y (gs:f/ aHeight len))
            saBaseStartx (gs:f- (gs:f* y (gs:f/ width len)))
            saBaseStarty (gs:f* x (gs:f/ width len))
            saBaseEndx   (gs:f- (gs:f* y (gs:f/ aWidth len)))
            saBaseEndy   (gs:f* x (gs:f/ aWidth len))
            
            eaBaseRefx   (gs:f- saBaseRefx)
            eaBaseRefy   (gs:f- saBaseRefy)
            eaBaseStartx (gs:f- saBaseStartx)
            eaBaseStarty (gs:f- saBaseStarty)
            eaBaseEndx   (gs:f- saBaseEndx)
            eaBaseEndy   (gs:f- saBaseEndy)
            
            arrows (arrows me))
      
      (when (or (null arrows) (eq arrows 'end))
        (setq saBaseRefx 0s0
              saBaseRefy 0s0
              saBaseEndx saBaseStartx
              saBaseEndy saBaseStarty))
      (when (or (null arrows) (eq arrows 'start))
        (setq eaBaseRefx 0s0
              eaBaseRefy 0s0
              eaBaseEndx eaBaseStartx
              eaBaseEndy eaBaseStarty))))
    ;; Update the points.
    (setf (aref (the vector points) 2) (gs:f.round (gs:f+ startx saBaseRefx saBaseEndx))
          (aref (the vector points) 3) (gs:f.round (gs:f+ starty saBaseRefy saBaseEndy))
          
          (aref (the vector points) 18) (gs:f.round (gs:f- (gs:f+ startx saBaseRefx) saBaseEndx))
          (aref (the vector points) 19) (gs:f.round (gs:f- (gs:f+ starty saBaseRefy) saBaseEndy))
          
          (aref (the vector points) 4) (gs:f.round (gs:f+ startx saBaseRefx saBaseStartx))
          (aref (the vector points) 5) (gs:f.round (gs:f+ starty saBaseRefy saBaseStarty))
          
          (aref (the vector points) 16) (gs:f.round (gs:f- (gs:f+ startx saBaseRefx) saBaseStartx))
          (aref (the vector points) 17) (gs:f.round (gs:f- (gs:f+ starty saBaseRefy) saBaseStarty))
          
          (aref (the vector points) 12) (gs:f.round (gs:f+ endx eaBaseRefx eaBaseEndx))
          (aref (the vector points) 13) (gs:f.round (gs:f+ endy eaBaseRefy eaBaseEndy))
          
          (aref (the vector points) 8) (gs:f.round (gs:f- (gs:f+ endx eaBaseRefx) eaBaseEndx))
          (aref (the vector points) 9) (gs:f.round (gs:f- (gs:f+ endy eaBaseRefy) eaBaseEndy))
          
          (aref (the vector points) 14) (gs:f.round (gs:f+ endx eaBaseRefx eaBaseStartx))
          (aref (the vector points) 15) (gs:f.round (gs:f+ endy eaBaseRefy eaBaseStarty))
          
          (aref (the vector points) 6) (gs:f.round (gs:f- (gs:f+ endx eaBaseRefx) eaBaseStartx))
          (aref (the vector points) 7) (gs:f.round (gs:f- (gs:f+ endy eaBaseRefy) eaBaseStarty)))
    ;; update the original points and the bounds rects.
    points))

(defun find-points-for-arrows (me)
    (sk8-multival-bind (startx starty endx endy) (endpoints me)
      (sk8-multival-setf (startx starty) (gs:real-logical-to-physical (gs:node-container me) startx starty))
      (sk8-multival-setf (endx endy) (gs:real-logical-to-physical (gs:node-container me) endx endy))
      (let* ((len (gs:coords-distance startx starty endx endy))
             width aWidth aHeight x y
             saBaseRefx saBaseRefy
             saBaseStartx saBaseStarty
             saBaseEndx saBaseEndy
             eaBaseRefx eaBaseRefy
             eaBaseStartx eaBaseStarty
             eaBaseEndx eaBaseEndy
             arrows)
        (if (zerop len)
          ;; No room for arrows: do not draw them!
          (setq saBaseRefx 0s0
                saBaseRefy 0s0
                saBaseEndx 0s0
                saBaseEndy 0s0
                eaBaseRefx 0s0
                eaBaseRefy 0s0
                eaBaseEndx 0s0
                eaBaseEndy 0s0)
          ;; There's room: go ahead!
          (progn (setq width (gs:fhalf (lineSize me))
                       aWidth (gs:fhalf (car (arrowSize me)))
                       aHeight (cadr (arrowSize me))
                       
                       x (gs:f- endx startx)
                       y (gs:f- endy starty)
                       
                       saBaseRefx   (gs:f* x (gs:f/ aHeight len))
                       saBaseRefy   (gs:f* y (gs:f/ aHeight len))
                       saBaseStartx (gs:f- (gs:f* y (gs:f/ width len)))
                       saBaseStarty (gs:f* x (gs:f/ width len))
                       saBaseEndx   (gs:f- (gs:f* y (gs:f/ aWidth len)))
                       saBaseEndy   (gs:f* x (gs:f/ aWidth len))
                       
                       eaBaseRefx   (gs:f- saBaseRefx)
                       eaBaseRefy   (gs:f- saBaseRefy)
                       eaBaseStartx (gs:f- saBaseStartx)
                       eaBaseStarty (gs:f- saBaseStarty)
                       eaBaseEndx   (gs:f- saBaseEndx)
                       eaBaseEndy   (gs:f- saBaseEndy)
                       
                       arrows (arrows me))
                 (when (or (null arrows) (eq arrows 'end))
                   (setq saBaseRefx 0s0
                         saBaseRefy 0s0
                         saBaseEndx saBaseStartx
                         saBaseEndy saBaseStarty))
                 (when (or (null arrows) (eq arrows 'start))
                   (setq eaBaseRefx 0s0
                         eaBaseRefy 0s0
                         eaBaseEndx eaBaseStartx
                         eaBaseEndy eaBaseStarty))))
        ;; Update the points.
        (sk8-multivals (gs:f.round (gs:f+ startx saBaseRefx saBaseEndx)) ;; #2
                       (gs:f.round (gs:f+ starty saBaseRefy saBaseEndy)) ;; #3
                       (gs:f.round (gs:f- (gs:f+ startx saBaseRefx) saBaseEndx)) ;; #18
                       (gs:f.round (gs:f- (gs:f+ starty saBaseRefy) saBaseEndy)) ;; #19
                       
                       (gs:f.round (gs:f- (gs:f+ endx eaBaseRefx) eaBaseEndx))  ;; #8
                       (gs:f.round (gs:f- (gs:f+ endy eaBaseRefy) eaBaseEndy))  ;; #9
                       (gs:f.round (gs:f+ endx eaBaseRefx eaBaseEndx))       ;; #12
                       (gs:f.round (gs:f+ endy eaBaseRefy eaBaseEndy)))      ;; #13
        )))

(defun add-the-arrows (me temp-region startx starty endx endy)
  (gs:let+ ((other-region (:region)))
    (sk8-multival-bind (p2 p3 p18 p19 p8 p9 p12 p13) (find-points-for-arrows me)
      (when (startArrow me)
        ;; We have the points! make the region.
        (#_OpenRgn)
        (#_moveTo startX starty)
        (#_lineTo p2 p3)
        (#_lineTo p18 p19)
        (#_lineTo startX startY)
        (#_closeRgn other-region)
        (#_unionRgn other-region temp-region temp-region))
      (when (endArrow me)
        ;; We have the points! make the region.
        (#_OpenRgn)
        (#_moveTo endX endy)
        (#_lineTo p8 p9)
        (#_lineTo p12 p13)
        (#_lineTo endX endY)
        (#_closeRgn other-region)
        (#_unionRgn other-region temp-region temp-region)))))

(define-handler makeBoundsRegion (Arrow)
  (if (= (lineSize me) 1)
    ;; If lineSize = 1, use the segment's method and add the arrows at the end.
    (progn 
      (call-next-method)
      (sk8-multival-bind (sx sy ex ey) (endpoints me :physical t)
        (sk8-multival-setf (sx sy ex ey) (gs:stage-to-window-rect-coords me sx sy ex ey))
        (add-the-arrows me (gs:node-boundsRegion me) sx sy ex ey)))
    (let* ((flags (gs:node-flags me))
           (points (get-arrow-points me))
           (len (length points))
           (counter 0)
           x y startX startY)
      (#_OpenRgn)
      ;; connect the points.
      (loop
        (setf x (aref points counter)
              y (aref points (1+ counter)))
        (incf counter 2)
        (if (= counter 2)
          (progn (setf startX x)
                 (setf startY y)
                 (#_moveTo x y))
          (#_lineTo x y))
        (when (= counter len)
          (return)))
      ;; close the polygon.  
      (#_lineTo startX startY)
      (#_closeRgn (gs:node-boundsRegion me))
      (gs:boundsDirty! flags 0))))

(define-handler makeFillRegion (Arrow)
  (if (= (lineSize me) 1)
    (call-next-method)
    (let ((flags (gs:node-flags me))
          (fillRegion (gs:node-fillRegion me))
          (point (gs:node-frameSize me)))
      (gs:recompute-bounds-region me flags)
      (#_copyRgn (gs:node-boundsRegion me) fillRegion)
      (#_InsetRgn fillRegion
       (gs:f.round (* (gs:node-xScale me) (gs:pt-h point)))
       (gs:f.round (* (gs:node-yScale me) (gs:pt-v point))))
      (gs:fillDirty! flags 0))))

(define-handler makeFrameRegion (Arrow)
  (if (= (lineSize me) 1)
    (call-next-method)
    (let ((flags (gs:node-flags me)))
      (gs:recompute-bounds-region me flags)
      (gs:recompute-fill-region me flags)
      (#_diffRgn (gs:node-boundsRegion me) 
       (gs:node-fillRegion me)
       (gs:node-frameRegion me))
      (gs:frameDirty! flags 0))))

;;; ____________
;;; Arrows.
;;; ____________

(define-handler (setf arrows) (arrows Arrow)
  (if (memq arrows '(start end both))
    (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds t)
      (setf (slot-value me 'arrows) arrows))
    (sk8-error PropertyTypeMismatchError
               :object        arrows
               :expectedType  '(start end both)
               :ownerObject   Arrow
               :propertyName 'arrows
               ))
  arrows)

;;; STARTARROW --  returns T if there's a start arrow.

(define-handler startarrow (Arrow)
  (when (memq (arrows me) '(start both))
    t))

;;; ENDARROW -- returns T if there's an end arrow.

(define-handler endarrow (Arrow)
  (when (memq (arrows me) '(end both))
    t))

;;; (SETF ARROWSIZE) -- changes the size of the arrows.

(define-handler (setf ArrowSize) (size Arrow &key physical relative)
  (declare (ignore physical))
  (let ((currentSize (arrowSize me)))
    (if relative
      (setf (car currentSize) (+ (car currentSize) (car size))
            (cadr currentSize) (+ (cadr currentSize) (cadr size)))
      (setf (car currentSize) (car size)
            (cadr currentSize) (cadr size)))
    (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds t)
      )
    size))

;;; (SETF STARTARROW) -- gives self a start arrow if set-p is t, and removes it otherwise.

(define-handler (setf startarrow) (boolean Arrow)
  (let ((arrows (arrows me)))
    (if boolean
      ;; adding the arrow only if necessary.
      (case arrows
        (end (setf (arrows me) 'both))
        (nil (setf (arrows me) 'start)))
      ;; removing the arrow only if necessary.
      (if (eq arrows 'start)
        (sk8-error GeneralProgrammaticError
                   :Strings '("Cannot remove the arrow's only remaining head."))
        (case arrows
          (start (setf (arrows me) nil))
          (both (setf (arrows me) 'end)))))))

;;; (SETF ENDARROW) -- gives self a start arrow if set-p is t, and removes it otherwise.

(define-handler (setf endarrow) (boolean Arrow)
  (let ((arrows (arrows me)))
    (if boolean
      ;; adding the arrow only if necessary.
      (case arrows
        (start (setf (arrows me) 'both))
        (otherwise (setf (arrows me) 'end)))
      ;; removing the arrow only if necessary.
      (if (eq arrows 'end)
        (sk8-error GeneralProgrammaticError
                   :Strings '("Cannot remove the arrow's only remaining head."))
        (case arrows
          (end (setf (arrows me) nil))
          (both (setf (arrows me) 'start)))))))

;;; __________________
;;; LineSize
;;; __________________

(define-handler (setf lineSize) (newSize Arrow)
  (if (= newSize 1)
    (setf (gs:node-drawFunction me) 'draw-segment)
    (when (eq (gs:node-drawFunction me) 'draw-segment)
      (setf (gs:node-drawFunction me) 'mf::sk8-simple-draw)))
  (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds t)
    (call-next-method)))

#|
	Change History (most recent last):
	1	4/8/94	Hernan	New file. Implements an actor which is just an
				arrow.
	2	4/8/94	Hernan	Fixing typo.
	3	7/5/94	Hernan	1172190: fixing find-points-for-arrows to work
				correctly when the line's length is 0.
	4	8/12/94	Hernan  	
	5  	 9/28/94	Hernan  	Moving the offseting code out of the region
							calculating methods. Also fixing makeboundsregion
							to stop forcing the fill and frame to be recomputed.
	6  	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	7  	 1/24/95	Hernan  	Using with-fast-node-slots where appropriate.
	8  	 2/ 2/95	Hernan  	set linesize has to use its own making-self-dirty
							because the arrow cares about more regions than the
							lineSegment.
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
