(in-package :sk8dev)

(provide "POLYGON")

(require "ACTOR")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#|
Modification History

06-02-93 Steve added pivots and magnification, default center-of-mass coordinates.  Rotation 
		works properly with any anchor.  rotation, scaling, 
		translation work on stage too.
05-24-93 Hernan	Can't remember...
05-21-93 Hernan	Lots of changes to make polygons work on the 
		stage. For setting the boundsRect, the actor
		method suffices (since it calls the polygon method
		for preparing for the wdef calculation). For setting
		the points, wrote a new function that does ALL
		the work.
02-23-94 ruben changed xAnchor & yAnchor to hAnchor and vAnchor
01-22-93 ruben id -> objectName
09-20-92 hernan changing this to work with MF VI
09-03-92 hernan making polygons work properly with the new physical (stage) coords.
05-12-92 ruben d29 conversion
03-29-92 ruben began d29; location, rectangle, points, autoclose, smooth, reshape
               nth-point and nth-point*, swept for "get-" and "set-"
03-26-92 ruben gentemp name in NEW
03-25-92 ruben call-next-method
03-24-92 adam The coords-distance macro now works properly (so methods that used it
              now work properly -- set-rotation, line-properties-to-points, 
              line-points-to-properties).
01-19-92 converted to MacFrames II
05-29-91 ruben 'LIST atom bug for make-code fixed
05-27-91 ruben make-code for scripts

|#

;; polygon
;; this is a multi-sided polygon object, which can also
;; be smoothed.

;;; 1.0
;;; POLYGON -- this is the polygon actor. Note that we copy the default points to the node. Note that we were
;;;           using the plist to store all this. Since the plist was getting large, it is faster to use the frame
;;;           and fastest when accessing with slot-value. 
;;;
;;; To avoid propagated round-off errors, the original shape of the polygon is retained and new positions are
;;; calculated from this shape.  What follows is a description of the properties:
;;;	pointsList--the actual points of the polygon, with respect to its container
;;;	originPointsList--the (almost constant) original points of the polygon
;;;	hanchor/vanchor--the position of the center of rotation.  When these are nil, the center is at the location

(new actor 
     :objectname "Polygon"
     :undisposable t
     :prototype t
     :properties '((pointsList :value #(50 50 150 50  100 200))
                   (originPointsList :value #(-50 -50  50 -50  0 100))
                   (angle :value 0)
                   (hAnchor :value nil)
                   (vAnchor :value nil))
     :project sk8)

(setf (private Polygon) nil)  ; PUBLIC API


(setf (private Polygon :property 'pointsList) t)
(setf (private Polygon :property 'originPointsList) t)

;;; OJO: these 5 need to be defined for compatibility with the old polygon
;;; because these two surviving properties were defined propagatable in it.
;;; We might be able to get rid of this.

(define-handler pointsList (polygon)
  (getValue 'pointsList me))

(define-handler originPointsList (polygon)
  (getValue 'originpointsList me)) 

(define-handler angle (polygon)
  (getValue 'angle me))

(define-handler hAnchor (polygon)
  (getValue 'hAnchor me))

(define-handler vAnchor (polygon)
  (getValue 'vAnchor me))


(setf (gs:node-physicalBoundsRect polygon) (gs:make-rect :left 50 :top 50 :right 150 :bottom 200))
(setf (gs:node-logicalBoundsRect polygon) (gs:make-rect :left 50 :top 50 :right 150 :bottom 200))

;;;
;;; (SETF ANGLE)--change the angle of rotation to the specified value
;;;  The angle is absolute.  Relative rotations can be performed through
;;;  the ROTATE handler.

(define-handler (setf angle) (angle polygon)
  (rotate me angle))

;;;
;;; --------------------------------------------------------------------------------------
;;; 				handlers to access ANCHORS
;;; --------------------------------------------------------------------------------------

(define-handler anchor (polygon)
  (let ((ha (hanchor me))
        (va (vanchor me)))
    (if (and ha va) (sk8-multivals ha va) nil)))

(defmacro set-Anchor (theActor hanchor vanchor)
  `(progn 
     (cond ((ccl::xor (null ,hanchor) (null ,vanchor))
            (sk8-error GeneralProgrammaticError
                       :strings '("You should set both anchor points to numbers before trying to modify an individual one.")))
           (t
            ;; Just set the slots.
            (sk8::setValue 'hanchor ,theActor ,hanchor)
            (sk8::setValue 'vanchor ,theActor ,vanchor)))))

(define-handler (setf hanchor) (hanchor polygon)
  (set-Anchor me hanchor (vanchor me)))

(define-handler (setf vanchor) (vanchor polygon)
  (set-Anchor me (hanchor me) vanchor))

(define-handler (setf anchor) (alist polygon)
  (definitely-recover-list alist)
  (sk8-destructuring-bind (h v) alist
    (set-Anchor me h v)))

;;;
;;; INITIALIZE. . .

(define-handler initialize (polygon original isNew initargs)
  (declare (ignore isNew initargs original))
  (call-next-method)
  ;; DO NOT TAKE THESE TWO LINES OUT!!! NEED TO COPY THE VECTORS.
  (setf (pointsList me) (copy-seq (pointsList me)))
  (setf (originPointsList me) (copy-seq (originPointsList me))))

;;; 1.0
;;; MAKEBOUNDSREGION -- computes the bounds region of me. The points are expressed in logical coordinates. 
;;;                   Thus, to compute the regions, we need to translate each point to a physical and do
;;;                    the drawing. All the complicated parts of computing polygon regions are here (the
;;;                   other two region computing methods are very simple).
;;;
;;; This method just loops through the points, converting them into physical coordinates and joining them
;;; with lines.

(define-handler makeBoundsRegion (polygon)
  (let* ((flags (gs:node-flags me))
         (points (getValue 'pointsList me))
         (len (length points))
         (counter 0)
         (container (gs:node-container me))
         logicalX logicalY startX startY)
    (#_OpenRgn)
    ;; connect the points.
    (loop
      (setf logicalX (aref points counter)
            logicalY (aref points (1+ counter)))
      (incf counter 2)
      (SK8-multival-bind (physicalX physicalY) 
                         (gs:real-logical-to-physical container logicalX logicalY)
        (if (= counter 2)
          (progn (setf startX physicalX)
                 (setf startY physicalY)
                 (#_moveTo startX startY))
          (#_lineTo physicalX physicalY)))
      (when (= counter len)
        (return)))
    ;; close the polygon.  
    (#_lineTo startX startY)
    (#_closeRgn (gs:node-boundsRegion me))
    (gs:boundsDirty! flags 0)))

;;; 1.0
;;; MAKEFILLREGION -- computes the fill region of me. This just involves insetting the bounds region by the
;;;                size of the frame.

(define-handler makeFillRegion (polygon)
  (let ((flags (gs:node-flags me))
        (fill (gs:node-fillRegion me))
        (point (gs:node-frameSize me)))
    (gs:recompute-bounds-region me flags)
    (#_copyRgn (gs:node-boundsRegion me) fill)
    (#_InsetRgn fill
     (gs:f.round (* (gs:node-xScale me) (gs:pt-h point)))
     (gs:f.round (* (gs:node-yScale me) (gs:pt-v point))))
    (gs:fillDirty! flags 0)))
      
;;; 1.0
;;; MAKEFRAMEREGION -- computes the frame region of self. This involves subtracting the fill region
;;;                  from the bounds region.


(define-handler makeFrameRegion (polygon)
  (let ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (gs:recompute-fill-region me flags)
    (#_diffRgn (gs:node-boundsRegion me) 
     (gs:node-fillRegion me)
     (gs:node-frameRegion me))
    (gs:frameDirty! flags 0)))

;;; 1.0
;;; GET-TOP-LEFT-POINTS -- returns the smallest x and y value in the point vector.

(defun get-top-left-points (point-vector)
  (let ((counter 2)
        (len (length point-vector))
        pt
        (smallX (aref point-vector 0))
        (smallY (aref point-vector 1)))
    (loop
      (setf pt (aref point-vector counter))
      (when (> smallX pt) (setf smallX pt))
      (setf pt (aref point-vector (1+ counter)))
      (when (> smallY pt) (setf smallY pt))
      (incf counter 2)
      (when (= counter len)
        (return)))
    (values smallX smallY)))

(defun get-boundsRect-coords (point-vector)
  (let* ((counter 2)
         (len (length point-vector))
         pt
         (smallX (aref point-vector 0))
         (smallY (aref point-vector 1))
         (bigX smallX)
         (bigY smallY))
    (loop
      (setf pt (aref point-vector counter))
      (when (> smallX pt) (setf smallX pt))
      (when (< bigX pt) (setf bigX pt))
      (setf pt (aref point-vector (1+ counter)))
      (when (> smallY pt) (setf smallY pt))
      (when (< bigY pt) (setf bigY pt))
      (incf counter 2)
      (when (= counter len)
        (return)))
    (values smallX smallY bigX bigY)))

;;;
;;;  Recompute the points of the polygon from the original points

(define-handler recomputePoints (polygon)
  (let* ((orig-pts (originPointsList me))
         (new-pts (pointsList me))
         (angle (angle me))
         (len (length orig-pts))
         x y)
    (do ((counter 0 (+ counter 2)))
        ((= counter len))
      (multiple-value-setq (x y) 
        (gs:rotate-coords-2d (aref orig-pts counter) 
                          (aref orig-pts (1+ counter)) angle))
      (setf (aref new-pts counter) x
            (aref new-pts (1+ counter)) y))))

;;; 1.0
;;; CHANGING-POLY-RECT -- This macro is called by  handlers that changes the bounds rect of self.
;;;                     It is used to recompute all the points based on their boundsRect. The idea is to
;;;                     keep the relationship between each point and the boundsrect constant.

(defmacro changing-poly-rect ((theActor location? bounds? fill? frame?) &body body)
  (let* ((rect (gensym "RECT-"))
         (old-left (gensym "OLD-LEFT-"))
         (old-top (gensym "OLD-TOP-"))
         (old-right (gensym "OLD-RIGHT-"))
         (old-bottom (gensym "OLD-BOTTOM-")))
    `(gs:let+ ((,rect (gs:node-logicalBoundsRect ,theActor))
            (,old-left (gs:rect-left ,rect))
            (,old-top (gs:rect-top ,rect))
            (,old-right (gs:rect-right ,rect))
            (,old-bottom (gs:rect-bottom ,rect)))
       (gs:making-self-dirty (,theActor ,location? ,bounds? ,fill? ,frame? (gs:node-boundsRegion ,theActor) :bounds t)
         (prog1
           ,@body
           (gs:let+ ((new-left (gs:rect-left ,rect))
                  (new-top (gs:rect-top ,rect))
                  (new-right (gs:rect-right ,rect))
                  (new-bottom (gs:rect-bottom ,rect))
                  (points (pointsList ,theActor))
                  (originalPoints (originPointsList ,theActor))
                  (angle (angle ,theActor))
                  (len (length points))
                  x y hfact vfact)
             ;; If only the location changed, minimize the calculations
             (cond (,location?
                    (setq hfact (- new-left ,old-left)
                          vfact (- new-top ,old-top))
                    ;; Move the points
                    (do ((counter 0 (+ counter 2)))
                        ((= counter len))
                      (incf (aref points counter) hfact)
                      (incf (aref points (1+ counter)) vfact)
                      ;; And also offset the original points. Note that copying the points
                      ;; is not correct because the polygon can be in the middle of a rotation.
                      (incf (aref originalPoints counter) hfact)
                      (incf (aref originalPoints (1+ counter)) vfact)))
                   (t
                    (when (eq (container ,theActor) stage)
                      (recomputePoints ,theActor))
                    (setq hfact (/ (- new-right new-left) (- ,old-right ,old-left))
                          vfact (/ (- new-bottom new-top) (- ,old-bottom ,old-top)))
                    (do ((counter 0 (+ counter 2)))
                        ((= counter len))
                      (setf (aref points counter) 
                            (setq x (+ (* (- (aref points counter) ,old-left) hfact) new-left))
                            (aref points (1+ counter)) 
                            (setq y (+ (* (- (aref points (1+ counter)) ,old-top) vfact) new-top)))
                      (multiple-value-setq (x y)
                        (gs:rotate-coords-2d x y (- angle)))
                      (setf (aref originalPoints counter) x
                            (aref originalPoints (1+ counter)) y))))
             ))))))

;;; 1.0
;;; (SETF BOUNDSRECT) -- sets the boundsrect of self to the rect specified. It basically calls changing-poly-rect
;;;                   to do all the work. The points are recomputed so that their positions relative to their
;;;                   boundsrect is preserved.

(define-handler setBoundsRect (polygon left top right bottom
                                                 &key physical relative justMoving)
  (declare (ignore left top right bottom physical relative))
  (changing-poly-rect (me justMoving t t t)
    (call-next-method))
  (when (gs:hasWindow? (gs:node-flags me))
    (adjust-points-for-stage (getValue 'pointsList me))))

;;; 1.0
;;; MODIFYING-POLY-POINTS -- this macro transforms each pair of original points using
;;;                       ptFunction and places the results in the points vector. This is used for rotation,
;;;                       where we rotate the original points by some angle to get the real points.
;;;
;;; Note that the original points are unaffected and that the boundsRect is recomputed.

(defmacro transforming-originals-to-poly-points (theActor ptFunction)
  (let* ((originalPoints (gensym "ORIGINALPOINTS-"))
         (points (gensym "POINTS-"))
         (counter (gensym "COUNTER-"))
         (bigX (gensym "BIGX-")) (smallX (gensym "SMALLX-"))
         (bigY (gensym "BIGY-")) (smallY (gensym "SMALLY-"))
         (firstX (gensym "FIRSTX-")) (firstY (gensym "FIRSTY-"))
         (x (gensym "X-")) (y (gensym "Y-")) (len (gensym)))
    `(let* ((,points (pointsList ,theActor))
            (,originalPoints (originPointsList ,theActor))
            (,len (length ,points))
            (,counter 0)
            ,bigX ,smallX ,bigY ,smallY ,firstX ,firstY ,x ,y)
       ;; If we are dealing with a window, offset the points lists to be Stage based. 
       (when (gs:hasWindow? (gs:node-flags ,theActor))
         (window-to-stage-points ,theActor))
       (multiple-value-setq (,firstX ,firstY) (funcall ,ptFunction 
                                                       (aref ,originalPoints ,counter) 
                                                       (aref ,originalPoints (1+ ,counter))))
       (setf  (aref ,points ,counter) ,firstX
              (aref ,points (1+ ,counter)) ,firstY
              ,bigX ,firstX
              ,smallX ,firstX
              ,bigY ,firstY
              ,smallY ,firstY
              ,counter 2)
       ;; Copy the coordinates into the vector.
       (loop
         (multiple-value-setq (,x ,y) (funcall ,ptFunction 
                                               (aref ,originalPoints ,counter) 
                                               (aref ,originalPoints (1+ ,counter))))
         ;; Update x value.
         (setf (aref ,points ,counter) ,x)
         (when (> ,x ,bigX) (setf ,bigX ,x))
         (when (< ,x ,smallX) (setf ,smallX ,x)) 
         ;; Update y value.
         (setf (aref ,points (1+ ,counter)) ,y)
         (when (> ,y ,bigY) (setf ,bigY ,y))
         (when (< ,y ,smallY) (setf ,smallY ,y))
         ;; Update counter and check for completion.
         (incf ,counter 2)
         (when (= ,counter ,len)
           (return)))
           ;; if the actor is on the stage, do appropriate updates
       (if (gs:hasWindow? (gs:node-flags ,theActor))
         (update-points-on-stage ,theActor ,smallX ,smallY ,bigX ,bigY)
         ;; otherwise, reset the boundsrect bypassing the boundsRect method.
         (gs:making-self-dirty (,theActor nil t t t (gs:node-boundsRegion ,theActor) :bounds t)
           (set-rectangle-internal ,theActor ,smallX ,smallY ,bigX ,bigY :physical nil))))))

;;; 1.0
;;; OFFSET-POLY-POINTS -- offsets each x point by x and each y point by y in the vector given.

(defun offset-poly-points (point-vector x y)
  (let ((len (length point-vector))
        (counter 0))
    (loop
      (setf (aref point-vector counter) (+ x (aref point-vector counter)))
      (setf (aref point-vector (1+ counter)) (+ y (aref point-vector (1+ counter))))
      (incf counter 2)
      (when (= counter len)
        (return)))))

;;; Given a vector of points expressed in center of gravity coordinates, this function
;;; returns the a list of the same points but in logical coordinates.

(defun offset-coords (thePoints dh dv)
  (let* ((result (gs:vector-to-list thePoints))
         (head result))
    (loop
      (unless head (return result))
      (setf (car head) (+ (car head) dh)
            (cadr head) (+ (cadr head) dv))
      (setf head (cddr head)))))

;;; 1.0
;;; POINTS -- returns the points used to draw the polygon. These are the current points
;;;         (not necessarily the original points).

(define-handler points (polygon &key physical)
  (if (gs:hasWindow? (gs:node-flags me))
    ;; Convert the points to stage coords.
    (let* ((thePoints (gs:vector-to-list (pointsList me)))
           (window-pos (view-position (gs:node-window me)))
           (dh (point-h window-pos))
           (dv (point-v window-pos))
           (counter 0))
      (dolist (pValue thePoints)
        (if (oddp counter)
          (setf (nth counter thePoints) (+ dv pValue))
          (setf (nth counter thePoints) (+ dh pValue)))
        (incf counter))
      thePoints)
    ;; Translate the points into logical coords. The deal with the physical (if wanted).
    (let* ((logicalPoints (gs:vector-to-list (pointsList me))))
      (if physical
        (if (container me)
          (logicalToPhysicalList (container me) logicalPoints)
          (mapcar #'round logicalPoints))
        logicalPoints))))

;;; 1.0
;;; (SETF POINTS) -- this method is used to explicitely set the points of the polygon. Every time this
;;;               happens we store the entered points into the plist as the "original points", and 
;;;               set the angle of rotation to 0. Then, whenever the angle of rotation becomes 0
;;;               these points are used to reset the polygon to its original shape.

(define-handler (setf points) (point-list polygon &key physical)
  (unless (inheritsFrom point-list PolygonPointsList)
    (sk8-error PropertyTypeMismatchError
               :object        point-list
               :expectedType  PolygonPointsList
               :ownerObject   Polygon
               :propertyName 'points))
  (let ((points-vector (gs:list-to-vector point-list))
        (container (gs::node-container me)))
    ;; Make sure the minimum size is not violated. 
    (multiple-value-bind (ll tt rr bb) (get-boundsrect-coords points-vector)
      (sk8-multival-bind (ww hh) (minimumSize me)
        (when (if physical 
                (or (< (- rr ll) ww) (< (- bb tt) hh))
                (or (< (* (- rr ll) (if container (gs:node-xScale container) 1)) ww)
                    (< (* (- bb tt) (if container (gs:node-yScale container) 1)) hh)))
          (sk8-error GeneralProgrammaticError
                     :strings '("The points provided violate the minimumSize restriction.")))))
    ;; Minimum size ok. Proceed. 
    (if (gs:hasWindow? (gs:node-flags me))
      ;; If the polygon is on the stage, update the points....
      (set-points-on-stage me point-list)
      (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds t)
        ;; if physical points given, we need to translate from the stage coordinate
        ;; system to the window coordinate system.
        (when physical
          (setf point-list (gs:stage-to-logical-list (gs:node-container me) point-list))
          (setf points-vector (gs:list-to-vector point-list)))
        ;; Install both vectors.
        (setf (originPointsList me) points-vector)
        (setf (pointsList me) (copy-seq points-vector))
        (setValue 'angle me 0)
        ;; Update the boundsRect.
        (multiple-value-bind (ll tt rr bb) (get-boundsRect-coords points-vector)
          (set-rectangle-internal me ll tt rr bb :resizing t))))))

;;; 1.0
;;; ROTATE -- rotates the polygon about the anchor.   If the anchor is NIL, the polygon is rotated about its location.
;;;          The angle is specified in RADIANS! If relative is t, the angle is added to the current angle of self, 
;;;          otherwise, we rotate to that angle.  Rotations are computed in relation to
;;;          the original points. In this way we avoid chains of rounding errors that can distort the polygon
;;;          beyond recognition.
;;;
;;; Finally, this method calls transforming-originals-to-poly points to do all the work.
;;;
;;; Physical means the anchoring points are given in physical coords.

(define-handler rotate (polygon angle &key relative physical hanchor vanchor)
  ;; Find the logical anchor.
  (if (and hanchor vanchor physical)
    (progn
      (sk8-multival-setf (hanchor vanchor) (gs:stage-to-window-coords me hanchor vanchor))
      (sk8-multival-setf (hanchor vanchor) 
                         (gs:real-physical-to-logical (gs:node-container me) hAnchor vanchor)))
    (sk8-multival-bind (h v) (location me)
      (setf hanchor (or hanchor (hAnchor me) h)
            vanchor (or vanchor (vAnchor me) v))))
  (when relative 
    (setf angle (+ angle (angle me))))
  ;; rotate each point relative to the pivot
  (transforming-originals-to-poly-points
   me
   #'(lambda (x y)
       (multiple-value-setq (x y) (gs:rotate-coords-2d (- x hanchor) (- y vanchor) angle))
       (values (+ x hanchor) (+ y vanchor))))
  ;; update the angle
  (sk8::setValue 'angle me angle))

;;; 1.0
;;; SETREGULARPOLYGON -- transforms self into a regular polygon with numSides sides. The location
;;;                    is preserved.

(define-handler setRegularPolygon (polygon &key 
                                                     (numSides (/ (length (points me)) 2))
                                                     radius (phase gs:1.5pi))
  (when (< numSides 3) 
    (sk8-error GeneralProgrammaticError "The polygon must have at least three sides."))
  (unless radius
    (SK8-multival-bind (height width) (size me :physical nil)
      (setq radius (gs:fhalf (gs::faverage height width)))))
  (gs:let+ ((points nil)
         x y
         (dtheta (/ gs:2pi numSides))
         (theta (+ dtheta phase)))
    (sk8-multival-setf (x y) (location me :physical nil))
    ;; Resize the points vector and compute its new points
    (dotimes (j numSides)
      (push (round (+ y (round (* radius (sin theta))))) points)
      (push (round (+ x (round (* radius (cos theta))))) points)
      (setq theta (+ theta dtheta)))
    ;; Call setf points to do the job.
    (setf (points me) points)))

(defun change-nth-point (point-list n h v)
  (setf (nth (* 2 (1- n)) point-List) h)
  (setf (nth (1+ (* 2 (1- n))) point-List) v)
  point-list)

;;; 1.0
;;; (SETF NTH-POINT) -- Changes one point of the polygon to the values wanted. This
;;;                  call has the same status that setf points has. The angle is reset
;;;                  to zero.

(define-handler setNthPoint (polygon n h v &key physical relative)
  (let* ((thePoints (pointsList me))
         (num-points (/ (length thePoints) 2)))
    ;; Error checking...
    (cond ((> n num-points)
           (sk8-error GeneralProgrammaticError
                      :strings '("Cannot set point number " " since " " only has " " points.")
                      :objects (list n me num-points)))
          ((zerop n) (sk8-error GeneralProgrammaticError :strings '("The first point is point number 1."))))
    (if (gs:hasWindow? (gs:node-flags me))
      (set-points-on-stage me (change-nth-point (points me) n h v))
      (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds t)
        (setq n (* 2 (1- n)))
        ;; Cases:
        (if physical
          (let ((container (gs:node-container me)))
            (if relative
              ;; we get the physical point from the logical, offset by the required
              ;; values, and finally turn the numbers back to the logical values.
              (SK8-multival-bind (physx physy) (gs:real-logical-to-physical container h v)
                (incf physx h)
                (incf physy v)
                (SK8-multival-setf (h v) (gs:real-physical-to-logical container physx physy))
                (setf (aref (the vector thePoints) n) h
                      (aref (the vector thePoints) (1+ n)) v))
              ;; Physical but not relative.
              (let ((clos-window (gs:node-window me)))
                (when clos-window
                  (let ((pos (view-position clos-window)))
                    (setf h (- h (point-h pos))
                          v (- v (point-v pos)))))
                (SK8-multival-setf (h v) (gs:real-physical-to-logical container h v))
                (setf (aref (the vector thePoints) n) h
                      (aref (the vector thePoints) (1+ n)) v))))
          ;; Logical: no problem!
          (cond
           (relative (setf (aref (the vector thePoints) n) (setf h (+ h (aref (the vector thePoints) n))))
                     (setf (aref (the vector thePoints) (1+ n)) (setf v (+ v (aref (the vector thePoints) (1+ n))))))
           (t (setf (aref (the vector thePoints) n) h
                    (aref (the vector thePoints) (1+ n)) v))))
        ;; And change the rectangle!!!
        (multiple-value-bind (ll tt rr bb) (get-boundsrect-coords thePoints)
          (set-rectangle-internal me ll tt rr bb :resizing t))
        ;; Now update the other vector and set the angle to 0.
        (let ((originalPoints (originPointsList me)))
          (if (zerop (angle me))
            (progn
              (setf (aref (the vector originalPoints) n) h)
              (setf (aref (the vector originalPoints) (1+ n)) v))
            ;; If the angle was not 0 we need to copy the points into the original points also.
            (setf (originPointsList me) (copy-seq thePoints)))
          (setValue 'angle me 0))))))

(define-handler (setf nthPoint) (point polygon n &key physical relative)
  (definitely-recover-list point)
  (sk8-destructuring-bind (x y) point
    (setNthPoint me n x y :physical physical :relative relative)))

;;; 1.0
;;; NTH-POINT -- returns the nth point of me.

(define-handler nthPoint (polygon n &key)
  (let* ((points (pointsList me))
         (num-points (/ (length points) 2)))
    (cond ((> n num-points)
           (sk8-error GeneralProgrammaticError
                      :strings '("Cannot get point number " " since " " only has " " points.")
                      :objects (list n me num-points)))
          ((zerop n)
           (sk8-error GeneralProgrammaticError :strings '("The first point is point number 1."))))
    (setq n (* 2 (1- n)))
    (SK8-multivals (aref (the vector points) n) (aref (the vector points) (1+ n)))))

;;; ________________________
;;; SUPPORT FUNCTIONS TO ALLOW POLYGONS TO SIT ON THE STAGE!!!
;;; ________________________

;;; Transforms point vectors in window coords (0 based) to the real stage values.

(defun window-to-stage-points (theActor)
  (let* ((theWindow (gs:node-window theActor))
         (points (pointsList theActor))
         (originalPoints (originPointsList theActor))
         (pos (view-position theWindow)))
    (offset-poly-points points (point-h pos) (point-v pos))
    (offset-poly-points originalPoints (point-h pos) (point-v pos))))
    
;;;
;;; Adjust points so that they are in proper stage coordinates

(defun adjust-points-for-stage (pts &optional sx sy &aux len)
  (unless (and sx sy)
    (multiple-value-setq (sx sy) (get-top-left-points pts)))
  (setq len (length pts))
  (do ((counter 0 (+ counter 2)))
      ((= counter len))
    (decf (aref pts counter) sx)
    (decf (aref pts (1+ counter)) sy)))

;;;
;;; Sync the points in the datastructures with those on the screen.
;;; THIS SHOULD ONLY BE CALLED WHEN THE POLYGON IS SITTING ON THE STAGE

(defun update-points-on-stage (thePolygon &optional smallX smallY bigX bigY)
  (let* ((flags (gs:node-flags thePolygon))
         (newPoints (pointsList thePolygon))
         (originalPoints (originPointsList thePolygon))
         (clos-window (gs:node-window thePolygon))
         width height)
    (unless (and smallX smallY bigX bigY)
      (multiple-value-setq (smallX smallY bigX bigY) (get-boundsRect-coords newPoints)))
    ;; move the origin to the top-left of the window
    (adjust-points-for-stage newPoints)
    (adjust-points-for-stage originalPoints)
    (setf (pointsList thePolygon) newPoints)
    (setq smallX (gs:f.round smallX) smallY (gs:f.round smallY) 
          bigX (gs:f.round bigX) bigY (gs:f.round bigY))
    (setf width (- bigX smallX)
          height (- bigY smallY))
    (gs:simple-set-physical-rect-size (gs:node-physicalBoundsRect thePolygon) width height)
    (gs:simple-set-rect-size (gs:node-logicalBoundsRect thePolygon) width height)
    ;; RESIZING THE WINDOW (from setf-tla-boundsRect).
    (gs:boundsDirty! flags)
    (gs:fillDirty! flags)
    (gs:frameDirty! flags)   
    (setWindowRect clos-window smallX smallY bigX bigY)
    (gs:SK8-update-window-gWorld clos-window :flushBits t)
    ;; Redraw!!!
    (gs:making-self-dirty (thePolygon nil t t t (gs:node-boundsRegion thePolygon) :bounds nil)
      (resized thePolygon))))

;;; set-points-on-stage -- sets the points when the polygon is on the stage. Does extra work to
;;;                   keep the window in synch with the polygon.
;;;                   (1) compute the boundsRect required to inscribe polygon.
;;;                   (2) offset the points to logical coords within the rect.
;;;                   (3) Set the points and the rect.
;;;                   (4) Resize the window.

(defun set-points-on-stage (thePolygon point-list)
  (let* ((flags (gs:node-flags thePolygon))
         (newPoints (gs:list-to-vector point-list))
         (clos-window (gs:node-window thePolygon))
         width height)
    (setf (originPointsList thePolygon) (copy-seq newPoints))
    (multiple-value-bind (smallX smallY bigX bigY)
                         (get-boundsRect-coords newPoints)
      (setq smallX (gs:f.round smallX) smallY (gs:f.round smallY)
            bigX (gs:f.round bigX) bigY (gs:f.round bigY))
      ;; Now use smallX and smallY to update the points.
      (adjust-points-for-stage newPoints smallX smallY)
      ;; Set the points.
      (setf (pointsList thePolygon) newPoints)
      (setf (angle thePolygon) 0)
      ;; Set the rects.
      (setf width (- bigX smallX)
            height (- bigY smallY))
      (gs:simple-set-physical-rect-size (gs:node-physicalBoundsRect thePolygon) width height)
      (gs:simple-set-rect-size (gs:node-logicalBoundsRect thePolygon) width height)
      ;; RESIZING THE WINDOW (from setf-tla-boundsRect).
      (gs:boundsDirty! flags)
      (gs:fillDirty! flags)
      (gs:frameDirty! flags)   
      (setWindowRect clos-window smallX smallY bigX bigY)
      (gs:SK8-update-window-gWorld clos-window :flushBits t)
      ;; Redraw!!!
      (gs:making-self-dirty (thePolygon nil t t t (gs:node-boundsRegion thePolygon) :bounds nil)
        (resized thePolygon)))))

(define-handler convert-to-tla-regions :private (polygon width height)
  (declare (ignore width height))
  (makeBoundsRegion me)
  ;; First we save some information needed to do the right offseting below.
  (let* ((old-phys-rect (gs:recompute-physicalBoundsrect me))
         (old-phys-left (gs:rect-left old-phys-rect))
         (old-phys-top (gs:rect-top old-phys-rect)))
    (sk8-multival-bind (ll tt rr bb) (boundsRect me)
      (declare (ignore bb rr))
      (call-next-method)
      ;; The rects are fine! Now offset the regions and the points.
      (let ((pointsList (pointsList me))
            dx dy)
        ;; We shift the regions so that the topLeft of the boundsRect of the bounds is {0,0}.
        (setf dx (- old-phys-left)
              dy (- old-phys-top))
        (#_offsetRgn (gs:node-boundsRegion me) dx dy)
        (#_offsetRgn (gs:node-fillRegion me) dx dy)
        (#_offsetRgn (gs:node-frameRegion me) dx dy)
        ;; We shift the logical points so that they are circumscribed by the logical 
        ;; boundsRect which now has its topLeft corner at {0,0}.
        (setf dx (- ll) 
              dy (- tt))
        (offset-poly-points pointsList dx dy)))))

(define-handler revert-to-nontla-regions :private (polygon clos-window)
  (call-next-method)
  ;; The rects are fine! Now offset the regions and the points.
  (let* ((pointsList (pointsList me))
         (pos (view-position clos-window))
         (dx (point-h pos))
         (dy (point-v pos)))
    (#_offsetRgn (gs:node-boundsRegion me) dx dy)
    (#_offsetRgn (gs:node-fillRegion me) dx dy)
    (#_offsetRgn (gs:node-frameRegion me) dx dy)
    ;; And now the points...
    (offset-poly-points pointsList dx dy)))

;;; A simplified version of changing-poly-rect. Updates the points to the right values.

(define-handler prepare-for-wdef-region-calculation :private (polygon width height)
  (declare (ignore width height))
  (gs:let+ ((rect (gs:node-logicalBoundsRect me))
         (old-right (gs:rect-right rect))
         (old-bottom (gs:rect-bottom rect)))
    (call-next-method)
    (gs:let+ ((new-right (gs:rect-right rect))
           (new-bottom (gs:rect-bottom rect))
           (xfactor (/ new-right old-right))
           (yfactor (/ new-bottom old-bottom))
           (points (pointsList me))
           (counter 0)
           (len (length points))
           x y)
      ;; Updating the points.
      (loop
        (setf x (aref points counter)
              y (aref points (1+ counter)))
        (setf (aref points counter) (* x xfactor)
              (aref points (1+ counter)) (* y yfactor))
        (incf counter 2)
        (when (= counter len)
          (return))))))

(setf (points polygon) '(53 113 38 66 78 37 118 66 103 113))

;;; _______________________________ 
;;; Changing parents.
;;; _______________________________ 

;;; Given a list of points and two rects (lists) it returns a the points mapped to the
;;; new rect. The assumption is that the points are inscribed in the oldRect and that
;;; everything is expressed in the same set of coordinates.

(defun rectMapPoints (points oldRect newRect)
  (destructuring-bind (oldLeft oldTop oldRight oldBottom) oldRect
    (destructuring-bind (newLeft newTop newRight newBottom) newRect
      (let* ((oldWidth (- oldRight oldLeft)) (oldHeight (- oldBottom oldTop))
             (newWidth (- newRight newLeft)) (newHeight (- newBottom newTop))
             (xFactor (/ newWidth oldWidth))
             (yFactor (/ newHeight oldHeight))
             x y)
        ;; Update the points list.
        (do ((newPoints points (cddr newPoints)))
            ((null newPoints) points)
          (setf x (car newPoints)
                y (cadr newPoints))
          (setf (car newPoints) (+ newLeft (* (- x oldLeft) xfactor))
                (cadr newPoints) (+ newTop (* (- y oldTop) yfactor))))))))
    
(defun sk8-rect-to-list (sk8Rect)
  (list (gs:rect-left sk8Rect) (gs:rect-top sk8Rect)
        (gs:rect-right sk8rect) (gs:rect-bottom sk8Rect)))
  
(defun initialize-for-new-polygon-parent (newDad me oldParents)
  (unless (some #'(lambda (x) (inheritsFrom x Actor)) oldparents)
    (initialize-for-new-actor-parent newDad me))
  ;; copy the points.
  (setf (pointsList me) (copy-seq (pointsList newDad)))
  (setf (originPointsList me) (copy-seq (originPointsList newDad)))
  ;; get the new points.
  (let ((points (points newDad))
        (oldRect (boundsRect newDad))
        (newRect (boundsRect me)))
    (rectMapPoints points oldRect newRect)
    (setf (points me) points)))
    
;;; After the actor method completes the boundsRect is set to the right place. We have to map the points
;;; to the logical boundsRect (0 based if the polygon is a window).

(define-handler AddedMeAsParent (polygon child oldparents)
  (gs:let+ ((oldBounds (:region)))
    ;; If the thing was already an actor, invalidate its bounds.
    (when (some #'(lambda (x) (inheritsFrom x Actor)) oldparents)
      (gs:recompute-bounds-region child (gs:node-flags child))
      (#_copyRgn (gs:node-boundsRegion child) oldBounds))
    (unless (some #'(lambda (x) (inheritsFrom x polygon)) oldparents)
      (gs:making-self-dirty (child t nil nil nil oldBounds :bounds t)
        (initialize-for-new-polygon-parent me child oldParents)))))

;;; _______________________________ 
;;; Editable properties. 
;;; _______________________________ 

(define-handler localVirtualProperties (polygon)
  (when (eq me polygon)
    '(points)))

#|
	Change History (most recent last):
	19	8/13/93	kleiman	Rewrite:  now inherits from Blob
	20	10/1/93	hernan	:generator defaultLineGenerator
	21	11/1/93	hernan	This is the end of this nonsense: polygon becomes
				an actor again!!! Still need to do the fast slot thing.
	22	11/6/93	rod	Changed the setting of the points.  That's it.
	23	11/19/93	hernan	sv-inherited -> getValue and
				set-sv-inherited -> setValue.
	24	11/29/93	chip	Fixed several VALUES/SK8-MULTIVALS mismatches!
	25	1/11/94	hernan	self -> me
	26	1/14/94	hernan	Labeling private properties.
	27	1/14/94	hernan	Adding the newParentInitialize stuff.
	28	1/14/94	kleiman	Removing newParentInitialize.3
	29	1/17/94	hernan	Removing newParentInitialize stuff out of here 
				and into its own file.
	30	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	31	3/3/94	Hernan	The great handler argument name renaming of 94!
	32	3/3/94	kleiman	private properties declared via makeprivateproperty
	33	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	34	3/21/94	kleiman	setvalue -> sk8::setValue
	35	4/5/94	Hernan	Moving the default location of the polygon closer
				to the topleft.
	36	4/5/94	kleiman	code review: approved for public API
	37	4/5/94	kleiman	took out compiler warnings, fixing any bugs
	38	4/8/94	Hernan	Hanchor and VAnchor made available as args to
				rotate.
	39	4/15/94	Hernan	Corrected some messed up error messages.
	40	7/27/94	Hernan	1176911: changing the error message in set-anchor
				to something more understandable.
	41 	 9/16/94	Hernan  	Removed from makeBoundsRegion a stupid line
							that force the frame and fill to recompute no
							matter what!!!
	42 	 9/28/94	Hernan  	New region computing methods. Moved offsetting
							code to lower level. The user no longer has to do
							anything to make use of the optimization.
	43 	10/ 7/94	Hernan  	Fixing stage-to-logical-list to stop using the container.
	44 	10/19/94	Hernan  	Setf points should translate the points into the
							logical space of the container.
	45 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	46 	 2/ 2/95	Hernan  	Got rid of pivot, magnification and origin. This
							makes the implementation simpler to the point
							that it really works...
	47 	 2/ 2/95	Hernan  	Removing leftover junk.
	48 	 2/ 2/95	Hernan  	Removing references to cm-coords and the pivot.
	49 	 2/10/95	Hernan  	Allowed rotation on the Stage and fixed setNthPoint
							to change the rect when one point changes.
	50 	 2/10/95	Hernan  	Fixing typo.
	51 	 2/16/95	sidney  	readable argument names for initialize handler
	2  	 6/23/95	Hernan  	1249810: changing-poly-rect was not updating the
						originalPoints when the location changed.
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3   5/ 6/96Hernan  1294326: set points does not let you set the points to some
						list that violates the minimum size of the Polygon.
|# ;(do not edit past this line!!)
