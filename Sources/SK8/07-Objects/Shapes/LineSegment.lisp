(in-package :sk8dev)

(provide "LINESEGMENT")

(require "ACTOR")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; The LineSegment, no longer has anything to do with a polygon. This will make it more light
;;; weight as well as simpler to implement.

(new Actor :objectname "LineSegment" :project sk8
     :prototype t
     :properties '((lineSize :value 1) ;; Physical "width" of the line.
                   (endpoints :value (10 10 100 100)) ;; In logical coords.
                   (angle :value 0)))

(define-handler initialize (LineSegment original isNew initArgs)
  (declare (ignore isNew initArgs original))
  (call-next-method)
  ;; Copy the points.
  (setf (slot-value me 'endpoints) (copy-list (slot-value me 'endpoints)))
  t)

;;; The line does not allow any contents since they cannot be drawn.

(define-handler newContentOk (LineSegment newContent)
  (declare (ignore newContent))
  nil)

;;; __________________
;;; Regions.
;;; __________________

;;; The line's fill and frame are both empty. The bounds is used for rendering.

(#_setEmptyRgn (gs:node-fillRegion LineSegment))
(#_setEmptyRgn (gs:node-frameRegion LineSegment))

(define-handler makeFillRegion (LineSegment)
  (gs:fillDirty! (gs:node-flags me) 0))

(define-handler makeFrameRegion (LineSegment)
  (gs:frameDirty! (gs:node-flags me) 0))

(defmacro draw-positive-slope-line ()
  `(progn
     (#_moveTo startx starty)
     (#_Line line-width 0)
     (#_LineTo endx (+ endy line-height))
     (#_Line 0 (- line-height))
     (#_Line (- line-width) 0)
     (#_LineTo startx (- starty line-height))
     (#_Line 0 line-height)))

(defmacro draw-negative-slope-line ()
  `(progn
     (#_MoveTo startx starty)
     (#_Line line-width 0)
     (#_LineTo endx (- endy line-height)) 
     (#_Line 0 line-height)
     (#_Line (- line-width) 0)
     (#_LineTo startx (+ starty line-height))
     (#_Line 0 (- line-height))))

;;; The line has to be inscribed in the boundsRect. The method below
;;; enforces this. The problem is that when the vertical or horizontal
;;; difference between the endpoints differs by less than the lineSize
;;; there a discontiguous jump in how the line is drawn. This is not a problem
;;; for lines of size 1. 

(define-handler makeBoundsRegion (LineSegment)
  (gs:let+ ((flags (gs:node-flags me))
         (line-width (lineSize me))
         (line-height line-width)
         (temp-region (:region))
         temp startx starty endx endy)
    ;; translate the start point to physical coords.
    (sk8-multival-bind (sx sy ex ey) (endpoints me :physical t)
      (sk8-multival-setf (startx starty endx endy) 
                         (gs:stage-to-window-rect-coords me sx sy ex ey)))
    ;; swap if necessary.
    (when (> startx endx)
      (setq temp startx startx endx endx temp
            temp starty starty endy endy temp))
    ;; Build the region!
    (#_OpenRgn)
    (cond
     ;; Case 0: horizontal or vertical: use boundsRect. 
     ((or (< (abs (- starty endy)) line-height)
          (< (abs (- startx endx)) line-width))
      (gs:let+ ((rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me))))
        ;; If the boundsRect could produce an empty region, do not allow 
        ;; it to happen.
        (when (= (rref rect :rect.left) (rref rect :rect.right))
          (rset rect :rect.right (+ line-width (rref rect :rect.right))))
        (when (= (rref rect :rect.top) (rref rect :rect.bottom))
          (rset rect :rect.bottom (+ line-width (rref rect :rect.bottom))))
        (#_frameRect rect)))
     ;; Case 1: a positive slope.
     ((> starty endy)
      (draw-positive-slope-line))
     ;; Case 2: a negative slope.
     (t
      (draw-negative-slope-line)))
    (#_CloseRgn temp-region)
    (#_copyRgn temp-region (gs:node-boundsRegion me))
    (gs:boundsDirty! flags 0)))

;;; __________________
;;; Draw Function.
;;; __________________

;;; The draw function! Just renders the framecolor into the bounds.

(defun draw-segment (theLine thePort draw-region)
  (declare (ignore draw-region))
  (render (gs:node-framecolor theLine) theLine (gs:node-boundsRegion theLine) thePort))

(setf (gs:node-drawFunction LineSegment) 'draw-segment)

;;; __________________
;;; EndPoints.
;;; __________________

(define-handler endpoints (LineSegment &key physical)
  (let ((endPoints (slot-value me 'endpoints)))
    (cond ((gs:hasWindow? (gs:node-flags me))
           (sk8-multival-bind (h v) (gs:window-topleft-offset me)
             (setf endpoints (gs:offset-point-list (copy-list endpoints) h v))))
          (physical
           (let ((container (gs:node-container me)))
             (when container
               (setf endpoints (apply #'logicalToPhysicalRect container endPoints))))))
    (if physical
      ;; And round them all!
      (sk8-multivals (gs:f.round (car endpoints)) (gs:f.round (cadr endpoints))
                     (gs:f.round (caddr endpoints)) (gs:f.round (cadddr endpoints)))
      (sk8-multivals (first endpoints) (second endpoints)
                     (third endpoints) (fourth endpoints)))))

(defun get-boundsRect-from-endpoints (theLine h1 v1 h2 v2)
  (declare (ignore theLine))
  (let (ll tt rr bb)
    (if (< h1 h2)
      (if (< v1 v2)
        (setf ll h1 tt v1 rr h2 bb v2)
        (setf ll h1 tt v2 rr h2 bb v1))
      (if (< v1 v2)
        (setf ll h2 tt v1 rr h1 bb v2)
        (setf ll h2 tt v2 rr h1 bb v1)))
    ;; Now return the right thing for horizontal or vertical lines.
    (when (= ll rr) (incf rr 1))
    (when (= tt bb) (incf bb 1))
    (sk8-multivals ll tt rr bb)))

(define-handler setEndpoints (LineSegment h1 v1 h2 v2 &key physical relative
                                                   justMoving)
  (if (gs:hasWindow? (gs:node-flags me))
    (set-segment-points-on-stage me h1 v1 h2 v2 :relative relative :justMoving justMoving)
    (let ((endPoints (slot-value me 'endpoints))
          container)
      ;; If points are physical, translate to logical.
      (when physical
        (if relative
          (when (setf container (gs:node-container me))
            (setf h1 (/ h1 (gs:node-xScale container))
                  h2 (/ h2 (gs:node-xScale container))
                  v1 (/ v1 (gs:node-YScale container))
                  v2 (/ v2 (gs:node-yScale container))))
          (sk8-multival-setf (h1 v1 h2 v2) (gs:user-to-logical-rect-coords (gs:node-container me) h1 v1 h2 v2))))
      (when relative 
        (setf h1 (+ (first Endpoints) h1)
              v1 (+ (second Endpoints) v1)
              h2 (+ (third endpoints) h2)
              v2 (+ (fourth endpoints) v2)))
      ;; Set the slot. In place. No consing.
      (setf (first endpoints) h1
            (second endpoints) v1
            (third endpoints) h2
            (fourth endpoints) v2)
      ;; Update the rects.
      (sk8-multival-bind (left top right bottom) 
                         (get-boundsRect-from-endpoints me h1 v1 h2 v2)
        (set-rectangle-internal me left top right bottom :resizing (not justMoving)))
      ;; Redraw.
      (gs:making-self-dirty (me justMoving t t t (gs:node-boundsRegion me) :bounds t)
        ;; Clear the angle.
        (setf (slot-value me 'angle) 0)
        ))))

(define-handler (setf endPoints) (points LineSegment &key physical relative)
  (definitely-recover-list points)
  (sk8-destructuring-bind (x1 y1 x2 y2) points
    (setEndpoints me x1 y1 x2 y2 :physical physical :relative relative))
  points)

(define-handler startPoint (LineSegment &key physical)
  (sk8-multival-bind (h1 v1 h2 v2) (endpoints me :physical physical)
    (declare (ignore h2 v2))
    (sk8-multivals h1 v1)))

(define-handler setStartPoint (LineSegment h v &key physical relative)
  (sk8-multival-bind (h1 v1 h2 v2) (endPoints me :physical physical)
    (if relative
      (setEndpoints me (+ h1 h) (+ v1 v) h2 v2 :physical physical)
      (setEndpoints me h v h2 v2 :physical physical))))

(define-handler (setf startPoint) (location LineSegment &key physical relative)
  (definitely-recover-list location)
  (sk8-destructuring-bind (x y) location
    (setStartPoint me x y :physical physical :relative relative)))

(define-handler endPoint (LineSegment &key physical)
  (sk8-multival-bind (h1 v1 h2 v2) (endpoints me :physical physical)
    (declare (ignore h1 v1))
    (sk8-multivals h2 v2)))

(define-handler setEndPoint (LineSegment h v &key physical relative)
  (sk8-multival-bind (h1 v1 h2 v2) (endPoints me :physical physical)
    (if relative
      (setEndpoints me h1 v1 (+ h2 h) (+ v2 v) :physical physical)
      (setEndpoints me h1 v1 h v :physical physical))))

(define-handler (setf endPoint) (location LineSegment &key physical relative)
  (definitely-recover-list location)
  (sk8-destructuring-bind (x y) location
    (setEndPoint me x y :physical physical :relative relative)))

;;; __________________
;;; BoundsRect.
;;; __________________

;;; Figures out the change in location and moves the points appropriately.

(define-handler setLocation (lineSegment h v &key physical relative)
  (if (gs:hasWindow? (gs:node-flags me))
    (call-next-method)
    (if relative
      (setEndpoints me h v h v :relative t :physical physical :justMoving t)
      (let (dh dv)
        (sk8-multival-bind (oldH oldV) (location me :physical physical)
          ;; Figure out the offset.
          (setf dh (- h oldH)
                dv (- v oldV))
          ;; And move the points.
          (setEndPoints me dh dv dh dv :physical physical :relative t :justMoving t))))))


;;; (SETF BOUNDSRECT) -- changes the boundsRect by matching the appropriate vertices of the rect
;;;                   to the appropriate endpoints. 
;;;
;;; The ugly ending of this function is due to the problem that the endpoints do not really lie on the
;;; boundsRect, but inside of it, to allow the vertices of the line to be on the boundsRect. Letting the
;;; endpoints be on the rectangle makes the line grow a few pixels each time we drag it. We fix this
;;; problem by insetting the line by the right amount (whatever the inset used to be before this change).
;;; And we need to compute this amount for both vertices...

(define-handler setBoundsRect (LineSegment left top right bottom 
                                                     &key physical relative justMoving)
  (if justMoving
    (if relative
      (setEndpoints me left top left top :physical physical :relative t :justMoving t)
      ;; Compute the offset and call setEndpoints to do the work.
      (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
        (declare (ignore rr bb))
        (let ((dh (- left ll))
              (dv (- top tt)))
          (setEndpoints me dh dv dh dv :physical physical :relative t :justMoving t))))
    ;; If we are resizing, not relative and the boundsRect given is not valid,
    ;; we arbitrarily make it valid. We are inconsistent with the rest of the system where
    ;; trying an invalid rectangle just returns, because it is very annoying to do this
    ;; with lines.
    (progn
      (when (and (not justMoving) (not relative))
        (when (> left right) (setf right left))
        (when (> top bottom) (setf bottom top)))
      (let* ((oldRect (gs:recompute-logicalBoundsRect me))
             (oldLeft (gs:rect-left oldRect))
             (oldTop (gs:rect-top oldRect))
             (oldRight (gs:rect-right oldRect))
             (oldBottom (gs:rect-bottom oldRect)))
        (if relative
          (setf left (+ oldLeft left) top (+ oldTop top)
                right (+ oldRight right) bottom (+ oldBottom bottom))
          (when physical
            (sk8-multival-setf (left top right bottom)
                               (gs:user-to-logical-rect-coords me left top right bottom))))
        ;; OK: we have the rect. Let us now fix the end points.
        (sk8-multival-bind (sx sy ex ey) (endpoints me)
          (let (dx dy ddx ddy)
            (if (<= sx ex)
              (if (<= sy ey)
                (progn
                  (setf dx (- sx oldLeft) dy (- sy oldTop)
                        ddx (- ex oldRight) ddy (- ey oldBottom))
                  (setEndPoints me (+ left dx) (+ top dy) (- right ddx) (- bottom ddy)
                                :justMoving justMoving))
                (progn
                  (setf dx (- sx oldLeft)
                        dy (- ey oldTop))
                  (setEndPoints me (+ left dx) (- bottom dy) (- right dx) (+ top dy) 
                                :justMoving justMoving)))
              (if (<= sy ey)
                (progn
                  (setf dx (- ex oldLeft)
                        dy (- sy oldTop))
                  (setEndPoints me (- right dx) (+ top dy) (+ left dx) (- bottom dy) 
                                :justMoving justMoving))
                (progn
                  (setf dx (- ex oldLeft)
                        dy (- ey oldTop))
                  (setEndPoints me (- right dx) (- bottom dy) (+ left dx) (+ top dy)
                                :justMoving justMoving))))))))))

;;; ____________
;;; Resizing.
;;; ____________

(defun live-line-resize (theLine startX startY)
  (Setq startX (gs:f.round startX)
        startY (gs:f.round startY))
  (let* ((endX startX)
         (endY startY)
         (oldX startX)
         (oldY startY))
    ;; loop to drag it out
    (loop
      ;; return if the mouse is up
      (when (not (mouse-down-p))
        (return))
      (sleep (/ 60))
      ;; get the new mouseLoc
      (SK8-multival-setf (endX endY) (mouseLoc stage))
      ;; constrain to straight line
      ;; changed?
      (when (or (/= endX oldX)
                (/= endY oldY))
        (setEndpoints theLine startX startY endX endY :physical t)
        (setq oldX endX
              oldY endY)))
    (sk8-multiVals startX startY endX endY)))

(defun getLineFromUser (startX startY)
  (without-interrupts
   (Setq startX (round startX)
         startY (round startY))
   (let* ((endX startX)
          (endY startY)
          (oldX startX)
          (oldY startY))
     ;; draw it
     (with-port (gs:get-wmgr-port)
       (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (#_MoveTo startX startY)
       (#_LineTo endX endY))
     ;; loop to drag it out
     (loop
       ;; return if the mouse is up
       (when (not (mouse-down-p))
         (return))
       (sleep (/ 60))
       ;; get the new mouseLoc
       (SK8-multival-setf (endX endY) (mouseLoc stage))
       ;; constrain to straight line
       ;; changed?
       (when (or (/= endX oldX)
                 (/= endY oldY))
         (with-port (gs:get-wmgr-port)
           (#_PenMode #$PatXor)
           (#_PenPat *gray-pattern*)
           (#_MoveTo startX startY)
           (#_LineTo oldX oldY)
           (#_MoveTo startX startY)
           (#_LineTo endX endY))
         (setq oldX endX
               oldY endY)))
     ;; erase it
     (with-port (gs:get-wmgr-port)
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (#_MoveTo startX startY)
       (#_LineTo endX endY))
     ;; return it
     (#_FlushEvents #$everyEvent 0)
     (sk8-multiVals startX startY endX endY))))

;;; Unless otherActors are passed, the line is resized using a getLineFromUser sort of thing.

(define-handler resize (LineSegment how &key (live (if (option-key-p) (not *live*) *live*)) otherActors)
  (declare (ignore how))
  (if otherActors
    (call-next-method)
    (sk8-multival-bind (sh sv eh ev) (endpoints me :physical t)
      (sk8-multival-bind (mh mv) (mouseloc stage)
        (let (hanchor vanchor)
          (if (> (gs:coords-distance mh mv sh sv) (gs:coords-distance mh mv eh ev))
            (setf hAnchor sh 
                  vAnchor sv)
            (setf hAnchor eh 
                  vAnchor ev))
          (if (and live (not (gs:hasWindow? (gs:node-flags me))))
            (live-line-resize me hAnchor vAnchor)
            (progn
              (sk8-multival-setf (sh sv eh ev) (getLineFromUser (gs:f.round hAnchor) (gs:f.round vAnchor)))
              (setEndpoints me sh sv eh ev :physical t))))))))

;;; __________________
;;; Misc.
;;; __________________

;;; (SETF FRAMECOLOR) -- sets the framecolor of lineSegment to color. All the work is done by the actor's method. 
;;;                   However, since the simple lineSegment 
;;;                   has no fill region, the dirtying mechanism used by that method is useless. So, we
;;;                   wrap that call in a making-self-dirty which uses the right region.

(define-handler (setf framecolor) (color LineSegment)
  (declare (ignore-if-unused color))
  (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds t)
    (call-next-method)))

(define-handler (setf lineSize) (newSize LineSegment)
  (ensureType newSize integer)
  (setf (slot-value me 'lineSize) newSize)
  (if (gs:hasWindow? (gs:node-flags me))
    ;; Forces the window to recompute its bounds.
    (sk8-multival-bind (h1 v1 h2 v2) (endpoints me)
      (set-segment-points-on-stage me h1 v1 h2 v2 :justMoving nil))
    (gs:making-self-dirty (me nil t nil nil (gs:node-boundsRegion me) :bounds t)
      )))

;;; __________________
;;; Putting segments on the stage.
;;; __________________

;;; When a segment is placed on the stage, the following happens.
;;; 1. As usual with an actor, the rects are offseted so that topLeft = 0.
;;; 2. The endpoints are offseted too, to the points in window based coords.

(define-handler convert-to-tla-regions :private (LineSegment width height)
                (declare (ignore width height))
                ;; Offset the points.
                (sk8-multival-bind (ll tt rr bb) (boundsRect me)
                  (declare (ignore rr bb))
                  (let ((endpoints (slot-value me 'endpoints)))
                    (decf (first endpoints) ll)
                    (decf (second endpoints) tt)
                    (decf (third endpoints) ll)
                    (decf (fourth endpoints) tt)))
                ;; Change the rects.
                (call-next-method))

(define-handler revert-to-nontla-regions :private (LineSegment clos-window)
                ;; Offset the points.
                (let* ((pos (view-position clos-window))
                       (left (point-h pos))
                       (top (point-v pos))
                       (endpoints (slot-value me 'endpoints)))
                  (incf (first endpoints) left)
                  (incf (second endpoints) top)
                  (incf (third endpoints) left)
                  (incf (fourth endpoints) top))
                ;; Change the rects.
                (call-next-method))

(defun set-segment-points-on-stage (theSegment h1 v1 h2 v2 &key relative justMoving)
  ;; First adjust the values so that they are the physical endpoints.
  (when relative
    (let ((endpoints (endpoints theSegment :physical t)))
      (setf h1 (+ h1 (first endpoints))
            v1 (+ v1 (second endpoints))
            h2 (+ h2 (third endpoints))
            v2 (+ v2 (fourth endpoints)))))
  (if justMoving
    ;; Just move the window!
    (sk8-multival-bind (smallX smallY bigX bigY) (get-boundsRect-from-endpoints theSegment h1 v1 h2 v2)
      (declare (ignore bigX bigY))
      (set-view-position (gs:node-window theSegment) (gs:f.round smallX) (gs:f.round smallY)))
    ;; Now do the window stuff.
    (let* ((flags (gs:node-flags theSegment))
           (clos-window (gs:node-window theSegment))
           width height)
      (sk8-multival-bind (smallX smallY bigX bigY) (get-boundsRect-from-endpoints theSegment h1 v1 h2 v2)
        (setq smallX (gs:f.round smallX) smallY (gs:f.round smallY)
              bigX (gs:f.round bigX) bigY (gs:f.round bigY))
        ;; Now use smallX and smallY to update the points.
        (setf (slot-value theSegment 'endpoints)
              (gs:offset-point-list (list h1 v1 h2 v2) (- smallX) (- smallY))) ;; CONSES!
        ;; Set the rects.
        (setf width (max 1 (- bigX smallX))
              height (max 1 (- bigY smallY)))
        (gs:simple-set-physical-rect-size (gs:node-physicalBoundsRect theSegment) width height)
        (gs:simple-set-rect-size (gs:node-logicalBoundsRect theSegment) width height)
        ;; RESIZING THE WINDOW (from setf-tla-boundsRect).
        (gs:boundsDirty! flags)
        (setf (slot-value clos-window 'mf::drawEnabled) nil)
        (setWindowRect clos-window smallX smallY (max bigX (1+ smallX)) (max bigY (1+ smallY)))
        (gs:SK8-update-window-gWorld clos-window :flushBits t)
        ;; Redraw!!!
        (gs:making-self-dirty (theSegment nil t nil nil (gs:node-boundsRegion theSegment) :bounds nil)
          (resized theSegment)
          (setf (slot-value clos-window 'mf::drawEnabled) t))))))

;;; _____________________
;;; Rotation
;;; _____________________

;;; Since all the state we have is the current one, all rotations are done relative
;;; to it. 
;;; Anchor point can be false (use location), 'start or 'end.

(define-handler rotate (lineSegment angle &key relative anchorPoint)
  (let ((rotateFirstPoint? t)
        (rotateSecondPoint? t)
        rotationAngle)
    (sk8-multival-bind (xLoc yLoc) (location me)
      ;; Deal with the angle.
      (if relative
        (setf rotationAngle angle
              angle (+ (angle me) rotationAngle))
        (setf rotationAngle (- (angle me) angle)))
      ;; Now rotate each point.
      (sk8-multival-bind (sx sy ex ey) (endpoints me)
        (case anchorPoint
          (start (setf rotateFirstPoint? nil)
                 (setf xLoc sx yLoc sy))
          (end (setf rotateSecondPoint? nil)
               (setf xLoc ex yLoc ey)
               ))
        ;; Make points be 0 based (0,0 being center of line).
        (when rotateFirstPoint? 
          (decf sx xloc) (decf sy yloc)
          ;; Rotate point 1.
          (multiple-value-setq (sx sy) (gs:rotate-coords-2d sx sy rotationAngle))
          (incf sx xLoc)
          (incf sy yLoc))
        (when rotateSecondPoint?
          (decf ex xloc) (decf ey yloc)
          ;; Rotate point 2.
          (multiple-value-setq (ex ey) (gs:rotate-coords-2d ex ey rotationAngle))
          (incf ex xLoc)
          (incf ey yLoc))
        ;; Set the angle and the points.
        (setEndpoints me sx sy ex ey)
        (setValue 'angle me angle)))))

(define-handler (setf angle) (newValue lineSegment &key relative anchorPoint)
  (rotate me newValue :relative relative :anchorPoint anchorPoint))

;;; __________________
;;; Settings for original.
;;; __________________

(setEndpoints LineSegment 10 10 100 100)
(setf (colorDepth LineSegment) nil) ;; Stupid to make a gWorld for a line!

(define-handler (setf colorDepth) (boolean LineSegment)
  (declare (ignore boolean))
  (sk8-error GeneralProgrammaticError
             :strings '("No double buffering for lines on the stage.")))

;;; _______________________________ 
;;; Changing parents.
;;; _______________________________ 

(define-handler AddedMeAsParent (lineSegment child oldparents)
  (gs:let+ ((oldBounds (:region)))
    (unless (some #'(lambda (x) (inheritsFrom x Actor)) oldparents)
      (if (some #'(lambda (x) (inheritsFrom x Actor)) oldparents)
        (initialize-for-new-actor-parent me child)
        (progn (gs:recompute-bounds-region child (gs:node-flags child))
               (#_copyRgn (gs:node-boundsRegion child) oldBounds))))
    ;; Initilize the endpoints and call set boundsRect to map them.
    (setf (slot-value child 'endpoints) (copy-list (slot-value me 'endpoints)))
    (setf (gs:node-drawfunction child) 'draw-segment)
    (gs:making-self-dirty (child t nil nil nil oldBounds :bounds t)
      (setf (boundsRect child) (boundsRect child)))))

#|
	Change History (most recent last):
	1	8/13/93	kleiman	Moved line to lineSegment
	2	8/16/93	kleiman	fixed bug in line-setpoints
	3	8/18/93	kleiman	many changes. . .
	4	9/22/93	kleiman	check for existing node struct during possible
				object initialization
	5	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	6	10/1/93	hernan	Removing not valid initargs.
	7	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	8	4/8/94	Hernan	Swapping this code for the new line segment code:
				a line that is not a polygon.
	9	4/11/94	Hernan	Adding rotation to our lines. It is a bit different
				from the polygon one in that you can only rotate
				around the line's center of around one of the two
				endpoints.
	10	7/6/94	Hernan	1172074: The function that computes the bounds
				from the points now takes into account the line's 
				size. This ensures no line returns 0 as its width or
				height.
	11	7/12/94	Hernan	1173467: Now the line does not let you add contents to
				itself!
	12 	 8/19/94	Hernan  	
	13 	 8/26/94	Hernan  	1182452: redefining setLocation for lineSegment
							to do the clever thing. Also changed setEndpoints
							and set-segments-points-on-stage to take 
							advantage of the fact that the segment is just
							moving.
	14 	 9/12/94	Hernan  	1186136: Calling logicalToPhysical using the right 
							container.
	15 	 9/12/94	Hernan  	1186136: Ooops. Cannot call logicalToPhysicalRect
							when the line does not have a container. Also 
							fixed endpoints to return the right thing when the
							line is a window.
	16 	 9/28/94	Hernan  	Moving the offseting code out of the region
							calculating methods. Also fixing makeboundsregion
							to stop forcing the fill and frame to be recomputed.
	17 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	18 	10/24/94	sidney  	declare some private handlers as :private that weren't
	19 	11/ 8/94	Hernan  	1198215: when the line is on the Stage and its
							lineSize changes, we have to refresh the window.
	20 	12/ 5/94	Hernan  	1203828: setEndpoint should force a recomputation of
							the fill and frame regions also. This is not required
							for lines but it is for arrows.
	21 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	22 	 1/24/95	Hernan  	Using with-fast-node-slots where appropriate.
	23 	 2/10/95	Hernan  	The lineSegment is now inscribed in its boundsRect (ALWAYS).
	24 	 2/16/95	sidney  	readable argument names for initialize handler
	25 	 3/31/95	Hernan  	1234225: fixed set-enpoints-on-stage to make sure the
							width and height are at least 1.
	2  	 6/23/95	Hernan  	1247534
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
