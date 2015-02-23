;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;; Takes the proposed new location and returns one that makes the object fit in the 
;;; constraining rectangle. Assumes that the constraining rectangle is larger than the
;;; actor being moved!

(defun enforce-Within-Rect (width height newX newY constrainingRect)
  (let (halfHeight halfWidth ll tt rr bb dl dr dt db)
    (setf halfHeight (gs:f.round (/ height 2))
          halfWidth (gs:f.round (/ width 2)))
    ;; Now find the proposed limits...
    (setf ll (- newX halfWidth)
          rr (+ newX halfWidth)
          tt (- newY halfHeight)
          bb (+ newY halfHeight))
    ;; Find the distance to the bounding rect (negative means a violation!)
    (setf dl (- ll (car constrainingRect))
          dr (- (caddr constrainingRect) rr)
          dt (- tt (cadr constrainingRect))
          db (- (cadddr constrainingRect) bb))
    ;; Resolve conflicts...
    (cond ((minusp dl)
           (sk8-multivals (- newX dl) (cond ((minusp dt) (- newY dt))
                                            ((minusp db) (+ newY db))
                                            (t newY))))
          ((minusp dr)
           (sk8-multivals (+ newX dr) (cond ((minusp dt) (- newY dt))
                                            ((minusp db) (+ newY db))
                                            (t newY))))
          (t (sk8-multivals newX (cond ((minusp dt) (- newY dt))
                                       ((minusp db) (+ newY db))
                                       (t newY)))))))

;;; 1.0
;;; TLA-DRAG -- this method allows an actor on the stage to be dragged. Uses Mac window routines.
;;;            Modified to preserve the offset from the cursor's location to the location of the
;;;            actor.

(defun drag-move-if-necessary (theActor otherActors xOffset yOffset constrainingRect mouseH mouseV)
  (let (dh dv newX newy)
    (setf newX (- mouseH xOffset)
          newY (- mouseV yOffset))
    (sk8-multival-bind (locH locV) (location theActor :physical t)
      (when (not (and (= locH newX) (= locV newY)))
        (withactorlocked (theActor)  ;;; we lock our initial actor just in case the other actors are in the same container.  Makes it look nice -BJR
          (when constrainingRect 
            (sk8-multival-setf 
             (newX newY) 
             (sk8-multival-bind (width height) (size theActor :physical t)
               (enforce-within-rect width height newX newY constrainingRect))))
          (setLocation theActor newX newY :physical t)
          ;; If the main actor is a window, the actor's location might be a pixel off
          ;; due to round off errors (window's locations defined as topleft). Thus, if this is the case
          ;; we have to get the location again to avoid "drifting". 
          (when (gs:hasWindow? (gs:node-flags theActor))
            (sk8-multival-setf (newx newy) (tla-location theActor)))
          ;; Compute difference for relative move...
          (setf dh (- newX locH)
                dv (- newY locV))
          ;; Move the otherActors.
          (dolist (otherGuy otherActors)
            (setLocation otherGuy dh dv :relative t :physical t))
          )))))

(defun TLA-drag (theactor xOffset yOffset &optional otherActors dropEvents constrainingRect
                            draggedOverEvents draggingMouseWithinEvents)
  (let* ((clos-window (gs:node-window theActor))
         ;; (wptr (wptr clos-window))
         (oldPos (view-position clos-window))
         (original-Pos oldPos)
         (oldMouseH 0)
         (oldMouseV 0)
         (mouseEventsWanted (or draggedOverEvents draggingMouseWithinEvents))
         sk8-windows newMouseH newMouseV newActorUnder
         enterFunction leaveFunction withinFunction)
    ;; Create the functions used to signal enter/leave and within.
    (when mouseEventsWanted
      (setf enterFunction #'(lambda (x) (draggingMouseEnter x theActor)))
      (setf leaveFunction #'(lambda (x) (draggingMouseLeave x theActor)))
      (setf withinFunction #'(lambda (x) (draggingMouseWithin x theActor))))
    (setq sk8-windows (windows :class 'gs:*SK8-WINDOW* :include-windoids t))
    ;; Do the movement!
    (loop
      (unless (mouse-down-p) (return))
      (sk8-multival-setf (newMouseH newMouseV) (mouseloc stage))
      (unless (and (= oldMouseH newMouseH) (= oldMouseV newMouseV))
        (drag-move-if-necessary theActor otherActors xOffset yOffset constrainingRect newMouseH newMouseV)
        (mapc #'window-update-event-handler sk8-windows)
        (setf oldMouseH newMouseH
              oldMouseV newMouseV)
        ;; Deal with draggedOver events!
        (when draggedOverEvents
          (sk8-multival-bind (h v) (mouseLoc stage)
            ;; Send the draggedOver event...
            (when mouseEventsWanted
              (setf newActorUnder (findDropObject h v :avoiding theActor))
              (when (neq *actorUnderDragging* newActorUnder)
                ;; Ok. We have the new and the old actors.
                ;; Dispatch the mouseEnter and mouseLeave events.
                (if *actorUnderDragging*
                  (gs:dispatch-mouseEnter&leave *actorUnderDragging* newActorUnder
                                                 enterFunction leaveFunction '*actorUnderDragging*)
                  (gs:just-do-mouseEnters newActorUnder enterFunction '*actorUnderDragging*)))
              ))))
      ;; Dispatch mouseWithin events.
      (when draggingMouseWithinEvents
        (gs:dispatch-mouseWithins newActorUnder withinFunction)))
    (setf *actorUnderDragging* nil)
    ;; Final stuff: drop events.
    (let* ((final-pos (view-position clos-window))
           (change (subtract-points final-pos original-pos)))
      ;; Deal with the drop event.
      (when dropEvents
        (sk8-multival-bind (x y) (mouseLoc stage)
          (let ((dropRecipient (findDropObject x y :avoiding theActor)))
            (when dropRecipient (drop theActor dropRecipient)))))
      ;; Return the total change.
      (SK8-multivals (point-h change) (point-v change)))))

;;; dragActors -- drags multiple actors in non live mode.

(define-sk8-function dragActors nil (actorList &optional (xOffset 0) (yOffset 0) constrainingRect)
  ;; drag actors
  (gs:let+ ((dragRgn (:region))
             (theRect (:Rect))
             (tempRgn (:region))
             oldX oldY newX newY wind mouseH mouseV width height)
    (sk8-multival-bind (ll tt rr bb) (actorsBounds actorList :physical t)
      (setf oldX (gs:f.round (/ (+ ll rr) 2))
            oldY (gs:f.round (/ (+ tt bb) 2))))
    (setf newX oldX newY oldY)
    (sk8-multival-setf (width height) (size (car actorList) :physical t))
    ;; compute the starting region
    (#_SetEmptyRgn dragRgn)
    (#_SetRect theRect -3000 -3000 3000 3000)
    ;; map over the objects to make the region
    (doList (obj ActorList)
      (setq wind (gs:node-window obj))
      ;; recompute the bounds
      (gs:recompute-bounds-region obj (gs:node-flags obj))
      ;; copy it to temp
      (#_CopyRgn (gs:node-boundsRegion obj) tempRgn)
      ;; offset by the window 
      (#_OffsetRgn tempRgn (point-h (view-position wind)) (point-v (view-position wind)))
      ;; add it in
      (#_UnionRgn dragRgn tempRgn dragRgn))
    (without-interrupts
     ;; OK, now draw the region
     (with-port (gs:get-wmgr-port)
       (#_ClipRect theRect)
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (#_FrameRgn dragRgn))
     ;; loop and drag
     (loop
       ;; return if the mouse is up
       (unless (mouse-down-p)
         (return))
       ;; pause
       (sleep (/ 60))
       (tickEventClock)
       ;; get the new mouse
       (sk8-multival-setf (mouseH mouseV) (mouseloc Stage))
       (setf newX (- mouseH xOffset)
             newY (- mouseV yOffset))
       (when constrainingRect
         (SK8-multival-setf 
          (newx newY) 
          (enforce-within-rect width height (- mouseH xOffset) (- mouseV yOffset) constrainingRect)))
       ;; have we moved?
       (when (or (/= newX oldx)
                 (/= newY oldY))
         ;; we have - copy the old
         (#_CopyRgn dragRgn tempRgn)
         (#_OffsetRgn dragRgn (- newX oldX) (- newY oldY))
         (setq oldX newX
               oldY newY)
         ;; erase and redraw
         (with-port (gs:get-wmgr-port)
           (#_PenMode #$PatXor)
           (#_PenPat *gray-pattern*)
           (#_FrameRgn dragRgn)
           (#_FrameRgn tempRgn))))
     ;; finally, erase the old one
     (with-port (gs:get-wmgr-port)
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (#_FrameRgn dragRgn)
       (#_PenMode #$PatCopy)
       (#_PenPat *black-pattern*))
     )
    ;; return the points
    (sk8-multiVals (+ newX xOffset) (+ newY yOffset))))

;;; 1.0
;;; DRAG -- A VERY, VERY simple version of draw for actors on a window. It just drags the actor
;;;        that received the mousedown event. Note that this function does not support dragging
;;;        outside of the window this actor sits in. Use dragOnStage for that!
;;;
;;; Now improved to drag multiple actors live and non live!!!

;;; This variable keeps the actor under the thing being dragged. It is always cleared by the
;;; drag operation. Thus there is no danger of actors being hanged on by it.

(defvar *actorUnderDragging* nil)

;;; Now written to mirror nonLiveDragOnStage, which works just fine. 
;;; No longer jumps when the halo drags stuff.

(defun liveDrag (me otherActors xOffset yOffset draggedOverEvents draggingMouseWithinEvents 
                      constrainingRect)
  (without-interrupts 
   (gs:let+ ((temp-region (:region :init-empty))
             (outline-region (:region :init-empty))
             (oldMouseH 0)
             (oldMouseV 0)
             dh dv newMouseH newMouseV)
     (sk8-multival-setf (oldMouseH oldMouseV) (mouseLoc Stage))
     ;; For outline drag, compute the outline region and do the initial draw
     (#_SetClip (rref (gs:get-wmgr-port) :cgrafport.visrgn))
     (dolist (i (cons me otheractors))
       (gs:recompute-bounds-region i (gs:node-flags i))  ;; needed?
       (#_copyRgn (gs:node-boundsRegion i) temp-region)
       (sk8-multival-bind (dh dv) (gs:window-topleft-offset i)
         (#_offsetRgn temp-region dh dv))
       (#_UnionRgn temp-region outline-region outline-region))
     ;; Save the original location.
     (sk8-multival-setf (newMouseH newMouseV) (mouseloc stage))   ;;; Just in case we never loop
     ;; Move the actors...
     (loop
       (unless (mouse-down-p) (return))
       (sk8-multival-setf (newMouseH newMouseV) (mouseloc stage))
       (if (and (= oldMouseH newMouseH) (= oldMouseV newMouseV))
         (when draggedOverEvents
           (DoDragEventsIfNecessary me newMouseH newMouseV draggingMouseWithinEvents))
         (progn
           (sk8-multival-setf (dh dv) 
                              (find-dh-and-dv outline-region xOffset yOffset constrainingRect newMouseH newMouseV))
           (when draggedOverEvents
             (DoDragEventsIfNecessary me newMouseH newMouseV draggingMouseWithinEvents))
           ;; Actual movement!
           (withActorLocked (me)
             (setLocation me dh dv :physical t :relative t)
             (dolist (c otherActors) (setLocation c dh dv :physical t :relative t)))
           (#_OffsetRgn outline-region dh dv)
           (sleep 1/60)
           (setf oldMouseH newMouseH
                 oldMouseV newMouseV))))
     )))

(defun region-location (region)
  (let (h v)
    (rlet ((r :rect))
      (gs:region-into-rect region r)
      (setf h (ash (+ (rref r :rect.left) (rref r :rect.right)) -1))
      (setf v (ash (+ (rref r :rect.top) (rref r :rect.bottom)) -1)))
    (sk8-multivals h v)))

(defun region-size (region)
  (let (width height)
    (rlet ((theRect :rect))
      (gs:region-into-rect region theRect)
      (setf width (- (rref theRect :rect.right) (rref theRect :rect.left)))
      (setf height (- (rref theRect :rect.bottom) (rref theRect :rect.top))))
    (sk8-multivals width height)))

(defun find-dh-and-dv (outlineRgn xOffset yOffset constrainingRect mouseH mouseV)
  (let (newX newY)
    (setf newX (- mouseH xOffset)
          newY (- mouseV yOffset))
    (when constrainingRect 
      (sk8-multival-bind (width height) (region-size outlineRgn)
        (sk8-multival-setf (newX newY) 
                           (enforce-within-rect width height newX newY constrainingRect))))
    (sk8-multival-bind (hLoc vLoc) (region-location outlineRgn)
      (sk8-multivals (- newX hLoc) (- newY vLoc)))))

(defun specialDrag (me otherActors live dropEvents draggedOverEvents draggingMouseWithinEvents
                           hLoc vLoc xOffset yOffset constrainingRect)
  ;; Call the appropriate drag method to do the work.
  (if live 
    (liveDrag me otherActors xOffset yOffset draggedOverEvents draggingMouseWithinEvents constrainingRect)
    ;; (nonLiveDrag me otherActors xOffset yOffset hLoc vLoc constrainingRect)
    (nonLiveDragOnStage me xoffset yoffset otherActors constrainingRect
                        nil draggedOverEvents draggingMouseWithinEvents
                        :changeLocation t)
    )
  ;; Deal with drop events if required.
  (when dropEvents
    (sk8-multival-bind (x y) (mouseLoc stage)
      (let ((dropRecipient (findDropObject x y :avoiding me)))
        (when dropRecipient (drop me dropRecipient)))))
  ;; Return the amount we have moved.
  (sk8-multival-bind (newHLoc newVLoc) (location me :physical t)
    (sk8-multivals (- newHLoc hLoc) (- newVLoc vLoc))))

(defun adjust-constrainingRect (mainActor otherActors proposedRect)
  (let (left top right bottom)
    (sk8-multival-bind (ll tt rr bb) (actorsBounds (cons mainActor otherActors) :physical t)
      (sk8-multival-bind (mainLL mainTT mainRR mainBB) (boundsRect mainActor :physical t)
        (setf left (car proposedRect) top (cadr proposedRect) 
              right (caddr proposedRect) bottom (cadddr proposedRect))
        (setf left (- left (- mainLL ll))
              right (+ right (- rr mainRR))
              top (- top (- mainTT tt))
              bottom (+ bottom (- bb mainBB)))
        (list left top right bottom)))))

(define-handler drag (Actor &key
                                    otheractors
                                    (live (if (option-key-p) (not *live*) *live*))
                                    (dropEvents nil)
                                    (draggedOverEvents nil)
                                    (draggingMouseWithinEvents nil)
                                    (onStage nil)
                                    (constrainingRect nil))
  (cond ((and (null onStage) (null (gs:node-window me)))
         (sk8-error GeneralProgrammaticError
                    :strings '("You cannot drag an actor that is not on the stage.")))
        ((and onStage otherActors)
         (sk8-error GeneralProgrammaticError
                    :strings '("You cannot drag more than 1 actor onStage.")))
        (t 
         ;; If the container is boundedByContents and a window, cannot drag live!
         (let ((theContainer (gs:node-container me)))
           (when (and theContainer (boundedByContents theContainer) 
                      (gs:hasWindow? (gs:node-flags theContainer)))
             (setf live nil)))
         ;; Make sure the otherActors are a list!
         (setf otherActors (if (listp otherActors) otherActors (list otherActors)))
         (when (draggable me)
           (let (xOffset yOffset hloc vloc)
             ;; compute the distance between the pointer and the actor's location. If we are dragging
             ;; a set of actors, compute the center of the union of their boundsRects. 
             (if otherActors
               (sk8-multival-bind (ll tt rr bb) (actorsBounds (cons me otherActors) :physical t)
                 (setf hloc (truncate (+ ll rr) 2)
                       vloc (truncate (+ tt bb) 2)))
               (sk8-multival-setf (hLoc vLoc) (location me :physical t)))
             (setf xOffset (- (gs:f.round (eventH)) hLoc)
                   yOffset (- (gs:f.round (eventV)) vLoc))
             ;; Make sure the other actors is a list.
             (unless (listp otherActors) (setf otherActors (list otherActors)))
             ;; If we have other actors and a constrainingRect and the rect does not
             ;; fit all the actors, change it to maintain the illusion that it affects the 
             ;; event actor only.
             (when (and otherActors constrainingRect)
               (setf constrainingRect (adjust-constrainingRect me otherActors constrainingRect)))
             ;; Call the right function.
             (cond ((and (eq (sk8::window me) me) live (not onstage))
                    (tla-drag me xOffset yOffset otherActors dropEvents constrainingRect
                              draggedOverEvents draggingMouseWithinEvents))
                   (onStage 
                    (dragOnStage me :dropEvents dropEvents :draggedOverEvents draggedOverEvents
                                 :draggingMouseWithinEvents draggingMouseWithinEvents :live live
                                 :hOffset xOffset :vOffset yOffset
                                 :otherActors otherActors))
                   (t (specialDrag me otherActors live dropEvents draggedOverEvents draggingMouseWithinEvents
                                   hLoc vLoc xOffset yOffset constrainingRect)
                      )))))))
      
#|
	Change History (most recent last):
	1	8/31/93	hernan	new file
	15	8/31/93	hernan	new file
	1	8/31/93	hernan	New file! The drag "system".
	2	9/24/93	hernan	Adding draggingMouseWithin event.
	3	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	4	10/1/93	hernan	Removing struct from dragActors.
	5	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	6	10/15/93	rod	
	7	10/29/93	rod	
	8	11/1/93	rod	
	9	11/17/93	chip	changed nonLiveDrag so that, if the actor is a toplevel 
                                        actor, it uses the actor's wptr's strucRgn instead of 
                                        the actor's boundsRegion
	10	11/30/93	hernan	Coercing the otherActors into a list.
	11	1/11/94	hernan	self -> me
	12	1/17/94	hernan	Ticking the eventClock during drag.
	13	2/12/94	kleiman	renaming
	14	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15	2/21/94	hernan	window -> sk8::window.
	16	3/10/94	dy	TickEventClock instead of #_MoviesTask
	17	3/17/94	Hernan	Drag errors when the container of the actor to
				be dragged is false (not on the stage).
	18	4/15/94	Hernan	Now uses the physical (new and improved) version
				of draggingMouseEnter and leave.
	19	4/20/94	Hernan	Replacing calls to get-record-field with calls to
				region-into-rect (which does not cons).
	20	4/27/94	Hernan	Removing extranous print statement from drag-
				move-if-necessary. Also fixed the "drifting"
				problem that happened when the main actor being
				dragged was a tla. Problem was that setting the
				location of a window does not guarantee the 
				window will be put in the exact place specified. 
				This is becase of round off errors that happen as
				a result of windows defining their positions as
				their topleft.
	21	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	22	7/18/94	Hernan	1171558: if the container of the thing to be dragged
				is boundedByContents we only let you do the drag live.
				This is because otherwise bindByContents does nothing since
				the window is locked in the process of redrawing the actor
				that is moving.
	23	7/22/94	Hernan	Adding dragged over events to tla-drag.
	24 	 8/31/94	Hernan  	1182974: allowing drag live when the boundedbycontents
							thing is not a window.
	25 	 9/20/94	Hernan  	1187542: dispatching draggingMouseWithin more
							than just when the actor under the dragged obj.
							changes.
	26 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	27 	 1/24/95	Hernan  	Using with-fast-node-slots where appropriate.
	28 	 2/ 3/95	Hernan  	get-record-field -> href or rref.
	29 	 3/ 8/95	Hernan  	Wrapping every function that does non live 
							dragging in a without-interrupts to avoid MCL 
							messing with the port as we drag.
	30 	 3/27/95	Hernan  	1232918: maybe we can use nonLiveDragOnStage
							to do non live dragging for everyone... Yes. 
							Obsoleted nonLiveDrag.
	31 	 3/28/95	Hernan  	Ooops. NonLiveDragOnStage should allow for a 
							change in location at the end.
	2  	 6/23/95	Hernan  	1253529: dragActors now does not make the outline region "jump" when it starts.
							1236942: the constrainingRect only affects the main actor.
	3  	 1/17/96	sidney  	ss::!coerce -> sk8::as
~~	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	6  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
						4   5/ 3/96Hernan  Enforcing the documented constraint of drag: onStage you
						can only draw 1 actor.
						5   7/29/96Hernan  liveDrag, removing the line that dirties the boundsRegion.
|# ;(do not edit past this line!!)
