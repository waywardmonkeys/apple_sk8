(in-package :SK8Development)



;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;;___________________________________________________________________
;;;___________________________________________________________________
;;;
;;; DRAG ON STAGE
;;; (c) 1992 ICE for Apple Computer, Inc.
;;;
;;; 09/20/92 Hernan modified activate to make it work properly (no displacement)
;;;          and deactivate to send the drop event.
;;;
;;;___________________________________________________________________
;;;___________________________________________________________________

#| Modification History

02-22-93 ruben inheritsFrom? -> inheritsFrom
01-22-93 ruben id -> objectName

|#


(new eventMode :objectname "DragOnStageMode" :project sk8
     :properties '(currentActor
                   prevContainer
                   prevLoc
                   theoffset
                   (lastLoc :value 0) ; Make this a number to avoid crashing.
                   modal
                   OtherActors
                   dropEvents
                   (live :value t)
                   draggedOverEvents
                   draggingMouseWithinEvents))

;;; ACTIVATE -- starts the dragging on the stage. Note that this works because the attach
;;;           method that attaches actors to the stage uses the logical bounds rect to figure
;;;           out the position of an actor. Beware!

(define-handler activate (DragOnStageMode)
  (let* ((theActor (currentActor me)))
    (when (and theActor (neq (container theactor) stage))
      (SK8-multival-bind (ph pv) (location theActor :physical t)
        (Setf (lastLoc me) (make-point ph pv))
        (setf (container theActor) nil)
        ;; Set the logical location to the physical location (the physical location of an
        ;; actor in a container is its logical location on the Stage!)
        (setLocation theActor ph pv)
        ;; And attach to the stage.
        (setf (container theActor) stage)))))

;;; DEACTIVATE -- ends the dragging on the stage. We set the container of the dragged actor
;;;             to its original container. No extra work is required. We lock the previous
;;;             container in order to avoid any flickering just in case the user's drop
;;;             handlers do change the container of the actor.

;;; The actor is returned to its original container because that is the only way to
;;; find out if the containment is changed by the drop handler. (Problem was that we 
;;; were setting the container to false and the user was too!)

(define-handler deactivate (DragOnStageMode)
  (let ((theActor (currentActor me))
        (prevContainer (prevContainer me)) 
        (dropRecipient nil)
        )
    ;; Clear the dragging variable. (just in case).
    (setf *actorUnderDragging* nil)
    (when theActor
      ;; Return the actor to its original container. The drop handler
      ;; might change the container and thus, the old container is locked
      ;; in order to avoid annoying flickerings...
      (flet ((doTheWork ()
               ;; Restoring the old location of the actor.
               (setf (container theActor) nil)
               (setLocation theActor (car (prevLoc me)) (cadr (prevLoc me)))
               (setf (container theActor) prevContainer)
               ;; Calling drop event.
               (when (dropEvents me)
                 (setf dropRecipient (findDropObject (gs:f.round (eventH)) (gs:f.round (eventV)) :avoiding theActor))
                 (drop theActor dropRecipient))))
        (if (or (eq prevContainer Stage) (null prevContainer))
          (doTheWork)
          (withActorLocked (prevContainer)
            (doTheWork)))
        (when (modal me)
          (throw :dosDone (or dropRecipient t)))))))

(define-handler handleMouseUp (DragOnStageMode)
  (exitMode me)
  t)

(defun DoDragEventsIfNecessary (draggee h v draggingMouseWithinEvents)
  (let* ((theActor (findDropObject h v :avoiding draggee))
         (lastActor *actorUnderDragging*))
    (if (eq lastActor theActor)
      (when draggingMouseWithinEvents
        (gs:dispatch-mouseWithins theActor 
                                   #'(lambda (x) (draggingMouseWithin x draggee))))
      ;; Define the functions and call them.
      (let ((enterFunction #'(lambda (x) (draggingMouseEnter x draggee)))
            (leaveFunction #'(lambda (x) (draggingMouseLeave x draggee))))
        (if lastActor
          (gs:dispatch-mouseEnter&leave lastActor theActor
                                         enterFunction leaveFunction '*actorUnderDragging*)
          (gs:just-do-mouseEnters theActor enterFunction '*actorUnderDragging*))))))

(define-handler eventTick (DragOnStageMode)
  (let ((theOffset (theOffset me)))
    (sk8-multival-bind (h v) (mouseLoc stage)
      (setLocation (currentActor me) (- h (car theOffset)) (- v (cadr theOffset)))
      ;; Send the draggedOver event...
      (when (draggedOverEvents me)
        (DoDragEventsIfNecessary (currentActor me) h v (draggingMouseWithinEvents me))))))

(defun DononLiveDragOnstageDraw (outline-region)
  (with-port (gs:get-wmgr-port)
    (#_SetClip (rref (gs:get-wmgr-port) :cgrafport.visrgn))
    (#_Pennormal)
    (#_PenMode #$PatXor)
    (#_PenSize 1 1)
    (#_PenPat *gray-pattern*)
    (#_FrameRgn outline-region)))

;;; This is only done when the thing we are dragging is a window and we do not
;;; drag other actors. Returns the extra offset. 

(defun compute-window-outline-region (me outline-region)
  (let ((extraOffsetH 0)
        (extraOffsetV 0)
        clos-window)
    ;; If this is a toplevel actor, use its window's strucRgn
    (when (gs:hasWindow? (gs:node-flags me))
      (setf clos-window (gs:node-window me))
      (SK8-multival-bind (boundsX boundsY) (region-location outline-region)
        (#_copyRgn (rref (wptr clos-window) :window.strucRgn) outline-region)
        (SK8-multival-bind (strucX strucY) (region-location outline-region)
          (setq extraOffsetH (- boundsX strucX)
                extraOffsetV (- boundsY strucY)))))
    (values extraOffsetH extraOffsetV)))

(defun nonLiveDragOnstage (theActor xOffset yOffset otherActors constrainingRect
                                       dropEvents draggedOverEvents draggingMouseWithinEvents
                                       &key changeLocation)
  (without-interrupts 
   (gs:let+ ((returnvalue nil)
          (temp-region (:region :init-empty))
          (outline-region (:region :init-empty))
          (extraHoffset 0)
          (extraVOffset 0)
          dh dv newMouseH newMouseV hLoc vLoc oldMouseH oldMouseV)
     (sk8-multival-setf (oldMouseH oldMouseV) (mouseLoc Stage))
     ;; For outline drag, compute the outline region and do the initial draw
     (#_SetClip (rref (gs:get-wmgr-port) :cgrafport.visrgn))
     (dolist (i (cons theactor otheractors))
       (gs:boundsdirty! (gs:node-flags i))
       (gs:recompute-bounds-region i (gs:node-flags i))  ;; needed?
       (#_copyRgn (gs:node-boundsRegion i) temp-region)
       (sk8-multival-bind (dh dv) (gs:window-topleft-offset i)
         (#_offsetRgn temp-region dh dv))
       (#_UnionRgn temp-region outline-region outline-region))
     (unless otherActors
       (multiple-value-setq (extraHOffset extraVOffset) (compute-window-outline-region theActor outline-region))
       (incf xOffset extraHOffset)
       (incf yOffset extraVOffset))
     ;; Save the original location.
     (sk8-multival-setf (hLoc vLoc) (region-location outline-region))
     ;; Do initial draw.
     (DononLiveDragOnstageDraw outline-region)
     (sk8-multival-setf (newMouseH newMouseV) (mouseloc stage))   ;;; Just in case we never loop
     ;; Move the outline...
     (loop
       (unless (mouse-down-p) (return))
       (sk8-multival-setf (newMouseH newMouseV) (mouseloc stage))
       (if (and (= oldMouseH newMouseH) (= oldMouseV newMouseV))
         (when draggedOverEvents
           (DoDragEventsIfNecessary theActor newMouseH newMouseV draggingMouseWithinEvents))
         (progn
           (sk8-multival-setf (dh dv) 
                              (find-dh-and-dv outline-region xOffset yOffset constrainingRect newMouseH newMouseV))
           (DononLiveDragOnstageDraw outline-region)
           (when draggedOverEvents
             (DoDragEventsIfNecessary theActor newMouseH newMouseV draggingMouseWithinEvents))
           (#_OffsetRgn outline-region dh dv)
           (DononLiveDragOnstageDraw outline-region)
           (sleep 1/60)
           (setf oldMouseH newMouseH
                 oldMouseV newMouseV))))
     ;; Erase the outline.
     (DononLiveDragOnstageDraw outline-region)
     (with-port (gs:get-wmgr-port)
       (#_PenMode #$PatCopy)
       (#_PenPat *black-pattern*))
     ;; If required, change the location.
     (when changeLocation
       (sk8-multival-bind (newHLoc newVLoc) (region-location outline-region)
         (setf dh (- newHloc hloc)
               dv (- newVLoc vLoc))
         (unless (and (zerop dh) (zerop dv))
           (withActorLocked (theActor)
             (dolist (c (cons theActor otherActors))
               (setLocation c dh dv :relative t :physical t))))))
     ;; Deal with drop events.
     (when dropEvents
       (setf returnvalue (findDropObject (gs:f.round newMouseH) (gs:f.round newMouseV)))
       (drop theActor returnvalue)
       )
     (setf *actorUnderDragging* nil)
     returnvalue
     )))

;;; Now drag calls this. This public function will become non public in the near future.

(define-sk8-function dragOnStage nil (theActor &key modal dropEvents draggedOverEvents draggingMouseWithinEvents
                                                 hOffset vOffset (live t) otherActors)
  (unless nil ;; (inheritsFrom theActor polygon)
    (setf (currentActor DragOnStageMode) theActor)
    (setf (modal DragOnStageMode) modal)
    (setf (dropEvents DragOnStageMode) dropEvents)
    (setf (draggedOverEvents DragOnStageMode) draggedOverEvents)
    (setf (otherActors DragOnStageMode) otherActors)
    (setf (draggingMouseWithinEvents DragOnStageMode) draggingMouseWithinEvents)
    (setf (prevContainer DragOnStageMode) (container theActor))
    (setf (prevLoc DragOnStageMode) (location theActor))
    (setf (theOffset DragOnStageMode) (if (and hOffset vOffset) (list hOffset vOffset) (list 0 0)))
    (if live
      (if modal 
        (catch :dosDone
          (enterMode DragOnStageMode))
        (enterMode DragOnStageMode))
      (nonLiveDragOnstage theActor hoffset voffset otherActors nil 
                          dropEvents draggedOverEvents 
                          draggingMouseWithinEvents))))

#|
	Change History (most recent last):
	2	6/4/93	Hernan	Fixed update bug when actor to be dragged was
				not immediately contained by a topLevelActor. The
				fix is to set the container to nil before setting the
				new boundsrect.
	8	8/31/93	hernan	Made dos work with the new drag system.
	9	9/24/93	hernan	Adding dropped event and fixing drag events.
	13	10/29/93	rod	Nonlive dragonstage is now a tightloop
	14	10/29/93	rod	
	15	11/1/93	rod	
	16	11/2/93	rod	
	17	12/10/93	hernan	Fixing nonLiveDragOnStage to stay with the mouse. Error was not here...
	18	12/13/93	rod	
	19	1/11/94	hernan	self -> me
	20	2/1/94	hernan	Experimenting...
	21	2/1/94	hernan	Deactivate of DOS now puts the actor in its
				original container and lets the user change the
				containment anywhere using his/her own drop
				handler.
	22	2/12/94	kleiman	name changes
	23	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	24	2/28/94	hernan	Deactivate should not try to lock the stage.
	25	3/3/94	kleiman	properties -> addproperty
	26	3/3/94	kleiman	private properties declared via makeprivateproperty
	27	3/17/94	Hernan	When deactivating, we should not try to lock the
				old container when the old container was false.
	28	4/15/94	Hernan	Now using the new version of enter and leave
				events (the physical interpretation).
	29	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	30 	12/16/94	rod     	Making dragonstage return the drop object if 
							there was one.
	31 	 1/24/95	Hernan  	Using with-fast-node-slots where appropriate.
	32 	 2/ 3/95	Hernan  	get-record-field -> href or rref.
	33 	 3/27/95	Hernan  	1232918: maybe we can use nonLiveDragOnStage
							to do non live dragging for everyone...
	34 	 3/28/95	Hernan  	Ooops. NonLiveDragOnStage should allow for a 
							change in location at the end.
	2  	 6/23/95	Hernan  	1245571: Allowing dragOnStage to work with polygons.
	3  	 6/23/95	Hernan  	nonLiveDragOnStage had to initialize oldMouseH and
						oldMouseV to the mouseLoc of the Stage.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 9/30/96	sidney  	eventmode handlers must now return T to indicate that they handled the event
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
