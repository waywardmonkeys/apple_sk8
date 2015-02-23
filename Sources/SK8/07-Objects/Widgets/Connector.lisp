(in-package :SK8Development)

(provide "CONNECTOR")

(require "LINESEGMENT")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; _________________________________________________________________________
;;;                              CONNECTORS
;;;
;;; A lineSegment that connects two actors.
;;; _________________________________________________________________________

#| Modification History:

01-22-93 ruben id -> objectName
10-03-92 ruben line -> lineSegment
09-26-92 ruben added private? argument to define-sk8-function in dragRubberBand
09-20-92 hernan changing this to work with MF VI
09-20-92 Hernan is rewriting this to work in SK8 1.0!
06-19-92 adam  made connector get/set its real geometry + changed default connector geometry to :center-projection
05-12-92 ruben d29 conversion
03-29-92 ruben began d29; connectors; swept for "get-" and "set-"
03-26-92 ruben create-object in NEW & gentemp name
03-25-92 ruben call-next-method
01-19-92 converted to MacFrames II
08-22-91 adam  entirely new connector object (now based on Actor)
08-16-91 ruben make-code cosmetics

|#

;;; CONNECTOR -- a lineSegment that connects two actors. A few restrictions apply: you cannot connect two actors
;;;            that are in different containers.

(new lineSegment :objectName "Connector" :undisposable t :prototype t :project sk8
     :properties '((startActor :value nil)
                   (endActor :value nil)
                   (endPointGeometry :value center)
                   (startPointGeometry :value center)))

(setf (localCreationRelations Connector) '(SK8::[startActor] SK8::[endActor]))

(setf (private Connector) nil) ; PUBLIC API

;;; We do not want connectors to loose their characteristic color! Do we?

(setf (lineSize connector) 1)
(setf (framecolor connector) black)
(setf (fillcolor connector) black)
(setEndPoints connector 10 10 100 100)
(setf (draggable connector) nil)
(setf (resizable connector) nil)

(define-handler initialize (connector original isNew initArgs)
  (declare (ignore isNew initArgs))
  (call-next-method)
  (setf original (originalAncestor me original Connector))
  (setf (startGeometry me) (startGeometry original))
  (setf (endGeometry me) (endGeometry original)))

(define-handler (setf endActor) (endActor connector)
  ;; Do the work. 
  (let ((oldEndActor (endActor me)))
    ;; Error checking...
    (cond ((and endActor (eq endActor (startActor me)))
           (sk8-error GeneralProgrammaticError
                      :strings '("Cannot connect an actor to itself.")))
          (t ;; Everything is fine! Get on with it...
           (when oldEndActor
             (removeConnector oldEndActor me))
           (setf (slot-value me 'endActor) endActor)
           (when endActor
             (addConnector endActor me)
             (update me))))))

(define-handler (setf startActor) (startActor connector)
  (let ((oldStartActor (startActor me)))
    ;; Error checking...
    (cond ((and startActor (eq startActor (endActor me)))
           (sk8-error GeneralProgrammaticError
                      :strings '("Cannot connect an actor to itself.")))
          (t ;; Everything is fine! Get on with it...
           (when oldstartActor
             (removeConnector oldstartActor me))
           (setf (slot-value me 'startActor) startActor)
           (when startActor
             (addConnector startActor me)
             (update me))))))

(define-handler isolate (connector)
  (setf (startActor me) nil)
  (setf (endActor me) nil))

(define-handler reverseConnection (connector &key (affectArrows t))
  (if (and (startActor me) (endActor me))
    ;; Change the values and revert the arrows if necessary.
    (let ((oldStart (startActor me))
          (oldEnd (endActor me)))
      (setf (slot-value me 'startActor) oldEnd)
      (setf (slot-value me 'endActor) oldStart)
      (when affectArrows
        (case (arrows me)
          (start (setf (arrows me) 'end))
          (end (setf (arrows me) 'start))
          )))
    (sk8-error GeneralProgrammaticError
               :strings '("The connector is not connected to two actors!"))))
    

;;; GET-CONNECT-POINT -- Returns the point where the connector should attach to actor given the geometry
;;;                   specified by the user. The options are :center (this is by far the fastest), :closest-edge,
;;;                   :vertical-edge and :horizontal-edge. The arguments from-x and from-y are the
;;;                   coordinates of the center of the physical bounds rect of the actor at the other
;;;                   end of the connector. This function is used by update to find out what points to
;;;                   use for the connector.
;;;
;;; Note that this function is essentially what it used to be in d29. The difference is that all coords used
;;; are in stage coords. This is why we need to offset the bounds region of the actor.

(define-handler get-connect-point :private (actor from-x from-y geometry)
                       (gs:let+ ((ray-region (:region))
                              (from-point (make-point (gs:f.round from-x) (gs:f.round from-y)))
                              (closest-horiz-edge-midpt (make-point 0))
                              (closest-vert-edge-midpt (make-point 0))
                              (line-hoffset 1)
                              (line-voffset 1)
                              centerH centerV ray-end trimmed-ray-box qd-point)
                         (setf from-x (gs:f.round from-x))
                         (setf from-y (gs:f.round from-y))
                         (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical t)
                           ;; Setting the rects and midpoint of actor.
                           (setf centerH (gs:f.round (/ (+ ll rr) 2))
                                 centerV (gs:f.round (/ (+ tt bb) 2)))
                           ;; Finding the closest vertical and horizontal edges.
                           (when (neq geometry 'projectCenter)
                             (setf closest-horiz-edge-midpt (make-point centerH (if (> from-y centerV) bb tt)))
                             (setf closest-vert-edge-midpt (make-point (if (> from-x centerH) rr ll) centerV)))
                           (setq ray-end
                                 (case geometry
                                   (projectCenter from-point)
                                   (horizontalEdge closest-horiz-edge-midpt)
                                   (verticalEdge closest-vert-edge-midpt)
                                   (closestEdge 
                                    (gs:closest-qd-point-to from-point closest-horiz-edge-midpt closest-vert-edge-midpt))))
                           (when (or (and (> (point-h ray-end) centerH)
                                          (> (point-v ray-end) centerV))
                                     (and (< (point-h ray-end) centerH)
                                          (< (point-v ray-end) centerV)))
                             (setq line-voffset (- line-voffset)))
                           ;; Build the region.
                           (#_OpenRgn)
                           (#_MoveTo centerH centerV)
                           (#_LineTo (point-h ray-end) (point-v ray-end))
                           (#_Line line-hoffset line-voffset)
                           (#_LineTo (+ centerH line-hoffset) (+ centerV line-voffset))
                           (#_LineTo centerH centerV)
                           (#_CloseRgn ray-region)
                           ;; Getting the boundsRegion and offseting it to be in stage coords.
                           ;; Making a region in the logical space (wow!)
                           (gs:let+ ((ownedRegion (:region)))
                             (#_SetRectRgn ownedRegion (gs:f.round ll) (gs:f.round tt) (gs:f.round rr) (gs:f.round bb))
                             ;; Intersecting the regions and other region stuff.
                             (#_SectRgn ray-region ownedRegion ray-region))
                           (with-dereferenced-handles ((region-ptr ray-region))
                             (setq trimmed-ray-box (rref region-ptr :region.rgnBBox :storage :pointer))
                             (setq qd-point 
                                   (if (#_EmptyRect trimmed-ray-box)
                                     (make-point centerH centerV)
                                     (gs:closest-qd-point-to
                                      ray-end
                                      (rref trimmed-ray-box :rect.topleft)
                                      (make-point (rref trimmed-ray-box :rect.right) (rref trimmed-ray-box :rect.top))
                                      (rref trimmed-ray-box :rect.bottomright)
                                      (make-point (rref trimmed-ray-box :rect.left) (rref trimmed-ray-box :rect.bottom))))))
                           (SK8-multivals (point-h qd-point)
                                          (point-v qd-point)))))

;;; UPDATE -- makes the connector point to the right places.

(define-handler update (connector)
  (let* ((startActor (startActor me))
         (endActor (endActor me))
         (startGeometry (startPointGeometry me))
         (endGeometry (endPointGeometry me))
         startx starty endx endy)
    ;; compute the coords for as if the geometry was :Center.
    (if startActor
      (SK8-multival-setf (startx starty) (location startActor :physical t))
      (sk8-multival-setf (startx starty) (startPoint me :physical t)))
    (if endActor
      (SK8-multival-setf (endx endy) (location endActor :physical t))
      (sk8-multival-setf (endx endy) (endPoint me :physical t)))
    ;; if it isn't, call get-connect-point.
    (when (and startActor (neq startGeometry 'center))
      (SK8-multival-setf (startx starty)
                         (get-connect-point startActor endx endy startGeometry)))
    (when (and endActor (neq endGeometry 'center))
      (SK8-multival-setf (endx endy) 
                         (get-connect-point endActor startx starty endGeometry)))
    ;; And set the end points of the lineSegment and we are done!
    (setEndPoints me startx starty endx endy :physical t)))

;;; StartGeometry -- returns the start geometry of the connector.

(define-handler startGeometry (connector)
  (startPointGeometry me))

;;; SETF STARTGEOMETRY -- sets the start geometry of a connector.

(define-handler (setf startGeometry) (geom connector)
  (when (memq geom '(projectCenter center horizontalEdge closestEdge verticalEdge))
    (setf (startPointGeometry me) geom)
    (update me)))

;;; EndGeometry -- returns the start geometry of the connector.

(define-handler endGeometry (connector)
  (endPointGeometry me))

;;; SETF STARTGEOMETRY -- sets the start geometry of a connector.

(define-handler (setf endGeometry) (geom connector)
  (when (memq geom '(projectCenter center horizontalEdge closestEdge verticalEdge))
    (setf (endPointGeometry me) geom)
    (update me)))

;;; _______________________________ 
;;; Actor additions.
;;; _______________________________ 

;;; CONNECTED -- event sent to both actors when they are connected.

(define-handler connected (actor otherActor theConnector)
  (declare (ignore otherActor theConnector))
  nil)

;;; DISCONNECT -- removes the connector from the lines field of the node.

(define-handler removeConnector (Actor theConnector)
  (setf (gs:node-lines me) (remove theConnector (gs:node-lines me)))
  (when (null (gs:node-lines me))
    (gs:hasConnectors! (gs:node-flags me) 0))
  t)

;;; CONNECT -- adds the connector to the lines of the node of me.

(define-handler addConnector (actor theConnector)
  (when (null (gs:node-lines me))
    (gs:hasConnectors! (gs:node-flags me)))
  (setf (gs:node-lines me) (push theConnector (gs:node-lines me)))      
  t)

;;; CONNECTORS -- returns the connectors that self has.

(define-handler connectors (actor)
  (gs:node-lines me))

;;; ConnectedActors -- returns all the actors connected to self.

(define-handler connectedActors (actor)
  (let (candidate)
    (mapcar #'(lambda (theConnector)
                (setf candidate (startActor theConnector))
                (if (eq me candidate)
                  (endActor theConnector)
                  candidate))
            (connectors me))))

;;; Returns every actor at the endActor end of the connector.

(define-handler connectedTo (actor)
  (let ((result nil))
    (dolist (c (connectors me) result)
      (unless (eq me (endActor c))
        (push (endActor c) result)))))

;;; Returns every actor at the endActor end of the connector.

(define-handler connectedFrom (actor)
  (let ((result nil))
    (dolist (c (connectors me) result)
      (unless (eq me (startActor c))
        (push (startActor c) result)))))

;;; connectActors -- connects two actors using a connector if one is provided (otherwise one is created).
;;;              by default the connector shows up behind the actors it connects.

(define-handler connect (actor endActor 
                                        &key theConnector
                                        (newConnector t))
  (let ((actorContainer (container me)))
    ;; error checking...
    (cond ((eq me endActor)
           (sk8-error GeneralProgrammaticError
                      :strings '("Cannot connect an actor to itself.")))
          ;; UF! Getting on with it.
          (t (let ((cnctr (if newConnector
                            (new (or theConnector connector) :project (project me))
                            (or theConnector (sk8-error GeneralProgrammaticError
                                                        :strings '("A connector should be provided for use!"))))))
               ;; set the connector data. This disconnects the old actors.
               (setf (startActor cnctr) me)
               (setf (endActor cnctr) endActor)
               ;; Now, get the connector to be in the right place and attach it to the
               ;; container of the guys it connects.
               (when actorContainer
                 (if (eq actorContainer Stage)
                   (progn (update cnctr)
                          (setf (container cnctr) actorContainer)
                          (sendToBack cnctr))
                   (withActorLocked (actorContainer)
                     (update cnctr)
                     (setf (container cnctr) actorContainer)
                     (sendToBack cnctr))))
               ;; Tell both actors that they have been connected.
               (connected me endActor cnctr)
               (connected endActor me cnctr)
               ;; return the connector
               cnctr)))))

;;; disconnect -- disconnects two actors by getting rid of the connectors that joins them. We find the
;;;           right connector and dispose it.

(define-handler disconnect (actor otherActor &key (disposeConnector t))
  (let ((theConnector (dolist (c (connectors me) nil)
                        (when (or (eq (startActor c) otherActor)
                                  (eq (endActor c) otherActor))
                          (return c)))))
    (if theConnector
      (progn
        (setf (startActor theconnector) nil)
        (setf (endActor theConnector) nil)
        (when disposeConnector 
          ;; (simpleObjectClear theConnector)
          ))
      (sk8-error GeneralProgrammaticError
                 :strings '("" " and " " are not connected!")
                 :objects (list me otherActor)))))

;;; _______________________________ 
;;; Editable properties.
;;; _______________________________ 

(define-handler localvirtualproperties (Connector)
  (when (eq me Connector)
    '(endGeometry startGeometry)))


#|
	Change History (most recent last):
	2	6/1/93	Hernan	Removed resizeable and draggable, two ancient
				properties that served no purpose now.
	3	6/8/93	Hernan	Directional connectors created. Also changed the
				startActor and endActor setters to be settable as
				well.
	4	6/22/93	Hernan	Made the connector non draggable and non resizable
				so that the UI does not let you move it around.
	5	6/25/93	Hernan	InitializeObject -> initialize.
	7	7/6/93	chip	
	11	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	12	1/11/94	hernan	self -> me
	13	2/12/94	kleiman	renaming
	14	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15	2/25/94	hernan	Using symbols instead of keywords for options!!!
	16	2/28/94	hernan	Commenting out dispose.
	17	3/3/94	kleiman	properties -> addproperty
	18	3/3/94	kleiman	private properties declared via makeprivateproperty
	19	4/5/94	kleiman	code review
				removed compiler warnings
				fixed bug in reverseConnection
	20	4/8/94	Hernan	Setting the linesize of the connector to 1. Also,
				directionalConnector is a child of Arrow instead
				of line.
	21	7/1/94	Hernan	1172178: fixed get-connect-point to use more
				modern SK8 programming. Fixed bug.
	22	7/18/94	Hernan	1172685: connectors are now very permissive. They never
				crash when the actors connected are missing or their
				container is different. This should make them a lot more
				usable.
	23 	 9/ 9/94	Hernan  	1185063: declaring get-connect-point :private.
	24 	 9/12/94	Hernan  	1180502: capitalizing object names.
	25 	10/17/94	dy      	ownedRegion is now in p-list
	26 	10/21/94	Hernan  	added connector's creationRelations
	27 	11/ 8/94	Hernan  	Ooops. The ownedRegion that Dave messed with
							had nothing to do with the actor's ownedRegion
							property.
	28 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	29 	 2/16/95	sidney  	readable argument names for initialize handler
	30 	 4/28/95	Hernan  	1244503: set startActor of Connector (and set endActor)
							now clear the slot when the new actor is nil.
							(Brian and Sidney were here too)
	2  	 6/23/95	Hernan  	1259048
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3  11/15/96Hernan  Initialize cannot assume that the "original" arg will have an
						object of the right type. Calling originalAncestor to deal with it.
|# ;(do not edit past this line!!)
