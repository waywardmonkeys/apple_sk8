;;; SK8 Actor Definitions
;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated 11-15-96   5:43 pm
                  SK8::DISPLAYSAMPLE SK8::EVENTWINDOW SK8::ORIGINALANCESTOR)

(provide "ACTOR")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

03-02-93 ruben *events-on* removed from attach/stage
02-22-93 ruben inheritsFrom? -> inheritsFrom
02-02-93 ruben without-interrupts in attach for stage
               containerCollection added to Actor here
01-31-93 ruben added menuselect event handler (Bug #324)
               various event handlers for actor now check container before passing mousedown to it
01-24-93 hernan changing all x/ys to h/vs and adding functionality to top/right/left/bottom methods
                to handle movement without resizing.
01-22-93 ruben id -> objectName
01-14-93 hernan defined locked on actor for consistency with the field (BUG#167)
10-03-92 ruben corrected handler congruencies, removed locals conflicting with SK8Script constants
30-09-92 Ice and Hernan removing all actor slots that we can.
26-09-92 Ruben & Ice - Dave integrated ruben's changes to NewChildDeep,
                       New, and 
20-09-92 hernan changing this to work with MF VI
07-26-92 ruben changed to SK8 package; cl-user -> sk8 package references where needed
07-21-92 Hernan making actor methods deal with actors that have windows.
06-19-92 adam  changed old :center to :center-projection. Now :center means THE center.
06-13-92 ruben drag [actor] takes delay? keyword, (setf fillColor) and dispose-color take care of
               temporary PICT handle colors
06-05-92   3:41 pm  ADAM  actor's make-code now saves clipobject relation
05-30-92 ruben netwalk/parents -> inheritsFrom?
05-12-92 ruben d29 conversion; get-group => group
03-29-92 ruben began d29; fillcolor, framecolor, textcolor, textfont,
               textsize, textstyle, checkcolor, location, rectangle, 
               size, framesize, visible, text, checksize, clipobject
               hilite, signified, removed name, number, height , width,
               project, window, card, layer, autohilite, partnerFrame, partnerSlot
               swept for get- and set-
03-28-92 ruben speeded up make-code by about 2x; got rid of unnecessary properties
03-26-92 ruben NEW w/create-object & define-object added min-width, min-height, popUpMenu
               & gentemp name fix in NEW
03-24-92 adam The coords-distance macro now works properly (so methods that used it
              now work properly -- set-rotation, line-properties-to-points, 
              line-points-to-properties).
03-16-92 ruben redefined Actor object
01-19-92 RUBEN converted to macframes II
08:50:48  09-24-1991 RUBEN  new no longer coerces new object into SK8 frameset
14:56:45  09-19-1991 RUBEN  new end-user method objects-near
14:56:28  09-19-1991 RUBEN  rect-promixity-measure for object-near
14:55:45  09-19-1991 RUBEN  rects-overlap-p new function for objects-near
14:54:57  09-19-1991 RUBEN  nmapcar utility
11:49:24  09-13-1991 RUBEN  dispose-colors doesn't dirty a movie object
09-09-91 RUBEN EXCEPTED-P is a new macro; make-code uses excepted-p macro
09:45:31  09-06-1991 RUBEN  SET-RECTANGLE calls gs:dispatch-event
10:18:56  09-05-1991 RUBEN  get-card checks for struct
08:47:42  09-05-1991 RUBEN  make-code ensures object in card before adding it to it
11:50:04  09-04-1991 ADAM  made get-selection consistent with selected
11:50:04  09-04-1991 ADAM  selected now returns true only if card in author mode
13:42:04  08-30-1991 RUBEN overlap called set-rectangle instead of get-rectangle
08-22-91 ruben sk8-color-p => sk8-color-object-p
08-21-91 ruben sed-object -> graphic-object
08-16-91 ruben make-code cosmetics
08-14-91 ruben added *default-Actor-size* var
08-13-91 ruben took out unnecessary checks; added send-message-if-test-passes macro
08-02-91 ruben dispose-colors checks for group and fields
07-27-91 ruben make-signified looks into default-signified slot for
               signification and in default-signified-relation slot
               for signification relation
07-25-91  adam - fixed :physical behavior of "set-location" and "get-size"

|#

(defmacro withActorLocked ((theActor &key ((:effect e)) speed) &body body)
  (let ((window (gensym)))
    `(let* ((,window (gs:node-window ,theActor)))
       (if ,window
         (gs:lockwindow ,window))
       (unwind-protect
         (progn
           ,@body)
         (if ,window
           (gs:unlockwindow ,window :visual-effect ,e :effect-speed ,speed))))))


(defmacro sk8::|WITH LOCKEDACTOR| ((actr &key ((:effect e)) speed) &body body)
  `(withActorLocked (,actr :effect ,e :speed ,speed)
     ,@body))

(defmacro withActorLockedPossiblyFalse ((theActor &key ((:effect e)) speed) &body body)
  (let ((window (gensym)))
    `(let* ((,window (and ,theActor (gs:node-window ,theActor))))
       (if ,window
         (gs:lockwindow ,window))
       (unwind-protect
         (progn ,@body)
         (if ,window
           (gs:unlockwindow ,window :visual-effect ,e :effect-speed ,speed))))))

;;; These are the things that are turned on:
;;; (1) system:  opaque, frameDirty, fillDirty, boundsDirty, onlylocationchanged, physicalBoundsRectDirty.
;;; (2) user: visible, draggable, resizable, inverted, acceptsDrops.

(defparameter *actor-default-flags* (cons 0 0))

(gs:opaque!                  *actor-default-flags* 1)
(gs:frameDirty!              *actor-default-flags* 1)
(gs:fillDirty!               *actor-default-flags* 1)
(gs:boundsDirty!             *actor-default-flags* 1)
(gs:onlyLocationChanged!     *actor-default-flags* 0)
(gs:physicalBoundsRectDirty! *actor-default-flags* 1)

(gs:visible!        *actor-default-flags* 1)
(gs:draggable!      *actor-default-flags* 1)
(gs:resizable!      *actor-default-flags* 1)
(gs:inverted!       *actor-default-flags* 1)
(gs:acceptsDrops!   *actor-default-flags* 1)
(gs:offsetsRegions! *actor-default-flags* 1)

;;; It Is CRUCIAL to keep this in synch with the node slot block definition
;;; in macros.lisp

;;; SOME DAY, make actor a Collection again.

(new AliasedCollection
     :otherparents Graphic
     :objectName "Actor"
     :undisposable t
     :prototype t
     :project sk8
     :properties
     `((OSWindow :value nil)
       (fillRegion :value nil)
       (frameRegion :value nil)
       (frameSize :value nil)
       (boundsRegion :value nil)
       (logicalBoundsRect :value nil)
       (physicalBoundsRect :value nil)
       (logicalXScale :value 1.0s0)
       (logicalYScale :value 1.0s0)
       (xScale :value 1.0s0)
       (yScale :value 1.0s0)
       (xOrigin :value 0s0)
       (yOrigin :value 0s0)
       (flags :value nil)
       (mouseStatus :value 0)
       (pixmap :value nil)
       (drawFunction :value nil)
       (contentsDrawFunction :value nil)
       (props :value nil)
       (translucentContents :value 0)
       (offsetBy :value nil)
       (lines :value nil)
       (doubleClickStyle :value sk8::standard)
       (frameColor :value ,sk8::black)
       (fillColor :value ,sk8::white)
       (container :value nil)
       (contents :private :value nil)))

(setf (localCreationRelations Actor) '(SK8::contents* SK8::[container] SK8::lines*))

(define-handler collectionProperty (Actor)
  'contents)

(setf (flags actor) *actor-default-flags*)

(setf (private Actor :property `OSWindow) t)
(setf (private Actor :property `logicalBoundsRect) t)
(setf (private Actor :property `physicalBoundsRect) t)
(setf (private Actor :property `logicalXScale) t)

(setf (private Actor :property `logicalYScale) t)
(setf (private Actor :property `xScale) t)
(setf (private Actor :property `yScale) t)
(setf (private Actor :property `xOrigin) t)
(setf (private Actor :property `yOrigin) t)
(setf (private Actor :property `flags) t)
(setf (private Actor :property `mouseStatus) t)
(setf (private Actor :property `pixmap) t)
(setf (private Actor :property `drawFunction) t)
(setf (private Actor :property `contentsDrawFunction) t)
(setf (private Actor :property `props) t)
(setf (private Actor :property `translucentContents) t)
(setf (private Actor :property `offsetBy) t)
(setf (private Actor :property `lines) t)

;;; *** THIS SHOULD BE DONE IN THE INITIALIZE FOR ALL ACTORS!!!

(setf (contents Actor) (gs:make-dlist))
(setf (fillRegion Actor) (T_NewRgnGC))
(setf (frameRegion Actor) (T_NewRgnGC))
(setf (boundsRegion Actor) (T_NewRgnGC))
(setf (frameSize Actor)  (gs:make-pt :h 1.0S0 :v 1.0S0))

;; OJO! It is crucial that these rects start at (0,0). Otherwise, the top level actor
;; of a window does not cover the whole window.

(setf (logicalBoundsRect Actor) (gs:make-rect :top 0s0 :left 0s0 :bottom 30s0 :right 30s0))
(setf (physicalBoundsRect Actor) (gs:make-rect :top 0s0 :left 0s0 :bottom 30s0 :right 30s0))
(setf (offsetBy Actor) (gs:make-pt :v 0 :h 0))
(setf (drawFunction Actor) 'gs:sk8-simple-draw)
(setf (contentsDrawFunction Actor) 'gs:sk8-draw-contents) 

;;; _______________________________ 
;;; Marking unsettable secret properties.
;;; _______________________________ 

(define-handler windowStyle (Actor)
  'sk8::blank)
(define-handler (setf windowStyle) (newValue Actor)
  (declare (ignore newValue))
  (sk8-error GeneralProgrammaticError
             :strings '("This property is not settable.")))

(define-handler OSWindow :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf OSWindow) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler logicalBoundsRect :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf logicalBoundsRect) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler physicalBoundsRect :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf physicalBoundsRect) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler logicalXScale :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf logicalXScale) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler logicalYScale :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf logicalYScale) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler XScale :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf XScale) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler YScale :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf YScale) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler XOrigin :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf XOrigin) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler YOrigin :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf YOrigin) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler flags :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf flags) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler mouseStatus :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf mouseStatus) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler pixmap :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf pixmap) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler drawFunction :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf drawFunction) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler contentsDrawFunction :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf contentsDrawFunction) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler props :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf props) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler translucentContents :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf translucentContents) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler offsetBy :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf offsetBy) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

(define-handler lines :private (Actor)
                gs:*unsettable-secret-property-message*)
(define-handler (setf lines) :private (newValue Actor)
                (declare (ignore newValue))
                (sk8-error GeneralProgrammaticError
                           :strings '("This property is not settable.")))

;;; _______________________________ 
;;; Now the real stuff...
;;; _______________________________ 

;;; We make actor have a struct right away, since the newChild method assumes this is so.
;;;(gs:set-struct actor (gs:make-node))

;;; RESIZED -- called when an actor has actuall changed its geometry. We make sure the connectors remain in
;;;          the right places.

(define-handler resized (actor)
  (let ((flags (gs:node-flags me)))
    ;; Move the connectors.
    (when (gs:hasConnectors? flags)
      (dolist (c (gs:node-lines me))
        (update c)))
    ;; Deal with resizesContents.
    (when (gs:resizesContents? flags)
      (scaleContents me))
    ;; Deal with bounded by contents.
    (when (gs:belongsToBoundedByContents? flags)
      (bindByContents (gs:node-container me)))
    (when (gs:someRenderersAreDynamic? flags)
      (when (gs:fillColorIsDynamic? flags) (resized (fillColor me)))
      (when (gs:textColorIsDynamic? flags) (resized (textColor me)))
      (when (gs:frameColorIsDynamic? flags) (resized (frameColor me)))))
  (sk8-return-nothing))

;;; MOVED -- called when an actor is just being moved. Makes sure the connectors are kept in synch.

(define-handler moved (actor)
  (let ((flags (gs:node-flags me)))
    (when (gs:hasConnectors? flags)
      (dolist (c (gs:node-lines me))
        (update c)))
    (when (gs:belongsToBoundedByContents? flags)
      (bindByContents (gs:node-container me)))
    (when (gs:someRenderersAreDynamic? flags)
      (when ( gs:fillColorIsDynamic? flags) (moved ( fillColor me)))
      (when ( gs:textColorIsDynamic? flags) (moved ( textColor me)))
      (when (gs:frameColorIsDynamic? flags) (moved (frameColor me))))))

;;; When an actor gets added to another, the new container need to do some work
;;; if the actor is bounded by contents, we need to make the newContent be a bindsByContentsMixin.

(define-handler prepareForNewContent (actor newContent)
  (let ((flags (gs:node-flags me)))
    (when (gs:resizesContents? flags)
      (cacheContentsRects me))
    (when (boundedByContents me)
      (when newContent
        (gs:belongsToBoundedByContents! (gs:node-flags newContent) 1))
      ;; Evaled enqueued because the newContents is not really in the actor yet,
      ;; and thus, the rect info is not ready yet to do the bindByContents.
      (gs:with-queued-drawing
        `(when (canDo 'bindByContents ,me)
           (bindByContents ,me))))))

(define-handler prepareForNewContainer (actor newContainer)
  (cond ((and newContainer (neq newContainer Stage) (boundedByContents newContainer))
         ;; Entering a bounded by contents Actor.
         (gs:belongsToBoundedByContents! (gs:node-flags me) 1)
         (bindByContents newContainer))
        ((gs:belongsToBoundedByContents? (gs:node-flags me))
         ;; Leaving a bounded by contents Actor.
         (gs:belongsToBoundedByContents! (gs:node-flags me) 0))))

(define-handler enteringStage (actor)
  (let ((flags (gs:node-flags me)))
    (when (gs:someRenderersAreDynamic? flags)
      (when ( gs:fillColorIsDynamic? flags) (enteringStage ( fillColor me)))
      (when ( gs:textColorIsDynamic? flags) (enteringStage ( textColor me)))
      (when (gs:frameColorIsDynamic? flags) (enteringStage (frameColor me)))))
  (sk8-return-nothing))

(define-handler leavingStage (actor)
  (let ((flags (gs:node-flags me)))
    (when (gs:someRenderersAreDynamic? flags)
      (when (gs:fillColorIsDynamic? flags) (leavingStage ( fillColor me)))
      (when (gs:textColorIsDynamic? flags) (leavingStage ( textColor me)))
      (when (gs:frameColorIsDynamic? flags) (leavingStage (frameColor me)))))
  (sk8-return-nothing))

(define-handler newContainerOK (actor newContainer)
  (declare (ignore newContainer))
  t)

(define-handler newContentOK (actor newContent)
  (declare (ignore newContent))
  t)

(define-handler newContentOk (stage newContent)
  (declare (ignore newContent))
  t)

(define-handler draggable (actor)
  (gs:draggable? (gs:node-flags me)))

(define-handler (setf draggable) (boolean actor)
  (if boolean
    (gs:draggable! (gs:node-flags me) 1)
    (gs:draggable! (gs:node-flags me) 0))
  boolean)

(define-handler resizable (actor)
  (gs:resizable? (gs:node-flags me)))

(define-handler (setf resizable) (boolean actor)
  (if boolean
    (gs:resizable! (gs:node-flags me) 1)
    (gs:resizable! (gs:node-flags me) 0))
  boolean)

(define-handler doubleClickStyle (actor)
  (gs:node-doubleClickStyle me))

(define-handler (setf doubleClickStyle) (style actor)
  (if (memq style '(sk8::standard doubleClickOnly clickOnly))
    (setf (gs:node-doubleClickStyle me) style)
    (sk8-error PropertyTypeMismatchError
               :object        style
               :expectedType  '(sk8::standard doubleClickOnly clickOnly)
               :ownerObject   Actor
               :propertyName 'doubleClickStyle
               )))

(define-handler floating (actor)
  (gs:floats? (gs:node-flags me)))

(define-handler (setf floating) (boolean actor)
  (unless (eq boolean (floating me))
    (let ((theFlags (gs:node-flags me)))
      (if boolean
        (gs:floats! theFlags)
        (gs:floats! theFlags 0))
      (when (gs:hasWindow? theFlags)
        (if boolean
          (progn (change-class (gs:node-window me) 'gs:*sk8-windoid*)
                 (gs:selecting-clos-window
                   ;; Make sure the window moves to the windoid layer.
                   ;; Note that using 0 instead of 1 throws off the window
                   ;; system!!!
                   ;;(set-window-layer (gs:node-window me) 1)))
                   (ccl::reselect-windows)))
          (progn (change-class (gs:node-window me) 'gs:*sk8-window*)
                 (gs:disable-whiting)
                 (set-window-layer (gs:node-window me) 
                                   (1+ (window-layer gs:*currentTla*)))))
        )
      boolean)))

(define-handler acceptsDrops (actor)
  (gs:acceptsDrops? (gs:node-flags me)))

(define-handler (setf acceptsDrops) (boolean actor)
  (if boolean
    (gs:acceptsDrops! (gs:node-flags me))
    (gs:acceptsDrops! (gs:node-flags me) 0)))

(define-handler minimumSize (actor)
  (sk8-multivals 2 2))

;;; _______________________________ 
;;; HANDLERS FOR TOP LEVEL ACTORS 
;;; _______________________________ 

;;; These are methods used only by the top level actors. They are all preceded by TLA. This is 
;;; ugly but necessary to allow all actors to be top level actors. 

(defun TLA-boundsRect (theactor)
  (let* ((clos-window (gs:node-window theActor))
         (position (view-position clos-window))
         (physRect (gs:node-physicalBoundsRect theActor))
         (left (point-h position))
         (top (point-v position)))
    (sk8-multivals left top 
                   (+ left (gs:rect-right physRect)) 
                   (+ top (gs:rect-bottom physRect)))))

;;; This function is used exclusively to change the size of tool windows. The problem is that tool
;;; windows do not work with zooming, crashing the system into mas bugs if you try to do things
;;; that way. Thus, when setWindowRect detects it is about to change the boundsrect of a tool
;;; window, it calls this function instead.

(defun setWindowRect-for-toolwindow (window left top right bottom)
  (let ((wptr (wptr window)))
    (without-interrupts
     (#_hideWindow wptr)
     (#_moveWindow wptr left top nil)
     (#_sizeWindow wptr (- right left) (- bottom top) t)
     (#_showWindow wptr))))

;;; This function only does the window related work: changes the view size and the view position so that
;;; the location of the window does not change.

(defun setWindowRect (window left top right bottom)
  (gs:let+ ((oldBounds (:region))
            (wptr (wptr window))
            (sk8windowClass (find-class 'gs:*sk8-window*))
            (bgkWindowClass (find-class 'gs:*bkg-window*))
            ;; (oldPosition (view-position window))
            )
    (#_CopyRgn (rref wptr :window.strucRgn) oldBounds)
    (if (eq (window-type window) :tool)
      (setWindowRect-for-toolWindow window left top right bottom)
      (progn
        ;; Do the visible part.
        (set-window-zoom-position window left top)
        (set-window-zoom-size window (- right left) (- bottom top))
        (gs:disable-whiting)
        (window-zoom-event-handler window 8)
        (gs:enable-whiting)))
    ;; In the PPC, need to recompute the WDEF regions BEFORE the
    ;; move takes place. This ensures window manager happiness. 
    #+ppc-target
    (when (eq (sk8::windowStyle (slot-value window 'gs:my-actor-object)) 'sk8::blank)
      (gs::wdef-calc-regions wptr))
    ;; The window regions do not seem to recompute without this line!!!
    (#_moveWindow wptr left top nil)
    ;; (#_diffRgn oldBounds (rref wptr :window.strucRgn) oldBounds)
    ;; Now set the udpate region of all windows that intersect the oldBounds.
    (without-interrupts
     (map-windows #'(lambda (w)
                      (when (or (eq sk8WindowClass (class-of w))
                                (eq bgkWindowClass (class-of w)))
                        (#_calcVis (wptr w))))
                  :include-windoids t)
     (#_paintBehind (wptr window) oldBounds)
     (gs:redraw-background))
    ))

;;; prepare-for-region-calculation -- this handler makes sure the actor is ready to use its
;;;                           boundsRegion as the window of its region. Gets called when actor
;;;                           is first attached to the stage. Offsets the rects so that their
;;;                           topLeft corner is {0,0}.

(define-handler convert-to-tla-regions :private (actor width height)
                       (let ((flags (gs:node-flags me)))
                         (gs:simple-set-rect-size (gs:node-logicalBoundsRect me) width height)
                         (gs:simple-set-physical-rect-size (gs:node-physicalBoundsRect me) width height)
                         (gs:physicalBoundsRectDirty! flags 0)
                         (gs:logicalBoundsRectDirty! flags 0)
                         (gs:onlyLocationChanged! flags 0)
                         (gs:boundsDirty! flags)
                         (gs:fillDirty! flags)
                         (gs:frameDirty! flags)))

;;; revert-to-nontla-regions -- called when actor is ready to leave the stage. 

(define-handler revert-to-nontla-regions :private (actor clos-window)
                       (let* ((flags (gs:node-flags me))
                              (pos (view-position clos-window))
                              (x (point-h pos))
                              (y (point-v pos)))
                         (gs:offset-rect (gs:node-logicalBoundsRect me) x y)
                         (gs:offset-rect (gs:node-physicalBoundsRect me) x y)
                         (gs:physicalBoundsRectDirty! flags 0)
                         (gs:logicalBoundsRectDirty! flags 0)
                         (gs:onlyLocationChanged! flags 0)))

;;; prepare-for-wdef-region-calculation -- called each time we are about to change the window's
;;;                               geometry. The idea is to leave everything ready for
;;;                               an accurate region calculation in the WDEF code. 

(define-handler prepare-for-wdef-region-calculation :private (actor width height)
                       (let ((flags (gs:node-flags me)))
                         (gs:simple-set-rect-size (gs:node-logicalBoundsRect me) width height)
                         (gs:simple-set-physical-rect-size (gs:node-physicalBoundsRect me) width height)
                         (gs:physicalBoundsRectDirty! flags 0)
                         (gs:logicalBoundsRectDirty! flags 0)
                         (gs:onlyLocationChanged! flags 0)))

(defun setf-TLA-boundsRect (theActor left top right bottom &key relative justmoving)
  (let* ((clos-window (gs:node-window theActor))
         (old-pos (view-position clos-window))
         (old-left (point-h old-pos))
         (old-top (point-v old-pos))
         (old-size (view-size clos-window))
         width height new-size
         (flags (gs:node-flags theActor)))
    (if relative
      (setf left (+ left (point-h old-pos))
            top (+ top (point-v old-pos))
            width (- (+ old-left (point-h old-size) right) left)
            height (- (+ old-top (point-v old-size) bottom) top))
      (setf width (- right left)
            height (- bottom top)))
    (setf new-size (make-point width height))
    ;; Set the Rects to allow the window to recompute its region properly.
    (prepare-for-wdef-region-calculation theActor width height)
    (unless (eq old-size new-size)
      (gs:boundsDirty! flags)
      (gs:fillDirty! flags)
      (gs:frameDirty! flags)
      ;; Changing the size/location of the window.
      (setf (slot-value clos-window 'gs:drawEnabled) nil))
    (setWindowRect clos-window left top (+ left width) (+ top height))
    (if (= old-Size new-Size)
      ;; Why this? We want to let the user control what event gets sent using the justMoving
      ;; arg. At the same time we do not want to cause gross redraw errors if the user wants
      ;; to call moved when the actor is actually being resized.
      (progn 
        (setf (slot-value clos-window 'gs:drawenabled) t)
        (if justMoving (moved theActor) (resized theActor)))
      (progn
        (gs:SK8-update-window-gWorld clos-window :flushBits t)
        (gs:unlockWindow clos-window :force t)
        ;; Redraw!!!
        (gs:making-self-dirty (theActor nil nil t t (gs:node-boundsRegion theActor) :bounds nil)
          (resized theActor)
          (setf (slot-value clos-window 'gs:drawEnabled) t))
        ))
    ;; The window moved and it is not the current window. Make sure it gets an update event.
    (unless (eq clos-window gs:*currentTla*)
      (gs:disable-whiting)
      (#_paintOne (wptr clos-window) (rref (wptr clos-window) :windowRecord.strucrgn)))
    ))

;;; LOCATION -- returns the position of the clos window of this actor.

(defun TLA-location (theactor &key physical)
  (declare (ignore physical))
  (let* ((clos-window (gs:node-window theActor))
         pos)
    (setf pos (view-position clos-window))
    (SK8-multival-bind (width height) (size theactor :physical t)
      (SK8-multivals (gs:f.round (+ (/ width 2) (point-h pos)))
                     (gs:f.round (+ (/ height 2) (point-v pos)))))))

;;; (SETF-TLA- LOCATION) -- sets the location of a window. This involves two things: moving the clos window
;;;                     and updating the location and rects of the top level actor.

(defun setf-TLA-location (theactor x y &key relative)
  (let ((clos-window (gs:node-window theActor)))
    (if relative
      (let* ((pos (view-position clos-window))
             (vx (point-h pos))
             (vy (point-v pos)))
        (set-view-position clos-window (+ vx (gs:f.round x)) (+ vy (gs:f.round y))))
      (SK8-multival-bind (width height) (size theactor)
        (set-view-position clos-window 
                           (gs:f.round (- x (/ width 2)))
                           (gs:f.round (- y (/ height 2))))))
    (moved theActor)))

;;; COLORDEPTH -- returns the depth the actor wants to use when it attaches itself to the stage.

(define-handler colorDepth (actor)
  (getf (gs:node-properties me) :depth 0))

;;; (SETF ColorDEPTH) -- sets the depth the actor wants to use to show itself on the stage.

(define-handler (setf colorDepth) (depth actor)
  (unless (eq (colorDepth me) depth)
    ;; Error checking...
    (unless (memq depth '(nil 0 1 2 4 8 16 32))
      (sk8-error PropertyTypeMismatchError
                 :object        depth
                 :expectedType  '(nil 0 1 2 4 8 16 32)
                 :ownerObject   Actor
                 :propertyName 'colorDepth
                 ))
    ;; update the plist.
    (setf (getf (gs:node-properties me) :depth) depth)
    (when (gs:hasWindow? (gs:node-flags me))
      (let ((closwindow (gs:node-window me)))
        (without-interrupts
         (setf (slot-value closwindow 'gs:depth) depth)
         (gs:sk8-update-window-gworld closwindow :flushBits t)
         (forceRedraw me)))))
  depth)

;;; If the window has its depth set to a non zero value, do nothing. If it is set
;;; to zero we need to update the gWorld. If it is set to nil, just forcing a redraw
;;; does the job.

(defun update-windows-for-new-depth ()
  (let (depth)
    (do-sk8-window-actors (c)
      (setf depth (colorDepth c))
      (if (null depth) 
        (forceRedraw c)
        (when (= depth 0)
          (let ((closwindow (gs:node-window c)))
            (setf (slot-value closwindow 'gs:depth) depth)
            (gs:sk8-update-window-gworld closwindow :flushBits t)
            (forceRedraw c)))))))

;;; DoFirstClick -- returns whether the actor gets a mousedown event when it activates
;;;             (only makes sense for top level actors).

(define-handler doFirstClick (actor)
  (getf (gs:node-properties me) :doFirstClick nil))

(define-handler (setf doFirstClick) (doFirstClick actor)
  (unless (eq (doFirstClick me) doFirstClick)
    ;; update the plist.
    (setf (getf (gs:node-properties me) :doFirstClick) doFirstClick)
    (when (gs:hasWindow? (gs:node-flags me))
      (let ((closwindow (gs:node-window me)))
        (setf (slot-value closwindow 'gs:first-click) doFirstClick))))
  doFirstClick)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                           HANDLERS                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DeactivateText and ActivateText -- called to activate the currentKey. When the currentKey is not a simpleText,
;;;                             we do nothing.

(define-handler activateText (actor)
  )

(define-handler deactivateText (actor)
  )

;;; 1.0
;;; ACTIVATE 

(define-handler activate (actor)
  (let (curkey)
    (when (gs:hasWindow? (gs:node-flags me))
      (setf curKey (keyTarget me))
      (when curkey (activateText curkey))))
  t)

;;; 1.0
;;; DEACTIVATE

(define-handler deactivate (actor)
  (let (curkey)
    (when (gs:hasWindow? (gs:node-flags me))
      (setf curKey (keyTarget me))
      (when curkey (deactivateText curkey))
      ))
  t)

;;; --------------
;;; REGIONS
;;; --------------

;;; The getters always return regions that have been recomputed.

(define-handler boundsRegion (actor)
  (gs:recompute-bounds-region me (gs:node-flags me))
  (gs:node-boundsRegion me))

(define-handler fillRegion (actor)
  (gs:recompute-fill-region me (gs:node-flags me))
  (gs:node-fillRegion me))

(define-handler frameRegion (actor)
  (gs:recompute-frame-region me (gs:node-flags me))
  (gs:node-frameRegion me))

;;; These default handlers clear the dirty bit so that they can be called using
;;; call next handler in SK8Script when writing new actor geometries.

(define-handler makeBoundsRegion (actor)
  (gs:boundsDirty! (gs:node-flags me) 0))

;;; NOTE: the following two methods are 

;;; In general, the default behaviour is for the frame = bounds - fill.

(define-handler makeFrameRegion (actor)
  (let ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (gs:recompute-fill-region me flags)
    (#_diffRgn (gs:node-boundsRegion me) 
     (gs:node-fillRegion me)
     (gs:node-frameRegion me))
    (gs:frameDirty! flags 0)))

;;; The fill is, in general, and inseted bounds.

(define-handler makeFillRegion (actor)
  (let ((theFlags (gs:node-flags me)))
    (gs:recompute-bounds-region me theFlags)
    (sk8-multival-bind (frameH frameV) (frameSize me :physical t)
      (#_copyRgn (gs:node-boundsRegion me) (gs:node-fillRegion me))
      (#_insetRgn (gs:node-fillRegion me) frameH frameV))
    (gs:fillDirty! (gs:node-flags me) 0)))

;;; These functions are provided for people that need to tell the system that one of the
;;; regions is ok but cannot call next method to do it.

(define-sk8-function boundsRegionOk nil (anActor)
  (gs:boundsDirty! (gs:node-flags anActor) 0))

(define-sk8-function fillRegionOk nil (anActor)
  (gs:FillDirty! (gs:node-flags anActor) 0))

(define-sk8-function frameRegionOk nil (anActor)
  (gs:frameDirty! (gs:node-flags anActor) 0))

(define-handler FillBoundsRect (actor &key physical)
  (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical physical)
    (sk8-multival-bind (hh vv) (framesize me :physical t)
      (sk8-multivals (+ ll hh) (+ tt vv) (- rr hh) (- bb vv)))))

;;; --------------
;;; CONTAINEMENT
;;; --------------

;;; 1.0 
;;; CONTAINER -- returns the container of self.

(define-handler container (actor)
  (if (gs:hasWindow? (gs:node-flags me))
    stage
    (gs:node-container me)))

;;; Runs through the contents without consing. Checks if theActor contains the suspect.

(defun checkContents (theActor suspect)
  (cond ((eq theActor suspect) t)
        (t (gs:docontents (subActor theActor)
             (when (checkContents subActor suspect)
               (return-from checkContents t))))))

;;; This gets called by set container to make sure the actor's colors are not used
;;; if they are translucent and the actor becomes a tla.

(defun preserve-renderers (me newContainer)
  (if (eq newContainer stage)
    ;; If fillcolor or framecolors are translucent, put them away.
    (let ((oldFillcolor (fillcolor me))
          (oldFrameColor (framecolor me))
          (properties (gs:node-properties me)))
      (when (translucent oldFillcolor)
        (setf (getf properties :savedFillcolor) oldFillcolor)
        (setf (fillcolor me) white))
      (when (translucent oldFrameColor)
        (setf (getf properties :savedFramecolor) oldFramecolor)
        (setf (framecolor me) black))
      (setf (gs:node-properties me) properties))
    ;; If there are any colors saved, restore them.
    (let ((properties (gs:node-properties me))
          color)
      (when (setf color (getf properties :savedFillcolor))
        (setf (fillcolor me) color))
      (when (setf color (getf properties :savedFramecolor))
        (setf (framecolor me) color))
      (remf properties :savedFillcolor)
      (remf properties :savedFrameColor)
      (setf (gs:node-properties me) properties))))

;;; 1.0
;;; (SETF CONTAINER) -- sets the container of self to actor. If actor is nil then self is
;;;                  detached from its current container.

(define-handler (setf container) (theActor actor &key following)
  (let ((oldContainer (container me)))
    
    ;; If old container is stage, make sure that it is properly initialized
    ;; (Remember that the container of this object will be the initialized
    ;; to be the same as its parent.) *** This can be taken out when
    ;; we have DEFAULT property value initialization in the object system
    ;; and we make the CONTAINER property's default be false.
    (when (or (and theActor
                   (eq oldContainer theActor)
                   ; Check whether it was wrongly initialized (weak test!):
                   (if (eq oldContainer Stage)
                     (not (position me (contents Stage))) ;; (positionOfItem Stage me))
                     (let ((contents (slot-value oldContainer 'contents)))
                       (dotimes (i (length contents) t)
                         (if (eq me (aref contents i)) (return nil))))))
              (not (or (eq oldContainer Stage)  ;; this should never happen, but
                       (inheritsfrom oldContainer Actor)))) ;; checking  prevents a crash when it does anyway
      (setq oldContainer nil)
      (setf (slot-value me 'container) nil))     ; Correct default initialization
    
    (gs:with-drawing-lock
      
      (cond ((null theActor)
             ;; Removal of the actor from its container!
             (when (and oldContainer (newContainerOk me theActor))
               (detach oldContainer me)
               (preserve-renderers me nil)))
            ;; We have an actor... do some error checking!
            ((eq theActor oldContainer)     ;; 1. container already set.
             t)                             ;;    (it is ignored)
            ((eq theActor me)             ;; 2. setting the container to itself.
             (sk8-error GeneralProgrammaticError
                        :strings '("Cannot set the container of an actor to itself!")))
            ((checkContents me theActor)  ;; 3. the new container is contained by self. 
             (sk8-error GeneralProgrammaticError
                        :strings '("Cannot put an actor into one of its contents.")))
            ;; No errors: proceed.
            (t
             ;; If permissions check, proceed with attachment.
             (when (and (newContainerOK me theActor)
                        (newContentOK theActor me))
               (if (eq theActor stage)
                 (progn (preserve-renderers me theActor)
                        (attach theActor me following))
                 (withActorLocked (theActor)
                   (attach theActor me following)
                   (preserve-renderers me theActor))))))
      )
    theActor))

(define-handler deepContents (actor)
  (let (result)
    (dolist (c (contents me) result)
      (setf result (nconc (cons c (deepContents c)) result)))))

(define-handler containers (actor)
  (let (Conts
        curCont
        (curItem me))
    (loop
      (setf curCont (container curItem))
      (if (or (eq curcont stage) (not curCont)) (return))
      (push curCont Conts)
      (setf curItem curCont))
    (nreverse conts)))

(defun every-subactor (theActor thePredicate &optional actorToAvoid)
  (let ((theResult t))
    (gs:docontents (c theActor)
      (when (and (neq c actorToAvoid) (not (funcall thePredicate c)))
        (setf theResult nil)
        (return)))
    theResult))

;;; 1.0
;;; CONTENTS -- returns the contents of self as a list of actors in front to back order (now it does!).

(define-handler contents (actor)
  (let (result)
    (dovector (oneActor (gs:node-contents me))
      (push oneActor result))
    result))

;;; Returns whether the actor has contents (without consing).

(define-handler hasContents (actor)
  (not (zerop (length (gs:node-contents me)))))

;;; Exactly the same as set contents of Stage. 

(define-handler (setf contents) (newValue Actor)
  (let ((oldContents (contents me))
        (count 1)
        (previousActor nil))
    ;;; [1] Remove old contents not in the new contents.
    (dolist (c oldContents)
      (unless (memq c newValue)
        (setf (container c) nil)))
    ;; [2] Add new contents and fix the layers. 
    (dolist (c newValue)
      (if (eq (container c) me)
        (unless (eql (layer c) count)
          (setf (layer c) count))
        (if previousActor
          (setf (container c :following previousActor) me)
          (setf (container c) me)))
      (incf count)
      (setf previousActor c))
    newValue))

(define-handler sk8::window (actor)
  (let ((container (container me)))
    (if container
      (if (eq container stage)
        me
        (sk8::window container))
      me)))

(define-handler containedBy (actor otherActor)
  (let ((container (container me)))
    (if (and container
             (neq container stage))
      (if (eq container otherActor)
        container
        (containedBy container otherActor)))))

;;; -------------
;;; OWNED REGION
;;; -------------

(define-handler ownedRegion (Actor)
  (or (getf (gs:node-properties me) :ownedRegion)
      (setf (getf (gs:node-properties me) :ownedRegion) (T_NewRgnGC))))

(define-handler (setf ownedRegion) :private (newRegion Actor)
  (setf (getf (gs:node-properties me) :ownedRegion) newRegion))

;;; OWNSREGION -- returns t if actor owns a region of the window.

(define-handler ownsRegion (actor)
  (gs:ownedRegion? (gs:node-flags me)))

;;; NOTE: The ownsRegion functionality allows an actor to own its region when it is already attached to a window.
;;;      This is necessary, for example when a QuickTimeRenderer is attached to an Actor.

(define-handler incrementOwnsRegionCount (Actor)
  (let* ((flags (gs:node-flags me))
         (clos-window (gs:node-window me)))
    (incf (getf (gs:node-properties me) :ownsRegionRefCount 0))
    (unless (gs:ownedRegion? flags)
      (gs:ownedRegion! flags)
      (when (and clos-window (not (position me (slot-value clos-window 'gs:owners))))
        (vector-push-extend me (slot-value clos-window 'gs:owners))
        (forceRedraw me) ;; To refresh the region.
        ))))

(define-handler decrementOwnsRegionCount (Actor)
  (let* ((flags (gs:node-flags me))
         (clos-window (gs:node-window me)))
    (assert (gs:ownedRegion? flags))
    (when (zerop (decf (getf (gs:node-properties me) :ownsRegionRefCount 0)))
      (gs:ownedRegion! flags 0)
      (remf (gs:node-properties me) :ownedRegion)
      (remf (gs:node-properties me) :ownsRegionRefCount)
      (when (and clos-window (position me (slot-value clos-window 'gs:owners)))
        (delete me (slot-value clos-window 'gs:owners))))))

;;; ------------------
;;; LOCATION AND SIZE
;;; ------------------

;;; 1.0
;;; Forceredraw -- makes any actor dirty.

(define-handler forceRedraw (actor)
  (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds nil)
    (gs:dirty-subnodes* me)))

(define-handler lightForceRedraw (Actor)
  (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds t)
    ))

;;;; BJR: for cases where the user wants things to update in a tight-loop...

(define-handler SendUpdateEvent (actor)
  (let ((clos-window (gs:node-window me)))
    (when clos-window
      (gs:sk8-update-event clos-window))))

;;; 1.0
;;; BOUNDSRECT -- returns the boundrect of self. Note that we need to translate the physical
;;;              bounds rect to stage coordinates before showing it to the user.

(define-handler boundsRect (Actor &key physical)
  (if (gs:hasWindow? (gs:node-flags me))
    (TLA-boundsRect me)
    (if physical
      (let ((rect (gs:recompute-physicalBoundsRect me)))
        ;; translating physical rect to stage coordinates.
        (gs:window-to-stage-rect me rect))
      (let ((rect (gs:recompute-logicalBoundsRect me)))
        (SK8-multivals (gs:rect-left rect) (gs:rect-top rect) (gs:rect-right rect) (gs:rect-bottom rect))))))

;;; 1.0
;;; SET-RECTANGLE-INTERNAL -- sets the size of the requested rectangle of self and
;;;                        updates the other rectangle. Changing the rect causes the
;;;                        invalidation of the regions the subactors have cached, and thus,
;;;                        we let them know this is the case.
;;;
;;; Important Note: this function assumes all coordinates given are in window coordinates
;;; (not in STAGE coordinates). Thus, if you call it, translate the stage coordinates first!!!

;;; There is no need to recompute the other rect. Just mark the one being set as clean and
;;; the other one as dirty. It will be recomputed when it needs to.

(defun set-rectangle-internal (me left top right bottom 
                                      &key 
                                      physical
                                      (resizing t) (dirtyContents t))
  (let ((flags (gs:node-flags me)))
    (if physical
      ;; Setting the physical rect.
      (let ((rect (gs:node-physicalBoundsRect me)))
        ;; rounding to integers.
        (setf left (gs:f.round left) top (gs:f.round top)
              right (gs:f.round right) bottom (gs:f.round bottom))
        (gs:set-rect rect left top right bottom)
        (gs:physicalBoundsRectDirty! flags 0)
        ;; updating the other rect.
        (gs:logicalBoundsRectDirty! flags)
        (gs:recompute-logicalBoundsRect me))
      ;; Setting the logical rect.
      (let ((rect (gs:node-logicalBoundsRect me)))
        ;; converting to short floats.
        (setf left (gs:int-to-fixed left) top (gs:int-to-fixed top)
              right (gs:int-to-fixed right) bottom (gs:int-to-fixed bottom))
        (gs:set-rect rect left top right bottom)
        (gs:logicalBoundsRectDirty! flags 0)
        ;; updating the other rect.
        (gs:physicalBoundsRectDirty! flags)
        (gs:recompute-physicalBoundsRect me)))
    ;; Let subactors know their regions are invalid.
    (when (and dirtyContents (not (gs:hasWindow? flags)))
      (gs:dirty-subnodes* me))
    ;; If the actor is being resized, and it offsets regions, we have to
    ;; recompute them right away! Why? Consider the following case: actor gets moved
    ;; outside the clip region of its container. Then gets resized. The regions are
    ;; not recomputed since there is no need. The regions do not match the physical 
    ;; boundsRect. Now move the actor. Since onlyLocationChanged will be t, the old
    ;; region will be offseted, which is incorrect. 
    (when (and resizing (gs:offsetsRegions? flags))
      (gs:recompute-bounds-region me flags)
      (gs:recompute-frame-region me flags)
      (gs:recompute-fill-region me flags))
    ;; Tell the actor it has resized itself.
    (if resizing
      (resized me)
      (moved me))
    t))

;;; 1.0
;;; (SETF BOUNDSRECT) -- Sets the bounds rect of self. The user has the option of setting the logical
;;;                   or the physical rects. If the user specifies a physical rect, and relative is nil,
;;;                   the rect is assumed to be expressed in stage coordinates (after all, this is how
;;;                   users believe rects to be represented). Therefore, we translate to window
;;;                   coordinates before calling set-rectangle-internal.

;;; Modified this to work with regions (not just rects!)

(macrolet ((callRendererSetBoundsRect (color)
             `(sk8-multival-setf (left top right bottom)
                                 (setBoundsRect (,color me)
                                                left top right bottom
                                                :physical physical))))
  
  (define-handler setBoundsRect (Actor left top right bottom
                                          &key physical relative justMoving)
      ;; [1] If relative, convert to not relative by applying offset.
      (when relative
        (SK8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
          (incf left ll) (incf top tt) (incf right rr) (incf bottom bb)))
      ;; [2] Enforcing the minimumSize!
      (unless justMoving
        (let (widthViolation heightViolation)
          ;; Determine whether a violation has occured.
          (sk8-multival-bind (width height) (minimumSize me)
            (if physical
              (progn
                (when (< (- right left) width) (setf widthViolation t))
                (when (< (- bottom top) height) (setf heightViolation t)))
              (let ((container (gs:node-container me)))
                (when (< (* (- right left) (if container (gs:node-xScale container) 1)) width)
                  (setf widthViolation t))
                (when (< (* (- bottom top) (if container (gs:node-yScale container) 1)) height)
                  (setf heightViolation t))))
                  ;; Any problems? Compute the desired physical location and offset from it using the size.
            (when (or widthViolation heightViolation)
              (unless physical
                (sk8-multival-setf (left top right bottom) 
                                   (gs:logical-to-user-rect-coords (gs:node-container me) left top right bottom)))
              (let ((physicalH (gs:f.round (/ (+ right left) 2)))
                    (physicalV (gs:f.round (/ (+ bottom top) 2))))
                (when widthViolation
                  (setf left (- physicalH (gs:f.round (/ width 2))) right (+ left width)))
                (when heightViolation
                  (setf top (- physicalV (gs:f.round (/ height 2))) bottom (+ top height)))
                ;; Return from this function calling setBoundsRect again.
                (return-from setBoundsRect (setBoundsRect me left top right bottom :physical t))
                ))))
        ;; [3] Give the dynamic renderers a chance to modify the bounds.
        (let ((flags (gs:node-flags me)))
          (when (gs:someRenderersAreDynamic? flags)
            ;; The frameColor doesn't get a say in this; it has to conform.
            (when (gs:fillColorIsDynamic? flags) (callRendererSetBoundsRect fillColor))
            (when (gs:textColorIsDynamic? flags) (callRendererSetBoundsRect textColor))))
        )
      ;; [4] Ok: now do it!
      (if (gs:hasWindow? (gs:node-flags me))
        (setf-TLA-boundsRect me (gs:f.round left) (gs:f.round top) (gs:f.round right) (gs:f.round bottom))
        (gs:making-self-dirty (me justMoving t t t (gs:node-boundsRegion me) :bounds nil)
          (when physical
            (SK8-multival-setf (left top right bottom)
                               (gs:stage-to-window-rect-coords me left top right bottom)))
          (set-rectangle-internal me left top right bottom
                                  :physical physical :resizing (not justMoving))))
      (sk8-return-nothing))
    )

(define-handler (setf boundsRect) (rect Actor &key physical relative justMoving)
  (definitely-recover-list rect)
  (sk8-destructuring-bind (left top right bottom) rect
    (setBoundsRect me left top right bottom :physical physical :relative relative :justMoving justMoving)))

;;; Just the part of the function above that builds the rect's cache. For each actor to be cached we record
;;; at what percentage of the enclosing boundsRect it should be. (eg. the left is at 20% of the width).

(defun construct-rect-cache (left top width height subActors)
  (let ((cacheList nil))
    (dolist (subActor subActors cacheList)
      (sk8-multival-bind (ll tt rr bb) (boundsRect subActor :physical t)
        (push (list subActor
                    (/ (- ll left) width)
                    (/ (- tt top) height)
                    ;; The bottom right, if the rect is 0 should be 100%.
                    (if (zerop width) 1 (/ (- rr left) width))
                    (if (zerop height) 1 (/ (- bb top) height))
                    )
              cacheList)))))

;;; Given an item cached, and the new containing boundsrect, returns the item's new rect.

(defun new-scaled-size (left top newWidth newHeight cacheItem)
  (sk8-multivals 
   (truncate (+ left (* (second cacheItem) newWidth)))
   (truncate (+ top (* (third cacheItem) newHeight)))
   (truncate (+ left (* (fourth cacheItem) newWidth)))
   (truncate (+ top (* (fifth cacheItem) newHeight)))))

;;; This returns the minimum boundsrect surrounding a group of actors...
;;; Note that the use of physical is quite incomplete. Will remove it. This stuff
;;; only works in physical coords.

(define-sk8-function actorsBounds nil (actorList &key (physical nil))
  (declare (ignore physical))
  (let ((minx 3000)
        (minY 3000)
        (maxx -3000)
        (maxy -3000))
    ;; find the rects
    (doList (curObj actorList)
      (SK8-multival-bind (rl rt rr rb) 
                         (boundsRect curObj :physical t)
        (setq minx (min rl minX)
              minY (min rt minY)
              maxX (max rr maxX)
              maxY (max rb maxY))))
    ;; return the values
    (if actorList
      (sk8-multiVals minX minY maxX maxY)
      (sk8-multiVals 0 0 0 0))))

;;; LOCATION -- returns the location of the actor. If the actor does not have a node, we
;;;           report a bug to the user. If the physical location is requested, we need to
;;;           translate it from window coordinates (how it is stored) to stage coordinates.

(define-handler location (Actor &key physical)
  (if (gs:hasWindow? (gs:node-flags me))
    (TLA-location me :physical physical)
    (if physical 
      (SK8-multival-bind (x y)
                         (gs:rect-center-coords (gs:recompute-physicalBoundsRect me))
        (gs:window-to-stage-coords me (gs:f.round x) (gs:f.round y)))
      (gs:rect-center-coords (gs:node-logicalBoundsRect me)))))
  
;;; (SETF LOCATION) -- The method an actor uses to change its location. Changing the location is
;;;                 just changing the logical rectangle of the actor. Once this is done, we need to
;;;                 note that the physical rect of the actor is dirty. Furthermore, we need to tell
;;;                 each subactor of this actor that their physical rect is dirty as well.
;;;
;;; This function only changes the location of the logical rect of the actor. The physical rect is
;;; recomputed on demand by the region computing functions.
;;;
;;; When physical is t, the user supplies values in stage coordinates. To make things work efficiently,
;;; we just calculate the difference between these coordinates and old position of self in the stage. Then
;;; we call setf boundsrect in relative mode. 

(define-handler setLocation (Actor x y &key relative physical)
  (let ((flags (gs:node-flags me))
        offsetx offsety)
    ;; Deals with setting the location of an actor which has a window and is not a window.
    (if (gs:hasWindow? flags)
      (setf-TLA-location me x y :relative relative)
      ;; Deals with moving actors within windows.
      (if physical
        ;; changing the physical location.
        (if relative 
          (setBoundsRect me x y x y :physical t :relative t :justMoving t)
          (SK8-multival-bind (oldx oldy) (location me :physical t)
            (setf offsetx (- x oldx)
                  offsety (- y oldy))
            (setBoundsRect me offsetx offsety offsetx offsety
                           :physical t :relative t :justMoving t)))
        ;; changing the logical location.
        (if relative
          (setBoundsRect me x y x y :physical nil :relative t :justMoving t)
          (SK8-multival-bind (oldx oldy) (location me)
            (setf offsetx (gs:f- x oldx)
                  offsety (gs:f- y oldy))
            (setBoundsRect me offsetx offsety offsetx offsety
                           :physical nil :relative t :justMoving t)))))))

(define-handler (setf location) (location Actor &key relative physical)
  (definitely-recover-list location)
  (sk8-destructuring-bind (x y) location
    (setLocation me x y :relative relative :physical physical)))

(define-handler h (actor &key physical)
  (SK8-multival-bind (xx yy) (location me :physical physical)
    (declare (ignore yy))
    xx))

(define-handler (setf h) (x actor &key physical relative)
  (SK8-multival-bind (xx yy) (location me :physical physical)
    (when relative (setf x (+ x xx)))
    (setLocation me x yy :physical physical)))


(define-handler v (actor &key physical)
  (SK8-multival-bind (xx yy) (location me :physical physical)
    (declare (ignore xx))
    yy))

(define-handler (setf v) (y actor &key physical relative)
  (SK8-multival-bind (xx yy) (location me :physical physical)
    (when relative (setf y (+ y yy)))
    (setLocation me xx y :physical physical)))

;;; FRAMEHEIGHT -- returns the height of the frame of the actor.

(define-handler frameHeight (actor)
  (gs:pt-v  (gs:node-frameSize me)))

;;; (SETF FRAMEHEIGHT) -- sets the height of the frame to the value provided.

(define-handler (setf frameHeight) (v actor)
  (SK8-multival-bind (oldh oldv) (framesize me)
    (declare (ignore oldv))
    (setFrameSize me oldh v)))

;;; FRAMEWIDTH -- returns the width of the frame of the actor.

(define-handler frameWidth (actor)
  (gs:pt-h (gs:node-frameSize me)))

;;; (SETF FRAMEWIDTH) -- sets the width of the frame to the value provided.

(define-handler (setf frameWidth) (h actor)
  (SK8-multival-bind (oldh oldv) (framesize me)
    (declare (ignore oldh))
    (setFrameSize me h oldv)))

;;; LEFT -- returns the logical y coordinate of the left of me.

(define-handler left (actor &key physical)
  (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
    (declare (ignore tt rr bb))
    ll))

;;; (SETF LEFT) -- sets the size of actor so that the left of its logical rect is
;;;             left.

(define-handler (setf left) (left actor &key physical relative (resizing t))
  (if resizing
    (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
      (setBoundsRect me (if relative (+ ll left) left) tt rr bb :physical physical))
    ;; Figure out how much we need to move this guy to put the top at the right
    ;; place.
    (if relative
      (setLocation me left 0 :relative t :physical physical)
      (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
        (declare (ignore tt rr bb))
        (setLocation me (- left ll) 0 :relative t :physical physical)))))

;;; TOP -- returns the logical y coordinate of the top of self.

(define-handler top (actor &key physical)
  (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
    (declare (ignore ll rr bb))
    tt))

;;; (SETF TOP) -- sets the size of actor so that the top of its logical rect is
;;;            top.

(define-handler (setf top) (top actor &key physical relative (resizing t))
  (if resizing
    (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
      (setBoundsRect me ll (if relative (+ tt top) top) rr bb :physical physical))
    ;; Figure out how much we need to move this guy to put the top at the right
    ;; place.
    (if relative
      (setLocation me 0 top :relative t :physical physical)
      (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
        (declare (ignore ll rr bb))
        (setLocation me 0 (- top tt) :relative t :physical physical)))))

;;; RIGHT -- returns the logical y coordinate of the right of me.

(define-handler right (actor &key physical)
  (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
    (declare (ignore ll tt bb))
    rr))

;;; (SETF RIGHT) -- sets the size of actor so that the right of its logical rect is
;;;             right.

(define-handler (setf right) (right actor &key physical relative (resizing t))
  (if resizing
    (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
      (setBoundsRect me ll tt (if relative (+ rr right) right) bb :physical physical))
    ;; Figure out how much we need to move this guy to put the top at the right
    ;; place.
    (if relative
      (setLocation me right 0 :relative t :physical physical)
      (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
        (declare (ignore ll tt bb))
        (setLocation me (- right rr) 0 :relative t :physical physical)))))

;;; BOTTOM -- returns the logical y coordinate of the bottom of self.

(define-handler bottom (actor &key physical)
  (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
    (declare (ignore ll tt rr))
    bb))

;;; (SETF BOTTOM) -- sets the size of actor so that the bottom of its logical rect is
;;;               bottom.

(define-handler (setf bottom) (bottom actor &key physical relative (resizing t))
  (if resizing
    (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
      (setBoundsRect me ll tt rr (if relative (+ bb bottom) bottom) :physical physical))
    ;; Figure out how much we need to move this guy to put the top at the right
    ;; place.
    (if relative
      (setLocation me 0 bottom :relative t :physical physical)
      (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
        (declare (ignore ll tt rr))
        (setLocation me 0 (- bottom bb) :relative t :physical physical)))))

;;; SIZE -- return the size of the actor.

(define-handler size (Actor &key physical)
  (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical physical)
    (sk8-multivals (- rr ll) (- bb tt))))

;;; (SETF SIZE) -- Sets the size of me.  We just get the location of the actor and compute the vertices of
;;;            its rect so that the size is (h,v) and the location is unchanged. (setf boundsrect) is
;;;            then called to do the deed.

;;; OJO! If the new sizes are physical and odd the resulting thing will not be of the 
;;; right size.

(define-handler setSize (Actor h v &key physical relative)
  (if relative
    (SK8-multival-bind (oldh oldv) (size me :physical physical)
      (gs:fincf h oldh)
      (gs:fincf v oldv))
    (when (or (minusp h) (minusp v))
      (sk8-error GeneralProgrammaticError 
                 :strings '("Both Width and Height must be positive."))))
  (if physical
    ;; This case ensures the physical size is what the user requested. The location
    ;; might not be preserved!
    (let ((halfH (truncate h 2))
          (halfV (truncate v 2)))
      (sk8-multival-bind (hh vv) (location me :physical t)
        (setBoundsRect me (- hh halfH) (- vv halfV) (+ hh (- h halfH)) (+ vv (- v halfV)) :physical t)))
    (let ((halfH (/ h 2))
          (halfV (/ v 2)))
      (withActorLocked (me)
        (sk8-multival-bind (hh vv) (location me)
          (setBoundsRect me (- hh halfH) (- vv halfV) (+ hh halfH) (+ vv halfV))
          ;; Unfortunately, the line above can change the location. Therefore, 
          ;; in the code below we make sure it is restored. 
          (sk8-multival-bind (newH newV) (location me)
            (unless (and (= newH hh) (= newV vv))
              (setLocation me hh vv))))
        ))))

(define-handler (setf size) (size Actor &key relative physical)
  (definitely-recover-list size)
  (sk8-destructuring-bind (width height) size
    (setSize me width height :relative relative :physical physical)))

;;; WIDTH -- returns the width of self in physical or logical coordinates.

(define-handler width (Actor &key physical)
  (SK8-multival-bind (w h) (size me :physical physical)
    (declare (ignore h))
    w))

;;; (SETF WIDTH) -- sets the width of self to width. The user has the option of requiring a physical or
;;;              a logical width change. The actor changes size without changing its location.

(define-handler (setf width) (width actor &key physical)
  (setSize me width (height me :physical physical) :physical physical))

;;; HEIGHT -- returns the height of self in physical or logical coordinates.

(define-handler height (Actor &key physical)
  (SK8-multival-bind (w h) (size me :physical physical)
    (declare (ignore w))
    h))

;;; (SETF HEIGHT) -- sets the height of self to height. The user has the option of requiring a physical or
;;;              a logical height change. The actor changes size without changing its location.

(define-handler (setf height) (height actor &key physical)
  (setSize me (width me :physical physical) height :physical physical))

;;; FRAMESIZE -- returns the framesize of self in logical coordinates.

(define-handler framesize (Actor &key physical)
  (let ((framesize (gs:node-frameSize me)))
    (if physical
      (let ((container (gs:node-container me)))
        (if container
          (sk8-multivals (gs:f.round (* (gs:pt-h framesize) (gs:node-xScale container)))
                         (gs:f.round (* (gs:pt-v framesize) (gs:node-yScale container))))
          (sk8-multivals (gs:f.round (gs:pt-h framesize)) (gs:f.round (gs:pt-v framesize)))))
      (SK8-multivals (gs:pt-h framesize) (gs:pt-v framesize)))))

;;; (SETF FRAMESIZE) -- sets the frame size of me to h and v. The frame and the fill become dirty. We optimize
;;;                  by only making dirty the region which is the union of the old framesize and the
;;;                  new framesize.

(define-handler setFrameSize (Actor h v &key physical)
  (let ((container (gs:node-container me)))
    (gs:making-self-dirty (me nil nil t t (gs:node-frameRegion me) :frame t)
      (if container
        (if physical
          (gs:set-point (gs:node-frameSize me) 
                     (* h (gs:node-xScale container)) 
                     (* v (gs:node-yScale container)))
          (gs:set-point (gs:node-frameSize me) h v))
        (gs:set-point (gs:node-frameSize me) h v))
      (let ((flags (gs:node-flags me)))
        (when (gs:someRenderersAreDynamic? flags)
          (when ( gs:fillColorIsDynamic? flags) (setFrameSize ( fillColor me) h v :physical physical))
          (when ( gs:textColorIsDynamic? flags) (setFrameSize ( textColor me) h v :physical physical))
          (when (gs:frameColorIsDynamic? flags) (setFrameSize (frameColor me) h v :physical physical)))))
    (sk8-multivals h v)))

(define-handler (setf frameSize) (size Actor &key physical)
  (definitely-recover-list size)
  (sk8-destructuring-bind (width height) size
    (setFrameSize me width height :physical physical)))

;;; MOUSELOC -- returns the coordinates of the point the mouse currently points to in the
;;;           logical coordinate system of me.

(define-handler mouseLoc (actor)
  (let ((clos-window (gs:node-window me)))
    (if clos-window
      (let* ((mouse-pos (view-mouse-position clos-window))
             (mouseX (point-h mouse-pos))
             (mouseY (point-v mouse-pos))
             (physRect (gs:recompute-physicalBoundsRect me)))
        (SK8-multivals (+ (gs:node-Xorigin me) 
                          (/ (- mouseX (gs:rect-left physRect)) (gs:node-Xscale me)))
                       (+ (gs:node-Yorigin me) 
                          (/ (- mouseY (gs:rect-top physRect)) (gs:node-Yscale me)))))
      (sk8-error GeneralProgrammaticError
                 :strings '("" " is not on the stage.")
                 :objects (list me)))))

;;; MOUSEH -- returns the x coordinate of the point the mouse currently points to in the
;;;          logical coordinate system of self.

(define-handler hMouse (actor)
  (SK8-multival-bind (x y) (mouseLoc me)
    (declare (ignore y))
    x))

;;; MOUSEV -- returns the y coordinate of the point the mouse currently points to in the
;;;          logical coordinate system of self.

(define-handler vMouse (actor)
  (SK8-multival-bind (x y) (mouseLoc me)
    (declare (ignore x))
    y))

;;; OVERLAPS -- returns t of the two actors overlap. Two actors overlap if they are in the same container
;;;           and their bounds regions overlap.

(define-handler overlaps (actor otherActor)
  (let* ((actorContainer (gs:node-container me))
         (otherContainer (gs:node-container otherActor)))
    (when (eq actorContainer otherContainer)
      (if actorContainer
        (progn (gs:recompute-bounds-region me (gs:node-flags me))
               (gs:recompute-bounds-region otherActor (gs:node-flags otherActor))
               (regions-overlap-p (gs:node-boundsRegion me) 
                                  (gs:node-boundsRegion otherActor)))
        ;; No container. Are they windows?
        (if (and (gs:hasWindow? (gs:node-flags me))
                 (gs:hasWindow? (gs:node-flags otherActor)))
          ;; both are windows, check whether they intersect.
          (let (r1 r2)
            (SK8-multival-bind (ll tt rr bb)
                               (TLA-boundsRect me)
              (setf r1 (gs:make-rect :left ll :top tt :right rr :bottom bb)))
            (SK8-multival-bind (ll tt rr bb)
                               (TLA-boundsRect otherActor)
              (setf r2 (gs:make-rect :left ll :top tt :right rr :bottom bb)))
            (gs:rects-overlap-p r1 r2))
          ;; If they are not windows and are not contained by anything, the
          ;; overlap test makes no sense and we return nil.
          nil)))))

;;; Returns t if theActor's is not visible because its siblings are covering it. Note that translucent siblings
;;; are not considered to cover theActor.

(defun covered-by-siblings? (theActor)
  (gs:let+ ((container (gs:node-container theActor))
            (temp-region (:region)))
    (when container
      (gs:recompute-bounds-region theActor (gs:node-flags theActor))
      (#_copyRgn (gs:node-boundsregion theActor) temp-region)
      ;; traverse list from front to back. If region is empty, return t. Otherwise
      ;; return nil when we find the actor.
      (mf::dovector-in-reverse (c (gs:node-contents container))
        (when (eq c theActor)
          (return nil))
        (unless (translucent (gs:node-fillcolor c))
          (gs:recompute-bounds-region c (gs:node-flags c))
          (#_diffRgn temp-region (gs:node-boundsregion c) temp-region)
          (when (#_emptyRgn temp-region)
            (return t)))))))

(defmacro within-boundsRect? (theActor theContainer)
  `(sk8-multival-bind (ll tt rr bb) (boundsRect ,theContainer :physical t)
     (sk8-multival-bind (innerLl innerTt innerRr innerBb) (boundsRect ,theActor :physical t)
       (and (or (and (>= innerLL ll) (<= innerLl rr))
                (and (>= innerRR ll) (<= innerRR rr)))
            (or (and (>= innerTt tt) (<= innerTt Bb))
                (and (>= innerBB tt) (<= innerBb bb)))))))

;;; SHOWN -- returns true if an actor is currently VISIBLE on the stage. This means it is on the stage, it is
;;;         visible (in the property's sense) and it is not obstructed from view by other actors. Note that
;;;         The actor is considered shown if any part of its boundsRect is shown.

(define-handler shown (actor)
  (let ((clos-window (gs:node-window me)))
    (when (and (visible me) clos-window)
      ;; Is the window within the boundsRect of the stage?
      (unless (within-boundsRect? (sk8::window me) stage)
        (return-from shown nil))
      ;; Yes! Is the actor the window itself?
      (if (gs:hasWindow? (gs:node-flags me))
        t
        ;; The heavy thing.
        (and (within-boundsRect? me (container me))
             (not (covered-by-siblings? me)))))))

;;; --------------
;;; PIXMAP CACHING 
;;; --------------

;;; cachesPixmap -- returns whether the actor caches its pixmap.

(define-handler cachesPixmap (actor)
  (or 
   ;; Maybe I cache the pixmap directly.
   (gs:cachesPIxmap? (gs:node-flags me))
   ;; Mmmm. Is there a window gWorld?
   (and (gs:hasWindow? (gs:node-flags me))
        (colorDepth me)
        t)
   ))

;;; (SETF cachesPixmap) -- makes the actor cache or uncache its pixmap according to the
;;;                    value of boolean.

(define-handler (setf cachesPixmap) (boolean actor)
  (if boolean
    (unless (cachesPixmap me)
      (when (or (translucent (fillcolor me)) (translucent (framecolor me)))
        (sk8-error GeneralProgrammaticError
                   :strings '("You cannot cache the pixmap of an actor rendered with a translucent color!")))
      ;; If we are a window, give ourselves a gWorld.
      (if (gs:hasWindow? (gs:node-flags me))
        (setf (colorDepth me) 0)
        (gs:cache-pixmap me)))
    (when (cachesPixmap me)
      (if (gs:hasWindow? (gs:node-flags me))
        (setf (colorDepth me) nil)
        (gs:uncache-pixmap me))))
  boolean)

;;; ------
;;; COLORS
;;; -------

(define-handler attachDynamicRenderer :private (Actor whichColor)
  (let ((flags (gs:node-flags me)))
    (gs:someRenderersAreDynamic! flags)
    (case whichColor
      (:fill  ( gs:fillColorIsDynamic! flags))
      (:text  ( gs:textColorIsDynamic! flags))
      (:frame (gs:frameColorIsDynamic! flags))))
  (incrementOwnsRegionCount me)
  (sk8-return-nothing))

(define-handler detachDynamicRenderer :private (Actor whichColor)
  (let ((flags (gs:node-flags me)))
    (case whichColor
      (:fill  ( gs:fillColorIsDynamic! flags 0))
      (:text  ( gs:textColorIsDynamic! flags 0))
      (:frame (gs:frameColorIsDynamic! flags 0)))
    (unless (or ( gs:fillColorIsDynamic? flags)
                ( gs:textColorIsDynamic? flags)
                (gs:frameColorIsDynamic? flags))
      (gs:someRenderersAreDynamic! flags 0)))
  (decrementOwnsRegionCount me)
  (sk8-return-nothing))

;;; 1.0
;;; FILLCOLOR -- returns the fill color of self.

(define-handler fillcolor (Actor)
  (gs:node-fillcolor me))

;;; 1.0
;;; (SETF FILLCOLOR) -- sets the fillcolor of self to color. Note that in the process it makes the node
;;;                 dirty. The color must be the color object (a frame).

(define-handler (setf fillcolor) (color Actor)
  (let ((flags (gs:node-flags me)))
    (when (and (gs:hasWindow? flags) (translucent color))
      (sk8-error GeneralProgrammaticError 
                 :strings '("Cannot render a window with a translucent color.")))
    (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
      (let* ((oldColor (fillColor me))
             (realColor (if (eq color oldColor)
                          color
                          (progn
                            (when (gs:fillColorIsDynamic? flags) ;this test is an optimization; most will not be dynamic
                              (setFillColorActor oldColor nil)) ;disconnect from old color
                            (setFillColorActor color me))))) ;connect to new color
        ;; first, update the translucency information
        (if (translucent realColor)
          ;; Opaque to translucent: update flags and container drawing function.
          (when (gs:opaque? flags)
            (gs:opaque! flags 0)
            (let ((container (gs:node-container me)))
              (when container 
                (unless (gs:translucentContents? container)
                  (setf (gs:node-contentsDrawFunction container) 
                        'gs:sk8-draw-contents-with-translucency))
                (gs:add-to-translucent-contents container))))
          ;; Translucent to opaque: update flags and container drawing function.
          (when (and (not (gs:opaque? flags))
                     (not (translucent (framecolor me))))
            (gs:opaque! flags)
            (let ((container (gs:node-container me)))
              (when container 
                (gs:remove-from-translucent-contents container)
                (unless (gs:translucentContents? container)
                  (setf (gs:node-contentsDrawFunction container) 
                        'gs:sk8-draw-contents))))))
        ;; actually install the color
        (setf (gs:node-fillColor me) realColor)
        (when (and (gs:fillColorIsDynamic? flags)
                   (gs:node-window me))
          (enteringStage realColor)) ; this may set the boundsRect of the actor
        realColor))))

;;; 1.0
;;; FRAMECOLOR -- returns the frame color of me.

(define-handler framecolor (Actor)
  (gs:node-framecolor me))

;;; 1.0
;;; (SETF FRAMECOLOR) -- sets the color of the frame of self to color.

(define-handler (setf frameColor) (color Actor)
  (let ((flags (gs:node-flags me)))
    (when (and (gs:hasWindow? flags) (translucent color))
      (sk8-error GeneralProgrammaticError
                 :strings '("Cannot render a window with a translucent color.")))
    (gs:making-self-dirty  (me nil nil nil nil (gs:node-frameRegion me) :frame t)
      (let* ((oldColor (frameColor me))
             (realColor (if (eq color oldColor)
                          color
                          (progn
                            (when (gs:frameColorIsDynamic? flags) ;this test is an optimization; most will not be dynamic
                              (setFrameColorActor oldColor nil)) ;disconnect from old color
                            (setFrameColorActor color me))))) ;connect to new color
        ;; first, update the translucency information
        (if (translucent realColor)
          ;; Opaque to translucent: update flags and container drawing function.
          (when (gs:opaque? flags)
            (gs:opaque! flags 0)
            (let ((container (gs:node-container me)))
              (when container 
                (unless (gs:translucentContents? container)
                  (setf (gs:node-contentsDrawFunction container) 
                        'gs:sk8-draw-contents-with-translucency))
                (gs:add-to-translucent-contents container))))
          ;; Translucent to opaque: update flags and container drawing function.
          (when (and (not (gs:opaque? flags))
                     (not (translucent (fillcolor me))))
            (gs:opaque! flags)
            (let ((container (gs:node-container me)))
              (when container 
                (gs:remove-from-translucent-contents container)
                (unless (gs:translucentContents? container)
                  (setf (gs:node-contentsDrawFunction container) 
                        'gs:sk8-draw-contents-with-translucency))))))
        ;; actually install the color
        (setf (gs:node-framecolor me) realColor)
        (when (and (gs:frameColorIsDynamic? flags)
                   (gs:node-window me))
          (enteringStage realColor)) ; this may set the boundsRect of the actor
        realColor))))

;;; ----------------------------------
;;; VISIBILITY, SELECTION AND LAYERING
;;; ----------------------------------

;;; 1.0
;;; VISIBLE -- returns whether self is visible. Note that visible means "not hidden by the user". Thus, an actor
;;;         that is obstructed by others is still visible.

(define-handler visible (Actor)
  (gs:visible? (gs:node-flags me)))


;;; DEEPLYVISIBLE -- returns whether self and all its containers up its containment hierarchy are visible. Note that
;;;               visible means "not hidden by the user". Thus, an actor that is obstructed by others is still visible.

(define-handler deeplyVisible (Actor)
  (gs:deeply-visible? me))

;; *** Should eventually write a setter for deeplyVisible

(defun doReferSetVisibleToRenderer (newValue me flags)
  (when ( gs:fillColorIsDynamic? flags) (setf (visible ( fillColor me)) newValue))
  (when ( gs:textColorIsDynamic? flags) (setf (visible ( textColor me)) newValue))
  (when (gs:frameColorIsDynamic? flags) (setf (visible (frameColor me)) newValue)))

(defmacro referSetVisibleToRenderer (me flags newValue)
  `(when (gs:someRenderersAreDynamic? ,flags)
     (doReferSetVisibleToRenderer ,newValue ,me ,flags)))

;;; (SETF VISIBLE) -- makes the actor visible. If it is a window, we open it. (Note that it is
;;;               necessary to bracket this call by setting the visible flag to nil because
;;;               otherwise sk8-open-window does not do the work). If we have an actor,
;;;               we set its visible flag and make it dirty so that the window redraws it.

(define-handler (setf visible) (visible? Actor)
  (let ((flags (gs:node-flags me)))
    (if (gs:hasWindow? flags)
      ;; It has a window: use window functions.
      (if visible?
        (let ((clos-window (gs:node-window me)))
          (gs:visible! flags 1)
          (gs:sk8-bring-up-new-window clos-window t)
          (referSetVisibleToRenderer me flags t))
        (progn (gs:visible! flags 0)
               (referSetVisibleToRenderer me flags nil)
               (window-hide (gs:node-window me))))
      ;; It is not the top level actor...
      (let ((clos-window (gs:node-window me)))
        (if visible?
          ;; User wants to show. If necessary, we tell the obejct it has become visible
          ;; and add it to the dirty list so that the window will redraw it.
          (unless (gs:visible? flags)
            (gs:visible! flags 1)
            (when clos-window
              (gs:withLockedWindow clos-window
                (gs:make-new-node-dirty me clos-window :showingOnly t)
                (referSetVisibleToRenderer me flags t)
                ;; We show all the connectors!
                (dolist (c (gs:node-lines me))
                  (if (eq me (startActor c))
                    (when (visible (endActor c))
                      (show c))
                    (when (visible (startActor c))
                      (show c))))
                )))
          ;; User wants to hide. If necessary, tell the object it is not visible and
          ;; add its region to the dirty region of the window for erasing.
          (when (gs:visible? flags)
            (gs:visible! flags 0)
            (when clos-window
              (gs:withLockedWindow clos-window
                (referSetVisibleToRenderer me flags nil)
                (gs:remove-dirty-from-window me clos-window)
                ;; Hide the connectors!
                (dolist (c (gs:node-lines me))
                  (hide c))))))))))

;;; 1.0
;;; SHOW -- makes the actor visible by calling setf visible.

(define-handler show (Actor)
  (setf (visible me) t))

;;; 1.0
;;; HIDE -- hides the actor by calling setf visible.

(define-handler hide (Actor)
  (setf (visible me) nil))

;;; 1.0
;;; SENDTOBACK -- sends actor to back of front-to-back list of its container's contents. In terms of the
;;;             contents vector, self is sent to the front.

(define-handler sendToBack (Actor)
  (let ((container (gs:node-container me)))
    (cond (container
           (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds nil)
             (gs:vector-move-item-before (gs:node-contents container) me 0)))
          ((gs:hasWindow? (gs:node-flags me))
           (let ((clos-window (gs:node-window me))
                 (theLayer 9999)
                 (theBackground (gs:findBackground)))
             ;; Making sure the window stays on top of a covered Stage.
             (when theBackground
               (setf theLayer (1- (window-layer theBackGround))))
             (when (typep clos-window 'gs:*sk8-windoid*)
               (setf theLayer (1- ccl::*windoid-count*)))
             (if (eq clos-window gs:*currentTla*)
               (progn (set-window-layer (gs:node-window me) theLayer)
                      (window-select (front-window)))
               (set-window-layer (gs:node-window me) theLayer))))
          (t (sk8-error GeneralProgrammaticError
                        :strings '("" " doesn't have a container.")
                        :objects (list me))))
    t))

;;; 1.0
;;; BRINGTOFRONT -- brings actor to front of front-to-back list of its container's contents. This means
;;;               making self the last item in the contents list.

(define-handler bringToFront (Actor)
  (let ((container (gs:node-container me)))
    (cond (container
           (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds t)
             (gs:vector-move-item-before (gs:node-contents container) me :end)))
          ((gs:hasWindow? (gs:node-flags me))
           (let ((clos-window (gs:node-window me)))
             (unless (eq clos-window gs:*currentTla*)
               (if (visible me)
                 (gs:selecting-clos-window
                   (gs:sk8-select-old-window clos-window nil))
                 (set-window-layer clos-window 0)))
             ))
          (t (sk8-error GeneralProgrammaticError
                        :strings '("" " doesn't have a container.")
                        :objects (list me))))
    t))

;;; 1.0
;;; BRINGCLOSER -- brings actor closer to the front of front-to-back list of its container's contents. The actor's
;;;              node moves towards the end of the contents vector.

(define-handler bringCloser (Actor)
  (let ((container (gs:node-container me)))
    (cond (container
           (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds t)
             (gs:vector-move-item-forward (gs:node-contents container) me)))
          ((gs:hasWindow? (gs:node-flags me))
           (let ((clos-window (gs:node-window me)))
             (unless (eq clos-window (front-window))
               (set-window-layer clos-window (1- (window-layer clos-window)))
               (when (eq clos-window (front-window))
                 (window-select clos-window)))))
          (t (sk8-error GeneralProgrammaticError
                        :strings '("" " doesn't have a container.")
                        :objects (list me))))
    t))

;;; 1.0
;;; SENDFARTHER -- sends self one more step towards the bottom of its container. The actor's node
;;;              moves one cell towards the head of the vector.

(define-handler sendFarther (Actor)
  (let ((container (gs:node-container me)))
    (cond (container
           (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds nil)
             (gs:vector-move-item-backward (gs:node-contents container) me)))
          ((gs:hasWindow? (gs:node-flags me))
           (let* ((clos-window (gs:node-window me))
                  (theLayer (1+ (window-layer clos-window))))
             ;; Make sure the new layer does not take the window behind the Stage.
             (when (and (gs:findbackground) (>= theLayer (window-layer (gs:findbackground))))
               (setf theLayer (1- (window-layer (gs:findbackground)))))
             (unless (eq clos-window (car (last (windows))))
               (when (typep clos-window 'gs:*sk8-windoid*)
                 (setf theLayer (min (1- ccl::*windoid-count*) theLayer)))
               (set-window-layer clos-window theLayer)
               (when (eq clos-window gs:*currentTla*)
                 (window-select (front-window))))))
          (t (sk8-error GeneralProgrammaticError
                        :strings '("" " doesn't have a container.")
                        :objects (list me))))
    t))

;;; 1.0
;;; CURRENT-KEY -- if the actor is a window, it returns its subactor which is currently getting
;;;              key events. If it is not a window, nothing happens.

(define-handler keyTarget (Actor)
  (getf (gs:node-properties me) :curKey nil))

;;; 1.0
;;; (SETF KEYTARGET) -- sets the current key of self to object, when self is a window. Nothing
;;;                   happens otherwise.
;;;
;;; Note: we modify this method to make only the field which is the currentKey get idle events.
;;;      Thus only one cursor flashes on the screen as wanted. This should make things faster as well.

(define-handler (setf keyTarget) (obj Actor)
  (let* ((props (gs:node-properties me))
         (oldKey (getf props :curKey nil))
         (activeWindow? (or (eq (gs:node-window me) gs:*currentTla*)
                            (eq (gs:node-window me) (gs:computed-active-window)))))
    ;; If the actor is a window and the window is the active window, do the keytarget change.
    (if (gs:hasWindow? (gs:node-flags me))
      ;; do nothing if object is already the current key.
      (unless (eq obj oldKey)
        (setf (getf props :curKey) obj)
        (if obj
          ;; setting a curkey
          (progn (when oldKey 
                   (when activeWindow? (deactivateText oldKey))
                   ;; removing old curKey from wantsIdle list.
                   (when (SK8::is-a oldKey gs:*EditText-the-object*)
                     (setf (wantsIdle oldKey) nil)))
                 (when activeWindow? (activateText obj))
                 ;; Adding new guy to the wantIdle list.
                 (when (SK8::is-a obj gs:*EditText-the-object*)
                   (setf (wantsIdle obj) t)))
          ;; clearing a curkey
          (progn (when activeWindow? (deactivateText oldKey))
                 (when (SK8::is-a oldKey gs:*EditText-the-object*)
                   (setf (wantsIdle oldKey) nil)))))
      ;; Not a window? Just replace stuff in the curkey.
      (setf (getf props :curKey) obj))
    (setf (gs:node-properties me) props)
    obj))

(define-handler autotab (actor)
  (gs:autotab? (gs:node-flags me)))

(define-handler (setf autotab) (boolean actor)
  (if boolean
    (gs:autotab! (gs:node-flags me) 1)
    (gs:autotab! (gs:node-flags me) 0))
  boolean)

;;; Run through containment hierarchy from front to back looking for the next autotab guy.

(define-handler tabToNextActor (actor &key (direction 'forward))
  (when (gs:node-container me)
    (let ((theContainer (gs:node-container me))
          searchMode? nextGuy)
      ;; The search function:
      (block nil
        (let ((searchFun #'(lambda (anActor) 
                             (if searchMode?
                               (if (eq anActor me)
                                 (return)
                                 (when (and (autotab anActor) (visible anActor)
                                            (not (and (is-a anactor edittext) (lockedText anActor))))
                                   (setf nextGuy anActor)
                                   (return)))
                               (when (eq anActor me)
                                 (setf searchMode? t))))))
          ;; Doing the work. Why twice? Well... The first time we search from the current actor
          ;; to the end of the vector. The second time we go from the start to the actor. This is
          ;; how wrapping is done.
          (if (eq direction 'forward)
            (dotimes (i 2)
              (gs:docontents (anActor theContainer)
                (funcall searchFun anActor)))
            (dotimes (i 2)
              (gs:doContents-btof (anActor theContainer)
                (funcall searchFun anActor))))))
      ;; If something was found, move to it!
      (when nextGuy
        (setf (keyTarget (sk8::window me)) nextGuy)))))

;;; 1.0
;;; NUMBER -- returns the number of the actor within its container. 1 is returned when the actor is at the
;;;          front of its container. 

(define-handler layer (Actor)
  (let ((container (gs:node-container me)))
    (cond (container
           (- (length (gs:node-contents container)) 
              (position me (gs:node-contents container))))
          ((gs:hasWindow? (gs:node-flags me))
           (1+ (position me (contents Stage))))
          (t 1))))

;;; 1.0
;;; (SETF LAYER) -- sets the position of the actor in its container to layer. Since the layer is the position
;;;               of the actor in the REVERSED contents, we need to convert the user's layer to the
;;;               internal equivalent.

(define-handler (setf layer) (num Actor)
  (unless (inheritsFrom num PositiveInteger)
    (sk8-error PropertyTypeMismatchError
               :object        num
               :expectedType  PositiveInteger
               :ownerObject   Actor
               :propertyName 'layer
               ))
  ;; Return the original number!
  (prog1 
    num
    (decf num 1) ;; Subtracting 1 for internal processing.
    (let ((container (gs:node-container me)))
      (cond (container
             (let ((contents (gs:node-contents container)))
               (setf num (- (1- (length contents)) num))
               (unless (<= 0 num (length contents))
                 (sk8-error GeneralProgrammaticError 
                            :strings '("The layer must be between 1 and " ".")
                            :objects (list (length contents))))
               (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds nil)
                 (gs:splice me contents num))))
            ((gs:hasWindow? (gs:node-flags me))
             (let ((clos-window (gs:node-window me)))
               ;; Make sure the new layer does not take the window behind the Stage.
               (when (and (gs:findbackground) (>= num (window-layer (gs:findbackground))))
                 (setf num (1- (window-layer (gs:findbackground)))))
               (set-window-layer clos-window num)
               (when (eq clos-window (front-window))
                 (window-select clos-window))))
            (t (sk8-error GeneralProgrammaticError 
                          :strings '("" " doesn't have a container.")
                          :objects (list me)))))))

;;; ---------------------
;;; ZOOMING AND PANNING
;;; ---------------------

;;; 1.0
;;; ORIGIN -- returns the origin of me (the point in the contents plane of self that is located at the
;;;        top left corner of the actor).

(define-handler origin (actor)
  (SK8-multivals (gs:node-xOrigin me)
                 (gs:node-yOrigin me)))

;;; 1.0
;;; OFFSET-RECTS-AND-DIRTY-THE-REST -- this function is used by the clever scroll method to move all actors
;;;                                scrolled by offseting their rects. At the end we let the actors know
;;;                                their regions are dirty.
;;;
;;; If an owned region is present, we offset its clip and fill regions so that everything can draw properly.

(defun offset-rects-and-dirty-the-rest (theactor xOffset yOffset)
  (let ((flags nil))
    (gs:map-subnodes* #'(lambda (subActor) 
                          (setf flags (gs:node-flags subActor))
                          (gs:offset-rect (gs:node-physicalBoundsRect subActor) xOffset yOffset)
                          (gs:boundsDirty! flags)
                          (gs:fillDirty! flags)
                          (gs:frameDirty! flags)
                          ;; Must update the ownedRegion...
                          (when (gs:ownedRegion? flags)
                            ;; Offset the clip.
                            (#_offsetRgn (ownedRegion subActor) xOffset yOffset)
                            ;; Offset the fill.)
                            (#_offsetRgn (gs:node-fillRegion subActor) xOffset yOffset)))
                      theActor)))

;;; 1.0
;;; (SETF ORIGIN) -- This is the main function for scrolling an actor. We just set the origin of the actor
;;;              to the requested values and invalidate the regions of all the subactors of self.
;;;              We make this node dirty to redraw all its subactors.  Note that this actor really
;;;              does not need to be redrawn, but we think it is faster to make it dirty than to
;;;              make all its immediate subactors dirty.
;;;
;;; We will optimize this as follows: if after reseting the origin, some of the stuff displayed on the
;;; window is still shown, we just copy it from the gworld. Then we just make dirty the new region
;;; that was exposed. This  substantially reduces the amount of actors that have to be
;;; redrawn.

(define-handler setOrigin (actor xOrigin yOrigin &key (forceRedraw t))
  (let* ((clos-window (gs:node-window me))
         (oldX (gs:node-xOrigin me))
         (oldY (gs:node-yOrigin me)))
    ;; Change the origin.
    (setf (gs:node-xOrigin me) (gs:int-to-fixed xOrigin))
    (setf (gs:node-yOrigin me) (gs:int-to-fixed yOrigin))
    (when clos-window 
      (if (and (slot-value clos-window 'gs:depth) (not forceRedraw)
               (null (slot-value clos-window 'gs:locked)))
        ;; Since there is a gWorld, we can be clever.
        (let* ((gWorld (slot-value clos-window 'gs:gWorld))
               (gWorld-pixmap (#_GetGWorldPixMap gWorld))
               (window-dirty-region (slot-value clos-window 'gs:dirty-region))
               frameH frameV)
          ;; Make sure we have the pixmap.
          (unless (handlep gworld-pixmap) 
            (sk8-error GeneralProgrammaticError :strings '("GWorld has no pixmap!!!")))
          (sk8-multival-setf (frameH frameV) (framesize me :physical t))
          (gs:let+ ((oldBoundsRect (gs:inset-rect (gs:copy-rect (gs:recompute-physicalBoundsRect me))
                                                  (gs:f.round frameh) (gs:f.round framev)))
                    ;; find how much we are really scrolling by.
                    (xOffset (gs:f.round (* (gs:node-xScale me) (- xOrigin oldX))))
                    (yOffset (gs:f.round (* (gs:node-yScale me) (- yOrigin oldY))))
                    ;; newBoundsRect => the new region of the contents of self to be shown.
                    ;; cleanRect => the rect of the current gWorld that is preserved after scrolling.
                    ;; dirtyRegion => the region we need to redraw.
                    (newBoundsRect (gs:offset-Rect (gs:copy-rect oldBoundsRect) xOffset yOffset))
                    (clean-rect (gs:intersect-rects oldBoundsRect newBoundsRect (gs:make-rect)))
                    (dirty-region (:region))
                    (qd-clean-rect (:rect :sk8rect clean-rect))
                    (dest-rect (gs:offset-rect (gs:copy-rect clean-rect) (- xOffset) (- yOffset)))
                    (qd-dest-rect (:rect :sk8rect dest-rect))
                    (fill (gs:node-fillRegion me))
                    (tempRgn (:region))
                    (theMask (:region)))
              ;; calculate the mask to use for copy bits.
              ;; First we intersect the destination rectangle with the fill region and out it in
              ;; the mask.
              (#_rectRgn tempRgn qd-dest-rect)
              (#_sectRgn tempRgn fill themask)
              ;; Then we intersect the source rect with the fill and offset it to make the bits be
              ;; where they would go after the copy. After this we intersect with the partial mask
              ;; we just calculated.
              (#_rectRgn tempRgn qd-clean-rect)
              (#_sectRgn tempRgn fill tempRgn)
              (#_offsetRgn tempRgn (- xOffset) (- yOffset))
              (#_sectRgn tempRgn themask themask)
              ;; calculate the new dirty region.
              (#_diffRgn fill themask dirty-region)
              (#_unionRgn window-dirty-region dirty-region window-dirty-region)
              ;; update the physical rects of all actors moved.
              (offset-rects-and-dirty-the-rest me (- xOffset) (- yOffset))
              (without-interrupts ;; CUZ MCL likes to take your port away...
               ;; copy the preserved region to the gWorld.
               (with-port gworld
                 (with-locked-pixels-force (gworld-pixmap)
                   (#_SetGWorld gworld (%null-ptr))
                   (#_PenNormal)
                   (#_ForeColor #$blackColor)
                   (#_BackColor #$whiteColor)
                   (gs:copy-bits gWorld-pixmap gWorld-pixmap qd-clean-rect qd-dest-rect 8 themask)))
               ;; Draw the dirty data.
               (let ((gs:*fast-draw* nil))
                 (declare (special gs:*fast-draw*))
                 (gs:sk8-draw-data clos-window gWorld window-dirty-region))
               (with-port (gs:get-wmgr-port)
                 ;; blit by hand.
                 (gs:gWorld-to-window gWorld clos-window nil (rref (wptr clos-window) :cGrafPort.visRgn)))
               (#_SetEmptyRgn window-dirty-region))))
          ;; no gWorld: just redraw everything in self.
          (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill nil)
            ;; Invalidate the regions of all subnodes.
            (gs:dirty-subnodes* me))))))

(define-handler (setf origin) (origin Actor &key forceRedraw)
  (definitely-recover-list origin)
  (sk8-destructuring-bind (x y) origin
    (setOrigin me x y :forceRedraw forceRedraw)))

(define-handler hOrigin (actor)
  (sk8-multival-bind (h v) (origin me)
    (declare (ignore v))
    h))

(define-handler (setf hOrigin) (h actor)
  (sk8-multival-bind (hh vv) (origin me)
    (declare (ignore hh))
    (setOrigin me h vv)))

(define-handler vOrigin (actor)
  (sk8-multival-bind (h v) (origin me)
    (declare (ignore h))
    v))

(define-handler (setf vOrigin) (v actor)
  (sk8-multival-bind (hh vv) (origin me)
    (declare (ignore vv))
    (setOrigin me hh v)))

;;; 1.0
;;; SCROLL -- scrolls the actor's contents by x and y.

(define-handler scroll (actor x y &key forceRedraw)
  (setOrigin me (+ x (gs:node-xOrigin me)) (+ y (gs:node-yOrigin me))
             :forceRedraw forceRedraw))

;;; 1.0
;;; SCALE -- returns the x and y scale self uses to show its contents.

(define-handler scale (actor)
  (SK8-multivals (gs:node-logicalXScale me)
                 (gs:node-logicalYScale me)))

(define-handler physicalScale (actor)
  (SK8-multivals (gs:node-XScale me)
                 (gs:node-YScale me)))

;;; When this is called we are ready to change the scale.

(defun update-trees-scale (theActor baseXScale baseYScale)
  (setf baseXScale (* baseXScale (gs:node-logicalXScale theActor))
        baseYScale (* baseYScale (gs:node-logicalYScale theActor)))
  ;; Setting the actor's slots.
  (setf (gs:node-xScale theActor) baseXScale)
  (setf (gs:node-yScale theActor) baseYScale)
  ;; Calling rescaled.
  (rescaled theActor)
  (dovector (c (gs:node-contents theActor))
    (update-trees-scale c baseXScale baseYScale)))

(defun update-actors-scale (theActor)
  (let ((newContainer (gs:node-container theActor))
        (baseXScale 1)
        (baseYScale 1))
    (when newContainer
      (setf baseXScale (gs:node-xScale newContainer)
            baseYScale (gs:node-yScale newContainer)))
    ;; Now run through the tree updating the scales and calling rescaled.
    (update-trees-scale theActor baseXScale baseYScale)))
    
;;; 1.0
;;; (SETF SCALE) -- sets the scale of the actor. Each node stores its physical and its logical scale.
;;;              An explicit setf scale only affects the logical. The physical has to be 
;;;              recomputed for this actor and all its subctors.
;;;
;;; Note that the scale cannot be set to anything below 0.1. This is arbitrary but it is
;;; probably as small as we can get without getting an overflow.

(define-handler setScale (actor xScale yScale)
  (unless (or (< xScale .05) (< yScale .05))
    (let* ((oldXScale (gs:node-logicalXScale me))
           (oldYScale (gs:node-logicalYScale me)))
      (unless (and (= xScale oldXscale) (= yScale oldYScale))
        ;; Updating the logical scale of self.
        (setf (gs:node-logicalXScale me) xScale)
        (setf (gs:node-logicalYScale me) yScale)
        (gs:making-self-dirty (me nil t t t (gs:node-fillRegion me) :fill t)
          ;; Invalidate the regions of all subnodes.
          (gs:dirty-subnodes* me)
          ;; now we update the physical scales in all the subactors of self.
          (update-actors-scale me))))))

;;; Lets the renderers know that the scale has changed.

(define-handler rescaled (actor)
  (let ((flags (gs:node-flags me)))
    (when (gs:someRenderersAreDynamic? flags)
      (when (gs:fillColorIsDynamic? flags) (rescaled (fillColor me)))
      (when (gs:textColorIsDynamic? flags) (rescaled (textColor me)))
      (when (gs:frameColorIsDynamic? flags) (rescaled (frameColor me))))
    ))

(define-handler (setf scale) (scale Actor)
  (definitely-recover-list scale)
  (sk8-destructuring-bind (x y) scale
    (setScale me x y)))

(define-handler hScale (actor)
  (sk8-multival-bind (h v) (scale me)
    (declare (ignore v))
    h))

;;; (setf hScale) -- sets the hscale of self to h
;;;

(define-handler (setf hScale) (h actor)
  (sk8-multival-bind (hh vv) (scale me)
    (declare (ignore hh))
    (setScale me h vv)
    h))

(define-handler vScale (actor)
  (sk8-multival-bind (h v) (scale me)
    v))

;;; (SETF vSCALE) -- sets the vscale of self to v.
;;;

(define-handler (setf vScale) (v actor)
  (sk8-multival-bind (hh vv) (scale me)
    (declare (ignore vv))
    (setScale me hh v)
    v))

;;; 1.0
;;; resetCoordinates -- scrolls and sets the scale of self to the original default:
;;;        (0,0) and scale of 1.

(define-handler resetCoordinates (actor)
  (let ((clos-window (gs:node-window me)))
    (sk8-multival-bind (oldXscale oldYScale) (scale me)
      (gs:withLockedWindow clos-window
        (setScale me 1.0s0 1.0s0)
        (setOrigin me 0.0s0 0.0s0
                   :forceRedraw (not (and (= oldXscale 1.0s0) (= oldYscale 1.0s0))))))))

;;; 1.0
;;; ZOOM -- zooms the contents of self by xFactor and yFactor.

(define-handler zoom (actor xFactor yFactor &key (preserveCenter t))
  (let* ((clos-window (gs:node-window me))
         (oldXScale (gs:node-logicalXScale me))
         (oldYScale (gs:node-logicalYScale me))
         (oldPhysicalXscale (gs:node-xScale me))
         (oldPhysicalYscale (gs:node-yScale me))
         (xScale (+ xFactor oldXScale))
         (yScale (+ yFactor oldYScale)))
    (gs:withLockedWindow clos-window
      ;; Change the scale.
      (setScale me xScale yScale)
      (when preserveCenter
        (let (cX cY)
          (setf xScale (gs:int-to-fixed xScale)
                yScale (gs:int-to-fixed yScale))
          ;; this is tricky - we have to determine the amount to pan in order to preserve the center.
          (SK8-multival-bind (x y) (size me)
            (setf cX (gs:f+ (gs:f/ x (gs:fdouble oldPhysicalXScale)) (gs:node-xOrigin me)))
            (setf cY (gs:f+ (gs:f/ y (gs:fdouble oldPhysicalYScale)) (gs:node-yOrigin me)))
            ;; and pan to the new center. Note that (setf origin) handles dirtying the nodes
            ;; and redrawing.
            (setOrigin me (gs:f- cX (gs:f/ x (gs:fdouble xScale))) (gs:f- cY (gs:f/ y (gs:fdouble yScale)))
                       :forceRedraw t)))))))

;;; 1.0
;;; -----
;;; TEXT
;;; -----

;;; ACTORTEXTSIZE -- given an actor, returns the size of the boundsrect required to fit
;;;                its text, or the text provided by tryText. 

(define-handler ActorTextSize (Actor &key theText)
  (gs:actor-text-size me :tryText thetext))

;;; 1.0
;;; TEXT -- returns the text of actor.

(define-handler text (Actor &key start end)
  (declare (ignore start end))
  (getf (gs:node-properties me) :text ""))

;;; 1.0
;;; (SETF TEXT) -- sets the text of self to text. The user can also specify a location.  In this function is
;;;             where an actor changes its draw function to one that can draw text.

(define-handler (setf text) (theText Actor &key textLocation)
  (unless (stringp theText)
    (if theText
      (setq theText (simpleObjectString theText :project (project me)))
      (setq theText "")))
    ;; If no location was passed, use the one in the actor's properties.
    (unless textlocation
      (setf textlocation (textLocation me)))
    (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill nil)
      (if (string= "" theText)
        ;; clearing text.
        (progn (setf (getf (gs:node-properties me) :text) "")
               (case (gs:node-drawFunction me)
                 (gs:sk8-text-draw (setf (gs:node-drawFunction me) 'gs:sk8-simple-draw))
                 (gs:sk8-full-draw (setf (gs:node-drawFunction me) 'gs:sk8-subActors-draw)))
               )
        ;; setting text.
        (progn (setf (getf (gs:node-properties me) :text) theText)
               (setf (getf (gs:node-properties me) :location) textlocation)
               (case (gs:node-drawFunction me)
                 (gs:sk8-simple-draw (setf (gs:node-drawFunction me) 'gs:sk8-text-draw))
                 (gs:sk8-subActors-draw (setf (gs:node-drawFunction me) 'gs:sk8-full-draw)))
               )))
  theText)

;;; 1.0
;;; TEXTLOCATION -- returns the location of the text of the actor. It is one of nine possibilities.
;;;               vertical options are top/center/bottom and horizontal options are
;;;               left/center/right.

(define-handler textLocation (actor)
  (getf (gs:node-properties me) :location 'center))

;;; 1.0
;;; (SETF TEXTLOCATION) -- sets the location of text to one of the nine options. If self does have
;;;                    text, it makes itself dirty to redraw the changes. If an invalid
;;;                    option is passed, an error is signaled.

(define-handler (setf textLocation) (location actor)
  (if (memq location '(topLeft topRight topCenter
                       centerLeft centerRight center
                       bottomLeft bottomRight bottomCenter))
    (progn (setf (getf (gs:node-properties me) :location) location)
           (when (not (string= (text me) ""))
             (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
               nil)))
    (sk8-error PropertyTypeMismatchError
               :object        location
               :expectedType  '(topLeft topRight topCenter
                                centerLeft centerRight center
                                bottomLeft bottomRight bottomCenter)
               :ownerObject   Actor
               :propertyName 'textLocation
               )))

;;; 1.0
;;; TEXTHOFFSET -- returns the number of logical units to use to displace the text
;;;              horizontally (a negative number displaces to the left).

(define-handler textHOffset (actor)
  (getf (gs:node-properties me) :textXOffset 0))

;;; 1.0
;;; (SETF TEXTHOFFSET) -- sets number of logical units to use to displace the text of self
;;;                   horizontally. If self has any text, it makes itself dirty to show the
;;;                   changes. 

(define-handler (setf textHOffset) (offset actor)
  (setf (getf (gs:node-properties me) :textXOffset) offset)
  (when (not (string= (text me) ""))
    (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
      nil)))

;;; 1.0
;;; TEXTVOFFSET -- returns the number of logical units to use to displace the text
;;;              vertically (a negative number displaces up).

(define-handler textVOffset (actor)
  (getf (gs:node-properties me) :textYOffset 0))

;;; 1.0
;;; (SETF TEXTVOFFSET) -- sets number of logical units to use to displace the text of self
;;;                   vertically. If self has any text, it makes itself dirty to show the
;;;                   changes. 

(define-handler (setf textVOffset) (offset actor)
  (setf (getf (gs:node-properties me) :textYOffset) offset)
  (when (not (string= (text me) ""))
    (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
      nil)))

(define-handler textOffset (actor)
  (sk8-multivals (textHOffset me) (textVOffset me)))

(define-handler (setf textOffset) (offsetList actor)
  (progn (setf (textHOffset me) (car offsetList))
         (setf (textVOffset me) (cadr offsetList))
         offsetList))

;;; 1.0
;;; TEXTFONT -- returns the name of the font self uses for its text.

(define-handler textFont (actor &key start end)
  (declare (ignore start end))
  (getf (gs:node-properties me) :text-font sk8::ChicagoFont))

;;; 1.0
;;; (SETF TEXTFONT) -- sets the font self uses to render its text. Accepts a string or
;;;                 a font object. The internal representation is the font object.

(define-handler (setf textfont) (theFont Actor &key start end)
  (declare (ignore start end))
  (if (stringp theFont)
    (progn (setf theFont (fontname-to-font theFont))
           (unless theFont (setf theFont sk8::ChicagoFont)))
    ;; Check to make sure the object is a font.
    (unless (inheritsFrom theFont font)
      (setf theFont sk8::ChicagoFont)))
  (setf (getf (gs:node-properties me) :text-font) theFont)
  (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
    nil)
  theFont)

;;; 1.0
;;; TEXTSIZE -- returns the size that self uses to render its text. The default is 12.

(define-handler textSize (actor &key start end)
  (declare (ignore start end))
  (getf (gs:node-properties me) :text-size 12))

;;; 1.0
;;; (SETF TEXTSIZE) -- sets the size of the text of self to size. When the actor has text, it
;;;                makes itself dirty to show the changes.

(define-handler (setf textSize) (size Actor &key start end)
  (declare (ignore start end))
  (setf (getf (gs:node-properties me) :text-size) size)
  (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
    nil)
  size)

;;; 1.0
;;; NUM-TO-STYLE -- given a number it returns a text style.

(defmacro num-to-style (num)
  `(case ,num
     (64 'expand) (32 'condense) (16 'sk8::shadow) (8 'outline) (4 'underline)
     (2 'italic) (1 'bold)))

;;; 1.0
;;; TEXTSYLE -- returns the style of self's text as a list of keywords.

(define-handler textStyle (actor &key num start end)
  (declare (ignore num start end))
  (let ((style (getf (gs:node-properties me) :text-style 0))
        (result nil))
    (dolist (num '(64 32 16 8 4 2 1))
      (when (>= style num)
        (push (num-to-style num) result)
        (decf style num)))
    (if (null result)
      '(plain)
      result)))

;;; 1.0
;;; (SETF TEXTSTYLE) -- sets the style of the actor's text to combinations based on the
;;;                  arguments. As usual, if the actor has any text, it makes
;;;                  itself dirty in order to show the changes.      

(define-handler (setf textStyle) (styleList actor &key start end)
  (declare (ignore start end))
  (let ((styleNum (gs:get-style-number styleList)))
    ;; Now the value is computed and we may set the textStyle.
    (setf (getf (gs:node-properties me) :text-style) styleNum)
    (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
      nil)
    (or styleList '(plain))))

(define-handler textcolor (Actor &key start end)
  (declare (ignore start end))
  (getf (gs:node-properties me) :text-color black))

(define-handler (setf textcolor) (color Actor &key start end)
  (declare (ignore start end))
  (let ((flags (gs:node-flags me)))
    (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill t)
      (let* ((oldColor (textColor me))
             (realColor (if (eq color oldColor)
                          color
                          (progn
                            (when (and oldColor
                                       (gs:textColorIsDynamic? flags)) ;this test is an optimization; most will not be dynamic
                              (setTextColorActor oldColor nil)) ;disconnect from old color
                            (setTextColorActor color me))))) ;connect to new color
        (setf (getf (gs:node-properties me) :text-color) realColor)
        (when (and (gs:textColorIsDynamic? flags)
                   (gs:node-window me))
          (enteringStage realColor)) ; this may set the boundsRect of the actor
        realColor))))

;;; -------------------------------
;;; BEHAVIOUR MODIFYING HANDLERS
;;; -------------------------------

;;; 1.0
;;; MOUSESENSITIVITY -- returns the mouse sensitivity of self.

(define-handler mouseSensitivity (actor)
  (case (gs:node-mouseStatus me)
    (0 'normal)
    (1 'opaque)
    (2 'transparent)
    (3 'invisible)
    (4 'custom)))

;;; 1.0
;;; (SETF MOUSESENSITIVITY) -- sets the sensitivity of self to mouse events to sensitivity.
;;;                        Valid options are normal, opaque and transparent.

(define-handler (setf mouseSensitivity) (sensitivity actor)
  (case sensitivity
    (normal (gs:mouseNormal! me))
    (opaque (gs:mouseOpaque! me))
    (transparent (gs:mouseTransparent! me))
    (invisible (gs:mouseInvisible! me))
    (custom (gs:mouseCustom! me))
    (t (sk8-error PropertyTypeMismatchError
                  :object        sensitivity
                  :expectedType  '(normal opaque transparent invisible custom)
                  :ownerObject   Actor
                  :propertyName 'mouseSensitivity
                  )))
  sensitivity)

;;; 1.0
;;; highlight -- returns whether self is highlighted.

(define-handler highlight (actor)
  (gs:hilited? (gs:node-flags me)))

;;; 1.0
;;; (SETF highlight) -- highlights self. 

(define-handler (setf highlight) (boolean actor)
  (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) :bounds nil)
    (if boolean
      (gs:hilited! (gs:node-flags me) 1)
      (gs:hilited! (gs:node-flags me) 0)))
  boolean)

(define-handler autohighlight (actor)
  (gs:autohilite? (gs:node-flags me)))

(define-handler inverts (actor)
  (gs:inverted? (gs:node-flags me)))

(define-handler (setf inverts) (boolean actor)
  (if boolean
    (gs:inverted! (gs:node-flags me) 1)
    (gs:inverted! (gs:node-flags me) 0))
  boolean)

;;; 1.0
;;; (SETF AUTOhighlight) --sets itself to autohighlight mode.

(define-handler (setf autohighlight) (boolean actor)
  (if boolean
    (gs:autohilite! (gs:node-flags me) 1)
    (gs:autohilite! (gs:node-flags me) 0))
  boolean)

;;; 1.0
;;; WANTSIDLE -- returns whether me wants idle events.

(define-handler wantsIdle (actor)
  (gs:wantsIdle? (gs:node-flags me)))

;;; 1.0
;;; (SETF WANTSIDLE) -- Tells self that it wants idle events. If self is contained in
;;;                  a window, the window is notified.

(define-handler (setf wantsIdle) (boolean actor)
  (let ((flags (gs:node-flags me))
        (clos-window (gs:node-window me)))
    (if boolean
      (unless (gs:wantsIdle? flags)
        (gs:wantsIdle! flags 1)
        (when clos-window
          (vector-push-extend me (slot-value clos-window 'gs:want-idle))))
      (when (gs:wantsIdle? flags)
        (gs:wantsIdle! flags 0)
        (when clos-window
          (setf (slot-value clos-window 'gs:want-idle)
                (delete me (slot-value clos-window 'gs:want-idle))))))))

;;; 1.0
;;; WANTMOUSEWITHIN -- returns whether self wants mouseWithin events.

(define-handler wantsMouseWithin (actor)
  (gs:wantsMouseWithin? (gs:node-flags me)))

;;; 1.0
;;; (SETF WANTSMOUSEWITHIN) -- Tells self that it wants mouseWithin events. If self is contained in
;;;                         a window, the window is notified.

(define-handler (setf wantsMouseWithin) (boolean actor)
  (let ((flags (gs:node-flags me)))
    (if boolean
      (unless (gs:wantsMouseWithin? flags)
        (gs:wantsMouseWithin! flags 1))
      (when (gs:wantsMouseWithin? flags)
        (gs:wantsMouseWithin! flags 0)))))

;;; 1.0
;;; LOCK -- locks the actor's window. If the actor is not attached to a window,
;;;        nothing happens.

(define-handler lock (actor)
  (let ((clos-window (gs:node-window me)))
    (when clos-window
      ;; Lock the window.
      (gs:lockWindow clos-window))))

;;; 1.0
;;; UNLOCK -- unlocks the actor's window one level. Redrawing happens with the visual effect and speed
;;;          passed in the keywords.

(define-handler unlock (actor &key theVisualEffect effectSpeed force)
  (let ((clos-window (gs:node-window me)))
    (when clos-window
      (gs:unlockWindow clos-window :visual-effect theVisualEffect :effect-speed effectSpeed :force force))))

;;; 1.0
;;; LOCKLEVEL -- returns the lock level of the actor's window. Returns nil if 
;;;            the actor is not attached to a window or the window is unlocked.

(define-handler lockLevel (actor)
  (let ((clos-window (gs:node-window me)))
    (when clos-window
      (slot-value clos-window 'gs:locked))))

;;; This is defined here for consistency with the edit field locked method. The idea is that
;;; lockLevel will go away...

(define-handler locked (actor)
  (when (lockLevel me)
    t))

;;; 1.0
;;; withVisualEffect -- executes body doing the visual effect when done. This is done by locking and 
;;;                unlocking the actor's window.
;;;                Note that if in the body the actor changes container, its original window will stay
;;;                locked.

(defmacro withVisualEffect ((theActor theVisualEffect effectSpeed) &body body)
  `(if (inheritsFrom ,theVisualEffect visualEffect)
     (prog2
      (lock ,theActor)
      (progn ,@body)
      (unlock ,theActor :theVisualEffect ,theVisualEffect :effectSpeed ,effectSpeed))
     (prog2 
      (lock ,theActor)
      (progn ,@body)
      (unlock ,theActor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                      OUR LOW LEVEL STUFF                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 1.0
;;; makeNodeFromObject -- creates and initializes a child node. This assumes that the parent ALREADY
;;;                    has a node. Note that translucency information is preserved.

(defun initialize-struct (parent child &key internal)
  (let* ((parentFlags (gs:node-flags parent))
         (new-rect (gs:make-rect))
         (new-rect2 (gs:make-rect))
         flags)
    (setf flags (copy-seq parentFlags))
    ;; Initializing non inheritable flags to defaults for new actors.
    (gs:boundsDirty! flags)
    (gs:fillDirty! flags)
    (gs:frameDirty! flags)
    (gs:hasConnectors! flags 0)
    (gs:onlyLocationChanged! flags 0)
    (gs:physicalBoundsRectDirty! flags)
    (gs:logicalBoundsRectDirty! flags 0)
    (gs:nodeSeenDirtyByWindow! flags 0)
    (gs:belongsToCachedPixmap! flags 0)
    (gs:contentsDirty! flags 1)
    (gs:boundsToDirtyRegion! flags)
    (gs:hasWindow! flags 0)
    ;; Copy the rects.
    (gs:copy-sk8rect (gs:node-logicalBoundsRect parent) new-rect)
    (gs:copy-sk8rect (gs:node-physicalBoundsRect parent) new-rect2)
    ;; Fill in the slots!
    ;; When used internally (called by Actor's initialize), the creationRelations have already handled these slots
    (unless internal
      (setf (gs:node-contents child) (gs:make-dlist))
      (setf (gs:node-lines child) nil))
    ;; bounds.
    (setf (gs:node-boundsRegion child) (T_NewRgnGC))
    (unless (handlep (gs:node-boundsRegion child))
      (sk8-error OSHeapFullError))
    ;; fill.
    (setf (gs:node-fillRegion child) (T_NewRgnGC))
    (unless (handlep (gs:node-fillRegion child))
      (sk8-error OSHeapFullError))
    ;; frame.
    (setf (gs:node-frameRegion child) (T_NewRgnGC))
    (unless (handlep (gs:node-frameRegion child))
      (sk8-error OSHeapFullError))
    (setf (gs:node-logicalBoundsRect child) new-rect)
    (setf (gs:node-physicalBoundsRect child) new-rect2)
    (setf (gs:node-xscale child) (gs:node-xscale parent))
    (setf (gs:node-yscale child) (gs:node-yscale parent))
    (setf (gs:node-logicalXScale child) (gs:node-logicalXScale parent))
    (setf (gs:node-logicalYScale child) (gs:node-logicalYScale parent))
    (setf (gs:node-xorigin child) (gs:node-xorigin parent))
    (setf (gs:node-yorigin child) (gs:node-yorigin parent))
    (setf (gs:node-framecolor child) (gs:node-frameColor parent))
    (setf (gs:node-framesize child) (gs:copy-pt (gs:node-framesize parent)))
    (setf (gs:node-fillcolor child) (gs:node-fillColor parent))
    (setf (gs:node-translucentContents child) (gs:node-translucentContents parent))
    (setf (gs:node-flags child) flags)
    (setf (gs:node-mousestatus child) (gs:node-mousestatus parent))
    (setf (gs:node-properties child) (copy-list (gs:node-properties parent)))
    ;; Next time someone asks for the ownedRegion, a new one will be generated.
    (remf (gs:node-properties child) :ownedRegion)
    (setf (gs:node-offsetBy child) (gs:make-pt))
    ;; OJO! The function references can change from the symbols to
    ;; the functions when we are done with the system.
    (setf (gs:node-drawfunction child) (or (gs:node-drawFunction parent)
                                           'gs:sk8-simple-draw))
    (setf (gs:node-contentsdrawfunction child) (or (gs:node-contentsDrawFunction parent)
                                                   'gs:sk8-draw-contents))
    (setf (gs:node-doubleClickStyle child) (gs:node-doubleClickStyle parent))))

;;; initialize-internal -- initializes internal node structure
;;;

(define-handler initialize (actor original isNew initArgs)
  (declare (ignore isNew initargs))
  (call-next-method)
  ;; Making sure original is its actor parent!
  (setf original (originalancestor me original Actor))
  ;; before anything initialize the slots from the old struct!
  (initialize-struct original me :internal t)
  ;; Sets the hasConnectors flag when necessary.
  (when (gs:node-lines me)
    (gs:hasConnectors! (gs:node-flags me) 1))
  ;; The new guy is not automatically in its original's container; if it hasn't been put into a new
  ;; container (by the creationRelations) then just clear the container-related fields.
  (when (eq (gs:node-container me) (container original))
    (setf (gs:node-container me) nil))
  ;; It's definitely not on stage yet, so set node-window to nil.
  (setf (gs:node-window me) nil)
  ;; If any of the renders is a dynamic Renderer, then use a copy of it.
  ;; We assume that ownsRegion is true
  (macrolet ((adjustRenderer (me region)
               `(let ((oldRenderer (,region ,me)))
                  (when (dynamic oldRenderer)
                    (setValue ',region ,me Black)
                    (setf (,region ,me) (sk8::copy oldRenderer))))))
    (adjustRenderer me fillColor)
    (adjustRenderer me frameColor)
    (adjustRenderer me textColor))
  ;; If the actor caches its pixmap, generate the pixmap struct.
  (when (cachesPixmap me)
    (gs:initialize-cached-pixmap-from-parent me))
  ;; If the actor resizes contents, cache their rects.
  (when (resizesContents me)
    (cacheContentsRects me)))

(defun add-one-node-to-window (clos-window theActor flags)
  (catch :crash 
    (gs:map-nodes* #'(lambda (subActor)
                       (setf flags (gs:node-flags subActor))
                       ;; Dirty the node.
                       (gs:dirty-one-node flags)
                       (gs:nodeSeenDirtyByWindow! flags 0)
                       (when (gs:wantsIdle? flags)
                         (vector-push-extend subActor (slot-value clos-window 'gs:want-Idle)))
                       (when (gs:ownedRegion? flags)
                         (vector-push-extend subActor (slot-value clos-window 'gs:owners)))
                       (setf (gs:node-window subActor) clos-window)
                       (when-unwind 
                         (enteringStage subActor)
                         (throw :crash subActor)))
                   theActor)))

;;; 1.0
;;; ADD-ACTOR-TO-WINDOW -- This function updates the window structure when an
;;;                       actor gets added to it. Most of the work is to add all the
;;;                       new subactors that want events to the window.
;;;
;;; If the actor is visible, we make it dirty WITHOUT adding it to the dirty
;;; region, since it does not have an old region to invalidate.

;;; Still have to decide how each actor will record this.

(defun add-actor-to-window (clos-window theActor)
  (let ((flags nil)
        culprit)
    (setf culprit
          (catch :crash
            (gs:map-nodes* 
             #'(lambda (subActor)
                 (setf flags (gs:node-flags subActor))
                 ;; Dirty the node.
                 (gs:dirty-one-node flags)
                 (when (gs:wantsIdle? flags)
                   (vector-push-extend subActor (slot-value clos-window 'gs:want-Idle)))
                 (when (gs:ownedRegion? flags)
                   (vector-push-extend subActor (slot-value clos-window 'gs:owners)))
                 (setf (gs:node-window subActor) clos-window)
                 (when-unwind
                   (enteringStage subActor)
                   (throw :crash subActor)))
             theActor)))
    ;; If culprit has something on it, an error has occured and we return the culprit.
    (if culprit
      culprit
      ;; Add the new actor the the window's dirty node list.
      (progn (gs:make-new-node-dirty theActor clos-window)
             nil))))

;;; 1.0
;;; DETACH -- detaches the actor from self. Nothing happens if it has no container.
;;;          NOTE: Shadowed in UI-ACTOR.LISP in OLD SYSTEM.
;;;
;;; Let A be the actor we are detaching from its container B.
;;;
;;; Detaching an object involves the following steps:  if the actor has a window, we just clear
;;; its containment fields and dispose the window. (NOTE that this function should not be called
;;; to swap two actors from a window: we need a shortcut).
;;;
;;; If the actor does not have a window, we need to update the window structure as we remove it.
;;; This involves the following steps: [1] Remove A from the contents field of B,
;;; and clear the container field of A. [2] if A is translucent or contains translucent objects, it
;;; might be the case that A is the actor of B that makes B contain translucent objects. In this
;;; case, we want to clear the translucent contents bit of B. Then we need to update this bit all
;;; the way up the hierarchy. [3] We have to make A dirty to add its region to the dirty region
;;; of the window. [4] We also need to remove all the detached objects from the window event
;;; lists.
;;;
;;; THINGS THAT THE OLD SYSTEM DOES: [1] for each object detached it removes its connectors.
;;; [2] for each object detached it unaffixes it if it is fixed to another actor. 


(define-handler detach (Object oldContent)
  (declare (ignore me oldContent))
  )

(define-handler detach (Actor theActor)
  (let ((flags (gs:node-flags theActor))
        (tla (sk8::window theActor))
        (clos-window (gs:node-window me))
        curkey)
    ;; [1] Updating containment hierarchy.
    (setf (gs:node-container theActor) nil)
    (setf (gs:node-contents me) (delete theActor (gs:node-contents me)))
    ;; [2] Updating translucency structure.
    (unless (gs:opaque? flags) 
      (gs:remove-from-translucent-contents me)
      (unless (gs:translucentContents? me)
        (setf (gs:node-contentsDrawFunction me) 'gs:sk8-draw-contents)))
    ;; Telling both actors to prepare for the container change.
    (prepareForNewContainer theActor nil)
    (prepareForNewContent me nil)
    (setf curkey (keyTarget tla))
    ;; Do window stuff.
    (unwind-protect 
      ;; [4] Removing nodes contained in actor from the window struct.
      (gs:map-nodes* #'(lambda (subActor)
                         (when (eq subActor curKey)
                           (setf (getf (gs:node-properties tla) :curKey) nil))
                         (when clos-window
                           (setf (gs:node-window subActor) nil)
                           (setf flags (gs:node-flags subActor))
                           ;; Dirty the rect and the regions.
                           (gs:physicalBoundsRectDirty! flags)
                           (gs:boundsDirty! flags)
                           (gs:fillDirty! flags)
                           (gs:frameDirty! flags)
                           ;; Do window specific stuff.
                           (when (gs:wantsIdle? flags)
                             (setf (slot-value clos-window 'gs:want-Idle)
                                   (delete subActor (slot-value clos-window 'gs:want-Idle))))
                           (when (gs:ownedRegion? flags)
                             (setf (slot-value clos-window 'gs:owners)
                                   (delete subActor (slot-value clos-window 'gs:owners))))
                           (leavingStage subActor)))
                     theActor)
      ;; [3] Making the region the node used to occupy dirty.
      (when clos-window
        (gs:withLockedWindow clos-window
          (gs:remove-dirty-from-window theActor clos-window)
          (gs:set-dirty-contents-flag me)))
      ;; [4] Changing the scale of the detached actor.
      (unless (and (= (gs:node-XScale me) 1) (= (gs:node-YScale me) 1))
        (update-actors-scale theActor))
      )))


;;; 1.0
;;; ATTACH -- attaches an actor to self.  This does not work for window actors, since 
;;;          a window actor can only be attached to the stage.
;;;
;;; Note that this only gets called when both actors consent to the attachment.

(defun attach (container content &optional after)
  (if (eq stage container)
    (attach-to-stage container content after)
    (attach-to-actor container content after)))

;;; If the user functions cause an error while attaching an actor, this functions tries to undo the damage.

(defun abort-attach-to-actor (me theActor)
  (let ((flags (gs:node-flags theActor))
        (clos-window (gs:node-window me))
        (tla (sk8::window me))
        curkey)
    ;; [1] Updating containment hierarchy.
    (setf (gs:node-container theActor) nil)
    (setf (gs:node-contents me) (delete theActor (gs:node-contents me)))
    ;; [2] Updating translucency structure.
    (unless (gs:opaque? flags) 
      (gs:remove-from-translucent-contents me)
      (unless (gs:translucentContents? me)
        (setf (gs:node-contentsDrawFunction me) 'gs:sk8-draw-contents)))
    ;; Do window stuff.
    (setf curKey (keyTarget tla))
    ;; [4] Removing nodes contained in actor from the window struct.
    (gs:map-nodes* #'(lambda (subActor)
                       (when (eq curkey subActor)
                         (setf (getf (gs:node-properties tla) :curKey) nil))
                       (when clos-window
                         (setf (gs:node-window subActor) nil)
                         (setf flags (gs:node-flags subActor))
                         (when (gs:wantsIdle? flags)
                           (setf (slot-value clos-window 'gs:want-Idle)
                                 (delete subActor (slot-value clos-window 'gs:want-Idle))))
                         (when (gs:ownedRegion? flags)
                           (setf (slot-value clos-window 'gs:owners)
                                 (delete subActor (slot-value clos-window 'gs:owners))))
                         ))
                   theActor)))

;;; Arg 1 is the new container.
;;; Arg 2 is the new content.

(defun attach-to-actor (me theActor &optional after)
    (let* ((subActorFlags (gs:node-flags theActor))
           (clos-window (gs:node-window me))
           (container (container theActor))
           (errors? t))
      ;; if the actor was in another actor, remove it from it.
      (when container (setf (container theActor) nil))
      ;; Change draw function of self if necessary.
      (when (gs:dlist-empty? (gs:node-contents me))
        (case (gs:node-drawFunction me)
          (gs:sk8-simple-draw (setf (gs:node-drawFunction me) 'gs:sk8-subActors-draw))
          (gs:sk8-text-draw (setf (gs:node-drawFunction me) 'gs:sk8-full-draw))))
      ;; Update containment info.
      (if after
        (gs:vector-insert-before (gs:node-contents me) theActor 
                                  (if (numberp after)
                                    after
                                    (position after (gs:node-contents me))))
        (vector-push-extend theActor (gs:node-contents me)))
      (setf (gs:node-container theActor) me)
      ;; if necessary, update translucency status of actor.
      (when (not (gs:opaque? subActorFlags))
        (when (not (gs:translucentContents? me))
          (setf (gs:node-contentsDrawFunction me)
                'gs:sk8-draw-contents-with-translucency))
        (gs:add-to-translucent-contents me))
      ;; Call preparation methods for container and content.
      (unwind-protect
        (progn
          (prepareForNewContainer theActor me)
          (prepareForNewContent me theActor)
          (setf errors? nil))
        (when errors?
          (abort-attach-to-actor me theActor)
          (return-from attach-to-actor nil)))
      ;; If the new container's scale is not {1,1}, update the actor's scale.
      (unless (and (= (gs:node-XScale me) 1) (= (gs:node-YScale me) 1))
        (update-actors-scale theActor))
      ;; update the window event lists and dirty node list.
      (when clos-window
        (when (add-actor-to-window clos-window theActor)
          ;; If it returned something, an error has occured!
          (abort-attach-to-actor me theActor)
          (return-from attach-to-actor nil))
        (gs:making-self-dirty (theActor nil t t t (gs:node-boundsRegion theActor) :bounds t)
          (gs:physicalBoundsRectDirty! subActorFlags)
          ))))

(defun actor-string-width (theString)
  (with-pstrs ((ps theString))
    (#_stringWidth ps)))
                         
;;; 1.0
;;; RENDER-TEXT -- The most basic text drawing method for actors. This is a copy of the
;;;              method the round button uses. The idea for the future is to allow text
;;;              to be located at various places within the actor with integer x and y
;;;              displacements for offseting.

(defun render-text (theActor thePort)
  (gs:let+ ((qd-rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect theActor)))
            (plist (gs:node-properties theActor))
            (theText (getf plist :text ""))
            (location (getf plist :location 'center))
            (container (gs:node-container theActor))
            (yScale (if container (gs:node-yscale container) 1s0))
            (xScale (if container (gs:node-xscale container) 1s0))
            (xOffset (getf plist :textXOffset 0))
            (yOffset (getf plist :textYOffset 0))
            (textColor (getf plist :text-color black))
            (style (getf plist :text-style 0))
            (thefont (getf plist :text-font sk8::ChicagoFont))
            (size (gs:f.round (* (getf plist :text-size 12) yScale)))
            x y)
    (when (numberp textColor)
      (setf textColor black)
      (setf (getf plist :text-color) black))
    (unless (string= theText "")
      (unless (stringp theText) (setq theText (format nil "~a" theText)))
      (#_TextFont (fontData thefont))
      (#_TextSize size)
      (#_TextFace style)
      ;; Deciding were to move to.
      (case location
        (topLeft
         (setf x (+ 3 (rref qd-rect rect.left) (gs:f.round (* (gs:pt-h (gs:node-frameSize theActor)) xScale)))
               y (+ (rref qd-rect rect.top) 
                    (gs:f.round (* (gs:pt-v (gs:node-frameSize theActor)) yScale))
                    ;; we only care about the height.
                    (font-info))))
        (topCenter
         (setf x (- (truncate (+ (rref qd-rect rect.left) (rref qd-rect rect.right)) 2)
                    (truncate (actor-string-width theText) 2))
               y (+ (rref qd-rect rect.top) 
                    (gs:f.round (* (gs:pt-v (gs:node-frameSize theActor)) yScale))
                    ;; we only care about the height.
                    (font-info))))
        (topRight
         (setf x (- (rref qd-rect rect.right) 
                    (gs:f.round (* (gs:pt-h (gs:node-frameSize theActor)) xScale))
                    (actor-string-width theText))
               y (+ (rref qd-rect rect.top) 
                    (gs:f.round (* (gs:pt-v (gs:node-frameSize theActor)) yScale))
                    ;; we only care about the height.
                    (font-info))))
        (centerLeft
         (setf x (+ 3 (rref qd-rect rect.left) (gs:f.round (* (gs:pt-h (gs:node-frameSize theActor)) xScale)))
               y (+ (truncate (+ (rref qd-rect rect.top) (rref qd-rect rect.bottom)) 2)
                    (multiple-value-bind (height descent) (font-info) 
                      (- (truncate (+ height descent) 2) descent)))))
        (center 
         (setf x (- (truncate (+ (rref qd-rect rect.left) (rref qd-rect rect.right)) 2)
                    (truncate (actor-string-width theText) 2))
               y (+ (truncate (+ (rref qd-rect rect.top) (rref qd-rect rect.bottom)) 2)
                    (multiple-value-bind (height descent) (font-info) 
                      (- (truncate (+ height descent) 2) descent)))))
        (centerRight
         (setf x (- (rref qd-rect rect.right) 
                    (gs:f.round (* (gs:pt-h (gs:node-frameSize theActor)) xScale))
                    (actor-string-width theText))
               y (+ (truncate (+ (rref qd-rect rect.top) (rref qd-rect rect.bottom)) 2)
                    (multiple-value-bind (height descent) (font-info) 
                      (- (truncate (+ height descent) 2) descent)))))
        (bottomLeft
         (setf x (+ 3 (rref qd-rect rect.left) (gs:f.round (* (gs:pt-h (gs:node-frameSize theActor)) xScale)))
               y (- (rref qd-rect rect.bottom) 
                    (gs:f.round (* (gs:pt-v (gs:node-frameSize theActor)) yScale))
                    (multiple-value-bind (height descent) (font-info) 
                      (declare (ignore height))
                      descent))))
        (bottomCenter
         (setf x (- (truncate (+ (rref qd-rect rect.left) (rref qd-rect rect.right)) 2)
                    (truncate (actor-string-width theText) 2))
               y (- (rref qd-rect rect.bottom) 
                    (gs:f.round (* (gs:pt-v (gs:node-frameSize theActor)) yScale))
                    (multiple-value-bind (height descent) (font-info) 
                      (declare (ignore height))
                      descent))))
        (bottomRight
         (setf x (- (rref qd-rect rect.right) 
                    (gs:f.round (* (gs:pt-h (gs:node-frameSize theActor)) xScale))
                    (actor-string-width theText))
               y (- (rref qd-rect rect.bottom) 
                    (gs:f.round (* (gs:pt-v (gs:node-frameSize theActor)) yScale))
                    (multiple-value-bind (height descent) (font-info) 
                      (declare (ignore height))
                      descent)))))
      (incf x (gs:f.round (* xOffset xScale)))
      (incf y (gs:f.round (* yOffset yScale)))
      ;; Getting ready to draw.
      (render-string textColor theActor theText x y thefont size style thePort))))

;;; PointOnWhichPart -- given an actor and a pair of coords, returns t if the part specified in the how
;;;                 keyword is hit.

(define-handler pointOnWhichPart (Actor x y &key (part 'bounds))
  (if (memq part '(bounds rect sk8::fill frame))
    (let ((flags (gs:node-flags me))             
          (tla (sk8::window me))
          (qd-point (gs:SK8Coords-to-point x y)))
      ;; convert point into window space...
      (when tla
        (setq qd-point (subtract-points qd-point (view-position (gs:node-window tla)))))
      ;; start with the rectangle
      (if (gs:coords-in-rect-p (point-h qd-point)
                               (point-v qd-point)
                               (gs:node-physicalBoundsRect me))
        ;; we are in the rectangle
        (if (eq part 'rect)
          (return-from pointOnWhichPart 'rect)
          ;; on to the region
          (progn
            (gs:recompute-bounds-region me flags)
            (if (#_PtInRgn qd-point (gs:node-boundsRegion me))
              ;; we are in the bounds
              (if (eq part 'bounds)
                (return-from pointOnWhichPart 'bounds)
                ;; on to the fill
                (progn
                  (gs:recompute-fill-region me flags)
                  (if (#_PtInRgn qd-point (gs:node-fillRegion me))
                    ;; we are in the fill
                    (if (eq part 'sk8::fill)
                      (return-from pointOnWhichPart 'sk8::fill))
                    (if (eq part 'frame)
                      (return-from pointOnWhichPart 'frame))))))))))
    (SK8-error ArgumentTypeMismatchError
               :handlerName 'pointOnWhichPart :argumentName 'part
               :object part :expectedType '(bounds rect fill frame)))
  nil)

;;; This Public version makes it much easier to use from SK8Script.
;;; by is a list of the form {x,y}.

(define-handler partHit (actor &key by (target 'bounds))
  (definitely-recover-list by)
  (pointOnWhichPart me (car by) (cadr by) :part target))

;;; FIND-RESIZE-DIRECTION -- Returns what point to resize from.
;;;                     This works for rectangles. Will not work at all with weird regions.

(define-handler find-resize-direction (actor x y)
  (let* ((rect (gs:node-physicalBoundsRect me))
         (left (gs:f+ (gs:rect-left rect) #%5))
         (top (gs:f+ (gs:rect-top rect) #%5))
         (right (gs:f- (gs:rect-right rect) #%5))
         (bottom (gs:f- (gs:rect-bottom rect) #%5))
         (tla (sk8::window me)))
    ;; adjust for logical space
    (when tla
      (let ((loc (view-position (gs:node-window tla))))
        (setq x (- x (point-h loc))
              y (- y (point-v loc)))))
    (cond ((<= x left)
           (cond ((<= y top) :topleft)
                 ((>= y bottom) :bottomleft)
                 (t :left)))
          ((>= x right)
           (cond ((<= y top) :topright)
                 ((>= y bottom) :bottomright)
                 (t :right)))
          ((<= y top) :top)
          ((>= y bottom) :bottom)
          (t nil))))

;;; 1.0
;;; RESIZE -- the resize method for actors. Works for top level actors as well!!! Top level actors resize non
;;;         live!

(define-handler resize (Actor how &key (live (if (option-key-p) (not *live*) *live*)) otherActors)
  (when (resizable me)
    ;; Fix the otherActors arg...
    (unless (listp otherActors)
      (setf otherActors (list otherActors)))
    (setf otherActors (cons me otherActors))
    ;; No live resizing for windows!
    (when (gs:hasWindow? (gs:node-flags me))
      (setf live nil))
    ;; Carry on!
    (let ((dx 0) (dy 0) (dxx 0) (dyy 0)
          (originalH (eventH))
          (originalV (eventV))
          (bogusfun #'(lambda (x y) (declare (ignore x y)) t)) ;;; alway true
          newXTarget newYTarget itemsCache Htestfunc Hcorner Vtestfunc VCorner 
          basel baset baser baseb TopFixed LeftFIxed
          fixedbasel fixedbaset fixedbaser fixedbaseb
          baseWidth baseHeight)
      (declare (special dx dy dxx dyy))
      (declare (dynamic-extent bogusfun))
      ;; Set up the variables.
      (sk8-multival-setf (basel baset baser baseb) (actorsBounds otherActors :physical t))
      (setf fixedbasel basel fixedbaset baset fixedbaser baser fixedbaseb baseb)
      (case how
        (top (setf newYTarget 'dy
                   Htestfunc bogusfun Vtestfunc #'<=
                   TopFixed t LeftFIxed nil
                   Hcorner nil VCorner baseb))
        (bottom (setf newYTarget 'dyy
                      Htestfunc bogusfun Vtestfunc #'>=
                      TopFixed nil LeftFIxed nil
                      Hcorner nil VCorner baset))
        (left (setf newXTarget 'dx
                    Htestfunc #'<= Vtestfunc bogusfun
                    TopFixed nil LeftFIxed t
                    Hcorner baser VCorner nil))
        (right (setf newXTarget 'dxx
                     Htestfunc #'>= Vtestfunc bogusfun
                     TopFixed nil LeftFIxed nil
                     Hcorner basel VCorner nil))
        (topleft (setf newXTarget 'dx newYTarget 'dy
                       Htestfunc #'<= Vtestfunc #'<=
                       TopFixed t LeftFIxed t
                       Hcorner baser VCorner baseb))
        (topright (setf newXTarget 'dxx newYTarget 'dy
                        Htestfunc #'>= Vtestfunc #'<=
                        TopFixed t LeftFIxed nil
                        Hcorner basel VCorner baseb))
        (bottomleft (setf newXTarget 'dx newYTarget 'dyy
                          Htestfunc #'<= Vtestfunc #'>=
                          TopFixed nil LeftFIxed t
                          Hcorner baser VCorner baset))
        (bottomright (setf newXTarget 'dxx newYTarget 'dyy
                           Htestfunc #'>= Vtestfunc #'>=
                           TopFixed nil LeftFIxed nil
                           Hcorner basel VCorner baset)))
      ;; Set up initialRect and cache the items.
      (setf itemsCache (construct-rect-cache basel baset (- baser basel) (- baseb baset) otherActors))
      ;; Just Do it!
      (if live
        ;; LIVE: change the boundsrects all the time!
        (let ((tracker #'(lambda (dh dv)
                           (when newXTarget (set newXTarget (gs:f.round dh)))
                           (when newYTarget (set newYTarget (gs:f.round dv)))
                           ;; Update the base rect!
                           (incf fixedbasel dx) (incf fixedbaset dy) (incf fixedbaser dxx) (incf fixedbaseb dyy)
                           (setf basel fixedbasel baset fixedbaset baser fixedbaser baseb fixedbaseb)
                           (unless (funcall Htestfunc basel Hcorner) (setf basel Hcorner))
                           (unless (funcall Vtestfunc baset VCorner) (setf baset VCorner))
                           (unless (funcall Htestfunc baser Hcorner) (setf baser Hcorner))
                           (unless (funcall Vtestfunc baseb VCorner) (setf baseb VCorner))
                           (setf baseWidth (- baser basel)
                                 baseHeight (- baseb baset))
                           ;; Do the resizing.
                           (withActorLocked (me)
                             (dolist (c itemsCache)
                               (sk8-multival-bind (ll tt rr bb) 
                                                  (new-scaled-size basel baset baseWidth baseHeight c)
                                 (sk8-multival-bind (minh minv) (minimumsize (car c))
                                   (if LeftFIxed
                                     (unless (> (- rr ll) minh)
                                       (setf ll (- rr minh)))
                                     (unless (> (- rr ll) minh)
                                       (setf rr (+ ll minh))))
                                   (if TopFixed
                                     (unless (> (- bb tt) minv)
                                       (setf tt (- bb minv)))
                                     (unless (> (- bb tt) minv)
                                       (setf bb (+ tt minv))))
                                   (setBoundsRect (car c) ll tt rr bb :physical t))))))))
          (declare (dynamic-extent tracker))
          (gs:track-mousedown tracker :initial-mouseloc (make-point originalH originalV)))
        ;; NONLIVE: highlight the boundsRects of all actors.
        (gs:let+ ((outline-rgn (:region))
               (old-outline-rgn (:region))
               (temp-region (:region))
               (rectsCache nil)
               (MinSizeList nil)
               theRect
               (tracker #'(lambda (dh dv)
                            (when newXTarget (set newXTarget (gs:f.round dh)))
                            (when newYTarget (set newYTarget (gs:f.round dv)))
                            ;; Update the base rect!
                            (incf fixedbasel dx) (incf fixedbaset dy) (incf fixedbaser dxx) (incf fixedbaseb dyy)
                            (setf basel fixedbasel baset fixedbaset baser fixedbaser baseb fixedbaseb)
                            (unless (funcall Htestfunc basel Hcorner) (setf basel Hcorner))
                            (unless (funcall Vtestfunc baset VCorner) (setf baset VCorner))
                            (unless (funcall Htestfunc baser Hcorner) (setf baser Hcorner))
                            (unless (funcall Vtestfunc baseb VCorner) (setf baseb VCorner))
                            (setf baseWidth (- baser basel)
                                  baseHeight (- baseb baset))
                            (with-port (gs:get-wmgr-port)
                              (#_copyRgn outline-rgn old-outline-rgn)
                              ;; Update region
                              (#_setEmptyRgn outline-rgn)
                              ;; Run through the cachedItems reseting the rects.
                              (do ((theItems itemsCache (cdr theItems))
                                   (theMinSizes MinSizeList (cdr theMinSizes))
                                   (theRects rectsCache (cdr theRects)))
                                  ((null theItems))
                                (sk8-multival-bind (ll tt rr bb) 
                                                   (new-scaled-size basel baset baseWidth baseHeight (car theItems))
                                  (if LeftFIxed
                                    (unless (> (- rr ll) (caar theMinSizes))
                                      (setf ll (- rr (caar theMinSizes))))
                                    (unless (> (- rr ll) (caar theMinSizes))
                                      (setf rr (+ ll (caar theMinSizes)))))
                                  (if TopFixed
                                    (unless (> (- bb tt) (cadar theMinSizes))
                                      (setf tt (- bb (cadar theMinSizes))))
                                    (unless (> (- bb tt) (cadar theMinSizes))
                                      (setf bb (+ tt (cadar theMinSizes)))))
                                  (setf theRect (car theRects))
                                  (rset theRect :rect.left ll)
                                  (rset theRect :rect.top tt)
                                  (rset theRect :rect.right rr)
                                  (rset theRect :rect.bottom bb))
                                (#_rectRgn temp-region theRect)
                                (#_unionRgn temp-region outline-rgn outline-rgn))
                              ;; Reset grafport. Required every time due to multithreading. 
                              (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
                              (#_PenMode #$PatXor)
                              (#_PenPat *gray-pattern*)
                              ;; Do the drawing!
                              (#_FrameRgn old-outline-rgn)      ;; erase!
                              (#_FrameRgn outline-rgn)
                              (sleep 1/60))
                            )))
          (declare (dynamic-extent tracker))
          (setf otherActors (cons me otherActors))
          ;; OK: initialize the rects and the region.
          (#_setEmptyRgn outline-rgn)
          (dolist (c otherActors)
            (push (minimumsize c) MinSizeList)
            (sk8-multival-bind (ll tt rr bb) (boundsRect c :physical t)
              (setf theRect (newRecordGCPtr :rect :left ll :top tt :right rr :bottom bb))
              (push theRect rectsCache)
              (#_rectRgn temp-region theRect)
              (#_unionRgn temp-region outline-rgn outline-rgn)))
          ;; Initialize the resize region.
          (unless live
            (with-port (gs:get-wmgr-port)
              (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
              (#_PenMode #$PatXor)
              (#_PenPat *gray-pattern*)
              (#_FrameRgn outline-rgn)))
          ;; Do the resizing!
          (sk8-multival-bind (totalh totalv) 
                             (gs:track-mousedown tracker :initial-mouseloc (make-point originalH originalV))
            ;; Final erase and set the rect.
            (with-port (gs:get-wmgr-port)
              (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
              (#_PenMode #$PatXor)
              (#_PenPat *gray-pattern*)
              (#_FrameRgn outline-rgn)
              (#_PenNormal))
            (when newXTarget (set newXTarget (gs:f.round totalH)))
            (when newYTarget (set newYTarget (gs:f.round totalV)))
            (unless (and (= totalh 0) (= totalv 0))
              (withActorLocked (me)
                (do ((c itemsCache (cdr c))
                     (theMinSizes MinSizeList (cdr theMinSizes))
                     )
                    ((null c))
                  (sk8-multival-bind (ll tt rr bb) 
                                     (new-scaled-size basel baset basewidth baseHeight (car c))
                    (if LeftFIxed
                      (unless (> (- rr ll) (caar theMinSizes))
                        (setf ll (- rr (caar theMinSizes))))
                      (unless (> (- rr ll) (caar theMinSizes))
                        (setf rr (+ ll (caar theMinSizes)))))
                    (if TopFixed
                      (unless (> (- bb tt) (cadar theMinSizes))
                        (setf tt (- bb (cadar theMinSizes))))
                      (unless (> (- bb tt) (cadar theMinSizes))
                        (setf bb (+ tt (cadar theMinSizes)))))
                    (setBoundsRect (caar c) ll tt rr bb :physical t)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        SPECIAL STAGE METHODS USING ACTOR's STUFF                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1.0
;;; ATTACH -- This method handles the attaching of an actor to the screen. In this case, the actor
;;;          gets a window of its own. So, a window is created and the actor is made its top level
;;;          actor. This overshadows the actor attach method.
;;;
;;; This first version of this function only handles the case when we are attaching a window
;;; actor to the stage. Now also allows rectangles to be attached to the stage. We should deal with creating
;;; wdefs for other shapes.
;;;
;;; Actor's locations are derived from their rects. Thus, the location of the window for actor is
;;; determined by the topleft of the actor's logical rect. The problem is that as we attach actor to
;;; a window, its rects have to be expressed in window coordinates (thus loosing the user's
;;; desired positioning information). To solve this problem, when a top level actor is detached, we
;;; transform its rectangles into rectangles that contain the positioning information.

(defun translateStyle (style)
  (case style
    (documentWithZoom        '(:document-with-zoom t))
    (documentWithZoomNoClose '(:document-with-zoom nil))
    (documentWithGrow        '(:document-with-grow t))
    (documentWithGrowNoClose '(:document-with-grow nil))
    (document                '(:document t))
    (documentNoClose         '(:document nil))
    (doubleEdgeBox           '(:double-edge-box t))
    (singleEdgeBox           '(:single-edge-box t))
    (shadowEdgeBox           '(:shadow-edge-box t))
    (movableDialog           '(:movable-dialog t))
    (tool                    '(:tool t))
    ))

(defun abort-attach-to-stage (me theActor)
  (declare (ignore me))
  ;; Deactivate the window.
  (when (eq (gs:node-window theActor) gs:*currentTla*)
    (deactivate theActor))
  ;; Do the work.
  (without-interrupts
   (let ((clos-window (gs:node-window theActor)))
     ;; restoring the position info to the rects.
     (revert-to-nontla-regions theActor clos-window)
     ;; Updating containment and disposing window.
     (setf (gs:node-container theActor) nil)
     (gs:hasWindow! (gs:node-flags theActor) 0)
     ;; Telling all nodes removed that they do not have a window.
     (gs:map-nodes* #'(lambda (subActor)
                        (setf (gs:node-window subactor) nil))
                    theActor)
     ;; Disposing the window.
     (gs:sk8-close-window clos-window))))

(defun attach-to-stage (me theActor &optional after)
  (declare (ignore after))
  (without-interrupts ; 2-2-93 ruben
   (let* ((logRect (gs:node-logicalBoundsRect theActor))
          (flags (gs:node-flags theActor))
          (properties (gs:node-properties theActor))
          (style (if (inheritsFrom theActor Rectangle)
                   (getf properties :style 'blank)
                   'blank))
          (depth (getf properties :depth 0))
          (dither (getf properties :dither nil)))
     ;; Detaching from any previous containers if necessary.
     (let ((container (gs:node-container theActor)))
       (when container   ;;; used to be "(and container (gs:node-window container))"?!?!?!  Why!  This caused an error.  -BJR
         (detach container theActor)))
     ;; Create a window and other stuff...
     (let* ((special-wdef (car (memq style '(sk8Window blank floating))))
            (mcl-style (unless special-wdef (translateStyle style)))
            (width (- (gs:rect-right logRect) (gs:rect-left logRect)))
            (height (- (gs:rect-bottom logRect) (gs:rect-top logRect)))
            ;; Instantiate MCL version of window
            (clos-window (make-instance (if (floating theActor)
                                          'gs:*sk8-windoid* 
                                          'gs:*SK8-window*)
                           :view-size (make-point (gs:f.round width) (gs:f.round height))
                           :view-position (make-point (gs:f.round (gs:rect-left logrect))
                                                      (gs:f.round (gs:rect-top logrect)))
                           :my-actor-object theActor
                           :window-title (if (inheritsFrom theActor rectangle)
                                           (windowTitle theActor)
                                           (objectString theActor))
                           :window-show nil
                           :color-p t
                           :window-type (if special-wdef :single-edge-box (car mcl-style))
                           :close-box-p (if mcl-style (cadr mcl-style) t)
                           :depth depth
                           :dither dither
                           :drawEnabled nil)))
       ;; Recording the style of the window.
       (setf (slot-value clos-window 'gs:style) style)
       ;; Update the event lists of the window.
       (convert-to-tla-regions theActor width height)
       ;; Install special WDEF, if necessary, and add window-specific information
       (gs:hasWindow! flags)
       (setf (gs:node-window theActor) clos-window)
       (when special-wdef (gs:install-sk8-wdef (wptr clos-window) special-wdef))
       ;; Entering the node and its contents in the window's data structure.
       (when (add-one-node-to-window clos-window theActor flags)
         ;; An error happend since the function returned the actor that crashed!
         (abort-attach-to-stage me theActor)
         (return-from attach-to-stage nil))
       ;; Open the window, if user requested
       (gs:sk8-bring-up-new-window clos-window))
     ))
  t)

;;; DETACH -- Detaches actor from the stage. This involves getting rid of the actor's
;;;          clos window (we know there is one!).

(define-handler detach (Stage theActor)
  (declare (ignore me))
  (when (inheritsfrom theActor Actor)
    (let ((clos-window (gs:node-window theActor))
          deactivateRequired?)
      (when clos-window
        ;; Record whether the window needs to be deactivated. 
        (setf deactivateRequired? (eq clos-window gs:*currentTla*))
        ;; Do the work.
        (without-interrupts
         (unwind-protect 
           (progn 
             (prepareForNewContainer theActor nil)
             ;; Dispose of the window and clear containment field.
             ;; restoring the position info to the rects.
             (revert-to-nontla-regions theActor clos-window)
             ;; Updating containment and disposing window.
             (setf (gs:node-container theActor) nil)
             (gs:hasWindow! (gs:node-flags theActor) 0)
             ;; Telling all nodes removed that they do not have a window.
             (gs:map-nodes* #'(lambda (subActor)
                                (setf (gs:node-window subactor) nil)
                                (leavingStage subActor))
                            theActor))
           ;; Disposing the window.
           (gs:sk8-close-window clos-window)))
        ;; some nasty actors (eg. selectionHalo) set their container to nil on deactivate.
        ;; to avoid problems, make sure this happens after the window has been removed from
        ;; the Stage.
        (when deactivateRequired?
          (deactivate theActor))))))

;;; ---------------------
;;; BASIC EVENT HANDLING
;;; ---------------------

;;; 1.0
;;; HITBYMOUSE -- this method is called from the hit testing loop when the mouse
;;;             sensitivity of the actor is "custom". If this method returns nil
;;;             the actor is assumed to not have been hit and hit testing progresses
;;;             as usual. If this method returns anything else, that thing returned
;;;             is assumed to be the actor that was hit. Thus, this method has to
;;;             return an actor in this case!
;;;
;;; Note that this method will only be called when the mouse is within the bounds region
;;; of the actor. 

(define-handler hitByMouse (actor x y)
  (declare (ignore x y))
  nil)

;;; 1.0
;;; PointOnWhichActor -- returns the actor contained by self which is currently under the mouse.

(define-handler pointOnWhichActor (actor x y &key (deep t))
  (let (h v qd-point obj)
    (sk8-multival-setf (h v) (gs:stage-to-window-coords me x y))
    (setf qd-point (make-point (gs:f.round h) (gs:f.round v))) 
    (if deep
      ;; Call the usual hit testing function.
      (Setq obj (catch :object (gs:sk8-point-in-which-node me qd-point h v)))
      ;; just loop through the immediate children of me.
      (setq obj 
            (mf:dovector-in-reverse (subActor (gs:node-contents me))
              (when (and (visible subActor) (not (gs:mouseInvisible? subActor)))
                (gs:recompute-Bounds-Region subActor (gs:node-flags subActor))
                (when (#_PtInRgn qd-point (gs:node-boundsRegion subActor))
                  (return subActor))))))
    obj))

(defun findValidDropRecipient (candidate)
  (if (or (eq candidate stage) (acceptsDrops candidate))
    candidate
    (findValidDropRecipient (container candidate))))

;;; FindDropObject -- given x and y coords on the stage, it returns the deepest object hit. This is
;;;               used to know that object something was dropped on. This returns the deepest thing
;;;               hit which accepts drops (or the stage if none is found).

(define-sk8-function findDropObject nil (x y &key avoiding)
  (let* ((window-to-avoid (when avoiding
                            (when (gs:hasWindow? (gs:node-flags avoiding))
                              (gs:node-window avoiding))))
         (clos-window (gs:sk8-point-in-which-window (make-point x y) window-to-avoid))
         theActorHit)
    (if (eq clos-window stage)
      stage
      (when clos-window
        (setf theActorHit (slot-value clos-window 'gs:my-actor-object))
        (if avoiding
          (let ((mouseSens (mouseSensitivity avoiding)))
            (unwind-protect 
              (progn 
                (setf (mouseSensitivity avoiding) 'invisible)
                ;; If pointOnWhichActor returns nil, the drop point was over the titlebar of a Mac
                ;; style window. Make sure theActorHit is preserved!
                (setf theActorHit (or (pointOnWhichActor theActorHit x y) theActorHit)))
              (setf (mouseSensitivity avoiding) mouseSens)))
          (setf theActorHit (or (pointOnWhichActor theActorHit x y) theActorHit)))
        (findValidDropRecipient theActorHit)))))

;;;_____________________________________________________________________________________________________
;;;ActorAtHVCoordinates is the same as FindDropObject except it ignores mousesensitivity.

(defun ignore-sensitivity-point-in-which-actor (clos-window qd-where)
  (let ((theActor (slot-value clos-window 'gs:my-actor-object)))
    (setq qd-where (gs:subtract-points qd-where (view-position clos-window)))
    (SK8-multival-bind (x y) (gs:point-to-SK8Coords qd-where)
      (or (catch :object
            (Ignore-sensitivity-point-in-which-node theActor qd-where x y))
          ;; OJO! MouseSensitivity not being considered!
          theActor))))

(defun Ignore-sensitivity-point-in-which-node (theActor where x y)
  (let ((flags (gs:node-flags theActor)))
    ;; When this actor is visible, not mouseInvisible,  and hit...
    (when (and (gs:visible? flags)
               (gs:coords-in-rect-p x y (gs:recompute-physicalBoundsRect theActor)))
      (gs:recompute-bounds-region theActor flags)
      (when (#_PtInRgn where (gs:node-boundsRegion theActor))
        ;; At this point actor has been physically hit. If it has custom mouse sensitivity,
        ;; we call its method to find out who has been hit. The custom method handles
        ;; examining the contents and thus we jump this step in this case.
        (unless (gs:hasNoContents? theActor)
          ;; YES: check each actor in contents.
          (let ((contents (gs:node-contents theActor)))
            (mf:dovector-in-reverse (subActor contents)
              (Ignore-sensitivity-point-in-which-node subActor where x y))))
        ;; if we got here, no subactor was hit. Then this actor is the most
        ;; specific actor hit and we are done. We return it, unless it is mouseTransparent.
        (when theActor
          (throw :object theActor))))))

(define-sk8-function ActorAtHVCoordinates nil (h v)
  (let* ((clos-window (gs:sk8-point-in-which-window (make-point (gs:f.round h) (gs:f.round v)))))
    (when clos-window
      (if (eq clos-window stage) 
        stage
        (Ignore-sensitivity-point-in-which-actor clos-window (make-point (gs:f.round h) (gs:f.round v)))))))


;;;_____________________________________________________________________________________________________
;;;various protocol prototypes.

(define-handler SelectAll (actor) 
  *undefined*)


(define-handler bestSize (actor)
  )

(define-handler idle (actor)
  )

(define-handler mouseWithin (actor)
  )

;;; this is are dummy handlers so that UIWindowColor, which checks for menubar and resizer, won't crash

(define-handler sk8::menubar (actor)
  (declare (ignore me))
  )

(define-handler resizer (actor)
  (declare (ignore me))
  )

;;; menuselect -- popup menu delegates to actor (caller of popup)
;;;

(define-handler menuselect (actor)
  (let ((container (gs:node-container me)))
    (when container (menuselect container))))

(define-sk8-function findCurrentEventActor nil ()
  (sk8-multival-bind (mh mv) (mouseLoc stage)
    (let* ((where (make-point mh mv))
           (sk8-window (gs:sk8-point-in-which-window where)))
      (if (eq sk8-window stage)
        stage
        (gs:sk8-point-in-which-object sk8-window where)))))

(define-sk8-function currentWindow nil ()
  (if (and gs:*currentTla* (gs:sk8-non-floater-p gs:*currentTla*))
    (slot-value gs:*currentTla* 'gs:my-actor-object)
    (let ((candidate (gs:computed-active-window)))
      (when candidate
        (slot-value candidate 'gs:my-actor-object)))))

(define-sk8-function currentFloatingWindow nil ()
  (if (and gs:*currentTla* (gs:sk8-windoid-p gs:*currentTla*))
    (slot-value gs:*currentTla* 'gs:my-actor-object)
    (let ((candidate (gs:computed-active-windoid)))
      (when candidate
        (slot-value candidate 'gs:my-actor-object)))))

;;; trackingMousedown -- called after a mousedown to make the Mac style hiliting work.

(define-handler trackingMousedown (actor)
  (gs:track-mousedown 
   #'(lambda (dh dv)
       (declare (ignore dh dv))
       (let ((newActor (findCurrentEventActor)))
         (if (eq me newActor)
           (unless (highlight me) (setf (highlight me) t))
           (when (highlight me) (setf (highlight me) nil)))))))

(define-handler mouseDown (actor)
  (when (and (eq me (eventActor)) (autohighlight me))
    ;; do the autohighlight
    (progn 
      (setf (highlight me) t)
      (trackingMousedown me)
      (when (highlight me)
        (setf (highlight me) nil)
        (setq gs:*force-click* t))))
  ;; do the usual
  (when (container me)
    (mouseDown (container me))))

;;; DROPPED -- call on actor when droppee is dropped into it. Default just passes the event up the
;;;          containment hierarchy selecting only containers that accept the drop.

(define-handler dropped (actor droppee)
  (let ((acceptingContainer (findValidDropRecipient (container me))))
    (when acceptingContainer
      (dropped acceptingContainer droppee))))

;;; DROP -- this handler is called when the actor is dropped onto something else. The default method
;;;        calls the dropped handler of the recipient of the drop.

(define-handler drop (actor dropRecipient)
  (dropped dropRecipient me))

;;; THESE SHOULD NOT BE PROPAGATED UP THE CONTAINMENT HIERARCHY!!!

(define-handler mouseEnter (actor)
  )

(define-handler mouseLeave (actor)
  )

(define-handler keydown (actor theChar)
  (require-type theChar Character)
  (when (and (autotab me) (char= theChar #\Tab))
    (if (shift-key-p)
      (tabToNextActor me :direction 'backward)
      (tabToNextActor me))))

(define-handler keyup (actor theChar)
  (declare (ignore theChar)))

(define-handler autokey (actor theChar)
  (keyDown me theChar))

;;______________________________________________________________________
;; CLICK
;; The default just passes it up.

(define-handler click (actor)
  (let ((container (container me)))
    (when container
      (click (container me)))))

(define-handler click (stage)
  )

;;______________________________________________________________________
;; DOUBLECLICK
;; The default just passes it up.

(define-handler doubleClick (actor)
  (let ((container (container me)))
    (when container
      (doubleClick (container me)))))

(define-handler doubleClick (stage)
  )

;;; NOTE: this function was "taken out" of the mouseup method because the picker needs to call a
;;;      special function on doubleclick. See the end of the picker file for details!

(defun dispatch-Click (me &optional clickFunction doubleClickFunction)
  (let ((eventTime (eventTime)))
    (case (doubleClickStyle me)
      (sk8::standard
       ;; check for double click
       (if (and (eq me gs:*last-click-object*)
                (< (- eventTime gs:*last-click-time*)
                   (#_GetDblTime)))
         ;; double click
         (progn
           (setq gs:*last-click-time* eventTime
                 gs:*last-click-object* me)
           (funcall (or doubleClickFunction 'doubleClick) me))
         ;; just a single click
         (progn
           (setq gs:*last-click-time* eventTime
                 gs:*last-click-object* me)
           (funcall (or clickFunction 'click) me))))
      (clickOnly
       (setq gs:*last-click-time* eventTime
             gs:*last-click-object* me)
       (funcall (or clickFunction 'click) me))
      (doubleClickOnly
       (if (and (eq me gs:*last-click-object*)
                (< (- eventTime gs:*last-click-time*)
                   (#_GetDblTime)))
         ;; double click
         (progn
           (setq gs:*last-click-time* eventTime
                 gs:*last-click-object* me)
           (funcall (or doubleClickFunction 'doubleClick) me))
         ;; just a click. Register the click object but do not dispatch the event.
         (setq gs:*last-click-time* eventTime
               gs:*last-click-object* me))))))

(define-sk8-function dispatchClick nil (me &optional clickFunction doubleClickFunction)
  (dispatch-click me clickFunction doubleClickFunction))

;;; Returns t if the actor moused is the same actor previously moused and
;;; if the time for a double click has not expired.

(define-sk8-function itsAClick nil (theActor)
  ;; see if the click time is enough
  (let ((result (or gs:*force-click*
                    (and (eq theActor gs:*last-mousedown-object*)
                         (< (- (eventTime) gs:*last-mousedown-time*)
                            (#_GetDblTime))
                         (double-click-spacing-p gs:*event-location*
                                                 gs:*last-mousedown-location*)))))
    (when result
      (setq gs:*force-click* nil))
    result))

(define-handler mouseUp (actor)
  ;; see if the click time is enough
  (when (itsAClick me)
    ;; it was a click, for sure
    (when (eq me (gs:object-under-mouse)) (dispatch-click me)))
  ;; Propagate up the containment hierarchy!
  (let ((theContainer (gs:node-container me)))
    (when theContainer
      (mouseup theContainer))))

(define-handler bringUp (actor)
  (when (eq me (sk8::window me))
    (without-interrupts  
     ;; This is needed to protect the user from doing something 
     ;; like bringing up a window then immediately 
     ;; detaching it before bringtofront happens.
     (unless (container me)
       (setf (container me) stage))
     (bringToFront me)  ;;We bring up the actor even if it is on the stage already.
     (unless (visible me)
       (%put-word (%int-to-ptr #x9dc) 0)
       (show me)))
    t))

;;--------------------------------------------------------------------------
;;BJR:  Some useful functions for moving around and aligning actors on the screen

(define-handler moveOffstage (actor)
  (sk8-multival-bind (ll tt rr bb) (boundsrect stage)
    (declare (ignore ll tt))
    (sk8-multival-bind (ww hh) (size me)
      (setLocation me (+ 1000 rr ww) (+ 1000 bb hh) :physical t))))

(define-handler moveOnstage (actor)
  (let (ll tt rr bb)
    (sk8-multival-bind (sll stt srr sbb) (boundsrect (if (container me)
                                                       (container me)
                                                       stage) :physical t)
      (sk8-multival-bind (all att arr abb) (boundsrect me :physical t)
        (setf ll all
              tt att
              rr arr
              bb abb)
        (when (> arr srr)
          (setf ll (- ll (- arr srr)))
          (setf rr srr))
        (when (< all sll)
          (setf ll sll)
          (setf rr (+ rr (- sll all))))
        (when (> abb sbb)
          (setf tt (- tt (- abb sbb)))
          (setf bb sbb))
        (when (< att stt)
          (setf tt stt)
          (setf bb (+ bb (- stt att))))
        (unless (and (= all ll) (= att tt) (= arr rr) (= abb bb))
          (setBoundsRect me ll tt rr bb :physical t))))))



;;--------------------------------------------------------------------------
;;; CUT/COPY/PASTE/CLEAR

(define-handler copySelectionToClipboard (actor)
  )

(define-handler cutSelectionToClipboard (actor)
  )

(define-handler pasteClipboardToSelection (actor)
  )

(define-handler clearSelection (actor)
  )

(define-handler copySelectionToClipboard (String)
  (put-scrap :text me))

;;; logicalToPhysical -- returns the physical counterparts of h and v which are coords in the
;;;                logical system of the actor.

(define-handler logicalToPhysical (actor h v)
  (let ((container (gs:node-container me)))
    (if (or container (gs:hasWindow? (gs:node-flags me)))
      (sk8-multival-bind (hh vv)
                         (gs:real-logical-to-physical me h v)
        (gs:window-to-stage-coords me hh vv))
      (sk8-multivals h v))))

(define-handler logicalToPhysicalRect (actor h1 v1 h2 v2)
  (gs:logical-to-user-rect-coords me h1 v1 h2 v2))

(define-handler logicalToPhysicalList (actor coordsList)
  (gs:logical-to-stage-list me coordsList (gs:node-container me)))

;;; physicalToLogical -- returns the logical counterparts of h and v which are coords in the
;;;                 global system. The coords returned are the equivalent coords in the
;;;                 logical system of the actor.

(define-handler physicalToLogical (actor h v)
  (sk8-multival-bind (hh vv) (gs:stage-to-window-coords me h v)
    (gs:real-physical-to-logical me hh vv)))

(define-handler physicalToLogicalRect (actor h1 v1 h2 v2)
  (gs:user-to-logical-rect-coords me h1 v1 h2 v2))

(define-handler physicalToLogicalList (actor coordsList)
  (gs:stage-to-logical-list me coordsList))

(define-handler sk8::close (actor)
  (setf (container me) nil))

(define-handler draggingMouseEnter (actor actorDragged)
  (declare (ignore actorDragged))
  nil)

(define-handler draggingMouseLeave (actor actorDragged)
  (declare (ignore actorDragged))
  nil)

(define-handler draggingMouseWithin (actor actorDragged)
  (declare (ignore actorDragged))
  nil)

(define-handler draggingMouseEnter (stage actorDragged)
  (declare (ignore actorDragged))
  nil)

(define-handler draggingMouseLeave (stage actorDragged)
  (declare (ignore actorDragged))
  nil)

(define-handler draggingMouseWithin (stage actorDragged)
  (declare (ignore actorDragged))
  nil)

;;; DragRubberBand -- this function lets the user drag a line on the stage starting at the point
;;;               specified by hanchor and vanchor (which default to the event location).
;;;               When done, the 2 end points are returned (in stage coords). The idea is that
;;;               the caller can do whatever it wants with the result. A typical course of
;;;               action is to find what object the end coords were on and connect it to
;;;               the source of the band (which the caller knows).

(define-sk8-function dragRubberBand nil (&key (hanchor (gs:f.round (eventH)))
                                                 (vAnchor (gs:f.round (eventV)))
                                                 (width 3)
                                                 (height 3))
  ;; error checking the pen size: positive integers only!
  (setf width (gs:f.round width)
        height (gs:f.round height))
  (unless (plusp width) (setf width 3))
  (unless (plusp height) (setf height 3))
  ;; Ready? Go!
  (let ((endx hanchor) (endy vAnchor) oldx oldy)
    (rlet ((old-pen :penstate))
      (unwind-protect
        (progn
          (with-port (gs:get-wmgr-port)
            (#_getpenstate old-pen)
            (#_PenSize width height)
            (#_PenMode #$patxor)
            (#_MoveTo hanchor vAnchor)
            (#_LineTo hanchor vAnchor)
            (loop
              (unless (mouse-down-p) (return))
              (setf oldx endx oldy endy)
              (SK8-multival-setf (endx endy) (mouseLoc stage))
              (unless (and (= oldx endx) (= oldy endy))
                (with-port (gs:get-wmgr-port)
                  ;; Reset port state. 
                  (#_PenSize width height)
                  (#_PenMode #$patxor)
                  ;; erase
                  (#_MoveTo hanchor vAnchor)
                  (#_LineTo oldx oldy)
                  ;; draw
                  (#_MoveTo hanchor vanchor)
                  (#_LineTo endx endy))))))
        ;; erase the line
        (with-port (gs:get-wmgr-port)
          (#_PenSize width height)
          (#_PenMode #$patxor)
          (#_MoveTo hanchor vAnchor)
          (#_LineTo endx endy)
          (#_SetPenState old-pen))))
    ;; return the end point.
    (SK8-multivals endx endy)))

;;; Defining this handler lets the collection protocol operate on the actor's text.

(define-handler |TEXT FOR MODS| (actor)
  (text me))

;;; _______________________________ 
;;; Access to the draw function.
;;; _______________________________ 

(define-handler drawFunction (Actor)
  (gs:node-drawFunction me))

(define-handler (setf drawFunction) (drawFunction Actor)
  (if (and (symbolp drawFunction)
           (fboundp drawFunction))
    (setf (gs:node-drawFunction me) drawFunction)
    (error "Draw function should the name of an existing function.")))
  
(define-sk8-function drawActorContents nil (theActor thePaper drawRegion &key (renderTheFill t))
  ;; Clipping to the fill area if necessary and drawing subActors.
  (gs:let+ ((clip-region (:region))
         (fill (gs:node-fillRegion theActor)))
    (gs:recompute-fill-region theActor (gs:node-flags theActor))
    (#_sectRgn fill drawRegion clip-region)
    (gs:with-clipped-region clip-region
      (funcall (gs:node-contentsDrawFunction theActor) theActor thePaper clip-region fill renderTheFill))
    ))

;;; _______________________________ 
;;; Stuff that was defined all over the system.
;;; _______________________________ 

(define-sk8-function eventWindow nil ()
  (let ((eventActor (eventActor)))
    (when eventActor
      (sk8::window eventActor))))

(define-handler discard (actor)
  ;; remove it from its container
  (setf (container me) nil)
  (call-next-method))

;;; CommandKey --  default handler for actor ignores the commandKey.

(define-handler commandKeyEvent (actor theChar)
  (when (container me)
    (commandKeyEvent (container me) theChar)))

(define-handler discard (actor)
  ;; remove it from its container
  (setf (container me) nil)
  (call-next-method))

;;; 1.0
;;; PRESERVE ACTOR -- throws away the regions all actors have.

(defun preserve-actor (me)
  (let ((flags (gs:node-flags me)))
    ;; If the actor caches its pixmap, uncache it without clearing the cachesPixmap flag.
    (when (gs:cachesPixmap? flags)
      (gs:uncache-pixmap me :force t))
    ;; Disposing the regions, just in case.
    (setf (gs:node-boundsRegion me) nil)
    (setf (gs:node-fillRegion me) nil)
    (setf (gs:node-frameRegion me) nil)
    ;; Make the actor dirty!
    (gs:physicalBoundsRectDirty! flags)
    (gs:boundsDirty! flags)
    (gs:fillDirty! flags)
    (gs:frameDirty! flags)
    ;; Owned Region stuff.
    (when (gs:ownedRegion? flags)
      (remf (gs:node-properties me) :ownedRegion))
    ;; Saving the actor's window.
    (when (gs:hasWindow? flags)
      (gs:set-preserved-info me (gs:preserve-clos-window me)))
    (when (gs:node-window me)
      (setf (gs:node-window me) nil)
      (leavingStage me))))

(define-handler preserve (Actor)
  (preserve-actor me))

;;; 1.0
;;; RESTORE ACTOR -- restore method for actor. Rebuilds the regions the actor uses, updates the window
;;;                 field of the node with the newly created window. It ends by calling restore
;;;                 on all contained actors.

(defun restore-actor (me)
  (let ((flags (gs:node-flags me)))
    ;; Setting the fields.
    (setf (gs:node-boundsRegion me) (T_NewRgnGC))
    (setf (gs:node-fillRegion   me) (T_NewRgnGC))
    (setf (gs:node-frameRegion  me) (T_NewRgnGC))
    (gs:boundsDirty! flags)
    (gs:frameDirty!  flags)
    (gs:fillDirty!   flags)
    ;; Creating a window if necessary and reseting the window field.
    (if (gs:hasWindow? flags)
      (setf (gs:node-window me) (gs:restore-clos-window me))
      (let ((container (gs:node-container me))
            container-window)
        (when container
          (setf container-window (gs:node-window container))
          (when container-window
            (setf (gs:node-window me) container-window)
            ))))
    ;; Restore all items contained.
    (dovector (contained (gs:node-contents me))
      (restore-actor contained))
    #|
      (when (gs:node-window me)
        (forceredraw me)  ;; for some reason, some windows need this here
        (enteringStage me))
|#
    ;; When the actor caches its pixmap, cache the bits!
    (when (gs:cachesPixmap? flags)
      (gs:cache-pixmap me :force t))))

(define-handler restore (Actor)
  ;; top level restore of Actors. Restore if a menu with a submenu, an actor with no container, or an actor on stage with no window
  ;;  anything else will be restored by its container
  (let ((contner (container me)))
    (when (or (and (inheritsFrom me Menu) (menu me))
              (null contner)
              (and (eq contner Stage) (null (gs:node-window me))))
      (restore-actor me))))

;;; _________
;;; Changing parents stuff.
;;; _________

;;; Returns the first sublist whose head is an actor.

(defun find-next-actor (theObjects)
  (loop 
    (when (or (null theObjects) (inheritsFrom (car theObjects) actor))
      (return-from find-next-actor theObjects))
    (setf theObjects (cdr theObjects))))

;;; Returns true if the new parents and old parents have the same actors in it and in
;;; the same order.

(defun check-change-in-actor-parents (me newParents)
  (let ((oldParents (parents me)))
    (loop
      (setf oldParents (find-next-actor oldParents))
      (setf newParents (find-next-actor newParents))
      (cond ((and (null oldParents) (null newParents)) 
             (return-from check-change-in-actor-parents nil))
            ((neq (car oldParents) (car newParents)) 
             (return-from check-change-in-actor-parents t)))
      (setf oldParents (cdr oldParents))
      (setf newParents (cdr newParents)))))

(defmacro actor-refresh (me &body body)
  ;; Yes: do actor refresh.
  `(if (gs:hasWindow? (gs:node-flags ,me))
     (progn (hide ,me)
            ,@body
            (when (inheritsFrom ,me Actor)
              (show ,me)))
     (progn (hide ,me)
            ,@body
            (when (inheritsFrom ,me Actor)
              (forceRedraw ,me)
              (show ,me)
              ))))

;;; There is a conflict when one of the newActorParents is an ancestor of one of the old
;;; actor parents.

(defun actor-change-parents-conflict? (oldActorParents newActorParents)
  (dolist (c newActorParents nil)
    (when (some #'(lambda (x) (inheritsFrom x c)) oldActorParents)
      (return-from actor-change-parents-conflict? t))))

;;; ChangeParents of Actor.

;;; The problem with changing parents is that all the parents are removed and then
;;; all the new parents are added. Thus, any info that old parents had would be lost
;;; by the time we are done. This is a problem when changing the parents from {Rectangle}
;;; to {oval}. In this case we would like to preserve things like the text, the boundsRect,
;;; etc. But by the time our actor becomes an oval it has already forgotten all its actor
;;; properties.

;;; The solution is to change this method to add all the new actor parents and then
;;; remove the old ones. There is a problem when a new parent is an ancestor of an old
;;; parent. In this case we just loose the information.

;;; oldParents, we know
;;; newParents, we get
;;; if newParents contains an actor then
;;;    (1) do non actor part: remove all old non actor parents and add new non actor parents.
;;;    (2) add new actor parents and then remove old actor parents. 
;;; else
;;;    do inherited. 

(define-handler changeParents (actor newParents)
  (let ((actorParentsChanged? (check-change-in-actor-parents me newParents))
        (actorBaseParent (find-if #'(lambda (x) (inheritsFrom x Actor)) newParents)))
    (if (and actorParentsChanged? actorBaseParent)
      ;; If there is a change in actor parents AND the thing will still be an actor after the change.
      (let* ((oldParents (parents me))
             (oldOtherParents (remove-if #'(lambda (x) (inheritsFrom x Actor)) oldParents))
             (oldActorParents (set-difference oldParents oldOtherParents))
             (newOtherParents (remove-if #'(lambda (x) (inheritsFrom x Actor)) newParents))
             (newActorParents (set-difference newParents newOtherParents)))
        ;; Make sure the draw function is updated correctly.
        (change-draw-function-for-new-parent actorBaseParent me)
        ;; If there are conflicts, call next method (loosing actor info).
        (if (actor-change-parents-conflict? oldActorParents newActorParents)
          (actor-refresh me (call-next-method))
          ;; No conflicts.
          (progn
            ;; Remove unwanted other parents.
            (dolist (c oldOtherParents)
              (unless (memq c newOtherParents) (removeParent me c)))
            ;; Add new other parents.
            (dolist (c newOtherParents)
              (unless (inheritsFrom me c) (addParent me c)))
            ;; Add new actor parents. Then remove old ones.
            (actor-refresh me
                           (dolist (c newActorParents) (addParent me c))
                           (dolist (c oldActorParents) (removeParent me c))))))
      (call-next-method))))

;;; Makes sure an actor is using the right drawfunction.

(defun select-correct-actor-draw-function (me)
  (if (not (zerop (length (gs:node-contents me))))
    (if (zerop (length (text me)))
      'gs:sk8-subactors-draw
      'gs:sk8-full-draw)
    (if (zerop (length (text me)))
      'gs:sk8-simple-draw
      'gs:sk8-text-draw)))

(defun change-draw-function-for-new-parent (newDad me)
  ;; Make sure the draw function is right...
  (let ((dadsFunction (gs:node-drawFunction newDad)))
    (if (memq dadsFunction '(gs:sk8-full-draw gs:sk8-subactors-draw
                             gs:sk8-text-draw gs:sk8-simple-draw))
      (setf (gs:node-drawFunction me) (select-correct-actor-draw-function me))
      (setf (gs:node-drawFunction me) dadsFunction))))

;;; New parent initialize stuff.

(defun initialize-for-new-actor-parent (newDad me)
  (unless (some #'(lambda (x) (inheritsFrom x actor)) (remove newDad (parents me)))
    (initialize-struct newDad me))
  (change-draw-function-for-new-parent newDad me))

(define-handler AddedMeAsParent (actor child oldparents)
  (declare (ignore oldParents))
  ;; (unless (some #'(lambda (x) (inheritsFrom x actor)) oldparents)
  (initialize-for-new-actor-parent me child)
  (forceRedraw child))

;;; A mess! Dispose node records. Make sure the actor is not in any container any more. Also 
;;; Remove all its contents from itself!

(define-handler removingMeAsParent (actor child newparents)
  (let ((stillAnActor? (some #'(lambda (x) (inheritsFrom x actor)) newparents)))
    (unless stillAnActor?
      ;; Remove it from its container if any.
      ;; Removes from the container WITHOUT asking for permission!!!
      (let ((container (gs:node-container child)))
        (cond (container
               ;; detaching from container if any.
               (detach container child))
              ((gs:hasWindow? (gs:node-flags child))
               ;; if this actor has a window, we get rid of it.
               ;; (detach-from-stage stage child)
               (setf (container child) nil)
               ))
        (gs:leave-object-under-mouse-if-in child)
        (gs:dispose-node-records child)
        ;; Remove all contents from the child, since it will no longer be an actor.
        (dolist (c (contents child))
          (when (realproperty c 'container)  ;; bulletproofing
            (setf (container c) nil)))
        (setf (gs:node-contents child) nil)))
    ;; Force the draw function to be recomputed from the remaining actor parent.
    (let ((remainingActorParent (find-if #'(lambda (x) (and (inheritsFrom x Actor)
                                                            (neq x me))) 
                                         newParents)))
      (when remainingActorParent
        (initialize-for-new-actor-parent remainingActorParent child)
        (forceRedraw child)))
    ))

;;; _______________________________ 
;;; Editable properties. 
;;; _______________________________ 

(define-handler localVirtualProperties (Actor)
  (when (eq me  Actor)
    '(acceptsDrops autohighlight autotab bottom boundedbycontents boundsrect cachespixmap 
      colordepth dofirstClick draggable inverts floating
      h highlight layer left location locked locklevel mousesensitivity origin 
      resizable resizesContents right scale physicalScale size text textcolor textfont textlocation 
      textsize textstyle textOffset textHOffset textVOffset top v visible wantsidle
      wantsmousewithin)))

;;; _______________________________ 
;;; Media sampling on actors. 
;;; _______________________________ 

;;; Handlers defined on media objects to allow them to be sampled on some
;;; actor.

(define-handler displaySample (Media theActor)
  (declare (ignore theActor))
  )

;;; This function is used by all the image renderer media to show themselves in an actor.

(define-sk8-function showMediaInActor nil (imageRendererMedia theActor)
  (let ((theColor (fillcolor theActor)))
    (if (inheritsFrom theColor ImageRenderer)
      (progn 
        (setf (media theColor) imageRendererMedia)
        (lightForceRedraw theActor))
      (let ((newColor (new imageRenderer :project (project theActor)
                           :media imageRendererMedia)))
        (setf (fillcolor theActor) newColor)))))

(define-handler displaySample (QDPicture theActor)
  (showMediaInActor me theActor))

(define-handler displaySample (ColorPattern theActor)
  (showMediaInActor me theActor))

(define-handler displaySample (BWPattern theActor)
  (showMediaInActor me theActor))

(define-handler displaySample (IconRSRC theActor)
  (showMediaInActor me theActor))

;; handler that causes containment in stage to be mentioned in the print string of an unnamed actor
(define-handler find-relation-or-pseudo-relation :hidden (Actor)
                (multiple-value-bind (relatedObject relationProperty) (mf::find-relation me t)
                  (cond
                   (relatedObject
                    (values relatedObject relationProperty))
                   ((eq (container me) Stage)
                    (values Stage 'contents))     
                   (t
                    (let* ((parent (baseParent me))
                           (kc (when (realProperty parent 'knownChildren)
                                 (getValue 'knownchildren parent))))
                      (when (memq me kc)
                        (values parent 'knownChildren)))))))

(define-handler collectionlike (Actor)
  (declare (ignore me))
  nil)


#|
	Change History (most recent last):
	2	5/11/93	Hernan	
	3	5/21/93	Hernan	withVisualEffect still locks and unlocks the window when an invalid 
						visualEffect is passed.
	4	5/21/93	Hernan	Any actor can now be attached to the stage!
	5	5/25/93	Hernan	Removed extraneous beep from set boundsrect.
	6	5/28/93	Hernan	Trying to make the menus work...
	7	6/1/93	Hernan	(1) Changed mouseH and mouseV to hMouse and 
						vMouse, (2) setBoundsRect now reports an error
						when you try to set a size below the minimumSize.
	8	6/1/93	Hernan	Removed want-mouseWithin slot of *sk8-window*.
	9	6/1/93	Hernan	SetBoundsRect no longer returns an error when the minimumsize
						is to be transgressed. (Told you so!)
	10	6/3/93	Hernan	(1) setf style only lets you set the style of 
				    	 rectangles.
						(2) Resize takes other actors and minimizes use
				     	of the case statement.
	11	6/3/93	Hernan	dragActors now resets the pen to xor and gray pattern ALL the time 
						(deals with some problem dragging movies).
	12	6/4/93	Hernan	Dragging on stage now also can generate drop events.
	13	6/8/93	Hernan	Added directional connector functionality and 
						a check to connect to stop the user from
						connecting an actor to itself.
	14	6/10/93	Hernan	An actor that has connectors, hides the connectors
						when its visible is set to false. The connectors are
						again shown when the actor becomes visible.
	15	6/11/93	Hernan	the after argument in attach can now be a number. 
						In that case it is interpreted as the desired position of the actor
						in the vector.
	16	6/14/93	Hernan	Weird non live resize bug fixed.
	17	6/15/93	Hernan	Removed silly beep from resize!
	18	6/15/93	Hernan	Resized and moved are sent only if gs:*events-on*
						is set (not that weird other variable!!!)
	19	6/21/93	chip	Got rid of tick
	20	6/22/93	Hernan	Layering functions now return true (instead of the node).
	21	6/22/93	Hernan	Set container now checks whether the new container
						is contained by the actor. Also, errors are now reported.
	22	6/22/93	Hernan	Added 3 pixels to the offset of the text when the 
						textLocation is xxxLeft. This separates the text from
						the edge of the actor.
	23	6/22/93	Hernan	(1) Added a force keyword to unlock which sets the lockLevel to nil.
						(2) You can now also get draggedOverEvents when
						dragging within the same container. (does not work for 
						live dragging though).
	24	6/22/93	Hernan	Moved actorTextSize to here! Everyone uses it.
	25	6/23/93	Hernan	BindByContents is now a handler, so that it can be
						redefined for the groupActor.
	26	6/24/93	Hernan	Setf Number now returns the number that was
						given to it (no more node list).
	27	6/24/93	Hernan	Second try to check in!
	28	6/25/93	Hernan	The Great Renaming of 93.
	29	6/25/93	Hernan	InitializeObject -> initialize.
	30	6/25/93	Hernan	Ooops, a title call was left behind...
	31	6/28/93	Hernan	Connectors cannot connect things on the Stage and
						the error message now states that when the user tries to connect them.
	32	6/28/93	Hernan	When attaching to the stage, only ask for the windowTitle
						of rectangles.
	39	7/28/93	chip	made DISPOSE for Actor null out the node's node-actor, and the actor's struct slot
	40	8/17/93	kleiman	withactorlocked define-sk8-macro -> defmacro
	41	8/18/93	kleiman	WITHACTORLOCKED made public (hopefully)
	42	8/27/93	chip	fixed 'withActorLocked' definition (was missing the NIL to signify "private")
	43	8/31/93	hernan	Integrating old patches...
	44	8/31/93	hernan	Integrating old patches... Also removed all drag
						functionality to the drag.lisp file.
	45	8/31/93	hernan	Changed detach to support the new field.
	46	8/31/93	hernan	Changed actorTextSize to make it always return
						a sensible result (no more 100 by 100s).
	47	9/1/93	hernan	The great integrating d4 build!
	48	9/16/93	hernan	We no longer allow locked TLA's to change size.
	49	9/17/93	hernan	Making (setf text) transform things given to it
						that are not strings using objectString.
	50	9/20/93	hernan	Making text properties redraw the actor even
						when there is no text (makes pickers happy).
	51	9/22/93	hernan	Fixing detach (of Stage) so that the low level field clears its
						window information.
	52	9/22/93	kleiman	initialize no longer creates node struct
	53	9/24/93	hernan	Adding dropped event and fixing drag events.
	54	9/29/93	hernan	added previous node slots as actor properties
	55	10/1/93	hernan	==============================
						= Sad but true: the node is gone!!! =
						==============================
	56	10/1/93	hernan	Forgot the window slot in actor.
	57	10/4/93	kleiman	initialize calls next method at the start among other things.
	58	10/5/93	hernan	Added fast slot block and
						with-fas-slots -> with-fast-node-slots.
	59	10/5/93	hernan	Moved the node block slot definition to macros.lisp
	60	10/5/93	hernan	Calling gs:node-container instead of container 
						in several places where it matters,
	61	10/6/93	hernan	Fixed set container to allow detachments (???)
	62	10/6/93	hernan	Fixing the set floating handler to avoid infinite
						loops while selecting windows.
	63	10/6/93	kleiman	with-fast-slots -> with-fast-node-slots.
	64	10/8/93	hernan	New attach/detach protocol added.
	65	10/8/93	hernan	Top level actors now resize when their locklevel
						is less than 2.
	66	10/11/93	hernan	Fixing bug in setf-tla-boundsRect: the locklevel
						can be false!
	67	10/15/93	hernan	Making cut/copy/paste of actor do nothing.
	68	11/1/93	hernan	gs:inheritsFrom? -> inheritsFrom.
	69	11/3/93	rod	
	70	11/10/93	chip	tabToNextActor no longer selects all the text of fields it activates
	71	11/10/93	chip	added deeplyVisible handler
	72	11/10/93	chip	tabToNextActor now makes sure it tabs to a visible actor!
	73	11/19/93	hernan	ActorTextSize now uses our own port to do its 
						stuff (we suspect that using MCL's temp port is
						not a good idea.
	74	11/19/93	hernan	Updated the ancient dispatch-click function to 
						work with the current concept of eventActor and
						object-under-mouse. (See clos-glue comments 
						for details).
	75	11/19/93	hernan	Making setOrigin do nothing if the window is 
						locked. Also enclosed the with-port part of
						ActorTextSize in a without-interrupts.
	76	11/22/93	hernan	ActorTextSize restores the font, size and face of
						its port. (This seems to keep the window manager's
						port happy).
	77	11/22/93	hernan	Autotab now works with the containment hierarchy!
	78	11/24/93	hernan	Added set-text-state (a function that given an
						actor sets the pen's text stuff in the right way).
	79	11/29/93	kleiman	initialize[Actor] sets container's contents for DUPLICATE
	80	11/29/93	hernan	Using let+ to get the ports.
	81	11/29/93	hernan	When a new actor is made, initialize makes sure
						the window and container slots are cleared!!!
	82	11/30/93	hernan	Making prepareForStage of Actor preserve and
						restore any translucent colors the actor may be
						using to render itself.
	83	11/30/93	hernan	Ooops! (wrong comment) The work is done by the
						set container handler of actor. This way it cannot
						be clobbered by the user.
	84	12/1/93	hernan	Making actorTextSize restore the font of the 
						system (just in case...)
	85	12/1/93	hernan	Avoiding use of font-info in actorTextSize. This
						function might have something to do with the
						font problem we have been having (it messes 
						with MCL's temporary port in strange ways).
	86	12/1/93	hernan	Declaring unused variables.
	87	12/1/93	hernan	sk8-select-old-window is now told it is called 
						indirectly from bringToFront.
	88	12/2/93	kleiman	dispose of Actor: diposed check BEFORE anything else
	89	12/2/93	hernan	Removing obsolete stuff from attach-to-stage.
	89	12/2/93	hernan	Second try!
	89	12/2/93	hernan	Got rid of obsolete stuff in attach-to-stage.
	90	12/7/93	hernan	Setf Visible does the same stuff that is done when
						windows are brought up (calls sk8-bring-up-new-window).
						It was not possible to just window-show the
						window because it might have changed size in the
						meantime.
	91	12/10/93	hernan	Fixing (setf floating) to bring the floaters to the
						top.
	92	12/17/93	hernan	Fixing set Size so that setting the physical size
						yields the proper result.
	93	12/21/93	hernan	Error checking trap stuff.
	94	12/21/93	sidney	Backquotes back in so we can compile the file
	95	1/5/94	hernan	Deepcontents of an actor does no longer return
						the actor itself.
	96	1/7/94	sidney	big mf/ss merge
	97	1/10/94	sidney	Bug in fix to deepcontents: Since it no longer returns self, the recursion has to be a little different.
	98	1/10/94	hernan	Making a function that gives us the font number
						given the font name. And fonts became objects!
	99	1/10/94	hernan	Moving descendants to 01-object.
	100	1/11/94	hernan	self -> me
	101	1/11/94	hernan	More self -> me.
	102	1/14/94	hernan	Added newParentInitialize stuff (commented out
							until we figure it out).
	103	1/14/94	hernan	Adding the newParentInitialize stuff.
	104	1/14/94	kleiman	Taking that stupid newParentInitialize out!
	105	1/17/94	hernan	Removing newParentInitialize stuff out of here 
							and into its own file.
	106	1/31/94	hernan	Moved actorsBounds to here.
	107	2/1/94	hernan	Making setf-tla-boundsrect send moved or 
							resized according to the value of justMoving (like
							all other setBoundsRects do!).
	108	2/12/94	kleiman	name changes
	109	2/14/94	rod	Fixed attach-to-stage.  For some reason, it was
							not calling detach unless the actor had a 
							node-window.  Now it always calls detach like
							it should (or else contents has bad info left in it).
	110	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	112	2/25/94	hernan	Using symbols instead of Keywords for options!!!
	113	2/28/94	sidney	Don't supress resizing  when events are turned off because things
							count on resizing happening even when SK8 is in the background
	114	2/28/94	hernan	Still getting rid of keywords...
	115	2/28/94	hernan	Fixing leftover keyword stuff.
	116	2/28/94	hernan	Deactivate is no longer responsible for telling
							the system whether detachment from the stage
							is ok!
	117	2/28/94	hernan	Now using GCable mac pointers and handles!
	118	3/1/94	kleiman	handle some pointers correctly
	119	3/1/94	rod	fixed quoted property in property spec
	120	3/2/94	Hernan	Porting to Fred 3.0
	121	3/3/94	kleiman	update-monitors-for-new-depth moved here
							from 07-device.lisp since node-window macro
							is not defined until graphics load sequence
	122	3/3/94	Hernan	The great handler argument name renaming of 94!
	123	3/3/94	kleiman	:private -> addproperty
	124	3/3/94	kleiman	private properties declared via makeprivateproperty
	125	3/4/94	kleiman	font-num -> fontData
	126	3/6/94	Hernan	Added coordinate translation functions for lists
							and rects.
	127	3/6/94	hernan	Patching initialize to stop assuming that original
							holds the actor. We need to look for the right
							parent too.
	128	3/10/94	Hernan	Obsoleting myWindows.
	129	3/12/94	dy		NowInNewWindow event called from attach-to-actor - temp hack?
	130	3/14/94	Hernan	Making set container return the right thing.
	131	3/14/94	Hernan	Removing nowInNewWindow. EnteringStage and
							leavingStage are enough!
	131	3/14/94	Hernan	Second try!
	132	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	133	3/16/94	chip	Actor no longer needs its own print-object
	134	3/16/94	Hernan	Fixing lots of things to allow dirtying of the
							physical as well as the logical rect.
	135	3/16/94	Hernan	Fixing more and more...
	136	3/16/94	Hernan	Rolling back to yesterday to undo all the mess I
							was doing (and what great fun that was!)
	137	3/17/94	Hernan	Messed with set-window-rectangle to get rid of
							the annoying whiting of other windows and the 
							garbage that is left on the stage when covered.
	138	3/18/94	Hernan	Construct-rect-cache was not working right when
							the width or height of the rect to be resized was 0.
	139	3/24/94	Hernan	Checking out why setting the floating to false
							flashes the window.
	140	3/26/94	rod	Changed SetBoundsRect and Resize so that 
							minimum size is handled a bit more nicely.  Only
							changed these two functions.
	141	3/26/94	Brian	Whoops, fixing it so resizing works from any
							corner.  (it may look complicated, but it's not.)
	142	3/28/94	Hernan	Fixing setOrigin to correctly set the
							port before blitting.
	143	3/28/94	Hernan	Fixing bringToFront to now show a window that is
							hidden.
	144	3/28/94	Hernan	Now that we have windoids, checking if something
							is the front window is not enough to know if it is
							the currentWindow. We should check instead the
							value of the gs:*currentWindow* variable and 
							we do (in bringToFront).
	145	3/29/94	rod	OK, OK, now resize non live works properly. :-)
	146	3/29/94	Hernan	Making the fields in a window redraw themselves
							when the window's boundsRect changes. This
							ensures they get redrawn when they come back 
							into a visible area (when they were off stage).
	147	3/30/94	Hernan	In setOrigin we only set the port to the wmgr 
							when we have to blit from gWorld to Window.
	148	3/31/94	Hernan	Removed code in setWindowRect that suposedly
							fixed the halo garbage problem. Since it did not, 
							I removed it from now.
	149	3/31/94	Hernan	Making setf inverts return the right thing.
	150	4/1/94	Hernan	Added all sorts of stuff to protect the user from
							his mistakes while attaching or detaching. This
							means that if your enteringStage, leavingStage,
							prepareForNewContainer and prepareForNewContent
							crash, the system gracefully recovers. It will let
							you look at your handler and it will clean up when
							you abort.
	151	4/2/94	kleiman	(setf container) is a virtual property, but it did not
							ensure that the container slot is set to NIL when
							the stage is the actor's container.
	152	4/2/94	NIL	in (setf container): removed optimizing check
							for setting the same container because during
							actor initialization the container is the one copied
							from the child's parent, yet the virtual property
							has not been called.
	153	4/3/94	kleiman	(setf container) default initialization is forced
							to FALSE when CONTAINER with a value of Stage
							is an initarg in a NEW or COPY of an actor
	154	4/4/94	kleiman	
	155	4/5/94	Hernan	Who the heck commented out setf contents???
							Adding it back to the system.
	156	4/5/94	kleiman	moved draggrubberband here from connector.lisp
	157	4/6/94	Hernan	Unmarking some properties private.
	158	4/8/94	Hernan	Making setWindowRect redraw the stage when it
							is done. This should solve the strange garbage left
							behind when a halo or a line moves.
	159	4/12/94	Hernan	Avoiding use of contents when not necessary.
	160	4/13/94	Hernan	Adding hasContents, a handler that returns t if
							the actor contains anything.
	161	4/13/94	Hernan	MouseEnter and MouseLeave cannot be propagated
							up the containment hierarchy anymore.
	162	4/15/94	Hernan	Making methods return the right things.
	163	4/15/94	Hernan	Fixing bindByContents to deal with the case when
							the boundedByContents guy is the tla. In the
							process found and fixed an ugly bug in setf-tla-boundsrect.
	164	4/19/94	sidney	define a null handler for menubar of actor so UIWindowColor 
							won't crash when it checks for a menubar
	165	4/19/94	sidney	define a null handler for resizer of actor so UIWindowColor 
							won't crash when it checks for resizer
	166	4/22/94	Hernan	Turned some actor functionality into mixins and 
							put it in its separate file.
	167	4/25/94	Hernan	Adding functions to let the graphics system know
							that the regions are ok.
	168	4/26/94	Hernan	I do not think the world is ready for my great
							optimization to set-rectangle-internal.
	169	4/27/94	Hernan	Nothing, just formating some text.
	170	4/28/94	chip	moving the low-level setting of the 'curkey' slot up in (setf keyTarget)
	171	4/28/94	Hernan	Making all our windows be single-edge-box
							before we install our own wdef. This will speed
							up window creation.
	172	5/11/94	Hernan	Removed leftover keyword options from
							pointOnWhichPart.
	173	6/7/94	Hernan	SendToBack no longer makes the window disapear 
							behind the covered stage.
	174	6/10/94	Hernan	1166407: tool windows need to be resized using
							something other than zoomWindow. Implemented
							a special method for them.
	175	6/10/94	Hernan	Ooops. Forgot print statements...
	176	6/13/94	Hernan	attach-to-stage should only deal with the window
							style when the actor to be attached is a rectangle.
	177	6/16/94	sidney	bulletproof detaching of actor from container
	178	7/5/94	Hernan	1172190: fixing disconnect to get rid of the 
							connector (the default). Adding an argument for 
							this purporse.
	179	7/6/94	Hernan	1169269: replacing the eventX variables with
							functions.
	180	7/15/94	dy		set fillColor calls set fillColorActor of Renderer
	181	7/18/94	Hernan	1172685: Relaxing all of the restrictions
							connectors had.
	182	7/18/94	dy		change "set textcolor" because textcolor is sometimes false
	183	7/20/94	Hernan	1175224: initializing actor to have the right draw
							functions.
	184	7/22/94	Hernan	Allowing connect to connect two windows.
	185	7/25/94	Hernan	1175911: making actors remember their keyTarget
							when they are not on the Stage.
	186	7/25/94	Hernan	1175873: tied in the cachesPixmap property to the
							colorDepth of windows.
	187	7/25/94	Hernan	1168879: If you try to resize a locked window, it
							unlocks itself!
	188	7/27/94	Hernan	1176713: pointOnWhichPart of actor complains if
							it gets an unrecognized thing for the part 
							argument.
	189	7/29/94	Hernan	1177193: Set keytarget does not do any 
							activation if the text being activated is not on the
							current window.
	190	8/1/94	Hernan	1177947: When detaching an actor, the physical
							rect and the regions have to be invalidated.
	191	8/1/94	Hernan	1177881: set keytarget should change the wantsIdle
							status of the editText no matter what (even it the
							field's window is not the current window).
	192	8/5/94	Hernan	Getting rid of the dither handler.
	193	8/11/94	rod	Fixed bug with selectionhalo by fixing bringup of actor.
	194	 8/12/94	Hernan  	1177757: setf-tla-boundsRect generates an update event
							for the window it is is not the current window.
							1167773: resize fixed to deal with keeping the location
							of the mouse in synch with everything else.
	195	 8/15/94	Hernan  	Since I moved the code for deeplyVisible to the
							CLOS-glue.lisp file, the actor version of this 
							function will call the one there.
	196	 8/19/94	Hernan  	Fixing set framecolor to deal correctly with
							translucent colors and telling the container about
							them.
	197	 8/31/94	Hernan  	Added 'floating' window style and changed
							*currentWindow* to *currentTla*.
	198	 8/31/94	Hernan  	Chicago -> ChicagoFont
	199	 9/ 2/94	Hernan  	1183700: Replacing references to EditText by
							references to gs:*editText-the-object*.
	200	 9/ 2/94	Hernan  	1177757: fixing setf-tla-boundsrect to only
							set the draw enabled flag to false when the
							window is changing size.
	201	 9/23/94	dy      	fix initialize to make a copy of dynamic renderers, if any
	202	 9/28/94	Hernan  	Folding in my changes to make the new region
							optimizations work.
	203	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point). Also to render-string and
							render-text.
	204	10/ 4/94	sidney  	fix eval-enqueue dying when actor is being destroyed
	205	10/ 5/94	chip    	initialize args now accessed with initializerArgument
	206	10/ 7/94	Hernan  	1190875: withActorLocked works when no container.
	207	10/ 7/94	Hernan  	Fixing stage-to-logical-list to stop using the container.
	208	10/10/94	chip    	now uses creationRelations to take care of contents/container and connectors
	209	10/17/94	dy      	ownedRegion is now in p-list, integrate DynamicRenderer stuff
	210	10/21/94	dy      	incr/decrOwnsRegionCount -> incr/decrementOwnsRegionCount
	211	10/21/94	Hernan  	Actor's initialize now sets the hasConnectors flag when necessary
	212	10/24/94	chip    	obsoleted applicableHandler
	213	10/24/94	dy      	change setScale to call new rescaled handler, add physicalScale getter
	214	11/ 4/94	Hernan  	Removing references to initial-draw-fields.
	215	11/10/94	Hernan  	The color depth of a window is changed without
							interrupts.
	216	11/23/94	dy      	dirty the rendered regions before calling enteringStage when changing container
	217	11/28/94	Hernan  	Dirtying the nodes just once when changing 
							containers. This is done before enteringStage gets
							called (not just before drawing takes place).
	218	11/30/94	Hernan  	Created new functions to change the physicalScale of an Actor
							when its container changes. These functions are also used by
							setScale. (Now... isn't that nice?)
	219	12/ 1/94	Hernan  	1202513: a lot more work is required in setBounds
							rect to react correctly to a resizing that violates
							the minimumSize of the Actor. The idea is to
							always preserve the desired location when this
							happens. For that, we figure out what the desired
							location is and then use the max of the min width
							and the desired width to offset to the right and 
							left from it. Same for the height.
	220	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	221	12/ 9/94	Hernan  	Adding the actor unsettable secret properties.
	222	12/13/94	rod     	Fixing resize of actor to properly use minimum
							size upon setting.
	223	12/13/94	sidney  	handlers to access unsettable secret properties should be private
	224	12/13/94	rod     	whoops.  fixing bug in how i used minimum size
							now i make sure to use the same algorithm.
	225	12/22/94	rod     	Changing nonlive resize to only set boundsrect
							if they change.  This removes ugly flashes from
							clicking on a resize button.
	226	 1/24/95	Hernan  	Making getters for boundsRegion, fillRegion and
							frameRegion.
	227	 1/25/95	Hernan  	Ooops. Fixing findDropObject to stop crashing
							when the avoiding argument is false.
	228	 1/26/95	dy      	change set {fill,frame,text}Color to do less when called to set the color to the same color.
	229	 2/ 3/95	Hernan  	TLA-boundsRect now only uses the window's
							location to compute the boundsRect. The problem
							with using more than this is that we might want
							to find the physical boundsRect while the window
							is being changed (this happens when the user
							defines a custom makeBoundsRegion). Resorting
							to just using the location and getting the size 
							from the physicalBoundsRect seems to work ok
							for this case.
	230	 2/ 3/95	Hernan  	get-record-field -> href or rref.
	231	 2/ 6/95	Hernan  	Keydown makes sure the thing given to it is a 
							character.
	232	 2/ 6/95	Hernan  	We should not create windows with the grow-icon-p
							argument specified because MCL disregards our
							choice of WDEF replacing it with one that does have
							a grow box. Aaaaaaaaaarrrrrrrrrrrgggghhhhhh...
	233	 2/ 7/95	rod     	Fixed bug in autotab.  it checked if the actor's
							lockedtext was true.   lockedtext is only defined
							on edittext.
	234	 2/ 8/95	Hernan  	Adding |TEXT FOR MODS| of Actor to allow the 
							collection protocol to mess with the actor's text.
	235	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	236	 2/16/95	sidney  	readable argument names for initialize handler
	237	 2/17/95	Hernan  	in set container, after -> below.
	238	 2/17/95	Hernan  	in set container, below -> following
	239	 3/ 1/95	rod     	changing setf text
	240	 3/ 3/95	Hernan  	Fixing dispatch-click to not send clicks to actor
							whose doubleClickStyle of 'doubleClickOnly.
	241	 3/13/95	Hernan  	Fixed bug in render-text. String-width was being
							called without a font spec. Bad idea. Now I call
							#StringWidth instead.
	242	 3/14/95	Hernan  	Adding new windowStyles that do not have a close
							box.
	243	 3/15/95	Hernan  	1227688: render-text now computes the location
							of the text correctly when the textLocation is a 
							'topXXX' location. There was no need to include the
							descent in our calculations.
	244	 3/16/95	till    	wrappin' some traps, #_lockpixels to setOrigin
	245	 3/17/95	till    	Better to use with-locked-pixels-force.
	246	 3/17/95	till    	D'oh
	247	 3/21/95	Hernan  	1230828: fixing stupid typo in setBoundsRect.
	248	 3/24/95	Hernan  	1231101: in setSize, when the desired size is
							logical, we pledge to preserve the location of the
							actor. The problem is that setBoundsRect, when a
							minimumSize violation occurs, might not preserve
							the location. The fix (kludge?) is to make setSize
							make sure the location is preserved by setting it
							by hand if it has changed. Yuck!
	249	 3/24/95	Hernan  	1231015: set layer and sendFarther were letting
							windows go behind the Stage. This has been fixed
							by comparing the proposed layer with the layer
							of the Stage's window.
	250	 3/27/95	sidney  	bulletproofing of detach still let some problems through. Cleaned it up using define-handlers to check for proper types of arguments
	251	 4/ 7/95	dy      	Fix 1236953.  Refer 'visible' property to renderers if dynamic.
	252	 4/18/95	Hernan  	1240592: set layer has to select the window if it becomes
							the frontmost window.
	253	 4/24/95	Hernan  	1228501: implementing 2 real layers: windows and windoids.
	254	 4/28/95	dy      	visible of Actor now simply looks at the flags.  This fixes a project load bug with QuickTime
	255	 4/28/95	Hernan  	1244503: disconnect calls simpleObjectClear instead of the
							evil, low level, only to be called by Sidney make-object-gcable. (Brian and Sidney were here too)
	2  	 6/23/95	Hernan  	1254507
	3  	 7/12/95	Hernan  	Changes of containment have to happen with the drawing
						lock grabbed by the process. Wrapping set container in a
						with-draw-lock macro. Also, changing the eval-enqueue in
						prepareForNewContent to a with-queued-drawing.
	3  	 7/12/95	Hernan  	
	4  	 7/17/95	Hernan  	Adding access to the draw function. This is required to be
						able to rewrite a number of SK8 actors in Sk8Script.
	5  	 7/31/95	Hernan  	Spliced out the code that tests whether a click has
						happened. Now it can get called by custom actors.
	6  	 1/30/96	Hernan  	1294004: I do not understant why, but resize did add 1 to 
						the minimumSize. This resulted in being able to shrink an 
						object to its minimumSize + 1. Why? I do not know. I do not
						remember. The usual problem of forgetting why a change
						is made. Wouldn't it be nice if source control systems had
						a consistency checking theorem prover built in?
	7  	 2/15/96	Brian   	adding handlers and functions from ui;sk8-utilities.lisp
						including Containers and ActorAtHVCoordinates
						and selectAll
	8  	 2/15/96	Brian   	fixing bug in ignore-sensitivity.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/16/96	Hernan  	Making *fast-draw* be stack based.
	4  	 4/16/96	Hernan  	Ooops! Better qualify that *fast-draw* symbol...
	5  	 4/17/96	Brian   	Adding collectionProperty handler for actor
	6  	 4/18/96	Hernan  	drawActorContents needs to have an extra keyword arg
						to allow the user to specify that the fill is not to be rendered.
	7  	 4/18/96	Hernan  	Adding set contents.
	8  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	9  	 4/26/96	Brian   	fixing with lockedactor macro
	10 	 5/ 9/96	sidney  	get rid of useless call to originalpropertyvalue so we don't have to define it
	11 	 5/23/96	sidney  	
	12 	 7/23/96	Brian   	setWindowRect has to do WDEF work when 
						running in the PPC.
	13 	 7/25/96	Hernan  	In setWindowRect, PPC, the wdef regions need to be
						computed before the window moves.
	14 	 7/26/96	sidney  	find-relation-or-pseudo-relation now defined here as a handler of actor
	15 	 7/29/96	Hernan  	Fixing set-rectangle-internal to deal correctly with actors
						that offset their regions changing size. (see the function
						for a long comment about it).
	16 	10/ 7/96	sidney  	disposing-clos-window no longer needed
	17 	10/ 9/96	Hernan  	Fixed resize to reset the XOR draw mode each time we draw.
	18 	11/15/96	Hernan  	Adding a function to ensure that in initialize the "original"
						argument is the ancestor of the right type. This is a problem
						when otherparents is used.
	19 	11/22/96	Hernan  	Removing originalAncestor from here (not early
						enough in the build).
	20 	12/12/96	Hernan  	Need to recompile do-sk8-window-actors.
	21 	 2/11/97	Hernan  	(copy -> (sk8::copy
	22 	 2/11/97	Hernan  	detach of Stage now calls deactivate AFTER the window 
						has been removed from the Stage. See code for details.
	23 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
