(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#|

A DynamicRenderer has to know which actor it is rendering to and
whether it is the fillColor, frameColor, or textColor of the actor.
DynamicRenderer has three virtual properties,
    fillColorActor
    frameColorActor
    textColorActor
The setters on these properties ensure that only one of them is set
at a time.  Assuming that a1 is an Actor, and r1 is a DynamicRenderer,
doing this:
    set a1's fillColor to r1
causes this to happen
    set r1's fillColorActor to nil
which releases r1 from any other actor it may have been associated with, and
    set r1's fillColorActor to a1
which sets a1's fillColor to r1.  (Similarly for frame and text.)

DynamicRenderer also has a read-only property:
    actorOwner
which holds the associated actor,
whether it be the fillColorActor, the frameColorActor, or the textColorActor.

|#
(SK8-override :SK8 (:UI)
              
              COMMON-LISP:STEP
              )

(SK8-declare-syms :SK8 :private
                  SK8::clipRegionGCHandle
                  SK8::movieClipRegionGCHandle
                  SK8::moveMovieFromRenderedRegion
                  SK8::moveMovieFromFill
                  SK8::moveMovieFromFrame
                  SK8::moveMovieFromBounds
                  SK8::resizeMovieFromRegion
                  sk8::doMoved
                  SK8::actionFilter
                  SK8::dragEnabled
                  )

(SK8-declare-syms :SK8 :public
                  SK8::boundingBoxMovieOffset
                  SK8::controllerPortion
                     )
;;; Set this to nil before compiling and no debugging code is generated.
;;; Set this to nil after  compiling and no debugging code is executed.
#|
(eval-when (:compile-toplevel)
  #+DYost (insert-features :debug-render :debug-actions :debug-actionFilter)
  )

(insert-features :debug-render)
(insert-features :debug-actions)
(insert-features :debug-actionFilter)

(eval-when (:load-toplevel)
  (remove-features :debug-render :debug-actions :debug-actionFilter)
  )
|#

(new DynamicRenderer
     :objectname "QuickTimeRenderer"
     :undisposable t
     :project sk8)
  
(addProperty QuickTimeRenderer 'controllerGCOSPtr                  :private t      ) ; the QuickTimeontroller
(addProperty QuickTimeRenderer 'media                                              ) ; the QuickTimeMovie object we're playing
(addProperty QuickTimeRenderer 'resizingStyle           :initialValue 'resizeMovieFromRenderedRegion)
(addProperty QuickTimeRenderer 'state                   :initialValue 'inactive    ) ; inactive, stopped, poster, paused, or playing
(addProperty QuickTimeRenderer 'previousState                                      ) ;
(addProperty QuickTimeRenderer 'newStateWhenDone        :initialValue 'paused      ) ; 'previous or other state value
(addProperty QuickTimeRenderer 'playWithNoOtherActivity                            ) ; ignore events while playing
(addProperty QuickTimeRenderer 'initialStateForNewMovie :initialValue 'poster      ) ; any valid state value
(addProperty QuickTimeRenderer 'maintainAspectRatio     :initialValue 'fitInside   ) ; boolean
(addProperty QuickTimeRenderer 'alignment               :initialValue '(1/2 0)     ) ; boolean
(addProperty QuickTimeRenderer 'controllerVisible                                  ) ; boolean
(addProperty QuickTimeRenderer 'usingBadge                                         ) ; boolean
(addProperty QuickTimeRenderer 'playingEveryFrame                                  ) ; boolean
(addProperty QuickTimeRenderer 'reservingRoomForController                         ) ; boolean
(addProperty QuickTimeRenderer 'keysEnabled                                        ) ; boolean
(addProperty QuickTimeRenderer 'activeEvents                       :private t      )
(addProperty QuickTimeRenderer 'hiddenWindow                       :private t      )
(addProperty QuickTimeRenderer 'callBackForDone                    :private t      )
(addProperty QuickTimeRenderer 'stepButtonsVisible    :initialValue t              )
(addProperty QuickTimeRenderer 'speakerButtonVisible  :initialValue t              )
(addProperty QuickTimeRenderer 'usingWindowPalette                                 )
(addProperty QuickTimeRenderer 'doingInvalidating                  :private t      )
#|
(addProperty QuickTimeRenderer 'pauseWithHighQuality                               ) ; boolean
|#
#|
(addProperty QuickTimeRenderer 'playSelectionOnly                                  ) ; boolean
(addProperty QuickTimeRenderer 'playEveryFrame                                     ) ; boolean
(addProperty QuickTimeRenderer 'dragEnabled :private t                             ) ; boolean
(addProperty QuickTimeRenderer 'controllerForeColor     :initialValue Black        ) ; an RGBColor
(addProperty QuickTimeRenderer 'controllerBackColor     :initialValue White        ) ; an RGBColor
|#

(eval-when (:compile-toplevel :load-toplevel)
  (define-sk8-constant QTRStateValues
    '(inactive stopped poster paused playing)
    :private? t :project SK8)

  (define-sk8-constant QTRNewStateWhenDoneValues
    '(previous inactive stopped poster paused)
    :private? t :project SK8)

  (define-sk8-constant QTRMaintainAspectRatioValues
    '(fitInside fitOutside nil)
    :private? t :project SK8))

;;;;;;;;;

(defmacro assumeMedia ((me myName) &body body)
  `(let ((,myName (media ,me)))
     (if ,myName
       (progn ,@body)
       *undefined*)))

(defmacro assumeMediaOrNil ((me myName) &body body)
  `(let ((,myName (media ,me)))
     (if ,myName
       (progn ,@body)
       nil)))

(defun QTRnoMedia (me)
  (sk8-error GeneralProgrammaticError
             :strings '("no media for ")
             :objects (list me)))

(defmacro assumeMediaWithError ((me myName) &body body)
  `(let ((,myName (media ,me)))
     (if ,myName
       (progn ,@body)
       (QTRnoMedia ,me))))

(defun qtrNoController (me)
  (sk8-error GeneralProgrammaticError
             :strings '("no controllerGCOSPtr for ")
             :objects (list me)))

(defmacro assumeControllerGCOSPtr ((me myName) &body body)
  `(let ((,myName (controllerGCOSPtr ,me)))
     (if ,myName
       (progn ,@body)
       *undefined*)))

(defmacro assumeControllerGCOSPtrWithError ((me myName) &body body)
  `(let ((,myName (controllerGCOSPtr ,me)))
     (if ,myName
       (progn ,@body)
       (qtrNoController ,me))))

(defmacro ifControllerGCOSPtr ((me myName) ifPart elsePart)
  `(let ((,myName (controllerGCOSPtr ,me)))
     (if ,myName
       ,ifPart
       ,elsePart)))

;; These two macros are for guarding against infinite recursion.
(defmacro unless-locking-action-event ((event) &body body)
  `(and (not (member ',event (activeEvents me)))
        (progn
          ,@body
          t)))

(defmacro locking-action-event ((event) &body body)
  `(unless-locking-action-event (,event)
     (push ',event (activeEvents me))
     (unwind-protect
       (progn ,@body)
       (pop (activeEvents me)))))

;;;;;;;;;;
;;; Our hooks into event processing

(unless (boundp 'sk8dev::*active-movie-controllers*)
  (defvar sk8dev::*active-movie-controllers* nil)) ;? should be a population

;;; If this returns t, the event has been handled.
;;; Called by do-event.

;;(when (fboundp 'sk8dev::is-movie-controller-event)
;;  (fmakunbound 'sk8dev::is-movie-controller-event))
(defun sk8dev::is-movie-controller-event (event)
  (ignore-errors
   (dolist (controller sk8dev::*active-movie-controllers*)
     (when (T_MCIsPlayerEvent (controllerGCOSPtr controller) event)
       (return-from sk8dev::is-movie-controller-event t))))
  nil)

(defun sk8dev::movie-controllers-idle ()
  (ignore-errors
   (dolist (controller sk8dev::*active-movie-controllers*)
     (when (T_MCIdle (controllerGCOSPtr controller))
       (done controller))))
  nil)

(unless (boundp 'sk8dev::*active-movies-without-controllers*)
  (defvar sk8dev::*active-movies-without-controllers* nil)) ;? should be a population

;;; Called by do-event.

(when (fboundp 'sk8dev::movies-task)
  (fmakunbound 'sk8dev::movies-task))

(defun sk8dev::movies-task ()
  (ignore-errors
   (dolist (movie sk8dev::*active-movies-without-controllers*)
     (let ((theMovieGCOSPtr (mediaData (media movie))))
       (T_MoviesTask theMovieGCOSPtr)
       (when (T_IsMovieDone theMovieGCOSPtr)
         (done movie)) ; removes it from the *active-movies-without-controllers* list
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QuickTimeRenderer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A movie should be servicing when
;;; 1. its controller is visible OR
;;; 2. its controller is not visible AND the movie is playing.

(defun make-sure-movie-is-serviced-if-it-needs-to (me serviceIt? controllerVisible?)
  (flet ((stop-servicing (me) ;; remove the renderer from any servicing variable it might be in. 
           ;; Very defensive code: remove them from both, even though it should never be in both
           ;; at the same time... 
           (when (memq me sk8dev::*active-movie-controllers*)
             (setf sk8dev::*active-movie-controllers* (delete me sk8dev::*active-movie-controllers*)))
           (when (memq me sk8dev::*active-movies-without-controllers*)
             (setf sk8dev::*active-movies-without-controllers* (delete me sk8dev::*active-movies-without-controllers*)))))
    (stop-servicing me)
    (when (media me)
      (if controllerVisible?
        (progn (pushnew me sk8dev::*active-movie-controllers*) t)
        (when serviceIt? 
          (progn (pushnew me sk8dev::*active-movies-without-controllers*) t))))))
      
;;; Returns true if movie is playing and it is being serviced.

(define-handler servicing (QuickTimeRenderer)
  (and (if (controllerVisible me)
         (memq me sk8dev::*active-movie-controllers*)
         (memq me sk8dev::*active-movies-without-controllers*))
       t))

(define-handler (setf servicing) (newValue QuickTimeRenderer)
  (make-sure-movie-is-serviced-if-it-needs-to me newValue (controllerVisible me)))

#|

  (if newValue
    (if (media me)
      (pushnew me sk8dev::*active-movie-controllers*)
      (setf newValue nil))
    (setf sk8dev::*active-movie-controllers*
          (remove me sk8dev::*active-movie-controllers*)))
  newValue)

|#

(define-handler render (QuickTimeRenderer theActor region paper)
  (trace-print-bracketing ("render" :debug-render)
    (gs:let+ ((effective-rgn (:region))
              (myMedia (media me)))
      (#_getClip effective-rgn) ;? check for errors
      (#_SectRgn effective-rgn region effective-rgn) ;? check for errors
      (if (neq theActor (actorOwner me))
        ;; We're being asked to render somewhere else.
        (if myMedia
          ;; We have a media, so show the poster
          (with-preserved-values (((servicing me)                    nil)
                                  ((suppressingVisualTracks myMedia) nil))
            #| ;;? temporarily disabled - gotta go over it and make it work
          (let ((oldClipRegion (clipRegionGCHandle me))
                (oldMovieBox   (movieBox me))) ;?boundsRect me ?
            (unwind-protect
              (progn
                (setf (displayClipRegionGCHandle myMedia) effective-rgn)
                (let ((regionBox (regionToBox region)))
                  (setf (movieBox myMedia :dontForwardToRenderer t)
                        (constrainBoundsToAspectRatio (preferredAspectRatio me)
                                                      regionBox)))
                (T_showMoviePosterInGWorld (mediaData myMedia) paper TC_nil))
              (progn
                (setf (movieBox me :dontDisplay t) oldMovieBox)
                (when oldClipRegion
                  (setf (clipRegionGCHandle myMedia) oldClipRegion)))))|#)
          ;; We have no media, so show the background
          (render (backgroundColor me) theActor region paper))
        ;; Rendering into our actorOwner
        (let ((state (if myMedia
                       (state me)
                       'inactive)))
          (case state
            ((playing paused poster)
             ;;? We could optimize here by drawing the background only when there's uncovered space
             (render (backgroundColor me) theActor region paper)
             (case state
               ;; Movie is playing, just set the clip! Actually drawing at this point will
               ;; cause set state to be called, actually stopping the movie. 
               (playing
                (setf (clipRegionGCHandle me) effective-rgn))
               (paused
                (setf (clipRegionGCHandle me) effective-rgn)
                (with-preserved-movieController-gworld ((controllerGCOSPtr me))
                  (setf (gWorld me) paper)
                  (actionDraw me paper)
                  (actionIdle me)
                  )
                )
               (poster
                (setf (clipRegionGCHandle me) effective-rgn)
                (with-preserved-movieController-gworld ((controllerGCOSPtr me))
                  (setf (gWorld me) paper)
                  (actionDraw me paper)
                  ; (actionIdle me) ;? not necessary?
                  )
                )
               ))
            (otherwise
             ;; The movie is not playing.  Render the fill region and the text
             (render (backgroundColor me) theActor region paper)
             (when (or (controllerVisible me)
                       (usingBadge me))
               (assumeControllerGCOSPtr (me myController)
                 (setf (clipRegionGCHandle me) effective-rgn)
                 (with-preserved-movieController-gworld (myController)
                   (setf (gWorld me) paper)
                   (actionDraw me paper)
                   (actionIdle me)))))))))
    (sk8-return-nothing)))

#|
;;; no longer used, but it's so cool, I hate to blow it away...
(defun flattenParents (me ancestorsThatMustBeParents)
  "Given a list of ancestors that are required to be direct parents,
returns a new, rearranged parent list, or nil if no rearrangement was necessary.
WARNING:  when the parent list is rearranged, necessarily some objects spliced
out of the hierarchy in the new list.  They had better not have any local properies
or local handlers!"
  (when ancestorsThatMustBeParents
    (labels ((flattenParents1 (me)
              (loop for parent in (parents me)
                    if (dolist (cand ancestorsThatMustBeParents nil)
                         (when (and (inheritsFrom parent cand)
                                    (not (eq parent cand)))
                           (return t)))
                    append (flattenParents1 parent)
                    else
                    append (list parent)
                    end)))
      (let ((newParents (flattenParents1 me)))
        (unless (equal newParents (parents me))
          newParents)))))

#| testing code:

(new Object :objectName "xx1" :project sk8)
(new Object :objectName "xx2" :project sk8)
(new Object :objectName "xx3" :project sk8)
(new Object :objectName "xx4" :project sk8)
(new xx1    :objectName "xx11" :project sk8)
(new xx3    :objectName "xx31" :project sk8)
(new xx11   :objectName "xxA" :project sk8)
(addParent xxA xx2)
(new xxA    :objectName "xxB" :project sk8)
(addParent xxB xx31)
(addParent xxB xx4)
(new xxB    :objectName "xxC" :project sk8)
(parents xxA)
(parents xxB)
(parents xxC)
(flattenParents xxC '()) ; => nil
(flattenParents xxC (list xxB)) ; => nil
(flattenParents xxC (list xx2 xx3)) ; => (xx4 xx3 xx2 xx11)
(flattenParents xxC (list xx4)) ; => (xx4 xx31 xxA)

|#
|#


(define-handler initialize :private (QuickTimeRenderer original isNew args)
  (declare (ignore isNew args))
  ;;? raise an error if they try to set certain properties
  (requireQuickTimeInitialized)
  (call-next-method)
  (let ((oldMedia (media original)))
    (when oldMedia
      ;; If making a child, then we can't let the parent keep its media because a Movie can't be in two places at once.
      ;; To keep this from happening, we grab the media away from our parent and give it to the child.
      ;; If making a copy, we just make the choice to do the same thing.
      ;; In short, making a copy or child of a QuickTimeRenderer is not reccommended.
      (setf (media original) nil) ; removes the movie from original
      (setf (media me) oldMedia)))
  (sk8-return-nothing))

;;;;;;;;;;;;;;;

(define-handler (setf backgroundColor) (newRenderer QuickTimeRenderer)
  (declare (ignore newRenderer))
  (call-next-method)
  (doRedraw me))

(define-handler dynamicRendererToUse :private (QuickTimeRenderer theActor)
  (if (eq me QuickTimeRenderer)
    (new QuickTimeRenderer :project (project theActor))
    me))

(define-handler translucent :private (QuickTimeRenderer)
  (translucent (backgroundColor me)))

(define-handler (setf active) (newValue QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (raiseUnsettablePropertyError 'active me)
    #|
    (if newValue
      (when (eq (state me) 'inactive)
        (setf (state me) 'stopped))
      (setf (state me) 'inactive))
    (setf (active (media me) :dontForwardToRenderer t) newValue)
    |#
    newValue))

;(defvar covered 0)
;(defvar uncovered 0)

(define-handler coverEvent :private (QuickTimeRenderer theMovieGCOSPtr changedRegion)
  (declare (ignore me theMovieGCOSPtr changedRegion))
  ;(setf covered (1+ covered)) ;? debug
  TC_noErr)

(define-handler uncoverEvent :private (QuickTimeRenderer theMovieGCOSPtr changedRegion)
  (declare (ignore me theMovieGCOSPtr changedRegion))
  ;(setf uncovered (1+ uncovered)) ;? debug
  TC_noErr)

;;;;;;;;;;;;;;;;;;;
;;; onStage and visible
;;; must both be true or all visual tracks are disabled
;;; so that only the sound (if any) plays in the movie

(define-handler handleOnStageAndVisible :private (QuickTimeRenderer)
   (when (media me)
     (if (and (visible me) (onStage me))
       (progn
         ;; Re‘nable all the tracks the user thinks are enabled
         (setf (suppressingVisualTracks (media me)) nil) ; gotta call it on the media!
         (setf (gWorld me) nil) ; this sets the GWorld to the actorOwner's window
         (doResizing me))
       (progn
         ;; Disable all visual tracks
         (setf (suppressingVisualTracks (media me)) t) ; gotta call it on the media!
         (setf (gWorld me) (hiddenWindow me)) ; this sets the GWorld to the actorOwner's window
         #|(actionDeactivate me)|# ;;?
         (when (actorOwner me)
           (lightForceRedraw (actorOwner me))))))
   (sk8-return-nothing))

(define-handler onStage :private (QuickTimeRenderer)
  (let ((actorOwner (actorOwner me)))
    (and actorOwner
         (let ((myWindow (sk8:window actorOwner)))
           (and myWindow
                (eq (container myWindow)
                    Stage))))))

(define-handler (setf visible) :private (newValue QuickTimeRenderer)
  (handleOnStageandVisible me)
  newValue)

;;; an event handler = justEnteredStage
(define-handler enteringStage :private (QuickTimeRenderer)
  (handleOnStageandVisible me)
  (sk8-return-nothing))

;;; an event handler
(define-handler leavingStage :private (QuickTimeRenderer)
  (handleOnStageandVisible me)
  (sk8-return-nothing))

(define-handler doRedraw :private (QuickTimeRenderer &key fullRedraw)
  (let ((actorOwner (actorOwner me)))
    (when actorOwner
      (if fullRedraw
        (forceRedraw      actorOwner)
        (lightForceRedraw actorOwner))))
  (sk8-return-nothing))

(eval-when (:compile-toplevel :load-toplevel)
  (define-sk8-constant QTRResizingStyleValues
    '(resizeMovieFromFrame
      resizeMovieFromFill
      ; resizeMovieFromText
      resizeMovieFromRenderedRegion
      resizeFillFromMovie
      resizeFrameFromMovie
      ; resizeTextFromMovie
      resizeRenderedRegionFromMovie
      nil)
    :project SK8))

(defun checkLegalValue (value legalValues)
  (unless (member value legalValues)
    (sk8-error TypeMismatchError
               :object value
               :expectedType legalValues))
  (sk8-return-nothing))

(define-handler doResizing :private (QuickTimeRenderer)
  ;; Only do resizing when we're associated with an actor.
  (let ((actorOwner (actorOwner me)))
    (when actorOwner
      (case (resizingStyle me)
        (resizeFillFromMovie           (resizeFillFromMovie           me))
        (resizeFrameFromMovie          (resizeFrameFromMovie          me))
        #|(resizeTextFromMovie         (resizeTextFromMovie           me)) |#
        (resizeRenderedRegionFromMovie (resizeRenderedRegionFromMovie me))
        (otherwise                     (resized me)))))
  (sk8-return-nothing))

(defun checkResizingStyle (me resizingStyle)
  (declare (ignore me))
  (when resizingStyle
    (checkLegalValue resizingStyle QTRResizingStyleValues))
  #|
  (when (and (eq resizingStyle 'resizeFillFromMovie)
             (media me)
             (actorOwner me)
             (< (quickTimeVersion) #x2100000))
    (sk8-error GeneralProgrammaticError
               :strings '("Can't set resizingStyle of "
                          " to 'resizeFillFromMovie' when the QuickTimeRenderer has an actorOwner and a media when using QuickTime 2.0 or earlier.")
               :objects (list me)))
  |#
  resizingStyle)

(define-handler (setf resizingStyle) (newResizingStyle QuickTimeRenderer)
  (checkResizingStyle me newResizingStyle)
  (let ((oldResizingStyle (resizingStyle me)))
    (setValue 'resizingStyle me newResizingStyle)
    (when (neq oldResizingStyle newResizingStyle)
      (doResizing me)))
  newResizingStyle)

;;; This setmedia handler is broken out from set media so we can hide the movingToNewRenderer argument.
(define-handler setMedia :private (QuickTimeRenderer
                                            newMedia
                                            &optional
                                            movingToNewRenderer
                                            initialStateSet
                                            initialState)
  (let ((actorOwner (actorOwner me))
        (oldMedia (media me))
        (oldState (state me)))
    ; (format t "set media from ~S to ~S~%" oldMedia newMedia)
    (unless (eq oldMedia newMedia)
      (when (eq me QuickTimeRenderer)
        (sk8-error GeneralError
                   :strings '("Can't set media of QuickTimeRenderer itself")))
      (withActorLockedPossiblyFalse (actorOwner)
        (when newMedia
          (if (eq newMedia t)
            (setf newMedia (new QuickTimeMovie :file (getFileFromUserWithPreview (project me)) :project (project me)))
            (ensureType newMedia QuickTimeMovie))
          (let ((previousRendererOfNewMedia (renderer newMedia)))
            (when previousRendererOfNewMedia
              (unless initialStateSet
                (setf initialState (state previousRendererOfNewMedia))
                (setf initialStateSet t))
              ;; detach newMedia from its current renderer
              (setMedia previousRendererOfNewMedia nil t))))
        (when oldMedia
          (setf (servicing me) nil)
          ;; When we're done, oldMedia will no longer have a renderer, so it can't be left visible or playing
          (unless (or (eq oldState 'inactive)
                      (eq oldState 'stopped))
            (if movingToNewRenderer
              ;; Let movie's time keep advancing, even though it won't be serviced. ;? this may be dangerous with movie controller
              (setValue 'state me 'inactive) ;pretend state is not poster, paused, or playing
              (stop me)))
          (dispose (callBackForDone me))
          (setf    (callBackForDone me) nil)
          (T_MCSetMovie (controllerGCOSPtr me) TC_nil)
          (setf (renderer oldMedia) nil)
          ;;? Maybe we should dispose the MovieController now rather tahn wait for GC.
          (setValue 'controllerGCOSPtr me nil))
        (setValue 'media me newMedia)
        (when newMedia
          (when-unwind
            (progn
              (with-port (or (hiddenWindow me)
                             (setf (hiddenWindow me) (makeGCOSPtr (wptr (make-instance 'window :view-size (make-point 1 1) 
                                                                                       :view-position (make-point -4000 -4000)))
                                                                  #'window-close)))
                (setf (active newMedia) t) ; why doesn't the controller handle this for me?
                (setValue 'controllerGCOSPtr me (T_NewMovieControllerGC
                                                 (mediaData newMedia)
                                                 :topLeftMovie t
                                                 :withBadge (getValue 'usingBadge me)
                                                 :notVisible (not (getValue 'controllerVisible me)))))
              (actionSetFlags me (logior                                                   #$mcFlagSuppressMovieFrame
                                         (if      (getValue 'stepButtonsVisible   me)  0 #$mcFlagSuppressStepButtons)
                                         (if      (getValue 'speakerButtonVisible me)  0 #$mcFlagSuppressSpeakerButton)
                                         (if      (getValue 'usingWindowPalette       me)  #$mcFlagsUseWindowPalette     0)
                                         (if (not (getValue 'doingInvalidating        me)) #$mcFlagsDontInvalidate       0)))
              (setf (renderer newMedia) me)

              ;;? We shouldn't have to mess with the movie like this, but here's what would happen
              ;; if we didn't do this:
              ;; set a's fillcolor to qtr
              ;; set a's fillcolor to blue
              ;; -- the blue is there in the GWorld, all right, but Quicktime
              ;; draws over it with white, presumably because of something in the uncoverProc
              (T_SetMovieCoverProcs (mediaData (media me)) (ObjectIDFromObject me))

              (setf (callBackForDone me) (new QuickTimeCallBackForRenderer
                                              :timeBase (timeBase me)
                                              :project (project me)))
              (T_MCSetActionFilterWithRefCon (controllerGCOSPtr me) (objectIDFromObject me))
              (setState me (cond
                            (initialStateSet
                             initialState)
                            ((initialStateForNewMovie me)
                             (initialStateForNewMovie me))
                            (t
                             (state me)))
                        nil nil :force t)
              (unless (or (active newMedia)
                          (eq (state me) 'inactive))
                (actionActivate me))
              (handleOnStageandVisible me)
              (when (controllerVisible me)
                (setf (servicing me) t))
              )
            (progn
              (setf (renderer newMedia) nil)
              (setValue 'media me nil)
              (setValue 'controllerGCOSPtr me nil)))))))
  newMedia)

(define-handler (setf media) (newMedia
                                       QuickTimeRenderer
                                       &key
                                       (initialState 'poster initialStateSet))
  (setMedia me newMedia nil initialStateSet initialState))

;;;;;;;;;;;;;;;;
;;; SK8 Interaction
;;;;;;;;;;;;;;;;

#|
(defun changingScale (oldMatrix newMatrix)
  (let ((oldScaleX               (aref oldMatrix 0 0))
        (oldScaleY               (aref oldMatrix 0 1))
        (newScaleX (if newMatrix (aref newMatrix 0 0) 1))
        (newScaleY (if newMatrix (aref newMatrix 0 1) 1)))
    (not (and (= oldScaleX newScaleX)
              (= oldScaleY newScaleY)))))
|#

(define-handler (setf matrix) (newMatrix QuickTimeRenderer)
  (declare (ignore newMatrix))
  (raiseUnsettablePropertyError 'matrix me))

(define-handler (setf scale) (newScale QuickTimeRenderer)
  (assumeMediaWithError (me myMedia)
    (raiseUnsettablePropertyError 'scale me)
    #| ;? we'll have to set the scale, get the movie's box, then set the controller's box to match
    (withActorLockedPossiblyFalse ((actorOwner me))
      (with-preserved-values
        (((resizingStyle me) (case (resizingStyle me)
                               (resizeMovieFromFrame          'resizeFrameFromMovie)
                               (resizeMovieFromFill           'resizeFillFromMovie)
                               (resizeMovieFromRenderedRegion 'resizeRenderedRegionFromMovie)
                               (otherwise                     (resizingStyle me)))))
        (setf (scale myMedia :dontForwardToRenderer t) newScale)
        (doResizing me)))
    (scale myMedia)
|#
    ))

(defun resizingBox (oldBox newBox)
  (let ((oldWidth  (- (elt oldBox 2) (elt oldBox 0)))
        (oldHeight (- (elt oldBox 3) (elt oldBox 1)))
        (newWidth  (- (gs:f.round (elt newBox 2)) (gs:f.round (elt newBox 0))))
        (newHeight (- (gs:f.round (elt newBox 3)) (gs:f.round (elt newBox 1)))))
    (not (and (= oldWidth  newWidth)
              (= oldHeight newHeight)))))

(defun changingBox (oldBox newBox)
  (not (and (= (elt oldBox 0) (gs:f.round (elt newBox 0)))
            (= (elt oldBox 1) (gs:f.round (elt newBox 1)))
            (= (elt oldBox 2) (gs:f.round (elt newBox 2)))
            (= (elt oldBox 3) (gs:f.round (elt newBox 3))))))

(define-handler resizeActorIfCalledFor :private (QuickTimeRenderer)
  (let ((actorOwner (actorOwner me))
        (myMedia    (media      me)))
    (when (and actorOwner myMedia)
      (case (resizingStyle me)
        ((resizeFrameFromMovie
          resizeFillFromMovie
          ; resizeTextFromMovie
          resizeRenderedRegionFromMovie)
         (doResizing me))
        (otherwise
         (lightForceRedraw actorOwner)))))
  (sk8-return-nothing))

(define-handler (setf movieBox) (newMovieBox QuickTimeRenderer)
  (declare (ignore newMovieBox))
  (assumeMediaWithError (me myMedia)
    (raiseUnsettablePropertyError 'movieBox me)))

(define-handler (setf maintainAspectRatio) (newValue QuickTimeRenderer)
  (checkLegalValue newValue QTRMaintainAspectRatioValues)
  (let ((oldValue (maintainAspectRatio me)))
    (setValue 'maintainAspectRatio me newValue)
    (when (neq oldValue newValue)
      (doResizing me)))
  newValue)

(define-handler (setf alignment) (newAlignment QuickTimeRenderer)
  (ensureType newAlignment Collection)
  (assert (= (length newAlignment) 2))
  #|
  (ensureType (elt newAlignment 0) Number0Through1)
  (ensureType (elt newAlignment 1) Number0Through1)
  |#
  (let ((oldAlignment (alignment me)))
    (setValue 'alignment me newAlignment)
    (unless (and (= (elt oldAlignment 0)
                    (elt newAlignment 0))
                 (= (elt oldAlignment 1)
                    (elt newAlignment 1)))
      (doResizing me)))
  newAlignment)

#|
;;; Sets offset of the boundsRect to the fillRegion
(define-handler alignMovieFromBounds (QuickTimeRenderer boundingBox) 
  (let* ((actorOwner (actorOwner me))
         (myMedia    (media      me)))
    (when (and actorOwner myMedia)
      (setf (boundsRect me)
            (alignSizeToBounds `#(,(width myMedia) ,(height myMedia))
                               boundingBox
                               :horizontalAlignment (elt (alignment me) 0)
                               :verticalAlignment   (elt (alignment me) 1)))))
  (sk8-return-nothing))
|#

(defun resize-box (op box size)
  (destructuring-bind (left top right bottom) (coerce box 'list)
    (destructuring-bind (width height) size
      (list left
            top
            (apply op right  width  nil)
            (apply op bottom height nil)))))

(defun offset-box (op box size)
  (destructuring-bind (left top right bottom) (coerce box 'list)
    (destructuring-bind (width height) size
      (list (apply op left width  nil)
            (apply op top  height nil)
            right
            bottom))))

(define-handler boundsRect :private (QuickTimeRenderer &key raw)
  (assumeControllerGCOSPtr (me myController)
    (rlet ((theBox :rect))
      (T_MCGetControllerBoundsRect myController theBox)
      (let ((box (list (rref theBox :rect.left)
                       (rref theBox :rect.top)
                       (rref theBox :rect.right)
                       (rref theBox :rect.bottom))))
        (unless (or raw
                    (reservingRoomForController me)
                    (controllerVisible me))
          (setf box (offset-box #'+ box (boundingBoxMovieOffset me)))
          (setf box (resize-box #'- box (controllerPortion me))))
        (sk8-multivals-sequence 4 box)))))

(define-handler (setf boundsRect) :private (box QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (let ((oldBox (boundsRect me)))
      (if (changingBox oldBox box) ; guard against recursion
        (locking-action-event (setBoundsRect) ;? This extra guard against recursion should no be necessary after QT 2.0.
          (unless (or (reservingRoomForController me)
                      (controllerVisible me))
            (setf box (resize-box #'+ box (controllerPortion me)))
            (setf box (offset-box #'- box (boundingBoxMovieOffset me))))
          (rlet ((boxRect :rect))
            (rset boxRect rect.left   (elt box 0))
            (rset boxRect rect.top    (elt box 1))
            (rset boxRect rect.right  (elt box 2))
            (rset boxRect rect.bottom (elt box 3))
            (T_MCSetControllerBoundsRect myController :boxRect boxRect))
          (resizeActorIfCalledFor me)
          box)
        oldBox))))

;;;? is there another version of this already?
(defun addPoint (a b)
  (sk8-multivals (+ (elt a 0)
                    (elt b 0))
                 (+ (elt a 1)
                    (elt b 1))))

;;; The portion of the boundingBox taken by the movie controller.
;;; The standard controller is (0 16)
(define-handler controllerPortion :private (QuickTimeRenderer)
  (assumeControllerGCOSPtr (me myController)
    (assumeMedia (me myMedia)
      (sk8-multival-bind (cleft ctop cright cbottom) (boundsRect me :raw t)
        (sk8-multival-bind (mleft mtop mright mbottom) (movieBox myMedia)
          (sk8-multivals
           (+ (- mleft  cleft)
              (- cright mright))
           (+ (- mtop    ctop)
              (- cbottom mbottom))))))))

;;; If the controller is to the left of and/or above the movie area, then
;;; this will return non-zero values for horizontal and/or vertical offset.
(define-handler boundingBoxMovieOffset :private (QuickTimeRenderer)
  (assumeMedia (me myMedia)
    (assumeControllerGCOSPtr (me myController)
      (sk8-multival-bind (cleft ctop cright cbottom) (boundsRect me :raw t)
        (declare (ignore cright cbottom))
        (sk8-multival-bind (mleft mtop mright mbottom) (movieBox myMedia)
          (declare (ignore mright mbottom))
          (sk8-multivals
           (- mleft cleft)
           (- mtop  ctop )))))))

(define-handler preferredSize (QuickTimeRenderer)
  (assumeMedia (me myMedia)
    (if (or (reservingRoomForController me)
            (controllerVisible me))
      (addPoint (preferredSize myMedia)
                (controllerPortion me))
      (preferredSize myMedia)
      )))

(define-handler preferredAspectRatio (QuickTimeRenderer)
  (assumeMedia (me myMedia)
    (sk8-multival-bind
      (width height)
      (preferredSize me)
      (if (zerop height)
        0
        (/ width height)))))

(defun regionToBox (region)
  (T_rectRecordValues (gs:hrref region :region.rgnbbox :storage :pointer)))

(define-handler moveMovieFromBounds :private (QuickTimeRenderer bounds)
  (let ((myMedia (media me)))
    (when myMedia
      (sk8-multival-bind (left top right bottom)
                         (boundsRect me)
        (setf (boundsRect me)
              (alignSizeToBounds `#(,(- right left) ,(- bottom top))
                                 bounds
                                 :horizontalAlignment (elt (alignment me) 0)
                                 :verticalAlignment   (elt (alignment me) 1))))))
  (sk8-return-nothing))

;;; Sets offset of the boundsRect to the fillRegion
(define-handler moveMovieFromFill :private (QuickTimeRenderer) 
  (let ((actorOwner (actorOwner me)))
    (when (and actorOwner (media me))
      (gs:recompute-fill-region actorOwner (gs:node-flags actorOwner))
      (moveMovieFromBounds me (regionToBox (gs:node-fillRegion actorOwner)))))
  (sk8-return-nothing))

(defun frameWindowRelativeBoundsRect (me)
  (let ((frameSize (frameSize me)))
    (if (and (zerop (first  frameSize))
             (zerop (second frameSize)))
      (let ((rectStruct (gs:recompute-physicalBoundsRect me)))
        (sk8-multivals (gs:rect-left   rectStruct)
                       (gs:rect-top    rectStruct)
                       (gs:rect-right  rectStruct)
                       (gs:rect-bottom rectStruct)))
      (progn
        (gs:recompute-frame-region me (gs:node-flags me))
        (T_rectRecordValues (gs:hrref (gs:node-frameRegion me)
                                   :region.rgnbbox
                                   :storage :pointer))))))

;;; Sets offset of the boundsRect to the frameRegion
;;;;;;;;??? not finished yet
(define-handler moveMovieFromFrame :private (QuickTimeRenderer)
  (when (media me)
    (let ((actorOwner (actorOwner me)))
      (when actorOwner
        (moveMovieFromBounds me (frameWindowRelativeBoundsRect actorOwner))))))

(define-handler moveMovieFromRenderedRegion :private (QuickTimeRenderer)
  (if (frameColorActor me)
    (moveMovieFromFrame me)
    (moveMovieFromFill  me))
  (sk8-return-nothing))

;;;-------------------------------------------------------------------------------------

#| not used
;;; Always does it maintainingAspectRatio and centered
(define-handler resizeMovieFromRegion :private (QuickTimeRenderer region) 
  (when (media me)
    (let ((regionBox (regionToBox region)))
      (setf (boundsRect me)
            (constrainBoundsToAspectRatio (preferredAspectRatio me)
                                          regionBox))))
  (sk8-return-nothing))
|#

(define-handler resizeMovieFromBounds :private (QuickTimeRenderer boundingBox &key maintainAspectRatio) 
  (when (media me)
    (let ((actorOwner (actorOwner me))
          (alignment  (alignment  me)))
      (withActorLockedPossiblyFalse
       (actorOwner)
       (setf (boundsRect me)
             (if (or maintainAspectRatio
                     (maintainAspectRatio me))
               (roundBoxDimensions
                (constrainBoundsToAspectRatio (preferredAspectRatio me)
                                              boundingBox
                                              :fitOutside (eq (maintainAspectRatio me) 'fitOutside)
                                              :horizontalAlignment (elt alignment 0)
                                              :verticalAlignment   (elt alignment 1)))
               boundingBox))
       (doRedraw me))))
  (sk8-return-nothing))

;;; Sets offset and scaling of the boundsRect to the fillRegion
(define-handler resizeMovieFromFill :private (QuickTimeRenderer) 
  (let ((actorOwner (actorOwner me)))
    (when (and actorOwner (media me))
      (gs:recompute-fill-region actorOwner (gs:node-flags actorOwner))
      (resizeMovieFromBounds me (T_rectRecordToRectArray 
                                 (gs:hrref
                                  (gs:node-fillRegion actorOwner)
                                  :region.rgnbbox :storage :pointer)))))
  (sk8-return-nothing))

;;; Sets offset and scaling of the boundsRect to the frameRegion
(define-handler resizeMovieFromFrame :private (QuickTimeRenderer)
  (let ((actorOwner (actorOwner me)))
    (when (and actorOwner (media me))
      (gs:recompute-frame-region actorOwner (gs:node-flags actorOwner))
      (setf (boundsRect me)
            (T_rectRecordToRectArray (gs:hrref (gs:node-frameRegion actorOwner) :region.rgnbbox :storage :pointer)))))
  (sk8-return-nothing))

(define-handler resizeMovieFromRenderedRegion :private (QuickTimeRenderer)
  (if (frameColorActor me)
    (resizeMovieFromFrame me)
    (resizeMovieFromFill  me))
  (sk8-return-nothing))

(defun enlargeSizeByFrameSize (size frameSize)
  `#(,(+ (elt size 0) (* 2 (elt frameSize 0)))
     ,(+ (elt size 1) (* 2 (elt frameSize 1)))))

(defun enlargeBoxByFrameSize (me box frameSize)
  (let ((resizingStyle (resizingStyle me)))
    (if (or (eq resizingStyle
                'resizeFrameFromMovie)
            (and (eq resizingStyle
                     'resizeRenderedRegionFromMovie)
                 (frameColorActor me)))
      box
      (sk8-multival-bind (frameWidth frameHeight)
                         (sk8-multivals-sequence 2 frameSize)
        `#(,(- (elt box 0) frameWidth)
           ,(- (elt box 1) frameHeight)
           ,(+ (elt box 2) frameWidth)
           ,(+ (elt box 3) frameHeight))))))

(defun reduceBoxByFrameSize (me box frameSize)
  (let ((resizingStyle (resizingStyle me)))
    (if (or (eq resizingStyle
                'resizeFrameFromMovie)
            (and (eq resizingStyle
                     'resizeRenderedRegionFromMovie)
                 (frameColorActor me)))
      box
      (sk8-multival-bind (frameWidth frameHeight)
                         (sk8-multivals-sequence 2 frameSize)
        `#(,(+ (elt box 0) frameWidth)
           ,(+ (elt box 1) frameHeight)
           ,(- (elt box 2) frameWidth)
           ,(- (elt box 3) frameHeight))))))

;;; resizeActorFromMovie
;;;   makes the size of the actor's fillRegion or frameRegion the same as the movie
;;;   and then adjusts the movie's position if necessary.
;;;   If this action doesn't make sense in the current context, then return false, else true
(define-handler resizeActorFromMovie :private (QuickTimeRenderer whichRegion)
  (let ((actorOwner (actorOwner me)))
    (and actorOwner
         (media me)
         (sk8-multival-bind (left top right bottom) (boundsRect me)
           (and (> (- right left) 0)
                (> (- bottom top) 0)))
         (let ((alignment (alignment me)))
           (sk8-multival-bind (newLeft newTop newRight newBottom)
                              (alignSizeToBounds (if (eq whichRegion :frame)
                                                   (preferredSize me)
                                                   (enlargeSizeByFrameSize (preferredSize me)
                                                                           (frameSize actorOwner :physical t)))
                                                 (boundsRect actorOwner :physical t)
                                                 :horizontalAlignment (elt alignment 0)
                                                 :verticalAlignment   (elt alignment 1))
             (if (resizingBox (list newLeft newTop newRight newBottom)
                                (boundsRect actorOwner))
               (withActorLocked (actorOwner)
                 (setBoundsRect actorOwner newLeft newTop newRight newBottom :physical t))
               (moved me)))
           t))))

(define-handler resizeFillFromMovie :private (QuickTimeRenderer)
  (resizeActorFromMovie me :fill)
  (sk8-return-nothing))

(define-handler resizeFrameFromMovie :private (QuickTimeRenderer)
  (resizeActorFromMovie me :frame)
  (sk8-return-nothing))

(define-handler resizeRenderedRegionFromMovie :private (QuickTimeRenderer)
  (if (frameColorActor me)
    (resizeActorFromMovie me :frame)
    (resizeActorFromMovie me :fill ))
  (sk8-return-nothing))

#|
(defun toint (num)
  (if (zerop (mod num 1))
    (floor num)
    num))

(defun testit (box1 box2 ind side)
  (let ((old (elt box1 ind))
        (new (elt box2 ind))
        (diffs '()))
    (cond
     ((> old new)
      (push (format nil "~? > ~A ~A" side '() (toint old) (toint new)) diffs))
     ((< old new)
      (push (format nil "~? < ~A ~A" side '() (toint old) (toint new)) diffs)))
    diffs))

(defun testit2 (oldBox newBox)
  (append (testit oldBox newBox 0 "left"   )
          (testit oldBox newBox 1 "top"    )
          (testit oldBox newBox 2 "right"  )
          (testit oldBox newBox 3 "bottom" )))
  
(defun commentOn (oldBox newBox modBox)
  (trace-print () "~A ~A~%"
      (testit2 oldBox newBox)
      (testit2 newBox modBox)))
|#

#| boy does this do something weird!
;;; first, select r2.  Then try to resize it with the mouse.
(define-handler setBoundsRect (sk8::r2 left top right bottom 
                                        &key physical relative justMoving)
  (setBoundsRect sk8::r1 (- left 200) (- top 200) (- right 200) (- bottom 200)
                  :physical physical :relative relative :justMoving justMoving)
  (sk8-multival-bind (left top right bottom)
                         (boundsRect sk8::r1 :physical physical)
  (call-next-method me left top right bottom
                                        :physical physical :relative relative :justMoving justMoving)))
|#

#|
(defvar old1)
(defvar old2)

(defun t1 (inc)
  (setf old2 (boundsRect b::r1))
  (sk8-multival-bind (left top right bottom)
                     (sk8-multivals-sequence 4 old2)   
    (setboundsrect b::r1 left top right (+ bottom inc)))
  (setf old1 (boundsRect b::r1))
  (gs:boxheight (boundsRect b::r1)))
|#

(define-handler setBoundsRect :private (QuickTimeRenderer left top right bottom &key physical)
  (let (myBoundsRect
        myResizingStyle)
    (cond
     ((not (media me))
      ;; no media
      (sk8-multivals left top right bottom))
     
     ((sk8-multival-bind (oldLeft oldTop oldRight oldBottom)
                         (sk8-multivals-sequence
                          4
                          (setf myBoundsRect (boundsRect (actorOwner me) :physical physical)))
        (and (= (-    right     left)
                (- oldRight  oldLeft))
             (= (-    bottom    top)
                (- oldBottom oldTop))))
      ;; New size is same as old size.
      (sk8-multivals left top right bottom))
     
     ((case (setf myResizingStyle (resizingStyle me))
        ((resizeMovieFromFill
          resizeMovieFromFrame
          ;resizeMovieFromText
          resizeMovieFromRenderedRegion)
         t)
        (otherwise nil))
      ;; resizingStyle wants to resize the movie from the actor
      ;; so whatever size is OK
      (sk8-multivals left top right bottom))
     
     ((case myResizingStyle
        ((resizeFillFromMovie
          resizeFrameFromMovie
          ;resizeMovieFromText
          resizeRenderedRegionFromMovie)
         t)
        (otherwise nil))
      ;; resizingStyle wants to resize the actor from the movie
      ;; If maintainAspectRatio then constrain the actor to aspect ratio.
      ;;               ***********************  Don't resize smaller than (minimumSize actorOwner)
      (if (maintainAspectRatio me)
        (let ((oldWidth  (gs:boxWidth  myBoundsRect))
              (oldHeight (gs:boxHeight myBoundsRect))
              (newWidth  (- right  left))
              (newHeight (- bottom top))
              enlargingOneDimension
              reducingOneDimension
              (changingSize t))
          (cond
           ((= oldWidth newWidth)
            (cond
             ((< oldHeight newHeight)
              (setf enlargingOneDimension t))
             ((> oldHeight newHeight)
              (setf reducingOneDimension t))
             (t
              (setf changingSize nil))))
           ((= oldHeight newHeight)
            (cond
             ((< oldWidth newWidth)
              (setf enlargingOneDimension t))
             ((> oldWidth newWidth)
              (setf reducingOneDimension t))
             (t
              (setf changingSize nil)))))
          (if changingSize
            (let* ((frameSize (frameSize (actorOwner me) :physical physical))
                   (alignment (alignment me))
                   (newBox (roundBoxDimensions (constrainBoundsToAspectRatio
                                                (preferredAspectRatio me)
                                                (reduceBoxByFrameSize me `#(,left ,top ,right ,bottom) frameSize)
                                                :fitOutside (cond
                                                             (enlargingOneDimension t)
                                                             ( reducingOneDimension nil)
                                                             (t                     (eq (maintainAspectRatio me) 'fitOutside)))
                                                :oldBounds (reduceBoxByFrameSize me myBoundsRect frameSize)
                                                :horizontalAlignment (elt alignment 0)
                                                :verticalAlignment   (elt alignment 1)))))
              ;(commentOn myBoundsRect `#(,left ,top ,right ,bottom) (enlargeBoxByFrameSize me newBox frameSize))
              (sk8-multivals-sequence 4 (enlargeBoxByFrameSize me newBox frameSize)))
            (sk8-multivals left top right bottom)))
        (sk8-multivals left top right bottom)))
     
     ((not myResizingStyle)
      ;; resizingStyle is false, so no resizing of Movie will take place
      (sk8-multivals left top right bottom))
     
     (t
      (sk8-multivals left top right bottom)))))

(define-handler rescaled :private (QuickTimeRenderer)
  (call-next-method)
  ;; The following "if" could be optimized out, but it's a place holder
  (if (resizingStyle me)
    (doResizing me)
    (progn
      ;; We should rescale the movie by the appropriate amount; but we don't have enough
      ;; information to do that, so for now we just leave the scaling alone and 
      ;; at least make sure it's properly aligned.
      (doResizing me)))
  (sk8-return-nothing))

;;; setFramesize - adjusts movie to the new framesize
;;;
;;; Called from setFrameSize of actor after the frame has been set
;;;?#
(define-handler setFrameSize :private (QuickTimeRenderer width height &key physical)
  (declare (ignore width height physical))
  (when (media me)
    (doResizing me))
  (sk8-return-nothing))

;;; event delivered to an actor that has been resized
(define-handler resized :private (QuickTimeRenderer)
  (when (actorOwner me)
    (case (resizingStyle me)
      ((resizeMovieFromFrame
        resizeFrameFromMovie)          (resizeMovieFromFrame          me))
      ((resizeMovieFromFill
        resizeFillFromMovie)           (resizeMovieFromFill           me))
      #|((resizeMovieFromText
          resizeTextFromMovie)           (resizeMovieFromText           me)) |#
      ((resizeMovieFromRenderedRegion
        resizeRenderedRegionFromMovie) (resizeMovieFromRenderedRegion me))
      (otherwise                       (moveMovieFromRenderedRegion   me))))
  (sk8-return-nothing))

(define-handler moved :private (QuickTimeRenderer)
  ;; Only do geometric tasks when we're associated with an actor.
  (when (actorOwner me)
    (case (resizingStyle me)
      ((resizeMovieFromFrame
        resizeFrameFromMovie)          (moveMovieFromFrame          me))
      ((resizeMovieFromFill
        resizeFillFromMovie)           (moveMovieFromFill           me))
      #|((resizeMovieFromText
          resizeTextFromMovie)         (moveMovieFromText           me)) |#
      ((resizeMovieFromRenderedRegion
        resizeRenderedRegionFromMovie) (moveMovieFromRenderedRegion me))
      (otherwise                       (moveMovieFromRenderedRegion me))))
  (sk8-return-nothing))

;;;;;;;;;;;;;;;;;
;;; Movie Sequencing
;;;;;;;;;;;;;;;;;

#|
    timeValue - the movie's time value
set timeValue
    active - true if movie is active
set active
    playing - true if movie is active and rate is non-0

    goToBeginning - sends movie to its beginning
    goToEnd - sends movie to its end
    step - steps movie by frame increments either backwards or forwards
    start - starts a movie from the beginning
    stop - stops a movie
    play - plays a movie from its current time

    started - event sent to a movie after it has begun playing
    stopped - event sent to a movie after it has been stopped
|#

(define-handler (setf newStateWhenDone) (newState QuickTimeRenderer)
  (checkLegalValue newState QTRNewStateWhenDoneValues)
  (when (and (eq newState 'stopped)
             (or (usingBadge me)
                 (controllerVisible me)))
    (sk8-error GeneralProgrammaticError
               :strings '("Can't set newStateWhenDone of " " to 'stopped' when either usingBadge or controllerVisible is True.")
               :objects (list me)))
  (setValue 'newStateWhenDone me newState))

(define-handler (setf initialStateForNewMovie) (newState QuickTimeRenderer)
  (checkLegalValue newState (append QTRStateValues nil))
  (setValue 'initialStateForNewMovie me newState))

;;; numeric versions of the state values so we can do magnitude comparisons
;;;? there should also be a 'playingWithNoOtherActivity state

(symbol-macrolet ((is-inactive   0)
                  (is-stopped    1)
                  (is-poster     2)
                  (is-paused     3)
                  (is-playing    4))
  (defun setState (me newState noOtherActivity rateSet &key force)
    (checkLegalValue newState QTRStateValues)
    (assumeControllerGCOSPtr (me myController)
      (let* ((myMedia (media me))
             (oldState (or (state me) (if (and myMedia
                                               (active myMedia))
                                        'stopped
                                        'inactive ;? I doubt this will ever happen, now that we use the controller
                                        ))))
        #|  (print (format nil "change state from ~A to ~A" oldState newState
                     ))  |#
        (unless force
          (when (eq oldState newState)
            (return-from setState newState)))
        
        (setf (previousState me) oldState)
        (setValue 'state me newState)
        (unless myMedia
          (return-from setState newState))
        
        (when-unwind
          (progn
            (let* ((theMovieGCOSPtr (mediaData myMedia))
                   (from-what (case oldState
                                (inactive is-inactive)
                                (stopped  is-stopped)
                                (poster   is-poster)
                                (paused   is-paused)
                                (playing  is-playing)))
                   (to-what (case newState
                              (inactive is-inactive)
                              (stopped  is-stopped)
                              (poster   is-poster)
                              (paused   is-paused)
                              (playing  is-playing)))
                   )
              
              ;; change to active or inactive?
              (cond
               ((eq from-what is-inactive) (setf (active myMedia :dontForwardToRenderer t) t))
               ((eq   to-what is-inactive) (setf (active myMedia :dontForwardToRenderer t) nil)))
              
              ;; start or stop playing?
              (cond
               ((eq to-what is-playing)
                ;; start playing
                (setf (servicing me) t)
                (doRedraw me) ;? We do this to set the clipping (is there a better way?)
                (progn ; without-interrupts ;? is this a good idea? -dy
                  (unless rateSet
                    (preroll me)
                    (actionPlay me (preferredRate myMedia)))
                  (if (or noOtherActivity (playWithNoOtherActivity me))
                    (progn
                      (started me)
                      (unwind-protect
                        (do () ;? perhaps here we should play all movies, not just us
                            ;;? Is this still the right way to do it, now that we're using the controller?
                            ((T_IsMovieDone theMovieGCOSPtr))
                          (actionIdle me))
                        (done me)))
                    (progn
                      (actionIdle me) ;get the ball rolling ASAP
                      (started me)))))
               ((eq from-what is-playing)
                ;; stop playing
                (actionPlay me 0)
                (stopped me :newState newState) ;Send this event to me
                (unless (controllerVisible me) 
                  (setf (servicing me) nil)))
               )
              
              (if (and (eq to-what is-poster)
                       (neq from-what is-poster))
                (setf (timeValue me) (posterTime me))
                (unless (eq to-what is-playing) ; already redrawn
                  (doRedraw me))
                )))
          (setValue 'state me oldState)))
      (state me) ; in case it stopped before we're done
      ))
  )

(define-handler (setf state) (newState QuickTimeRenderer
                                                &key
                                                noOtherActivity)
  (setState me newState noOtherActivity nil)
  )

(define-handler nextInterestingTime
                (QuickTimeRenderer
                 &key
                 startTime
                 rate
                 (flags 0)
                 (mediaSample         nil mediaSampleSet)
                 (mediaEdit           nil mediaEditSet)
                 (trackEdit           nil trackEditSet)
                 (syncSample          nil syncSampleSet)
                 (edgeOK              nil edgeOKSet)
                 (ignoreActiveSegment nil ignoreActiveSegmentSet)
                 (mediaTypes '("eyes"))
                 interestingDurationWanted)
  (assumeMediaWithError (me myMedia)
    (trap-flag-arg flags mediaSample         #$nextTimeMediaSample)
    (trap-flag-arg flags mediaEdit           #$nextTimeMediaEdit)
    (trap-flag-arg flags trackEdit           #$nextTimeTrackEdit)
    (trap-flag-arg flags syncSample          #$nextTimeSyncSample)
    (trap-flag-arg flags edgeOK              #$nextTimeEdgeOK)
    (trap-flag-arg flags ignoreActiveSegment #$nextTimeIgnoreActiveSegment)
    (prog1
      (nextInterestingTime myMedia :dontForwardToRenderer t
                           :startTime startTime
                           :rate rate
                           :flags flags
                           :mediaTypes                mediaTypes
                           :interestingDurationWanted interestingDurationWanted)
      (T_MCMovieChanged (controllerGCOSPtr me) (mediaData myMedia))
      (doRedraw me))))

(define-handler goToBeginning (QuickTimeRenderer)
  (actionGoToTime me 0))

(define-handler goToEnd (QuickTimeRenderer)
  (actionGoToTime me (duration (media me))))

(define-handler timeValue (QuickTimeRenderer)
  (assumeMedia (me myMedia)
    (timeValue myMedia)))

(define-handler (setf timeValue) (newTimeValue QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (actionGoToTime me newTimeValue)
    (timeValue (media me))))

;;; returns the actual number of steps
;;;? there was a bug here.  If you're at the end and you step forward, you go to beginning.  Is it fixed now?
(define-handler sk8:step (QuickTimeRenderer
                                   &key
                                   by
                                   showingEveryFrame)
  (assumeMediaWithError (me myMedia)
    (pause me)
    (let* ((rate         (rate      me))
           (duration     (duration  me))
           (stepsDesired (cond
                          (by                               (require-type by integer))
                          ((not (zerop rate))               (if (> rate 0) 1 -1))
                          ((not (zerop (preferredRate me))) (if (> (preferredRate me) 0) 1 -1))
                          (t                                1))))
      (if showingEveryFrame
        (cond
         ((> stepsDesired 0)
          (dotimes (tmp stepsDesired tmp)
            (when (= duration (timeValue me))
              (return tmp))
            (actionStep me 1)))
         ((< stepsDesired 0)
          (- (dotimes (tmp (- stepsDesired) tmp)
               (when (= 0 (timeValue me))
                 (return tmp))
               (actionStep me -1))))
         (t
          ))
        (actionStep me stepsDesired)))
    (sk8-return-nothing)))

;;; a convenience function
(define-handler play (QuickTimeRenderer &key noOtherActivity)
  (setf (state me :noOtherActivity noOtherActivity) 'playing)
  (sk8-return-nothing))

;;; a convenience function
(define-handler stop (QuickTimeRenderer)
  (setf (state me) 'stopped)
  (sk8-return-nothing))

;;; a convenience function
(define-handler showPoster (QuickTimeRenderer)
  (setf (state me) 'poster)
  (sk8-return-nothing))

;;; a convenience function
(define-handler pause (QuickTimeRenderer)
  (setf (state me) 'paused)
  (sk8-return-nothing))

(define-handler start (QuickTimeRenderer &key noOtherActivity)
  (goToBeginning me)
  (setf (state me :noOtherActivity noOtherActivity) 'playing)
  (sk8-return-nothing))

(define-handler movieChanged :private (QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (T_MCMovieChanged myController (mediaData (media me)))
    (sk8-return-nothing)))

;;? test
(define-handler (setf rate) (rate QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (if (zerop rate)
      (pause me)
      (progn
        (preroll me :rate rate)
        (actionPlay me rate)
        (setState me 'playing nil t))))
  rate)

#|
(define-handler (setf pauseWithHighQuality) (newValue QuickTimeRenderer)
  (when (xor newValue (pauseWithHighQuality me))
    (setValue 'pauseWithHighQuality me newValue)
    (when (eq (state me) 'paused)
      (doRedraw me)))
  newValue)
|#

;;;;;;;;;;;;;;;;
;;; These handlers are called at important times
;;;;;;;;;;;;;;;;

;;; called when a movie has been put into playing mode.

(define-handler started (QuickTimeRenderer)
  (sk8-return-nothing))

;;; called when a movie has been takein out of playing mode.

(define-handler stopped (QuickTimeRenderer &key newState)
  (declare (ignore newState))
  (sk8-return-nothing))

;;; When you specialize this, do inherited first!
(define-handler done (QuickTimeRenderer)
  (let ((newStateWhenDone (newStateWhenDone me)))
    (setf (state me) (case newStateWhenDone
                       (previous (previousState me))
                       ((inactive stopped poster paused)
                        newStateWhenDone)
                       (otherwise 'stopped)))) ;shouldn't happen
  (sk8-return-nothing))

(define-handler cutSelection (QuickTimeRenderer)
  (assumeMediaWithError (me myMedia)
    (prog1
      (cutSelection myMedia :dontForwardToRenderer t)
      (doRedraw me))))

(define-handler pasteSelection (QuickTimeRenderer sourceMovie)
  (assumeMediaWithError (me myMedia)
    (prog1
      (pasteSelection myMedia sourceMovie :dontForwardToRenderer t)
      (doRedraw me))))


;;;;;;;;;;;;;;;;
;;; Movie Drawing
;;;;;;;;;;;;;;;;

#|
    unselect - called by SK8 whenever an actor is unselected by user ;? not implemented
    updateMovie - called by SK8 graphics system when window must be updated ;? not implemented
    gWorld - returns the movie's gWorld
set gWorld - sets the movie's gWorld
        (called by initialize when an actor is added to a window)
        ;? not implemented as a setter
    isPointInMovie - returns true if the point is within the movie's display area
|#

;;; This could be specialized on QuickTimeMovie instead of QuickTimeRenderer, but it's not meaningful there
;;; because it's only when the movie has a renderer that its GWorld is set up proplerly.
(define-handler gWorld :private (QuickTimeRenderer)
  (assumeControllerGCOSPtr (me myController)
    (T_MCGetControllerPort myController)))

(define-handler (setf gWorld) :private (newGWorld QuickTimeRenderer)
  (assumeControllerGCOSPtr (me myController)
    (unless newGWorld
      (let ((actorOwner (actorOwner me)))
        (when actorOwner
          ;; Use the wptr.
          (let ((osWindow (gs:node-window actorOwner)))
            (unless (and osWindow (wptr osWindow))
              (sk8-error GeneralError
                         :strings '("No window associated with movie ")
                         :objects (list me)))
            (setf newGWorld (wptr osWindow))))))
    (if newGWorld
      (progn
        ;; SetMovieGWorld is expensive so we take some pains to avoid it unless necessary
        (unless (eql newGWorld (T_MCGetControllerPort myController))
          (T_MCSetControllerPort myController newGWorld))
        newGWorld)
      ;; This should never happen.
      (T_MCGetControllerPort myController))))

#|
            (with-rgb (theColor (mcl-color (controllerForeColor me)))
              (#_RGBForeColor theColor))
            (with-rgb (theColor (mcl-color (controllerBackColor me)))
              (#_RGBBackColor theColor))
|#

(define-handler clipRegionGCHandle :private (QuickTimeRenderer)
  (assumeControllerGCOSPtr (me myController)
    (sk8-multival-bind (theClip movieClip)
                       (T_MCGetClipGC myController :movieClipGCHandle nil)
      (declare (ignore movieClip))
      theClip)))

(define-handler (setf clipRegionGCHandle) (newValue QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (T_MCSetClip myController :movieClip newValue)))

(define-handler movieClipRegionGCHandle :private (QuickTimeRenderer)
  (assumeControllerGCOSPtr (me myController)
    (sk8-multival-bind (theClip movieClip)
                       (T_MCGetClipGC myController :movieClipGCHandle nil)
      (declare (ignore theClip))
      movieClip)))
#|
(define-handler (setf controllerForeColor) (newValue QuickTimeRenderer)
  (ensureType newValue RGBColor)
  (setValue 'controllerForeColor me newValue)
  (doRedraw me))

(define-handler (setf controllerBackColor) (newValue QuickTimeRenderer)
  (ensureType newValue RGBColor)
  (setValue 'controllerBackColor me newValue)
  (doRedraw me))
|#

(defmacro doBackedQTRBooleanProperty (todo property &body body)
  (cond
   ((eq todo :set)
    `(progn
       (setf newValue (and newValue t))
       (let ((myController (controllerGCOSPtr me)))
         (when myController
           ,@body)
         (setValue ,property me newValue))))
   ((eq todo :get)
    `(let ((myController (controllerGCOSPtr me)))
       (if myController
         (progn ,@body)
         (getValue ,property me))))
   (t
    (error "first argument must be either :set or :get"))))

(define-handler (setf reservingRoomForController) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'reservingRoomForController
    (when (xor newValue (reservingRoomForController me))
      (setValue 'reservingRoomForController me newValue)
      (doResizing me))))


(define-handler (setf controllerVisible) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'controllerVisible
    (T_MCSetVisible myController newValue)
    (setValue 'controllerVisible me newValue)
    (when newValue
      (when (eq (newStateWhenDone me) 'stopped)
        (setf (newStateWhenDone me) 'paused))
      (when (eq (state me) 'stopped)
        (setf (state me) 'paused)))
    (doResizing me)
    (make-sure-movie-is-serviced-if-it-needs-to me newValue newValue)))

(define-handler controllerVisible (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'controllerVisible
    (T_MCGetVisible myController)))


(define-handler (setf usingBadge) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'usingBadge
    (actionSetUseBadge me newValue)
    (if (setValue 'usingBadge me newValue)
      (progn
        (when (eq (newStateWhenDone me) 'stopped)
          (setf (newStateWhenDone me) 'paused))
        (doResizing me) ; This will move it to the new alignment.
        )
      (doRedraw me))))

(define-handler usingBadge (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'usingBadge
    (actionGetUseBadge me)))


(define-handler (setf keysEnabled) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'keysEnabled
    (actionSetkeysEnabled me newValue)))

(define-handler keysEnabled (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'keysEnabled
    (actionGetkeysEnabled me)))


(defmacro replace-bit-if (newValue destination-integer bit-integer)
  `(boole (if ,newValue boole-ior boole-andc2)
          ,destination-integer
          ,bit-integer))

(define-handler (setf stepButtonsVisible) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'stepButtonsVisible
    (actionSetFlags me (replace-bit-if (not newValue)
                                       (actionGetFlags me)
                                       #$mcFlagSuppressStepButtons))
    (actionDraw me (gWorld me))))

(define-handler stepButtonsVisible (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'stepButtonsVisible
    (zerop (logand (actionGetFlags me)
                   #$mcFlagSuppressStepButtons))))


(define-handler (setf speakerButtonVisible) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'speakerButtonVisible
    (actionSetFlags me (replace-bit-if (not newValue)
                                       (actionGetFlags me)
                                       #$mcFlagSuppressSpeakerButton))
    (actionDraw me (gWorld me))))

(define-handler speakerButtonVisible (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'speakerButtonVisible
    (zerop (logand (actionGetFlags me)
                   #$mcFlagSuppressSpeakerButton))))


(define-handler (setf usingWindowPalette) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'usingWindowPalette
    (actionSetFlags me (replace-bit-if newValue
                                       (actionGetFlags me)
                                       #$mcFlagsUseWindowPalette))
    (actionDraw me (gWorld me))))


(define-handler usingWindowPalette (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'usingWindowPalette
    (not (zerop (logand (actionGetFlags me)
                        #$mcFlagsUseWindowPalette)))))


(define-handler (setf doingInvalidating) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'doingInvalidating
    (actionSetFlags me (replace-bit-if (not newValue)
                                       (actionGetFlags me)
                                       #$mcFlagsDontInvalidate))))

(define-handler doingInvalidating (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'doingInvalidating
    (zerop (logand (actionGetFlags me)
                   #$mcFlagsDontInvalidate))))


(define-handler (setf playingEveryFrame) (newValue QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :set 'playingEveryFrame
    (actionSetplayEveryFrame me newValue)))

(define-handler playingEveryFrame (QuickTimeRenderer)
  (doBackedQTRBooleanProperty
    :get 'playingEveryFrame
    (actionGetplayEveryFrame me)))


(define-handler repeating (QuickTimeRenderer)
  (assumeControllerGCOSPtr (me myController)
    (cond
     ((actionGetLooping me)
      (if (actionGetLoopIsPalindrome me)
        'palindrome
        'SK8::loop))
     nil)))

(define-handler (setf repeating) (newRepeating QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (cond
     ((eq newRepeating 'SK8::loop)  (actionSetLoopIsPalindrome me nil) (actionSetLooping me t))
     ((eq newRepeating 'palindrome) (actionSetLoopIsPalindrome me t)   (actionSetLooping me t))
     ((eq newRepeating nil)         (actionSetLoopIsPalindrome me nil) (actionSetLooping me nil))
     (t (error "invalid repeating style"))))
  newRepeating)

#|
(define-handler displayBoundsRegion (QuickTimeRenderer)
	(prog1 (#_GetMovieDisplayBoundsRgnGC (mediaData myMedia))
	  (checkMovieError)))

(define-handler movieSegmentDisplayBoundsRegion (QuickTimeRenderer segment)
	(prog1 (#_GetMovieSegmentDisplayBoundsRgnGC (mediaData myMedia) (first segment) (second segment))
	  (checkMovieError)))

(define-handler isPointInMovie (QuickTimeRenderer x y)
	(prog1 (#_PtInMovie (mediaData myMedia) (gs:SK8Coords-to-point x y))
	  (checkMovieError)))

|#

;;;;;;;;;
;;; Poster
;;;;;;;;;

(define-handler (setf posterTime) (newPosterTime QuickTimeRenderer)
  (assumeMediaWIthError (me myMedia)
    (prog1
      (setf (posterTime myMedia :dontForwardToRenderer t) newPosterTime)
      (doRedraw me))))

;;;------------------------------------------------------------------------------------------
;;; Action filters and Actions

(defmacro defActionHandler (action &rest args)
  `(define-handler ,(intern (string-upcase action) cl::*package*) :private (QuickTimeRenderer ,@args)
                   (assumeControllerGCOSPtrWithError (me myController)
                     (T_MCDoAction myController ,(intern (concatenate 'string "TC_MC" (string-upcase action)) cl::*package*) ,@args))))

(defmacro defActionHandlerSetBoolean (action)
  `(define-handler ,(intern (string-upcase action) cl::*package*) :private (QuickTimeRenderer value)
                   (assumeControllerGCOSPtrWithError (me myController)
                     (T_MCDoAction myController ,(intern (concatenate 'string "TC_MC" (string-upcase action)) cl::*package*) (%lisp-to-boolean value)))))

(defmacro defActionHandlerGetBoolean (action)
  `(define-handler ,(intern (string-upcase action) cl::*package*) :private (QuickTimeRenderer)
                   (assumeControllerGCOSPtrWithError (me myController)
                     (rlet ((valuePtr :boolean))
                       (T_MCDoAction myController ,(intern (concatenate 'string "TC_MC" (string-upcase action)) cl::*package*) valuePtr)
                       (%get-boolean valuePtr)))))

;;;===============

(defActionHandler "actionIdle")

(define-handler actionIdleEvent :private (QuickTimeRenderer)
                nil)

;;;===============
#|
    (with-rgb (theColor (mcl-color (controllerForeColor me)))
      (#_RGBForeColor theColor))
    (with-rgb (theColor (mcl-color (controllerBackColor me)))
      (#_RGBBackColor theColor))
|#

(define-handler actionDraw :private (QuickTimeRenderer paper)
  (trace-print-bracketing ("draw" :debug-actions)
    (locking-action-event (actionDraw)
      (assumeControllerGCOSPtrWithError (me myController)
        (let ((actorOwner (actorOwner me)))
          (when actorOwner
            (trace-print (:debug-actions) "draw ok")
            (T_MCDoAction myController TC_mcActionDraw paper)))))))

(define-handler actionDrawEvent :private (QuickTimeRenderer)
  (unless-locking-action-event (actionDraw)
    (trace-print (:debug-actions) "drawEvent ok")
    (doRedraw me))
  nil)

;;;===============

(defactionhandler "actionActivate")

(define-handler actionActivateEvent :private (QuickTimeRenderer)
  nil)

;;;===============

(defActionHandler "actionDeactivate")

(define-handler actionDeactivateEvent :private (QuickTimeRenderer)
  nil)

;;;===============

(defActionHandler "actionMouseDown" eventPtr)

(define-handler actionMouseDownEvent :private (QuickTimeRenderer eventPtr)
  (declare (ignore eventPtr))
  nil)

;;;===============

(defActionHandler "actionKey" eventPtr)

(define-handler actionKeyEvent :private (QuickTimeRenderer eventPtr)
  (declare (ignore eventPtr))
  nil)

;;;===============

(define-handler actionPlay :private (QuickTimeRenderer rate)
  (trace-print-bracketing (`("play ~D" ,rate) :debug-actions)
    (locking-action-event (actionPlay)
      (assumeControllerGCOSPtrWithError (me myController)
        (trace-print (:debug-actions) "play ok")
        (T_MCDoAction myController TC_mcActionPlay (T_numberToFixed rate))))))

(define-handler actionPlayEvent :private (QuickTimeRenderer rate)
  (locking-action-event (actionPlay)
    (trace-print (:debug-actions) "playEvent ok ~S" (state me))
    (if (zerop rate)
      (progn
        (setValue 'state me 'paused)
        (stopped me :newState 'paused)
        (doRedraw me))
      (setValue 'state me 'playing))
    )
  nil)

;;;===============

(define-handler actionGoToTime :private (QuickTimeRenderer timeValue)
  (assumeControllerGCOSPtrWithError (me myController)
    (trace-print-bracketing (`("GoToTime ~D" ,timeValue) :debug-actions)
      (rlet ((timeRec :TimeRecord))
        (checking-toolbox-error (:Value (#_GetMoviesError))
                                (#_GetMovieTime (mediaData (media me)) timeRec))
        (rsetTimeValue64 timeRec timeValue)
        (T_MCDoAction myController TC_mcActionGoToTime timeRec)))))

(define-handler actionGoToTimeEvent :private (QuickTimeRenderer timeValue)
  (declare (ignore timeValue))
  nil)

;;;===============

(define-handler actionSetVolume :private (QuickTimeRenderer volume)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((volumePtr :integer))
      (%put-word volumePtr (T_numberToShortFixed volume))
      (T_MCDoAction myController TC_mcActionSetVolume volumePtr)))
  (actionGetVolume me))

(define-handler actionSetVolumeEvent :private (QuickTimeRenderer volume)
  (declare (ignore volume))
  nil)

;;;=======

(define-handler actionGetVolume :private (QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((volumePtr :integer))
      (T_MCDoAction myController TC_mcActionGetVolume volumePtr)
      (T_ShortFixedToNumber (%get-word volumePtr)))))

(define-handler actionGetVolumeEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(define-handler actionStep :private (QuickTimeRenderer &optional (steps 1))
  (assumeControllerGCOSPtrWithError (me myController)
    (T_MCDoAction myController TC_mcActionStep steps)))

(define-handler actionStepEvent :private (QuickTimeRenderer steps)
  (declare (ignore steps))
  nil)

;;;===============

(defActionHandlerSetBoolean "actionSetLooping")

;;;? test
(define-handler actionSetLoopingEvent :private (QuickTimeRenderer loop)
  (declare (ignore loop))
  #|(assumeMediaOrNil (me myMedia)
    (when (xor (repeating myMedia) loop)
      (if loop
        (setvalue 'repeating myMedia 'sk8::loop)
        (setvalue 'repeating myMedia nil))))|#
  nil)

;;;=======

(defActionHandlerGetBoolean "actionGetLooping")

(define-handler actionGetLoopingEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(defActionHandlerSetBoolean "actionSetLoopIsPalindrome")

;;;? test
(define-handler actionSetLoopIsPalindromeEvent :private (QuickTimeRenderer loopIsPalindrome)
  (declare (ignore loopIsPalindrome))
  #|(assumeMediaOrNil (me myMedia)
    (if loopIsPalindrome
      (setvalue 'repeating myMedia 'palindrome)
      (if (eq (repeating myMedia) 'palindrome)
        (setvalue 'repeating myMedia 'sk8::loop))))|#
  nil)

;;;=======

(defActionHandlerGetBoolean "actionGetLoopIsPalindrome")

(define-handler actionGetLoopIsPalindromeEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(defActionHandler "actionSetGrowBoxBounds" box)

;;? is this really an event?  IM-QTC doesn't say so
(define-handler actionSetGrowBoxBoundsEvent :private (QuickTimeRenderer boundsBox)
  (declare (ignore boundsBox))
  nil)

;;;===============

(define-handler actionControllerSizeChangedEvent :private (QuickTimeRenderer)
  (unless-locking-action-event (actionDraw)
    (trace-print (:debug-actions) "drawEvent ok")
    (doResizing me))
  nil)

;;;===============

(define-handler actionSetSelectionBegin :private (QuickTimeRenderer timeValue)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((timeRec :TimeRecord))
      (checking-toolbox-error (:Value (#_GetMoviesError))
                              (#_GetMovieTime (mediaData (media me)) timeRec))
      (rsetTimeValue64 timeRec timeValue)
      (T_MCDoAction myController TC_mcActionSetSelectionBegin timeRec))))

(define-handler actionSetSelectionBeginEvent :private (QuickTimeRenderer timeValue)
  (declare (ignore timeValue))
  nil)

;;;===============

(define-handler actionSetSelectionDuration :private (QuickTimeRenderer timeValue)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((timeRec :TimeRecord))
      (checking-toolbox-error (:Value (#_GetMoviesError))
                              (#_GetMovieTime (mediaData (media me)) timeRec))
      (rsetTimeValue64 timeRec timeValue)
      (T_MCDoAction myController TC_mcActionSetSelectionDuration timeRec))))

(define-handler actionSetSelectionDurationEvent :private (QuickTimeRenderer timeValue)
  (declare (ignore timeValue))
  nil)

;;;===============

(defActionHandlerSetBoolean "actionSetKeysEnabled")

(define-handler actionSetKeysEnabledEvent :private (QuickTimeRenderer keysEnabled)
  (declare (ignore keysEnabled))
  #|(setf (keysEnabled me) keysEnabled)
  t|#
  nil)

;;;=======

(defActionHandlerGetBoolean "actionGetKeysEnabled")

(define-handler actionGetKeysEnabledEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(defActionHandlerSetBoolean "actionSetPlaySelection")

(define-handler actionSetPlaySelectionEvent :private (QuickTimeRenderer playSelectionOnly)
  (declare (ignore playSelectionOnly))
  #|(setf (playSelectionOnly me) playSelectionOnly)
  t|#
  nil)

;;;=======

(defActionHandlerGetBoolean "actionGetPlaySelection")

(define-handler actionGetPlaySelectionEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(defActionHandlerSetBoolean "actionSetUseBadge")

(define-handler actionSetUseBadgeEvent :private (QuickTimeRenderer useBadge)
  (declare (ignore useBadge))
  #|(setf (usingBadge me) useBadge)
  t|#
  nil)

;;;=======

(defActionHandlerGetBoolean "actionGetUseBadge")

(define-handler actionGetUseBadgeEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(define-handler actionSetFlags :private (QuickTimeRenderer flags)
  (assumeControllerGCOSPtrWithError (me myController)
    (T_MCDoAction myController TC_mcActionSetFlags flags))
  (actionGetFlags me))

(define-handler actionSetFlagsEvent :private (QuickTimeRenderer flags)
  (declare (ignore flags))
  #|(setf (flags me) flags)|#
  nil)

;;;=======

(define-handler actionGetFlags :private (QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((flagsPtr :unsigned-long))
      (T_MCDoAction myController TC_mcActionGetFlags flagsPtr)
      (%get-unsigned-long flagsPtr))))

(define-handler actionGetFlagsEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))


;;;===============

(defActionHandlerSetBoolean "actionSetPlayEveryFrame")

(define-handler actionSetPlayEveryFrameEvent :private (QuickTimeRenderer playEveryFrame)
  (declare (ignore playEveryFrame))
  #|(setf (playEveryFrame me) playEveryFrame)
  t|#
  nil)

;;;=======

(defActionHandlerGetBoolean "actionGetPlayEveryFrame")

(define-handler actionGetPlayEveryFrameEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(define-handler actionGetPlayRate :private (QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((ratePtr :long))
      (T_MCDoAction myController TC_mcActionGetFlags ratePtr)
      (T_fixedToNumber (%get-word ratePtr)))))

;;;===============

(define-handler actionShowBalloonEvent :private (QuickTimeRenderer showBalloon)
  (declare (ignore showBalloon))
  nil)

;;;===============

(define-handler actionBadgeClickEvent :private (QuickTimeRenderer clicked)
  (declare (ignore clicked))
  (sk8-multivals nil nil))

;;;===============

(define-handler actionMovieClickEvent :private (QuickTimeRenderer clicked)
  (declare (ignore clicked))
  (sk8-multivals nil nil))

;;;===============

(define-handler actionSuspendEvent :private (QuickTimeRenderer)
  nil)

;;;===============

(define-handler actionResumeEvent :private (QuickTimeRenderer)
  nil)

;;;===============

(defActionHandlerSetBoolean "actionSetControllerKeysEnabled") ;; Hoddie says not used

;;;===============

;;; returns array
(define-handler actionGetTimeSliderRect :private (QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (let ((rectPtr :rect))
      (T_MCDoAction myController TC_mcActionGetTimeSliderRect rectPtr)
      (T_rectRecordToRectArray rectPtr))))

;;;===============

(define-handler actionMovieEditedEvent :private (QuickTimeRenderer)
  nil)

;;;===============

(defActionHandlerSetBoolean "actionSetDragEnabled") ;? probably wrong

(define-handler actionSetDragEnabledEvent :private (QuickTimeRenderer dragEnabled)
  (declare (ignore dragEnabled))
  #|(setf (dragEnabled me) dragEnabled)
  t|#
  nil)

;;;=======

(defActionHandlerGetBoolean "actionGetDragEnabled") ;? probably wrong

(define-handler actionGetDragEnabledEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

;;;? is this right?
(define-handler actionGetSelectionBegin :private (QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((timeRec :TimeRecord))
      (T_MCDoAction myController TC_mcActionGetSelectionBegin timeRec)
      (rrefTimeValue64 timeRec))))

;;;? is this right?
(define-handler actionGetSelectionDuration :private (QuickTimeRenderer)
  (assumeControllerGCOSPtrWithError (me myController)
    (rlet ((timeRec :TimeRecord))
      (T_MCDoAction myController TC_mcActionGetSelectionDuration timeRec)
      (rrefTimeValue64 timeRec))))

;;;===============

(define-handler actionGetSelectionBeginEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(define-handler actionGetSelectionDurationEvent :private (QuickTimeRenderer)
  (sk8-multivals nil nil))

;;;===============

(define-handler actionPrerollAndPlayEvent :private (QuickTimeRenderer rate)
  (locking-action-event (actionPlay)
   (trace-print (:debug-actions) "playEvent ok ~S" (state me))
   (if (zerop rate)
     (progn
       (setValue 'state me 'paused)
       (stopped me :newState 'paused)
       (doRedraw me))
     (setValue 'state me 'playing))
   )
  nil)

;;;===============

(define-handler actionFilter :private (QuickTimeRenderer theMovieController action params)
  (declare (ignore theMovieController))
  (let* ((paramValue params)
         actionSymbol)
    (when-debug (:debug-actionFilter)
      (setf actionSymbol
            (case action
              (#.TC_mcActionIdle                  (setf paramValue :none)                                     #|'Idle|#       'dontShow)
              (#.TC_mcActionDraw                  (setf paramValue :none)                                       'Draw)
              (#.TC_mcActionActivate              (setf paramValue :none)                                       'Activate)
              (#.TC_mcActionDeactivate            (setf paramValue :none)                                       'Deactivate)
              (#.TC_mcActionMouseDown             (setf paramValue :none)                                     #|'MouseDown|#  'dontShow)
              (#.TC_mcActionKey                   (setf paramValue :none)                                     #|'Key|#        'dontShow)
              (#.TC_mcActionPlay                  (setf paramValue (T_fixedToNumber (%ptr-to-int params)))      'Play)
              (#.TC_mcActionGotoTime              (setf paramValue (rrefTimeValue64 params))                    'GotoTime)
              (#.TC_mcActionSetVolume             (setf paramValue (T_shortFixedToNumber (%get-word params)))   'SetVolume)
              (#.TC_mcActionGetVolume             (setf paramValue :none)                                       'GetVolume)
              (#.TC_mcActionStep                  (setf paramValue (%ptr-to-int params))                        'Step)
              (#.TC_mcActionSetLooping            (setf paramValue (%boolean-to-lisp (%ptr-to-int params)))     'SetLooping)
              (#.TC_mcActionGetLooping            (setf paramValue :none)                                       'GetLooping)
              (#.TC_mcActionSetLoopIsPalindrome   (setf paramValue (%boolean-to-lisp (%ptr-to-int params)))     'SetLoopIsPalindrome)
              (#.TC_mcActionGetLoopIsPalindrome   (setf paramValue :none)                                       'GetLoopIsPalindrome)
              (#.TC_mcActionControllerSizeChanged (setf paramValue :none)                                       'ControllerSizeChanged)
              (#.TC_mcActionSetSelectionBegin     (setf paramValue (rrefTimeValue64 params))                    'SetSelectionBegin)
              (#.TC_mcActionSetSelectionDuration  (setf paramValue (rrefTimeValue64 params))                    'SetSelectionDuration)
              (#.TC_mcActionSetKeysEnabled        (setf paramValue (%boolean-to-lisp (%ptr-to-int params)))     'SetKeysEnabled)
              (#.TC_mcActionGetKeysEnabled        (setf paramValue :none)                                       'GetKeysEnabled)
              (#.TC_mcActionSetPlaySelection      (setf paramValue (%boolean-to-lisp (%ptr-to-int params)))     'SetPlaySelection)
              (#.TC_mcActionGetPlaySelection      (setf paramValue :none)                                       'GetPlaySelection)
              (#.TC_mcActionSetUseBadge           (setf paramValue (%boolean-to-lisp (%ptr-to-int params)))     'SetUseBadge)
              (#.TC_mcActionGetUseBadge           (setf paramValue :none)                                       'GetUseBadge)
              (#.TC_mcActionSetFlags              (setf paramValue (%get-long params))                          'SetFlags)
              (#.TC_mcActionGetFlags              (setf paramValue :none)                                       'GetFlags)
              (#.TC_mcActionSetPlayEveryFrame     (setf paramValue (%boolean-to-lisp (%ptr-to-int params)))     'SetPlayEveryFrame)
              (#.TC_mcActionGetPlayEveryFrame     (setf paramValue :none)                                       'GetPlayEveryFrame)
              (#.TC_mcActionShowBalloon           (setf paramValue (%get-boolean params))                       'ShowBalloon)
              (#.TC_mcActionSetGrowBoxBounds      (setf paramValue "rect")                                      'SetGrowBoxBounds)
              (#.TC_mcActionBadgeClick            (setf paramValue (%get-boolean params))                       'BadgeClick)
              (#.TC_mcActionMovieClick            (setf paramValue (%get-boolean params))                       'MovieClick)
              (#.TC_mcActionSuspend               (setf paramValue :none)                                       'Suspend)
              (#.TC_mcActionResume                (setf paramValue :none)                                       'Resume)
              (#.TC_mcActionMovieEdited                                                                         'MovieEdited)
              (#.TC_mcActionSetDragEnabled        (setf paramValue (%boolean-to-lisp (%ptr-to-int params)))     'SetDragEnabled)
              (#.TC_mcActionGetDragEnabled        (setf paramValue :none)                                       'GetDragEnabled)
              (#.TC_mcActionGetSelectionBegin     (setf paramValue :none)                                       'GetSelectionBegin)
              (#.TC_mcActionGetSelectionDuration  (setf paramValue :none)                                       'GetSelectionDuration)
              (#.TC_mcActionPrerollAndPlay        (setf paramValue (T_fixedToNumber (%ptr-to-int params)))      'PrerollAndPlay)
              (otherwise                          action))))
    (trace-print-bracketing ((and (neq actionSymbol 'dontShow)
                                  (if (eq paramValue :none)
                                    `("actionEvent ~a~%"    ,actionSymbol)
                                    `("actionEvent ~a ~a~%" ,actionSymbol ,paramValue)))
                             :debug-actionFilter)
      (case action
        (#.TC_mcActionIdle                                     (actionIdleEvent                  me))
        (#.TC_mcActionDraw                                     (actionDrawEvent                  me))
        (#.TC_mcActionActivate                                 (actionActivateEvent              me))
        (#.TC_mcActionDeactivate                               (actionDeactivateEvent            me))
        (#.TC_mcActionMouseDown                                (actionMouseDownEvent             me  params))
        (#.TC_mcActionKey                                      (actionKeyEvent                   me  params))
        (#.TC_mcActionPlay                                     (actionPlayEvent                  me  (T_fixedToNumber (%ptr-to-int params))))
        (#.TC_mcActionGotoTime                                 (actionGotoTimeEvent              me  (rrefTimeValue64 params)))
        (#.TC_mcActionSetVolume                                (actionSetVolumeEvent             me  (T_shortFixedToNumber (%ptr-to-int params))))
        (#.TC_mcActionGetVolume             (sk8-multival-bind
                                              (handled result) (actionGetVolumeEvent             me)
                                              (when handled                                          (%put-word params result))
                                              handled))
        (#.TC_mcActionStep                                     (actionStepEvent                  me  (%ptr-to-int params)))
        (#.TC_mcActionSetLooping                               (actionSetLoopingEvent            me  (%boolean-to-lisp (%ptr-to-int params))))
        (#.TC_mcActionGetLooping            (sk8-multival-bind
                                              (handled result) (actionGetLoopingEvent            me)
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionSetLoopIsPalindrome                      (actionSetLoopIsPalindromeEvent   me  (%boolean-to-lisp (%ptr-to-int params))))
        (#.TC_mcActionGetLoopIsPalindrome   (sk8-multival-bind
                                              (handled result) (actionGetLoopIsPalindromeEvent   me)
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionControllerSizeChanged                    (actionControllerSizeChangedEvent me))
        (#.TC_mcActionSetSelectionBegin                        (actionSetSelectionBeginEvent     me  (rrefTimeValue64 params)))
        (#.TC_mcActionSetSelectionDuration                     (actionSetSelectionDurationEvent  me  (rrefTimeValue64 params)))
        (#.TC_mcActionSetKeysEnabled                           (actionSetKeysEnabledEvent        me  (%boolean-to-lisp (%ptr-to-int params))))
        (#.TC_mcActionGetKeysEnabled        (sk8-multival-bind
                                              (handled result) (actionGetKeysEnabledEvent        me)
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionSetPlaySelection                         (actionSetPlaySelectionEvent      me  (%boolean-to-lisp (%ptr-to-int params))))
        (#.TC_mcActionGetPlaySelection      (sk8-multival-bind
                                              (handled result) (actionGetPlaySelectionEvent      me)
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionSetUseBadge                              (actionSetUseBadgeEvent           me  (%boolean-to-lisp (%ptr-to-int params))))
        (#.TC_mcActionGetUseBadge           (sk8-multival-bind
                                              (handled result) (actionGetUseBadgeEvent           me)
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionSetFlags                                 (actionSetFlagsEvent              me  (%ptr-to-int params)))
        (#.TC_mcActionGetFlags              (sk8-multival-bind
                                              (handled result) (actionGetFlagsEvent              me)
                                              (when handled                                          (%put-long params result))
                                              handled))
        (#.TC_mcActionSetPlayEveryFrame                        (actionSetPlayEveryFrameEvent     me  (%boolean-to-lisp (%ptr-to-int params))))
        (#.TC_mcActionGetPlayEveryFrame     (sk8-multival-bind
                                              (handled result) (actionGetPlayEveryFrameEvent     me)
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionShowBalloon                              (actionShowBalloonEvent           me  (%get-boolean params)))
        (#.TC_mcActionSetGrowBoxBounds                         (actionSetGrowBoxBoundsEvent      me  params))
        (#.TC_mcActionBadgeClick            (sk8-multival-bind
                                              (handled result) (actionBadgeClickEvent            me  (%get-boolean params))
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionMovieClick            (sk8-multival-bind
                                              (handled result) (actionMovieClickEvent            me  (%get-boolean params))
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionSuspend                                  (actionSuspendEvent               me))
        (#.TC_mcActionResume                                   (actionResumeEvent                me))
        (#.TC_mcActionMovieEdited                              (actionMovieEditedEvent           me))
        (#.TC_mcActionSetDragEnabled                           (actionSetDragEnabledEvent        me)  (%boolean-to-lisp (%ptr-to-int params)))
        (#.TC_mcActionGetDragEnabled        (sk8-multival-bind
                                              (handled result) (actionGetDragEnabledEvent        me)
                                              (when handled                                          (%put-boolean params result))
                                              handled))
        (#.TC_mcActionGetSelectionBegin     (sk8-multival-bind
                                              (handled result) (actionGetSelectionBeginEvent     me)
                                              (when handled                                          (rsetTimeValue64 params result))
                                              handled))
        (#.TC_mcActionGetSelectionDuration  (sk8-multival-bind
                                              (handled result) (actionGetSelectionDurationEvent  me)
                                              (when handled                                          (rsetTimeValue64 params result))
                                              handled))
        (#.TC_mcActionPrerollAndPlay                           (actionPrerollAndPlayEvent        me  (T_fixedToNumber (%ptr-to-int params))))

        (otherwise                                           nil)))))


(defvar QTRVirtualProperties '(servicing
                                   gWorld
                                   visible
                                   onStage
                                   preferredAspectRatio
                                   ))

(define-handler localVirtualProperties (QuickTimeRenderer)
   (if (eq me QuickTimeRenderer)
     QTRVirtualProperties
     '()))

;;; -----------------------------------
;;; PRESERVING AND RESTORING QuickTimeRenderer
;;; -----------------------------------

(define-handler localPropertiesToSaveAsFalse (QuickTimeRenderer)
  (and (eq me QuickTimeRenderer)
       '(controllerGCOSPtr
         )))

(define-handler localPropertiesToSaveSpecially (QuickTimeRenderer)
  (and (eq me QuickTimeRenderer)
       '(state
         previousState)))

(define-handler saveToStore :private (QuickTimeRenderer property)
  (case property
    (state
     (if (eq (state me) 'playing)
       'paused
       (state me)))
    (previousState
     'paused)
    (otherwise
     (call-next-method))))

(define-handler preserve :private (QuickTimeRenderer)
  (call-next-method)
  ;; (preserveLikeSave me)
  (sk8-return-nothing))

(define-handler restoreOrInitializeFromStore :private (QuickTimeRenderer whichFunction)
  (trace-print-bracketing ("initialize renderer" :debug-initialize)
    (let ((myMedia (media me)))
      (or (controllerGCOSPtr me)
          (eq me QuickTimeRenderer) ; doesn't need initialization
          (not myMedia)
          (progn (setf (hiddenWindow me) nil)
                 (and (funcall whichFunction myMedia)
                      (funcall whichFunction (callBackForDone me)) ; If myMedia is initialized, then this will succeed.
                      (progn
                        (with-port (setf (hiddenWindow me) (makeGCOSPtr (wptr (make-instance 'window :view-size (make-point 1 1) 
                                                                                             :view-position (make-point -4000 -4000)))
                                                                        #'window-close))
                          (setf (active myMedia :dontForwardToRenderer t) t)
                          (setValue 'controllerGCOSPtr me (T_NewMovieControllerGC
                                                           (mediaData myMedia)
                                                           :withBadge (usingBadge me)
                                                           :notVisible (not (controllerVisible me)))))
                        (T_MCSetActionFilterWithRefCon (controllerGCOSPtr me) (objectIDFromObject me))
                        (setf (servicing me) t)
                        t)))))))

(define-handler initializeFromStore :private (QuickTimeRenderer)
  (and (call-next-method)
       (restoreOrInitializeFromStore me #'initializeFromStore)))

(define-handler restore :private (QuickTimeRenderer)
  (and (call-next-method)
       (restoreOrInitializeFromStore me #'restore)))

;;;------------------------------------------------------------------------------------------

(defmacro defReferredPropertyGetter (prop private)
  (append `(define-handler ,prop) private '((QuickTimeRenderer))
          `((assumeMedia (me myMedia)
                         (,prop myMedia)))))

(defmacro defReferredPropertySetter (prop private)
  (append `(define-handler (setf ,prop)) private '((newValue QuickTimeRenderer))
          `((assumeMediaWithError (me myMedia)
                                  (setf (,prop myMedia) newValue)))))

(defvar QTMHandlersDone '())
; (setf QTMHandlersDone '())


(defmacro defReferredProperty (prop me referredObject &key print)
  `(let ((itsGetter (first (ccl::compute-applicable-methods
                                (symbol-function ,prop)
                                (list ,referredObject)))))
     (when itsGetter
       (unless (private itsGetter)
         (push (SK8:name itsGetter) QTRVirtualProperties)
         (push itsGetter QTMHandlersDone)
         (let ((myGetter (mf::find-local-handler ,prop ,me)))
           (unless myGetter
             (let ((result (eval `(defReferredPropertyGetter ,,prop nil))))
               (when ,print
                 (print result)))))))
     (let ((itsSetter (setter itsGetter)))
       (when itsSetter
         (unless (private itsSetter)
           (push itsSetter QTMHandlersDone)
           (let ((mySetter (mf::find-local-handler `(setf ,,prop) ,me)))
             (unless mySetter
               (let ((result (eval `(defReferredPropertySetter ,,prop nil))))
                 (when ,print
                   (print result))))))))))

#| (defReferredProperty 'mediaData QuickTimeRenderer QuickTimeMovie :print t) |#

(defun defRemainingReferredProperties (me referredObject &key print)
  (dolist (prop (set-difference
                 (set-difference (properties referredObject)
                                 (properties Object))
                 '(renderer
                   resType
                   height
                   width)))
    (defReferredProperty prop me referredObject :print print)))

(defRemainingReferredProperties QuickTimeRenderer QuickTimeMovie :print nil)
 
#|
;;; cover the rest of the handlers of QuickTimeMovie
(defun handlerNames (me)
  (loop for h in me
      collect (SK8:name h)))

(defun publicHandlers (me &rest rest)
  (loop for h in (apply #'handlers me rest)
        unless (private h)
        collect h))

(loop for h in (set-difference
                (set-difference
                 (set-difference
                  (set-difference
                   (handlerNames (publicHandlers QuickTimeMovie :inherited nil))
                   (handlerNames QTMHandlersDone))
                  (handlerNames (publicHandlers QuickTimeRenderer :inherited nil :local t)))
                 (handlerNames (publicHandlers Object)))
                (properties object))
      do (print (let ((handlr (gs:find-applicable-handler
               (symbol-function h)
               QuickTimeMovie))) (list handlr (arglist handlr :include-bindings t)))))
|#
(setf QTMHandlersDone nil)

(define-handler trackFromID (QuickTimeRenderer id)
  (assumeMediaWithError (me myMedia)
    (trackFromID myMedia id)))

(define-handler putOnScrap (QuickTimeRenderer
                                     &key
                                     (flags         nil flagsSet)
                                     (dontZeroScrap nil dontZeroScrapSet)
                                     (onlyPutMovie  nil onlyPutMovieSet))
  (assumeMediaWithError (me myMedia)
    (apply #'putOnScrap myMedia
           (append (if flagsSet         (list :flags         flags)         '())
                   (if dontZeroScrapSet (list :dontZeroScrap dontZeroScrap) '())
                   (if onlyPutMovieSet  (list :onlyPutMovie  onlyPutMovie)  '())))))

(define-handler copySelection (QuickTimeRenderer)
  (assumeMediaWithError (me myMedia)
    (copySelection myMedia)))

(define-handler preroll (QuickTimeRenderer
                                  &key
                                  (startTime nil startTimeSet)
                                  (rate      nil rateSet))
  (assumeMediaWithError (me myMedia)
    (apply #'preroll myMedia
           (append (if startTimeSet (list :startTime startTime) '())
                   (if rateSet      (list :rate      rate)      '())))))

(define-handler moviePicture (QuickTimeRenderer
                                       &key
                                       (timeValue nil timeValueSet))
  (assumeMediaWithError (me myMedia)
    (apply #'moviePicture myMedia
           (if timeValueSet (list :timeValue timeValue) '()))))

(define-handler posterPicture (QuickTimeRenderer)
  (assumeMediaWithError (me myMedia)
    (posterPicture myMedia)))

(define-handler loadIntoRam
                       (QuickTimeRenderer
                        &key
                        (segment                nil segmentSet)
                        (flags                  nil flagsSet)
                        (keepInRam              nil keepInRamSet)
                        (unkeepInRam            nil unkeepInRamSet)
                        (flushFromRam           nil flushFromRamSet)
                        (loadForwardTrackEdits  nil loadForwardTrackEditsSet)
                        (loadBackwardTrackEdits nil loadBackwardTrackEditsSet))
  (assumeMediaWithError (me myMedia)
    (apply #'loadIntoRam myMedia
           (append (if segmentSet                (list :segment                segment)                '())
                   (if flagsSet                  (list :flags                  flags)                  '())
                   (if keepInRamSet              (list :keepInRam              keepInRam)              '())
                   (if unkeepInRamSet            (list :unkeepInRam            unkeepInRam)            '())
                   (if flushFromRamSet           (list :flushFromRam           flushFromRam)           '())
                   (if loadForwardTrackEditsSet  (list :loadForwardTrackEdits  loadForwardTrackEdits)  '())
                   (if loadBackwardTrackEditsSet (list :loadBackwardTrackEdits loadBackwardTrackEdits) '())))))

;;; -----------------------------------
;;; moviefy me (an actor)
;;; -----------------------------------

;;; moviefy
;;; Specify a movie, a file object, a file name, or nothing
(define-handler moviefy (Actor &key
                                        (movie nil movieSet)
                                        ((:file fileArg) nil)
                                        logicalName
                                        physicalName)
  ;; Deal with the keyword arguments
  (let ((movieSettingArgs (count-if-not #'null (list movie
                                                     fileArg
                                                     logicalName
                                                     physicalName))))
    (cond
     ((< 1 movieSettingArgs)
      ;;? test the following
      ;; conflicting keyword args selected
      (sk8-error IncorrectArgumentsError
                 :handlername 'moviefy
                 :arguments (append (when movie        (list :movie        movie))
                                    (when fileArg      (list :file         fileArg))
                                    (when logicalName  (list :logicalName  logicalName))
                                    (when physicalName (list :physicalName physicalName)))))
     ((= 1 movieSettingArgs)
      ;; ok
      )
     ; (= 0 movieSettingArgs)
     (movieSet
      ;; They set the movie keyword arg to nil
      )
     (t
      ;; by default we give them the open file dialog to select a media
      (setf movie t))))
  
  ;; Try to prevent erroneous moviefy attempts
  (when (member me (list Actor Rectangle)) ; catch the most obvious certain death errors
    (sk8-error GeneralProgrammaticError
               :strings '("You can't moviefy " " itself.")
               :objects (list me)))
  
  ;; Do it
  (let* ((proj (project me))
         (theFile (or fileArg
                      (cond
                       (logicalName
                        (new File :logicalName  logicalName  :project proj))
                       (physicalName
                        (new File :physicalName physicalName :project proj))
                       nil)))
         (myMovie (if (or movie movieSet)
                    movie
                    (new QuickTimeMovie :project proj :file theFile)))
         (oldFillColor (fillColor me)))
    
    (when (eq myMovie QuickTimeMovie)
      (sk8-error GeneralError
                 :strings '("movie argument must be a descendant of QuickTimeMovie, not QuickTimeMovie itself")))
    
    (when-unwind
      (progn
        (if (inheritsFrom (fillColor me) QuickTimeRenderer)
          (setf (state (fillColor me)) 'inactive)
          (setf (fillColor me) (new QuickTimeRenderer :project proj)))
        (setf (media (fillColor me)) myMovie))
      (setf (fillColor me) oldFillColor)))
  me)

#|
(untrace)
(trace ;enlargeBoxByFrameSize
       ;reduceBoxByFrameSize
       ;setboundsrect
       ;boundsrect
       alignmentOfBoundsToBounds
       alignSizeToBounds
       constrainSizeToAspectRatio
       constrainboundstoaspectratio
       )
(trace setboundsrect
       ; boundsrect
       (setf boundsRect)
       )
(trace ;alignMovieFromBounds
       controllerPortion
       boundingBoxMovieOffset
       visible
       onStage
       (setf onstage)
       enlargeBoxByFrameSize
       reduceBoxByFrameSize
       enlargeSizeByFrameSize
       moveMovieFromBounds
       movemovieFromRenderedregion
       movemoviefromfill
       movemoviefromFrame
       resizemoviefromBounds
       resizemoviefromframe
       resizemoviefromfill
       resizemoviefromrenderedRegion
       resizeActorFromMovie
       resizeFillFromMovie
       resizeFrameFromMovie
       resizeRenderedRegionFromMovie
       preferredAspectRatio
       calculatePreferredSize
       preferredSize
       setsize
       ;setboundsrect
       doresizing
       setframesize
       ; resized
       ; moved
       moviebox
       handleonstageandVisible
       ;makefillregion
       ;makeFrameRegion
       resizeActorIfCalledFor
      resizeMovieFromBounds
       suppressingVisualTracks
       (setf media)
       scale
       (setf scale)
       (setf matrix)
       (setf moviebox)
       (setf suppressingVisualTracks)
       (setf actorOwner)
       (setf fillColorActor)
       lightForceRedraw
       forceRedraw
       ;setvalue ; don't trace this - setvalue arg in qtmfinalizeinitialization can't be printed
       enteringStage
       leavingStage
       resizingStyle
       alignment
       maintainAspectRatio
       changingBox
       ;changingScale

       alignmentOfBoundsToBounds
       alignSizeToBounds
       constrainSizeToAspectRatio
       constrainboundstoaspectratio
       )
(untrace)
|#

#|
	QT-SimplePlayer
	1	3/8/94	dy	used to be in QT-Player.lisp which is now reserved for the one with a Movie Controller
	2	3/11/94	dy	:private handlers and other changes
	3	3/12/94	dy	various bug fixes before I leave
	4	3/16/94	kleiman	add keyword to remove-parent so we can force it for unnamed children
	5	3/22/94	dy	various fixes
	6	3/22/94	dy	remove beep
	7	3/23/94	dy	fix setf movieParent when newMovieParent already has a player
	8	3/24/94	dy	keep movie playing if switching players
	9	3/28/94	Hernan	AddingMeAsParent -> addedMeAsParent.
	10	4/1/94	dy	changeParents etc.
	11	4/7/94	dy	resizing stuff
	12	4/11/94	dy	startTime, duration -> segment
	13	4/13/94	dy	use preroll always
	14	4/13/94	sidney	Add parameters to addedMeAsParent and removingMeAsParent to allow handlers to avoid work when an ancestor is added back in after a remove
	15	4/19/94	dy	Poster & Preview handling
	16	4/19/94	dy	poster fix in setf movieParent
	17	4/20/94	dy	resize when no movieParent bug
	18	4/20/94	dy	new when prototype already has actorParent or movieParent
	19	4/21/94	yost	fix changeParents
	19	4/21/94	yost	set xxxParent in changeParents, default fillColor to black
	20	4/21/94	dy	stepBy with noIntermediates
	21	4/21/94	dy	fix stepBy step arg
	22	4/21/94	dy	stepBy  -> step by: with play
	23	4/21/94	chip	source server comments messed up
	27	4/21/94	yost	
	28	4/21/94	yost	source server comments ok now?
	29	4/21/94	yost	source server comments ok now?
	30	4/21/94	yost	source server comments ok now?
	31	4/21/94	dy	symbols
	32	4/21/94	dy	To do comments
	33	4/28/94	dy	update change notes; pauseWithHighQuality not finished
	34	4/28/94	dy	pauseWithHighQuality
	35	4/28/94	dy	new :project (project me) in initialize; new QuickTimeRectangle object
	36	4/29/94	dy	MovieRectangle
	37	5/2/94	dy	add moviefy, set movieParent to true now works, resizingStyle initialValue is resizeActorToMovie
	38	5/3/94	dy	moviefy will complain if actor already movified
	39	5/3/94	dy	1159162 <-- prev change was to fix this bug
	40	5/3/94	dy	moviefy even more robust
	41	5/3/94	dy	minor moviefy fix
	42	5/3/94	yost	update change notes
	43	5/4/94	dy	update changes comment
	44	5/4/94	dy	fix moviefy with filename
	45	5/6/94	dy	fix MovieRectangle
	46	5/6/94	dy	set movieRectangle's prototype to true
	47	5/6/94	dy	change comment
	48	5/6/94	dy	move MovieRectangle to its own source file
	49	5/31/94	yost	preroll, remove get matrix, change set matrix to call-next-method instead of doing it all itself
	50	5/31/94	yost	move previewMode from QTSP to QTM
	51	6/3/94	yost	box -> movieBox
	52	6/8/94	yost	fix bug 1166879, better error when movieParent not set; fix all errors to be sk8-error
	53	6/9/94	sidney	remove extra ) typo
	54	6/9/94	dy	fix new movieGCOSPtr handler
	55	6/10/94	dy	fix bug #1156639  - initialize of QTSP reparenting & related set movieParent & set state
	56	6/10/94	dy	fix bug #1167173 - moviefy should set actorParent.  Also, moviefy should leave the actor as it was if anything goes wrong.
	57	6/12/94	dy	working on Bug #1159303 - can't save and restore movies
	58	6/12/94	dy	more save-restore work
	59	6/12/94	kleiman	
	60	6/12/94	dy	tweak unMoviefy and move unused save-restore code to QTM
	61	6/13/94	dy	fix bug #1167967 - moviefy by default brings up the open dialog
	62	6/14/94	dy	fix sk8-error call in moviefy and other moviefy problems
	63	6/16/94	dy	Bug #1168288
	64	6/16/94	dy	Bug #1167969
	65	6/17/94	dy	Fix #1169002
	66	6/17/94	dy	Fix #1169002
	67	6/17/94	dy	Fix project store/load
	68	6/17/94	dy	fix moviefy
	69	6/20/94	dy	work on project load
	70	6/20/94	dy	force saved players into stopped state
	71	6/20/94	dy	put unmoviefy args in sk8 package
	72	6/20/94	dy	remove &rest from moviefy args
	73	6/20/94	dy	move symbol to specialsyms
	74	6/20/94	dy	Fix #1168945 - arrange for hidden unMoviefy args
	75	6/22/94	kleiman	1167173
	76	6/22/94	dy	removeProperty gWorldSet
	77	6/23/94	dy	really remove gWorldSet
	78	6/30/94	dy	

|#
#|
	Change History (most recent last):
	1	7/20/94	dy	QT-Renderer introduced
	2	7/20/94	dy	cosmetic
	3	7/21/94	dy	take out debug stuff
	4	8/7/94	dy	make movieBox property always a list
	5	8/11/94	dy	documentation
	6	8/11/94	dy	tweaks for the documentation
	7  	 8/22/94	dy      	gotoBeginning&End modified
	8  	 8/24/94	dy      	gotoX changes, fix object Editor crash, localVirtProps to end
	9  	 9/ 1/94	dy      	fixes while documenting
	10 	 9/ 6/94	dy      	Fix object editor bugs; render black when theActor is not my actorOwner
	11 	 9/15/94	dy      	Fix 1186514 - allow moviefy on already moviefied Actor
	12 	 9/20/94	dy      	In initialize, when the original's media is not false, do the right thing
	13 	 9/22/94	dy      	fix for resizingStyle false, assumeMediaWithError macro instead of call-next-method
	14 	 9/23/94	dy      	fix resizingStyle = false, maintainAspectRatio
	15 	 9/27/94	dy      	NoValuexxx -> *undefined*
	16 	 9/29/94	dy      	unbreak set timeValue, etc.
	17 	 9/30/94	dy      	Fix 1182762 - rendering to a palette
	18 	 9/30/94	dy      	return *undefined* when no useful return value instead of (sk8-multivals)
	19 	10/ 3/94	dy      	handle -> mediaData, render with paper, other changes
	20 	10/ 5/94	dy      	resizing fixes
	21 	10/ 6/94	dy      	resizing fixes
	22 	10/ 6/94	chip    	took out QuickTimeRenderer's new -- the behavior it was trying to get is now part of the basic new
	23 	10/ 7/94	dy      	move constrainBoundsToAspectRatio and constrainSizeToAspectRatio to 2d-baseline;utilities.lisp
	24 	10/17/94	dy      	fix resizing stuff (major)
	25 	10/19/94	dy      	add setf scale
	26 	10/21/94	dy      	better resizing
	27 	10/21/94	dy      	fix setBoundsRect
	28 	10/24/94	dy      	add rescaled handler.  fix movieBox rounding error
	29 	10/28/94	dy      	no more parenting
	30 	10/28/94	dy      	typos
	31 	10/31/94	dy      	eliminate flashing when starting a movie
	32 	10/31/94	dy      	remove pauseWithHighQuality property
	33 	10/31/94	dy      	less verbose build
	34 	11/ 2/94	dy      	fix loadIntoRam
	35 	11/ 4/94	dy      	
	36 	11/14/94	dy      	remove warnings;  add handlers for localPropertiesToSaveAsFalse and localPropertiesToSaveSpecially
	37 	11/15/94	dy      	cosmetic
	38 	11/21/94	dy      	If state is 'playing', save it as 'paused', otherwise save it as is.  Was always saving as 'inactive'.
	39 	11/22/94	dy      	fix setf active to set the media's active
	40 	11/22/94	dy      	unless state is 'inactive' initializeFromStore calls initializeFromStore on my media and sets its active to True.  Same for restore.
	41 	11/22/94	dy      	don't make referred property for resType from QTM
	42 	11/30/94	dy      	change args to rescaled, but defer making it work right
	43 	12/ 1/94	dy      	rescaling changed to always call doResizing - see comment there for how this is a bit of a punt
	44 	 1/25/95	dy      	Adjust to new API of T_SetMovieCoverProcs
	45 	 1/26/95	dy      	remove "set backgroundColor", now properly handled in DynamicRenderer
	46 	 1/26/95	dy      	shore up initializeFromStore against movie not found
	47 	 1/27/95	dy      	put setf backgroundColor back in so it does a redraw.  still needs looking at.
	48 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	49 	 2/16/95	sidney  	readable argument names for initialize handler
	50 	 2/28/95	dy      	MovieController!
	51 	 3/ 2/95	dy      	MCMovieChanged in nextInterestingTime, etc.
	52 	 3/ 3/95	dy      	myMedia changed to (media me) in setf active.
							fix bug about option clicking the sound thingy
							fix bug about setting resizingStyle to 'resizeMovieFromFrame'
	53 	 3/ 3/95	dy      	change useBadge to usingBadge
	54 	 3/ 6/95	dy      	fix set/get repeating of QTR
	55 	 3/ 6/95	dy      	fix moviefy bug #1225907
	56 	 3/ 8/95	dy      	store fixes
	57 	 3/ 8/95	dy      	fix resize-box so setting frameColor works
	58 	 3/10/95	dy      	another Library:Interfaces fix (again), and fixes for done of QuickTimeRenderer
	59 	 3/11/95	dy      	fix resizeActorFromMovie so loading from store works when resizeActorFromXxx is set
	60 	 3/11/95	dy      	align with new QTCallBack API: callMeWhen x -> setf (active x) t
	61 	 3/14/95	dy      	implement movieChanged which calls MCMovieChanged
	62 	 3/15/95	dy      	paritial fix for resizingFrameFromMovie
	63 	 3/21/95	dy      	try to fix store stuff, introduce flags operations
	64 	 3/22/95	dy      	assumeControllerGCOSPtrWithError macro was inadvertently blown away
	65 	 3/24/95	dy      	fix done & loading from store
	66 	 3/28/95	dy      	Try to fix preserve/restore.
	67 	 3/29/95	dy      	suppressingSpeakerButton -> speakerButtonVisible; same for StepButtons
	68 	 3/29/95	dy      	fix hiddenWindow property re: save/restore
	69 	 4/ 3/95	dy      	Fix bug 1226383 'resizeFrameFromMovie' with framesize set to {0,0}
	70 	 4/ 4/95	dy      	Fix stepping
	71 	 4/ 7/95	dy      	Fix 1236953.  Rename displaying to visible and make it virtual.  onStage is now a vitual read-only.  Entering and leavingStage now simply call handlingOnStageAndVisible of me.
	72 	 4/12/95	rod     	Disabling the resizeFillFromMovie option for B3 
							due to crash problems.
	73 	 4/18/95	dy      	Be less drastic in the QT 2.0 workaround bug for resizeFillFromMovie: disallow setting the property when we already have a media and are already filling an actor.
	74 	 4/19/95	dy      	Fix 1240874 - restoreOrInitializeFromStore was setting hiddenWindow to False when it shouldn't
	75 	 4/24/95	dy      	put in a recursion blocker for the 'resizeFillFromMovie' but with QT 2.0
	76 	 5/ 1/95	dy      	render and other internal handlers marked private.  Step handler now always returns #undefined#
	2  	 6/12/95	dy      	use new getFileFromUserWithPreview function.  Other code reading minor fixes.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 5/ 7/96	sidney  	Changes for new object system
	5  	 5/ 7/96	Hernan  	Fixing undefined function warnings.
	7  	 6/25/96	sidney  	accidentally checked in some debugging stuff, revert
	8  	 9/23/96	Hernan  	Lots of fixes. The most dangerous one might be adding a
						call to set servicing inside of setState.
	9  	 9/24/96	Hernan  	setMedia does not set servicing to true if the movie does
						not have a controller. Also, changed render to just set the
						clip when the movie is playing.
	10 	10/ 7/96	Brian   	commented out fmakunbound.  with threads an event occured between this and it being redefined so an error occured.   redefining the function does the same thing anyway.
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
