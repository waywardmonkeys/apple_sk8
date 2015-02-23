(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(provide "DYNAMICRENDERER")

(SK8-declare-syms :SK8 :special
                  SK8::DynamicRenderer
                  )

(new Renderer :objectName "DynamicRenderer"
         :undisposable t
         :project SK8)

(addProperty DynamicRenderer 'actorOwner     ) ; the Actor we're playing into
(addProperty DynamicRenderer  'fillColorActor) ; the Actor if fillColor
(addProperty DynamicRenderer 'frameColorActor) ; the Actor if frameColor
(addProperty DynamicRenderer  'textColorActor) ; the Actor if textColor
(addProperty DynamicRenderer 'backgroundColor :initialValue Black)

(define-handler (setf actorOwner) (newValue DynamicRenderer)
  (declare (ignore newValue))
  (raiseUnsettablePropertyError 'actorOwner me))

(define-handler (setf fillColorActor) (newValue DynamicRenderer)
  (declare (ignore newValue))
  (raiseUnsettablePropertyError 'fillColorActor me))

(define-handler (setf frameColorActor) (newValue DynamicRenderer)
  (declare (ignore newValue))
  (raiseUnsettablePropertyError 'frameColorActor me))

(define-handler (setf textColorActor) (newValue DynamicRenderer)
  (declare (ignore newValue))
  (raiseUnsettablePropertyError 'textColorActor me))

(define-handler localVirtualProperties (DynamicRenderer)
  (and (eq me DynamicRenderer)
       '(visible)))

;;; overrides Renderer, which returns nil.
;;; This is a faster way to tell if the renderer is a dynamicRenderer than calling inheritsFrom
(define-handler dynamic (DynamicRenderer)
  t)

(define-handler initialize :private (DynamicRenderer original isNew initArgs)
  (declare (ignore original initargs))
  (unless isNew
    (setValue 'actorOwner      me nil)
    (setValue  'fillColorActor me nil)
    (setValue 'frameColorActor me nil)
    (setValue  'textColorActor me nil))
  (sk8-return-nothing))

(define-handler (setf backgroundColor) (newRenderer DynamicRenderer)
  (ensureType newRenderer Renderer) ; the render handler requires this
  (setValue 'backgroundColor me newRenderer)
  (when (xor (translucent newRenderer)
             (translucent (backgroundColor me)))
    ;; Call the appropriate setter of the actor so it will deal with the change in translucency.
    (cond
     (( fillColorActor me) (setf ( fillColor (actorOwner me)) me))
     ((frameColorActor me) (setf (frameColor (actorOwner me)) me))
     (( textColorActor me) (setf ( textColor (actorOwner me)) me)))))

;;;-------------------------------------------------------------------------------
;;; The remaining handlers are called from Actor:

;;; These 3 setters:
;;;    set     fillColorActor
;;;    set frameColorActor
;;;    set    textColorActor
;;; are mutually exclusive.  This macro will be used to generate each of them.
;;; These functions are only called from
;;;    set fillColor      of Actor
;;;    set frameColor of Actor
;;;    set textColor    of Actor
;;;  respectively.
;;; These are the cases:
;;;   ¥ we're already rendering some other Actor
;;;     ¥ we're being connected to a new Actor
;;;         Yank us away from the other Actor.
;;;         To do this, we must set that Actor's xxxColor to something else.
;;;            (That will cause us to be re‘ntered setting our Actor to nil.)
;;;         That something else is our backgroundColor unless that's rendering somewhere else, in which case White.
;;;     ¥ we're being disconnected from our Actor
;;;   ¥ we're not already rendering somewhere else
(macrolet ((defThisColorActor (thisStr)
              (let* ((thisColorActor (intern (concatenate 'string thisStr "COLORACTOR") cl::*package*))
                     (thisColor      (intern (concatenate 'string thisStr "COLOR"     ) cl::*package*))
                     (realMe (gensym)))
                `(let ((,realMe (dynamicRendererToUse me newActor))) ; guards against using QuickTimeRenderer itself
                   (unless (eq newActor (,thisColorActor ,realMe))
                     (let ((oldActorOwner (actorOwner ,realMe)))
                       (when oldActorOwner
                         ;; Can't be in two places at once.
                         (if newActor
                           ;; Disconnect from present actorOwner before connecting to newActor.
                           ;; We do so by setting actorOwner's color to something else.
                           (let ((backgroundColor (backgroundColor ,realMe))
                                 restoredColor)
                             (if (and (dynamic    backgroundColor)
                                      (actorOwner backgroundColor)) ; already in use somewhere else
                               (setf restoredColor White)
                               (setf restoredColor backgroundColor))
                             (cond
                              (( fillColorActor ,realMe) (setf ( fillColor oldActorOwner) restoredColor))
                              ((frameColorActor ,realMe) (setf (frameColor oldActorOwner) restoredColor))
                              (t                         (setf ( textColor oldActorOwner) restoredColor))))
                           ;; Simply disconnect from present actorOwner.
                           (progn
                             ;; thisColorActor of me is set to newActor (i.e. nil) below
                             (setValue 'actorOwner ,realMe nil) ; Must come before leavingStage (tho repeated below).
                             (leavingStage ,realMe) ; Turn off the visual tracks.
                             ;; Inform the Actor that one of its dynamic Renderers is going away.
                             (detachDynamicRenderer oldActorOwner (intern ,thisStr :keyword)))))
                       ;; Connect to newActor.
                       (setValue ',thisColorActor ,realMe newActor)
                       (setValue 'actorOwner ,realMe newActor)
                       (when newActor
                         (attachDynamicRenderer newActor (intern ,thisStr :keyword)))))
                   ,realMe))))
  
  (define-handler setFillColorActor :private (DynamicRenderer newActor)
                  (defThisColorActor "FILL"))
  
  (define-handler setFrameColorActor :private (DynamicRenderer newActor)
                  (defThisColorActor "FRAME"))
  
  (define-handler setTextColorActor :private (DynamicRenderer newActor)
                  (defThisColorActor "TEXT"))
  )

(define-handler dynamicRendererToUse (DynamicRenderer theActor)
  (declare (ignore theActor))
  (sk8-error GeneralProgrammaticError
                  :strings '("You can't use DynamicRenderer itself"))
  (sk8-return-nothing))

(define-handler visible (DynamicRenderer)
  (let ((actorOwner (actorOwner me)))
    (and actorOwner
         (visible actorOwner))))

(define-handler (setf visible) (newValue DynamicRenderer)
  newValue)

(define-handler enteringStage (DynamicRenderer)
  (sk8-return-nothing))

(define-handler leavingStage (DynamicRenderer)
  (sk8-return-nothing))

(define-handler setBoundsRect (DynamicRenderer left top right bottom 
                                                  &key physical justMoving)
  (declare (ignore physical justMoving))
  (sk8-multivals left top right bottom)) ; no modification of left, top, right, or bottom requested

(define-handler rescaled (DynamicRenderer)
  (sk8-return-nothing))

(define-handler moved (DynamicRenderer)
  (sk8-return-nothing))

(define-handler resized (DynamicRenderer)
  (sk8-return-nothing))

(define-handler setFrameSize (DynamicRenderer h v &key physical)
  (declare (ignore h v physical))
  (sk8-return-nothing))

#|
	Change History (most recent last):
	1	7/15/94	dy	
	2	7/19/94	dy	boundsRect
	3  	 8/24/94	dy      	Upper-lower case syms
	4  	 9/19/94	dy      	introduce an initialize method
	5  	 9/19/94	dy      	some more clarity around removing the mixin parent
	6  	 9/22/94	dy      	backgroundColor initialValue Black
	7  	 9/30/94	dy      	set actorOwner before leavingStage
	8  	 9/30/94	dy      	return *undefined* when no useful return value instead of (sk8-multivals)
	9  	10/ 3/94	dy      	*undefined* -> (sk8-return-nothing)
	10 	10/ 5/94	dy      	call set actorOwner instead of setValue
	11 	10/17/94	dy      	reorg - enteringStage is now called by set xxxColor of Actor
	12 	10/17/94	dy      	fix problem setting backgroundColor
	13 	10/24/94	dy      	add rescaled
	14 	11/16/94	dy      	Fix looping bug when setting fillcolor to itself.  Cosmetic cleanups.
	15 	12/ 1/94	dy      	change args to rescaled
	16 	 1/26/95	dy      	change set backgroundColor to call set {fill,frame,text}Color again if the translucency of the new and old backgroundColor differ
	17 	 2/16/95	sidney  	readable argument names for initialize handler
	18 	 3/ 2/95	dy      	don't take backgroundColor from the previous fillColor
	19 	 4/ 7/95	dy      	Fix 1236953.  Add 'visible' virtual property.
	2  	 6/12/95	dy      	cleanups to match documentation, and make several properties readonly
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
