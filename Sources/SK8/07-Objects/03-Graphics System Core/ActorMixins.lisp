(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; ____________________
;;; 1. Resizes Contents.
;;; ____________________

;;; Caches the rects of the contents of actor...

(define-handler cacheContentsRects (Actor)
  (let ((cacheList nil)
        width height)
    (sk8-multival-bind (left top right bottom) (boundsRect me :physical t)
      (setf width (- right left)
            height (- bottom top))
      (gs:docontents (subActor me)
        (sk8-multival-bind (ll tt rr bb) (boundsRect subActor :physical t)
          (push (list subActor
                      (/ (- ll left) width)
                      (/ (- tt top) height)
                      (/ (- rr left) width)
                      (/ (- bb top) height))
                cacheList)))
      (setf (getf (gs:node-properties me) :cachedRects) cacheList))))

(define-handler scaleContents (Actor)
  (let (newWidth newHeight)
    (sk8-multival-bind (left top right bottom) (boundsRect me :physical t)
      (setf newWidth (- right left)
            newHeight (- bottom top))
      (dolist (cacheItem (getf (gs:node-properties me) :cachedRects))
        (setBoundsRect (car cacheItem)
                       (truncate (+ left (* (second cacheItem) newWidth)))
                       (truncate (+ top (* (third cacheItem) newHeight)))
                       (truncate (+ left (* (fourth cacheItem) newWidth)))
                       (truncate (+ top (* (fifth cacheItem) newHeight)))
                       :physical t)))))

;;; RESIZESCONTENTS -- returns whether this actor resizes its contents when it resizes itself.

(define-handler resizesContents (actor)
  (gs:resizesContents? (gs:node-flags me)))

(define-handler (setf resizesContents) (boolean actor)
  (let ((flags (gs:node-flags me)))
    (unless (eq boolean (gs:resizesContents? flags))
      (if boolean
        (progn
          ;; Scream if the actor is already bounded by contents.
          (when (boundedByContents me)
            (sk8-error GeneralProgrammaticError
                       :strings '("You cannot make a boundedByContents actor resize its contents.")))
          (gs:resizesContents! flags 1)
          (cacheContentsRects me))
        (progn
          (gs:resizesContents! flags 0)
          (remf (gs:node-properties me) :cachedRects)))
      boolean)))
    
;;; ____________________
;;; 2. Bounded By Contents.
;;; ____________________

;;; FIND-RECT-TO-BIND-CONTENTS -- computes the rect that contains all subActors of actor.
;;;                           This is done in the stupidest possible way: loop through all
;;;                           subactors and union their boundRects.
;;; 
;;; Note that the idea is that the rect is big enough to show the guys that bind it in their entirety.
;;; This is why we outset the rect with the framesize. In this way we can show both the actors and
;;; the frame of the container.

(defun find-rect-to-bind-contents (theactor)
  (let ((final-rect (gs:make-rect))
        frameH frameV)
    (sk8-multival-setf (frameH frameV) (framesize theActor :physical t))
    (dovector (subActor (gs:node-contents theActor))
      (gs:union-rects final-rect 
                      (gs:recompute-physicalBoundsRect subActor)
                      final-rect))
    (SK8-multivals (- (gs:rect-left final-rect) frameH) (- (gs:rect-top final-rect) frameV)
                   (+ (gs:rect-right final-rect) frameH) (+ (gs:rect-bottom final-rect) frameV))))

;;; BIND-BY-CONTENTS -- makes actor bind its contents. This involves two steps: computing the
;;;                   rectangle that would bind all contents (even invisible ones), resetting the
;;;                   origin of self so that the contents stay in their place, and changing the
;;;                   boundsrect of self to the new rect.

(define-handler bindByContents (Actor)
  ;; Only do this mess if you have contents!
  (unless (zerop (length (gs:node-contents me)))
    (let* ((container (gs:node-container me))
           (boundsRect (gs:recompute-physicalBoundsRect me))
           (ll (gs:rect-left boundsRect))
           (rr (gs:rect-right boundsRect))
           (tt (gs:rect-top boundsRect))
           (bb (gs:rect-bottom boundsRect))
           (clos-window (gs:node-window me)))
      (SK8-multival-bind (left top right bottom) (find-rect-to-bind-contents me)
        (unless (and (= ll left) (= tt top) (= rr right) (= bb bottom))
          (if (gs:hasWindow? (gs:node-flags me))
            ;; Window Case.
            (let ((newWidth (- right left))
                  (newHeight (- bottom top))
                  (topLeftOk? (and (= ll left) (= tt top))))
              (if topLeftOk?
                ;; Just expand the window from bottom right.
                (progn 
                  (sk8-multival-setf (ll tt rr bb) (boundsRect me :physical t))
                  (setf-tla-boundsRect me ll tt (+ ll newWidth) (+ tt newHeight)))
                (let ((hScroll (* (gs:node-xScale me) (- left ll)))
                      (vScroll (* (gs:node-yScale me) (- top tt))))
                  (gs:withDrawDisabled clos-window
                    ;; Set the origin.
                    (scroll me hScroll vScroll :forceRedraw t))
                  ;; Resize the window.
                  (setf-tla-boundsRect me (- left ll) (- top tt) (- right rr) (- bottom bb)
                                       :relative t))))
            ;; Normal Case. Use logical coords.
            (gs:withLockedWindow clos-window
              ;; reset the bounds rect.
              (SK8-multival-bind (logLeft logTop)
                                 (gs:real-physical-to-logical container left top)
                (SK8-multival-bind (logRight logBottom)
                                   (gs:real-physical-to-logical container right bottom)       
                  (setBoundsRect me logLeft logTop logRight logBottom)))
              (unless (and (= ll left) (= tt top))
                ;; update the origin of self
                (scroll me (* (gs:node-xScale me) (- left ll))
                        (* (gs:node-yScale me) (- top tt)) :forceRedraw t)))))))))
    
;;; BOUNDEDBYCONTENTS -- returns whether self is bounded by its contents.

(define-handler boundedByContents (Actor)
  (gs:boundedByContents? (gs:node-flags me)))

;;; (SETF BOUNDEDBYCONTENTS) -- makes self be bounded by its contents of boolean is t, and
;;;                          makes it unbounded by contents if nil. In the first case,
;;;                          we tell self its bounded by contents and resize its boundsRect
;;;                          so that all objects are contained in it. In the second case
;;;                          we just let self know it is no longer bounded.

(define-handler (setf boundedByContents) (boolean actor)
  (if boolean
    ;; Bounding by contents: set the flags and reset the boundsRect.
    (unless (boundedByContents me)
      (when (resizesContents me)
        (sk8-error GeneralProgrammaticError
                   :strings '("You cannot make an actor that resizesContents be boundedByContents.")))
      (gs:docontents (subActor me)
        (gs:belongsToBoundedByContents! (gs:node-flags subActor) 1))
      (bindByContents me)
      (gs:boundedByContents! (gs:node-flags me)))
    ;; Undoing bounded by contents: just reset the flag.
    (when (boundedByContents me)
      (gs:docontents (subActor me)
        (gs:belongsToBoundedByContents! (gs:node-flags subActor) 0))
      (gs:boundedByContents! (gs:node-flags me) 0)))
  boolean)

(define-sk8-function copyTextProperties nil (dest src)
  (unless (eq (textFont dest) (textFont src))
    (setf (textFont dest) (textFont src)))
  (unless (eq (textColor dest) (textColor src))
    (setf (textColor dest) (textColor src)))
  (unless (eq (textSize dest) (textSize src))
    (setf (textSize dest) (textSize src)))
  (unless (eq (textStyle dest) (textStyle src))
    (setf (textStyle dest) (textStyle src)))
  (unless (eq (textlocation dest) (textlocation src))
    (setf (textlocation dest) (textlocation src)))
  (unless (eq (textoffset dest) (textoffset src))
    (setf (textoffset dest) (textoffset src))))

;;; We need to define this, to allow callers of while-scrolling to load without
;;; loading the scroller first. 

(define-sk8-var sk8::lockedScroller
  :initial-value nil)

(defmacro while-scrolling (theScroller &body body)
  `(unwind-protect 
     (progn (setf sk8::lockedScroller ,theScroller)
            ,@body)
     (setf sk8::lockedScroller nil)))


#|
	Change History (most recent last):
	1		 4/22/94	Hernan	New file. Some actor functionality now implemented as 
							mixins.
	2		 7/5/94	Hernan	1172294: I no longer let you set both the resizes-
							contents and boundedByContents to true at the
							same time.
	3		 7/18/94	Hernan	1173466: bindByContents does nothing when the actor to
							be bound has no contents.
	4  	 9/12/94	Hernan  	1180502: capitalizing object names.
	5  	 9/28/94	Hernan  	Changing these back to actor flags. No need for
							the mixins, will just have to complicate the moved
							and resized methods of Actor a bit.
	6  	10/ 7/94	Hernan  	1191190: set resizesContents complains if the 
							actor is already bounded by contents.
	7  	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
