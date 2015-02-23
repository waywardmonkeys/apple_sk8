;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :sk8dev)

(provide "SPLITTER")

(require "RECTANGLE")

;;; _______________________________
;;; The Splitter.
;;; _______________________________

(new rectangle :objectname "Splitter" :project sk8
     :properties '((actorAbove :value nil)         ;; Actors whose bottom aligns to splitter.
                   (actorBelow :value nil)         ;; Actors whose bottom aligns to the abslute bottom.
                   (actorsAffected :value nil)     ;; Ordered list of actors under splitter's influence.
                   (absoluteTop :value nil)        ;; The top end of the splitter's effect area.
                   (absoluteBottom :value nil)))   ;; The bottom end of the splitter's effect area.

;;; splitActors -- does all the work. Assumes the actors and the splitter are in the same container and
;;; the splitter is in the right place. Just sets the properties.

(define-handler splitActors (splitter actorsAbove actorsBelow actorsAffected 
                                        absoluteTop absoluteBottom)
  (setf (actorAbove me) actorsAbove)
  (setf (actorBelow me) actorsBelow) 
  (setf (actorsAffected me) actorsAffected)
  (setf (absoluteTop me) absoluteTop)
  (setf (absoluteBottom me) absoluteBottom))

(define-handler mouseEnter (splitter)
  (setf (cursor stage) cursorHSplitter))

(define-handler mouseLeave (splitter)
  (setf (cursor stage) standardCursor))

;;; Aligns itself with car of its actorAbove. Spans its container horizontally.

(define-handler bestsize (splitter)
  (let ((width (width (container me)))
        (mainActorAbove (car (actorAbove me)))
        v)
    (if mainActorAbove
      (setf v (bottom mainActorAbove))
      (setf v (v me)))
    (setBoundsRect me 0 v width (+ v 2))))

(define-handler mousedown (splitter)
  (drag me :live nil))

;;; See how far the last guy can be pushed before it breaks.

(defun find-safe-dv (shifters proposedDv)
  (let ((safeArea (abs proposedDv))
        newSafeArea)
    (dolist (c shifters safeArea)
      (sk8-multival-bind (minh minv) (minimumSize c)
        (declare (ignore minh))
        (setf newSafeArea (- (height c) minv))
        (when (< newSafeArea safeArea)
          (setf safeArea newSafeArea))))
    (if (= safeArea (abs proposedDv))
      proposedDv
      (if (plusp proposedDv)
        (max safeArea 0)
        (min (* -1 safeArea) 0)))))

(defun fix-splitter-damage (self oldTop)
  (let* ((newTop (top self))
         (dv (- newTop oldTop))
         (actAbove (actorAbove self))
         (unhappyActors (cdr (member (car (last actAbove)) (actorsAffected self))))
         (actorsAtBottom (actorBelow self))
         (safeDv (find-safe-dv (if (plusp dv) actorsAtBottom actAbove) dv)))
    (unless (= safeDv dv)
      ;; Pull the splitter back to safeDv territory.
      (setf (v self :relative t) (if (plusp dv) (- (- dv safedv)) (abs (- dv safedv)))))
    (unless (zerop safedv)
      ;; Make the actors above larger.
      (dolist (c actAbove)
        (setf (bottom c :relative t) safedv))
      ;; Push everyone below the splitter and above the actors at the bottom.
      (dolist (aSadGuy unhappyActors)
        (when (eq aSadGuy (car actorsAtBottom))
          (return))
        (setf (v aSadGuy :relative t) safedv))
      ;; Resize the actors at the bottom.
      (dolist (bottomGuy actorsAtBottom)
        (sk8-multival-bind (ll tt rr bb) (boundsRect bottomGuy)
          (setBoundsRect bottomGuy ll (+ tt safedv) rr bb))))))

(define-handler drag (splitter &key live otherActors dropEvents DraggedOverEvents 
                                draggingMouseWithinEvents onStage constrainingRect)
  (declare (ignore live otherActors dropEvents DraggedOverEvents 
                   draggingMouseWithinEvents onStage))
  (let ((oldTop (top me))
        (abstop (absolutetop me))
        (absbot (absolutebottom me))
        )
    (sk8-multival-bind (ll tt rr bb) (boundsRect (container me) :physical t)
      (declare (ignore bb))
      (incf abstop tt)
      (incf absbot tt)
      (withActorLocked (me)
        (sk8-multival-bind (h v) (location me)
          (unless constrainingrect (setf constrainingRect (list ll abstop rr absbot)))
          ;; [1] Do the drag.
          (call-next-method me :live nil :constrainingRect constrainingrect)
          ;; [2] Deal with the result. (only if the location has changed!)
          (sk8-multival-bind (newh newV) (location me)
            (unless (and (= newH h) (= newV v))
              (fix-splitter-damage me oldTop))))
        ))))

#|
	Change History (most recent last):
	1	11/5/93	hernan	New file. Implements a horizontal splitter. Some
				day will make it vertical.
	2	1/11/94	hernan	self -> me
	3	2/12/94	kleiman	renaming
	4	2/21/94	rod	Modified the drag of splitter to use the absolute
				top and absolute bottom as part of the 
				constraining rect.
	6	4/18/94	rod	Making splitter more friendly to users.
	7	4/29/94	rod	Fixed spliiter from it's habit of setting the bottom
				of "ActorsBelow" to the absolute bottom.
	8  	 9/12/94	Hernan  	1180502: capitalizing object names.
	2  	 6/23/95	Hernan  	1249801: fix-splitter-damage should not be called if the
						splitter has not moved.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
