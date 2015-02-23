;;; SK8 Built-In RoundRect Actor Definition
;;;
;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)

(provide "ROUNDRECT")
(require "ACTOR")

#| Modification History

01-22-93 ruben id -> objectName
10-03-92 ruben removed #% -> xs0
09-20-92 hernan changing this to work with MF VI
07-26-92 ruben changed to SK8 package
07-19-92 ruben converted to 1.0
05-12-92 ruben d29 conversion
03-29-92 ruben began d29; roundedness; swept for "get-" and "set-"
03-26-92 ruben gentemp name in NEW
03-25-92 ruben call-next-method
01-19-92 converted to MacFrames II
05-29-91 ruben draw _invertrect => _invertrgn for :hilite
05-23-91 ruben use stack where possible
03-26-91 Ice made a few changes to bring it in line with the SK8 1.0a2 event system.
             In particular, added autohilite code.
03-23-91 ruben converted to 1.0a2

|#



;; RoundRect
;; the round button is a button with rounded corners,
;; much like the buttons usually found in Mac dialog
;; boxes.  The button has a piece of text centered
;; in it.

(new Actor :objectname "RoundRect" :undisposable t :prototype t :project sk8)
(setf (private RoundRect) nil) ; PUBLIC API

;;; roundedness -- returns the roundedness for the actor
;;;

(define-handler roundedness (RoundRect)
  (let ((plist (gs:node-properties me)))
    (SK8-multivals (getf plist :hroundedness 'auto)
                   (getf plist :vroundedness 'auto))))

;;; (setf roundedness) -- use it to set the amount of roundedness of the actor
;;;

(define-handler setRoundedness (RoundRect width height)
  (setf (getf (gs:node-properties me) :hroundedness) width
        (getf (gs:node-properties me) :vroundedness) height)
  (if (gs:hasWindow? (gs:node-flags me))
    (progn
      (setf (container me) nil)
      (setf (container me) Stage))
    ;; And cause a redraw to happen!
    (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds nil)
      )))

(define-handler (setf roundedness) (roundedness RoundRect)
  (definitely-recover-list roundedness)
  (sk8-destructuring-bind (width height) roundedness
    (setRoundedness me width height)))

;;; calc-roundedness -- given a horizontal and vertical roundednes, returns appropiate
;;;                 h and v roundedness given the setting
;;;

(defun calc-roundedness (theRoundButton h v)
  (cond ((or (eq h 'oval) (eq h 'auto))
         (let (roundedness width height)
           (sk8-multival-setf (width height) 
                              (gs:rect-size (gs:recompute-physicalBoundsRect theRoundButton)))
           (setf roundedness (min width height))
           (when (eq h 'auto)
             (setq roundedness (gs:f* roundedness 2/3)))
           (SK8-multivals roundedness roundedness)))
        (t
         (SK8-multivals h v))))

(define-handler makeBoundsRegion (RoundRect)
  (gs:let+ ((plist (gs:node-properties me))
            h v
            (qd-rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me)))
            (rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me))))
    (sk8-multival-setf (h v)
                       (calc-roundedness me
                                         (getf plist :hroundedness 'auto)
                                         (getf plist :vroundedness 'auto)))
    (#_openrgn)
    (#_frameroundrect rect (gs:f.round h) (gs:f.round v))
    (#_closergn (gs:node-BoundsRegion me))
    (gs:boundsDirty! (gs:node-flags me) 0)))

(define-handler makeFillRegion (RoundRect)
  (gs:let+ ((plist (gs:node-properties me))
            h v hsize vsize
            (qd-rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me))))
    (sk8-multival-setf (h v) (calc-roundedness me
                                               (getf plist :hroundedness 'auto)
                                               (getf plist :vroundedness 'auto)))
    (sk8-multival-setf (hsize vsize) (frameSize me :physical t))
    (setq hsize (round hsize) vsize (round vsize))
    (#_openrgn)
    (#_insetrect qd-rect hsize vsize)
    ;; Subtract the total frame width & height from the roundedness to get the inner radius!
    (#_frameroundrect qd-rect
     (max 0 (- (gs:f.round h) (* 2 hsize)))
     (max 0 (- (gs:f.round v) (* 2 vsize))))
    (#_closergn (gs:node-fillRegion me))
    (gs:fillDirty! (gs:node-flags me) 0)))

(define-handler makeFrameRegion (RoundRect)
  (let ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (gs:recompute-fill-region me flags)
    (#_diffRgn (gs:node-boundsRegion me)
     (gs:node-fillRegion me)
     (gs:node-frameRegion me))
    (gs:frameDirty! flags 0)))

(setBoundsRect RoundRect 0 0 27 18)

;;; _______________________________ 
;;; Editable properties. 
;;; _______________________________ 

(define-handler localvirtualproperties (roundRect)
  (when (eq me roundRect)
    '(roundedness)))

#|
	Change History (most recent last):
	2	6/11/93	Brian Roddy	Set the BoundsRect to something pretty for my browsers...
	8	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	9	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	10	11/12/93	chip	fixed makeFillRegion so the roundedness of the fill is inset from the outer roundedness
	11	11/30/93	hernan	Making setRoundedness cause a redraw.
	12	1/11/94	hernan	self -> me
	13	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	14	4/5/94	kleiman	code review; approved for public API
	15	6/7/94	Hernan	Removing left over keywords in roundedness property.
	16 	 1/25/95	Hernan  	1214309: when the roundRect is a window, the
							set roundedness handler should update the window.
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
