;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(provide "HALO")
(require "ACTOR")

(new actor :objectName "Halo" :project sk8
     :properties '(insetSize)
     :prototype t
     :undisposable t)

(setf (private halo) nil)   ; THIS IS A PUBLIC API OBJECT

(setf (insetsize halo) '(2 2))
(setsize halo 20 20)
(setf (fillcolor halo) Yellow)
(setf (prototype halo) t)

(define-handler AddedMeAsParent (halo child oldparents)
  (unless (some #'(lambda (x) (inheritsFrom x halo)) oldparents)
    (setf (insetsize child) (insetsize me))
    ))

;;; Should tell the graphics system the work is done! 

(define-handler makeFrameRegion (halo)
  (gs:frameDirty! (gs:node-flags me) 0))

;;; For the record, note that this is 2 times faster than doing 4 union regions!

(define-handler makeBoundsRegion (halo)
  (gs:let+ ((rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me)))
            (temp (:region))
            (bounds (gs:node-BoundsRegion me))
            (insetSize (insetSize me)))
    (#_rectRgn bounds rect)
    (#_copyRgn bounds temp)
    (#_insetRgn temp (car insetSize) (cadr insetSize))
    (#_diffRgn bounds temp bounds)
    (gs:boundsDirty! (gs:node-flags me) 0)))

(define-handler makeFillRegion (halo)
  (let* ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (#_copyRgn (gs:node-boundsRegion me) (gs:node-fillRegion me))
    (gs:fillDirty! flags 0)))

(define-handler (setf insetSize) :after
  (newsize halo)
  (declare (ignore newSize))
  (forceRedraw me))

(define-handler surroundObject (Halo actortosurround &key (outset '(0 0)))
  (if actortosurround
    (sk8-multival-bind (ll tt rr bb) (boundsrect actortosurround :physical t)
      (decf ll (car outset))
      (decf tt (cadr outset))
      (incf rr (car outset))
      (incf bb (cadr outset))
      (sk8-multival-bind (ll2 tt2 rr2 bb2) (boundsrect me :physical t)
        (sk8-multival-bind (hh vv) (insetsize me)
          (when (container me)
            (bringToFront me))
          (unless (and (eql (- ll hh) ll2)
                       (eql (- tt vv) tt2)
                       (eql (+ rr hh) rr2)
                       (eql (+ bb vv) bb2))
            (setboundsrect me (- ll hh) (- tt vv) (+ rr hh) (+ bb vv) :physical t)))))
    (moveoffstage me)))



#|
	Change History (most recent last):
	2	5/11/93	Brian Roddy	
	3	6/10/93	Brian Roddy	
	4	6/14/93	Brian Roddy	
	5	6/18/93	Hernan	Set the prototype to true.
	6	6/23/93	Brian Roddy	
	1	6/24/93	Brian Roddy	
	2	6/25/93	Brian Roddy	
	8	9/20/93	rod	
	9	10/1/93	rod	Removed Node References and Added With-fast-slots.
	10	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	11	11/19/93	rod	
	13	1/14/94	hernan	Solo se que no se nada.
	14	2/18/94	rod	Added the new parent stuff
	15	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	16	3/28/94	Hernan	AddingMeAsParent -> addedMeAsParent.
	17	4/5/94	kleiman	halo officially made a public object
	18	4/5/94	kleiman	
	19	4/8/94	Hernan	MakeFrameRegion marks the frame as not dirty.
	20	4/13/94	sidney	Add parameters to addedMeAsParent and removingMeAsParent to allow handlers to avoid work when an ancestor is added back in after a remove
	21 	 9/ 6/94	rod     	
	22 	12/ 4/94	rod     	Fixing bug in addedmeasparent
	23 	 4/12/95	rod     	
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
