;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(provide "SELECTIONDOTS")

(require "ACTOR")

(new actor :objectName "SelectionDots" :project sk8
     :properties '((DotSize :inherit)
                   selectedItems
                   (liveresize :inherit)
                   )
     :prototype t
     :undisposable t)

(setf (private SelectionDots) nil)  ; PUBLIC API

(setf (liveresize selectiondots) t)
(setf (fillcolor selectiondots) black)
(setf (dotsize selectiondots) '(10 10))
(setSize SelectionDots 120 90)

(defun make-dots-region (bounds ll tt rr bb sizeH sizeV)
  (gs:let+ ((temp (:region))
         MiddleH MiddleV halfsizeH halfsizeV
         (Dot1Rect (:rect))
         (Dot2Rect (:rect))
         (Dot3Rect (:rect))
         (Dot4Rect (:rect))
         (Dot5Rect (:rect))
         (Dot6Rect (:rect))
         (Dot7Rect (:rect))
         (Dot8Rect (:rect)))
    
    (setf MiddleH (+ (round (- rr ll) 2) ll)
          MiddleV (+ (round (- bb tt) 2) tt))
    
    (setf halfsizeH (round SIzeH 2)
          halfsizeV (round SIzeV 2))
    
    (rset Dot1Rect :rect.left ll)
    (rset Dot1Rect :rect.top tt)
    (rset Dot1Rect :rect.right (+ ll SIzeH))
    (rset Dot1Rect :rect.bottom (+ tt SizeV))
    
    (rset Dot2Rect :rect.left (- MiddleH halfsizeH))
    (rset Dot2Rect :rect.top tt)
    (rset Dot2Rect :rect.right (+ MiddleH (- halfsizeH) sizeH))
    (rset Dot2Rect :rect.bottom (+ tt SizeV))
    
    (rset Dot3Rect :rect.left (- rr SIzeH))
    (rset Dot3Rect :rect.top tt)
    (rset Dot3Rect :rect.right rr)
    (rset Dot3Rect :rect.bottom (+ tt SizeV))
    
    (rset Dot4Rect :rect.left (- rr SIzeH))
    (rset Dot4Rect :rect.top (- MiddleV halfsizeV))
    (rset Dot4Rect :rect.right rr)
    (rset Dot4Rect :rect.bottom (+ MiddleV (- halfsizeV) sizeV))
    
    (rset Dot5Rect :rect.left (- rr SIzeH))
    (rset Dot5Rect :rect.top (- bb SIzeV))
    (rset Dot5Rect :rect.right rr)
    (rset Dot5Rect :rect.bottom bb)
    
    (rset Dot6Rect :rect.left (- MiddleH halfsizeH))
    (rset Dot6Rect :rect.top (- bb SIzeV))
    (rset Dot6Rect :rect.right (+ MiddleH (- halfsizeH) sizeH))
    (rset Dot6Rect :rect.bottom bb)
    
    (rset Dot7Rect :rect.left ll)
    (rset Dot7Rect :rect.top (- bb SIzeV))
    (rset Dot7Rect :rect.right (+ ll SIzeH))
    (rset Dot7Rect :rect.bottom bb)
    
    (rset Dot8Rect :rect.left ll)
    (rset Dot8Rect :rect.top (- MiddleV halfsizeV))
    (rset Dot8Rect :rect.right (+ ll SIzeH))
    (rset Dot8Rect :rect.bottom (+ MiddleV (- halfsizeV) sizeV))
    
    (#_rectRgn bounds Dot1Rect)
    (#_rectRgn temp Dot2Rect)
    (#_UnionRgn bounds temp bounds)
    (#_rectRgn temp Dot3Rect)
    (#_UnionRgn bounds temp bounds)
    (#_rectRgn temp Dot4Rect)
    (#_UnionRgn bounds temp bounds)
    (#_rectRgn temp Dot5Rect)
    (#_UnionRgn bounds temp bounds)
    (#_rectRgn temp Dot6Rect)
    (#_UnionRgn bounds temp bounds)
    (#_rectRgn temp Dot7Rect)
    (#_UnionRgn bounds temp bounds)
    (#_rectRgn temp Dot8Rect)
    (#_UnionRgn bounds temp bounds)
    ))


(define-handler makeBoundsRegion (SelectionDots)
  (gs:let+ ((BoundsRectRect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me)))
            (bounds (gs:node-BoundsRegion me))
            ll tt rr bb SIzeH SIzeV
            (DotSize (DotSize me))
            )      
    (setf tt (rref BoundsRectRect :rect.top)
          bb (rref BoundsRectRect :rect.bottom)
          ll (rref BoundsRectRect :rect.left)
          rr (rref BoundsRectRect :rect.right))
    
    (setf SIzeH (car DotSize)
          SIzeV (cadr DotSize))
    
    (make-dots-region bounds ll tt rr bb sizeH sizeV)
    (gs:boundsDirty! (gs:node-flags me) 0)))


(define-handler makeFillRegion (SelectionDots)
  (let ((fill (gs:node-fillregion me))
        (flags (gs:node-flags me))
        frameX frameY)
    (gs:recompute-bounds-region me flags)
    (sk8-multival-setf (frameX frameY) (framesize me :physical t))
    (#_copyRgn (gs:node-boundsRegion me) fill)
    (#_InsetRgn fill frameX frameY)
    (gs:fillDirty! (gs:node-flags me) 0)))

(define-handler makeFrameRegion (SelectionDots)
  (let ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (gs:recompute-fill-region me flags)
    (#_diffRgn (gs:node-boundsRegion me) 
     (gs:node-fillRegion me)
     (gs:node-frameRegion me))
    (gs:frameDirty! flags 0)))

(define-handler (setf DotSize) :after
  (newsize SelectionDots)
  (declare (ignore newSize))
  (forceRedraw me)
  newsize)

(define-handler pointonwhichpart (selectiondots x y)
  (let* ((sizeH (car (dotsize me)))
         (sizeV (cadr (dotsize me)))
         (rect (gs:node-physicalBoundsRect me))
         (left (gs:f+ (gs:rect-left rect) sizeH))
         (top (gs:f+ (gs:rect-top rect) sizeV))
         (right (gs:f- (gs:rect-right rect) sizeH))
         (bottom (gs:f- (gs:rect-bottom rect) sizeV))
         (tla (sk8::window me)))
    ;; adjust for logical space
    (when tla
      (let ((loc (view-position (gs:node-window tla))))
        (setq x (- x (point-h loc))
              y (- y (point-v loc)))))
    (cond ((<= x left)
           (cond ((<= y top) 'topleft)
                 ((>= y bottom) 'bottomleft)
                 (t 'left)))
          ((>= x right)
           (cond ((<= y top) 'topright)
                 ((>= y bottom) 'bottomright)
                 (t 'right)))
          ((<= y top) 'top)
          ((>= y bottom) 'bottom)
          (t nil))))

(define-handler AddedMeAsParent (selectiondots child oldparents)
  (unless (some #'(lambda (x) (inheritsFrom x selectiondots)) oldparents)
    (setf (dotsize child) (dotsize selectiondots))
    (setf (selecteditems child) nil)
    ))

(define-handler (setf selectedItems) :after 
  (theItems selectiondots)
  (declare (ignore theItems))
  (fitToSelectedItems me))

(define-handler mousedown (selectiondots)
  (fitToSelectedItems me)
  (let ((theguys (if (listp (selecteditems me)) (selecteditems me) (list (selecteditems me)))))
    (when theguys
      (hide me)
      (resize (car theguys) 
              (pointonwhichpart me (eventh) (eventv)) 
              :otheractors (cdr theguys) 
              :live (liveresize me))
      (fittoselecteditems me)
      (show me)
      )))

(define-handler fitToSelectedItems (selectiondots)
  (let* ((theitems (selecteditems me))
         (oneitem (and theitems (not (listp theitems)))))
    (if theItems
      (progn
        (when oneitem (setf theitems (list theitems)))
        (setf theitems (remove-if #'(lambda (x) (eq x me)) theitems))
        (when theitems
          (sk8-multival-bind (hh vv) (dotsize me)
            (sk8-multival-bind (ll tt rr bb) (actorsbounds theItems :physical t)
              (setboundsrect me (- ll (round hh 2)) (- tt (round vv 2)) 
                             (+ rr (round hh 2)) (+ bb (round vv 2)) :physical t)))
          (bringtofront me))
        (if oneitem (setf theitems (car theitems)))
        (unless (equal theitems (selecteditems me)) (setf (selecteditems me) theitems)))
      (moveoffstage me))))


#|
	Change History (most recent last):
	2	5/11/93	Brian Roddy	
	3	6/10/93	Brian Roddy	
	4	6/18/93	Hernan	Set the prototype to true.
	1	6/24/93	Brian Roddy	
	2	6/25/93	Brian Roddy	
	8	9/20/93	rod	Dot's now square: fixed round off error
	9	10/1/93	rod	Removed Node References and Added With-fast-slots.
	10	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	11	10/18/93	rod	
	12	10/18/93	rod	
	14	2/18/94	rod	Added the AddingMeAsParent handler.
				Fixed location of dots when they select an object.
	18	3/8/94	rod	The Final Renaming for Alpha! (yeah, right...)
				173/1/94rodFixed POWP keyword problem
	19	3/28/94	Hernan	AddingMeAsParent -> addedMeAsParent.
	20	4/5/94	kleiman	selectionDots officially made a public object
	21	4/13/94	sidney	Add parameters to addedMeAsParent and removingMeAsParent to allow handlers to avoid work when an ancestor is added back in after a remove
	22	4/26/94	rod	Abstracting out the boundsregion stuff so the
				halo can use it in the interest of minimizing
				duplicate code.
	23	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	24 	 9/28/94	Hernan  	Getting rid of useless variable in makeBoundsRegion.
	25 	10/27/94	rod     	making no selected items move it offstage.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 7/ 7/96	sidney  	changes for native PPC build
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
