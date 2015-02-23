(in-package :graphics-system)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

01-22-93 ruben id -> objectName
09-26-92 ruben took out getObj
07-26-92 ruben moved exports into exports.lisp; cl-user -> sk8 package references where needed
05-30-92 ruben netwalk -> inheritsFrom?
05-12-92 ruben d29 conversion
03-30-92 ruben began d29
03-24-92 adam The coords-distance macro now works properly (so methods
              that used it now work properly -- point-distance-from-line)

|#

;;; _________________
;;; Debugging!!
;;; _________________

(defun flashRegion (actor region)
  (when (node-window actor)
    (with-focused-view (node-window actor)
      (#_invertRgn region)
      )))

(defun flashBounds (actor)
  (when (node-window actor)
    (with-focused-view (node-window actor)
      (#_invertRgn (node-boundsRegion actor))
      )))

(defun flashFill (actor)
  (when (node-window actor)
    (with-focused-view (node-window actor)
      (#_invertRgn (node-fillRegion actor))
      )))

(defun flashFrame (actor)
  (when (node-window actor)
    (with-focused-view (node-window actor)
      (#_invertRgn (node-frameRegion actor))
      )))

;;; _________________
;;; End of debugging.
;;; _________________

(defconstant 0.5pi (* 0.5 pi))
(defconstant 1.5pi (* 1.5 pi))
(defconstant 2pi (* 2 pi))

;;; 1.0
;;; DIFF-RECTS -- returns a region which is the difference of two rects. The region
;;;            must exist prior to this call.

(defun diff-rects (rect1 rect2 resultRegion)
  (let+ ((reg1 (:region :init-sk8rect rect1))
         (reg2 (:region :init-sk8rect rect2)))
    (#_DiffRgn reg1 reg2 resultRegion)))

;;; 1.0
;;; map-subnodes* -- maps function over all subactors in actor's tree (transitive closure)

(defun map-subnodes* (function actor)
  (when actor
    (dovector (subactor (node-contents actor))
      (funcall function subactor)
      (map-subnodes* function subactor))))

;;; 1.0
;;; map-nodes* -- maps function over all actors in actor's tree (including actor itself)

(defun map-nodes* (function actor)
  (when actor
    (funcall function actor)
    (map-subnodes* function actor)))

;;; 1.0
;;; map-subnodes -- maps function over immediate subactors in actor's tree

(defun map-subnodes (function actor)
  (when actor
    (dovector (subactor (node-contents actor))
      (funcall function subactor))))

;;; 1.0
;;; map-nodes -- maps function over this actor and its immediate subactors.

(defun map-nodes (function actor)
  (when actor
    (funcall function actor)
    (map-subnodes function actor)))

;;; 1.0
;;; DIRTY-SUBNODES -- This function is called to invalidate the regions and physical bounds
;;;                 rect of actor. This is necessary, since everytime we move an actor
;;;                 we just recompute its rects.

(defmacro dirty-one-node (flags)
  `(progn (physicalBoundsRectDirty! ,flags)
          (boundsDirty! ,flags)
          (fillDirty! ,flags)
          (frameDirty! ,flags)
          (onlyLocationChanged! ,flags 0)))

(defun dirty-subnodes* (actor)
  (map-subnodes* #'(lambda (subactor)
                     (let ((flags (node-flags subactor)))
                       (dirty-one-node flags)))
                 actor))

(defun dirty-nodes* (actor)
  (map-nodes* #'(lambda (subactor)
                  (let ((flags (node-flags subactor)))
                    (dirty-one-node flags)))
              actor))

;;; -----------------------------------------
;;; COORDINATE SYSTEM CONVERSION UTILITIES!
;;; -----------------------------------------

;;; 1.0
;;; REAL-PHYSICAL-TO-LOGICAL -- translates physical coords (in window space) to the logical
;;;                          coordinates of the container.
;;; 
;;; To do this, find the distance in pixels between the point and the top left point of the
;;; container. Multiply this by the scale of the container and add the origin.

(defun real-physical-to-logical (container lx ly)
  (cond ((null container) (SK8-multivals lx ly))
        (t 
         (let ((physRect (recompute-physicalBoundsRect container)))
           (SK8-multivals
            (+ (/ (- lx (rect-left physRect)) (node-xScale container))
               (node-xOrigin container))
            (+ (/ (- ly (rect-top physRect)) (node-yScale container))
               (node-yOrigin container)))))))
  
;;; 1.0
;;; REAL-LOGICAL-TO-PHYSICAL -- returns the physical analog of a logical coordinate of a point
;;;                          of an actor.
;;;
;;; To compute this we get the distance in pixels between the point and the top left point of its
;;; container. Then we add the distance in pixels between the top left point and the position of
;;; the window.

(defun real-logical-to-physical (container lx ly)
  (if container
    (let ((boundsRect (recompute-physicalBoundsRect container)))
      (SK8-multivals
       (f.round 
        (+ (rect-left boundsRect)
           (* (- lx (node-xOrigin container)) (node-xScale container))))
       (f.round 
        (+ (rect-top boundsRect)
           (* (- ly (node-yOrigin container)) (node-yScale container))))))
    (SK8-multivals (f.round lx) (f.round ly))))

(defun window-topLeft-offset (actor)
  (let ((thewindow (node-window actor)))
    (if thewindow
      (let ((thePos (view-position theWindow)))
        (sk8-multivals (point-h thePos) (point-v thePos)))
      (progn
        (setf theWindow (sk8::window actor))
        (if (neq actor thewindow)
          (let ((theRect (node-physicalBoundsRect theWindow)))
            (sk8-multivals (rect-left theRect) (rect-top theRect)))
          (sk8-multivals 0 0))))))
                
;;; 1.0
;;; WINDOW-TO-STAGE-COORDS -- offsets x and y by the position of the window they sit in.

(defun window-to-stage-coords (actor x y)
  (sk8-multival-bind (dh dv) (window-topleft-offset actor)
    (sk8-multivals (+ dh x) (+ dv y))))

(defun stage-to-window-coords (actor x y)
  (if actor
    (sk8-multival-bind (dh dv) (window-topleft-offset actor)
      (sk8-multivals (- x dh) (- y dv)))
    (sk8-multivals x y)))

(defun stage-to-window-list (actor point-list)
  (let (result x y)
    (loop 
      ;; we are done when the original list is empty.
      (when (null point-list)
        (return-from stage-to-window-list result))
      ;; get the points and reduce the list.
      (setf x (car point-list)
            y (cadr point-list)
            point-list (cddr point-list))
      ;; translate the points to window coords and push into the result list.
      (SK8-multival-bind (newx newy) (stage-to-window-coords actor x y)
        (push newy result)
        (push newx result)))))

(defun stage-to-logical-list (actor point-list)
  (let ((result (list 0)) x y)
    (loop 
      ;; we are done when the original list is empty.
      (when (null point-list)
        (pop result)
        (return-from stage-to-logical-list result))
      ;; get the points and reduce the list.
      (setf x (car point-list)
            y (cadr point-list)
            point-list (cddr point-list))
      ;; translate the points to window coords and push into the result list.
      (SK8-multival-bind (newx newy) (stage-to-window-coords actor x y)
        (SK8-multival-bind (logx logy) (real-physical-to-logical actor newx newy)
          (nconc result (list logX logY)))))))
          
(defun logical-to-stage-list (actor point-list container)
  (declare (ignore container))
  (let ((result (list 0)) x y)
    (loop 
      ;; we are done when the original list is empty.
      (when (null point-list)
        (pop result)
        (return-from logical-to-stage-list result))
      ;; get the points and reduce the list.
      (setf x (car point-list)
            y (cadr point-list)
            point-list (cddr point-list))
      ;; translate the points to window coords and push into the result list.
      (SK8-multival-bind (newx newy) (real-logical-to-physical actor x y)
        (SK8-multival-bind (physX physY) (window-to-stage-coords actor newx newy)
          (nconc result (list physX physY)))))))

(defun window-to-stage-rect (actor rect)
  (let ((x (rect-left rect))
        (xx (rect-right rect))
        (y (rect-top rect))
        (yy (rect-bottom rect)))
    (sk8-multival-bind (dh dv) (window-topleft-offset actor)
      (sk8-multivals (+ dh x) (+ dv y) (+ dh xx) (+ dv yy)))))

(defun stage-to-window-rect (actor rect)
  (let ((x (rect-left rect))
        (xx (rect-right rect))
        (y (rect-top rect))
        (yy (rect-bottom rect)))
    (sk8-multival-bind (dh dv) (window-topleft-offset actor)
      (SK8-multivals (- x dh) (- y dv) (- xx dh) (- yy dv)))))
      
(defun window-to-stage-rect-coords (actor x y xx yy)
  (sk8-multival-bind (dh dv) (window-topleft-offset actor)
    (sk8-multivals (+ dh x) (+ dv y) (+ dh xx) (+ dv yy))))
  
(defun stage-to-window-rect-coords (actor x y xx yy)
  (sk8-multival-bind (dh dv) (window-topleft-offset actor)
    (sk8-multivals (- (f.round x) dh) (- (f.round y) dv) (- (f.round xx) dh) (- (f.round yy) dv))))

(defun user-to-logical-rect-coords (actor x y xx yy)
  (if actor
    (progn
      (sk8-multival-setf (x y xx yy) (stage-to-window-rect-coords actor x y xx yy))
      (sk8-multival-setf (x y) (real-physical-to-logical actor x y))
      (sk8-multival-setf (xx yy) (real-physical-to-logical actor xx yy))
      (sk8-multivals x y xx yy))
    (sk8-multivals x y xx yy)))

(defun logical-to-user-rect-coords (actor x y xx yy)
  (if actor
    (sk8-multival-bind (newx newy) (real-logical-to-physical actor x y)
      (sk8-multival-bind (newXx newYy) (real-logical-to-physical actor xx yy)
        (window-to-stage-rect-coords actor newX newY newXX newYy)))
    (sk8-multivals x y xx yy)))

(defun copy-SK8Rect-and-round (source dest)
  (setf (rect-left dest) (f.round (rect-left source))
        (rect-top dest) (f.round (rect-top source))
        (rect-right dest) (f.round (rect-right source))
        (rect-bottom dest) (f.round (rect-bottom source))))

;;; 1.0
;;; RECOMPUTE-PHYSICALBOUNDSRECT -- recomputes the physical bounds rect of its
;;;                               actor, and of all actors on its path to the top
;;;                               level object.
;;;
;;; Recomputes the physicalBoundsRect of its container and adds its
;;; local displacement to it.

(defun recompute-physicalBoundsRect (actor)
  (let ((flags (node-flags actor))
        (container (node-container actor)))
    (if (not (physicalBoundsRectDirty? flags))
      (node-physicalBoundsRect actor)
      (if (or (not container) (hasWindow? flags))
        (progn (copy-SK8Rect-and-round (node-logicalBoundsRect actor) (node-physicalBoundsRect actor))
               (physicalBoundsRectDirty! flags 0)
               (node-physicalBoundsRect actor))
        (let* ((log-rect (node-logicalBoundsRect actor))
               (phys-rect (node-physicalBoundsRect actor))
               (xScale (node-xScale container))
               (yScale (node-yScale container))
               (containerphysicalleft 0) 
               (containerphysicaltop 0)
               left top)
          (declare (fixnum left top containerphysicalleft containerphysicaltop))
          (when (node-container container)
            (let ((container-flags (node-flags container)))
              (when (not (haswindow? container-flags))
                (let ((container-rect (if (physicalBoundsRectDirty? container-flags)
                                        (recompute-physicalBoundsRect container)
                                        (node-physicalBoundsRect container))))
                  (setf containerPhysicalLeft (rect-left container-rect)
                        containerPhysicalTop (rect-top container-rect))))))
          ;; Compute the topleft.
          (setf left (+ (the fixnum (logical-to-physical (rect-left log-rect) xScale (node-xOrigin container))) 
                        containerPhysicalLeft)
                top (+ (the fixnum (logical-to-physical (rect-top log-rect) yScale (node-yOrigin container))) 
                       containerPhysicalTop))
          ;; Set the rect. Bottom right computed by offseting from topleft.
          (set-rect phys-rect
                    left
                    top
                    (ccl::%i+ left (f.round (* xScale (- (rect-right log-rect) (rect-left log-rect)))))
                    (ccl::%i+ top (f.round (* yScale (- (rect-bottom log-rect) (rect-top log-rect))))))
          (physicalBoundsRectDirty! flags 0)
          phys-rect)))))

;;; 1.0
;;; RECOMPUTE-LOGICALBOUNDSRECT -- returns the logical bounds rect of actor. If the rect is not
;;;                              dirty or the actor is a window, we are done. Otherwise,
;;;                              we use the physical bounds rect of the container, together
;;;                              with its scale and origin to calculate the new logical rect.

(defun recompute-logicalBoundsRect (actor)
  (let ((flags (node-flags actor))
        (container (node-container actor)))
    (if (not (logicalBoundsRectDirty? flags))
      (node-logicalBoundsRect actor)
      (if (or (not container) (hasWindow? flags))
        (progn (copy-SK8Rect-and-round (node-physicalBoundsRect actor) (node-logicalBoundsRect actor))
               (logicalBoundsRectDirty! flags 0)
               (node-logicalBoundsRect actor))
        (let* ((phys-rect (node-physicalBoundsRect actor))
               (log-rect (node-logicalBoundsRect actor))
               (ox (node-xOrigin container))
               (oy (node-yOrigin container))
               (xScale (node-xScale container))
               (yScale (node-yScale container))
               (containerRect (recompute-physicalBoundsRect container))
               (containerTop (rect-top containerRect))
               (containerLeft (rect-left containerRect)))
          (when (and (null (node-container container)) (not (haswindow? (node-flags container))))
            (setf containerTop 0 
                  containerLeft 0))
          (set-rect log-rect
                    (physical-to-logical (- (rect-left phys-rect) containerLeft) xScale ox)
                    (physical-to-logical (- (rect-top phys-rect) containerTop) yScale oy)
                    (physical-to-logical (- (rect-right phys-rect) containerLeft) xScale ox)
                    (physical-to-logical (- (rect-bottom phys-rect) containerTop) yScale oy))
          (logicalBoundsRectDirty! (node-flags actor) 0)
          log-rect)))))

;;; A handy function

(defun list-to-vector (theList)
  (let ((length (length theList)))
    (make-array length :fill-pointer t :adjustable t :initial-contents theList)))

(defun vector-to-list (theVector &optional len)
  (let ((length (if len
                  (min len (length theVector))
                  (length theVector))))
    (when (> length 0)
      (let* ((theList (cons (aref (the vector theVector) 0) nil))
             (next theList)
             (i 1))
        (loop
          (when (eql i length) (return))
          (setf (cdr next) (cons (aref (the vector theVector) i) nil))
          (setq next (cdr next))
          (incf i))
        theList))))

;;; While the mouse button is down, calls FUNCTION each time there is a change in the mouse-position; the arguments to
;;; FUNCTION are DH and DV of the mouse-position.  TRACK-MOUSEDOWN returns the net mouse offset as two values (h & v).
;;; If INITIAL-CALL is T, FUNCTION is called as soon as TRACK-MOUSEDOWN is invoked, with DH = DV = 0.

(defun track-mouseDown (function &key (initial-call t) (initial-mouseloc (view-mouse-position nil)))
  (let+ (last-mouseloc
         mouseloc)
    (setq last-mouseloc initial-mouseloc
          mouseloc initial-mouseloc)
    (tagbody
      (when initial-call (go !CALL!))
      !TOP!
      (when (eql mouseloc last-mouseloc) (go !READMOUSE!))
      !CALL!
      (funcall function
               (- (point-h mouseloc) (point-h last-mouseloc))
               (- (point-v mouseloc) (point-v last-mouseloc)))
      (setq last-mouseloc mouseloc)
      !READMOUSE!
      (setq mouseloc (view-mouse-position nil))
      (when (mouse-down-p) (go !TOP!)))
    (SK8-multivals (- (point-h mouseloc) (point-h initial-mouseloc))
                   (- (point-v mouseloc) (point-v initial-mouseloc)))))

;;; Just like the one above but keeps calling the function even if the mouse does not move.

(defun eager-track-mouseDown (function &key (initial-call t) (initial-mouseloc (view-mouse-position nil)))
  (let+ (last-mouseloc
         mouseloc)
    (setq last-mouseloc initial-mouseloc
          mouseloc initial-mouseloc)
    (tagbody
      (when initial-call (go !CALL!))
      !TOP!
      !CALL!
      (funcall function
               (- (point-h mouseloc) (point-h last-mouseloc))
               (- (point-v mouseloc) (point-v last-mouseloc)))
      (setq last-mouseloc mouseloc)
      !READMOUSE!
      (setq mouseloc (view-mouse-position nil))
      (when (mouse-down-p) (go !TOP!)))
    (SK8-multivals (- (point-h mouseloc) (point-h initial-mouseloc))
                   (- (point-v mouseloc) (point-v initial-mouseloc)))))

(defun closest-qd-point-to (the-point &rest other-points)
  (let ((closest-metric most-positive-fixnum)
        (closest-point nil)
        metric)
    (dolist (other-point other-points)
      (setq metric (+ (abs (- (point-h the-point) (point-h other-point)))
                      (abs (- (point-v the-point) (point-v other-point)))))
      (when (< metric closest-metric)
        (setq closest-metric metric
              closest-point other-point)))
    closest-point))

;; 10% faster than #_PtInRect !!!

(defun coords-in-rect-p (x y SK8Rect &optional (slop 0))
  (locally
    (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
    (cond ((eq 0 slop)
           (and (<= (rect-left SK8Rect) x (rect-right SK8Rect))
                (<= (rect-top SK8Rect) y (rect-bottom SK8Rect))))
          (t
           (and (<= (f- (rect-left SK8Rect) slop) x (f+ (rect-right SK8Rect) slop))
                (<= (f- (rect-top SK8Rect) slop) y (f+ (rect-bottom SK8Rect) slop)))))))

;; 3 times faster than #_InsetRect !!!

(defun inset-rect (SK8Rect dh dv)
  (locally
    (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
    (fincf (rect-left SK8Rect) dh)
    (fincf (rect-top SK8Rect) dv)
    (fdecf (rect-right SK8Rect) dh)
    (fdecf (rect-bottom SK8Rect) dv)
    SK8Rect))

;; 3 times faster than #_OffsetRect !!!

(defun offset-rect (SK8Rect dh dv)
  (locally
    (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
    (fincf (rect-left SK8Rect) dh)
    (fincf (rect-top SK8Rect) dv)
    (fincf (rect-right SK8Rect) dh)
    (fincf (rect-bottom SK8Rect) dv)
    SK8Rect))

(defun rects-overlap-p (SK8Rect1 SK8Rect2)
  (locally
    (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
    (let ((left1   (rect-left   SK8Rect1))
          (top1    (rect-top    SK8Rect1))
          (right1  (rect-right  SK8Rect1))
          (bottom1 (rect-bottom SK8Rect1))
          (left2   (rect-left   SK8Rect2))
          (top2    (rect-top    SK8Rect2))
          (right2  (rect-right  SK8Rect2))
          (bottom2 (rect-bottom SK8Rect2)))
      (and (or (<= left1 left2 right1)
               (<= left1 right2 right1)
               (<= left2 left1 right2)
               (<= left2 right1 right2))
           (or (<= top1 top2 bottom1)
               (<= top1 bottom2 bottom1)
               (<= top2 top1 bottom2)
               (<= top2 bottom1 bottom2))))))

(defun union-rects (SK8Rect1 SK8Rect2 dest-SK8Rect)
  (locally
    (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
    (let ((left1   (rect-left   SK8Rect1))
          (top1    (rect-top    SK8Rect1))
          (right1  (rect-right  SK8Rect1))
          (bottom1 (rect-bottom SK8Rect1))
          (left2   (rect-left   SK8Rect2))
          (top2    (rect-top    SK8Rect2))
          (right2  (rect-right  SK8Rect2))
          (bottom2 (rect-bottom SK8Rect2)))
      (cond ((and (eq left1 right1) (eq top1 bottom1))
             (setf (rect-left dest-SK8Rect) left2
                   (rect-top dest-SK8Rect) top2
                   (rect-right dest-SK8Rect) right2
                   (rect-bottom dest-SK8Rect) bottom2))
            ((and (eq left2 right2) (eq top2 bottom2))
             (setf (rect-left dest-SK8Rect) left1
                   (rect-top dest-SK8Rect) top1
                   (rect-right dest-SK8Rect) right1
                   (rect-bottom dest-SK8Rect) bottom1))
            (t
             (setf (rect-left dest-SK8Rect) (if (< left1 left2) left1 left2)
                   (rect-top dest-SK8Rect) (if (< top1 top2) top1 top2)
                   (rect-right dest-SK8Rect) (if (> right1 right2) right1 right2)
                   (rect-bottom dest-SK8Rect) (if (> bottom1 bottom2) bottom1 bottom2))))
      dest-SK8Rect)))

(defun intersect-rects (SK8Rect1 SK8Rect2 dest-SK8Rect)
  (locally
    (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
    (let ((left1   (rect-left   SK8Rect1))
          (top1    (rect-top    SK8Rect1))
          (right1  (rect-right  SK8Rect1))
          (bottom1 (rect-bottom SK8Rect1))
          (left2   (rect-left   SK8Rect2))
          (top2    (rect-top    SK8Rect2))
          (right2  (rect-right  SK8Rect2))
          (bottom2 (rect-bottom SK8Rect2)))
      (setf (rect-left   dest-SK8Rect) (if (> left1   left2)   left1   left2)
            (rect-top    dest-SK8Rect) (if (> top1    top2)    top1    top2)
            (rect-right  dest-SK8Rect) (if (< right1  right2)  right1  right2)
            (rect-bottom dest-SK8Rect) (if (< bottom1 bottom2) bottom1 bottom2)))
    dest-SK8Rect))

(defun rect-to-SK8Rect (rect SK8Rect)
  (setf (rect-left SK8Rect) (int-to-fixed (rref rect :rect.left))
        (rect-top SK8Rect) (int-to-fixed (rref rect :rect.top))
        (rect-right SK8Rect) (int-to-fixed (rref rect :rect.right))
        (rect-bottom SK8Rect) (int-to-fixed (rref rect :rect.bottom))))

(defun SK8Point-to-point (SK8Point)
  (make-point (f.round (pt-h SK8Point)) (f.round (pt-v SK8Point))))

(defun SK8Coords-to-point (h v)
  (make-point (f.round h) (f.round v)))

(defun point-to-SK8Point (point SK8Point)
  (setf (pt-h SK8Point) (int-to-fixed (point-h point))
        (pt-v SK8Point) (int-to-fixed (point-v point))))

(defun point-to-SK8Coords (point)
  (SK8-multivals (int-to-fixed (point-h point))
                 (int-to-fixed (point-v point))))

(defun copy-point (SK8Point-source SK8Point-dest)
  (setf (pt-h SK8Point-dest) (pt-h SK8Point-source)
        (pt-v SK8Point-dest) (pt-v SK8Point-source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; "CONTAINER"-VECTOR MANIPULATION TOOLS:
;;;
     
(defun vector-move-item-before* (theVector item-index before-index)
  (let ((item (aref theVector item-index))
        i)
    (when (eq before-index :end) (setq before-index (length theVector)))
    (cond
     ((< item-index before-index)
      (decf before-index)
      (setq i item-index)
      (loop
        (when (eq i before-index) (return))
        (setf (aref theVector i) (aref theVector (incf i))))
      (setf (aref theVector before-index) item))
     (t
      (setq i item-index)
      (loop
        (when (eq i before-index) (return))
        (setf (aref theVector i) (aref theVector (decf i))))
      (setf (aref theVector before-index) item)))))

(defun vector-insert-before (theVector item before-index)
  (vector-push-extend item theVector)
  (when (neq before-index :end)
    (vector-move-item-before* theVector (1- (length theVector)) before-index)))

(defun vector-move-item-forward (theVector item)
  (let ((item-index (position item theVector)))
    (when (and item-index (neq item-index (1- (length theVector))))
      (vector-move-item-before* theVector item-index (+ 2 item-index)))))

(defun vector-move-item-backward (theVector item)
  (let ((item-index (position item theVector)))
    (when (and item-index (neq item-index 0))
      (vector-move-item-before* theVector item-index (1- item-index)))))

(defun vector-move-item-before (theVector item before-index)
  (vector-move-item-before* theVector (position item theVector) before-index))

;;____________________________________________________________________
;; MAKE-BITMAP
;; Basically, lifted from the MCL Quickdraw.lisp file.

(defun make-bitmap (r &aux rowbytes bm)
  (setq rowbytes 
        (logand
         #xfffe 
         (+ 2  (ash (- (pref r rect.right) (pref r rect.left) 1) -3))))
  (setq bm 
        (t_newPtr (+ 14 (* rowbytes (- (pref r rect.bottom) (pref r rect.top))))))
  (setf (pref bm bitmap.bounds) r)
  (setf (pref bm bitmap.rowbytes) rowbytes)
  (setf (pref bm bitmap.baseaddr) (%inc-ptr bm 14))
  bm)


;;____________________________________________________________________
;; STRING-REGION
;; Given a rect and a string, makes a region.

;;; Look how simple this is now!!! The cool trick is to make a
;;; gWorld 1 bit deep and use its bmap like pixmap. What a system!

(defun string-region (rect region theString x y fontNum size style)
  (let ((xf (rref rect :rect.left))
        (yf (rref rect :rect.top))
        gWorld)
    (#_OffsetRect rect (- xf) (- yf))
    (setf gWorld (T_NewGWorldGC rect :depth 1 :noGC t))
    (when (and (pointerp gWorld) (not (%null-ptr-p gWorld)))
      (unwind-protect 
        (with-pstrs ((pString theString))
          (without-interrupts
           (with-offscreen-gWorld 
            (gWorld)
            (#_ClipRect rect)
            (#_EraseRect rect)
            (#_textfont fontNum)
            (#_rgbforecolor *black-rgb*)
            (#_textsize size)
            (#_TextFace style)
            (#_moveto (- x xf) (- y yf))
            (#_drawstring pString))
           ;; convert it to a region
           (#_BitmapToRegion region (rref gWorld :grafport.portbits))
           (#_offsetRgn region xf yf)))
        (#_disposeGWorld gWorld))
      t)))

(defun disable-whiting ()
  (%put-word (%int-to-ptr #x9dc) 0))

(defun enable-whiting ()
  (%put-word (%int-to-ptr #x9dc) 65535))

(defun get-style-number (styleList)
  (let ((styleTable '((sk8::bold . 1) (sk8::italic . 2) (sk8::underline . 4) (sk8::outline . 8)
                      (sk8::shadow . 16) (sk8::condense . 32) (sk8::expand . 64)))
        (result 0)
        temp)
    (dolist (aStyle styleList result)
      (setf temp (cdr (assoc aStyle styleTable)))
      (when temp (incf result temp)))))

;;; clear-event-system-references -- clears references to disposed objects
;;;   in event system globals... too bad we have to do this!
;;;
(defun reset-event-system-globals ()
  (setf *eventactor* nil)
  ;; no longer so global -- in a let in the event handling (set-object-under-mouse nil)
  )

(defun rect-in-region-p (SK8Rect region)
  (locally
    (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
    (rlet ((r :rect))
      (SK8Rect-to-rect SK8Rect r)
      (#_RectInRgn r region))))

(defun make-dlist ()
  (make-array 0 :fill-pointer t :adjustable t))


#|
	Change History (most recent last):
	4	9/29/93	kleiman	nodes obsoleted
	5	10/1/93	hernan	=====================================
						= Sad but true: the node is gone!!! =
						=====================================
	6	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	7	11/12/93	kleiman	reset-event-system-globals
	8	11/24/93	chip	disposed now works on things that aren't standard-instances
	9	12/21/93	hernan	Checking for memory errors when making new
						mac stuff.
	10	12/21/93	sidney	Take out #. so we can compile the file
	11	1/7/94	sidney	big mf/ss merge
	12	1/10/94	hernan	Fonts just became objects!!!
	13	2/25/94	hernan	Using symbols instead of keywords for options!!!
	14	2/28/94	hernan	TextStyle symbols should be in SK8!!!
	15	2/28/94	hernan	Now using GCable mac things.
	16	3/1/94	kleiman	obsolete get-new-sk8-id
	17	3/6/94	Hernan	Added functions to do physicalToLogical stuff
						for lists and rects.
	18	3/9/94	Hernan	Getting rid of disposed.
	19	3/15/94	Hernan	Fixing window-to-stage-rect to use the location
						of the top level actor even when it is not on the
						stage (a clos-window is not available).
				                             *** IMPORTANT ***
						Subtle problem in recompute-physicalBoundsRect
						was corrected. As we go up the containment 
						hierarchy, we add the topleft of each physical
						rect to the previous one. This should be done
						always except when (1) the container is the 
						window, or (2) the container's node-container is
						nil. Now we just have to wait to see what else 
						breaks because of this fix...
	20	3/16/94	Hernan	Trying to fix the mess I have created with the 
						previous change.
	21	3/16/94	Hernan	Ooops. Screwed up a comment bracket.
	22	3/16/94	Hernan	Fixing more and more...
	23	3/16/94	Hernan	Rolling back to yesterday to really fix the label
						run away problem. 
				
						IGNORE COMMENTS FROM 3/15 and 3/16!!!
	24	3/17/94	Hernan	Fixing stage-to-window functions to get the
						window offset from the window and the tla if
						the window is not available.
	25	3/17/94	Hernan	Something's rotten in recompute-logicalboundsRect.
	26	3/18/94	Hernan	Fixing logical-to-user-rect-coords.
	27	3/23/94	Hernan	Fixing recompute-physicalboundsRect to only
						compute the topleft corner. The bottom right is
						computed by adding to the topleft the physical
						size of the actor. This will get rid of the large
						oscilations that result from round off errors.
	28	3/30/94	rod		stage-to-window-rect-coords rounds the results.
	29	3/31/94	Hernan	Real-logical-to-physical no longer returns floats.
	30	3/31/94	rod		Adding a function that make a non gcable gWorld.
	31	4/19/94	Hernan	Optimized a lot of recompute-physicalBoundsRect.
						Most involved declaring things of the right type.
	32	4/19/94	Hernan	Getting rid of consing in dirty-subnodes*.
	33	4/19/94	Hernan	Ooops! You don't want to know.
	34	6/25/94	sidney	use some safer functions for gworld allocation
	35	6/25/94	sidney	back out changes until we know why they crash
	36	7/14/94	Hernan	1174587: string-region rewritten, simpler and faster and 
						no longer making that hideous pixmap.
	37	8/12/94	Hernan  	1167773: track-mousedown functions now take the initial
						location of the mouse as an argument. This lets these
						functions correct for any change in the position of the mouse
						from the time when the handler (eg. resize) was invoked to
						the time in which the handler is actually ready to call this
						function.
	38 	 9/ 7/94	dy      	newGWorld -> T_NewGWorld :noGC t
	39 	 9/ 7/94	dy      	newGWorld -> T_NewGWorldGC :noGC t
	40 	 9/ 9/94	Hernan  	1185059: user-to-logical-stage-coords now
							returns the actor's logical values (not its container's).
	41 	 9/12/94	Hernan  	1186136: user-to-logical-rect-coords has to deal 
							with the possibility that the actor argument given
							to it is false.
	42 	 9/28/94	Hernan  	Adding dirty-nodes*.
	43 	10/ 7/94	Hernan  	Fixing stage-to-logical-list to stop using the container.
	44 	10/ 7/94	Hernan  	Adding a function to flash a region of an actor in 
							its window.
	45 	10/21/94	Hernan  	Stage-to-window-coords is fixed to work when
							the actor passed is False.
	46 	11/28/94	Hernan  	Defining dirty-one-node which marks all the regions dirty.
	47 	 1/25/95	dy      	add flashRegion
	48 	 3/17/95	till    	trap-wrapping, newptr in make-bitmap
	2  	11/ 9/95	sidney  	clear *event-actor* when we are clearing event globals
	2  	 4/10/96	Hernan  	Removing with-fast-node-slots.
	3  	 9/30/96	sidney  	no longer have to clear object-under-mouse
	4  	 2/20/97	Hernan  	Sanitizing comments...
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
