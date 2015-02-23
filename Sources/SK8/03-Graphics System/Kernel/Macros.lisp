(in-package :graphics-system)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

02-22-93 ruben obsoleted without-preconceptions
01-22-93 ruben id -> objectName
07-26-92 ruben exports moved to exports.lisp
05-12-92 ruben d29 conversion
03-25-92 adam with-temporary-port returns last form in body
03-24-92 adam coords-distance * 1.0 due to coerce bug
08-22-91 ruben render macro uses new SK8-COLOR-OBJECT defstruct
02-25-91 ICE Converted to MCL 2.0

|#

;;; _______________________________ 
;;; Some structs. 
;;; _______________________________ 

;;; RECT -- The 128-bit accuracy fixed-point-representation SK8 rectangle structure 

(defstruct (rect)
  (left 0)
  (top 0)
  (right 0)
  (bottom 0))

;;; IMPORTANT: I am here redefining the structure accesing to be done via
;;;           %svref. This means that if any of these accessors get passed 
;;;           something that is not a rect, bad things will happen.
;;;           (all we need to undo this change is to comment out the next
;;;           few lines and build again).

(fmakunbound 'rect-left)
(fmakunbound 'rect-top)
(fmakunbound 'rect-right)
(fmakunbound 'rect-bottom)

(ccl::def-accessors ccl::%svref
  nil
  rect-left
  rect-top
  rect-right
  rect-bottom)

;;; _____________________
;;; Node FLAGS. A cons of the form: (userFlags . systemFlags).
;;; _____________________

(defmacro set-bit (int bitPos)
  `(LOGIOR (ASH 1 ,bitPos) ,int))

(defmacro clear-bit (int bitPos)
  `(LOGAND (LOGNOT (ASH 1 ,bitPos)) ,int))

;;; Define the bit accessor and the setter. IntAccessor tells which integer in the flags to look out.
;;; Should be car or cdr. 

(defmacro def-flag-bit (accessor setter intAccessor bitPosition)
  (let ((forms nil))
    ;; Define accessor.
    (push `(defmacro ,accessor (flags) 
             `(logbitp ,,bitPosition (the fixnum (,',intAccessor (the cons ,flags)))))
          forms)
    ;; Define setter.
    (push `(defmacro ,setter (flags &optional (setting 1))
             (cond ((eql setting 1)
                    `(setf (,',intAccessor (the cons ,flags)) 
                           (set-bit (,',intAccessor (the cons ,flags)) ,,bitPosition)))
                   ((eql setting 0)
                    `(setf (,',intAccessor (the cons ,flags)) 
                           (clear-bit (,',intAccessor (the cons ,flags)) ,,bitPosition)))
                   (t (error "The optional arg to ~a must be 1 or 0." ',setter))))
          forms)
    ;; Define them.
    `(progn ,@(nreverse forms))))

;;; System Flags.

(def-flag-bit opaque? opaque! cdr 0)
(def-flag-bit frameDirty? frameDirty! cdr 1)
(def-flag-bit fillDirty? fillDirty! cdr 2)
(def-flag-bit boundsDirty? boundsDirty! cdr 3)
(def-flag-bit hilited? hilited! cdr 4)
(def-flag-bit hasConnectors? hasConnectors! cdr 5)
(def-flag-bit hasWindow? hasWindow! cdr 6)
(def-flag-bit onlyLocationChanged? onlyLocationChanged! cdr 7)
(def-flag-bit physicalBoundsRectDirty? physicalBoundsRectDirty! cdr 8)
(def-flag-bit logicalBoundsRectDirty? logicalBoundsRectDirty! cdr 9)
(def-flag-bit nodeSeenDirtyByWindow? nodeSeenDirtyByWindow! cdr 10)
(def-flag-bit pixmapDirty? pixmapDirty! cdr 11)
(def-flag-bit belongsToCachedPixmap? belongsToCachedPixmap! cdr 12)
(def-flag-bit cachesPixmap? cachesPixmap! cdr 13)
(def-flag-bit contentsDirty? contentsDirty! cdr 14)
(def-flag-bit boundsToDirtyRegion? boundsToDirtyRegion! cdr 15)
(def-flag-bit fillToDirtyRegion? fillToDirtyRegion! cdr 16)
(def-flag-bit frameToDirtyRegion? frameToDirtyRegion! cdr 17)
(def-flag-bit belongsToBoundedByContents? belongsToBoundedByContents! cdr 18)
(def-flag-bit someRenderersAreDynamic? someRenderersAreDynamic! cdr 19)
(def-flag-bit fillColorIsDynamic? fillColorIsDynamic! cdr 20)
(def-flag-bit textColorIsDynamic? textColorIsDynamic! cdr 21)
(def-flag-bit frameColorIsDynamic? frameColorIsDynamic! cdr 22)

;;; User Flags.

(def-flag-bit visible? visible! car 0)
(def-flag-bit autohilite? autohilite! car 1)
(def-flag-bit autotab? autotab! car 2)
(def-flag-bit ownedRegion? ownedRegion! car 3)
(def-flag-bit boundedByContents? boundedByContents! car 4)
(def-flag-bit resizesContents? resizesContents! car 5)
(def-flag-bit wantsIdle? wantsIdle! car 6)
(def-flag-bit wantsMouseWithin? wantsMouseWithin! car 7)
(def-flag-bit draggable? draggable! car 8)
(def-flag-bit resizable? resizable! car 9)
(def-flag-bit inverted? inverted! car 10)
(def-flag-bit floats? floats! car 11)
(def-flag-bit acceptsDrops? acceptsDrops! car 12)
(def-flag-bit objectDirty? objectDirty! car 13)
(def-flag-bit offsetsRegions? offsetsRegions! car 14)

;;; These two macros are used for debugging purposes only!

(defmacro actflags (actor)
  `(node-flags ,actor))

(defmacro translucentContents? (actor)
  `(> (node-translucentContents ,actor) 0))

(defmacro add-to-translucent-contents (actor)
  `(incf (node-translucentContents ,actor)))

(defmacro remove-from-translucent-contents (actor)
  `(decf (node-translucentContents ,actor)))

(defmacro point-in-rect-p (SK8Point SK8Rect &optional (slop 0))
  `(let ((pt ,SK8Point))
     (coords-in-rect-p (pt-h pt) (pt-v pt) ,SK8Rect ,slop)))

(defmacro set-point (SK8Point h v)
  `(let ((SK8Point ,SK8Point))
     (setf (pt-h SK8Point) ,h
           (pt-v SK8Point) ,v)))

(defmacro set-rect (SK8Rect left top right bottom)
  `(let ((SK8Rect ,SK8Rect))
     (setf (rect-top SK8Rect) ,top
           (rect-left SK8Rect) ,left
           (rect-bottom SK8Rect) ,bottom
           (rect-right SK8Rect) ,right)))

(defmacro set-rect-size (SK8Rect width height &key maintain-center)
  (if maintain-center
    `(let ((SK8Rect ,SK8Rect)
           (width ,width)
           (height ,height))
       (setf (rect-left SK8Rect) (f- (faverage (rect-left SK8Rect) (rect-right SK8Rect)) (fhalf width))
             (rect-top SK8Rect) (f- (faverage (rect-top SK8Rect) (rect-bottom SK8Rect)) (fhalf height)))
       (set-rect-size SK8Rect width height))
    `(let ((SK8Rect ,SK8Rect))
       (setf (rect-right SK8Rect) (f+ (rect-left SK8Rect) ,width)
             (rect-bottom SK8Rect) (f+ (rect-top SK8Rect) ,height)))))

;;; 1.0
;;; Sets the size of rects of top level actors (top left must be 0 0).

(defmacro simple-set-rect-size (SK8Rect width height)
  (let ((rect (gensym)))
    `(let ((,rect ,SK8Rect))
       (setf (rect-right ,rect) ,width
             (rect-bottom ,rect) ,height
             (rect-left ,rect) 0s0
             (rect-top ,rect) 0s0))))

(defmacro simple-set-physical-rect-size (SK8Rect width height)
  (let ((rect (gensym)))
    `(let ((,rect ,SK8Rect))
       (setf (rect-right ,rect) (f.round ,width)
             (rect-bottom ,rect) (f.round ,height)
             (rect-left ,rect) 0
             (rect-top ,rect) 0))))

(defmacro set-rect-center (SK8Rect x y)
  `(let* ((SK8Rect ,SK8Rect)
          (offseth (f- ,x (faverage (rect-left SK8Rect) (rect-right SK8Rect))))
          (offsetv (f- ,y (faverage (rect-top SK8Rect) (rect-bottom SK8Rect)))))
     (offset-rect SK8Rect offseth offsetv)))

(defmacro copy-SK8Rect (SK8Rect1 SK8Rect2)
  `(let ((SK8Rect1 ,SK8Rect1)
         (SK8Rect2 ,SK8Rect2))
     (setf (rect-top SK8Rect2) (rect-top SK8Rect1)
           (rect-left SK8Rect2) (rect-left SK8Rect1)
           (rect-bottom SK8Rect2) (rect-bottom SK8Rect1)
           (rect-right SK8Rect2) (rect-right SK8Rect1))))

(defmacro rect-size (SK8Rect)
  `(locally
     (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
     (let ((SK8Rect ,SK8Rect))
       (SK8-multivals (f- (rect-right SK8Rect) (rect-left SK8Rect))
                      (f- (rect-bottom SK8Rect) (rect-top SK8Rect))))))

(defmacro rect-center-coords (SK8Rect)
  `(locally
     (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
     (let ((SK8Rect ,SK8Rect))
       (SK8-multivals
        (faverage (rect-left SK8Rect) (rect-right SK8Rect))
        (faverage (rect-top SK8Rect) (rect-bottom SK8Rect))))))

(defmacro rect-center-point (SK8Rect &optional SK8Point)
  `(locally
     (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
     (let ((SK8Rect ,SK8Rect)
           (SK8Point ,(if SK8Point SK8Point `(make-pt))))
       (set-point SK8Point
                  (faverage (rect-left SK8Rect) (rect-right SK8Rect))
                  (faverage (rect-top SK8Rect) (rect-bottom SK8Rect)))
       SK8Point)))

(defmacro coords-distance (x1 y1 x2 y2)
  `(locally
     (declare (optimize (safety 0) (space 1) (debug 0) (speed 3)))
     (let ((hdist (* 1.0 (f- ,x1 ,x2)))  
           (vdist (* 1.0 (f- ,y1 ,y2))))
       (float-to-fixed (sqrt (+ (* hdist hdist) (* vdist vdist)))))))

(defmacro distance (SK8Point1 SK8Point2)
  `(let ((SK8Point1 ,SK8Point1)
         (SK8Point2 ,SK8Point2))
     (coords-distance (pt-h SK8Point1) (pt-v SK8Point1) (pt-h SK8Point2) (pt-v SK8Point2))))

;;; LOGICAL-TO-PHYSICAL                                                [macro]
;;; logical scale offset
;;; Converts a single coordinate from logical space to physical space.
;;; logical => the coordinate in logical space
;;; scale   => the scale for the coordinate's axis (xScale or yScale)
;;; offset  => the offset for the coordinate's axis (oX or oY)

(defmacro logical-to-physical (logical scale offset)
  `(f.round (f* (f- ,logical ,offset) ,scale)))

;;; PHYSICAL-TO-LOGICAL                                                [macro]
;;; physical scale offset
;;; Converts a single coordinate from physical space to logical space
;;; physical => the coordinate in physical space
;;; scale    => the scale for the coordinate's axis (xScale or yScale)
;;; offset   => the offset for the coordinate's axis (oX or oY)

(defmacro physical-to-logical (physical scale offset)
  `(f+ (f/ ,physical ,scale) ,offset))

(defmacro withLockedWindow (clos-window &body body)
  `(unwind-protect
     (progn
       (when ,clos-window (lockWindow ,clos-window))
       ,@body)
     (when ,clos-window (unlockWindow ,clos-window))))

(defmacro withLockedActor (actor &body body)
  (let ((clos-window (gensym)))
    `(let ((,clos-window (node-window ,actor)))
       (withLockedWindow ,clos-window
         ,@body))))

(defmacro window-unlocked-p (window-struct)
  `(and (not *lock-screen*) (not (slot-value ,window-struct 'locked))))

(defmacro withLockedMenubar (&body body)
  `(unwind-protect
     (progn 
       (setf ccl::*menubar-frozen* t)
       ,@body)
     (setf ccl::*menubar-frozen* nil)
     (ccl::draw-menubar-if)))
     
(defmacro withDrawDisabled (window &body body)
  (let ((oldState (gensym)))
    `(let ((,oldState (slot-value ,window 'drawEnabled)))
       (unwind-protect
         (progn (setf (slot-value ,window 'drawEnabled) nil)
                ,@body)
         (setf (slot-value ,window 'drawEnabled) ,oldState)))))

(defmacro object-under-mouse ()
  `(car *object-under-mouse*))

;; is this obsolete? We may still need it to clear the entry when closing a window
(defmacro set-object-under-mouse (object)
   `(rplaca *object-under-mouse* ,object))

;;; 1.0
;;; WITH-CLIPPED-REGION -- This macro clips to region, executes the body and restores the
;;;                     original clip region.

(defmacro with-clipped-region (region &body body)
  (let* ((old-region (gensym)))
    `(let+ ((,old-region (:region)))
       (require-trap #_getClip ,old-region)
       (require-trap #_setClip ,region)
       (prog1 (progn ,@body)
         (require-trap #_setClip ,old-region)))))

;;; Composes *clip* with region and calls with-clipped-region.

(defmacro with-composed-clip (region &body body)
  (let* ((temp-rgn (gensym))
         (current-clip (gensym)))
    `(let+ ((,temp-rgn (:region))
            (,current-clip (:region)))
       (require-trap #_getClip ,current-clip)
       (require-trap #_sectRgn ,region ,current-clip ,temp-rgn)
       (with-clipped-region ,temp-rgn
         ,@body))))

;;; 1.0
;;; REGIONS-INTERSECT -- returns t if the two regions intersect.

(defmacro regions-intersect (reg1 reg2)
  (let ((reg3 (gensym)))
    `(let+ ((,reg3 (:region)))
       (require-trap #_sectRgn ,reg1 ,reg2 ,reg3)
       (not (require-trap #_emptyRgn ,reg3)))))

(defmacro rects-intersect (rect1 rect2)
  (let ((rect3 (gensym)))
    `(let+ ((,rect3 (:rect)))
       (require-trap #_sectRect ,rect1 ,rect2 ,rect3)
       (not (require-trap #_emptyRect ,rect3)))))    

;;_______________________________________________________________________________
;; END MACROS.LISP
;;_______________________________________________________________________________

;;; doContents is used by the graphics system

(defmacro doContents ((element-var theactor) &body body)
  (let* ((v (gensym "VECTOR-"))
         (index (gensym "INDEX-")))
    `(let* ((,v (node-contents ,theActor))
            (,index (length ,v))
            ,element-var)
       (loop
         (when (minusp (setq ,index (ccl::%i- ,index 1)))
           (return))
         (setq ,element-var (aref ,v ,index))
         ,@body))))

(defmacro doContents-btof ((element-var theactor) &body body)
  `(dovector (,element-var (node-contents ,theActor))
     ,@body))

;;; Gets the rect out of a region without consing. Note that REALLY BAD
;;; things will happen if a region is not passed!!!

(defmacro region-into-rect (region rect)
  `(%region-into-rect ,region ,rect))

;;; FIND-PHYSICAL-OFFSET -- returns the amount the object has moved. This is done by comparing the physical boundsRect
;;;                     (which is kept current throughout the moving) to the boundsRegion which is not kept current.

(defun find-physical-offset (boundsRect boundsRegion)
  (let (dh dv)
    (rlet ((r :rect))
      (region-into-rect boundsRegion r)
      (setf dh (- (rect-left boundsRect) (rref r :rect.left))
            dv (- (rect-top boundsRect) (rref r :rect.top))))
    (sk8-multivals dh dv)))

(defun find-region-offset (fromRgn toRgn)
  (let (dh dv)
    (rlet ((fromRect :rect)
           (toRect :rect))
      (region-into-rect fromRgn fromRect)
      (region-into-rect toRgn toRect)
      (setf dh (- (rref fromRect :rect.left) (rref toRect :rect.left))
            dv (- (rref fromRect :rect.top) (rref toRect :rect.top))))
    (sk8-multivals dh dv)))

;;; ______________________________
;;; Stubs for vector access and all that.
;;; ______________________________

(defmacro node-container (actor)
  `(slot-value ,actor 'sk8::container))

(defmacro node-contents (actor)
  `(slot-value ,actor 'sk8::contents))

(defmacro node-window (actor)
  `(slot-value ,actor 'sk8::OSWindow))

(defmacro node-fillRegion (actor)
  `(slot-value ,actor 'sk8::fillRegion))

(defmacro node-frameRegion (actor)
  `(slot-value ,actor 'sk8::frameRegion))

(defmacro node-frameSize (actor)
  `(slot-value ,actor 'sk8::frameSize))

(defmacro node-boundsRegion (actor)
  `(slot-value ,actor 'sk8::boundsRegion))

(defmacro node-logicalBoundsRect (actor)
  `(slot-value ,actor 'sk8::logicalBoundsRect))

(defmacro node-physicalBoundsRect (actor)
  `(slot-value ,actor 'sk8::physicalBoundsRect))

(defmacro node-logicalXScale (actor)
  `(slot-value ,actor 'sk8::logicalXScale))

(defmacro node-logicalYScale (actor)
  `(slot-value ,actor 'sk8::logicalYScale))

(defmacro node-xScale (actor)
  `(slot-value ,actor 'sk8::xScale))

(defmacro node-yScale (actor)
  `(slot-value ,actor 'sk8::yScale))

(defmacro node-xOrigin (actor)
  `(slot-value ,actor 'sk8::xOrigin))

(defmacro node-yOrigin (actor)
  `(slot-value ,actor 'sk8::yOrigin))

(defmacro node-framecolor (actor)
  `(slot-value ,actor 'sk8::framecolor))

(defmacro node-fillcolor (actor)
  `(slot-value ,actor 'sk8::fillcolor))

(defmacro node-flags (actor)
  `(slot-value ,actor 'sk8::flags))

(defmacro node-mouseStatus (actor)
  `(slot-value ,actor 'sk8::mouseStatus))

(defmacro node-pixmap (actor)
  `(slot-value ,actor 'sk8::pixmap))

(defmacro node-drawFunction (actor)
  `(slot-value ,actor 'sk8::drawFunction))

(defmacro node-contentsDrawFunction (actor)
  `(slot-value ,actor 'sk8::contentsDrawFunction))

(defmacro node-properties (actor)
  `(slot-value ,actor 'sk8::props))

(defmacro node-translucentContents (actor)
  `(slot-value ,actor 'sk8::translucentContents))

(defmacro node-offsetby (actor)
  `(slot-value ,actor 'sk8::offsetby))

(defmacro node-lines (actor)
  `(slot-value ,actor 'sk8::lines))

(defmacro node-doubleClickStyle (actor)
  `(slot-value ,actor 'sk8::doubleClickStyle))

;;; RECOMPUTE-FRAME-REGION -- If the frame region is dirty, it is recomputed.

(defmacro recompute-frame-region (actor flags)
  `(when (frameDirty? ,flags) 
     (update-frame-region ,actor ,flags)))

;;; RECOMPUTE-FILL-REGION -- if the fill region is dirty, it is recomputed.

(defmacro recompute-fill-region (actor flags)
  `(when (fillDirty? ,flags) 
     (update-fill-region ,actor ,flags)))

;;; RECOMPUTE-BOUNDS-REGION -- If the bounds region is dirty it is recomputed.

(defmacro recompute-bounds-region (actor flags)
  `(when (boundsDirty? ,flags) 
     (update-bounds-region ,actor ,flags)))

#|
	Change History (most recent last):
	2	9/29/93	kleiman	nodes obsoleted
	3	9/29/93	hernan	sk8::properties -> sk8::props
	4	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	5	10/1/93	hernan	Exporting with-fast-slots to mf.
	6	10/1/93	hernan	Forgot the node-window.
	7	10/5/93	hernan	Redefining with-fast-slots to use the fast slot
				accessor stuff. Also renaming it with-fast-node-slots.
	8	10/5/93	hernan	Added the node slot block definition.
	9	10/5/93	hernan	Getting rid of two REALLY old macros having to do
				with author mode stuff.
	10	12/2/93	hernan	Moving describe flags to the end of the file.
	11	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	12	2/22/94	hernan	clos-window -> OSWindow.
	13	2/25/94	hernan	Using symbols instead of keywords for options!!!
	14	3/2/94	Hernan	Porting to Fred 3.0
	15	3/6/94	Hernan	Obsoleted acceptsBoundsRectOptimization and
				reserved bit 31 for specific actors (though no
				one uses it yet.....)
	16	3/6/94	Hernan	Oops! Forgot one reference.
	17	3/24/94	Hernan	Adding withLockedMenubar which locks the
				menubar ON THE STAGE.
	18	3/24/94	Hernan	Adding withDrawDisabled.
	19	3/31/94	sidney	remove container and contents from actor's fast-slots because they are in actorCollection, not actor
	20	4/12/94	kleiman	doContents used by Graphics system
	21	4/20/94	Hernan	Adding a macro that calls %region-into-rect to
				avoid using get-record-field to get the rect of a
				region (because it conses).
	22	4/20/94	Hernan	Replacing calls to get-record-field with calls to
				region-into-rect (which does not cons).
	23	4/21/94	Brian	Fixing ordering problem (minor change.  look for "%%%%" to find what I did.
	24	4/26/94	Hernan	Defining the rect struct here!
	25 	 8/12/94	Hernan  	Added the macro with-composed-clip which
							composes the region given with the current clip
							and then calls with-clipped-region.
	26 	 8/31/94	Hernan  	*scrolling* -> lockedScroller.
	27 	 9/16/94	Hernan  	Made rect struct and svref accessors agree on
							the order of the fields (can yuo believe that the
							system actually worked with this discrepancy?).
	28 	 9/26/94	chip    	moved dovector-in-reverse to mf-kernel;macros
	29 	 9/28/94	Hernan  	Flags are now two integers. Modified all the 
							macros to deal with this. The def-flag-bit macro
							is used to define the macros.
	30 	10/17/94	dy      	new xxxDynamic? flags
	31 	 2/14/95	sidney  	remove make-dlist, which was defined here a second time
	2  	 4/10/96	Hernan  	Removing with-fast-node-slots.
	3  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	4  	 5/ 7/96	sidney  	Changes for new object system
	5  	 7/18/96	sidney  	move without-events to earlier in build
	6  	 9/30/96	sidney  	no longer need set-object-under-mouse
						get rid of mode macros. If their needed, let them be functions!
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
