
;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; _______________________________________________________________________
;;;                              PIXMAP CACHING 
;;;                           by Hernan Epelman-Wang
;;; _______________________________________________________________________

;;; This file containes code required to implement pixmap caching of actors. When an actor requires
;;; to be cached, we draw it into the *pixmap-port* and then copyBits to the actor's own pixmap in
;;; the node structure. A new draw function is added, called draw-pixmap which draws by copying
;;; the bits. It is also important to let all its subactors know that they have been cached. They are
;;; immediately made dirty.
;;;
;;; When a subActor of the cached pixmap gets dirty, we make the pixmap actor know its pixmap 
;;; dirty. The draw-pixmap function has to recreate the pixmap in this case before the bits can be
;;; copied.

(in-package :graphics-system)

#| Modification History

01-22-93 ruben id -> objectName

|#


;;; 1.0
;;; CACHED-PIXMAP -- this structure holds the pixmap that actor caches. It is installed into the
;;;                pixmap field of the node.

(defstruct (cached-pixmap (:conc-name cpix-)
                            (:print-function 
                             (lambda (s theStream d)
                               (declare (ignore d))
                               (format thestream "<Cached Pixmap: ~a>"
                                       (cpix-gWorld s)))))
  (gWorld nil)
  (drawFunction nil))

;;; 1.0
;;; INITIALIZE-CACHED-PIXMAP -- this function is called to initialize the pixmap of an actor, either at creation
;;;                         time or when the actor is attached to the window. This does the work of
;;;                         setting the pointers to the gworld properly.

(defun initialize-cached-pixmap (me)
  (let ((pixmap (node-pixmap me))
        theGWorld)
    (sk8-multival-bind (width height) (sk8dev::size me :physical t)
      ;; Make the gWorld.
      (rlet ((r :rect :topleft 0 :right (max 1 (f.round width)) :bottom (max 1 (f.round height))))
        (setf theGWorld (T_newGWorldGC r)))
      ;; If there was a saved gWorld, dispose it.
      (when (and pixmap (macptrp (cpix-gWorld pixmap)))
        (t_deactivategcosptr (cpix-gWorld pixmap) :disposeNow t))
      ;; Set the slot in the struct.
      (when theGWorld
        (setf (cpix-gWorld pixmap) theGWorld)
        t))))

(defun initialize-cached-pixmap-from-parent (actor)
  (let ((parent (sk8::baseParent actor)))
    ;; Dirty trick: clear the flag and let the real method do all the work!
    (cachesPixmap! (node-flags actor) 0)
    (setf (sk8::cachesPixmap actor) t)
    ;; Important: replace the old Drawfunction with its parents.
    (setf (cpix-drawFunction (node-pixmap actor))
          (cpix-drawFunction (node-pixmap parent)))))

;;; 1.0
;;; CACHE-PIXMAP -- this function is called to tell actor that its pixmap should be
;;;                cached. The function is clever enough not to cache a pixmap when the flags
;;;                say the pixmap is already cached. This commonsense behaviour can be overridden by
;;;                setting the :force keyword to t (this is necessary when restoring and loading
;;;                and actor which caches its pixmap).

(defun cache-pixmap (actor &key force)
  (let ((flags (node-flags actor))
        pix)
    (cond ((hasWindow? flags)
           (sk8-error sk8::GeneralProgrammaticError
                      :strings '("" " (a top level actor) already caches its pixmap.")
                      :objects (list actor)))
          ((belongsToCachedPixmap? flags)
           (sk8-error sk8::GeneralProgrammaticError
                      :strings '("" " is already contained in a cached pixmap.")
                      :objects (list actor)))
          (t (when (or force (not (cachesPixmap? flags)))
               ;; create the pixmap struct and install it in the node.
               (setf pix (make-cached-pixmap))
               (setf (cpix-drawFunction pix) (node-drawFunction actor))
               (setf (node-pixmap actor) pix)
               (pixmapDirty! flags)
               ;; tell the object it now caches its pixmap.
               (setf (node-drawFunction actor) 'draw-pixmap)
               ;; tell its subactors that they are contained by an actor who caches its pixmap
               (map-subnodes* #'(lambda (node)
                                  (belongsToCachedPixmap! (node-flags node)))
                              actor)
               (cachesPixmap! flags)
               ;; It is important to redraw at this point to fill the bits in the pixmap.
               (sk8::forceRedraw actor))))))

;;; 1.0
;;; UNCACHE-PIXMAP -- This function is called to uncache the pixmap that actor is caching.
;;;                 Nothing happens if no pixmap is cached. Note that when a pixmap caching actor
;;;                 is preserved/restored, we clear the caching info but keep the flag that tells
;;;                 the actor it should be caching its bits. This is what the :force keyword is
;;;                 used for.

(defun uncache-pixmap (actor &key force)
  (let ((flags (node-flags actor)))
    (when (cachesPixmap? flags)
      (let ((pixmap (node-pixmap actor)))
        ;; restore the draw function.
        (setf (node-drawFunction actor) (cpix-drawFunction pixmap))
        ;; dispose the memory used.
        ;; (#_disposePtr (cpix-baseAddr pixmap))
        ;; tell all subnodes that they do not belong to a pixmap anymore.
        (map-subnodes* #'(lambda (node)
                           (belongsToCachedPixmap! (node-flags node) 0))
                       actor)
        ;; tell all subnodes that they are dirty.
        (dirty-subnodes* actor)
        ;; clear the pixmap struct
        (setf (node-pixmap actor) nil)
        (unless force (cachesPixmap! flags 0))))))
       
(defun revalidate-cached-pixmap-internal (me)
  (declare (special *fast-draw*))
  (let+ ((qd-rect (:rect :sk8Rect (recompute-physicalBoundsRect me)))
         (draw-region (:region))
         (theRect (:rect))
         thePort thePixmap)
    ;; Ok: draw the thing!
    (when (initialize-cached-pixmap me)
      (setf thepixmap (node-pixmap me))
      (setf thePort (cpix-gWorld thePixMap))
      (with-locked-pixels-force ((#_GetGworldPixmap theport))
        (with-port thePort
          (#_setOrigin (rref qd-rect :rect.left) (rref qd-rect :rect.top))
          ;; This line makes it work!
          (#_SetRect theRect -3000 -3000 3000 3000)
          (#_ClipRect theRect)
          (#_rectRgn (rref thePort :cgrafport.visrgn) theRect)
          (#_rectRgn draw-region qd-rect)
          ;; Now draw the actor! Make all the regions dirty so that they
          ;; get recomputed!!!
          (sk8::makeBoundsRegion me)
          (sk8::makeFillRegion me)
          (sk8::makeFrameRegion me)
          (#_copyRgn (node-boundsRegion me) draw-region)
          ;; It is important to switch off fast-draw before drawing because we want to draw
          ;; EVERYTHING since the pixmap is empty to start with.
          (setf *fast-draw* nil)
          (with-clipped-region draw-region
            (funcall (cpix-drawFunction thePixMap) me thePort draw-region))
          (pixmapDirty! (node-flags me) 0)
          ;; Tell contained actors that they are dirty!
          (dirty-subnodes* me)))
      t)))

;;; If the actor is on that stage or is contained by something else, we call the function above
;;; to do the work. Otherwise we have to make the actor be in the right state (the physical boundsRect's corners
;;; should be 0,0 or bad things happen). Thus we move the actor to (0,0), capture and then restore
;;; the original position. This is a pain. This problem is caused by our clever hack to save the window's location
;;; in the physical rect when removing the actor from the stage.

(defun revalidate-cached-pixmap (me)
  (if (and (not (node-container me))
           (not (hasWindow? (node-flags me))))
    (sk8-multival-bind (ll tt rr bb) (sk8::boundsRect me :physical t)
      (let ((h (- rr ll))
            (v (- bb tt)))
        (unwind-protect 
          (progn
            (sk8::setBoundsRect me 0 0 h v :physical t)
            (revalidate-cached-pixmap-internal me))
          (sk8::setBoundsRect me ll tt rr bb :physical t))))
    (revalidate-cached-pixmap-internal me)))

;;; Should never be called without a rect!!!

(defun transfer-cached-pixmap (cachedActor theport rect)
  (let* ((gWorld (cpix-gWorld (node-pixmap cachedActor)))
         (source-qd-rect (gworld-rectangle gWorld)))
    ;; Copying: pixmap to pixmap.
    (#_ForeColor #$blackColor)
    (#_BackColor #$whiteColor)
    (lock-gWorld gWorld)
    (copy-bits (#_getGWorldPixmap gWorld)
                        (#_GetGWorldPixMap theport) 
                        source-qd-rect rect)
    (unlock-gWorld gWorld)))

;;; DRAW-PIXMAP -- draws an actor which caches its pixmap.

(defun draw-pixmap (actor port draw-region)
  (let+ ((flags (node-flags actor))
         (effective-draw (:region))
         (bounds (node-boundsRegion actor)))
    (recompute-bounds-region actor flags)
    (recompute-fill-region actor flags)
    (recompute-frame-region actor flags)
    ;; redraw the pixmap if necessary.
    (when (pixmapDirty? flags)
      (unless (revalidate-cached-pixmap actor)
        (return-from draw-pixmap nil)))
    ;; copy the bits with the draw-region clipped.
    (#_sectRgn draw-region bounds effective-draw)
    (with-clipped-region effective-draw
      (rlet ((r :rect))
        (region-into-rect bounds r)
        (transfer-cached-pixmap actor port r)))
    ;; subtract this actor's bounds from the draw region.
    (#_diffRgn draw-region bounds draw-region)))

#|
	Change History (most recent last):
	2	6/25/93	Hernan	The Great Renaming of 93.
	3	9/1/93	hernan	The great integrating build for d4!!!
	4	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	5	10/1/93	hernan	Fixing things related to nodes going away.
	6	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	7	11/29/93	hernan	Using let+ to get the ports. (thus getting rid of
				the pixmap-port variables.
	8	2/28/94	hernan	Now using GCable mac things.
	9	4/15/94	Hernan	Reimplemented using the methods from snapshots.
	10	4/20/94	Hernan	Replacing calls to get-record-field with calls to
				region-into-rect (which does not cons).
	11 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	12 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	13 	 3/16/95	till    	wrappin' some traps, #_lockpixels to revalidate-cached-pixmap-internal
	14 	 3/17/95	till    	Better to use with-locked-pixels-force
	2  	 4/10/96	Hernan  	Removing with-fast-node-slots.
	3  	 4/18/96	Hernan  	Making *fast-draw* be stack based.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
