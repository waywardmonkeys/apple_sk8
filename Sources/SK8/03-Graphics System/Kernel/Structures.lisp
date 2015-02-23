;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :graphics-system)

#| Modification History

01-22-93 ruben id -> objectName
07-26-92 ruben moved exports into exports.lisp; cl-user -> sk8 package references where appropiate
06-13-92 ruben obsoleted used-by slot of sk8colorinfo -- it wasn't used
05-31-92 ruben clone-node no longer automatically creates object when it is not supplied
               (done because this is inefficient)
05-12-92 ruben d29 conversion
04-04-92 ruben graphics-method-p uses equalp, remove-graphics-method uses graphics-method-p and equalp
10-02-91 adam  introduced the SK8COLORINFO struct for use by the *sk8-colors* list and the DEFINE-SK8-COLOR macro
08-22-91 ruben added sk8-color-object
08-21-91 ruben removed unnecessary consing
03-10-91 ICE Finished first pass of the conversion.
02-25-91 ICE Converted to MCL 2.0

|#

;;; _______________________________ 
;;; STRUCTURES
;;; _______________________________ 

;;; Each Structures definitions consists of the defstruct,then the macros and
;;; functions needed to work with it.  SK8 uses these structures for its own
;;; simple internel object system, since the performance of MacFrames
;;; is currently not good enough to go fully native.  Thus, this may all
;;; change in the future.

;;; PT -- The 64-bit accuracy fixed-point-representation SK8 point structure

(defstruct (pt)
  (v #%0 :type fixed-point)
  (h #%0 :type fixed-point))

;;; and more macros ...

(defmacro mouseNormal? (actor)
  `(eql (node-mouseStatus ,actor) 0))
(defmacro mouseOpaque? (actor)
  `(eql (node-mouseStatus ,actor) 1))
(defmacro mouseTransparent? (actor)
  `(eql (node-mouseStatus ,actor) 2))
(defmacro mouseInvisible? (actor)
  `(eql (node-mouseStatus ,actor) 3))
(defmacro mouseCustom? (actor)
  `(eql (node-mouseStatus ,actor) 4))

(defmacro mouseNormal! (actor)
  `(setf (node-mouseStatus ,actor) 0))
(defmacro mouseOpaque! (actor)
  `(setf (node-mouseStatus ,actor) 1))
(defmacro mouseTransparent! (actor)
  `(setf (node-mouseStatus ,actor) 2))
(defmacro mouseInvisible! (actor)
  `(setf (node-mouseStatus ,actor) 3))
(defmacro mouseCustom! (actor)
  `(setf (node-mouseStatus ,actor) 4))

(defmacro hasNoContents? (actor)
  `(zerop (length (node-contents ,actor))))

;;; 1.0
;;; is-topLevelActor -- an actor is a top level actor if it has a window or
;;;                 its container is null.

(defmacro is-topLevelActor? (actor)
  `(or (null (node-container ,actor))
       (hasWindow? (node-flags ,actor))))

;;; 1.0
;;; CAUSES-TRANSLUCENT-DRAW -- Returns t if the actor whose flags we are examining will
;;;                           cause translucent drawing.

(defmacro causes-translucent-draw (actor flags)
  `(or (translucentContents? ,actor)
       (not (opaque? ,flags))))

#|

;;; 1.0 
;;; MAKE-FLAGS -- makes a flags bit vector.

(defmacro make-flags ()
  `(make-array 32 :element-type 'bit
               :initial-contents #*00000000000000000000000000000000))

|#

;;; DISPOSE-NODE-RECORDS -- Disposes of all of the secondary storage used by an actor.  Call this
;;;                        BEFORE allowing an actor to be garbage collected to 
;;;                        prevent memory leaks.

(defun dispose-node-records (actor)
  (when actor ; *** is this check necessary?
    (setf (node-fillRegion actor) nil)
    (setf (node-frameRegion actor) nil)
    (setf (node-boundsRegion actor) nil)))

;;; Since there is no reason to avoid using the object's region for this. Let us try it. If no hit
;;; is detectable in the rect test, we sould keep it. The next step is to invalidate regions other
;;; than the bounds.

(defmacro nodeRgn-into-windowDirtyRgn (window-dirty-region actor-dirty-region)
  `(require-trap #_UnionRgn ,actor-dirty-region ,window-dirty-region ,window-dirty-region))

;;; add-to-window-dirty-region -- adds the actor's dirty region to the window

(defmacro add-to-window-dirty-region (clos-window window-dirty-region actor-dirty-region)
  `(if ,window-dirty-region
     (nodeRgn-into-windowDirtyRgn ,window-dirty-region ,actor-dirty-region)
     (setf (slot-value ,clos-window 'dirty-window-p) t)))

(defmacro remove-from-window-dirty-nodes (actor window-struct)
  `(setf (slot-value ,window-struct 'dirty-nodes)
         (delete ,actor (slot-value ,window-struct 'dirty-nodes))))

;;; remove-dirty-from-window -- This function is called by "remove-self" when an object is
;;;                          taken out of a window -- it lets the window know that the region
;;;                          the object occupied has to be redrawn, and takes it off the
;;;                          window's "dirty-nodes" list.

(defun remove-dirty-from-window (actor clos-window)
  (with-slots (dirty-region) clos-window
    (remove-from-window-dirty-nodes actor clos-window)
    (nodeSeenDirtyByWindow! (node-flags actor) 0)
    (add-to-window-dirty-region clos-window dirty-region (node-boundsRegion actor))
    (when (slot-value clos-window 'drawOnlyDirty)
      (setf (slot-value clos-window 'drawOnlyDirty) nil))))

;;; 1.0
;;; nodeAlreadyDirty -- This is true when the actor was made dirty before this call
;;;                 to make-node-dirty. In this case we do not need to add the actor to the
;;;                 dirty actor list since it will be there already.

(defmacro nodeAlreadyDirty? (flags)
  `(or (frameDirty? ,flags) (fillDirty? ,flags)))

;;; 1.0
;;; CLEAR-DIRTY-CONTENTS-FLAG -- walks down the hierarchy from the top level actor, clearing the
;;;                           dirty contents flag.

(defun clear-dirty-contents-flag (actor)
  (let ((flags (node-flags actor)))
    (when (contentsDirty? flags)
      (contentsDirty! flags 0)
      (dovector (subactor (node-contents actor))
        (clear-dirty-contents-flag subactor)))))

;;; 1.0
;;; SET-DIRTY-CONTENTS-FLAG -- walks up the hierarchy setting the dirty contents bit.

(defun set-dirty-contents-flag (actor)
  (when actor
    (let ((flags (node-flags actor)))
      (unless (contentsDirty? flags)
        (contentsDirty! flags)
        (set-dirty-contents-flag (node-container actor))))))

;;; 1.0
;;; MAKE-PIXMAP-CACHER-DIRTY -- this function is called when an actor contained by a cached
;;;                           pixmap becomes dirty. The idea is to tell its pixmap caching
;;;                           container that the pixmap is dirty.

(defun make-pixmap-cacher-dirty (actor)
  (let ((flags (node-flags actor)))
    (if (cachesPixmap? flags)
      (pixmapDirty! flags)
      (make-pixmap-cacher-dirty (node-container actor)))))

;;; 1.0
;;; MAKE-REGIONS-DIRTY -- makes the regions dirty when specified by the args.

(defmacro make-regions-dirty (flags recompute-frame? recompute-fill? recompute-bounds? offset-does-it?)
  `(progn 
     ;; Let the actor know what parts need to be recomputed.
     (when (and ,recompute-frame? (not (frameDirty? ,flags)))
       (frameDirty! ,flags))
     (when (and ,recompute-fill? (not (fillDirty? ,flags)))
       (fillDirty! ,flags))
     (when (and ,recompute-bounds? (not (boundsDirty? ,flags)))
       (boundsDirty! ,flags))
     (when (and (not ,offset-does-it?) (onlyLocationChanged? ,flags))
       (onlyLocationChanged! ,flags 0))))

;;; Add-right-dirty-region-to-window -- distributed several times in the code of make-node-dirty.
;;;                              Computes the effective fill region to add to the dirty region
;;;                              of the window.
;;;  clos-window - a clos window instance
;;;  container - an actor
;;;  dirtyRegion - a region
;;;

(defmacro add-right-dirty-region-to-window (clos-window container dirtyRegion)
  (let ((tempRgn (gensym)))
    `(if ,container
       (let+ ((,tempRgn (:region)))
         (recompute-fill-region ,container (node-flags ,container))
         (require-trap #_sectRgn ,dirtyRegion (node-fillRegion ,container) ,tempRgn)
         (add-to-window-dirty-region ,clos-window (slot-value ,clos-window 'dirty-region) ,tempRgn))
       (add-to-window-dirty-region ,clos-window (slot-value ,clos-window 'dirty-region) ,dirtyRegion))))

;;; 1.0    
;;; MAKE-NODE-DIRTY -- This function is called by every method which modifies an actor
;;;                  such that its region should be redrawn in the next draw cycle.
;;;
;;; dirtyRegion is either :bounds, :fill, :frame.

(defun make-node-dirty (actor clos-window offset-does-it? recompute-bounds? 
                                 recompute-fill? recompute-frame? dirtyRegion
                                 add-to-dlist? fastRedraw?)
  (let* ((flags (node-flags actor))
         (container (node-container actor)))
    ;; Add the old region to the window's dirty region and, if necessary
    ;; add the actor to the list of dirty actors.
    ;; Adding the effective dirty region to the window's dirty region.
    (unless (nodeSeenDirtyByWindow? flags)
      ;; Adding the actor to the dirty list.
      (nodeSeenDirtyByWindow! flags)
      (vector-push-extend actor (slot-value clos-window 'dirty-nodes))
      ;; Tell the containers of this actor that they contain dirty things.
      (set-dirty-contents-flag container)
      ;; Adding the original dirty region.
      (add-right-dirty-region-to-window clos-window container dirtyRegion))
    ;; Record what part of the actor will enter the dirty region.
    (case add-to-dlist?
      (:bounds (unless (boundsToDirtyRegion? flags) 
                 (add-right-dirty-region-to-window clos-window container dirtyRegion)
                 (unless offset-does-it? (pixmapDirty! flags))
                 (boundsToDirtyRegion! flags)))
      (:fill (unless (or (boundsToDirtyRegion? flags) (fillToDirtyRegion? flags))
               (add-right-dirty-region-to-window clos-window container dirtyRegion)
               (pixmapDirty! flags)
               (fillToDirtyRegion! flags)))
      (:frame (unless (or (boundsToDirtyRegion? flags) (frameToDirtyRegion? flags))
                (add-right-dirty-region-to-window clos-window container dirtyRegion)
                (pixmapDirty! flags)
                (frameToDirtyRegion! flags))))
    ;; Let the actor know what parts need to be recomputed.
    (make-regions-dirty flags recompute-frame? recompute-fill? recompute-bounds? offset-does-it?)
    (when (and (slot-value clos-window 'drawOnlyDirty) (not fastRedraw?))
      (setf (slot-value clos-window 'drawOnlyDirty) nil))
    ;; if the actor belongs to a cached pixmap, make the pixmap dirty.
    (when (belongsToCachedPixmap? flags)
      (make-pixmap-cacher-dirty container))))

;;; 1.0
;;; MAKE-NEW-NODE-DIRTY -- this function makes an actor we just added to the window dirty.
;;;                      This is simpler than the normal case since we know that the actor is
;;;                      completely dirty (fill + frame). Since the actor was not in
;;;                      the window, we do not need to invalidate the old region.

(defun make-new-node-dirty (actor clos-window &key showingOnly)
  (let ((flags (node-flags actor)))
    (withLockedWindow clos-window
      (unless showingOnly
        (recompute-frame-region actor flags)
        (recompute-fill-region actor flags)
        (recompute-bounds-region actor flags))
      ;; Do not make this actor seen by the window since we can get its region
      ;; from the dirty region when add-dirty... collects.
      (vector-push-extend actor (slot-value clos-window 'dirty-nodes))
      (nodeSeenDirtyByWindow! flags 1)
      (onlyLocationChanged! flags 0)
      (boundsToDirtyRegion! flags)
      (when (slot-value clos-window 'drawOnlyDirty)
        (setf (slot-value clos-window 'drawOnlyDirty) nil)))))

(defun deeply-visible? (theActor)
  (let ((container theActor))
    (loop
      (unless (visible? (node-flags container))
        (return-from deeply-visible? nil))
      (setf container (node-container container))
      (unless container
        (return-from deeply-visible? t)))))

;;; 1.0 
;;; make-self-dirty -- will make appropiate parts of the actor dirty
;;;                Used as a wrapper around handlers which may change the
;;;                appearance of the actor
;;;
;;;     actor - the actor
;;;     location? - has the actor's location changed?
;;;     size? - has the actor's size changed?
;;;     fill? - has the actor's fill region changed?
;;;     frame? - has the actor's frame region changed?

;;; Note: SIZE? parameters taken out

(defmacro making-self-dirty ((actor offset-does-it? recompute-bounds? 
                                       recompute-fill? recompute-frame? dirtyRegion
                                       add-to-dlist? fastRedraw?) &body body)
  (let* ((clos-window (gensym))
         (flags (gensym)))
    `(let ((,clos-window (node-window ,actor))
           (,flags (node-flags ,actor)))
       (if ,clos-window
         (withLockedWindow ,clos-window
           (if (deeply-Visible? ,actor)
             (make-node-dirty ,actor ,clos-window ,offset-does-it? 
                              ,recompute-bounds? ,recompute-fill? ,recompute-frame?
                              ,dirtyRegion ,add-to-dlist? ,fastRedraw?)
             (make-regions-dirty ,flags ,recompute-frame? ,recompute-fill? ,recompute-bounds?
                                 ,offset-does-it?))
           ,@body)
         (progn ,@body)))))

;;; 1.0
;;; DLIST-EMPTY? -- macro that returns t when the vector passed is empty.

(defmacro dlist-empty? (dlist)
  `(zerop (length ,dlist)))

#|
	Change History (most recent last):
	2	9/29/93	kleiman	nodes obsoleted
	3	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	4	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	5	11/1/93	kleiman	without-events moved out of here into macframes
	6	12/21/93	hernan	Making sure we have a region before we try to
				dispose it.
	7	2/28/94	hernan	No need to dispose the regions since they will be
				GCed.
	8	4/19/94	Hernan	Redefining rect accessors and setters using
				def-accessors.
	9	4/19/94	Hernan	Exploring ways to reduce consing.
	10	4/26/94	Hernan	Moving the rect definition to the macro file.
	11 	 9/16/94	Hernan  	Removed obsolete stuff.
	12 	 9/28/94	Hernan  	Flags are now two integers.
	13 	11/ 5/94	Hernan  	make-node-dirty should make the actor be seen
							dirty by its window.
	14 	11/ 8/94	Hernan  	Moving deeply-visible? to here since it is used by
							making-self-dirty. Also changed making-self-dirty
							to only add the actor to the window when the 
							actor and all of its containers are visible. This will
							slow down the system a bit and forces us to do
							a recompile everything build. (dou!).
	2  	 4/16/96	Hernan  	Getting rid of *temp-region*.
	3  	 4/18/96	Hernan  	Making *fast-draw* be stack based.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
