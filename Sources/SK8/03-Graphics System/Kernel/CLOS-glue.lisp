(in-package :graphics-system)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

03-29-93 hernan *currentWindow* now holds the current clos-window.
01-31-93 ruben mf::my-frames slots for MENU and MENU-ITEM defined in MCL patch (in Patches To MCL)
01-22-93 ruben id -> objectName
07-26-92 ruben changed cl-user references to sk8 package references, where appropiate
07-06-92 hernan taking mode considerations out of the event dispatching functions. Click and
                double click computations are moved from the dispatchers to the default
                actor handlers.
07-01-92 hernan major rewriting of the engine for SK8 1.0!
05-30-92 ruben netwalk -> inheritsFrom
05-12-92 ruben d29 conversion
04-04-92 ruben (setf currentwindow) in window-select
03-30-92 ruben began d29
02-15-92 ruben sk8-null-event and window-event, also now use object-under-mouse macros
09:39:02  08-29-1991 RUBEN restore-graphics-memory no longer unconditionally dirtys the window
08-21-91 ruben direct gWorld traps or calls to gWorld macros
08-08-91 adam  ALL-NODES-FROM-BACK-TO-FRONT and ALL-SLINES-FROM-BACK-TO-FRONT now
               work even when a layer has no card;
              WINDOW-EVENT now allows layers and windows to be the objects of events
               (in addition to just cards and Actors).
06-03-91 ruben new flags
05-29-91 ruben added sk8-window-zoom via last-zoom-place
05-23-91 ruben fixed memory leak in le SK8-draw-current-selection & now uses stack;
         SK8-DRAW-DATA now uses stack; SK8-POINT-IN-WHICH-OBJECT now uses stack;
         SK8-DRAW-DATA will use card picture cache, if any has been set.
03-04-91 ICE Finished up the first pass of the conversion.
02-25-91 ICE Began converting the file to compile under MCL 2.0a5p2.

|#

;;; CLOS Object glue
;;; This file consists of CLOS object class definitions, followed by method definitions, for all of the CLOS 
;;; used by SK8.  Each object is followed by its own definitions.
;;;
;;; Certified SK8 1.0 function are prefaced by "1.0" in their documentation. The idea is that anything
;;; not thus labeled can go away from the system.
;;;

;;; _______________________________ 
;;; Classes
;;; _______________________________ 

;;; 1.0
;;; *SK8-WINDOW*
;;; This is the main CLOS object for SK8.  Every graphic window in SK8 is an instance of this window.
;;; It has the following slots:
;;; my-actor-object => the frame for the top level actor.
;;; want-idle     => A list of actors that want idle events
;;; dither        => Whether or not this window should be dithered onto the screen.
;;; logicalSpace  => The rectangle that encompasses every object in the window
;;; popUpMenu    => The current popup menu for the window
;;; my-frame      => The doobie for this window's frame
;;; dlist         => This window's display list
;;; first-click   => Whether or not this window gets the click that activates it
;;; locked        => Whether or not toplevel actor (window) is locked
;;; gWorld        => This window's offscreen gWorld, if any
;;; depth         => The depth of this windows offScreen gWorld, or nil if it doesn't have one.
;;; dirty-window-p => is the whole window dirty?
;;; dirty-region => the current dirty region
;;; last-zoom-place => zoom box last position


(defclass *SK8-WINDOW* (window)
  ((my-actor-object     :initform nil :initarg :my-actor-object)
   (owners              :initform (make-dlist))   ; vector of actors that own regions in the window.
   (want-idle           :initform (make-dlist))   ; vector of actors that want idle events.
   (first-click         :initform nil)
   (locked              :initform nil)
   (gWorld              :initform nil :initarg :gWorld)
   (depth               :initform nil :initarg :depth)
   (dither              :initform nil :initarg :dither)
   (movies              :initform nil)             ; list of movie actors playing in the window
   (dirty-window-p      :initform t)               ; True if the whole window needs redrawing   
   (dirty-nodes         :initform (make-dlist))    ; list of actors whose region must be redrawn
   (dirty-region        :initform nil)             ; accummulated dirty region for redraw
   (last-zoom-place     :initform nil)             ; cons last-zoom-position last-zoom-size (zoom box)
   (style               :initform nil)
   (drawOnlyDirty       :initform t)               ; whether to use the draw optimization that only dirty things be drawn.
   (drawEnabled         :initform t 
                        :initArg :drawEnabled))    ; tells the system that the window can be drawn.
  (:default-initargs 
    :color-p t
    :grow-icon-p nil))


(defclass *sk8-windoid* (*sk8-window* windoid)
  ())

(defclass *sk8-menu-item* (menu-item)
  ())

(defclass *sk8-menu* (menu)
  ())

#| move this into window-show instead of the creation of the instance
(defmethod initialize-instance ((w *SK8-WINDOW*) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (unless (ccl::window-process w)
    (setf (ccl::window-process w) (sk8dev::make-sk8-window-process (ccl::window-title w)))))
|#

;;; _______________________________ 
;;; Helpers
;;; _______________________________ 

(defmacro sk8-window-p (theWindow)
  `(typep ,theWindow '*sk8-window*))

(defmacro sk8-windoid-p (theWindow)
  `(typep ,theWindow '*sk8-windoid*))

(defmacro sk8-non-floater-p (theWindow)
  `(and (typep ,theWindow '*sk8-window*)
        (not (typep ,theWindow '*sk8-windoid*))))

(defun computed-active-window (&optional avoiding)
  (let ((candidate (front-window :class '*sk8-window* :include-windoids nil)))
    (if (and avoiding (eq candidate avoiding))
      (car (remove avoiding (windows :class '*sk8-window* :include-windoids nil)))
      candidate)))

(defun computed-active-windoid (&optional avoiding)
  (let ((candidate (front-window :class '*sk8-windoid* :include-windoids nil)))
    (if (and avoiding (eq candidate avoiding))
      (car (remove avoiding (windows :class '*sk8-windoid* :include-windoids nil)))
      candidate)))

(defun find-old-window (newWindow)
  (if (and (sk8-non-floater-p *currentTla*) (neq *currentTla* newWindow))
    *currentTla*
    (computed-active-window newWindow)))

(defun find-old-windoid (newWindow)
  (if (and (sk8-windoid-p *currentTla*) (neq *currentTla* newWindow))
    *currentTla*
    (computed-active-windoid newWindow)))

;;; ______________________
;;; SK8 WINDOIDS!!! 
;;; ______________________

(defmethod ccl::accept-key-events ((w *sk8-windoid*))
  t)

(defvar *bringing-up-window*)

(defmethod window-show ((clos-window *sk8-windoid*))
  (call-next-method)
  (let ((*bringing-up-window* t))
    (declare (special *bringing-up-window*))
    (window-select clos-window)))

;;; ___________________
;;; MEMORY AND GWORLDS.
;;; ___________________

;;; 1.0
;;; DISPOSE-GRAPHICS-MEMORY -- gets rid of memory blocks which aren't necessary 
;;;                          while window is hidden.

(defun dispose-graphics-memory (clos-window)
  (with-slots (gWorld dirty-region depth) clos-window
    (without-interrupts
     (when (and depth (pointerp gWorld) (not (%null-ptr-p gWorld)))
       (#_DisposeGWorld gWorld)
       (setf gWorld nil))
     (when (and (handlep dirty-region) (not (%null-ptr-p dirty-region)))
       ;; (#_disposergn dirty-region)
       (setf dirty-region nil)))))

;;; 1.0
;;; RESTORE-GRAPHICS-MEMORY -- restores graphics mempory when an actor becomes
;;;                          visible again.

(defun restore-graphics-memory (clos-window)
  (without-interrupts
   (with-slots (gWorld dirty-region dirty-window-p depth) clos-window
     (let ((dirty? dirty-window-p))
       (unless (and (pointerp gWorld) (not (%null-ptr-p gWorld)))
         (when depth
           (setq dirty? t)
           (sk8-update-window-gWorld clos-window :flushbits t)))
       (unless (and (handlep dirty-region) (not (%null-ptr-p dirty-region)))
         (setq dirty? t)
         (setf dirty-region (T_NewRgnGC))
         (unless (and (handlep dirty-region) (not (%null-ptr-p dirty-region)))
           (error "No memory to create the dirty region!")))
       (setf dirty-window-p dirty?)))))

;;; 1.0
;;; SK8-UPDATE-WINDOW-GWORLD -- This function takes care of ensuring the fidelity of the offscreen 
;;;                            gWorld used for the SK8 window. If the window does not want a 
;;;                            gWorld, it will dispose of one it already has.  It will adjust any existing
;;;                            one for changes in window-size or gWorld depth.

(defun SK8-update-window-gWorld (clos-window &key (flushBits t) size)
  (let+ ((depth (slot-value clos-window 'depth))
         (gWorld (slot-value clos-window 'gWorld))
         (qd-rect (:rect)))
    (when size
      (rset qd-rect :rect.topleft 0)
      (rset qd-rect :rect.right (car size))
      (rset qd-rect :rect.bottom (cadr size)))
    (if (and gWorld depth (not flushBits))
      ;; There is a gWorld already: preserve what we can.
      (let* ((sourceRect (rref gWorld :cGrafPort.portRect))
             (newRect (if size qd-rect (window-rectangle clos-window)))
             (newGworld (T_NewGWorldGC newRect :depth depth :noGC t))
             (oldGWPixmap (#_getgworldPixmap gWorld))
             (newGWPixmap (#_getGWorldPixmap newGWorld))
             (maskRgn (rref (wptr clos-window) windowRecord.contRgn)))
        (with-port (get-wmgr-port)
          (with-locked-pixels-force (oldGWPixmap)
            (with-locked-pixels-force (newGWPixmap)
              ;; Copy the old stuff onto a new gworld.
              (copy-bits oldGWPixmap newGWPixmap sourceRect sourceRect 8 maskRgn))))
        ;; Install new gWorld in the slot.
        (setf (slot-value clos-window 'gWorld) newGWorld)
        (#_disposeGWorld gWorld))
      (progn
        (when gWorld 
          (#_disposeGWorld gWorld)
          (setf (slot-value clos-window 'gWorld) nil))
        (when depth 
          (setf (slot-value clos-window 'gWorld) 
                (T_NewGWorldGC (if size qd-rect (window-rectangle clos-window))
                               :depth depth :noGC t)))))))

;;; ________________
;;; LOCKING MECHANISM
;;; ________________

;;; 1.0
;;; lockWindow -- Locks the window (increments the lock counter of the window if already locked
;;;            or sets it to 1 if the window was unlocked).

(defun lockWindow (clos-window)
  (if (slot-value clos-window 'locked)
    (incf (slot-value clos-window 'locked))
    (setf (slot-value clos-window 'locked) 1)))

;;; 1.0
;;; UNLOCK-WINDOW -- unlocks clos-window by decreasing its lock counter. If the
;;;                 counter reaches 0, the window is unlocked, and we call
;;;                 sk8-draw-window to draw it.

(defun unlockWindow (clos-window &key visual-effect effect-speed force)
  (let ((locked-val (slot-value clos-window 'locked)))
    (when locked-val
      (unless (setf (slot-value clos-window 'locked)
                    (setq locked-val (if (or (eql locked-val 1) force) NIL (1- locked-val))))
        (sk8-draw-window clos-window visual-effect effect-speed nil))
      locked-val)))

;;; 1.0
;;; WITH-VISUAL-EFFECT -- executes body with the visual effect effect in the actor's window. This means
;;;                    that after bopdy is executed, copying from the Gworld to the window is done
;;;                    using the effect. This will do the effect on the whole window!!!

(defmacro with-visual-effect ((actor effect speed) &body body)
  (let ((clos-window (gensym)))
    `(let ((,clos-window (node-window ,actor)))
       (if ,clos-window
         (prog2 (lockWindow ,clos-window)
                (progn ,@body)
                (unlockWindow ,clos-window :visual-effect ,effect :effect-speed ,speed))
         ,@body))))

;;; 1.0
;;; LOCKED-WINDOW -- returns t when clos-window is locked.

(defun lockedWindow (clos-window)
  (slot-value clos-window 'locked))

;;; ______________
;;; DRAWING HELPERS
;;; ______________

;;; 1.0
;;; ADD-DIRTY-NODE-REGIONS -- Puts the regions of the dirty actors together into the dirty region
;;;                        of their window. This is where the final region of the dirty
;;;                        actors is taken into account.

(defmacro add-dirty-node-regions (clos-window dirty-nodes window-dirty-region)
  (declare (ignore clos-window))
  (let* ((container (gensym))
         (flags (gensym))
         (tempRegion (gensym))
         (tempRegion2 (gensym)))
    `(let+ ((,tempRegion2 (:region))
            ,container ,flags ,tempRegion)
       (dovector (dirty-actor ,dirty-nodes)
         (setf ,flags (node-flags dirty-actor))
         ;; Getting the right dirty region.
         (cond ((boundsToDirtyRegion? ,flags)
                (recompute-bounds-region dirty-actor ,flags)
                (setf ,tempRegion (node-boundsRegion dirty-actor)))
               ((fillToDirtyRegion? ,flags)
                (recompute-fill-region dirty-actor ,flags)
                (setf ,tempRegion (node-fillRegion dirty-actor)))
               ((frameToDirtyRegion? ,flags) 
                (recompute-frame-region dirty-actor ,flags)
                (setf ,tempRegion (node-frameRegion dirty-actor))))
         ;; An actor can get dirty using an arbitrary region. Thus it is possible that none of
         ;; the three actor regions will have to be added to the window's dirty region before redraw.
         (when ,tempRegion
           ;; clipping to fill of container and adding.
           (setf ,container (node-container dirty-actor))
           (if ,container
             (progn 
               (recompute-fill-region ,container (node-flags ,container))
               (require-trap #_sectRgn ,tempRegion (node-fillRegion ,container) ,tempRegion2)
               (require-trap  #_UnionRgn ,tempRegion2 ,window-dirty-region ,window-dirty-region))
             (require-trap 
              #_UnionRgn ,tempRegion ,window-dirty-region ,window-dirty-region)))
         (setf ,tempRegion nil)))))
  
;;; 1.0
;;; UPDATE-CLIP-OF-DIRTY-OWNERS -- This function loops through the actors in the window that own regions.
;;;                             If an actor intersects the dirty region, its clip is set to the empty region
;;;                             and its bounds are added to the dirty region.

(defun update-clip-of-dirty-owners (clos-window dirty-region dirty-nodes)
  (let ((owners (slot-value clos-window 'owners))
        bounds)
    (unless (dlist-empty? owners)
      (rlet ((theRect :rect))
        (dovector (actor owners)
          (when (deeply-visible? actor)
            ;; This allows owners to work in the fast-draw phase of the
            ;; draw loop!
            (set-dirty-contents-flag actor)
            (recompute-bounds-region actor (node-flags actor))
            (setf bounds (node-boundsRegion actor))
            (region-into-rect bounds theRect)
            (when (or (position actor dirty-nodes) (#_rectInRgn theRect dirty-region))
              (#_unionRgn bounds dirty-region dirty-region)
              (#_setEmptyRgn (SK8::ownedRegion actor)))))))))
     
;;; 1.0
;;; CLEAR-WINDOW-DIRTY-INFO -- clears the dirty information for the window. This is
;;;                         done when we are done redrawing.

(defmacro clear-window-dirty-info (clos-window dirty-actors dirty-region dirty-window-p)
  (let* ((flags (gensym)))
    `(progn
       (let (,flags)
         ;; Cleaning the dirty list.
         (dovector-in-reverse (dirty-actor ,dirty-actors)
           (setf ,flags (node-flags dirty-actor))
           (nodeSeenDirtyByWindow! ,flags 0)
           (onlyLocationChanged! ,flags 1)
           ;; Clearing the flags for adding to the dirty region.
           (boundsToDirtyRegion! ,flags 0)
           (fillToDirtyRegion! ,flags 0)
           (frameToDirtyRegion! ,flags 0)
           ;; removing node from the dlist.
           (vector-pop ,dirty-actors))
         ;; Emptying the dirty region.
         (setf ,dirty-window-p nil)
         (require-trap #_SetEmptyRgn ,dirty-region)
         ;; Clearing the dirty contents info.
         (clear-dirty-contents-flag (slot-value ,clos-window 'my-actor-object))
         ;; Resetting *fast-draw*.
         (setf (slot-value ,clos-window 'drawOnlyDirty) t)))))

;;; _____________
;;; DRAW FUNCTIONS
;;; _____________

;;; 1.0
;;; SK8-DRAW-TOP-LEVEL-ACTOR -- draws the top level actor. Modified to mess with a copy of the
;;;                           dirty region.

;;; NOTE: why can't we just call the drawFunction of the tla? We can!!!

(defmacro sk8-draw-top-level-actor (actor port draw-region)
  `(progn (when (and *fast-draw* (nodeSeenDirtyByWindow? (node-flags ,actor)))
            (setf *fast-draw* nil))
          (funcall (node-drawFunction ,actor) ,actor ,port ,draw-region)))

;;; 1.0 
;;; SK8-DRAW-DATA -- this function calls the drawing function for the top level actor of the window.
;;;                 The debugging code that shows the dirty region sits here.

#|
(defmacro with-temporary-black-white-penColor (&body body)
  `(let+ ((preserved-forecolor :RGBColor)
          (preserved-backcolor :RGBColor))
     (unwind-protect
       (progn
         (require-trap #_GetForeColor preserved-forecolor)
         (require-trap #_GetBackColor preserved-backcolor)
         (require-trap #_ForeColor #$BlackColor)
         (require-trap #_BackColor #$WhiteColor)
         ,@body)
       (progn
         (require-trap #_ForeColor (%get-long preserved-forecolor))
         (require-trap #_BackColor (%get-long preserved-backcolor))))))
|#

(defun sk8-draw-data (clos-window port draw-region)
  (declare (special *fast-draw*))
  (let+ ((oldPort (:pointer))
         (oldGdev (:pointer))
         (save-clipRgn (:region))
         (saved-penState (:penState))
         (actor (slot-value clos-window 'my-actor-object))
         (preserved-forecolor (:RGBColor))
         (preserved-backcolor (:RGBColor)))
    ;; Saving and setting the clips and gworlds.
    (#_GetGWorld oldPort oldGdev)
    (#_SetGWorld port *null-ptr*)
    (#_GetPenState saved-penState)
    (#_GetClip save-clipRgn)
    (#_SetClip draw-region)
    (#_GetForeColor preserved-forecolor)
    (#_GetBackColor preserved-backcolor)
    (#_ForeColor #$BlackColor)
    (#_BackColor #$WhiteColor)
    (unwind-protect
      (progn
        ;; Show dirty region for debugging.
        (when *graphics-debug*
          (#_SetGWorld (wptr clos-window) *null-ptr*)
          (dotimes (i 6)
            (#_InvertRgn draw-region)
            (dotimes (j 10000))
            (#_InvertRgn draw-region)
            (dotimes (j 10000)))
          (#_SetGWorld port *null-ptr*))
        ;; Walk actors and draw those that intersect the draw-region:
        ;; (funcall (mf::node-drawFunction actor) actor port draw-region)
        (with-locked-pixels-force ((#_GetGworldPixmap port))
          (sk8-draw-top-level-actor actor port draw-region)))
      (progn
        (#_ForeColor (%get-long preserved-forecolor))
        (#_BackColor (%get-long preserved-backcolor))
        (#_SetClip save-clipRgn)
        (#_SetPenState saved-penState)
        (#_SetGWorld (%get-ptr oldPort) (%get-ptr oldGdev))))))
       
;;; 1.0
;;; SK8-FULL-DRAW -- this function draws the most complex actor (has subactors and text). This is called
;;;                as SK8-draw-data was called in the old system (drawing the data in a window is just 
;;;                drawing its top level actor.
;;;
;;; Before we draw we need to check whether the regions are dirty and if so, recompute each one.
;;; This involves three checks. We could have a nodeDirty flag to turn this into just one check.
;;;
;;; Note that now the contents draw function is responsible for drawing the fill region of the actor.
;;; The idea is that after drawing the contents the remaining dirty region might not intersect the
;;; fill and thus we do not need to render it. This yields big savings when an actor's fillcolor is a
;;; complex color.

(defun sk8-full-draw (actor port draw-region)
  (let+ ((flags (node-flags actor))
         (clip-region (:region))
         (more-clip (:region))
         frame fill)
    ;; Recomputing the regions on demand if necessary.
    (recompute-frame-region actor flags)
    (recompute-fill-region actor flags)
    (setf frame (node-frameRegion actor)
          fill (node-fillRegion actor))
    ;; Rendering the frame and the fill.
    (when (or (frameToDirtyRegion? flags)
              (regions-Intersect frame draw-region))
      (sk8::render (node-frameColor actor) actor frame port))
    ;; Clipping to the fill area if necessary and drawing subActors.
    (#_sectRgn fill draw-region clip-region)
    (#_copyRgn clip-region more-clip)
    (with-clipped-region clip-region
      (funcall (node-contentsDrawFunction actor) actor port clip-region fill))
    ;; Why clip again to the same thing? Well... it turns out that the contentsDrawFunction
    ;; destructively modifies the clip given to it without calling with-clipped-region. 
    (with-clipped-region more-clip
      (sk8::render-text actor port))
    ;; Inverting if hilited.
    (when (and (hilited? flags) (inverted? flags))
      (recompute-bounds-region actor flags)
      (#_invertRgn (node-boundsRegion actor)))))

;;; 1.0
;;; SK8-subActors-DRAW -- this function is used to draw actors that have subactors but
;;;                    no text.
;;;
;;; Note that now the contents draw function is responsible for drawing the fill region of the actor.
;;; The idea is that after drawing the contents the remaining dirty region might not intersect the
;;; fill and thus we do not need to render it. This yields big savings when an actor's fillcolor is a
;;; complex color.

(defun sk8-subActors-draw (actor port draw-region)
  (let+ ((flags (node-flags actor))
         frame fill
         (clip-region (:region :init-empty)))
    ;; Recomputing the regions on demand if necessary.
    (recompute-frame-region actor flags)
    (recompute-fill-region actor flags)
    (setf frame (node-frameRegion actor)
          fill (node-fillRegion actor))
    ;; Rendering the frame.
    (when (or (frameToDirtyRegion? flags)
              (regions-Intersect frame draw-region))
      (sk8::render (node-frameColor actor) actor frame port))
    ;; Clipping to the fill area if necessary and drawing subactors.
    (#_sectRgn fill draw-region clip-region)
    (funcall (node-contentsDrawFunction actor) actor port clip-region fill)
    ;; Inverting if hilited.
    (when (and (hilited? flags) (inverted? flags))
      (recompute-bounds-region actor flags)
      (#_invertRgn (node-boundsRegion actor)))))

;;; 1.0
;;; SK8-TEXT-DRAW -- This function is used to draw actors that have text but no subactors.
;;;
;;; Note: if the actor's fillcolor is none, it should be treated as translucent. Seems to be the
;;;      only way of dealing with this problem.

(defun sk8-text-draw (actor port draw-region)
  (let+ ((flags (node-flags actor))
         (clip-region (:region))
         fill frame)
    ;; Recomputing the regions on demand if necessary.
    (recompute-frame-region actor flags)
    (recompute-fill-region actor flags)
    (setf fill (node-fillRegion actor)
          frame (node-frameRegion actor))
    ;; Rendering the frame and the fill.
    (when (or (frameToDirtyRegion? flags)
              (regions-Intersect frame draw-region))
      (sk8::render (node-frameColor actor) actor (node-frameRegion actor) port))
    (sk8::render (node-fillColor actor) actor fill port)
    ;; Rendering the text.
    (#_sectRgn draw-region fill clip-region)
    (with-clipped-region clip-region
      (sk8::render-text actor port))
    ;; Inverting if hilited.
    (when (and (hilited? flags) (inverted? flags))
      (recompute-bounds-region actor flags)
      (#_invertRgn (node-boundsRegion actor)))))

;;; 1.0
;;; SK8-SIMPLE-DRAW -- draws the simplest type of actor: has neither subactors nor
;;;                  text.

(defun sk8-simple-draw (actor port draw-region)
  (let ((flags (node-flags actor)))
    ;; Recomputing the regions on demand if necessary.
    (recompute-frame-region actor flags)
    (recompute-fill-region actor flags)
    ;; Rendering the frame and the fill.
    (when (or (frameToDirtyRegion? flags)
              (regions-Intersect (node-frameRegion actor) draw-region))
      (sk8::render (node-frameColor actor) actor (node-frameRegion actor) port))
    (sk8::render (node-fillColor actor) actor (node-fillRegion actor) port)
    (when (and (hilited? flags) (inverted? flags))
      (recompute-bounds-region actor flags)
      (#_invertRgn (node-boundsRegion actor)))))

;;; 1.0 
;;; SK8-DRAW-CONTENTS -- draws the contents of an actor.  Does not deal with translucent
;;;                     actors.
;;;
;;; Note: we tried to intersect the rects before doing the region calculation. The system slowed
;;; down 300%. We tried using traps and using the arithmetic functions in utilities. This optimization
;;; was not adopted.

(defun sk8-draw-contents (actor port clip-region fill-region &optional no-fill?)
  (declare (special *fast-draw*))
  (let ((contents (node-contents actor))
        flags
        bounds)
    ;; for each sub actor from front to back ...
    (dovector-in-reverse (subActor contents)
      (when (visible? (node-flags subActor))
        (setf flags (node-flags subActor))
        (recompute-bounds-region subActor flags)
        (setf bounds (node-boundsRegion subActor))
        ;; Switch off fast draw mechanism.
        (when (and *fast-draw* (nodeSeenDirtyByWindow? flags))
          (setf *fast-draw* nil))
        ;; if subActor intersects the dirty region
        (when (regions-intersect clip-region bounds)
          (if *fast-draw* 
            ;; Fast draw: only draw if node is dirty.
            (when  (contentsDirty? flags)
              (with-clipped-region clip-region
                (funcall (node-drawFunction subActor) subActor port clip-region)))
            ;; Not in fast draw: draw everything.
            (with-clipped-region clip-region
              (funcall (node-drawFunction subActor) subActor port clip-region)))
          (#_DiffRgn clip-region bounds clip-region))
        ;; if the dirty region is empty, we are done!
        (when (#_emptyRgn clip-region)
          (return-from sk8-draw-contents nil))))
    ;; Rendering the fill of actor if necessary.
    (unless no-fill?
      (let+ ((effective-fill (:region)))
        (#_sectRgn fill-region clip-region effective-fill)
        (unless (#_emptyRgn effective-fill)
          (with-clipped-region effective-fill
            (sk8::render (node-fillcolor actor) actor fill-region port)))))))

;;; 1.0
;;; SK8-DRAW-CONTENTS-WITH-TRANSLUCENCY -- draws the contents of actor, taking into
;;;                                      account translucent sub actors.
;;;
;;; We draw opaque actors in front to back order. As we go along, when we
;;; find a translucent actor, we put it in the translucent list. The problem is that when we draw each
;;; translucent actor from back to front, we need to remember its clip region (which does not include 
;;; the space covered by all opaque actors in front of it. So, in the front pass, each time we encounter
;;; a translucent actor, we copy the current clip region and store it in a dlist with the actor. Then, the
;;; back to front pass is very easy, we just traverse this dlist in reverse, setting the clip and drawing
;;; the actors.
;;;
;;; This function will certainly be MUCH SLOWER than the simple draw contents function above.

(defun sk8-draw-contents-with-translucency (actor port clip-region fill-region &optional no-fill?)
  (declare (special *fast-draw*))
  (let* ((contents (node-contents actor))
         (translucent-queue (make-array (the fixnum (* (node-translucentContents actor) 2))))
         (counter 0)
         bounds subActor subFlags new-region nodeDirty)
    (declare (fixnum counter) (dynamic-extent translucent-queue))
    ;; FRONT TO BACK PASS: for each sub actor ...
    (dovector-in-reverse (subActor contents)
      (setf subFlags (node-flags subActor))
      (when (visible? subFlags)
        (recompute-bounds-region subActor subFlags)
        (setf bounds (node-BoundsRegion subActor))
        (setf nodeDirty (nodeSeenDirtyByWindow? subflags))
        (when (and *fast-draw* nodeDirty)
          (setf *fast-draw* nil))
        ;; if subActor intersects the dirty region
        (when (regions-intersect clip-region bounds)
          (if (opaque? subFlags)
            ;; draw the actor and subtract its bound region from the dirty region.
            (progn (if *fast-draw* 
                     ;; Fast draw: only draw if node is dirty.
                     (when (or nodeDirty (contentsDirty? subflags))
                       (with-clipped-region clip-region
                         (funcall (node-drawFunction subActor) subActor port clip-region)))
                     ;; Not in fast draw: draw everything.
                     (with-clipped-region clip-region
                       (funcall (node-drawFunction subActor) subActor port clip-region)))
                   (#_diffRgn clip-region bounds clip-region)
                   (#_setClip clip-region))
            ;; add the actor to the translucent-draw-queue.
            (progn (when *fast-draw* (setf *fast-draw* nil))
                   (setf new-region (get-region-from-pool))
                   (#_copyRgn clip-region new-region)
                   (setf (aref translucent-queue counter) subActor)
                   (setf (aref translucent-queue (1+ counter)) new-region)
                   (incf counter 2))))
        ;; if the dirty region is empty, we are done!
        (when (#_emptyRgn clip-region)
          (return))))
    ;; Render the fill region!
    (unless no-fill?
      (let+ ((effective-fill (:region)))
        (#_sectRgn fill-region clip-region effective-fill)
        (unless (#_emptyRgn effective-fill)
          (with-clipped-region effective-fill
            (sk8::render (node-fillcolor actor) actor fill-region port)))))
    ;; BACK TO FRONT PASS: we draw the translucent objects.
    (#_setClip fill-region)
    (do ((newCounter counter (- newCounter 2)))
        ((eql newCounter 0))
      (setf subActor (aref translucent-queue (- newCounter 2))
            new-region (aref translucent-queue (1- newCounter)))
      ;; clip the objects above the translucent object and draw.
      (with-clipped-region new-region
        (funcall (node-drawFunction subActor) subActor port new-region))
      ;; Return the region to the pool.
      (return-region-to-pool new-region)
      )))

;;; 1.0
;;; SK8-DRAW-WINDOW -- The draw function checks to window caches to see exactly how much 
;;;                   redrawing needs to be done. It will ensure the fidelity of the offscreen 
;;;                   gWorld, if any, then make it the current port and call SK8-DRAW-DATA to 
;;;                   actually draw the contents of the window.

(defun SK8-draw-window (clos-window &optional visual-effect effect-speed avoid-blitting)
  (without-interrupts     ;;***  Cuz MCL likes to pull the current port away from you at bad times.
   (with-drawing-lock
     (when (and (window-unlocked-p clos-window) (slot-value clos-window 'drawEnabled))
       (let ((wptr (wptr clos-window))
             (dirty-region-done-p nil)
             (*fast-draw* (slot-value clos-window 'drawOnlyDirty)))
         (declare (special *fast-draw*))
         (with-slots (gWorld dirty-window-p dirty-region dirty-nodes dither) clos-window
           ;; When the gWorld is not valid, make it valid.
           (when (and gWorld (not (lock-gWorld gWorld)))
             (SK8-update-window-gWorld clos-window)
             (unless (lock-gWorld gWorld)
               (setf gWorld nil)))
           ;; If the WHOLE window is dirty make the dirty region be the whole window.
           (if gworld
             (when dirty-window-p
               (#_RectRgn dirty-region (rref wptr :cGrafPort.portRect))
               (setq dirty-region-done-p t))
             (when dirty-window-p
               (#_CopyRgn (rref wptr :cGrafPort.visRgn) dirty-region)
               (setq dirty-region-done-p t)
               (setf *fast-draw* nil)))
           ;; Does the data need redrawing?
           (when (or dirty-window-p (not (dlist-empty? dirty-nodes)) (not (#_EmptyRgn dirty-region)))
             ;; Yes: proceed with intelligent redraw
             (unless dirty-region-done-p
               (add-dirty-node-regions clos-window dirty-nodes dirty-region))
             (unless (or gWorld dirty-region-done-p)
               (#_SectRgn dirty-region (rref wptr :cGrafPort.visRgn) dirty-region))             
             ;; Draw the data to the appropriate port.
             (update-clip-of-dirty-owners clos-window dirty-region dirty-nodes)
             (SK8-draw-data clos-window (or gWorld wptr) dirty-region)
             (when (and gworld (not avoid-blitting))
               (#_SectRgn dirty-region (rref wptr :cGrafPort.visRgn) dirty-region)
               (gWorld-to-window gWorld clos-window dither dirty-region visual-effect effect-speed)
               ))
           ;; Clean up window dirty info.
           (clear-window-dirty-info clos-window dirty-nodes dirty-region dirty-window-p)))))))

;;; _________
;;; HIT TESTING
;;; _________

;;; 1.0
;;; SK8-POINT-IN-WHICH-WINDOW -- Given a point, returns the CLOS object for the window 
;;;                           which contains it.  

(defun SK8-point-in-which-window (where &optional avoiding)
  (dolist (clos-window (windows :include-windoids t :class '*sk8-window*) *stage*)
    (when (and (neq clos-window avoiding)
               (slot-value clos-window 'my-actor-object) ;; this excepts background window
               (#_PtInRgn where (rref (wptr clos-window) :window.strucRgn)))
      (return clos-window))))

;;; 1.0
;;; SK8-POINT-IN-WHICH-OBJECT -- This is the hit testing function of the system. We just call
;;;                           sk8-point-in-which-node with the node for the top level actor
;;;                           of the window. It returns an actor.  If no actor in the window
;;;                           is hit, the stage is returned.

(defun sk8-point-in-which-object (clos-window qd-where)
  (let ((actor (slot-value clos-window 'my-actor-object)))
    (when (#_PtInRgn qd-where (rref (wptr clos-window) :window.strucRgn))
      (setq qd-where (subtract-points qd-where (view-position clos-window)))
      (or (when actor
            (or (SK8-multival-bind (x y) (point-to-SK8Coords qd-where)
                  (catch :object
                    (sk8-point-in-which-node actor qd-where x y)))
                ;; If nothing was hit, it may be the case that the mouse is over
                ;; the titlebar of a mac window. Return the actor if its mouseSensitivity
                ;; does not preclude this.
                (when (< (node-mouseStatus actor) 2) actor)))
          ;; Well... Although the actor was touched by the mouse, it is not hit.
          sk8::Stage))))

;;; 1.0
;;; SK8-POINT-IN-WHICH-NODE -- This function finds the most specific actor hit by the event. The variable
;;;                         containerHit holds the closest ancestor to this object which is not 
;;;                         mouseTransparent (and was thus hit).
;;;
;;; Note: wouldn't it be great to use tagbody and eliminate recursion???
;;; Note2: We only want to see if subactors have been hit when the event does not happen in the
;;;       frame of actor. We need an extra region check for this, with the potential of a
;;;       substantial slow down of this function.

;;; A custom object is always mouse opaque. When we encounter one of these guys, we call its
;;; beingHit method which returns t if THIS OBJECT WANTS THE EVENT! The args to this function
;;; are: node, x and y.


(defun sk8-point-in-which-node (actor where x y)
  (let ((flags (node-flags actor))
        (mouseTransparent? (mouseTransparent? actor)))
    ;; When this actor is visible, not mouseInvisible,  and hit...
    (when (and (visible? flags)
               (not (mouseInvisible? actor))
               (coords-in-rect-p x y (recompute-physicalBoundsRect actor)))
      (recompute-bounds-region actor flags)
      (when (#_PtInRgn where (node-boundsRegion actor))
        ;; At this point actor has been physically hit. If it has custom mouse sensitivity,
        ;; we call its method to find out who has been hit. The custom method handles
        ;; examining the contents and thus we jump this step in this case.
        (if (mouseCustom? actor) 
          (SK8-multival-bind (h v) (window-to-stage-coords actor x y)
            (setf actor (sk8::hitByMouse actor h v)))
          ;; Does it pass mouse events and does it have contents to pass them to?
          ;; and it does not have custom sensitivity.
          (unless (or (mouseOpaque? actor) (hasNoContents? actor))
            ;; YES: check each actor in contents.
            (let ((contents (node-contents actor)))
              (dovector-in-reverse (subActor contents)
                (sk8-point-in-which-node subActor where x y)))))
        ;; if we got here, no subactor was hit. Then this actor is the most
        ;; specific actor hit and we are done. We return it, unless it is mouseTransparent.
        (when (and actor (not mouseTransparent?))
          (throw :object actor))))))

;;; 1.0
;;; LEAVE-0BJECT-UNDER-MOUSE-IF-IN -- leaves object if object is under the mouse. This is necessary
;;;                                we might delete the object under the mouse. Then, we would
;;;                               be dispatching events to an object which no longer exists, with
;;;                               dire consequences.
;;;
;;; Note: this is a simplified version of the original. Check SK8d29 if problems.

(defun leave-object-under-mouse-if-in (theObj)
  (let ((under-mouse (object-under-mouse)))
    ;; When the object under the mouse is the given object or is contained in it...
    (when (and under-mouse (eq under-mouse theObj))
      (sk8::mouseLeave theObj)
      (when (eq under-mouse *eventActor*)
        (setf *eventActor* nil))
      (set-object-under-mouse nil))))

;;; _____________
;;; EVENT HANDLING
;;; _____________

;;; This function sends the mouseWithin message to ALL actors in the path
;;; from the current event actor to the stage.

(defun dispatch-mouseWithins (curActor &optional (withinFunction 'sk8::mouseWithin specified-p))
  (unless (eq curActor sk8::Stage)
    (loop 
      (unless curActor (return))
      (when (or specified-p (wantsMouseWithin? (node-flags curActor)))
        (funcall withinFunction curActor))
      (setf curActor (node-container curActor)))))

;;; This function dispatches mouseEnter and mouseLeave events. MouseLeave is sent to all actors
;;; from oldActor to the commonContainer (not including the commonContainer).
;;; MouseEnter is sent to all actors on the path from the commonContainer (not including it) to 
;;; newActor. 

;;; An important side effect of all this is that for every actor except newActor, mouseEnter
;;; will be called and the eventActor will not be itself!!! Also that mouseEnter cannot be propagated up the hierarchy.

;;; MouseLeave is done from the bottom up and mouseEnter from the top down.

;;; This is slightly 15% than the pure recursive version.

(defun depth (anActor)
  (if (eq anActor sk8::stage)
    1
    (if (not (node-window anActor))
      0
      (let ((result 1))
        (loop 
          (incf result)
          (setf anActor (node-container anActor))
          (unless anActor (return result)))))))

(defun findCommonContainer (deepActor shallowActor depthDelta)
  ;; [1] bring them to same depth.
  (dotimes (i depthDelta)
    (unless deepActor (return-from findCommonContainer nil))  ;;;This can be nil when there has been a race condition removing it from the stage.
    (setf deepActor (sk8::container deepActor)))
  ;; [2] search and return when equal.
  (loop
    (when (eq deepActor sk8::Stage) (return sk8::Stage))
    (when (eq deepActor shallowActor) (return deepActor))
    (when (or (eq deepActor nil) (eq shallowActor nil)) (return nil))   ;;;This can be nil when there has been a race condition removing it from the stage.
    (setf deepActor (sk8::container deepActor)
          shallowActor (sk8::container shallowActor))))

;;; Pass 1 builds a list from commonContainer to newActor and the calls mouseEnter on each item in it.

(defun reversed-dispatch-mouseEnter (newActor commonContainer depth enterFunction)
  (when (> depth 0)
    (let ((containers (make-list depth))
          (count (1- depth)))
      (declare (dynamic-extent containers count))
      (loop
        (when (eq newActor commonContainer) (return))
        (setf (nth count containers) newActor)
        (decf count)
        (setf newActor (sk8::container newActor)))
      ;; Now call mouseEnter.
      (dolist (aContainer containers nil)
        (when aContainer (funcall enterFunction aContainer))
        ))))
    
;;; The optionals are provided to allow other parts of the system to use this facility.

(defun dispatch-mouseEnter&Leave (oldActor newActor 
                                                &optional
                                                (enterFunction 'sk8::mouseEnter)
                                                (leaveFunction 'sk8::mouseLeave)
                                                (stateVariable '*eventActor*))
  (let ((oldDepth (depth oldActor))
        (newDepth (depth newActor))
        commonContainer)
    ;; If the oldActor is no longer on the stage, do not send mouseLeaves to it.
    (if (zerop oldDepth)
      (progn
        (setf (symbol-value stateVariable) newActor)
        (reversed-dispatch-mouseEnter newActor nil newDepth enterFunction))
      (progn
        ;; [1] Find common container.
        (if (> oldDepth newDepth)
          (setf commonContainer (findCommonContainer oldActor newActor (- oldDepth newDepth)))
          (setf commonContainer (findCommonContainer newActor oldActor (- newDepth oldDepth))))
        ;; [2] Dispatch mouseLeave (from deepest up).
        (when commonContainer  ;;;This can be nil when there has been a race condition removing it from the stage.
          (loop 
            (when (or (eq oldActor commonContainer) (not oldActor)) (return))
            (funcall leaveFunction oldActor)
            (setf oldActor (sk8::container oldActor))))
        ;; [3] Set the event actor.
        (setf (symbol-value stateVariable) newActor)
        ;; [4] Dispatch mouseEnter (from shallowest up).
        (reversed-dispatch-mouseEnter newActor commonContainer newDepth enterFunction)))
    ))
  
;;; When the oldActor was nil, we throw up our arms. We just set the eventActor and
;;; dispatch mouseEnter to everything from the Stage up to the new actor.

(defun just-do-mouseEnters (newActor &optional 
                                         (enterFunction 'sk8::mouseEnter)
                                         (stateVariable '*eventActor*))
  (setf (symbol-value stateVariable) newActor)
  (when newActor
    (reversed-dispatch-mouseEnter newActor nil (depth newActor) enterFunction)))
  
;;; 1.0
;;; SK8-NULL-EVENT -- This function dispatches idle events to objects. It deals with
;;;                 mouseEnters, mouseLeaves and mouseWithins.

(defun sk8-null-event (window &key (indirect nil))
  (declare (ignore window) (special mf::*events-on*))
  (when mf::*events-on*
    (unless indirect
      (let ((curObject *eventActor*)
            (mouseObject (object-under-mouse)))
        (when mouseObject
          (if (eq curObject mouseObject)
            ;; If the mouse is still in the same object, and the object (or anyone up the hierarchy) wants mouseWithins
            (when (neq curobject sk8::stage)
              (dispatch-mouseWithins curObject))
            ;; If the mouse is on a new object, mouseLeave the old object, mouseEnter the
            ;; new one and reset the object-under-mouse.
            (if curObject
              (dispatch-mouseEnter&Leave curObject mouseObject)
              (just-do-mouseEnters mouseObject)))))
      ;; Dispatch idle events to all the actors that want some.
      (map-windows
       #'(lambda (clos-window)
           (when (slot-value clos-window 'drawenabled) ;; don't do windows we are in process of closing
             (dovector (actor (slot-value clos-window 'want-Idle))
               (sk8::idle actor))))
       :class '*sk8-window* :include-invisibles t :include-windoids t)
      (let ((mb (sk8::menubar sk8::stage)))
        (when (and mb (sk8::wantsidle mb))
          (sk8::idle mb)))
      )))
  
;;; 1.0
;;; SK8-ZOOM-WINDOW -- We use the slot last-zoom-place to record the old size and location of the window.
;;;                   When the value of this slot is nil, the window is zoomed out. When it is not null, it
;;;                   contains thw size of the window. We will have window-zoom-event-handler do all the work.
;;;                   We need to change the boundsRect of the toplevel actor, and dispose the window's gworld
;;;                   so that when zooming a new one is generated.

(defun sk8-window-zoom (window part where)
  (let* ((actor (slot-value window 'my-actor-object))
         (last-zoom-place (slot-value window 'last-zoom-place)))
    (when (#_TrackBox (wptr window) where part)
      (if last-zoom-place
        ;; ZOOM OUT! (shrinks window)
        (let ((width (point-h last-zoom-place))
              (height (point-v last-zoom-place)))
          ;; Changing the boundsRects of the top level actor. Also dirty the regions.
          (simple-set-physical-rect-size (node-physicalBoundsRect actor) width height)
          (simple-set-rect-size (node-logicalBoundsRect actor) width height)
          (window-zoom-event-handler window 7)
          (sk8-update-window-gworld window)
          (when (sk8::inheritsFrom actor *EditText-the-object*)
            (sk8::update-field-rect actor t nil))
          (sk8::resized actor)
          (setf (slot-value window 'dirty-window-p) t)
          (making-self-dirty (actor nil t t t (node-boundsRegion actor) :bounds nil))
          ;; Clearing the stored info.
          (setf (slot-value window 'last-zoom-place) nil))
        ;; ZOOM IN! (enlarges window)
        (let* ((zoomSize (window-zoom-size window))
               (width (point-h zoomSize))
               (height (point-v zoomSize)))
          ;; Saving the original size.
          (SK8-multival-bind (oldWidth oldHeight) (sk8::size actor :physical t)
            (setf (slot-value window 'last-zoom-place) (make-point oldWidth oldHeight)))
          ;; Changing the boundsRects of the top level actor. Also dirty the regions.
          (simple-set-physical-rect-size (node-physicalBoundsRect actor) width height)
          (simple-set-rect-size (node-logicalBoundsRect actor) width height)
          ;; Disable drawing since gWorld is out of shape.
          (withDrawdisabled window
            (sk8-update-window-gworld window :size (list width height))
            (window-zoom-event-handler window 8)
            (sk8::resized actor))
          (setf (slot-value window 'dirty-window-p) t)
          ;; Now do the draw!
          (making-self-dirty (actor nil t t t (node-boundsRegion actor) :bounds nil))
          )))))

;;; 1.0
;;; SK8-MOUSEDOWN-EVENT -- The mouseDown event handler dispatches mouseDown events.
;;;                       Here we handle the special mouseDown operations that have to do with
;;;                       Mac window functionality. If the event happens on the contents, we just
;;;                       dispatch the event to the object under the mouse.

(defun sk8-mouseDown-event (clos-window)
  (declare (special *current-event*))
  (let* ((curObject (object-under-mouse))
         (where (rref *current-event* :event.where))
         (part (%stack-block ((ww 4))
                 (#_FindWindow where ww))))
    (setq *last-mousedown-time* (rref *current-event* :event.when)
          *last-mousedown-location* *event-location*)
    (case part
      (3
       ;; contents
       (setq *last-mousedown-object* curObject)
       (sk8::mouseDown curObject))
      (4
       ;; Drag (Title Bar)
       (setq *last-mousedown-object* nil)
       (SK8-Drag-Window clos-window))
      (5 
       ;; Grow Box
       (setq *last-mousedown-object* nil)
       (SK8-Grow-Window clos-window))
      (6
       ;; Close Box
       (setq *last-mousedown-object* nil)
       (let ((actor (slot-value clos-window 'my-actor-object))
             (inside-close-box (#_TrackGoAway (wptr clos-window)
                                (rref *current-event* :event.where))))
         (when inside-close-box
           (if actor
             (sk8::close actor)
             (error "BUG: window without actor")))))
      ((7 8)
       ;; ZoomIn or ZoomOut
       (sk8-window-zoom clos-window part where)))))

;;; 1.0
;;; SK8-MOUSEUP-EVENT -- The mouseup event dispatches the mouseUp to the current object.
;;;                    Click and double click behaviour is moved to the actor's default mouseUp
;;;                    handler (in actor.lisp).

(defun sk8-mouseUp-event ()
  (when (object-under-mouse)
    (sk8::mouseUp (object-under-mouse))))

(defun getEventKey ()
  (declare (special *current-event*))
  (code-char (logand 255 
                     (rref *current-event* :event.message))))

;;; 1.0
;;; SK8-KEYDOWN-EVENT -- This method handles dispatching key events.  In SK8, if an object 
;;;                     wants key events it requests it.  Only one object per window gets 
;;;                     key events.  If no object is requesting key events, then no one gets it!

(defun SK8-keyDown-event (window)
  (declare (special *current-event*))
  (let* ((windowObject (slot-value window 'my-actor-object))
         (curObject (sk8::keytarget windowObject))
         (key (code-char (logand 255 
                                 (rref *current-event* :event.message)))))
    ;; do we have an object that is requesting key events?
    (when curObject
      (sk8::keyDown curObject key))))

;;; 1.0
;;; SK8-KEYUP-EVENT -- This method handles dispatching key events.  In SK8, if an object 
;;;                  wants key events it requests it.  Only one object per window gets 
;;;                  key events.  If no object is requesting key events, then no one gets it!

(defun sk8-keyUp-event (window)
  (declare (special *current-event*))
  (let* ((windowObject (slot-value window 'my-actor-object))
         (curObject (sk8::keytarget windowObject))
         (key (code-char (logand 255 
                                 (rref *current-event* :event.message)))))
    ;; do we have an object that is requesting key events?
    (when curObject
      (sk8::KeyUp curObject key))))

;;; 1.0
;;; SK8-AUTOKEY-EVENT -- This method handles dispatching key events.  In SK8, if an object 
;;;                    wants key events it requests it.  Only one object per window gets 
;;;                    key events.  If no object is requesting key events, then no one gets it!

(defun sk8-autoKey-event (window)
  (declare (special *current-event*))
  (let* ((windowObject (slot-value window 'my-actor-object))
         (curObject (sk8::keytarget windowObject))
         (key (code-char (logand #xFF (rref *current-event* :event.message)))))
    ;; Do we have an object that is requesting key events?
    (when curObject
      (sk8::AutoKey curObject key))))
  
;;; SK8-DRAW-WINDOW-FOR-UPDATE -- a very much simplified version of SK8-draw-window. It is used to refresh
;;;                             a window on an update event. It is only called for windows which do not have
;;;                             gWorlds.
;;;
;;; Note that since all drawing is clipped to the visRgn of the window, it is not necessary to recompute the clip
;;; regions of the owned region guys. All owned region guys should behave by only updating their regions when
;;; the regions are NOT empty.

(defun SK8-draw-window-for-update (clos-window)
  (when (slot-value clos-window 'drawEnabled)
    (without-interrupts     ;;***  Cuz MCL likes to pull the current port away from you at bad times.
     (let ((wptr (wptr clos-window)))
       (with-slots (dirty-window-p dirty-region dirty-nodes dither) clos-window
         ;; If the WHOLE window is dirty make the dirty region be the whole window.
         (#_CopyRgn (rref wptr :cGrafPort.visRgn) dirty-region)
         ;; (#_CopyRgn (rref wptr :windowrecord.updateRgn) dirty-region) OJO!!!
         (let ((*fast-draw* nil))
           (declare (special *fast-draw*))
           ;; Draw the data to the appropriate port.
           (SK8-draw-data clos-window wptr dirty-region))
         ;; Clean up window dirty info.
         (clear-window-dirty-info clos-window dirty-nodes dirty-region dirty-window-p))))))

;;; 1.0
;;; SK8-UPDATE-EVENT -- This method handles redrawing the window in response to an update 
;;;                    event from the window manager.
;;;
;;; Note that the code that handles movies calls the macro object-being-shown-p defined in the
;;; d29 version of this file. In the new system we should right a function that returns whether
;;; an object is visible in a window (meaning it is neither obstructed by an object in its front
;;; nor out of the window because of panning). In any case, the current version of this function
;;; does not check whether the movie is visible in the window.
;;;
;;; When window has a gWorld all we do is blast it to the screen REGARDLESS of wheather the
;;; window is locked or not. If there is no gWorld, the only thing to do is to redraw. The problem
;;; is that if the window was locked, all graphical changes will be shown.

(defun sk8-update-event (window)
  (let ((wptr (wptr window)))
    (#_BeginUpdate wptr)
    (with-slots (movies) window
      (when movies
        (let (handle)
          (mapc #'(lambda (movie)
                    ;; took this out: (object-being-shown-p movie).
                    ;; weren't you wandering why we have an and with just 1 conjunct?
                    (when (and (handlep (setq handle (sk8::handle movie))))
                      (#_UpdateMovie handle)))
                movies))))
    (when (slot-value window 'drawEnabled) 
      (let ((gWorld (slot-value window 'gWorld)))
        (if gWorld
          (gWorld-to-window gWorld window nil (rref wptr :cGrafPort.visRgn))
          (SK8-draw-window-for-update window))))
    (#_EndUpdate wptr)))

(defmethod window-update-event-handler ((w *sk8-window*))
  (let ((curmode (sk8:ActiveMode)))
    (or (when curmode
          (sk8::handleUpdate curmode))
        (sk8-update-event w))))

;;; 1.0
;;; SK8-SPURIOUS-EVENT

(defun sk8-spurious-event (clos-window)
  (declare (ignore clos-window))
  (format t "BUG: A spurious event happened!~%")
  (format t "     Please send a bug report.  The SK8 Team~%")
  (ed-beep))

;;; 1.0
;;; WINDOW-EVENT -- the main event loop!!!

(declaim (special *window-eval-queue* *end-window-eval-queue*))

;; event loop for sk8 windows. Bind the event globals so they become per-process. That's easier than changing all existing code that refers to the globals

(defun windowLoop (&aux ev)
  (unwind-protect
    (let ((ccl::*current-event*
           (setf ev (make-record :event)))
          *event-window*
          (*eventH* 0)
          (*eventV* 0)
          (*eventTime* 0)
          (*event-location* 0)
          (*event-x* 0)
          (*event-y* 0)
          (*object-under-mouse* (list nil nil)))
      (flet ((window-event-queued (me)
               (declare (special ccl::*processing-events*))
               (and (or (not ccl::*processing-events*)              ; and not the guy who set *processing-events*
                        (neq me ccl::*processing-events*))
                    (symbol-value-in-process '*window-eval-queue* me))))
        (declare (dynamic-extent window-event-queued))
        (let ((*window-eval-queue* nil)
              (*end-window-eval-queue* nil)
              (me *current-process*)) ;; use this if we use more than just *window-process*
          (loop
            (process-wait "Waiting for event" #'window-event-queued me)
            (let ((thing (pop *window-eval-queue*))
                  func where)
              ;; elements of queue are lists of a function to call, followed by values
              ;; for the various event values
              ;; (window what message where eventwhen modifiers where-relative-to-tla curObject)
              (setf func (pop thing)
                    *event-window* (pop thing)
                    (rref ev :event.what) (pop thing)
                    (rref ev :event.message) (pop thing)
                    where (pop thing)
                    (rref ev :event.where) where
                    *eventH* (point-h where)
                    *eventV* (point-v where)
                    *eventTime* (pop thing)
                    (rref ev :event.when) *eventTime*
                    (rref ev :event.modifiers) (pop thing))
              (let ((evloc (or (pop thing) *event-location*)))
                (setf *event-location* evloc
                      *event-x* (point-h evloc)
                      *event-y* (point-v evloc)))
              (let ((curobj (pop thing)))
                (when curobj
                  (setf (car *object-under-mouse*) curobj)))
              ;; now that the environment is all set up, execute the function
              (funcall func))))))
    (when ev (dispose-record ev :event))))

;; this is very low level, but we need a way of knowing when a process has initialized
(defun symbol-value-bound-in-process-p (sym process)
  #+ppc-target
  (let ((loc (ccl::%symbol-value-locative-in-stack-group sym (process-stack-group process))))
    (if (null loc)
      (boundp sym)
      (neq (ccl::%fixnum-ref loc) (ccl::%unbound-marker-8))))
  #-ppc-target
  (let ((loc (ccl::symbol-value-locative-in-stack-group sym (process-stack-group process))))
    (if (null loc)
      (boundp sym)
      (let ((res (ccl::lap-inline ()
                   (:variable loc)
                   (ccl::move.l (ccl::varg loc) ccl::atemp0)
                   (ccl::move.l ccl::@atemp0 ccl::acc))))
        (neq res (ccl::%unbound-marker-8))))))

(defun sk8dev::make-sk8-window-process (name)
  (let ((p (make-process name :background-p t
                         :stack-size ccl::*listener-process-stackseg-size*)))
    (process-preset p #'(lambda ()
                          (loop
                            (catch :exitmode
                              (catch :abort
                                (catch :cancel
                                  (windowloop)))))))
    (process-enable p)
    (process-wait "Waiting for window process to initialize"
                  #'(lambda () (symbol-value-bound-in-process-p '*window-eval-queue* p)))
    p))

;; if we go to using a separate process per window, create them when the window object is initialized
;; Even if we do, make one process for any windows that somehow don't have their own
(defparameter *window-process* (sk8dev::make-sk8-window-process "SK8 Window Events"))
(defparameter *SK8-System-process* (sk8dev::make-sk8-window-process "SK8 System Events"))

(defun sk8-modal-event-dispatch (window event curmode)    ;; Dispatch the event.
  ;; handle the user-specified mode
   ;; check that curmode is still current mode before each step in case another thread exits it
  (when (eq curmode (sk8:ActiveMode))
    (sk8::eventTick curmode))
  (when (eq curmode (sk8:ActiveMode))
    (or
     (case event
       (0 (sk8::handleIdle curmode))
       (1 (sk8::handleMouseDown curmode))
       (2 (sk8::handleMouseUp curmode))
       (3 (sk8::handleKeyDown curmode))
       (4 (sk8::handleKeyUp curmode))
       (5 (sk8::handleAutoKey curmode))
       (6 (sk8::handleUpdate curmode))
       (8 (sk8::handleActivate curmode))
       (T (sk8::handleOther curmode)))
     (if (typep window '*sk8-window*)
       (sk8-window-event-dispatch window event)
       (window-event window)))))

(defun sk8-window-event-dispatch (window event)    ;; Dispatch the event.
  (when (wptr window) ;; partial protection against window closing before this event is dispatched
    (case event
      (0  (sk8-null-event window))
      (1  (if (is-current-in-its-layer window)
            (sk8-mouseDown-event window)
            (sk8-select-old-window window)))
      (2  (sk8-mouseUp-event))
      (3  (sk8-keyDown-event window))
      (4  (sk8-keyUp-event window))
      (5  (sk8-autoKey-event window))
      (6  (sk8-update-event window))
      (8  (sk8-select-old-window window))
      (T  (sk8-spurious-event window)))))

(defun queue-up-sk8-window-event (winproc winfun win event message where eventwhen modifiers
                                               where-relative-to-tla curObject &optional clear-queue-p)
  (without-interrupts
   (without-event-processing
     (let ((queue (unless clear-queue-p
                    (symbol-value-in-process '*window-eval-queue* winproc)))
           (thing (list (list winfun win event message where eventwhen modifiers
                              where-relative-to-tla curObject))))
       (if queue
         (progn
           (nconc (symbol-value-in-process '*end-window-eval-queue* winproc)
                  thing)
           (setf (symbol-value-in-process '*end-window-eval-queue* winproc)
                 thing))
         (setf (symbol-value-in-process '*window-eval-queue* winproc)
               thing
               (symbol-value-in-process '*end-window-eval-queue* winproc)
               thing))))))

(defmethod sk8-window-process ((w window))
  (ccl::window-process w))

(defmethod sk8-window-process ((w *sk8-window*))
  (or (ccl::window-process w)
      (let ((proj (sk8:project (slot-value w 'my-actor-object))))
        (if (or (eq proj sk8:sk8) (eq proj sk8:ui))
          *sk8-system-process*
          *window-process*))))

;; make sure that any globals for the event are per-process (bind them in let) so that window events can be
;; in multiple threads
;; This function can be called by either a *sk8-window* window-event, or an eventmode handler on an eventhook
(defun do-sk8-mode-or-window-event (window curmodeprocess curmode)
  (declare (special ccl::*current-event* mf::*events-on*))
  (let* ((currentEvent ccl::*current-event*)
         (event (rref currentEvent :event.what)))
    (when (or (eql event 6) mf::*events-on*)
      (when (and *currentTla* (null (wptr *currentTla*))) (setf *currentTla* nil))
      (let* ((message (rref currentEvent :event.message))
             (where (rref currentEvent :event.where))
             (eventwhen (rref currentEvent :event.when))
             (modifiers (rref currentEvent :event.modifiers))
             ;; get the object or connector under the mouse, if any
             (sk8-window (sk8-point-in-which-window where))
             (curObject (if (eq sk8-window *stage*)
                          *stage*
                          (sk8-point-in-which-object sk8-window where)))
             (where-relative-to-tla (subtract-points where (if *currentTla*
                                                             (view-position *currentTla*)
                                                             (if window
                                                               (view-position window)
                                                               0))))
             ;;(curmodeprocess (sk8:ActiveModeProcess))
             ;;(curmode (when curmodeprocess (sk8:ActiveMode)))
             )
        ;; Dispatch the event.
        ;; deal with emergency event escape
        (if (and curmode (control-key-p) (option-key-p) (command-key-p)
                 (not (shift-key-p)))
          (progn
            (sk8:forceExitCurrentMode) ;; should this force the modal process to gracefully exit?
            nil)
          ;; normal sk8 window events get processed in a different thread so they can't bog everything down
          (let ((win (or window *currentTla*)))
            (if (and (neq event 0) (neq event 6)) ;; idle and window-update events must be processed immediately
              (flet ((winfun()
                       (sk8-window-event-dispatch win event))
                     (modefun()
                       (sk8-modal-event-dispatch win event curmode)))
                (declare (dynamic-extent winfun modefun))
                (let ((p (or (when curmode
                               (or curmodeprocess *sk8-system-process*))
                             (sk8-window-process win))))
                  (if (memq :halt (process-arrest-reasons p))  ;; don't even try if window process is in error or debug halt state
                    (when (eql event 1)  ;; just beep for mousedown
                      (ed-beep))
                    (progn
                      (queue-up-sk8-window-event p
                                                 (if curmode #'modefun #'winfun)
                                                 win event message where eventwhen modifiers
                                                 where-relative-to-tla curObject)
                      ;; give immediate priority to the process associated with this event
                      ;; if it is the selected window. This relies on non-selected windows setting their process to be background
                      (let-globally ((ccl::*in-scheduler* t))
                        (ccl::%activate-process p))
                      ))))
              (progn
                (setf *event-window* win
                      *eventH* (point-h where)
                      *eventV* (point-v where)
                      *eventTime* eventwhen)
                (let ((evloc (or where-relative-to-tla *event-location*)))
                  (setf *event-location* evloc
                        *event-x* (point-h evloc)
                        *event-y* (point-v evloc)))
                (when curObject
                  (setf (car *object-under-mouse*) curObject))
                (if curmode
                  (sk8-modal-event-dispatch win event curmode)
                  (sk8-window-event-dispatch win event)))
              )))))))

(defmethod window-event ((window *SK8-WINDOW*))
  (do-sk8-mode-or-window-event window nil nil))

;; for windoid
(defmethod window-event ((w *sk8-windoid*))
  (declare (special ccl::*current-event*))
  (let* ((currentEvent ccl::*current-event*)
         (event (rref currentEvent :event.what))
         (where (rref currentEvent :event.where))
         (eventwhen (rref currentEvent :event.when)))
    (if (or (eq event 1) (eq event 6))
      (call-next-method)
      (if (eq w *currentTla*)
        ;; Calls window-event of *sk8-window* to do the work.
        (let* ((curKey (sk8::keytarget (slot-value w 'my-actor-object)))
               (activeWindow (front-window :include-windoids nil :include-invisibles nil))
               (active-is-sk8-window (and activeWindow (sk8-window-p activeWindow))))
          (if (and (null curKey)
                   active-is-sk8-window
                   (or (eq event 3) (eq event 4) (eq event 5)))
            (progn
              (setf *event-window* activeWindow
                    *eventH* (point-h where)
                    *eventV* (point-v where)
                    *eventTime* eventwhen
                    *event-x* (point-h *event-location*)
                    *event-y* (point-v *event-location*))
              (sk8-window-event-dispatch activeWindow event nil))
            (call-next-method)))
        (if (and *currentTla* (wptr *currentTla*))
          ;; Pass the event to the real target.
          (window-event *currentTla*)
          ;; This should not happen. This warning is left to find out
          ;; whether it does happen.
          (format t "~%WARNING (window-event): event ~a slips by ~a.~%" event w))))))

;; stuff for eventmode eventhook

(defun checkforeventmodeevent ()
  (let ((curmode (sk8:ActiveMode)))  ;; only do any of this if there is a current mode
    (when curmode
      (let* ((event *current-event*)
             (ecode (rref event eventrecord.what))
             wob)
        (with-macptrs (w)
          (case ecode
            (#.$MButDwnEvt  ;; 1
             (let ((code (#_FindWindow (rref event eventrecord.where)
                          (%inc-ptr event $evtMessage))))
               (unless (or (eq code $inDesk) (eq code $inMenubar) (eq code $inSysWindow))
                 (%setf-macptr w (%get-ptr event $evtMessage)))))
            (#.$UpdatEvt  ;; 6
             (%setf-macptr w (%get-ptr event $evtMessage)))
            ((#.$nullevt #.$mbutupevt #.$keydwnevt  ;; 0, 2, 3, 4, 5, 7, 8
              #.$keyupevt #.$autokeyevt #.$keyevtdev
              #.$activateevt)
             (%setf-macptr w (#_frontwindow))))
          (unless (%null-ptr-p w)
            (setq wob (window-object w))))
        (when wob ;; this means that it is an event that the mode may be interested in and there's a window to go with it
          ;; This must handle the event one way or the other at this point
          (do-sk8-mode-or-window-event wob nil curmode)
          t  ;; indicate that the event was handled
          )))))

;; can't actually do this until the eventmodes have been defined
(defun turn-on-checking-for-eventmode ()
  (unless (listp *eventhook*)
    (setf *eventhook* (list *eventhook*)))
  (push #'checkforeventmodeevent *eventhook*))


;;; ________________
;;; WINDOW ACTIVATION
;;; ________________

(defmacro selecting-clos-window (&body body)
  `(unwind-protect
     (progn (setf *selecting-clos-window* t)
            ,@body)
     (setf *selecting-clos-window* nil)))

(defun sk8-bring-up-new-window (clos-window &optional justShowing? fullredraw?)
  (without-events
    ;; Create this window's graphics memory. And set the drawEnabled to true!
    (unless justShowing? 
      (restore-graphics-memory clos-window))
    (setf (slot-value clos-window 'dirty-window-p) t)
    (setf (slot-value clos-window 'drawEnabled) t)
    ;; Dirty the nodes in preparation and draw into gWorld...
    (let ((actor (slot-value clos-window 'my-actor-object))
          (wptr (wptr clos-window)))
      (sk8::makeBoundsRegion actor)
      #+ppc-target
      (when (eq (sk8::windowStyle actor) 'sk8::blank)
        (wdef-calc-regions wptr))
      (withLockedWindow clos-window
        (if fullredraw?
          (sk8::forceRedraw actor)
          (sk8::lightForceRedraw actor)))
      ;; If the window is visible, show it and activate it.
      (when (visible? (node-flags actor))
        (disable-whiting)
        (let ((gWorld (slot-value clos-window 'gWorld)))
          (window-show clos-window)
          (if (and (pointerp gWorld) (not (%null-ptr-p gWorld)))
            (gWorld-to-window gWorld clos-window)
            (sk8::lightForceRedraw actor)))
        #+ppc-target
        (#_calcVisBehind wptr (rref wptr windowRecord.contRgn))
        ))))

;;; Does the following:
;;; (1) keeps the window variables in synch.
;;; (2) If the thing to activate is a window, it is activated the new window. And the old one deactivated.
;;;    If the thing is a windoid, no activation/deactivation takes place.
;;; (3) The project of the window being brought to the front is activated.
;;; (4) A mousedown is dispatched to the window (if its doFirstClick is true).

(defun dispatch-generated-mousedown (clos-window newActor direct?)
  ;; [5] Dispatch the mousedown.
  ;; If this actor's dofirstClick is T, we send a mousedown to the deepest thing
  ;; hit in the new window!!!
  (when (and direct? 
             ;; wait! if the actor-under-the-mouse is not in the window we are
             ;; activating it must mean that the window was not selected using the mouse.
             (object-under-mouse)
             (neq (object-under-mouse) SK8::Stage)
             (eq (node-window (object-under-mouse)) clos-window))
    ;; Make sure the object under the mouse is the eventActor...
    (if *eventActor*
      (dispatch-mouseEnter&Leave *eventActor* (object-under-mouse))
      (just-do-mouseEnters (object-under-mouse)))
    ;; We send a mouseDown if the new actor was already active when the mouse went down on it
    ;; or when its doFirstClick property is True.
    (when (sk8::doFirstClick newActor)
      ;; Call the low level one who does all the variable set up for
      ;; click dispatching functions to work.
      ;; set event variables that depend on *currentTLA* to use this newly selected window
      (let ((evloc (subtract-points (make-point *eventH* *eventV*)
                                    (view-position clos-window))))
        (setf *event-location* evloc
              *event-x* (point-h evloc)
              *event-y* (point-v evloc)))
       (sk8-mousedown-event clos-window))))

(defun is-current-in-its-layer (clos-window)
  (or (eq clos-window *currentTla*)
      (if (sk8-windoid-p clos-window)
        (eq clos-window (front-window :include-invisibles nil :include-windoids t))
        (eq clos-window (front-window :include-invisibles nil :include-windoids nil)))))

;;; This is a simpler version used by sk8-close-window.

(defun sk8-select-after-close-window (windowToActivate oldActor)
  (let* ((newActor (slot-value windowToActivate 'my-actor-object))
         (project-activation-required (neq (sk8::project oldActor)
                                           (sk8::project newActor))))
    ;; Deactivate.
    (when project-activation-required 
      (sk8::Deactivate (sk8::project oldActor)))
    ;; Set state.
    (setf *currentTla* windowToActivate)
    (selecting-clos-window (window-select windowToActivate))
    ;; Activate.
    (when project-activation-required (sk8::activate (sk8::project newActor)))
    (sk8::activate newActor)))

(defun sk8-select-old-window (clos-window &optional (direct? t) force?)
  (declare (special *current-event* *bringing-up-window*))
  (let ((p (sk8-window-process clos-window)))
    (when p
      (setf (process-background-p p) nil)))
  (if (and (not force?)
           (is-current-in-its-layer clos-window)
           (not (boundp '*bringing-up-window*)))
    ;; The window to select is already the current window in its layer.
    ;; But if this happens when a Lisp window is closing, as evidenced by the fact that it is
    ;; hidden first, then we do need to send the activate event. 
    (let* ((wasALispWindowClosing (and (not (sk8-window-p *currentTla*))
                                       (not (window-shown-p *currentTla*)))))
      (setf *currentTla* clos-window)
      (selecting-clos-window (window-select clos-window))
      (when wasALispWindowClosing (sk8::activate (slot-value clos-window 'my-actor-object)))
      (dispatch-generated-mouseDown clos-window 
                                    (slot-value clos-window 'my-actor-object)
                                    direct?))
    ;; Not a current window, do all the work.
    (let* ((newActor (slot-value clos-window 'my-actor-object))
           (project-activation-required (and *currentTla* (sk8-window-p *currentTla*)
                                             (neq (sk8::project (slot-value *currentTla* 'my-actor-object))
                                                  (sk8::project (slot-value clos-window 'my-actor-object)))))
           (oldWindow (if (sk8-windoid-p clos-window)
                        (find-old-windoid clos-window)
                        (find-old-window clos-window)))
           (oldActor (when oldWindow (slot-value oldWindow 'my-actor-object))))
      (unless (sk8-window-p *currentTla*)
        (sk8dev::getTextFromMCLScrap))
      ;; Deactivate.
      (let ((p (when oldWindow (sk8-window-process oldWindow))))
        (when p
          (setf (process-background-p p) t)))
      (when oldActor 
        (sk8::deactivate oldActor)
        (when project-activation-required 
          (sk8::Deactivate (sk8::project oldActor))))
      ;; Set state.
      (setf *currentTla* clos-window)
      (selecting-clos-window (window-select clos-window))
      ;; Activate.
      (when project-activation-required (sk8::activate (sk8::project newActor)))
      (sk8::activate newActor)
      ;; do Mouse stuff...
      (dispatch-generated-mouseDown clos-window newActor direct?))))

(defun SK8-close-window (clos-window)
  (without-events 
    (let* ((actor (slot-value clos-window 'my-actor-object))
           (visible (visible? (node-flags actor)))
           (curWindow (eq clos-window *currentTla*))
           (closing-windoid? (sk8-windoid-p clos-window)))
      (setf (slot-value clos-window 'drawEnabled) nil)
      (leave-object-under-mouse-if-in actor)
      ;; [1] Get rid of the window!
      (without-interrupts
       (dispose-graphics-memory clos-window)
       (window-hide clos-window)
       ;; Since we do our own selection below, prevent sk8-select-old-window from being called.
       (flet ((clos-close ()
                (selecting-clos-window
                  (window-close clos-window))))
         (declare (dynamic-extent clos-close))
         (let ((p (ccl::window-process clos-window)))
           (if (eq p *current-process*) ;; if about to kill ourselves, queue it up for when we are done here
             (queue-up-sk8-window-event p
                                        #'clos-close
                                        clos-window
                                        (rref *current-event* :event.what)
                                        (rref *current-event* :event.message)
                                        (rref *current-event* :event.where)
                                        (rref *current-event* :event.when)
                                        (rref *current-event* :event.modifiers)
                                        *event-location*
                                        nil
                                        t   ;; this says flush queue first
                                        )
             (clos-close))))
       )
      ;; [2] Activate something.
      (when (and visible curWindow)
        ;; What to activate? If the thing going away was a windoid, activate the
        ;; top remaining windoid (nothing if there are no remaining windoids). 
        ;; If it was a window, activate the frontmost window left. 
        (let ((windowToActivate (if closing-windoid?
                                  (or (computed-active-windoid) (computed-active-window))
                                  (or (computed-active-window) (computed-active-windoid)))))
          (if windowToActivate
            (if (sk8-window-p windowToActivate)
              ;; A sk8 window? Do the "simple" activation required.
              (sk8-select-after-close-window windowToActivate actor)
              ;; Lisp window activates on its own.
              (setf *currentTla* windowToActivate))
            ;; Is this when we run out of windows?
            (setf *currentTla* windowToActivate)))))))

(defun clean-up-bogus-windows ()
  (dolist (w (windows :class '*sk8-window* :include-invisibles t))
    (when (or ;; (typep (slot-value w 'mf::my-actor-object) 'sk8::DisposedObject)
           (not (sk8::container (slot-value w 'my-actor-object))))
      (setf (slot-value w 'drawEnabled) nil) ;; make sure we don't try to treat this as a visible sk8 window
      (window-close w) ;; and finish up closing that somehow wasn't done
      )))

;;; Should go through the current mode if one is active.

#|
(defmethod window-select ((clos-window *SK8-WINDOW*))
  (if *selecting-clos-window*
    (call-next-method)
    (let ((curmode (sk8:ActiveMode)))
      (if curmode
        (sk8::handleActivate curmode)
        (sk8-select-old-window clos-window nil)))))
|#

(defmethod window-select ((clos-window *SK8-WINDOW*))
  (if *selecting-clos-window*
    (call-next-method)
    (sk8-select-old-window clos-window nil)))

(defmethod window-close ((clos-window *sk8-window*))
  (if (slot-value clos-window 'drawenabled) ;; if nil, we already did the close, now treat as normal clos window
    (sk8::close (slot-value clos-window 'my-actor-object))
    ;; Set the pixpat free by installing a pattern in its place.
    (progn
      (with-port (wptr clos-window)
        (#_penPat *gray-pattern*))
      (call-next-method))
    ))

(defmethod window-hide ((clos-window *sk8-window*))
  (setf (slot-value clos-window 'drawEnabled) nil)
  (call-next-method))

(defmethod window-show ((clos-window *sk8-window*))
  (setf (slot-value clos-window 'drawEnabled) t)
  (call-next-method)
  ;; create window process when showing. Let window-close kill it
  ;;; The following expression is disabled to cause all SK8 windows to run in either the sk8 system process or the user window process
  #+ignore
  (unless (ccl::window-process clos-window)
    (setf (ccl::window-process clos-window)
          (sk8dev::make-sk8-window-process (ccl::window-title clos-window))))
  ;; Force an update to show the window contents right away.
  (sk8-update-event clos-window))

(defun lisp-windows-null-event ()
  (dolist (nextWindow (windows :class '*sk8-window*))
    (sk8-null-event nextWindow :indirect t)))

(defun SK8-drag-window (window)
  (let ((actor (slot-value window 'my-actor-object)))
    (if actor
      (sk8::drag actor))))

(defun SK8-grow-window (window)
  (let ((actor (slot-value window 'my-actor-object)))
    (when actor (sk8::resize actor 'sk8::bottomRight))))

;;; This is called by our Lisp windows when they activate. The purpose is to deactivate
;;; the sk8 window that was active before that.

(defun sk8::restore-cursor ()
  )

(defun lisp-windows-activate-event (w)
  (unless (or (eq w *currentTla*) (null (wptr w)))
    (if (sk8-window-p *currentTla*)
      (ignore-errors
       (let ((theCurrentWindow (find-old-window w)))
         (when theCurrentWindow
           (sk8::deactivate (slot-value theCurrentWindow 'my-actor-object)))
         ;; Clear the currentCommandKey of the System.
         (setf (sk8::currentCommandKey sk8::system) nil)
         ;; SK8 window -> Lisp window: send text to MCL scrap.
         (sk8::sendTextToMclScrap)))
      (when *currentTla*
        (view-deactivate-event-handler *currentTla*)))
    (setf *currentTla* w)
    (sk8::restore-cursor)))

;;; Define these just in case there are problems during the build...

(defun sk8::getTextFromMCLScrap ()
  )

(defun sk8::sendTextToMCLScrap ()
  )

;; MCL 3.0 now queries fred's window for a key-handler, so we better return one to prevent a crash
;;*** This works around a bug in the current MCL patch (beta of patch 3) see comments in the code

(defmethod current-key-handler ((w *sk8-window*))
  ;; a bug in the current patch level 3 beta of MCL causes this function to sometimes be called with
  ;; the wrong argument. This contains a workaround that tests for the result being nil, which indicates
  ;; the error condition, and forces the correct result in that case. After MCL is fixed, we can get
  ;; rid of the test and the use of *currenttla*, but leaving if we forget and leave it in, it will not break anything
  (flet ((sk8-key-handler (ww) 
           (let ((theActor (slot-value ww 'my-actor-object)))
             (when (and *EditText-the-object*
                        (sk8dev::inheritsFrom (sk8::keyTarget theActor) *EditText-the-object*))
               (sk8::editData (sk8::keytarget theActor))))))
    (let ((result (sk8-key-handler w)))
      (if result
        result
        (when (and *currentTla* (neq w *currenttla*))
          (if (sk8-window-p *currenttla*)
            (sk8-key-handler *currenttla*)
            (current-key-handler *currentTla*)))))))

;;; _______________________________ 
;;; Patches to MCL!
;;; _______________________________ 

(defmethod view-activate-event-handler :around ((w window))
  (unless (sk8-window-p w) 
    (lisp-windows-activate-event w))
  (call-next-method))

;;; This fixed the menu select problem. Selects the right window to act on for
;;; window menu items. 

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  (defun ccl::get-window-event-handler ()
    (or gs::*currentTla*
        (ccl::window-event-handler (front-window :include-windoids t)))
    ))

;;; __________
;;; WINDOW ???
;;; __________

;;; 1.0
;;; SET-VIEW-SIZE -- This method sets the window's size, making sure to update any 
;;;               offscreen worlds.

(defmethod set-view-size ((window *SK8-WINDOW*) h &optional v)
  (unless v
    (setq v (point-v h)
          h (point-h h)))
  ;; first, do the usual thing (this actually changes the window size)
  (#_SizeWindow (wptr window) h v nil)
  ;; update the gWorld, if any
  (SK8-update-window-gWorld window))

(defmethod window-save ((window *SK8-WINDOW*))
  (let* ((frame (slot-value window 'my-actor-object))
         (project (sk8::project frame)))
    (unless (or (eq project sk8::sk8) 
                (and (boundp 'sk8::ui) (eq project sk8::ui)))
      (sk8::messageToUser 
       (format nil "To save ~a, save its project: ~a"
               (sk8::objectString frame)
               (sk8::objectString project))))))
                          
(defmethod window-save-as ((window *SK8-WINDOW*))
  (window-save window))

(defmethod menu-item-action ((menuItem *SK8-MENU-ITEM*))
  (let ((frame (mf::my-frame menuItem)))
    (if frame
      (sk8::menuSelect frame))))

(defmethod menu-update ((self *SK8-menu*))
  (let ((frame (mf::my-frame self)))
    (if frame
      (sk8::update frame))))

;;; ______________________
;;; CUT/COPY/PASTE/CLEAR...
;;; ______________________

(defmethod copy ((window *sk8-window*))
  (let ((theActor (slot-value window 'my-actor-object)))
    (when (sk8::keyTarget theActor)
      (sk8::copySelectionToClipboard (sk8::keyTarget theActor)))))

(defmethod cut ((window *sk8-window*))
  (let ((theActor (slot-value window 'my-actor-object)))
    (when (sk8::keyTarget theActor)
      (sk8::cutSelectionToClipboard (sk8::keyTarget theActor)))))

(defmethod paste ((window *sk8-window*))
  (let ((theActor (slot-value window 'my-actor-object)))
    (when (sk8::keyTarget theActor)
      (sk8::pasteClipboardToSelection (sk8::keyTarget theActor)))))

(defmethod clear ((window *sk8-window*))
  (let ((theActor (slot-value window 'my-actor-object)))
    (when (sk8::keyTarget theActor)
      (sk8::clearSelection (sk8::keyTarget theActor)))))

#|
	Change History (most recent last):
	2	6/1/93	Hernan	Making simpleText update its rect when zooming its window.
	3	6/1/93	Hernan	Removed want-mouseWithin slot from *sk8-window*.
	4	6/3/93	kleiman	window-event commented out access to movies
						slot in a toplevel actor; now removes movie from
						*movies-are-playing-p*
	5	6/14/93	Hernan	Replaced call of windows to map-windows to 
						reduce consing during idle time.
	6	6/14/93	Hernan	Second try to check in...
	7	6/21/93	Hernan	Added an optional arg to the contents draw 
						functions that lets you specify whether you want
						the fill rendered or not. Usefull for the picker...
	8	6/22/93	Hernan	Clipping as usual is required when dealing with actors
						which are boundedByContents. This is because the frames need
						to be rendered too.
	9	6/22/93	Hernan	UnlockWindow now takes a force argument which
						sets the locklevel to nil (unlocks the window completely).
	10	6/25/93	Hernan	The Great Renaming of 93.
	11	6/28/93	Hernan	Removed two unused args from sk8-draw-window.
	12	8/31/93	hernan	Slightly changing the order in which the dirty
						region is initialized to allow the new (direct-
						blitting) field to work.
	13	9/3/93	hernan	Added glue to the new clipboard.
	14	9/3/93	hernan	Created functions to be defined later so that an
						eventual call to lisp-windows-activate-event
						during the build does not crash the system.
	15	9/20/93	kleiman	temporarily nuked lisp-window-... stuff
	16	9/20/93	hernan	Fixing add-dirty-node-regions to allow the
						dirtying of PARTS of an actor.
	17	9/29/93	hernan	Uncomenting view-activate-event-handler of window.
	18	9/29/93	hernan	node obsoleted
	19	10/1/93	hernan	=====================================
						= Sad but true: the node is gone!!! =
						=====================================
	20	10/1/93	hernan	Fixing things related to nodes going away.
	21	10/4/93	kleiman	Stage problems in sk8-null-events. Temp fix.
	22	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	23	10/5/93	hernan	The eventActor variable is what the user sees. To
						the user, the eventActor is the original actor to 
						which a message is sent. The graphics system, on
						the other hand, keeps the object-under-mouse
						which has at all times what is under the mouse. 
						The two variables are put in synch in sk8-null-event
						meaning that within event modes, if this function
						is not running, the eventActor will never be updated. 
						Thus, in this case it is the responsability of the
						eventMode to keep things in track. I think this is
						fine since defining event modes is a really 
						advanced SK8 feature: the user who wants to 
						define it can go the extra mile...
	24	10/6/93	hernan	Using object-under-mouse instead of the eventActor
						to find out the recipient of events.
	25	10/20/93	hernan	When an object ceases to be under the mouse
						because it is disposed, we clear the eventActor
						variable if required.
	26	11/1/93	hernan	mf::inheritsFrom? -> inheritsFrom.
	27	11/5/93	hernan	*fast-draw* is now switched off when the tla is
						seen by the window.
	28	11/5/93	kleiman	Making the scriptEditText draw when the window
						comes up.
	29	11/8/93	hernan	Fixing very serious bug that is ruining the chances
						that d4 will be the real thing: extra check is sk8-
						select-old-window at the end (where it deals with
						doFirstClick stuff) makes sure the mousedown is
						only sent when the window was activated actually
						using the mouse.
	30	11/10/93	chip	a couple more fixes to SK8-select-old-window
	31	11/10/93	hernan	Making temporary fix to have the SK8 clipboard
						talk to the MCL scrap when we move from the SK8
						world to the MCL world and viceversa.
	32	11/19/93	hernan	Added *oldWindow* which remembers who the
						previously selected window was and uses this
						knowledge when  a window gets closed to figure
						out who will get activated.
	33	11/19/93	hernan	Fixed sk8-select-old-window to send a low level
						mousedown event to the window. This sets up all
						that is required for the click dispatch handlers to 
						work.
	34	11/22/93	hernan	Removing autotabs slot from *sk8-window*.
	35	11/29/93	hernan	Window-event only runs when SK8 is the current
						application.
	36	11/30/93	hernan	Undid all changes having to do with SK8 being in
						the background. We can get all we need by using
						the *events-on* variable (set it to false on 
						suspend and true on resume).
	37	11/30/93	kleiman	*current-event* declared special
	38	11/30/93	hernan	Enclosing dangerous part of lisp-windows-... with
						ignore-errors.
	39	12/2/93	dy		Change window-event to call the new keep-movies-playing
						function instead of doing the movie stuff inline.
	40	12/2/93	dy		fixed previous checkin comment
	41	12/7/93	hernan	Fixing window-event to play the movies.
	42	12/7/93	hernan	Making a function that redraws all editTexts 
						when a window comes up.
	43	12/10/93	hernan	The dirtyContents flag of owners has to be set or
						they will not draw properly during fast-draw.
	44	12/10/93	hernan	SK8-point-in-which-object now returns the stage
						if sk8-point-in-which-node returns nil.
	45	12/21/93	hernan	Checking for memory errors when making new
						regions.
	46	2/10/94	kleiman	took out movies loop from window-event
						(now done in do-event MCL patch)
	47	2/12/94	kleiman	name changes
	48	2/18/94	dy		delete the commented-out old movie event stuff
	49	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	50	2/28/94	hernan	Now using GCable mac things.
	51	3/1/94	sidney	T_NewRgnGC has to be SK8::T_NewRgnGC here
	52	3/2/94	Hernan	Porting to Fred 3.0
	53	3/9/94	Hernan	Getting rid of disposed.
	54	3/11/94	rod		Dispatching idle events to user menubars in the 
						stage, if they want them.
	55	3/17/94	Hernan	Adding a size argument to sk8-update-window-
						gWorld so that the size is not always the window
						size.
	56	3/18/94	Hernan	Bug in sk8-select-old-window. It is incorrect for
						*oldWindow* and *currentWindow* to have the
						same thing in it. This happens when this function
						gets called more than once for a window coming 
						up.
	57	3/21/94	Hernan	Changing window-close method to make sure
						the window's pixmap is not the desktop's pixmap.
	58	3/24/94	Hernan	Fixing sk8-window-zoom to call resized when the
						window has actually changed size.
	59	3/24/94	Hernan	Keyword problem in sk8-grow-window.
	60	3/25/94	Hernan	Fixing sk8-bring-up-new-window to draw all the
						fields as soon as the window comes up. This gets
						rid of the garbage-when-coming-up problem.
	61	3/28/94	Hernan	Making sk8-select-old-window do nothing when
						the *currentWindow* is the window we want to
						select.
	62	3/31/94	Hernan	Removing error message when selecting a window
						that is already selected.
	63	3/31/94	Hernan	Making window gWorlds not be gcable.
	64	4/15/94	Hernan	Implementing the new mouseEnter and 
						mouseLeave. MouseEnter and leave now take the
						physical meaning: if the mouse is within your
						bounds then mouseEnter was sent to you (even
						though you might not be the current event actor).
				
						The new functions are also general enough to be
						called for different types of enter/leave operations. 
						(eg. draggingMouseEnter).
	65	4/15/94	Hernan	Depth will return 0 when the actor is not on the
						stage at all.
	66	4/18/94	Hernan	add
	67	4/18/94	Hernan	update-clip-of-dirty-owners now also clears the
						ownedRegion when the owner is responsible for
						the drawing (it is in the dirty-nodes).
	68	4/19/94	Hernan	Getting rid of consing in sk8-draw-contents-with-translucency.
	69	4/19/94	Hernan	Exploring ways to reduce consing.
	70	4/20/94	Hernan	Replacing calls to get-record-field with calls to
						region-into-rect (which does not cons).
	71	4/26/94	Hernan	When the optional event is passed to dispatch-
						mouseWithins, we do not check if the actor wants
						mousewithins before we dispatch the event.
	72	6/23/94	sidney	declare *events-on* special to get rid of warnings
	73	6/25/94	sidney	use the error checking wrappers on trap calls for getting a new gworld
	74	6/25/94	sidney	back out changes until we figure out why they crash
	75	7/6/94	Hernan	1169269: replacing the eventX variables with
						functions.
	76	7/14/94	Hernan	1171670: need this file to recompile for next build.
	77	7/18/94	Hernan	Defining restore-cursor to do nothing until it is
						really defined by the cursors file.
	78	7/25/94	Hernan	1175911: making actors remember their keyTarget
						when they are not on the Stage. This gets rid of
						the mf::curKey slot of windows.
	79	8/10/94	Hernan	Made some methods (as in things defined with
						defmethod) of *sk8-window* go through the
						eventmode handlers if an event mode is currently
						active.
	80	8/11/94	Rod 	Patching window-select code.  Old code is there
						This is the bug when the editor would not be
						visible due to modalDialogMode.
	81 	 8/12/94	Hernan  	The values of the eventH and V variables are no
							longer turned into short floats (since they
							represent physical coords anyway.
							Also, sk8-select-old-window updates the eventActor,
							dispatching all mouse enters and leaves required.
	82 	 8/15/94	Hernan  	1162071: update-clip-of-dirty-owners does not
							need to add the owner's region to the dirty region
							if the owner is not deeply visible.
	83 	 8/26/94	Hernan  	11821467: initial-draw-fields calls lightForceRedraw
							to force editText to go through its draw function.
							Also had to change Sk8-select-old-window to
							refresh the fields AFTER the window comes to the
							front.
	84 	 8/31/94	Hernan  	Windoids no longer deactivate windows below
							them. This means that we need to remember the
							currentWindow in addition to the current active
							one (which can be a window or windoid). For that
							we use the *currentTla* variable. sk8-select-old-
							window has become more complex as a result of
							this change.
	85 	 9/ 2/94	Hernan  	1183700: Using *editText-the-object* instead of
							a direct reference to EditText.
	87 	 9/ 7/94	dy      	1184299: changed sk8-point-in-which-actor to
							deal correctly with the case when the mouseDown
							happened on the actor's window titlebar.
	88 	 9/28/94	Hernan  	sk8-simple-draw: recomputing the boundsRegion
							only when required. Also fixed sk8-select-old-
							window to deal with deactivating windoids. Added
							the *oldTla* variable.
	89 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	90 	11/ 4/94	Hernan  	Removed all references to initial-draw-fields. This
							was the last kludge remaining from the days when
							EditText could not draw into gWorlds.
	91 	11/ 8/94	Hernan  	Moving deeply-visible? to structures.lisp
	92 	11/10/94	Hernan  	Window-show forces an update to show the
							window as soon as possible.
	93 	11/28/94	Hernan  	Making sk8-bring-up-new-window not force the
							recomputation of all the regions since it might 
							have been done already.
	94 	12/ 5/94	Hernan  	1203859: sk8-full-draw should clip before drawing
							the text.
	95 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	96 	12/12/94	Hernan  	1205352: sk8-draw-contents-with-translucency
							should draw the translucent things from back to
							front...
	97 	 1/21/95	sidney  	remove some class defintions that are already in utilties.lisp
	98 	 1/29/95	sidney  	added a comment, but I'm really forcing recompile of a call to a changed macro
	99 	 2/ 2/95	Hernan  	sk8-full-draw has to clip to again to draw the
							text.
	100	 2/ 8/95	Hernan  	1215529: window-event now recognizes a set of
							keys that will guarantee to throw you out of an
							event mode.
	101	 3/ 3/95	Hernan  	Fixed sk8-select-old-window to correctly
							activate/deactivate things when a previously
							active window is being activated as the result of
							the deactivation of a windoid.
	102	 3/14/95	Hernan  	In sk8-select-old-window if the window to be
							selected is a previously active window, then there
							is no need to activate it but the mouseDown is
							interpreted as a mouseDown event regardless of
							the value of the doFirstClick property.
	103	 3/16/95	till    	wrappin' some traps, #_lockpixels to sk8-update-window-gworld, sk8-draw-data
	104	 3/17/95	till    	Better to use with-locked-pixels-force
	105	 4/ 6/95	sidney  	sometimes (i.e., on restore) new windows need fullforceredraw, most of the time they can be done faster
	106	 4/18/95	dy      	sk8-draw-data is now robust in the face of render
							handlers that don't restore the Fore or Back color.
	107	 4/24/95	Hernan  	1228501: implementing 2 real layers: windows and windoids.
	108	 4/26/95	Hernan  	sk8-select-old-window gets the text from the MCL scrap
							when the window being deselected was a lisp window.
	2  	 6/23/95	Hernan  	Fixes having to do with the two window layers and with
							active windows getting clicks right away.
	3  	 6/26/95	Hernan  	1250510: when a SK8 window closes, we now activate the
							next window ourselves (without calling sk8-select-old-window).
	4  	 6/30/95	sidney  	(no change) force recompile after dynamic-extent bug has been fixed in MCL
	5  	 7/12/95	Hernan  	Drawing with the drawing lock grabbed.
	6  	 9/20/95	sidney  	define a currrent-key-handler for sk8 windows because MCL 3.0 fred expects there to be one
	7  	 9/27/95	sidney  	workaround bug in the current MCL patch
	8  	 9/27/95	sidney  	last change assumed *currenttla* was a sk8 window. Handle case in which it isn't.
	2  	 4/ 9/96	Hernan  	Changing sk8-draw-contents-with-translucency to use the
						region pool mechanism.
	3  	 4/16/96	Hernan  	Getting rid of *temp-region*, *sk8-penState* and 
						*original-dirty-region*.
	4  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	5  	 5/ 1/96	Hernan  	Making lisp-windows-activate-event set the currentCommandKey 
						of the System to nil.
	6  	 7/23/96	Hernan  	sk8-bring-up-new-window has to do WDEF work when 
						running in the PPC.
	7  	 7/25/96	Hernan  	Added windowStyle of Actor. Simplified sk8-bring-up-new-window's check for blank style.
	8  	 7/25/96	Hernan  	Redefining MCL's get-window-event-handler to make sure
						menu commands affect the *currentTla* and the user gets
						expected SK8 behaviour. Also fixing sk8-select-old-window
						to correctly send an activate event to a SK8 window when
						a lisp window has just been closed.
	9  	 9/30/96	sidney  	process window events in separate thread
	10 	10/ 7/96	sidney  	don't need disposing-clos-window anymore
	11 	10/10/96	sidney  	check for mode exit happening after a mode event is queued but before it is executed
	12 	10/13/96	sidney  	set some global state variables to reflect new selection of window before dispatching a dofirstclick mousedown
	13 	10/14/96	Brian   	Adding error checking to dispatch mouseleave to 
						catch weird thread caused cases.
	14 	10/14/96	Brian   	Adding yet another check to find common
						container.
	15 	10/17/96	sidney  	catch :cancel around windowloop so a process won't be hosed when someone cancels a dialog
						make selected window's process be only one not in background and activate it when an event is queued to speed up interactive processing
						add another check for a race condition when clicking on close box
	16 	10/22/96	Hernan  	do-sk8-mode-or-window-event should not assume that the
						*currentTla* is valid...
	17 	10/22/96	sidney  	add MCL3.1 68K version of low level hack for symbol-bound-in-process-p
	18 	11/14/96	sidney  	simplify process stuff by running window events in either one system or one user process
						redefinition of get-window-event-handler had wrong package and so did nothing
						disable sk8 window event handling when process is in sk8 debugger
	19 	12/12/96	Hernan  	Fixed sk8-close-window to avoid queueing the window-close event
						when the window-process is the sk8-system-process. This 
						was responsible for the wptr problem when resume got
						called.
	20 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
