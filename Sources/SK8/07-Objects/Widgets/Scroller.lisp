(in-package :SK8Development)

(provide "SCROLLER")

(require "RECTANGLE")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

01-22-93 ruben id -> objectName
09-24-92 ICE removed the add-properties
09-23-92 ice added default cursors and icons
09-20-92 hernan changing this to work with MF VI

|#

;;; MinimumValue:  the smallest possible currentValue.
;;; MaximumValue: the largest possible currentValue.
;;; CurrentValue: the location of the top/left of the thumb.
;;; scrollStep: amount to change the currentValue by when the arrows get moused.
;;; pageStep: amount to change the currentValue by when the body gets moused.
;;; range: maximumValue - minimumValue + thumbview (!!!)
;;; thumbview: how much of the range shows in the scrollee at any one time. This makes sense
;;;          for actors like editText and pickers. Somehow artificial for others. The important
;;;          thing is that the thumbview controls the size of the thumb.
;;;          NOTE: a thumbview of nil means the thumb has a constant size and does not
;;;               resize when the range changes. The usual scroller icon is used to render the
;;;               thumb in this case.

;;;;  -------------------------------------------------------------------------------------------------
;;;;  -------------------------------------------------------------------------------------------------
;;;;  Brian is adding the slider.  To do this we need to abstract out this "Thickness" variable.  This is the
;;;;  inset of the body from the scrollers edges.  It is called thickness because arrows are square and thus the
;;;;  default inset should be the thickness of the scroller.  The slider will redefine this.
;;;;  We also add maximumThumbSize, which if set to false has no maximum.  (This is a nice feature anyway).
;;;;  -------------------------------------------------------------------------------------------------

(defmacro |WITH LOCKEDSCROLLER| ((theScroller) &body body)
  `(while-scrolling ,theScroller
    ,@body))

;;; SCROLLER -- the SK8 scroller object is here!

(new rectangle :objectname "Scroller"
     :properties '((minimumValue :value 0)
                   (maximumValue :value 100)
                   (currentValue :value 0)
                   (scrollStep :value 1)
                   (pageStep :value 10)
                   (thumbView :value nil)
                   (minimumThumbSize :value 5)
                   (maximumThumbSize :value nil))
     :undisposable t
     :prototype t
     :project sk8)

(setf (fillColor scroller) white)
(setFrameSize scroller 0 0)
(setBoundsRect scroller 0 0 16 200)

;;; 1. Compute pixel value. Store it in the properties.

(defun scroller-pixel-value (theScroller)
  (getf (gs:node-properties theScroller) :pixel-value 0))

(defun set-scroller-pixel-value (theScroller pixVal)
  (setf (getf (gs:node-properties theScroller) :pixel-value) pixval))

;;; Giving users acces to these functions.

(define-handler pixelValue (Scroller)
  (scroller-pixel-value me))

(define-handler (setf pixelValue) (newValue Scroller)
  (set-scroller-pixel-value me newValue))

;;; Computes the new pixel value. Should be called whenever the range or thumbview changes: whenever
;;; the maximumValue, minimumValue, or thumbView change.

(define-handler computePixelValue (Scroller)
  (let* ((minval (minimumValue me))
         (maxval (maximumValue me)))
    (cond ((zerop (- maxVal minVal)) 0)
          (t 
           (let* ((orientation (orientation me))
                  (range (- maxVal minVal))
                  (theBody (scrollerBody me))
                  numPixels thumbsize)
             (if (eq orientation 'vertical)
               (setf numpixels (height theBody)
                     thumbSize (height (thumb me)))
               (setf numpixels (width theBody)
                     thumbSize (width (thumb me))))
             ;; And now return the pixel value.
             (if (zerop (- numPixels thumbsize))
               0
               (/ range (- numPixels thumbSize))))))))

;;; Given the currentValue, compute the startThumb.

(define-handler currentValueToThumbPosition (Scroller)
  (let* ((orientation (orientation me))
         (minval (minimumValue me))
         (theBody (scrollerBody me))
         (startPixel (currentValue me))
         (offset (if (eq orientation 'horizontal) (left thebody) (top thebody)))
         (pixelValue (scroller-pixel-value me)))
    (+ offset (if (zerop pixelValue)
                0
                (/ (- startPixel minVal) pixelValue)))))

;;; Given the startThumb, computes the currentValue.

(define-handler ThumbPositionToCurrentValue (Scroller)
  (let ((numPixels (if (eq (orientation me) 'vertical)
                     (- (top (thumb me)) (top (scrollerBody me)))
                     (- (left (thumb me)) (left (scrollerBody me))))))
    (+ (minimumValue me) (* numpixels (scroller-pixel-value me)))))

;;; Common things: (1) resize thumb and recompute pixelValue. (2) Given a currentValue, move the thumb to the right
;;;             place.

;;; MOUSEENTER (scroller) -- changes the cursor to show the user we are inside.

(define-handler mouseEnter (scroller)
  (setf (cursor stage) standardCursor))

;;; MOUSELEAVE (scroller) -- changes the cursor to show the user we have left.

(define-handler mouseLeave (scroller)
  (setf (cursor stage) standardCursor))

;;; THICKNESS

(define-handler thickness (scroller)
  (let ((orientation (orientation me))
        thickNess left top right bottom)
    (SK8-multival-setf (left top right bottom) (boundsRect me :physical t))
    (if (eq orientation 'horizontal)
      (setq thickness (- bottom top))
      (setq thickness (- right left)))
    thickness))

;;; orientation -- returns vertical if the scroller is vertical and horizontal
;;;            otherwise. This is used a lot by the bestsize and resized methods
;;;            of the scroller's contents.

(define-handler orientation (Scroller)
  (sk8-multival-bind (hSize vSize) (size me)
    (if (> hSize vSize) 'horizontal 'vertical)))

;;; SCROLLERBODY -- the body of the scroller.

(new rectangle :objectname "ScrollerBody" :project sk8)
(setf (fillColor scrollerBody) scrollerBodyColor)
(setFrameSize scrollerBody 1 1)
(setBoundsRect scrollerBody 0 16 16 184)
(setf (container scrollerBody) scroller)
(tagPart scroller scrollerBody 'scrollerBody)

;;; BESTSIZE (scrollerBody) -- resizes the body of the scroller to fit within the scroller. The subtraction of
;;;                      thickness is done to remove the arrows from the body. The body, thus is
;;;                      the scroller minus the arrows (the area where the thumb moves).
;;;
;;; Note that using thickness here works because the rectangles that contain the arrows are always squares!

(define-handler bestSize (scrollerBody)
  (declare (special left top right bottom thickNess orientation))
  (if (eq orientation 'vertical)
    (setBoundsRect me left (+ top thickNess -1) right (- bottom thickness -1)
                   :physical t)
    (setBoundsRect me (+ left thickNess -1) top (- right thickness -1) bottom
                   :physical t)))

;;; SCROLLERTHUMB -- the thumb.

(new rectangle :objectname "ScrollerThumb" :project sk8)
(setf (frameColor scrollerThumb) black)
(setf (fillColor scrollerThumb) white)
(setFrameSize scrollerThumb 1 1)
(setBoundsRect scrollerThumb 1 50 15 150)
(setf (container scrollerThumb) scroller)
(tagPart scroller scrollerThumb 'thumb)

;;; MOUSEENTER (scrollerThumb) -- changes the cursor into the open hand to show that we may drag the thumb.

(define-handler mouseEnter (scrollerThumb)
  (setf (cursor stage) standardCursor))

;;; BESTSIZE (scrollerThumb) -- compute the size of the thumb and call update to move it to
;;;                       the right currentValue.

(define-handler bestsize (scrollerThumb)
  (let* ((theScroller (container me))
         (view (thumbview theScroller))
         (orientation (orientation theScroller))
         (minThumbSize (minimumThumbSize theScroller))
         (maxThumbsize (maximumThumbsize theScroller)))
    (withActorLocked (me)
      (if view
        ;; There is a thumbview! We compute the size of the thumb and set it.
        (let* ((theBody (scrollerBody theScroller))
               (minval (minimumValue theScroller))
               (maxval (maximumValue theScroller))
               (range (- maxVal minVal))
               (numPixels (if (eq orientation 'horizontal) (width theBody) (height theBody)))
               (thumbSize (if (or (zerop range) (zerop (+ range view))) ;; (or (zerop range) (> view maxVal)) 
                            numPixels
                            (max minThumbSize (* (/ view (+ range view)) numPixels))
                            )))
          (if maxThumbsize (setf ThumbSize (min maxThumbsize ThumbSize)))
          ;; We have the size! Now set it and update! We make sure the thumb fits perfectly
          ;; within the body!
          (sk8-multival-bind (ww hh) (size theScroller)
            (if (eq orientation 'vertical)
              (setBoundsRect me 1 (v me) (1- ww) (+ (v me) thumbsize))
              (setBoundsRect me (h me) 1 (+ (h me) thumbsize) (1- hh))))
          ;; The pixel value has changed! Recompute it and set it.
          (set-scroller-pixel-value theScroller (computePixelValue theScroller)))
        ;; No view: make the thumb's size be the scroller's thickness.
        (sk8-multival-bind (ThumbWidth ThumbHeight) (size me)
          (sk8-multival-bind (width height) (size theScroller)
            (unless (and (= ThumbWidth width) (= ThumbHeight height))
              (if (eq orientation 'vertical)
                (setBoundsRect me 0 (v me) width (+ (v me) width))
                (setBoundsRect me (h me) 0 (+ (h me) height) height))
              ;; The pixel value has changed! Recompute it and set it.
              (set-scroller-pixel-value theScroller (computePixelValue theScroller))))))
        (update theScroller))))

;;; SCROLLERUPARROW --

(new rectangle :objectname "ScrollerArrow" :project sk8 :undisposable t)

(new scrollerArrow :objectname "ScrollerUpArrow" :project sk8)
(setf (fillColor scrollerUpArrow) scrollerUpRenderer)
(setFrameSize scrollerUpArrow 0 0)
(setBoundsRect scrollerUpArrow 0 0 16 16)
(setf (container scrollerUpArrow) scroller)
(tagPart scroller scrollerUpArrow 'upArrow)

(define-handler bestSize (scrollerUpArrow)
  (declare (special left top right bottom thickNess orientation))
  (if (eq orientation 'vertical)
    (progn
      (unless (eq (fillcolor me) scrollerUpRenderer)
        (setf (fillColor me) scrollerUpRenderer))
      (setBoundsRect me left top right (+ top thickness) :physical t))
    (progn
      (unless (eq (fillcolor me) scrollerLeftRenderer)
        (setf (fillCOlor me) scrollerLeftRenderer))
      (setBoundsRect me left top (+ left thickness) bottom :physical t))))

;;; SCROLLERDOWNARROW --

(new scrollerArrow :objectname "ScrollerDownArrow" :project sk8)
(setf (fillColor scrollerDownArrow) scrollerDownRenderer)
(setFrameSize scrollerDownArrow 0 0)
(setBoundsRect scrollerDownArrow 0 184 16 200)
(setf (container scrollerDownArrow) scroller)
(tagPart scroller scrollerDownArrow 'downArrow)

(define-handler bestSize (scrollerDownArrow)
  (declare (special left top right bottom thickNess orientation))
  (if (eq orientation 'vertical)
    (progn
      (unless (eq (fillcolor me) scrollerDownRenderer)
        (setf (fillColor me) scrollerDownRenderer))
      (setBoundsRect me left (- bottom thickNess) right bottom :physical t))
    (progn
      (unless (eq (fillcolor me) scrollerRightRenderer)
        (setf (fillColor me) scrollerRightRenderer))
      (setBoundsRect me (- right thickNess) top right bottom :physical t))))

;;; RESIZED (scroller) -- when the scroller is resized this method is called to resize all its components.
;;;                  The code befor your eyes just sets a few variables that are put on the stack
;;;                  for the bestsize methods of its contents to use.

(define-handler resized (scroller)
  (let ((orientation (orientation me))
        thickNess left top right bottom)
    (declare (special orientation thickNess left top right bottom))
    (SK8-multival-setf (left top right bottom) (boundsRect me :physical t))
    (setq thickness (thickness me))
    ;; resize interfaceItems
    (dovector (subActor (gs:node-contents me))
      (bestsize subActor))))

;;; RESCALETHUMB -- nothing misterious here: we just set up the variable to put on the stack and
;;;               then call bestSize to do all the work. We can probably do away with this altogether.

(define-handler rescaleThumb (scroller)
  (bestSize (thumb me)))

;;; Makes sure the thumb is at the right place for the currentValue of the scroller.
;;; There is no need to rescale the thumb.

(define-handler update (scroller)
  (if (eq (orientation me) 'horizontal)
    (setf (left (thumb me) :resizing nil) (currentValueToThumbPosition me))
    (setf (top (thumb me) :resizing nil) (currentValueToThumbPosition me))))

(define-handler (setf currentValue) (value scroller)
  (let ((oldValue (currentValue me)))
    ;; Make sure the current value is within the max and min.
    (cond ((> value (maximumValue me)) (setf value (maximumValue me)))
          ((< value (minimumValue me)) (setf value (minimumValue me))))
    (setf (slot-value me 'currentValue) value)
    (when (and (neq lockedScroller me) (not (= oldValue value)))
      (update me))))

(define-handler (setf minimumValue) (value scroller)
  (let ((oldValue (minimumValue me)))
    (setf (slot-value me 'minimumValue) value)
    (when (and (neq lockedScroller me) (not (= oldValue value)))
      (rescaleThumb me))
    (when (< (currentValue me) value)
      (setf (currentValue me) value))))

(define-handler (setf maximumValue) (value scroller)
  (let ((oldValue (maximumValue me)))
    (setf (slot-value me 'maximumValue) value)
    (when (and (neq lockedScroller me) (not (= oldValue value)))
      (rescaleThumb me))
    (when (> (currentValue me) value)
      (setf (currentValue me) value))))

;;; No longer changing the fillcolor here so that the user can set it.

(define-handler (setf thumbView) (value scroller)
  (let ((oldValue (thumbview me)))
    (setf (slot-value me 'thumbView) value)
    (when (and (neq lockedScroller me) (not (eq oldValue value)))
      (rescaleThumb me))))

;;; MOUSEDOWN (scrollerThumb) -- we translate the mouse location is interpreted to be the desired location of
;;;                          the thumb. We constrain this location to be in within the scroller. We then find the 
;;;                          base of the thumb from that location which we turn into the currentValue
;;;                          of the scroller.

;;; New version of this!!! The Idea: with the user's input, move the thumb (an easy sort of dragging).
;;; once this is done, set the current value to the right thing. The ONLY THING WE NEED in order to
;;; compute the new currentValue is: (1) min, (2) pixel value, (3) valid offset.
;;;
;;; NOTE: no resizing of the thumb is required!!!

(define-handler mousedown (scrollerThumb)
  (call-next-method)
  (let* ((theScroller (container me))
         (orientation (orientation theScroller))
         (theBody (scrollerBody theScroller)))
    ;; Got it! Loop getting the valid offset and setting the currentValue!
    (while-scrolling theScroller
      (if (eq orientation 'vertical)
        (let* ((theTop (+ (top theBody :physical t) (truncate (height me) 2)))
               (theBottom (- (bottom theBody :physical t) (truncate (height me) 2)))
               (curhh (h me :physical t))
               (curvv (v me :physical t))
               (vvoffset (- (cadr (mouseloc stage)) curvv)))
          (withcursor standardCursor
            (loop
              (when (up mouse) (return))
              (sk8-multival-bind (hh vv) (mouseloc stage)
                (declare (ignore hh))
                (setf vv (- vv vvoffset))
                (unless (<= vv theBottom)
                  (setf vv theBottom))
                (unless (>= vv theTop)
                  (setf vv theTop))
                ;; Move the thumb and set the currentValue.
                (unless (eql vv curvv) 
                  (setLocation me curhh vv :physical t)
                  (setf (currentValue theScroller) (ThumbPositionToCurrentValue theScroller)))
                (setf curvv vv)))))
        (let* ((TheLeft (+ (left theBody :physical t) (truncate (width me) 2)))
               (theRight (- (right theBody :physical t) (truncate (width me) 2)))
               (curhh (h me :physical t))
               (curvv (v me :physical t))
               (hhoffset (- (car (mouseloc stage)) curhh)))
          (withcursor standardCursor
            (loop
              (when (up mouse) (return))
              (sk8-multival-bind (hh vv) (mouseloc stage)
                (declare (ignore vv))
                (setf hh (- hh hhoffset))
                (unless (<= hh theRight)
                  (setf hh theRight))
                (unless (>= hh TheLeft)
                  (setf hh TheLeft))
                ;; Move the thumb and set the currentValue.
                (unless (eql hh curhh) 
                  (setLocation me hh curvv :physical t)
                  (setf (currentValue theScroller) (ThumbPositionToCurrentValue theScroller)))
                (setf curhh hh)))))
        ))))

;;; MOUSEDOWN (scrollerBody) -- scrolls by page.

(define-handler mouseDown (scrollerBody)
  (let* ((container (container me))
         (thumb (thumb container))
         (orientation (orientation container))
         (step (pageStep container))
         tLeft tTop tRight tBottom)
    (boundsRect me :physical t)
    (withCursor standardCursor
      (while-scrolling container
        (loop (unless (mouse-down-p) (return))
              (SK8-multival-setf (tLeft tTop tRight tBottom) (boundsRect thumb :physical t))
              (if (eq orientation 'horizontal)
                (if (< (eventH) tleft)
                  (decf (currentValue container) step)
                  (when (> (eventH) tRight) 
                    (incf (currentValue container) step)))
                (if (< (eventV) ttop)
                  (decf (currentValue container) step)
                  (when (> (eventV) tBottom) 
                    (incf (currentValue container) step))))
              (update container))))))

;;; Returns the renderer to be used for the pressed scroller arrow. The unpressed
;;; versions are stored in the fillcolor.

(define-handler arrowPressedRenderer (scroller &optional (whichArrow 'down))
  (case whichArrow
    (down scrollerDownPressedRenderer)
    (up scrollerUpPressedRenderer)
    (left scrollerLeftPressedRenderer)
    (right scrollerRightPressedRenderer)))

(define-handler orientation (scrollerUpArrow)
  (let ((theScroller (container me)))
    (if (eq (orientation theScroller) 'vertical)
      'up
      'left)))

(define-handler orientation (scrollerDownArrow)
  (let ((theScroller (container me)))
    (if (eq (orientation theScroller) 'vertical)
      'down
      'right)))

;;; MOUSEDOWN (scrollerUpArrow)

(define-handler mouseDown (scrollerUpArrow)
  (call-next-method)
  (let* ((container (container me))
         (scrollStep (scrollStep container))
         (oldColor (fillcolor me))
         (newFillcolor (arrowPressedRenderer container (orientation me))))
    (unwind-protect
      (progn
        (setf (fillcolor me) newFillColor)
        ;; ok, now loop
        (withCursor standardCursor
          (while-scrolling container
            ;; Note that the fillcolor changes WITHIN the loop to show the user that
            ;; the mouse has left the hot area.
            (loop (if (eq me (findCurrentEventActor))
                    (progn (unless (eq (fillcolor me) newFillcolor)
                             (setf (fillcolor me) newFillcolor))
                           ;; Decrease the currentValue.
                           (decf (currentValue container) scrollStep)
                           (update container))
                    (unless (eq (fillcolor me) oldColor)
                      (setf (fillcolor me) oldColor)))
                  (unless (mouse-down-p) (return))
                  ))))
      (setf (fillcolor me) oldColor))))
   
;;; MOUSEDOWN (scrollerDownArrow)

(define-handler mouseDown (scrollerDownArrow)
  (call-next-method)
  (let* ((container (container me))
         (scrollStep (scrollStep container))
         (oldColor (fillcolor me))
         (newFillcolor (arrowPressedRenderer container (orientation me))))
    (unwind-protect
      (progn
        (setf (fillcolor me) newFillColor)
        ;; ok, now loop
        (withCursor standardCursor
          (while-scrolling container
            ;; Note that the fillcolor changes WITHIN the loop to show the user that
            ;; the mouse has left the hot area.
            (loop (if (eq me (findCurrentEventActor))
                    (progn (unless (eq (fillcolor me) newFillcolor)
                             (setf (fillcolor me) newFillcolor))
                           ;; Increase the currentValue.
                           (incf (currentValue container) scrollStep)
                           (update container))
                    (unless (eq (fillcolor me) oldColor)
                      (setf (fillcolor me) oldColor)))
                  (unless (mouse-down-p) (return))
                  )))
          (setf (fillcolor me) oldColor)))))

(define-handler setScrollerVals (scroller &key 
                                             minVal
                                             maxVal
                                             curVal
                                             (thumbView (thumbView me) supplied)
                                             scrollstep
                                             pagestep
                                             (finalRedraw t))
  (while-scrolling me
    (when supplied (setf (thumbView me) thumbView))
    (when minVal (setf (minimumValue me) minVal))
    (when maxVal (setf (maximumValue me) maxVal))
    (when curVal (setf (currentValue me) curVal))
    (when pageStep (setf (pageStep me) pageStep))
    (when scrollStep (setf (scrollStep me) scrollStep)))
  ;; And rescale the thumb!
  (when finalRedraw (rescaleThumb me)))

(defun set-partnerScroller (me myProperty myScrollerProperty newScroller)
  (let ((inPort (findPort me myProperty 'input))
        (outPort (findPort me myProperty 'output)))
    
    ;; Remove the old scroller.
    (when outPort
      (let ((scroInPort (find 'currentValue (wiredTo outPort) :key #'portProperty :test #'eq)))
        (when scroInPort
          (let* ((oldScroller (portObject scroInPort))
                 (scroOutPort (findPort oldScroller 'currentValue 'output)))
            ;; [1] Unwire the existing connections (do the tests just in case some ports are not there!)
            (when (and outPort scroInPort)
              (unwirePorts outPort scroInPort))
            (when (and scroOutPort inPort)
              (unwirePorts scroOutPort inPort))))))
    
    ;; Add the new one when necessary.
    (when newScroller
      ;; [1] Set up the ports.
      (unless inPort (setq inPort (addInputPort me myProperty :prototype SafeInputPort)))
      (unless outPort (setq outPort (addOutputPort me myProperty :prototype SafeOutputPort)))
      (let ((scroInPort (or (findPort newScroller 'currentValue 'input)
                            (addInputPort newScroller 'currentValue :prototype SafeInputPort)))
            (scroOutPort (or (findPort newScroller 'currentValue 'output)
                             (addOutputPort newScroller 'currentValue :prototype SafeOutputPort))))
        (wirePorts outPort scroInPort)
        (wirePorts scroOutPort inPort))
      ;; [2] Update the scroller.
      (updatePartnerScroller me newScroller))
    
    ;; Install the new scroller (or False) in the slot.
    (when myScrollerProperty (setValue myScrollerProperty me newScroller))
    
    newScroller))

#|
	Change History (most recent last):
	2	5/28/93	false	The thumb does not revert to white anymore, 
				and the scroller arrows now call the handler
				getArrorRenderer to toggle their fillcolor when
				pressed. But there's more! The arrows now
				inherit from scrollerArrow.
	8	9/17/93	hernan	Getting the scroller to behave properly for small
				ranges. The problem is with using the arrows to
				navigate the space...
				In the process fixed all sorts of other things.
	9	9/17/93	hernan	I'll just fix it next week.
	10	9/22/93	hernan	Redid a lot of the scroller so that there is only one
				procedure to recompute the location, size of the
				thumb and the pixelValue. Everyone calls these 
				procedures now.
	11	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	12	10/4/93	kleiman	Fixing undefined symbol.
	13	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	14	11/12/93	hernan	Made all mousedown methods in the file call the
				next method to propagate the event.
	15	11/24/93	hernan	Fixing bestsize of the thumb to resize itself
				correctly in all cases.
	16	11/30/93	hernan	Added the minimumThumbSize property and set it
				to 5 for all scrollers. This is enforced in bestsize of
				scrollerThumb.
	17	11/30/93	hernan	Initializing the minimumThumbSize to 5.
	18	12/17/93	hernan	Obsoleting real-currentValue.
	19	12/20/93	rod	Added slider object.  This is a child of scroller. 
				Needed to add a new property, maxThumbSize, 
				and cull out the notion of "Thickness" in order to
				make it work.  The results are nice and simple.
				Everything is inherited.  -Brian
	20	12/21/93	rod	Rewrote mousedown of scroller thumb to be a lot
				simpler and not use track mousedown.  Rather
				it just repeats until the mouse is up making sure
				the location of the mouse is equal to the location
				of the thumb (minus the initial offset).
	21	1/11/94	hernan	self -> me
	22	1/14/94	hernan	Now using ports to talk to scrollees.
	23	2/12/94	kleiman	renaming
	24	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	25	2/25/94	hernan	Using symbols instead of keywords for options!!!
	26	2/26/94	rod	
	27	2/28/94	hernan	Avoiding disposing things directly.
	28	3/3/94	Hernan	The great handler argument name renaming of 94!
	29	3/6/94	Hernan	Obsoleted acceptsBoundsRectOptimization.
	30	4/6/94	Hernan	Fixing bestSize of scrollerThumb to resize the
				thumb to the thickness of the scroller.
	31	4/6/94	Hernan	Creating a handler called arrowRenderers which
				returns 4 vals: the renderer to use when the up
				arrow is released and pressed and the same for
				the down arrow. This is used instead of 
				getArrowRenderer.
				Also removed that stupid code that changed the
				fillcolor of the thumb. From now on, when the 
				user sets the fillcolor of the thumb it will stay
				that way.
	32	4/14/94	sidney	can't use the arrow as an argument any more since there is now an Arrow object
	33	4/15/94	Hernan	Removing references to hand cursors.
	34	4/15/94	Hernan	Slight clean up of set currentValue.
	35	6/7/94	Hernan	Making sure the scroller scrolls at least once
				when the arrows are clicked.
	36	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	37	7/13/94	Hernan	1172695: Removing the mapvalue handler.
	38 	 8/26/94	Hernan  	Making the pixel computation functions public and
							adding WITH LOCKEDSCROLLER which calls mf::while-scrolling.
	39 	 8/31/94	Hernan  	Making public the *scrolling* variable and the
							pixel value functions to allow users to create their
							own scrollers out of ours.
	40 	 9/12/94	Hernan  	1180502: capitalizing object names.
	41 	 9/27/94	Hernan  	Moving the lockedScroller variable to the new
							2-d-baseline globals.lisp file.
	42 	12/29/94	rod     	Removing hand cursor still here.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 8/ 8/96	Hernan  	body -> scrollerBody.
	5  	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
