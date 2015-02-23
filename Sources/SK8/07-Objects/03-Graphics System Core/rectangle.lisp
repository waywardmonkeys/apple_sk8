;;; SK8 Built-In Rectangle Actor Definition
;;;

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)
(provide "RECTANGLE")

#| Modification History

01-22-93 ruben id -> objectName
10-03-92 ruben use xs0 instead of #%
09-20-92 hernan changing this to work with MF VI
07-26-92 ruben changed to SK8 package
07-07-92 hernan added the methods that make the bounds, frame and fill region of the
                rectangle.
05-12-92 ruben d29 conversion
03-30-92 ruben began d29; swept for "get-" and "set-"
01-19-92 ruben converted to MacFrames II

|#


(new Actor 
     :undisposable t
     :objectname "Rectangle"
     :prototype t
     :project sk8)

(gs:offsetsRegions! (gs:node-flags rectangle) 0)

(setf (private Rectangle) nil) ; PUBLIC API

(define-handler minimumSize (Rectangle)
  (sk8-multivals 1 1))

;;; 1.0
;;; MakeBoundsRegion -- makes the bounds region for the rectangle. The region is saved in the
;;;                 boundsRegion field which should ALREADY have a region in it! When
;;;                 we are done we label the node's bounds as not dirty.

(define-handler makeBoundsRegion (rectangle)
  (gs:let+ ((rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me))))
    (#_rectRgn (gs:node-BoundsRegion me) rect)
    (gs:boundsDirty! (gs:node-flags me) 0)))
  
;;; 1.0
;;; makeFillRegion -- makes the fill region for the rectangle. The region is saved in the
;;;               fillRegion field which should ALREADY have a region in it! When
;;;               we are done, we label the node's fill as not dirty.

(define-handler makeFillRegion (rectangle)
  (gs:let+ ((rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me)))
            frameX frameY)
    (sk8-multival-setf (frameX frameY) (framesize me :physical t))
    (#_InsetRect rect frameX frameY)
    (#_RectRgn (gs:node-fillRegion me) rect)
    (gs:fillDirty! (gs:node-flags me) 0)))

;;; 1.0
;;; makeFrameRegion -- makes the frame region for the rectangle. The region is saved in the
;;;                 frameRegion field which should ALREADY have a region in it! When
;;;                 we are done, we label the node's frame as not dirty.
;;; 
;;; Since the only way to compute this region is to compute the bounds region and the
;;; fill region, we might as well save these in the fields for the region. This means that calling this
;;; method alone insures that the whole actor is cached properly. Therefore, if we just call this
;;; method, everything will be clean... (IS THIS TRUE FOR ALL ACTORS?)

(define-handler makeFrameRegion (rectangle)
  (let ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (gs:recompute-fill-region me flags)
    (#_diffRgn (gs:node-boundsRegion me) 
     (gs:node-fillRegion me)
     (gs:node-frameRegion me))
    (gs:frameDirty! flags 0)))
  
(setBoundsRect rectangle 0 0 60 40)

;;; STYLE -- returns the window style the actor wants to use to attach itself to the stage.

(define-handler windowStyle (rectangle)
  (getf (gs:node-properties me) :style 'blank))

;;; (SETF windowStyle) -- sets the window style the actor wants to use to attach itself to the stage
;;;              to style.

(define-handler (setf windowStyle) (style rectangle)
  (unless (eq (windowStyle me) style)
    (let ((flags (gs:node-flags me)))
      ;; Error checking...
      (unless (memq style '(blank sk8Window documentWithZoom documentWithGrow
                            document doubleEdgeBox singleEdgeBox shadowEdgeBox
                            documentNoClose documentWithZoomNoClose documentWithGrowNoClose
                            tool movableDialog floating))
        (sk8-error PropertyTypeMismatchError
                   :object        style
                   :expectedType  '(blank sk8Window documentWithZoom documentWithGrow
                                    document doubleEdgeBox singleEdgeBox shadowEdgeBox
                                    documentNoClose documentWithZoomNoClose documentWithGrowNoClose
                                    tool movableDialog floating)
                   :ownerObject   Rectangle
                   :propertyName 'windowStyle))
      ;; update the plist.
      (setf (getf (gs:node-properties me) :style) style)
      ;; if self has a window we have to change the style. We do this by attaching and
      ;; detaching the window to the stage.
      (when (gs:hasWindow? flags)
        (setf (container me) nil)
        (setf (container me) stage)))
    style))

;;; WINDOWTITLE -- returns the window title the actor wants to use to attach itself to the stage.

(define-handler windowTitle (rectangle)
  (getf (gs:node-properties me) :windowTitle (objectString me)))

;;; (SETF WINDOWTITLE) -- sets the window style the actor wants to use to attach itself to the stage
;;;                   to style.

(define-handler (setf windowTitle) (title rectangle)
  (unless (eq (windowTitle me) title)
    ;; Allowing use of nil.
    (unless title
      (setf title ""))
    (unless (stringp title)
      (sk8-error PropertyTypeMismatchError
                 :object        title
                 :expectedType  String
                 :ownerObject   Rectangle
                 :propertyName 'windowTitle
                 ))
    (let* ((flags (gs:node-flags me))
           (clos-window (and (gs:hasWindow? flags) (gs:node-window me))))
      ;; update the plist.
      (setf (getf (gs:node-properties me) :windowTitle) title)
      ;; Update the window.
      (when clos-window
        (set-window-title clos-window title)))
    title))
   
(define-handler (setf objectName) (objectName Rectangle &key force)
  (declare (ignore objectname force))
  (call-next-method)
  ;; If this actor has a window and its window-title has not been set
  ;; explicitely, change the window title of the window to the actor's new objectString.
  (when (and (gs:node-window me)  ;; this catches an uninitialized new rectangle whose flags have not been set yet
             (gs:hasWindow? (gs:node-flags me))  ;; because this test is "optimized" and may test true on garbage input in that case
             (not (getf (gs:node-properties me) :windowTitle nil)))
    (set-window-title (gs:node-window me) (objectString me))))

;; since the objectname is not actually set until the very end of initialization of an object, check if we are going to set it,
;; and if we are and there is not an explicit value given for windowTitle, then use the objectname that we are going to have

(define-handler initialize (rectangle original isnew initargs)
  (declare (ignore original isnew))
  (call-next-method)
  (let ((name (initializerargument initargs 'objectname :use nil))
        (wtitle (initializerargument initargs 'windowtitle :use nil))
        (inheritedWindowTitle (getf (gs:node-properties me) :windowtitle nil)))
    (when (and name (not wtitle) (not inheritedWindowTitle))
      (setf (windowtitle me) name))))

;;; _______________________________ 
;;; Changing parents.
;;; _______________________________ 

;;; If the thing is no longer a rectangle, and it it still on Stage,
;;; need to make new window for it.

(define-handler changeParents (rectangle newParents)
  (let ((actorParentsChanged? (check-change-in-actor-parents me newParents)))
    (if (and actorParentsChanged?
             (gs:hasWindow? (gs:node-flags me)))
      (progn
        (setf (container me) nil)
        (call-next-method)
        (when (inheritsFrom me Actor)
          (setf (container me) stage)))
      (call-next-method))))

;;; The rectangle should get rid of the junk it leaves in the properties
;;; of the actor.

(define-handler removingMeAsParent (Rectangle child newParents)
  (declare (ignore-if-unused child newParents))
  (let ((theProps (gs:node-properties child)))
    ;; Clear the windowStyle and windowTitle.
    (remf theProps :style)
    (remf theProps :windowTitle)
    (setf (gs:node-properties child) theProps))
  ;; Do the work.
  (call-next-method))

;;; _______________________________ 
;;; Editable properties.
;;; _______________________________ 

(define-handler localvirtualproperties (rectangle)
  (when (eq me rectangle)
    '(windowStyle windowTitle)))

#|
	Change History (most recent last):
	2	5/24/93	Hernan	Nothing: forgot to load menus and modified this
						file temporarily to load from here...
	3	6/11/93	rod		Set the size of the rectangle
	4	6/25/93	Hernan	The Great Renaming of 93.
	5	6/25/93	Hernan	Ooops! Replaced objectName for objectString in windowTitle.
	11	8/31/93	hernan	Fixed set windowTitle to make sure a string is
						passed for the title.
	12	10/1/93	hernan	=====================================
						= Sad but true: the node is gone!!! =
						=====================================
	13	10/1/93	hernan	Fixing thing dealing with nodes going away.
	14	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	15	10/27/93	chip	added mimimumSize handler so rectangles can be as small as 1x1 pixel
	16	11/6/93	rod		Changed the setting of the boundsrect.  That's it.
	17	1/11/94	hernan	self -> me
	18	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	19	2/25/94	hernan	Using symbols instead of keywords for options!!!
	20	3/6/94	Hernan	Obsoleted acceptsBoundsRectOptimization.
	21	4/5/94	kleiman	code review; approved for public API
	22 	 8/31/94	Hernan  	Added 'floating' window style.
	23 	 9/28/94	Hernan  	Telling rectangle not to use the offset optimization.
	24 	11/17/94	Hernan  	Defining setf objectName on Rectangle to change the window title
							to the actor's new name.
	25 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	26 	 2/ 2/95	sidney  	inspired by 1214913: set windowtitle of new rectangle from the objectname in a way that works
	27 	 2/ 3/95	Hernan  	Getting rid of obsolete comment.
	28 	 2/20/95	Hernan  	1221961: fixing initialize to use the inherited
							windowTitle if one is available.
	29 	 3/14/95	Hernan  	Adding new windowStyles that do not have a close
							box.
	30 	 4/ 3/95	sidney  	fix setf objectname to work ok when new skips initialization
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
