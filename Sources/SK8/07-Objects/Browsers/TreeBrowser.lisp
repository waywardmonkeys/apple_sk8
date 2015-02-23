;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "TREEBROWSER")

(require "RECTANGLE")
(require "SCROLLER" "objects;Widgets:Scroller")
(require "COMPLEXRGBCOLOR" "objects;Effects:ComplexRGBColor")

(SK8-declare-syms :SK8 :public ; Updated 11-29-94   5:46 pm
                  SK8::COMPACTTREE SK8::TREEVIEWER)

;;; ____________________________________________________________________________
;;;                         SK8 Tree Browser: 1. Tree Browser
;;;                             by Hernan Epelman-Wang
;;;                                Apple Computer
;;; ____________________________________________________________________________

;;; TreeViewer
;;; _________

;;; This is the simplest possible tree viewer given the time we have. Each object being browsed is
;;; represented by a rectangle in the tree. The tree is presented vertically with the root at the
;;; top.

(new rectangle  :objectName "TreeViewer"
     :properties 
     '((selectedItem :value nil)     ;; the selected node.
       (overViewer :value nil)       ;; the overviewer object for this browser.
       (relation :value knownChildren)    ;; relation to expand to next level.
       (rootItem :value nil)         ;; The Display Item for the root.
       (yedge :value nil)            ;; The bottomMost rectangle at each level.
       (spotsTaken :value nil)       ;; All the rectangles in each level.
       (extent-H-range :value (0 0)) ;; These two specify how big the 
       (extent-V-range :value (0 0)) ;;  underlying world is.
       (filter :value nil)           ;; Function used to filter the tree.
       ;; PREFERENCES!!!
       (displayItemSize :value (100 15))  ;; Default size of display items.
       (displayItemVSeparation :value 20) ;; default vertical separation of gnodes.
       )
     :project sk8)

(setf (private TreeViewer :property 'extent-v-range) t)
(setf (private TreeViewer :property 'extent-h-range) t)
(setf (private TreeViewer :property 'yedge) t)
(setf (private TreeViewer :property 'spotsTaken) t)

(setf (rootItem treeViewer) object)
(setf (relation treeViewer) 'knownChildren)

(define-handler localVirtualProperties (TreeViewer) 
  (when (eq me TreeViewer)
    '(partnerVScroller partnerHScroller)))

;;; Does not take any contents!!!

(define-handler newContentOk (TreeViewer newContent)
  (declare (ignore newContent))
  nil)

(setf (textFont TreeViewer) EspySansFont)
(setf (textSize TreeViewer) 9)

(define-handler initialize (TreeViewer original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (initialize-treeBrowser me))

(defun initialize-treeBrowser (me)
  ;; Clear the arrays.
  (setf (yedge me) (make-array 320 :element-type 'integer :initial-element 0))
  (setf (spotsTaken me) (make-array 320))
  t)

(initialize-treeBrowser treeViewer)

(setFrameSize treeViewer 2 2)
(setf (frameColor treeViewer) Black)
(setf (fillcolor treeViewer) LightGreen)

;;; Extent mainetnance.

(defun find-extent-size (theViewer)
  (let ((x-level 0)
        (spotsTaken (spotsTaken theViewer))
        (rr 99999) (tt 99999) (bb -99999)
        thisLevel)
    (if (aref spotsTaken x-level)
      (progn 
        (loop
          (setf thisLevel (aref spotsTaken x-level))
          ;; if there is nothing there, return the partial results!
          (unless thisLevel
            (return))
          ;; Update state vars...
          (when (< (second (gn-bounds (car thisLevel))) tt)
            (setf tt (second (gn-bounds (car thisLevel)))))
          (when (> (fourth (gn-bounds (car (last thisLevel)))) bb)
            (setf bb (fourth (gn-bounds (car (last thisLevel))))))
          (setf rr (third (gn-bounds (car thisLevel))))
          ;; Increment!
          (incf x-level))
        (values rr (- tt 10) bb))
      (values 0 0 0))))

(defun root-gnode (theViewer)
  (car (aref (spotsTaken theViewer) 0)))

;;; ______________________
;;; Gnodes.
;;; ______________________

(defstruct (gnode (:conc-name gn-)
                   (:print-function (lambda (p s k)
                                      (declare (ignore k))
                                      (format s "<Gnode for ~a>" (gn-objectString p)))))
  realObj             ;; The object this gnode represents.
  objectString        ;; The string with which it represents itself.
  bounds              ;; Its boundsRect in logical treeViewer coords.
  dad                 ;; Its parent gnode (in the tree).
  kids                ;; Its subnodes.
  newx                
  newy
  locationvalid       
  justALeaf)          ;; Whether this node is a leaf.

(defun create-tbDisplayItem (obj tb)
  (let ((item (make-gnode)))
    (setf (gn-realobj item) obj)
    (setf (gn-objectString item) (objectString obj))
    ;; Set the style according to branchness...
    (if (get-gnode-kids tb obj)
      (setf (gn-justALeaf item) nil)
      (setf (gn-justALeaf item) t))
    item))

(defun gn-location (gnode)
  (let ((theRect (gn-bounds gnode)))
    (sk8-multivals (/ (+ (first theRect) (third theRect)) 2)
                   (/ (+ (second theRect) (fourth theRect)) 2))))

;;; ________________
;;; Scrollers
;;; ________________

(define-handler partnerScroller (TreeViewer orientation)
  (if (eq orientation 'horizontal)
    (let* ((theInPort (findPort me 'hOrigin 'input))
           theOutPort)
      (when theInport
        (setf theOutPort (car (wiredTo theInPort)))
        (when theOutPort (portObject theOutPort))))
    (let* ((theInPort (findPort me 'vOrigin 'input))
           theOutPort)
      (when theInport
        (setf theOutPort (car (wiredTo theInPort)))
        (when theOutPort (portObject theOutPort))))))

(define-handler updatepartnerScroller (treeViewer theScroller)
  (let (start end curVal view)
    (if (eq (orientation theScroller) 'horizontal)
      (setf start (car (extent-H-range me))
            end (cadr (extent-H-range me))
            curVal (hOrigin me)
            view (width me))
      (setf start (car (extent-V-range me))
            end (cadr (extent-V-range me))
            curVal (vOrigin me)
            view (height me)))
    ;; Now set the slots and tell the scroller to resize itself!
    (setScrollerVals theScroller
                     :minVal start
                     :maxVal end
                     :curval curVal
                     :thumbview view
                     :pageStep view
                     :scrollStep 20)
    ;; (rescaleThumb theScroller)
    ))

(define-handler updateScrollers (treeViewer)
  (multiple-value-bind (right top bottom) (find-extent-size me)
    (setf (extent-H-range me) (list 0 right)
          (extent-V-range me) (list top bottom))
    ;; And now update the scrollers!
    (when (partnerScroller me 'horizontal)
      (updatepartnerScroller me (partnerScroller me 'horizontal)))
    (when (partnerScroller me 'vertical)
      (updatepartnerScroller me (partnerScroller me 'vertical)))))

(defun unwire-treeViewer-ports (me orientation)
  ;; Disconnect the scroller.
  (let (scrollerGetter scrollerProperty)
    (if (eq orientation 'vertical)
      (setf scrollerGetter 'vertical
            scrollerProperty 'vOrigin)
      (setf scrollerGetter 'horizontal
            scrollerProperty 'hOrigin))
    (when (partnerScroller me scrollerGetter)
      (let ((theInputPort (findPort me scrollerProperty 'input))
            (theoutputPort (findPort (partnerScroller me scrollerGetter) 'currentValue 'output)))
        (when (and theinputPort theoutputPort)
          (unwirePorts theoutputPort theinputPort))))))
      
;;; Output port on currentValue -> Input port on hOrigin

(define-handler (setf partnerScroller) (theScroller TreeViewer &key (orientation 'vertical))
  (let (scrollerProperty)
    (if (eq orientation 'vertical)
      (setf scrollerProperty 'vOrigin)
      (setf scrollerProperty 'hOrigin))
    (if theScroller
      (progn
        (unwire-treeviewer-ports me orientation)
        (let ((theInPort (findPort me scrollerProperty 'input :resultEnsured t))
              (theOutPort (findPort theScroller 'currentValue 'output :resultEnsured t)))
          (wirePorts theOutPort theInPort))
        (updateScrollers me))
      (unwire-treeviewer-ports me orientation))))
  
(define-handler partnerHScroller (TreeViewer)
  (partnerScroller me 'horizontal))

(define-handler (setf partnerHScroller) (theScroller TreeViewer)
  (setf (partnerScroller me :orientation 'horizontal) theScroller))

(define-handler partnerVScroller (TreeViewer)
  (partnerScroller me 'vertical))

(define-handler (setf partnerVScroller) (theScroller TreeViewer)
  (setf (partnerScroller me :orientation 'vertical) theScroller))

;;; ________________
;;; Drawing... 
;;; ________________

;;; This is a list of the form (startLevel EndLevel visibleTop visibleBottom).
;;; It is used to determine whether a gnode gets drawn or not.

(defvar *gnodes-drawn* (list nil nil nil nil))

(defun gnode-drawn (gnode)
  ;; Either not on the right level or its top and bottom do not
  ;; fit within the visible bounds.
  (let* ((gnodeBounds (gn-bounds gnode))
         (gnodeLevel (x-to-level (car gnodeBounds))))
    (and (<= (car *gnodes-drawn*) gnodeLevel (cadr *gnodes-drawn*))
         (or (<= (third *gnodes-drawn*) (cadr gnodeBounds) (fourth *gnodes-drawn*))
             (<= (third *gnodes-drawn*) (fourth gnodeBounds) (fourth *gnodes-drawn*))))))

;;; This function is called after the backgroundRenderer has been drawn.
;;; Need to draw the tree taking into account the origin of the actor and its
;;; location relative to its window. Should already be clipped to the right thing.

(defun draw-tree (theViewer region)
  (declare (ignore region))
  (let* ((windowBounds (maskBoundsRect (gs:node-fillRegion theViewer)))
         (windowLeftOffset (car windowBounds))
         (windowTopOffset (cadr windowBounds)))
    (sk8-multival-bind (visibleLeft visibleTop) (origin theViewer)
      (sk8-multival-bind (width height) (size theViewer)
        (let* ((visibleRight (+ visibleLeft width))
               (visibleBottom (+ visibleTop height))
               (startLevel (x-to-level visibleLeft))
               (endLevel (x-to-level visibleRight))
               (takenSpots (spotsTaken theViewer)))
          (setf (car *gnodes-drawn*) startLevel
                (cadr *gnodes-drawn*) endLevel
                (third *gnodes-drawn*) visibleTop
                (fourth *gnodes-drawn*) visibleBottom)
          ;; Run through the spotsTaken array for the levels selected.
          (do* ((curLevel startLevel (1+ curLevel))
                (levelNodes (aref takenSpots curLevel) (aref takenSpots curLevel)))
               ((> curLevel endLevel))
            (unless (zerop (length levelNodes))
              (draw-one-level theViewer levelNodes visibleTop visibleBottom 
                              windowLeftOffset windowTopOffset))))))))

;;; Draws each level in the tree. First search until we find the first visible node.
;;; Then we draw until we hit the first invisible node.

(defun draw-one-level (theViewer levelNodes visibleTop visibleBottom windowLeftOffset windowTopOffset)        
  ;; Search for first node to draw.
  (let ((curNode levelNodes)
        bounds)
    (loop
      (unless curNode (return-from draw-one-level))
      (setf bounds (gn-bounds (car curNode)))
      (when (or (>= (cadr bounds) visibleTop)
                (>= (fourth bounds) visibleTop))
        (return))
      (setf curNode (cdr curNode)))
    ;; If we got here we have the first node to draw.
    (dolist (theNode curNode)
      (if (<= (cadr (gn-bounds theNode)) visibleBottom)
        (draw-gNode theViewer theNode windowLeftOffset windowTopOffset)
        (return-from draw-one-level nil)))))
      
;;; h and v are the origin of the viewer.

(defun draw-connections (theGnode h v)
  ;; Draw the connections.
  (let* ((bounds (gn-bounds theGnode))
         (connectionV (truncate (- (internal-v theGnode) v)))
         (dadPoint (- (car bounds) h))
         (childPoint (- (third bounds) h))
         (dad (gn-dad theGnode))
         (kids (gn-kids theGnode)))
    (when (and dad (not (gnode-drawn dad)))
      (#_moveTo dadPoint connectionV)
      (#_lineTo (gs:f.round (- (third (gn-bounds dad)) h)) (gs:f.round (- (internal-v dad) v))))
    ;; When the kids are off the screen, you need to draw them!
    ;; This will happen when the kids' level is too large.
    (dolist (aChild kids)
      (#_moveTo childPoint connectionV)
      (#_lineTo 
       (- (car (gn-bounds aChild)) h) 
       (- (truncate (+ (cadr (gn-bounds aChild)) (fourth (gn-bounds aChild))) 2) v)))
    ))

(defun draw-gNode (theViewer theGnode windowLeftOffset windowTopOffset)
  (gs:let+ ((bounds (gn-bounds theGnode))
         (tempRgn (:region)))
    ;; Offset by origin.
    (sk8-multival-bind (h v) (origin theViewer)
      (setf h (gs:f.round (- h windowLeftOffset)) v (gs:f.round (- v windowTopOffset)))
      (setf bounds (list (gs:f.round (- (first bounds) h)) (gs:f.round (- (second bounds) v))
                         (gs:f.round (- (third bounds) h)) (gs:f.round (- (fourth bounds) v))))
      (with-rgb (boxColor (mcl-color Black))
        (#_RGBForeColor boxColor))
      (draw-connections theGnode h v))
    ;; Draw the box.
    (if (gn-justALeaf theGnode)
      (#_penSize 1 1)
      (#_penSize 2 2))
    (rlet ((r :rect :left (car bounds) :top (cadr bounds)
              :right (third bounds) :bottom (fourth bounds)))
      (#_eraseRect r)
      (#_frameRect r)
      (#_penNormal)
      (#_insetRect r 2 2)
      (#_rectRgn tempRgn r)
      (gs:with-composed-clip tempRgn
        ;; Draw the text.
        (with-rgb (textColor (mcl-color (textColor theViewer)))
          (#_RGBForeColor textColor))
        (#_textFont (fontData (textFont theViewer)))
        (#_textSize (textSize theViewer))
        ;; 2 pixels above the bottom left.
        (#_moveTo (+ (car bounds) 4) (- (fourth bounds) 4))
        (drawstring Pen (gn-objectString theGNode))
        (#_pennormal))
      ;; If this is the selected node, invert it.
      (when (eq theGnode (getValue 'selectedItem theViewer))
        (#_insetRect r 1 1)
        (#_invertRect r))
      )))
  
;;; ______________________
;;; And the draw function.
;;; ______________________


(defun TreeViewer-draw (theViewer theport draw-region)
  (declare (ignore thePort))
  (let ((flags (gs:node-flags theViewer))
        (fill (gs:node-fillRegion theViewer)))
    ;; Recomputing the regions on demand if necessary.
    (gs:recompute-frame-region theViewer flags)
    (gs:recompute-fill-region theViewer flags)
    ;; Rendering the frame and the fill.
    (when (or (gs:frameToDirtyRegion? flags)
              (gs:regions-Intersect (gs:node-frameRegion theViewer) draw-region))
      (render (gs:node-frameColor theViewer) theViewer (gs:node-frameRegion theViewer) port))
    (render (gs:node-fillColor theViewer) theViewer fill port)
    ;; Now draw the tree.
    (gs:with-composed-clip fill
      (draw-tree theViewer draw-region))
    (when (and (gs:hilited? flags) (gs:inverted? flags))
      (gs:recompute-bounds-region theViewer flags)
      (#_invertRgn (gs:node-boundsRegion theViewer)))))

;;; And install the draw function.

(setf (gs:node-drawFunction TreeViewer) 'TreeViewer-draw)

;;; _________________________
;;; Selection and Mouse Behaviour...
;;; _________________________

(defun find-gnode-internal (root theObj)
  (cond ((null root) nil)
        ((eq (gn-realobj root) theObj) (throw :foundObject root))
        (t (dolist (c (gn-kids root))
             (find-gnode-internal c theObj)))))

(defun find-gnode (tb theObj)
  (catch :foundObject
    (find-gnode-internal (root-gnode tb) theObj)))

(define-handler selectedItem (treeViewer)
  (let ((selectedGnode (getValue 'selectedItem me)))
    (when selectedGnode
      (gn-realobj selectedGnode))))

;;; The selected item should be something that is currently being displayed
;;; in the tree.

(define-handler (setf selectedItem) (newValue TreeViewer)
  (let ((theGnode (find-gnode me newValue)))
    (if theGnode
      (progn
        (setValue 'selectedItem me theGnode)
        (centerOnItem me theGnode))
      (sk8-error generalProgrammaticError
                 :strings '("" " is not in the tree.")
                 :objects (list newValue)))))

;;; Uses the location of the mouse to find the gnode that was hit.

(defun point-on-which-gnode (theViewer &optional h v)
  (unless (and h v)
    (sk8-multival-setf (h v) (mouseLoc theViewer)))
  (let ((level (x-to-level h))
        levelNodes bounds)
    (unless (minusp level)
      (setf levelNodes (aref (spotsTaken theViewer) level))
      ;; Check that the h coords are right.
      (when (and levelNodes
                 (<= (car (gn-bounds (car levelNodes))) h (caddr (gn-bounds (car levelNodes)))))
        ;; Find the gnode that contains the required v.
        ;; (Would do better with a binary search...)
        (loop
          (when (null levelNodes) (return))
          (setf bounds (gn-bounds (car levelNodes)))
          (when (<= (cadr bounds) v (fourth bounds))
            (return-from point-on-which-gnode (car levelNodes)))
          (setf levelNodes (cdr levelNodes)))))))

(define-handler pointOnWhichPart (TreeViewer x y &key part)
  (if (eq part 'item)
    (sk8-multival-bind (h v) (physicalToLogical me x y)
      (let ((theGnode (point-on-which-gnode me h v)))
        (when theGnode (gn-realobj theGnode))))
    (call-next-method)))

(define-handler mouseDown (TreeViewer)
  (let ((theGnode (point-on-which-gnode me)))
    (when theGnode
      (setValue 'selectedItem me theGnode)
      (lightForceRedraw me))))

(defmacro preserving-gnode-origin (theViewer gnode &body body)
  (let* ((tbBounds (gensym))
         (itemBounds (gensym))
         (dh (gensym))
         (dv (gensym)))
    `(let* ((,tbBounds (boundsRect ,theViewer :physical t))
            (,itemBounds (logicalToPhysicalList ,theViewer (gn-bounds ,gnode)))
            (,dh (- (car ,itemBounds) (car ,tbBounds)))
            (,dv (- (cadr ,itemBounds) (cadr ,tbBounds))))
       (prog1
         (progn ,@body)
         ;; Fix the origin to preserve the offset.
         (setf ,itembounds (gn-bounds ,gnode))
         (setOrigin ,theViewer (- (car ,itemBounds) ,dh) (- (cadr ,itemBounds) ,dv)
                    :forceRedraw t)))))

(define-handler expandSelection (TreeViewer)
  (let ((theGnode (getValue 'selectedItem me)))
    (when theGnode
      (withactorLocked (me)
        (preserving-gnode-origin me theGnode
          (plant-new-children me theGnode)
          (updateScrollers me)
          (when (overviewer me)
            (update (overviewer me))))))))

(define-handler collapseSelection (TreeViewer)
  (let ((theGnode (getvalue 'selectedItem me)))
    (when theGnode
      (withactorLocked (me)
        (preserving-gnode-origin me theGnode
          (dolist (subTree (gn-kids theGnode))
            (remove-tree-from-taken-spots me subTree))
          (setf (gn-kids theGnode) nil)
          (updateScrollers me)
          (when (overviewer me)
            (update (overviewer me)))
          )))))

(define-handler doubleClick (TreeViewer)
  (let ((theGnode (getvalue 'selectedItem me)))
    (when theGnode
      (if (gn-kids theGnode)
            (collapseSelection me)
        (expandSelection me)
        ))))

;;; _________________________
;;; Full Tree Drawing
;;; _________________________

(define-handler drawTree (TreeViewer &key depth)
  (withActorLocked (me)
    (staticDisplay me (rootItem me) depth)
    (updateScrollers me)
    (when (overviewer me)
      (update (overviewer me)))))

;;; Redraws the current tree using the optimal space.

(defun collect-objects (gnode)
  (declare (special filter))
  (pushnew (gn-realobj gnode) filter)
  (dolist (c (gn-kids gnode))
    (collect-objects c)))

(define-handler compactTree (TreeViewer)
  (let ((filter nil)
        (oldFilter (filter me)))
    (declare (special filter))
    (when (root-gnode me) (collect-objects (root-gnode me)))
    (unwind-protect
      (progn 
        (setf (filter me) filter)
        (drawTree me))
      (setf (filter me) oldFilter))))

;;; _______________________________ 
;;; Associating an overviewer.
;;; _______________________________ 

;;; Associates the treeBrowser to its overviewer.

(define-handler (setf overviewer) (newValue TreeViewer)
  (let ((oldOverviewer (overviewer me)))
    (when oldOverviewer
      (setf (treeViewer oldOverviewer) nil))
    (setValue 'overviewer me newValue)
    (when newValue
      (setf (treeViewer newValue) me))))

#|

(setf (container treeViewer) Stage)
(setf (container treeViewer) nil)

(staticDisplay TreeViewer Object 2)
(lightForceRedraw TreeViewer)

(sk8-multival-bind (h v) (origin TreeViewer)
  (declare (ignore h))
  (dotimes (i 40)
    (setf v (- v 10))
    (setOrigin TreeViewer 0 v :forceRedraw t))
  (dotimes (i 40)
    (setf v (+ v 10))
    (setOrigin TreeViewer 0 v :forceRedraw t)))

(setOrigin TreeViewer 0 407 :forceRedraw t)
(setOrigin TreeViewer 0 0 :forceRedraw t)

(new rectangle :objectname "pipo" :container Stage :project sk8)
(setf (container treeViewer) pipo)
(setLocation treeViewer 100 100)

(new scroller :objectname "Scro" :container pipo :project sk8)
(setf (partnerScroller treeViewer) scro)

(updateScrollers treeViewer)

(new scroller :objectname "HScro" :container pipo :project sk8)
(setf (partnerScroller treeViewer :orientation 'horizontal) HScro)

;;; More trees...

(setf (relation treeViewer) 'contents)
(setf (rootItem treeViewer) ui::infowindow)
(drawTree treeViewer)

(setf (relation treeViewer) 'knownChildren)
(setf (rootItem treeViewer) sk8::Error)
(drawTree treeViewer)


(setf (relation treeViewer) 'knownChildren)
(setf (rootItem treeViewer) object)
(drawTree treeViewer :depth 3)

(setf (relation treeViewer) 'knownChildren)
(setf (rootItem treeViewer) Renderer)
(drawTree treeViewer :depth 2)

(new treeOverviewer :objectname "tito" :project sk8)
(setf (container tito) pipo)

(setf (overviewer treeViewer) tito)
(setf (overviewer treeViewer) nil)

;;; TO DO:

1. Add compact tree.
2. Fix problem of unseen connections...
3. Fix preserving-gnode-origin to inform scroller of change in origin from tree itself

|#

#|
	Change History (most recent last):
	1  	11/23/94	Hernan  	New file. A new tree browser that does not use
							real objects for the graphical nodes.
	2  	11/30/94	Hernan  	Making it work with the overviewer.
	3  	11/30/94	Hernan  	Moving setOrigin to the overviewer file.
	4  	12/ 2/94	Hernan  	Ooops... fixing args to initialize.
	5  	12/ 2/94	Hernan  	Making selectedItem return to the user the item
							that the selected gnode represents.
	6  	12/ 2/94	Hernan  	Ooops. Silly typo.
	7  	12/ 7/94	Hernan  	Adding functionality for set selection.
	8  	 2/13/95	Hernan  	Adding partnerVScroller and partnerHScroller.
	9  	 2/16/95	sidney  	readable argument names for initialize handler
|# ;(do not edit past this line!!)

;;; ____________________________________________________________________________
;;;                         SK8 Tree Browser: 2. Dynamic Layout
;;; ____________________________________________________________________________

;;; This file contains the code that implements the MVL dynamic viewer.
;;; This code makes thorough use of the code in the viewer.lisp file.
;;; It is organized as follows:

;;; 0. Global variables.
;;; 1. Advicing the necessary MVL functions.
;;; 2. The Dynamic Viewer
;;;    2.1 Initialization
;;;    2.2 The Main Function
;;;    2.3 Planting A New Subtree
;;;    2.4 Utilities
;;;        2.4.1 Positioning Utilities
;;;        2.4.2 Tree Shifting Utilities
;;;        2.4.3 Information Gathering Utilities (search, etc)...
;;;        2.4.4 Book Keeping Utilities
;;;        2.4.5 Dealing with User Requests During Dynamic Viewing
;;;    2.5 Updating the Overviewer
;;;    2.6 Showing Truth Value Changes
;;;    2.7 Static Display During Dynamic Viewing

;;; 2.2 The Main Function
;;;     -----------------

;;; This is called to show the children of gnode (when the user doubleClicks on it).

(defun plant-new-children (tb gnode)
  (let* ((newChildren (get-gnode-kids tb (gn-realobj gnode))))
    (when newChildren
      (dynamic-plant-and-reshape tb newChildren gnode))))

;;; 2.3 Planting a new Subtree
;;;     ----------------------

;;; dynamic-plant-and-reshape. This function is called with the roots of all
;;;                       the new subtrees to be added to our gnode tree.

;;; The first problem we encounter is that the roots we got (the children
;;; of the planted ancestor) might not be visible in the gnode tree. If only
;;; inference nodes are visible, then the planted ancestor is an inference
;;; node but its children are not. Thus they are not visible. The real roots
;;; are the first inference children that descend from them. Thus, the
;;; first thing we do is reset the root tasks to the real root tasks. These
;;; we obtain by calling find-visible-gnodes on the original root tasks.

;;; We find the parent-gnode and the y level of the parent and we are ready
;;; to start. 

;;; For each new root R we do the following. The xedge is cleared. Then
;;; we call grow-the-tree which returns a gnode-tree rooted at R. This 
;;; creates all the gnodes for the tree. But grow-the-tree works assuming
;;; the tree it creates is the only thing there is. Our next task is to
;;; shift this new tree so that it is placed below its parent gnode. This
;;; is done by calling plant-new-root. Adding the tree might cause collisions,
;;; and the next step is to correct them. After all this we sort the gnodes
;;; by x position.

;;; Our previous work has given a position to each new node but we have not
;;; yet used this position to set the locations of all the components of the
;;; gnode. We call anchor-completely to do this. Then we correct the location
;;; of the parent of the new tree so that it lies above its kids at the
;;; average of their position.

;;; And we are done. We call redraw-everything to show the changes and 
;;; we call clean the mess to deal with user requests
;;; that we could not attend to during the work.

(defun dynamic-plant-and-reshape (tb new-Children dads-gnode)
  (let ((*dirty-parents* nil)
        x-level root-gnode new-gnodes)
    (declare (special *dirty-parents*))
    (if dads-gnode
      (setf x-level (x-to-level (internal-h dads-gnode)))
      (setf x-level -1))
    ;; Position Each Root
    (dolist (new-root new-children)
      (clear-yedge tb)
      (setf root-gnode (grow-the-tree tb new-root dads-gnode (1+ x-level) 1))
      (push root-gnode new-gnodes)
      (enroll-tree-in-taken-spots tb root-gnode)
      (plant-new-root tb root-gnode dads-gnode (1+ x-level))
      (when dads-gnode 
        (check-and-correct-collisions tb root-gnode (1+ x-level))))
    (when dads-gnode
      (propagate-locations-to-immediate-parent tb dads-gnode))
    ;; And now, really move the gnodes!
    (withActorLocked (tb)
      (move-dirty-gnodes tb (root-gnode tb)))))

;;; plant-new-root. This function is called when a new subtree has been
;;;                 created in order to put this subtree below its
;;; planted ancestor. We do this in three steps: we put the new tree
;;; below its planted ancestor, we correct any collisions we might have
;;; with left siblings, and the we try to move all siblings so that they
;;; stay centered around their parent. This last step, if possible, avoids
;;; the tree from growing towards the right all the time.

;;; We find the x position of the parent. If this position is greater than
;;; the position of the new root, then we are done if we increase the x
;;; position of the root (and all its children) by the difference between
;;; the parent's position and its own. On the other hand, if the new root
;;; is located to the right of the parent, then this can be corrected by
;;; shifting the new root to the left. But, by the way grow-the-tree works,
;;; the leftmost node of the new tree is at the left edge of the extent, and
;;; therefore shifting it to the left would push it out of the world. So, the
;;; solution in this case is to shift the whole tree until the new subtree 
;;; can be placed below its planted ancestor without being shifted to the left.

;;; But once we place the new tree below its planted ancestor, we might have
;;; a collision with the older siblings (the ones to the left). So, we find
;;; out if any collisions exist and if so shift the new tree to the right as
;;; mush as is necessary to avoid them.

;;; Since new siblings are added to the right of old siblings, the tree
;;; rooted at the parent will grow to the right all the time. But, if
;;; there is space to the left we can move all the siblings to the left,
;;; thus giving the impression that the tree grows from the center to the
;;; outside. We call shift-kids-to-left to attempt this.

(defun plant-new-root (tb root-gnode parent-gnode x-level)
  (when parent-gnode
    (setf *top-edge* nil)
    (setf *bottom-edge* nil)
    (let* ((root-pos (find-root-position tb parent-gnode))
           (dpos (- root-pos (internal-v root-gnode)))
           (*gnode-separation* (displayItemVSeparation tb))
           dy top-edge siblings-to-the-top bottom-edge all-sibs)
      ;; Placing The New Tree Below Its Planted Ancestor
      (when (minusp dpos)
        (setf dy (max 1000 (+ *gnode-separation* (- dpos)))) ;; (max 1000 (+ *gnode-separation* (- dpos))))
        (setf all-sibs (gn-kids parent-gnode))
        (setf (gn-kids parent-gnode) (remove root-gnode all-sibs))
        (shift-subtree-by-dy tb (car (aref (spotsTaken tb) 0)) x-level dy)
        ;; (setf *viewer-extent-width* (+ *viewer-extent-width* dx))
        (setf (gn-kids parent-gnode) all-sibs)
        (setf dpos (- (+ root-pos dy) (internal-v root-gnode))))
      (shift-subtree-by-dy tb root-gnode x-level dpos)
      ;; Shifting New Tree Away From Left Sibs
      (setf top-edge (compile-top-edge root-gnode x-level)
            siblings-to-the-top (find-siblings-to-the-top tb root-gnode x-level)
            bottom-edge (compile-bottom-edge-for-kids 
                         (reverse siblings-to-the-top) x-level)
            dy (worst-collision tb bottom-edge top-edge))
      (unless (zerop dy)
        (shift-subtree-by-dy tb root-gnode x-level (+ *gnode-separation* dy)))
      ;; Centering Siblings
      (shift-kids-to-top tb parent-gnode))))

;;; shift-kids-to-left. When this function is called, a subtree has just
;;;                     been added to parent, to the right of all the
;;; previous subtrees. This function is called to move all the subtrees
;;; of parent to the left so that the parent is located at the center of
;;; all its children.

;;; We first determine how big the required shift will be and whether
;;; shifting by this amount is possible. If it is, we shift each subtree
;;; and we are done.

;;; The shift is possible when two conditions are met:
;;; 1. The trees to be shifted do not collide with other trees as a
;;;    result of the shift and,
;;; 2. No node belonging to the trees to be shifted gets moved to a
;;;    negative location as a result of the shift (this is tested by
;;;    the predicate shift-plausible).

(defun shift-kids-to-top (tb parent)
  (let* ((kids (gn-kids parent))
         (kid-level (x-to-level (internal-h (car kids))))
         (bad-root-pos (avg-kids kids))
         (current-root-pos (internal-v parent))
         (desired-dy (- bad-root-pos current-root-pos))
         top-edge bottom-edge generation)
    (unless (zerop desired-dy)
      (setf *top-edge* nil
            *bottom-edge* nil)
      (setf top-edge (add-to-edge (compile-top-edge-for-kids kids kid-level)
                                  (- desired-dy))
            generation (aref (spotsTaken tb) kid-level)
            bottom-edge (compile-bottom-edge-for-kids 
                         (reverse
                          (subseq generation 
                                  0
                                  (position (car (last kids)) generation)))
                         kid-level))
      (when (and (shift-plausible top-edge desired-dy)
                 (zerop (worst-collision tb bottom-edge top-edge)))
        (dolist (child kids)
          (shift-subtree-by-dy tb child kid-level (- desired-dy)))))))

;;; check-and-correct-collisions. This function is called when the new 
;;;                               subtree has been planted below its
;;; parent, at the right of the older siblings. The new subtree might
;;; have collided with other trees to the right of it. This function
;;; corrects these collisions by shifting the trees to the right away
;;; from the new subtree (ie more to the right).

;;; This is quite easy. We compile the right edge of the new subtree and
;;; the left edge of all subtrees rooted at all the nodes to the right
;;; of the new root. We find out the amount to shift by. If it is non
;;; zero we shift each subtree to the right by the required amount. Each
;;; time we shift a tree we correct the location of its parent so that
;;; it stays immediately above the child.

;;; At the end, we call propagate-locations-to-top on all the trees shifted
;;; to deal with ancestors that have been left very far away from its their
;;; children.

(defun check-and-correct-collisions (tb root x-level)
  (when (gn-dad root)
    (let ((siblings-to-the-bottom 
           (find-siblings-to-the-bottom tb root x-level))
          (dy (find-collision-magnitude tb root x-level)))
      (unless (zerop dy)
        (dolist (sib siblings-to-the-bottom)
          (shift-subtree-by-dy tb sib x-level dy) 
          (propagate-locations-to-immediate-parent tb (gn-dad sib))
          (unless (collisions-persist tb sib x-level)
            (return-from check-and-correct-collisions)))
        (propagate-locations-to-left tb siblings-to-the-bottom)))))

;;; propagate-locations-to-top. This function is called when the trees rooted
;;;                             at the gnodes gnodes have been shifted to the
;;; right. As a result of this shift, all the ancestors of these roots are
;;; no longer placed to be at the center of their children's positions. This
;;; function corrects this situation by working bottom up changing the
;;; locations of the ancestors to the right thing. The effect is a small shift
;;; of all ancestors to the right.

;;; Since shifting the whole path to the root too often is unnecessary (since
;;; the displacement might be too small to bother the user), for each 
;;; ancestor only relocate it if a threshold displacement is exceeded. For
;;; this we call relocate-gnode-if-necessary.

;;; The loop in this function builds a list which is the union of all the
;;; immediate parents of gnodes (duplicates removed). We recurse on this
;;; list which gets smaller and smaller as we go up the tree.

(defun propagate-locations-to-left (tb gnodes)
  (cond ((null gnodes) nil)
        (t (let (parent-gnodes)
             (dolist (gnode gnodes)
               (unless (or (null (gn-dad gnode))
                           (member (gn-dad gnode) parent-gnodes))
                 (push (gn-dad gnode) parent-gnodes)
                 (relocate-gnode-if-necessary tb (car parent-gnodes))))
             (propagate-locations-to-left parent-gnodes)))))

;;; propagate-locations-to-immediate-parent. Shifts dad so that it is located
;;;                                          at the center of its offspring.
;;; Then it calls propagate-if-necessary to shift the ancestors (only if they
;;; are too far off from the correct position).

(defun propagate-locations-to-immediate-parent (tb dad)
  (when dad (relocate-gnode tb dad))
  (propagate-if-necessary tb (gn-dad dad)))

;;; propagate-if-necessary. Shifts ancestors so that they lie at the center
;;;                         of their children, but only if the shift is
;;; necessary. So, for each node on the path from gnode to the root of the
;;; tree we call relocate-gnode-if-necessary to do this.

(defun propagate-if-necessary (tb gnode)  
  (cond ((null gnode) nil)
        (t (relocate-gnode-if-necessary tb gnode)
           (propagate-if-necessary tb (gn-dad gnode)))))

(defmacro shift-gnode-anchoring-by (gnode dy)
  `(progn (setf (gn-newY ,gnode) (+ (gn-newY ,gnode) ,dy))
          (setf (gn-locationValid ,gnode) nil)))

;;; relocate-gnode-if-necessary. Shifts gnode so that it is at the center of
;;;                              its children's positions only if necessary.
;;; The shift is necessary if gnode is very far away from its correct 
;;; position (3 *gnode-separation*s).

;;; We find the x position where gnode should be and compare it to the
;;; position it is in. If the required displacement is larger than the
;;; threshold, we shift the subtree rooted at gnode by the required amount,
;;; and check and correct any collisions we might have created. When we are
;;; done with this node we recurse on its parent.

(defun relocate-gnode-if-necessary (tb gnode)
  (let ((kids (gn-kids gnode))
        dy)
    (if (= 1 (length kids))
      (setf dy (- (internal-v (car kids)) (internal-v gnode)))
      (setf dy (- (avg-kids kids) (internal-v gnode))))
    (when (> dy (* 3 (displayItemVSeparation tb)))
      (shift-gnode-anchoring-by gnode dy)
      (check-and-correct-collisions tb gnode (x-to-level (internal-h gnode))))))

;;; relocate-gnode. Shifts gnode so that it is placed at the center of its
;;;                 children's positions. This function is the same as
;;; relocate-gnode-if-necessary except that here gnode is relocated even
;;; if it is very close from where it should be.

(defun relocate-gnode (tb gnode)
  (let ((kids (gn-kids gnode))
        dy)
    (if (= 1 (length kids))
      (setf dy (- (internal-v (car kids)) (internal-v gnode)))
      (setf dy (- (avg-kids kids) (internal-v gnode))))
    (unless (zerop dy)
      (shift-gnode-anchoring-by gnode dy)
      (check-and-correct-collisions tb gnode (x-to-level (internal-h gnode))))))

;;; 2.4 Utilities
;;;     ---------

;;; 2.4.1 Positioning Utilities. Functions used by the functions above to
;;;       ---------------------  determine where to place subtrees.

;;; add-to-edge. Given an edge structure (a list of (level xpos) pairs),
;;;              this function adds dx to the xpos in each pair. By
;;; doing this we can test the effects of shifting the tree whose edge
;;; we are processing by dx.

(defun add-to-edge (edge dy)
  (cond ((null edge) nil)
        (t (cons (list (caar edge) (+ (cadar edge) dy))
                 (add-to-edge (cdr edge) dy)))))

;;; shift-plausible. A shift is plausible if it does not put any gnode
;;;                  to close to the left edge of the extent (x < 200).

(defun shift-plausible (edge dy)
  (cond ((null edge) t)
        ((< (cadar edge) 0) nil)
        (t (shift-plausible (cdr edge) dy))))

;;; worst-collision. Returns the amount to shift the tree by 
;;;                  to avoid collisions to the left. 0 is returned 
;;; if no collisions are encountered. The assumption is that the
;;; tree whose left edge we are processing might be colliding with
;;; some tree to its left (right is the right edge of all the trees
;;; to the left of the crasher).

(defun worst-collision (tb bottom top)
  (let ((largest-crash 0)
        bpos tpos)
    (setf bottom (reverse bottom))
    (setf top (reverse top))
    (loop
      (when (or (null bottom) (null top))
        (return-from worst-collision largest-crash))
      (setf bpos (cadr (pop bottom)))
      (setf tpos (cadr (pop top)))
      (when (> (+ bpos (displayItemVSeparation tb)) tpos)
        (setf largest-crash 
              (max largest-crash (- (+ bpos (displayItemVSeparation tb)) tpos)))))))

;;; collisions-persist. Returns t if the tree rooted at root is in a
;;;                     collision state (nodes are position on top of
;;; each other). We find the size of the worst collision. If it is positive
;;; then we know a collision occured and we return t.

(defun collisions-persist (tb root x-level) 
  (plusp (find-collision-magnitude tb root x-level)))

;;; find-collision-magnitude. Returns the size of the worst collision
;;;                           between the tree rooted at root and all
;;; subtrees to the right of it.

(defun find-collision-magnitude (tb root x-level)
  (setf *top-edge* nil)
  (setf *bottom-edge* nil)
  (let* ((bottom (compile-bottom-edge root x-level))
         (siblings-to-the-bottom 
          (find-siblings-to-the-bottom tb root x-level))
         (top (compile-top-edge-for-kids (reverse siblings-to-the-bottom) x-level)))
    (worst-collision tb bottom top)))

;;; 2.4.3 Information Gathering Utilities. Used to find and test lots of
;;;       -------------------------------  things...

;;; find-root-position. Returns the position of a new subtree whose root
;;;                     is a child of parent. If parent has only one kid
;;; (the new root), its position is right below the parent. If there are
;;; more siblings, the new root goes to the right of the last one. Since
;;; kids are stored in reverse order of appeareance (youngest first), the
;;; first kid is the new root and the second is its predecessor (the last
;;; planted child). We get that node's position and add *gnode-separation*.

(defun find-root-position (tb parent)
  (let ((kids (cdr (gn-kids parent))))
    (if kids
      (+ (displayItemVSeparation tb) (internal-v (car kids)))
      (internal-v parent))))

;;; find-siblings-to-the-right. Returns all gnodes that are to the right of
;;;                             gnode at level y-level. Since we have the
;;; *viewer-spots-taken* array, this is easy: we find gnode in its level
;;; and return all nodes that are after it.

(defun find-siblings-to-the-bottom (tb gnode x-level)
  (let* ((taken-positions (aref (spotsTaken tb) x-level))
         (pos (1+ (position gnode taken-positions))))
    (subseq taken-positions pos (length taken-positions))))

;;; find-siblings-to-the-left. Returns all gnodes that are to the left of
;;;                            gnode at level y-level. Since we have the
;;; *viewer-spots-taken* array, this is easy: we find gnode in its level
;;; and return all nodes that are before it.

(defun find-siblings-to-the-top (tb gnode x-level)
  (let* ((taken-positions (aref (spotsTaken tb) x-level))
         (pos (position gnode taken-positions)))
    (subseq taken-positions 0 pos)))

#|

????????????????????????

;;; find-planted-siblings-to-the-right. Returns all the subtrees to the
;;;                                     right and below root which have
;;; already been planted. This is done because we do not want to deal with
;;; subtrees that we might be ready to plant after the current one. We will
;;; deal with the collisions of these as they get planted.

(defun find-planted-siblings-to-the-bottom (tb root x-level)
  (let ((sibs (find-siblings-to-the-right tb root x-level)))
    (remove-if #'(lambda (x) (null (gn-prop-pos x))) sibs)))
|#
;;; 2.4.4 Book Keeping Utilities. These functions deal with maintaining the
;;;       ----------------------  data structures the dynamic viewer uses.
;;;                               The array *viewer-spots-taken* is the most
;;;                               important.

;;; clear-xedge. This function sets the variable *viewer-xedge* to a clean
;;;              xedge. This is required to allow grow-the-tree to make
;;; each new tree as if it was the only one in existance.

(defun clear-yedge (tb)
  (setf (yedge tb) (make-array 320 :element-type 'integer :initial-element 0)))

;;;  enroll-tree-in-taken-spots. Enrolls all the gnodes of the tree rooted
;;;                              at root in the *viewer-spots-taken* array.
;;; Just calls enroll-in-taken-spots for all nodes in the tree.

(defun enroll-tree-in-taken-spots (tb root)
  (enroll-in-taken-spots tb root)
  (dolist (child (reverse (gn-kids root)))
    (enroll-tree-in-taken-spots tb child)))

;;; enroll-in-taken-spots. Adds gnode to the *viewer-spots-taken* array at
;;;                        the appropriate level and position. There are a
;;; number of cases to deal with:

;;; 1. If gnode is the root it is easy. We list gnode and put it in level
;;;    0 of the array.
;;; 2. If the node is not the root, we check to see if it is not the first
;;;    child. If so, it fits right after its previous sibling and we call
;;;    insert-in-taken-positions to do it.
;;; 3. If the node is an only child, we know that it fits before all its
;;;    cousins who are children of uncles to the left of its parent. So we
;;;    call find-spot-by-ancestry to deal with it.

;;; When done we check whether we have placed a node outside of the extent and
;;; take appropriate actions.

(defun enroll-in-taken-spots (tb gnode)
  (let* (;; (y (gn-newy gnode))
         (x (gn-newx gnode))
         (x-level (x-to-level x))
         (taken-positions (copy-list (aref (spotsTaken tb) x-level)))
         (sibs (when (gn-dad gnode) (gn-kids (gn-dad gnode))))
         (pos (position gnode sibs))
         (prev-sib (if (and pos (not (= pos (1- (length sibs)))))
                     (elt sibs (1+ pos))
                     nil)))
    (if (gn-dad gnode)
      (setf (aref (spotsTaken tb) x-level)
            (if prev-sib
              (insert-in-taken-positions taken-positions gnode prev-sib)
              (find-spot-by-ancestry tb gnode x-level)))
      (setf (aref (spotsTaken tb) x-level) (list gnode)))
    ;;(check-extent-limits x y)
    ))

;;; find-spot-by-ancestry. Returns the entry of *viewer-spots-taken*
;;;                        for the level of gnode with gnode inserted in it.
;;; This is called when gnode is an only child. This function, thus, will
;;; place gnode after all its cousins which are children of uncles which
;;; lie to the left of the parent of gnode.

;;; It loops through all the nodes to its left until it finds one whose
;;; dad is not a left uncle. At that point it calls insert-in-taken-positions
;;; to insert the gnode in the list. If we reach the end of the nodes and
;;; they all are left cousins, we call insert-at-end to place the gnode at
;;; the end of the list.

(defun find-spot-by-ancestry (tb gnode x-level)
  (let* ((taken-positions (copy-list (aref (spotsTaken tb) x-level)))
         (dad (gn-dad gnode))
         (above-taken-positions (aref (spotsTaken tb) (1- x-level)))
         (pos (position dad above-taken-positions))
         (top-uncles (subseq above-taken-positions 0 pos))
         (my-pos 0))
    (dolist (sib taken-positions (insert-at-end gnode taken-positions))
      (when (not (member (gn-dad sib) top-uncles))
        (return-from find-spot-by-ancestry
          (insert-in-taken-positions taken-positions gnode 
                                     (if (zerop my-pos) 
                                       nil 
                                       (elt taken-positions (1- my-pos))))))
      (incf my-pos))))

;;; insert-at-end. Inserts gnode at the end of the list taken-positions.

(defun insert-at-end (gnode taken-positions)
  (insert-in-taken-positions taken-positions 
                             gnode 
                             (car (last taken-positions))))

;;; insert-in-taken-positions. Inserts gnode right after prev-sib. 
;;;                            If prev sib is null it inserts gnode
;;; at the head of the list.

(defun insert-in-taken-positions (taken-positions gnode prev-sib)
  (if prev-sib
    (append 
     (subseq taken-positions 0 (1+ (position prev-sib taken-positions)))
     (cons gnode 
           (nthcdr (1+ (position prev-sib taken-positions))
                   taken-positions)))
    (cons gnode taken-positions)))

;;; remove-tree-from-taken-spots. Removes all gnodes in root from the taken spots array.
;;;                          Used for collapsing trees.

(defun remove-tree-from-taken-spots (tb root)
  (remove-from-taken-spots tb root)
  (dolist (child (gn-kids root))
    (remove-tree-from-taken-spots tb child)))

(defun remove-from-taken-spots (tb root)
  (let ((takenSpots (spotsTaken tb))
        (x-level (x-to-level (internal-h root))))
    (setf (aref takenSpots x-level)
          (remove root (aref takenSpots x-level)))))


#|
;;; check-extent-limits. This function is called to check whether a gnode is
;;;                      being placed outside of the extent. If so, the extent
;;; is resized. Since the overviewer shows the whole extent, it needs to be
;;; repainted so that all nodes can be shown.

(defun check-extent-limits (x y)
  (let ((no-change t)
        (beyond-height (< y (- 32000 *viewer-extent-height*)))
        (beyond-width (> x *viewer-extent-width*)))
    (when beyond-height
      (setf *viewer-extent-height* (+ *viewer-extent-height* 2000))
      (setf no-change nil))
    (when beyond-width
      (setf *viewer-extent-width* (+ *viewer-extent-width* 3000))
      (setf no-change nil))
    (when (or beyond-height beyond-width)
      (update-overviewer))))

;;; 2.7 Static Display During Dynamic Viewing
;;;     -------------------------------------

;;; static-display-during-dynamic-viewing. This function is called when the
;;;                                user decides the dynamic display
;;; has become too messy and requests a perfect arrangement of all the nodes
;;; we have so far. The cursor becomes a clock to show the user that nothing
;;; can be done during this period.

(defun static-display-during-dynamic-viewing ()
  (let* ((root-gnode (car (aref *viewer-spots-taken* 0)))
	 (task (gn-task root-gnode)))
    (dynamic-unview)
    (update-mouse-cursor 'busy)
    (lock-viewer)
      (init-viewer-during-dynamic-viewing)
      (plant-and-reshape task)
      (setf *viewer-spots-taken* (make-array 320 :initial-element nil))
      (enroll-tree-in-taken-spots (prop-find-gnode *viewer-root-prop*))
      (reset-selected-list)
      (when *gnodes*
	(unless *dynamic-viewer-active* 
	  (resize-downstairs-world))
	(init-overviewer-during-dynamic-viewing)
	(shrink-forest (prop-find-gnode *viewer-root-prop*)))
    (unlock-viewer)
    (dynamic-view)
    (setf *viewer-current-root* (find-gnode (gn-task *viewer-current-root*)))
    (if *dynamic-viewer-active*
	(overviewer-draw-rect #'cw::draw-rectangle)
      (overviewer-draw-rect #'cw::draw-filled-rectangle))
    (update-mouse-cursor 'active)))

;;; init-viewer-during-dynamic-viewing. Reinitializes the data structures
;;;                                     the static display function needs
;;; in order to work.

(defun init-viewer-during-dynamic-viewing ()
 (let* ((width (car *viewer-size*))
	(height (cadr *viewer-size*))
	(bottom (- 32000 height))
	(left 0))
    (setf (cw::window-stream-extent-width *viewer*) 32000)
    (setf (cw::window-stream-extent-height *viewer*) 32000)
    (setf (cw::window-stream-y-offset *viewer*) bottom)
    (setf *viewer-region* (cw::make-region :bottom bottom
				       :left left
				       :width width
				       :height height))
    (setf *gnodes* nil)
    (setf *gprops* (make-hash-table :test #'equal))
    (setf *visible-gnodes* nil)
    (setf *collisions-occured* nil)
    (setf *viewer-xedge*  (make-array 320 :element-type 'integer
				      :initial-element 100))))

|#
#|
	Change History (most recent last):
	8		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	9		2/21/94	hernan	window -> sk8::window.
	10 	11/23/94	Hernan  	Updating to work with new treeViewer.
	11 	11/30/94	Hernan  	More tree browser fixes.
|# ;(do not edit past this line!!)

;;; ____________________________________________________________________________
;;;                            SK8 Tree Browser: 3. Layout
;;; ____________________________________________________________________________

;;; MVL VIEWER CODE
;;; ===============

;;; Globals
;;; _____

(defvar *dirty-parents*)

;;; This variable is set to t when a gnode is drawn in an x position
;;; to the left of the current edge at the appropriate level (it is
;;; being drawn on top of already existing gnodes).

(defvar *collisions-occured* nil)

;;; This variable is used to compute the x coordinates of the left
;;; edge of a tree (the left edge is made of the left most leaf of
;;; each level). We use it to see how far a tree can be shifted to
;;; the right without hitting the next tree.

(defvar *top-edge* nil)

;;; This variable is used to compute the x coordinates of the right
;;; edge of a tree (the right edge is made of the right most leaf of
;;; each level). We use it to see how far a tree can be shifted to
;;; the right without hitting the next tree.

(defvar *bottom-edge* nil)

;;; The horizontal minimum separation of gnodes. The center of two
;;; gnodes on the same level will be at least 250 pixels away.

(defparameter *gnode-separation* 20)

;;; This variable is used to control how big a spread between
;;; sibling gnodes we can tolerate. The value of this variable is
;;; multiplied by *gnode-separation* and if two siblings are further
;;; than the result, we try to get the closer together. This is
;;; used to decide when shifting a sibling to the right is required.

(defparameter *gnode-tolerable-spread* 1)

;;; Utilities and Helpers
;;; _______________

;;; This is where we make the filter work...

(defun get-gnode-kids (tb dad)
  (multiple-value-bind (children error?)
                       (ignore-errors (funcall (relation tb) dad))
    (if error?
      nil
      ;; Use the filter!
      (let ((filter (filter tb))
            result)
        (cond ((null filter) children)
              ((listp filter) 
               ;; Only allow things in the list!
               (dolist (c children (reverse result))
                 (when (memq c filter)
                   (push c result))))
              (t ;; A Symbol: a function to be funcalled on each child!
               (dolist (c children (reverse result))
                 (when (funcall filter c)
                   (push c result)))))))))

(defun internal-location (gnode)
  (if (gn-locationValid gnode)
    (gn-location gnode)
    (sk8-multivals (gn-newx gnode) (gn-newy gnode))))

(defun internal-h (gnode)
  (if (gn-locationValid gnode)
    (sk8-multival-bind (h v) (gn-location gnode)
      (declare (ignore v))
      h)
    (gn-newx gnode)))

(defun internal-v (gnode)
  (if (gn-locationValid gnode)
    (sk8-multival-bind (h v) (gn-location gnode)
      (declare (ignore h))
      v)
    (gn-newy gnode)))

(defun internal-setLocation (gnode x y)
  (setf (gn-newx gnode) x)
  (setf (gn-newy gnode) y)
  (setf (gn-locationValid gnode) nil))

(defun updateBounds (gNode tb)
  (let* ((x (gn-newx gnode))
         (y (gn-newy gnode))
         (gnodeSize (displayItemSize tb))
         (left (- x (truncate (car gnodeSize) 2)))
         (top (- y (truncate (cadr gnodeSize) 2))))
    (setf (gn-bounds gnode) (list left top (+ left (car gnodeSize)) (+ top (cadr gnodeSize))))))

;;; Should be called with the treeBrowser locked!!!

(defun move-dirty-gnodes (tb rootGnode)
  (declare (special *dirty-parents*))
  (unless (gn-locationValid rootGnode)
    (setf (gn-locationValid rootGnode) t)
    ;; Now update the boundsRect.
    (updateBounds rootGnode tb)
    (when (gn-dad rootGnode)
      (pushNew (gn-dad rootGnode) *dirty-parents*))
    (when (gn-kids rootGnode)
      (pushNew rootGnode *dirty-parents*)))
  (dolist (child (gn-kids rootGnode))
    (move-dirty-gnodes tb child)))

;;; rootOnItem -- changes the origin so that item is at the center right of the tree viewer.
;;;            Note that tb is not the browser but the treeViewer.

(defun rootOnItem (tb item)
  (let (newXorigin newYorigin)
    (sk8-multival-bind (x y) (gn-location item)
      ;; calculate the new origin.
      (setf newYorigin (- y (/ (height tb) 2)))
      (setf newXOrigin (- x 60)))
    (withActorLocked (tb)
      (setOrigin tb newXOrigin newYOrigin :forceRedraw nil))))

(defun centerOnItem (tb item)
  (let (newXorigin newYorigin)
    (SK8-multival-bind (x y) (gn-location item)
      ;; calculate the new origin.
      (setf newYorigin (- y (/ (height tb) 2)))
      (setf newXOrigin (- x (/ (width tb) 2))))
    (withActorLocked (tb)
      (setOrigin tb newXOrigin newYOrigin :forceRedraw nil)
      (updateScrollers tb))))

;;; Mess with the treeViewer's so that the item is dh from hOrigin and 
;;; dv from vOrigin.

(defun restoreItemPosition (tb item dh dv)
  (sk8-multival-bind (h v) (gn-location item)
    (setOrigin tb (- h dh) (- v dv) :forceRedraw nil)))

;;; xedge. Given a depth, it returns the next available position at that
;;;        depth in extent coordinates.

(defmacro yedgeLine (tb level)
  `(aref (yedge ,tb) ,level))

;;; avg-kids. Given a list of children, returns the position where their
;;;           parent should be placed. This position is the average
;;; of the positions of the first and last sibling.

(defun avg-kids (siblings)
  (SK8-multival-bind (x1 y1) (internal-location (car siblings))
    (declare (ignore x1))
    (SK8-multival-bind (x2 y2) (internal-location (car (last siblings)))
      (declare (ignore x2))
      (/ (+ y1 y2) 2))))

;;; level-to-y. Given a depth in the tree, this function returns the y
;;;             coordinate at which nodes of this level are drawn in the
;;; extent.

(defun level-to-x (level)
  (+ 60 (* level 150)))

;;; y-to-level. The inverse of level-to-y.

(defun x-to-level (x)
  (gs:f.round (/ (- x 60) 150)))

;;; find-worst-crash. Given a list of the levels and the amounts of
;;;                   the collisions we return the worst one (the one
;;; with the biggest miss).

(defun find-worst-crash (all-collisions)
  (car (sort all-collisions #'> :key #'car)))

;;; generic-plant. After a gnode has been planted at position x and level
;;;                y-level, this function is called to reset the next
;;; available position at y-level and to set the variable *collisions-
;;; occured* if necessary. Note that a collision has occured when the
;;; x coordinate of the new gnode is *less* than the next available
;;; position at the desired level.

(defun generic-plant (tb y x-level)
  (when (> (+ (displayItemVSeparation tb) (yedgeLine tb x-level)) y)
    (push (list (- (yedgeLine tb x-level) y) x-level) *collisions-occured*))
  (when (> y (yedgeLine tb x-level))
    (setf (aref (yedge tb) x-level) y)))

;;; plant-a-branch. This function is used to plant a gnode that is not a
;;;                 leaf. This is done *after* its children have been
;;; planted and so, the position of the parent is the average of the
;;; positions of its first and last child. Once we have the x coordinate
;;; we call generic plant to do book keeping.

(defun plant-a-branch (tb gnode x-level)
  (let ((x (level-to-x x-level))
        (y (avg-kids (gn-kids gnode))))
    (Internal-setLocation gnode x y)
    (generic-plant tb y x-level)))

;;; plant-a-leaf. This function is called to plant a gnode which is a leaf.
;;;               We plant it at the next available position which is
;;; given by looking at the xedge array. Once we have the x coordinate
;;; we call generic plant to do book keeping.

(defun plant-a-leaf (tb gnode x-level)
  (let ((x (level-to-x x-level))
        (y (+ (yedgeLine tb x-level) (displayItemVSeparation tb))))
    (internal-setLocation gnode x y)
    (generic-plant tb y x-level)))

(defun plant-a-what (tb gnode x-level)
  (if (gn-kids gnode)
    (plant-a-branch tb gnode x-level)
    (plant-a-leaf tb gnode x-level)))

;;; find-dx. Given lists representing the right edge of the tree we want to
;;;          shift (sibling), and the left edge of the tree we have already
;;; shifted (shifter), we find the distance between the edges of the trees
;;; at each level and pick the smallest of these. We return this value which
;;; is the amount by which we can safely shift the sibling to the right.

(defun find-dy (tb top bottom)
  (let ((safe-dy 32000)
        (*gnode-separation* (displayItemVSeparation tb))
        tpos bpos)
    (setf top (reverse top))
    (setf bottom (reverse bottom))
    (loop
      (when (or (null top) (null bottom))
        (return-from find-dy safe-dy))
      (setf bpos (cadr (pop bottom)))
      (setf tpos (cadr (pop top)))
      (setf safe-dy (min safe-dy
                         (- tpos (+ bpos *gnode-separation*)))))))

;;; compile-left-edge-for-kids. This function calls compile-left-edge for
;;;                             each gnode in the list children. Notice
;;; that we reverse the kids in order that the first kid represents the
;;; leftmost tree (gn-kids holds the children in reverse planting order).
;;; This function was defined (as opposed to being an inline section of
;;; compile-left-edge) because it is needed by shift-left-siblings to
;;; compile the left edge of the subtrees rooted at the already planted
;;; siblings as if it were just one tree.

(defun compile-top-edge-for-kids (children level)
  (dolist (kid (reverse children) *top-edge*)
    (compile-top-edge kid level)))

;;; compile-left-edge. This functions builds a list which holds the
;;;                    x coordinates of the lefmost node at each level
;;; of the tree rooted at gnode. We use the global variable *left-edge*.
;;; Our strategy is to just record the x coordinate of the *first* node
;;; we find at each level.

(defun compile-top-edge (gnode level)
  (unless (assoc level *top-edge*)
    (SK8-multival-bind (x y) (internal-location gnode)
      (declare (ignore x))
      (push (list level y) *top-edge*)))
  (compile-top-edge-for-kids (gn-kids gnode) (1+ level)))

;;; compile-right-edge. This functions builds a list which holds the
;;;                     x coordinates of the rightmost node at each level
;;; of the tree rooted at gnode. We use the global variable *right-edge*.
;;; Our strategy is to just record the x coordinate of the *first* node
;;; we find at each level. Notice that now, since we do not reverse
;;; gn-kids, the first node we find is the rightmost one.

(defun compile-bottom-edge (gnode level)
  (unless (assoc level *bottom-edge*)
    (SK8-multival-bind (x y) (internal-location gnode)
      (declare (ignore x))
      (push (list level y) *bottom-edge*)))
  (compile-bottom-edge-for-kids (gn-kids gnode) (1+ level)))

;;; Note that the kids should be sorted so that the first one is the
;;; right most one.

(defun compile-bottom-edge-for-kids (children level)
  (dolist (kid children *bottom-edge*)
    (compile-bottom-edge kid level)))

;;; shift-subtree-by-dx. This function shifts the subtree rooted at root
;;;                      dx pixels to the right.

(defun shift-subtree-by-dy (tb root x-level dy)
  (SK8-multival-bind (x y) (internal-location root)
    (let ((new-y (+ y dy)))
      (internal-setLocation root x new-y)
      (when (> new-y (yedgeLine tb x-level))
        (setf (aref (yedge tb) x-level) new-y))
      (dolist (child (gn-kids root))
        (shift-subtree-by-dy tb child (1+ x-level) dy)))))

;;; shift-tree-by-dx. When we plant a tree and find that collisions have
;;;                   occured, this function is called to shift the tree
;;; to the right to correct the situation. The parameter all-collisions
;;; holds a list of the levels at which collisions occured and the severity
;;; of the collisions (by how much we missed). We pick the worst collision
;;; and shift the whole tree by its amount.

(defun shift-tree-by-dy (tb all-collisions gnode x-level)
  (when gnode
    (let* ((worst-crash (find-worst-crash all-collisions))
           (dy (+ (displayItemVSeparation tb) (car worst-crash))))
      (shift-subtree-by-dy tb gnode x-level dy)
      (setf *collisions-occured* nil))))

;;; Adopt -- a parent and child gnodes get linked.

(defun adopt (gnode parent-gnode)
  (setf (gn-dad gnode) parent-gnode)
  (setf (gn-kids parent-gnode) (push gnode (gn-kids parent-gnode))))

;;; The Static Display Functions
;;; _____________________

;;; shift-left-siblings. When we shift a tree rooted at gnode to the right
;;;                      due to collisions, the siblings of the shifted
;;; gnode might be left too far away to the left and this may not be 
;;; necessary because the collisions might have been caused by a tree not
;;; rooted at any of them. Thus, this function is called to try to bring
;;; the siblings back together by shifting the already planted ones (the
;;; ones to the left of the current gnode) to the right as much as possible.
;;; We work recursively starting with the sibling closest to the current
;;; gnode and ending with the first sibling planted.

;;; To see how much we can shift each one we compute two lists: the x
;;; coordinates of the rightmost node at each level of the tree rooted
;;; at the first sibling, and the x coordinates of the left most node
;;; at each level of the tree we get by joining all the subtrees rooted
;;; at the already shifted siblings. Once we have the two lists we find
;;; the distance between the edges of the tree at each level and the
;;; smallest of these differences turns out to be the amount by which we
;;; can safely shift the sibling. If this amount is 0, we don't do anything,
;;; otherwise we shift every node in the subtree by the amount.

;;; Note that at the start, already-shifted contains just the current gnode, 
;;; and siblings contains all its siblings already planted. As we go along,
;;; we shift the first sibling, remove it from the siblings list and add it 
;;; to the already-shifted list.

(defun shift-top-siblings (tb siblings x-level already-shifted)
  (cond ((null siblings) nil)
        (t 
         (setf *top-edge* nil)
         (setf *bottom-edge* nil)
         (let* ((first-sib (car siblings))
                (top (compile-top-edge-for-kids already-shifted x-level))
                (bottom (compile-bottom-edge first-sib x-level))
                (dy (find-dy tb top bottom)))
           (unless (zerop dy)
             (shift-subtree-by-dy tb first-sib x-level dy))
           (shift-top-siblings tb 
                               (cdr siblings)
                               x-level 
                               (append already-shifted (list first-sib)))))))

;;; grow-the-tree. This is the function that arranges the nodes into
;;;                a tree. It tries to build the most compact tree
;;; possible, being constrained by always placing a parent at the
;;; position which is the average of the positions of its first and
;;; last child. The algorithm traverses the tree from top to bottom
;;; but only draws when working backwards to the top (traversal is
;;; top-down and drawing is bottom-up).

;;; Note that "position" refers to the x coordinate only. The y position
;;; is no problem. Also this algorithm always respects the order of the 
;;; children of a task.

;;; The idea is to always draw the leafs at the lefmost available 
;;; position of their level. Then we work backwards, positioning
;;; the parent at the average of the positions of its first and
;;; last children. As we place the parent, we may notice that we are
;;; placing it at a position which is more to the left than the next
;;; available position at its level. At this point, we signal a
;;; collision and its magnitude (how far we are from the next
;;; available position at this level). Eventually we get back to
;;; the calling node which discovers that a collision has occured.
;;; From *collisions-occured* we can determine which was the worst
;;; crash (and by how much we missed) and we fix this by shifting the
;;; crashed subtree to the right by this amount.

;;; But, as we shift a subtree to the right, we separate it from its
;;; siblings that were already planted next to it, when it might be
;;; the case that the subtree did not crash to its siblings but to
;;; a tree further away. So, what we need to do is see if we can keep
;;; the already planted siblings to the shifting subtree.

;;; A description in pseudocode follows:

;;; grow-the-tree task parent-gnode y-level
;;;  if (task is visible) then
;;;     let gnode be the gnode created for task
;;;         children be the inference-children of task
;;;       if (the gnode is a leaf) then
;;;          plant the leaf at the next available position of level
;;;                y-level + 1
;;;       else
;;;          for each inference-child of task
;;;              grow-the-tree child gnode (1+ y-level)
;;;              correct-mistakes gnode y-level
;;;          end for
;;;          plant the gnode at the position which is the average of
;;;           the positions of its first and last children
;;;       end if
;;;  else
;;;   for each inference-child of task
;;;       grow-the-tree child parent-gnode y-level
;;;       correct-mistakes parent-gnode (1- y-level)
;;;   end for
;;;  end if
;;; end grow-the-tree

;;; where correct mistakes is:

;;;  when (collisions have occured)
;;;    shift the tree rooted at child to the left
;;;          by the necessary amount
;;;  when (the gnode just positioned is "too far away"
;;;        from it closest planted sibling)
;;;    shift the subtrees rooted at all its already
;;;          planted siblings to the right as much as
;;;          possible

(defun correct-mistakes (tb gnode x-level) 
  (when gnode
    (when *collisions-occured*
      (shift-tree-by-dy tb *collisions-occured* (car (gn-kids gnode)) (1+ x-level)))
    (let ((kids (gn-kids gnode)))
      (when (> (length kids) 1)
        ;; get x location of the recent most gnode
        (SK8-multival-bind (x1 y1) (internal-location (car kids))
          (declare (ignore x1))
          ;; get x location of its immediately older sibling.
          (SK8-multival-bind (x2 y2) (internal-location (cadr kids))
            (declare (ignore x2))
            (when (> (- y1 y2) (* *gnode-tolerable-spread* (displayItemVSeparation tb)))
              (shift-top-siblings tb (cdr kids) (1+ x-level) (list (car kids))))))))))

;;; Note: grow-the-tree now plants num-levels levels. If nothing is passed, it does the whole tree.

(defun grow-the-tree (tb obj parent-gnode x-level &optional num-levels)
  (when (or (null num-levels) (plusp num-levels))
    (let ((gnode (create-tbDisplayItem obj tb))
          children)
      (when parent-gnode (adopt gnode parent-gnode))
      (setf children (get-gnode-kids tb obj)) 
      (if (null children)
        (plant-a-leaf tb gnode x-level)
        (progn 
          (unless (listp children) (setf children (list children)))
          (dolist (child children (plant-a-what tb gnode x-level))
            (grow-the-tree tb child gnode (1+ x-level) (when num-levels (1- num-levels)))
            (correct-mistakes tb gnode x-level))))
      gnode)))

;;; StaticDisplay -- This is the main function, called to do all the work. We initialize the browser's data
;;;              structures, plant the tree (off screen), and show it (we attach all to the screen and
;;;              add the connectors).

(defun staticDisplay (tb rootObj &optional depth)
  (initialize-treeBrowser tb)
  (let ((*dirty-parents* nil)
        rootGnode)
    (declare (Special *dirty-parents*))
    (setf rootGnode (grow-the-tree tb rootObj nil 0 depth))
    ;; Clear the extent limits...
    (sk8-multival-bind (h v) (size tb)
      (setf (extent-H-Range tb) (list 0 h)
            (extent-V-Range tb) (list 0 v)))
    (enroll-tree-in-taken-spots tb rootGnode)
    ;; Really set the location of all the nodes!
    (withActorLocked (tb)
      (resetCoordinates tb)
      (move-dirty-gnodes tb rootGnode)
      (rootOnItem tb rootGnode))
    ))

#|
	Change History (most recent last):
	8	12/10/93	hernan	A number of things including making get-gnode-kids
				return the kids in the same order regardless of
				whether a filter is being used.
	9	2/12/94	kleiman	name changes
	10	2/25/94	hernan	Using symbols instead of keywords for options!!!
	11	3/4/94	kleiman	extent-h-range -> extentHRange
	12 	11/23/94	Hernan  	Updating to work with new treeViewer.
	13 	11/30/94	Hernan  	More tree browser fixes.
|# ;(do not edit past this line!!)

;;; ____________________________________________________________________________
;;;                    SK8 Tree Browser: 4. The treeBrowser overviewer.
;;; ____________________________________________________________________________

;;; Overviewer -- the overview rectangle.
;;; _________

(new Rectangle :objectName "TreeOverviewer"
     :properties '((treeViewer :value nil)
                   (HScale :value 1)
                   (Vscale :value 1))
     :project sk8)

(setf (private TreeOverviewer :property 'treeViewer) t)
(setf (private TreeOverviewer :property 'hScale) t)
(setf (private TreeOverviewer :property 'vScale) t)

(setBoundsRect TreeOverviewer 0 0 246 130)
(setf (fillcolor TreeOverviewer) (fillcolor treeViewer))
(setf (frameSize TreeOverviewer) '(2 2))

(defmacro redrawing-overviewer (tdContainer &body body)
  `(gs:making-self-dirty (,tdContainer t nil nil nil (gs:node-fillRegion ,tdContainer) :fill t)
     ,@body
     ))

(new rectangle :objectName "TreeHighlighter" :project sk8)
(setf (fillcolor TreeHighlighter) lighter)
(setf (container TreeHighlighter) TreeOverviewer)
(tagPart treeOverviewer TreeHighlighter 'TreeHighlighter)

(define-handler mousedown (treeHighLighter)
  (drag me :constrainingRect (boundsRect (container me) :physical t)))

;;; The Overviewer
;;; ____________

;;; init-overviewer -- This function sets the values of the scale slots above. We need to find the bottom and
;;;                the rightmost position of the tree. Then we find the size of the fill region of the 
;;;                overviewer, divide and set the slots.

(defun init-overviewer (ov)
  (let* ((tViewer (treeViewer ov))
         (rightEnd (+ 50 (- (cadr (extent-H-range tViewer)) (car (extent-H-range tViewer)))))
         (bottomEnd (+ 50 (- (cadr (extent-V-range tViewer)) (car (extent-V-range tViewer))))))
    (SK8-multival-bind (h v) (size ov)
      (setf (Hscale ov) (/ rightEnd h))
      (setf (Vscale ov) (/ bottomEnd v)))))

;;; scale-to-ov-x. Translates a viewer x coordinate into the corresponding
;;;                overviewer value.

(defmacro scale-to-ov-x (x ov)
  `(truncate (/ ,x (Hscale ,ov))))

;;; unscale-x. Translates an overviewer x coordinate into its corresponding
;;;            viewer coordinate.

(defmacro unscale-x (ov-x ov)
  `(* ,ov-x (Hscale ,ov)))

;;; scale-to-ov-x. Translates a viewer y coordinate into the corresponding
;;;                overviewer value.

(defmacro scale-to-ov-y (y ov)
  `(truncate (/ ,y (Vscale ,ov))))

;;; unscale-y. Translates an overviewer y coordinate into its corresponding
;;;            viewer coordinate.

(defmacro unscale-y (ov-y ov)
  `(* ,ov-y (Vscale ,ov)))

;;; This new version draws the rectangles as well!

(defun ov-draw-gnode (tOverviewer gnode thePort dadX dadY)
  (let ((vRangeStart (car (extent-V-Range (treeViewer tOverviewer))))
        (bounds (gn-bounds gnode))
        ovx ovy ovxx ovyy midy x y xx yy)
    (setf x (car bounds) y (cadr bounds) xx (third bounds) yy (fourth bounds))
    (SK8-multival-bind (ll tt rr bb) (boundsRect tOverviewer)
      (declare (ignore rr bb))
      (setf ovx (gs:f.round (+ ll (scale-to-ov-x x tOverviewer)))
            ovy (gs:f.round (+ tt (scale-to-ov-y (- y vRangeStart) tOverviewer)))
            ovxx (gs:f.round (+ ll (scale-to-ov-x xx tOverviewer)))
            ovyy (gs:f.round (+ tt (scale-to-ov-y (- yy vRangeStart) tOverviewer)))
            midy (gs:f.round (/ (+ ovy ovyy) 2)))
      (rlet ((r :rect :top ovy :left ovx :bottom ovyy :right ovxx))
        (with-port thePort
          (#_penNormal)
          (#_foreColor #$blackColor)
          (#_paintRect r)
          (if dadX
            (#_moveTo dadX dadY)
            (#_moveTo ovX midy))
          (#_lineTo ovX midy))))
    (values ovxx midy)))

;;; shrink-forest. This function draws the whole tree in the overviewer.
;;;                It does a DFS traversal.

(defun shrink-forest (tOverviewer root thePort dadX dadY)
  (let ((children (gn-kids root)))
    (multiple-value-bind (newdadX newdadY)
                         (ov-draw-gnode tOverviewer root thePort dadX dadY)
      (dolist (child children)
        (shrink-forest tOverviewer child thePort newdadX newdadY)))))

;;; static-overviewer-display. Shows in the overviewer the whole tree
;;;                            displayed in the viewer. Then it initializes
;;; the hilited region which shows what part of the tree is being shown in
;;; the viewer.

(defun static-overviewer-display (tOverviewer thePort)
  (let ((rootItem (root-gnode (treeViewer tOverviewer))))
    (when rootItem
      (shrink-forest tOverviewer rootItem thePort nil nil))))

;;; overviewer-draw -- the draw function of the overviewer object. Paints the fill and the frame and
;;;                 then draws the tree.

(defun overviewer-draw (theActor thePort draw-region)
  (gs:let+ ((flags (gs:node-flags theActor))
         (tempRgn (:region))
         (fill (gs:node-fillRegion theActor)))
    ;; Recomputing the regions on demand if necessary.
    (gs:recompute-frame-region theActor flags)
    (gs:recompute-fill-region theActor flags)
    (gs:recompute-bounds-region theActor flags)
    ;; Rendering the frame and the fill.
    (#_sectRgn (gs:node-frameRegion theActor) draw-region tempRgn)
    (unless (#_emptyRgn tempRgn)
      (render (gs:node-frameColor theActor) theActor (gs:node-frameRegion theActor) thePort))
    (#_sectRgn fill draw-region tempRgn)
    (gs:with-clipped-region temprgn
      (render (gs:node-fillColor theActor) theActor fill thePort)
      (when (treeViewer theActor)
        (static-overviewer-display theActor thePort)
        (funcall (gs:node-contentsDrawFunction theActor) theActor thePort tempRgn fill t)))))

(setf (gs:node-DrawFunction TreeOverviewer) 'overviewer-draw)

;;; HILITER functionality...

;;; init-overviewer-hilite -- inits the rectangle that shows in the overviewer the portion
;;;                     of the tree visible in the viewer.

(defun init-overviewer-hilite (overviewer)
  (let ((tHiliter (treeHighlighter overviewer))
        (tViewer (treeViewer overviewer))
        ll tt rr bb)
    (SK8-multival-bind (ox oy) (origin tViewer)
      (SK8-multival-bind (h v) (size tViewer)
        (setf ll (scale-to-ov-x ox overviewer))
        (setf tt (scale-to-ov-y oy overviewer))
        (setf rr (scale-to-ov-x (+ ox h) overviewer))
        (setf bb (scale-to-ov-y (+ oy v) overviewer))
        (setBoundsRect tHiliter ll tt rr bb)))
    (bringToFront tHiliter)))

(define-handler mouseDown (TreeOverviewer)
  (SK8-multival-bind (x y) (mouseLoc me)
    (setLocation (treeHighlighter me) x y)))

(resized treeOverviewer)

(define-handler resized (TreeOverviewer)
  (when (treeViewer me)
    (init-overviewer me)
    (init-overviewer-hilite me)))

;;; _______________________________ 
;;; Hooking up the treeViewer and its overviewer.
;;; _______________________________ 

(define-handler moved (treeHighlighter)
  (when (treeViewer (container me))
    (SK8-multival-bind (ll tt rr bb) (boundsRect me)
      (declare (ignore rr bb))
      (let* ((overviewer (container me))
             (tViewer (treeViewer overviewer))
             (realx (unscale-x ll overviewer))
             (realy (+ (unscale-y tt overviewer) (car (extent-V-Range tViewer)))))
        (setOrigin tViewer realx realy :forceRedraw nil)
        (updateScrollers tViewer)))))

;;; This needs to be here because the macro is not defined in the treeBrowser file.

(define-handler setOrigin :after
  (treeViewer xOrigin yOrigin &key (forceRedraw nil))
  (declare (ignore forceRedraw))
  (let ((theOv (overviewer me)))
    (when theOv
      (SK8-multival-bind (h v) (size me)
        (redrawing-overviewer theOv 
                              (setBoundsRect 
                               (treeHighlighter theOv)
                               (scale-to-ov-x xOrigin theOv)
                               (scale-to-ov-y yOrigin theOv)
                               (scale-to-ov-x (+ xOrigin h) theOv)
                               (scale-to-ov-y (- (+ yOrigin v) (car (extent-V-Range me))) theOv)))
        t))))

(define-handler update (treeOverviewer)
  (when (treeViewer me)
    (redrawing-overviewer me
                          (init-overviewer me)
                          (init-overviewer-hilite me)
                          )))

(define-handler (setf treeViewer) (tViewer treeOverviewer)
  (unless (eq (treeViewer me) tViewer)
    (setValue 'treeViewer me tViewer)
    (redrawing-overviewer me
                          (update me))))

#|
	Change History (most recent last):
	2		5/13/93	Hernan	The treeOverviewer is now a windoid.
	8		10/1/93	hernan	=====================================
							= Sad but true: the node is gone!!! =
							=====================================
	9		11/22/93	dy		Got rid of the node (a bit late for it!). Hernan.
	10		12/10/93	hernan	Overviewer rect drags with constrained rectangle.
	11		1/11/94	hernan	self -> me
	12		2/12/94	kleiman	name changes
	13		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	14		3/4/94	kleiman	extent-h-range -> extentHRange
	15 	11/28/94	Hernan  	Outfitting the overviewer for the new treeViewer.
	16 	11/30/94	Hernan  	Updated for b2 and the new treeViewer.
	17 	11/30/94	Hernan  	Moving setOrigin to the overviewer file.
	18 	12/ 2/94	Hernan  	Declaring some properties private.
	2  	 4/10/96	Hernan  	Removing with-fast-node-slots.
	3  	 4/10/96	Hernan  	Fixing source server wreckage...
	5  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
