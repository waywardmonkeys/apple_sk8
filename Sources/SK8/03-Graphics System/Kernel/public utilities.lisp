(in-package :gs)

;;; This file contains functions that the graphics system exports for the
;;; convenience of the implementors of things like Actor.

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

02-22-93 ruben inheritsFrom? -> inheritsFrom
06-24-92 ruben relativize-pathname treats relative pathnames
05-12-92 ruben d29 conversion

|#

(defun update-event-vars ()
  (sk8-multival-bind (h v) (sk8::mouseloc sk8::stage)
    (setf *eventH* h
          *eventV* v)))

(defmacro with-temporary-port (&body body)
  (let ((thePort (gensym)))
    `(let+ ((,thePort (:cgrafport)))
       (with-port ,thePort
         ,@body))))

;;; ____________________________________________________
;;; Hilite Related stuff.  -BJR 4/15/94
;;; ____________________________________________________

(defconstant $hilite 50)

(defmacro with-hilitecolor (theport sk8Renderer &body body)
  (let ((grafvars (genSym)) (oldr (genSym)) (oldg (genSym)) (oldb (genSym)))
    `(let* ((,grafvars (hrref (rref ,theport :cgrafport.grafvars) :grafvars.rgbhilitecolor :storage :pointer))
            (,oldr (rref ,grafvars :rgbcolor.red))
            (,oldg (rref ,grafvars :rgbcolor.green))
            (,oldb (rref ,grafvars :rgbcolor.blue))
            )
       (unwind-protect
         (progn
           (when (and ,sk8renderer (neq ,sk8renderer sk8::highlighted))
             (with-rgb (rgb (sk8::mcl-color ,sk8renderer))
               (require-trap #_hiliteColor rgb)))
           ,@body)
         (with-rgb (rgb (make-color ,oldr ,oldg ,oldb))
           (require-trap #_hiliteColor rgb))))))

(defmacro |WITH HIGHLIGHTCOLOR| ((thePaper theRenderer) &body body)
  `(with-hiliteColor ,thePaper ,theRenderer
     ,@body))

;;; ____________________________________________________
;;; Font Related stuff.
;;; ____________________________________________________

(defmacro set-text-state (anActor)
  (let* ((plist (gensym))
         (theFont (gensym))
         (theSize (gensym))
         (theStyle (gensym)))
    `(let* ((,Plist (node-properties ,anActor))
            (,theFont (or (getf ,plist :text-font) (sk8::textFont ,anActor)))
            (,theStyle (or (getf ,plist :text-style) (get-style-number (sk8::textStyle ,anActor))))
            (,theSize (or (getf ,plist :text-size) (sk8::textSize ,anActor))))
       (require-trap #_TextFont (sk8::fontData ,theFont))
       (require-trap #_TextFace ,theStyle)
       (require-trap #_TextSize (f.round (f* ,theSize (node-yScale ,anActor)))))))

(defmacro restore-text-state ()  ;; OJO! None of this should be necessary!!!
  `(progn (require-trap #_textFont 0)
          (require-trap #_textFace 0)
          (require-trap #_textSize 12)))

(defun actor-line-height (theActor)
  (let+ ((thePort (:cgrafport))
         (fi (:fontInfo)))
    (without-interrupts 
     (with-port thePort
       (set-text-state theActor)
       (#_getFontInfo fi)
       (restore-text-state)
       (+ (rref fi :fontInfo.ascent) (rref fi :fontInfo.descent) (rref fi :fontInfo.leading))))))

(defmacro do-pstr-chunks ((pstrVar str) &body body)
  (LET* ((strVar (gensym))
         (len (gensym))
         (s (gensym))
         (e (gensym))
         (done? (gensym)))
    `(LET* ((,strVar ,str)
            (,len (LENGTH ,strVar))
            (,s 0)
            (,e NIL)
            (,done? NIL))
       (%VSTACK-BLOCK (,pstrVar 256)
         (LOOP
           (WHEN (>= (SETQ ,e (+ ,s 255)) ,len)
             (SETQ ,e ,len ,done? T))
           (CCL::%PSTR-SEGMENT-POINTER ,strVar ,pstrVar ,s ,e)
           (PROGN ,@body)
           (WHEN ,done? (RETURN))
           (SETQ ,s ,e))))))

(defun compute-text-width (str &optional maximumWidth)
  (let ((width 0))
    (do-pstr-chunks (pstr str)
      (incf width (#_StringWidth pstr))
      (when (and maximumWidth (>= width maximumWidth))
        (return)))
    width))


;;; This function simply returns the given string if its width is <= maximumWidth; otherwise it
;;; computes and returns the largest truncated string (with ellipsisStr glommed on) that will
;;; fit in the given maximumWidth

(defun compute-maybe-truncated-text (str maximumWidth ellipsisStr)
  (let ((width 0)
        (blockCount 0)
        (currentBlockWidth 0)
        previousBlockWidth)
    (do-pstr-chunks (pstr str)
      (setq previousBlockWidth currentBlockWidth
            currentBlockWidth (#_StringWidth pstr))
      (incf width currentBlockWidth)
      (incf blockCount)
      (when (> width maximumWidth)
        (decf width currentBlockWidth)
        (let* ((ellipsisWidth (with-pstrs ((e ellipsisStr)) (#_StringWidth e)))
               (high (* blockCount 255))
               (base (- high 255))
               (low 0)
               current
               gap)
          (decf maximumWidth ellipsisWidth)
          (cond
           ((> width maximumWidth)
            (decf width previousBlockWidth)
            (decf high 255)
            (decf low 255))
           (t
            (setq high (min high (length str)))))
          (setq low base
                gap (- maximumWidth width))
          (loop
            (setq current (+ low (truncate (- high low) 2)))
            (if (> (with-pstrs ((s str base current)) (#_StringWidth s)) gap)
              (setq high current)
              (setq low current))
            (when (<= high (1+ low))
              (return-from compute-maybe-truncated-text
                (concatenate 'string (subseq str 0 (max 0 (1- high))) ellipsisStr)))))))
    str))

(define-sk8-function computeMaybeTruncatedText nil (theString maximumWidth ellipsisString)
  (compute-maybe-truncated-text theString maximumWidth ellipsisString))

(defun actor-text-size (theActor &key tryText maximumWidth)
  (let+ ((thePort (:cgrafport))
         (fi (:fontInfo))
         (theString (require-type (or tryText (sk8::text theActor)) 'string))
         x y)
    (when maximumWidth (setq maximumWidth (round maximumWidth)))
    (without-interrupts 
     (with-port thePort
       (set-text-state theActor)
       (#_getFontInfo fi)
       (setf y (+ (rref fi :fontInfo.ascent) 
                  (rref fi :fontInfo.descent) 
                  (rref fi :fontInfo.leading)))
       ;; And now the width of the string.
       (setq x (+ (compute-text-width theString maximumWidth) 10))
       (restore-text-state))
     (sk8-multivals x y))))

;;; ____________________________________________________
;;; ____________________________________________________

(defun spliceAtPosition (theActor seq position)
  (let ((length (length seq)))
    (vector-push-extend nil seq)
    (loop
      (when (= length position)
        (setf (elt seq position) theActor)
        (return-from spliceAtPosition))
      (setf (elt seq length) (elt seq (1- length)))
      (decf length))))

;;; Splices a node into a sequence (lists, vectors, etc.) into a position
;;;       node = the node to splice
;;;       from = the position the node now occupied

(defun splice (theActor seq position)
  (let* ((from (position theActor seq))
         (shift-down-p (> position from))
         (len (length seq))
         (end (1- len)))
    (unless (eql position from)
      (if shift-down-p
        (dotimes (i len)
          (when (and (>= i from)
                     (< i position)
                     (not (eql i end)))
            (setf (elt seq i) (elt seq (1+ i)))))
        (do ((i end (1- i)))
            ((eql i position))
          (when (<= i from)
            (setf (elt seq i) (elt seq (1- i))))))
      (setf (elt seq position) theActor))
    seq))

(defun offset-point-list (thePointList xOffset yOffset)
  (do* ((count 0 (1+ count))
        (head thePointList (cdr head)))
       ((null head) thePointList)
    (if (oddp count)
      (setf (car head) (+ (car head) yOffset))
      (setf (car head) (+ (car head) xOffset)))))

;;; _______________________________________________
;;; SHIFTING AN ITEM IN A LIST WITHOUT ALLOCATING MEMORY...
;;; _______________________________________________

;;; Shifts item at start to end. All items in between preserve their order.

(defun move-to-position-help (theList start end)
  (let (startItem startPlace endPlace)
    (setf startPlace (nthCdr start theList))
    (setf startItem (car startPlace))
    (setf endPlace startPlace)
    ;; Find the end!
    (dotimes (i (- end start))
      (setf (car endPlace) (cadr endPlace)
            endplace (cdr endPlace)))
    (setf (car endplace) startItem)
    theList))
   
;;; moves item in theList to position n. Since the helper can only move items forward, the last
;;; case transforms the problem into this form before calling it. Then it is unstransformed.

(defun moveToPosition (item thelist n)
  (let ((startPos (position item theList))
        (theLength (1- (length theList))))
    (cond ((= startPos n) theList)
          ((< startPos n) (move-to-position-help theList startPos n))
          (t 
           (setf theList (nReverse theList))
           (nReverse (move-to-position-help theList 
                                            (- theLength startPos) 
                                            (- theLength n)))))))

;;;-------------------------------------------------------------------------------

(define-sk8-function sk8::roundBoxDimensions nil (box)
  (sk8-multival-bind (left top right bottom)
                     (sk8-multivals-sequence 4 box)
    (multiple-value-bind (num rem) (round left)
      (if (zerop rem)
        (setf (elt box 2) (round right))
        (progn
          (setf (elt box 0) num)
          (setf (elt box 2) (round (- right rem))))))
    (multiple-value-bind (num rem) (round top)
      (if (zerop rem)
        (setf (elt box 3) (round bottom))
        (progn
          (setf (elt box 1) num)
          (setf (elt box 3) (round (- bottom rem)))))))
  box)

(defun boxWidth (box)
  (- (elt box 2) (elt box 0)))

(defun boxHeight (box)
  (- (elt box 3) (elt box 1)))

(defun boxSize (box)
  (sk8-multivals (- (elt box 2) (elt box 0))
                 (- (elt box 3) (elt box 1))))

(define-sk8-function sk8::alignmentOfBoundsToBounds nil (boundingBox1 boundingBox2)
  (let (left top right bottom containerLeft containerTop containerRight containerBottom)
    (sk8-multival-setf (left top right bottom) (sk8-multivals-sequence 4 boundingBox1))
    (sk8-multival-setf (containerLeft containerTop containerRight containerBottom) 
                       (sk8-multivals-sequence 4 boundingBox2))
    (let* ((width (- right left))
           (height (- bottom top))
           (widthDifference (- (- containerRight containerLeft) width))
           (heightDifference (- (- containerBottom containerTop)  height)))
      (sk8-multivals (if (zerop  widthDifference) 
                       (if (= left containerLeft) 1/2 nil)
                       (/ (- left containerLeft) widthDifference))
                     (if (zerop heightDifference) 
                       (if (= top  containerTop) 1/2 nil)
                       (/ (- top  containerTop) heightDifference))))))

;;; If width of size is > width of box, then left and right returned are same as for box; similarly for height
;;; returns left top right bottom

(define-sk8-function sk8::alignSizeToBounds nil (size boundingBox
                                                          &key
                                                          (horizontalAlignment 1/2)
                                                          (verticalAlignment 1/2))
  (sk8-multival-bind (left top right bottom)
                     (sk8-multivals-sequence 4 boundingBox)
    (let* ((newLeft   left)
           (newTop    top)
           (newRight  right)
           (newBottom bottom))
      (when horizontalAlignment
        (let* ((sizeWidth  (elt size 0))
               (boxWidth (- right left)))
          (unless (= sizeWidth boxWidth)
            (let ((leftSpace (* horizontalAlignment (- boxWidth sizeWidth))))
              ;; Adjust width and horizontal position to maintain aspectRatio
              (setf newLeft  (+ left leftSpace))
              (setf newRight (+ newLeft sizeWidth))))))
      (when verticalAlignment
        (let* ((sizeHeight (elt size 1))
               (boxHeight (- bottom top)))
          (unless (= sizeHeight boxHeight)
            (let ((topSpace  (* verticalAlignment (- boxHeight sizeHeight))))
              ;; Adjust height and vertical position to maintain aspectRatio
              (setf newTop    (+ top topSpace))
              (setf newBottom (+ newTop sizeHeight))))))
      (sk8-multivals newLeft newTop newRight newBottom))))

(define-sk8-function sk8::constrainSizeToAspectRatio nil (aspectRatio
                                                                size
                                                                &key fitOutside) ; instead of inside
  (let ((width (elt size 0))
        (height (elt size 1)))
    (let ((oldAspectRatio (if (zerop height)
                            0
                            (/ width height))))
      (if (= aspectRatio oldAspectRatio)
        (sk8-multivals width height)
        (if (xor (> aspectRatio oldAspectRatio)
                 fitOutside)
          (sk8-multivals width (/ width aspectRatio))
          (sk8-multivals (* height aspectRatio) height))))))

(defun limitTo01 (val)
  (cond
   ((null val) 1/2)
   ((< val 0)    0)
   ((< 1 val)    1)
   (t          val)))

(define-sk8-function sk8::constrainBoundsToAspectRatio nil (aspectRatio
                                                                  boundingBox
                                                                  &key
                                                                  (horizontalAlignment 1/2)
                                                                  (verticalAlignment 1/2)
                                                                  oldBounds
                                                                  fitOutside)
  (let ((newSize (sk8::constrainSizeToAspectRatio 
                  aspectRatio
                  (boxSize boundingBox) :fitOutside fitOutside)))
    (when oldBounds
      (sk8-multival-bind (horiz vert) (sk8::alignmentOfBoundsToBounds oldBounds boundingBox)
        (when (and horiz (<= 0 horiz 1))
          (setf horizontalAlignment horiz))
        (when (and vert
                   (<= 0 vert 1))
          (setf verticalAlignment vert))))
    (sk8::alignSizeToBounds newSize
                            boundingBox
                            :horizontalAlignment horizontalAlignment
                            :verticalAlignment verticalAlignment)))

;;;
;;; ROTATE-COORDS-2D--rotate a 2-D point about the origin by the angle theta
;;;

(defmacro rotate-coords-2d (x y theta)
  `(let ((c (cos ,theta))
         (s (sin ,theta))
         (x ,x)
         (y ,y))
     (values (- (* x c) (* y s)) (+ (* x s) (* y c)))))

;;; Makes sure the value is between the range specified. 

(defun range (low val high)
  (cond
   ((< val low)
    low)
   ((> val high)
    high)
   (t val)))

(defun sk8::relativize-pathname (pathname &optional (relative "ccl;"))
  (if (and (stringp pathname)
           (find #\; pathname))
    pathname
    (let ((full1 (namestring (translate-logical-pathname pathname)))
          (full2 (namestring (truename relative))))
      (when (and (>= (length full1) (length full2))
                 (string-equal full2 (subseq full1 0 (length full2))))
        (format nil "~a~a" relative (subseq full1 (length full2) (length full1)))))))

#|
	Change History (most recent last):
	2	6/10/93	Hernan	getRectShapeFromUser now resets the penpattern
				and mode BEFORE drawing each frame!
	8	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	9	11/15/93	hernan	Adding function to tell whether a string is a valid
				objectname for an object (it is not already in use
				by another object in the same project).
	10	12/1/93	hernan	Adding functions to compute the lineheight of an
				actor and the width of a string.
	11	12/1/93	hernan	Adding with-temporary-port.
	12	1/10/94	hernan	Adding removeSpaces: a function that removes all
				spaces from a string (actually returns a new string).
	13	1/10/94	hernan	Fonts just became objects!!!
	14	1/10/94	hernan	Taking removeSpaces out of here and into 15-media.
	15	1/11/94	hernan	self -> me
	16	1/14/94	hernan	Added useful port utilities.
	17	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	18	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	19	3/4/94	kleiman	font-num -> fontData
	20	3/7/94	chip	fixed error signalling in ensureType
	21	3/25/94	Hernan	Fixing leftover keywords.
	22	3/28/94	rod	Speeding Settable-Property up by a factor of 40.
	23	3/29/94	sidney	changeparents moved to another file and changed
	24	3/30/94	rod	Removing settable-property
	25	4/8/94	Hernan	Added a function to offset a list of points in place.
	26	4/12/94	Hernan	Adding a function to check if everything in an
				actor's contents satisfies a predicate.
	27	4/15/94	rod	adding with-hilitecolor macro
	28	4/15/94	rod	Adding require-trap to the front of the
				hilite macro
	29	5/4/94	till	FeedbackAction keyword is no longer.
	30	6/16/94	till	portType keywords begone.
	31 	 8/31/94	Hernan  	Chicago -> ChicagoFont.
	32 	 9/ 1/94	chip    	actor-text-size no longer dies with huge strings (radar #1183704); also, added related function: compute-maybe-truncated-text
	33 	 9/12/94	Hernan  	1180502: capitalizing object names.
	34 	10/ 5/94	rod     	Fixed compute-maybe-truncated-text for
							boundary case of no width.
	35 	10/ 7/94	dy      	add aspectRatio and alignment functions at end
	36 	10/ 7/94	dy      	alignSizeWithinBox -> alignSizeToBounds & related name changes and argument changes
	37 	10/13/94	chip    	got rid of disconnect-scroller-ports; added set-partnerScroller to do ALL the work in a general way
	38 	10/13/94	chip    	made set-partnerScroller more robust (now finds scroller through the port connection)
	39 	10/17/94	dy      	tweak aspect ratio stuff
	40 	10/19/94	dy      	more fixes to aspect ratio stuff
	41 	10/21/94	dy      	more alignment fixes
	42 	10/24/94	dy      	fix roundBoxDimensions
	43 	10/28/94	dy      	use lisp's rounding in roundBoxDimensions
	44 	11/30/94	Hernan  	Added an argument to findPort to actually create
							the port if a port of the right type was not found.
	45 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	46 	12/ 7/94	dy      	pointList arg -> thePointList so it will compile
	47 	12/13/94	Hernan  	set-text-state should call the real getter when
							it cannot find a textFont in the plist.
	48 	 1/27/95	Hernan  	Adding a function to reset the eventH and eventV
							variables using the current location of the mouse.
	49 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	50 	 2/20/95	dy      	no change, just force recompile
	2  	 7/31/95	Hernan  	Adding a public |with highlightColor| to be used in
						writing custom draw methods.
	3  	 4/19/96	Brian   	Moving wait functions to core sk8
	5  	 5/23/96	sidney  	Putting define-sk8-function back in the code.
	6  	 7/ 7/96	sidney  	changes for native PPC build
	7  	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
