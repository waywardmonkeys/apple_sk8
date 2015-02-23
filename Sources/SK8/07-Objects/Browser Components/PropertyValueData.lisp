;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8development)

(provide "PROPERTYVALUEDATA")
(require "UNDOABLESETLOG" "objects;Browser Components:UndoableSetLog")

;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;;                GetValueFromUser
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________

(new object :project sk8 :objectname "PropertyValueData"
     :private t
     :properties '(propertiesCovered))

(setf (propertiesCovered PropertyValueData) (make-hash-table :test #'eq))

(defmacro setPropertyValuesBoolean (property)
  `(setf (gethash ',property (propertiesCovered PropertyValueData))
         (list (list t nil) 1)))

(defmacro setPropertyValues (property values)
  `(setf (gethash ',property (propertiesCovered PropertyValueData))
         (list ,values 1)))

(defmacro lookupPropertyValues (property)
  `(gethash ,property (propertiesCovered PropertyValueData)))

;;; Valid Property Values. 

(setPropertyValuesBoolean autohighlight)
(setPropertyValuesBoolean Highlight)
(setPropertyValuesBoolean acceptsdrops)
(setPropertyValuesBoolean autotab)
(setPropertyValuesBoolean resizesContents)
(setPropertyValuesBoolean boundedbycontents)
(setPropertyValuesBoolean cachespixmap)
(setPropertyValuesBoolean dofirstclick)
(setPropertyValuesBoolean draggable)
(setPropertyValuesBoolean resizable)
(setPropertyValuesBoolean visible)
(setPropertyValuesBoolean wantsidle)

(setPropertyValues penmode (mapcar #'car *sk8-pen-modes*))
(setPropertyValues fillcolor renderer)
(setPropertyValues checkcolor rgbcolor)
(setPropertyValues framecolor renderer)
(setPropertyValues textcolor renderer)
(setPropertyValues textlocation '(topLeft topRight topCenter
                                  centerLeft centerRight center
                                  bottomLeft bottomRight bottomCenter))
(setPropertyValues textstyle '(expand condense sk8::shadow
                               outline underline italic
                               bold plain))
(setPropertyValues text "")  ;; *** should be string, fix when adam makes handlers defined on string/symbol/etc. call when invoked on the prototype.
(setPropertyValues windowtitle "")  ;; *** should be string, fix when adam makes handlers defined on string/symbol/etc. call when invoked on the prototype.

(define-handler textfont (propertyvaluedata) 
  (list (fonts system) 1))   ; simulate it...

(setPropertyValues doubleclickstyle '(sk8::standard doubleClickOnly clickOnly))
(setPropertyValues mousesensitivity '(normal opaque transparent invisible custom))
(setPropertyValues windowstyle '(blank document documentNoClose
                                 documentWithGrow documentWithGrowNoClose
                                 documentWithZoom documentWithZoomNoClose
                                 doubleEdgeBox floating movableDialog 
                                 shadowEdgeBox singleEdgeBox sk8Window 
                                 tool))

;;; QuickTimeRenderer
(setPropertyValues        state                   '(inactive stopped poster paused playing))
(setPropertyValues        previousState           '(inactive stopped poster paused playing))
(setPropertyValues        newStateWhenDone        '(inactive stopped poster paused playing previous))
(setPropertyValuesBoolean displaying)
(setPropertyValuesBoolean playWithNoOtherActivity)
(setPropertyValuesBoolean onStage)
(setPropertyValues        initialStateForNewMovie '(inactive stopped poster paused nil))
(setPropertyValues        maintainAspectRatio     '(fitInside fitOutside nil))
(setPropertyValuesBoolean controllerVisible)
(setPropertyValuesBoolean usingBadge)
(setPropertyValuesBoolean playingEveryFrame)
(setPropertyValuesBoolean reservingRoomForController)
(setPropertyValuesBoolean keysEnabled)
(setPropertyValuesBoolean stepButtonsVisible)
(setPropertyValuesBoolean speakerButtonVisible)
(setPropertyValuesBoolean usingWindowPalette)
(setPropertyValuesBoolean doingInvalidating)
(setPropertyValuesBoolean servicing)

;;; QuickTimeMovie
(setPropertyValuesBoolean previewMode)
(setPropertyValuesBoolean active)
(setPropertyValuesBoolean changed)
(setPropertyValuesBoolean suppressingVisualTracks)
(setPropertyValues        resizingStyle           '(resizeMovieFromFill resizeMovieFromFrame resizeMovieFromRenderedRegion 
                                                    resizeFillFromMovie resizeFrameFromMovie resizeRenderedRegionFromMovie nil))
(setPropertyValues        repeating               '(sk8::loop palindrome nil))

(defun OKToGetValueFromUser (obj prop)
  (declare (ignore obj))
  (memq prop (hash-table-keys (propertiesCovered propertyvaluedata))))

(defun valuefromuserasamenu (obj prop)
  (declare (ignore obj))
  (let* ((theval (lookupPropertyValues prop))
         (val (car theval))
         (num (cadr theval)))
    (and (listp val) (< (length val) 15) (eql num 1))
    ))

(defun GetValueFromUser (obj prop &key relativeactor)
  (when (OKToGetValueFromUser obj prop)
    (let* ((theval (lookupPropertyValues prop))
           (val (car theval))
           (num (cadr theval))
           res)
      (unless (listp obj) (setf obj (list obj)))
      (setf res (getfromuser val 
                             :project (project (car obj))
                             :multiplevalues (eq num t) 
                             :relativeactor (and (listp val) relativeactor)))
      (undoableset prop obj res)
      )))

#|
	Change History (most recent last):
	1	9/28/93	rod	
	2	9/29/93	kleiman	
	3	10/11/93	rod	
	4	10/11/93	rod	Added color getter
	5	10/25/93	rod	Now getvaluefromuser takes a single object or a list of objs to set
	6	10/25/93	rod	fixed getfromuser string...
	7	11/1/93	rod	
	8	11/1/93	rod	
	9	11/1/93	rod	
	10	11/3/93	rod	
	11	11/5/93	rod	
	12	11/8/93	rod	
	13	11/8/93	rod	
	14	11/8/93	kleiman	
	15	11/12/93	rod	
	16	11/29/93	rod	
	17	11/29/93	rod	Fixed bug where I used SK8-multival-bind to
				destructure a list.
	18	12/5/93	rod	
	19	12/6/93	rod	
	20	12/10/93	rod	
	21	12/10/93	rod	
	22	12/10/93	rod	
	23	12/13/93	rod	
	24	12/14/93	rod	
	25	12/14/93	rod	
	26	12/14/93	rod	
	27	12/17/93	till	#.'s be goneL: doubleclick, extendedmousedown, keyDown
	28	1/11/94	rod	self->me
	29	1/13/94	rod	UNDO stuff
	30	1/17/94	rod	
	31	1/31/94	rod	
	32	2/12/94	kleiman	name changes
	33	2/14/94	sidney	rename descendants to knowndescendants, children to to knownchildren
	34	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	35	2/25/94	rod	
	36	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	37	2/26/94	rod	hilighter->highlighter
	38	2/28/94	rod	Bye Bye GetcurrentProject
	39	2/28/94	hernan	Avoiding calling dispose directly.
	40	3/2/94	Hernan	Porting to Fred 3.0
	41	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	46	5/6/94	rod	Redoing buttons
	47	5/10/94	rod	
	48	5/10/94	rod	
	49	5/13/94	till	Make update (of colormenu) cons less.
	50	6/3/94	rod	
	51	6/13/94	rod	1167832:  Making the switch of palettes use a
				spinning watch cursor.
	52	6/13/94	rod	Making the renderer palette behave nicer.
	53	6/29/94	chip	case-saving-intern --> intern-symbol/intern-legal-varName/intern-legal-handlerName (for radar #107741)
	54	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	55	7/18/94	rod	
	56	7/21/94	rod	Fixed getfromuser of fake classes.
	57	8/22/94	rod	
	58 	 8/22/94	nil     	
	59 	 8/26/94	rod     	
	60 	 9/ 1/94	Hernan  	The penmode of ComplexRGBColor is now stored
							as a symbol.
	61 	 9/13/94	rod     	Fixing windowstyles.
	62 	 9/20/94	rod     	
	63 	10/28/94	dy      	new QuickTimeRenderer property values
	64 	11/14/94	rod     	
	65 	12/ 3/94	rod     	error message fixes.
	66 	12/ 7/94	rod     	making getnewfromuser of rgbcolor prompt for
							an objectname if one isn't specified.
	67 	12/22/94	rod     	adding special GetFromUser with swatches
							for media that work with imageRenderer.
	68 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	69 	 2/ 8/95	rod     	fixing tab order in dialog.
	70 	 2/15/95	rod     	Fixed getnewfromuser of rgbcolor.
	71 	 3/ 3/95	rod     	fixing getcontentfromuser functions which don't
							seem to have ever worked?!?!
	72 	 3/ 6/95	dy      	loop -> sk8::loop
	73 	 3/14/95	Hernan  	Adding new windowStyles that do not have a close
							box.
	74 	 3/24/95	rod     	fixing the color of the halo
	75 	 3/28/95	dy      	add new default values for qtrenderer properties
	76 	 3/29/95	dy      	suppressingSpeakerButton -> speakerButtonVisible; same for StepButtons
	77 	 3/29/95	rod     	
	78 	 3/29/95	sidney  	stepsbuttonvisible was not added as a property.
	79 	 3/29/95	dy      	remove suppressingStepButtons suppressingSpeakerButton  properties
	80 	 4/13/95	rod     	
	81 	 4/25/95	rod     	Fixing calls to getanswerfromuser from
							setting the height.
	2  	 1/17/96	Hernan  	Folding in the new compiler API.
	2  	 2/27/97	Hernan  	define-sk8-function->defun.
|# ;(do not edit past this line!!)
