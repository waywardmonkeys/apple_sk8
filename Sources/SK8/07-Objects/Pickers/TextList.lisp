(in-package :SK8Development)

(provide "TEXTLIST")

(require "RECTANGLE")
(require "PICKER" "objects;Pickers:Picker")
(require "SCROLLER" "objects;Widgets:Scroller")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; The new textList which uses the fastPicker... All text viewer functionality is supported by
;;; THE SAME API!!!
;;;
;;; The textList contains: a titleBar, a picker and a scroller. The fast picker and the scroller
;;; are hooked together...

#| Modification History 

03-24-93 ruben dev't kitchen GetFromUser

|#

(new rectangle :objectName "TextList" 
     :undisposable t
     :prototype t
     :project sk8)

(setBoundsRect textList 3 3 97 197)

(define-handler bestsize (textList)
  (resized me))

;;; Brian's butt ugly new resized method.  Que cagada...
;;; This has to resize its components directly because we do not know in advance
;;; what sort of thing will go into the textlist.

(define-handler resized (textList)
  (let (hsize vsize)
    (SK8-multival-setf (hSize vSize) (size me))
    (setBoundsRect (titlebar me) 0 0 (- hsize 0) 17)
    (setBoundsRect (vscroller me) (- hSize 16) 
                   (if (visible (titlebar me))
                     17
                     0) hSize vSize)
    (setBoundsRect (picker me) 0 (if (visible (titlebar me))
                                     17
                                     0)
                   (- hSize 15) (- vsize 0))
    ))

;;; The title bar...

(new rectangle :objectName "TextListTitleBar" :project sk8)
(setBoundsRect textListTitleBar 1 1 93 17)
(setf (container textListTitleBar) textList)
(setf (fillcolor textListTitleBar) gray)
(setf (textColor textListTitleBar) white)
(tagPart textList textListTitleBar 'titleBar)

(define-handler (setf visible) (boolean textListTitleBar)
  (declare (ignore-if-unused boolean))
  (call-next-method)
  (resized (container me)))

(define-handler bestSize (textListTitleBar)
  (declare (special hSize vSize))
  (setBoundsRect me 1 1 (- hsize 1) 17))

(define-handler title (textList)
  (let ((theTitle (titlebar me)))
    (when theTitle
      (text theTitle))))

(define-handler (setf title) (theText textList)
  (let ((theTitle (titlebar me)))
    (when theTitle
      (setf (text theTitle) theText))))

(define-handler mousedown (textList)
  (setf (keytarget (sk8::window me)) (picker me))
  (call-next-method))

;;; The picker...

(new picker :objectName "TextListPicker" :project sk8)
(setBoundsRect textListPicker 1 17 81 193)
(setf (container textListPicker) textList)
(tagPart textList textListPicker 'picker)

(define-handler bestsize (textListPicker)
  (declare (special hSize vSize))
  (setBoundsRect me 0 (if (visible (titlebar (container me)))
                          17
                          0)
                 (- hSize 15) vsize))

;;; We always want the textList to be the currentKey. Thus, this method lets the picker's method
;;; set the currentKey to the picker but then resets it to the textList.

(define-handler mouseUp (textListPicker)
  (call-next-method)
  (let ((tla (sk8::window me))
        (theList (container me)))
    (when (and tla (neq theList (keyTarget tla)))
      (setf (keyTarget tla) theList))
    (selectionCompleted theList)))

(define-handler keyup (textListPicker theChar)
  (declare (ignore-if-unused theChar))
  (call-next-method)
  (selectionCompleted (container me)))

(define-handler (setf items) (itemList textListPicker)
  (declare (ignore-if-unused itemList))
  (call-next-method)
  (selectionCompleted (container me)))

;;; The Scroller: hooked up to the picker!!!

(new scroller :objectName "TextListVScroll" :project sk8)

(setf (objectName (thumb textListVScroll)) "textListVScrollThumb")
(setf (objectName (scrollerBody textListVScroll)) "textListVScrollBody")
(setf (objectName (upArrow textListVScroll)) "textListVScrollUpArrow")
(setf (objectName (downArrow textListVScroll)) "textListVScrollDownArrow")

(setBoundsRect textListVScroll 81 17 94 194)
(resized textListVScroll)
(setf (container textListVScroll) textList)
(tagPart textList textListVScroll 'vScroller)

(setf (partnerVScroller textListPicker) textListVScroll)

(define-handler bestSize (textListVScroll)
  (declare (special hSize vSize))
  (setBoundsRect me (- hSize 16) 
                 (if (visible (titlebar (container me)))
                   17
                   0) hSize vSize))

(resized textList)

;;; ALIASED HANDLERS: functionality from the picker

(define-handler items (textList)
  (items (picker me)))

(define-handler (setf items) (itemList textList)
  (setf (items (picker me)) itemList))

(define-handler showSelection (textList &key curItem)
  (showSelection (picker me) :curItem curItem))

(define-handler selectedItems (textList)
  (selectedItems (picker me)))

(define-handler itemVisible (textList item)
  (itemVisible (picker me) item))

(define-handler pointOnWhichPart (textList x y &key (part 'bounds))
  (if (eq part 'items)
    (pointOnWhichPart (picker me) x y :part 'item)
    (call-next-method)))

(define-handler lineSpacing (textList)
  (lineSpacing (picker me)))

(define-handler (setf lineSpacing) (spacing textList)
  (setf (lineSpacing (picker me)) spacing))

(define-handler keyDown (textList theChar)
  (keyDown (picker me) theChar))

(define-handler autokey (textList theChar)
  (keyDown (picker me) thechar))

(define-handler keyUp (textList theChar)
  (keyUp (picker me) thechar))

;;; The method that sets it all up!!!

(define-handler initialize (textList original isNew initArgs)
  (declare (ignore isNew initArgs))
  (call-next-method)
  ;; Other things...
  (setf original (originalAncestor me original TextList))
  (setf (title me) (title original)))

(define-handler contentArea (textList)
  (picker me))

(define-handler selectedItems (textList)
  (selectedItems (picker me)))

(define-handler (setf SelectedItems) (items TextList &key upwards (deselecting t))
  (setf (selectedItems (picker me) :upwards upwards :deselecting deselecting) items))

(define-handler selection (textList)
  (selection (picker me)))

(define-handler (setf selection) (newValue textList &key upwards (deselecting t))
  (setf (selection (picker me) :upwards upwards :deselecting deselecting) newValue))

(define-handler addItems (textList itemList)
  (addItems (picker me) itemList))

(define-handler alphabeticalDisplay (textList)
  (alphabeticalDisplay (picker me)))

(define-handler (setf alphabeticalDisplay) (adisp textList)
  (setf (alphabeticalDisplay (picker me)) adisp))

(define-handler selectionStyle (textList)
  (selectionStyle (picker me)))

(define-handler (setf selectionStyle) (style textList)
  (setf (selectionStyle (picker me)) style))

(define-handler selectionCompleted (textList)
  )

(define-handler pickerPrototype (textlist)
  (baseParent (picker me)))

(define-handler (setf pickerPrototype) (pickerproto textlist)
  (when (or (inheritsfrom pickerproto editText)
            (inheritsfrom pickerproto picker)
            (inheritsfrom pickerproto tablepicker))
    (withactorlocked (me)
      ;; Unhook and remove the existing picker...
      (setf (partnervScroller (picker me)) nil)
      (setf (container (picker me)) nil)
      ;; Add and hook up a new picker...
      (setf (picker me) (new pickerProto :project (project me)))
      (setf (container (picker me)) me)
      (resized me)
      (setf (partnervScroller (picker me)) (vScroller me)))))

(define-handler localVirtualproperties (textList)
  (when (eq me textList)
    '(items selectedItems lineSpacing selectionStyle)))

#|
	Change History (most recent last):
	2	6/24/93	Hernan	Added an optional arg to showSelection that lets
				you specify what item within the selection you 
				want to see. (required for multiple selection).
	3	6/25/93	Hernan	The Great Renaming of 93.
	4	6/25/93	Hernan	InitializeObject -> initialize.
	10	9/20/93	hernan	Making textlists work for Brian's browser
				components.
	11	9/22/93	kleiman	initialize-internal
	12	9/28/93	rod	removed getfromuser
	13	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	14	10/4/93	kleiman	initialize-internal -> initialize
	15	11/1/93	rod	changed resized so pickers frame is visible
	16	11/1/93	rod	
	17	11/2/93	rod	
	18	11/23/93	rod	Made the default mousedown set the keytarget
				to the picker
	19	1/11/94	hernan	self -> me
	20	1/14/94	hernan	Now using ports to talk to scrollers!
	21	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	22	2/21/94	hernan	window -> sk8::window.
	23	2/25/94	hernan	Using symbols instead of keywords for options!!!
	24	2/28/94	hernan	Avoiding disposing things directly.
	25	3/2/94	Hernan	Porting to Fred 3.0
	26	3/3/94	Hernan	The great handler argument name renaming of 94!
	27	3/9/94	Hernan	alphabetical -> alphabeticalDisplay
	28	4/11/94	kleiman	obsoleted mouseDownAction handler
	29	6/21/94	Hernan	Obsoleting the setSelectedItems that is not used
				and does nothing useful.
	30	6/23/94	rod	
	31 	 9/12/94	Hernan  	1180502: capitalizing object names.
	32 	10/11/94	chip    	TextList's initialize no longer needs to hook up the scroller (ports are handled automatically)
	33 	10/13/94	chip    	(setf pickerPrototype) now unhooks the scroller from the old picker
	34 	 1/20/95	Hernan  	Conforming to new picker set selected items API.
	35 	 2/16/95	sidney  	readable argument names for initialize handler
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 8/ 8/96	Hernan  	body -> scrollerBody.
	5  	11/15/96	Hernan  	Initialize cannot assume that the "original" arg will have an
						object of the right type. Calling originalAncestor to deal with it.
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
