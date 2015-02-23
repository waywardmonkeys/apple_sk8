(in-package :SK8Development)

(provide "CHECKBOX")

(require "RECTANGLE")
(require "LABEL" "objects;Widgets:Label")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;;______________________________________________________________________
;;;______________________________________________________________________
;;;
;;;                              CHECK-BOX BUTTON
;;;
;;;                   Originally by -> (c) 1992 ICE for Apple Computer, Inc.
;;;                     New implementation by Hern‡n Epelman-Wang
;;;
;;;______________________________________________________________________
;;; This is so simple in SK8.  Just a rectangle with a polygon inside of it, and a few
;;; methods for propagating size changes.
;;;
;;; The new version does not have a polygon and carries a label with it.
;;;______________________________________________________________________
;;;______________________________________________________________________

#| Modification History.

01-22-93 ruben id -> objectName
20-09-92 hernan changing this to work with MF VI

|#

;;______________________________________________________________________
;;______________________________________________________________________
;; CHECKBOX
;; Just a rectangle.  But, it has contents and a checkSize property.  All the rest is done
;; with methods.  The checkStyle property tells what shape to make the checkmark.
;; The CheckColor property is what color to make it.

(new label :objectName "CheckBox" :project sk8 :undisposable t :prototype t
     :properties '((checkSize :value 1)
                   (checkStyle :value nil)
                   (checkColor :inherit)
                   (checked :value nil)))

(setf (checked checkBox) t)
(Setf (mouseSensitivity checkBox) 'opaque)
(setFramesize checkBox 0 0)
(setf (textLocation checkBox) 'centerLeft)
(setf (textHOffset checkbox) 18)
(setf (resizable checkbox) t)

(new rectangle :objectName "CheckBoxCheck" :project sk8)
(setf (container checkBoxCheck) checkBox)
(setBoundsRect checkboxcheck 4 10 16 22)

(defun checkBoxCheck-draw (theActor thePort draw-Region)
  (gs:let+ ((flags (gs:node-flags theActor))
            (rect (gs:recompute-physicalBoundsRect theActor))
            (container (container theActor))
            (penSize (checkSize container))
            (tempRgn (:region))
            (checked (checked container)))
    ;; Recomputing the regions on demand if necessary.
    (gs:recompute-frame-region theActor flags)
    (gs:recompute-fill-region theActor flags)
    (gs:recompute-bounds-region theActor flags)
    ;; Rendering the fill.
    (cond
     ;; Check is in mixed state
     ((eq checked 'mixed)
      (render GrayPattern theActor (gs:node-fillRegion theActor) thePort))
     ;; Check is on
     (checked
      (render (gs:node-fillColor theActor) theActor (gs:node-fillRegion theActor) thePort)
      ;; Prepare to draw.
      (with-rgb (rgb (mcl-color (checkColor container)))
        (#_RGBForeColor rgb))
      (with-rgb (rgb (mcl-color (gs:node-fillcolor theActor)))
        (#_RGBBackColor rgb))
      (#_penSize penSize penSize)
      (#_sectRgn draw-region (gs:node-fillRegion theActor) tempRgn)
      (gs:with-clipped-region tempRgn
        ;; Draw the check.
        (#_moveTo (gs:rect-left rect) (gs:rect-top rect))
        (#_lineTo (gs:rect-right rect) (gs:rect-bottom rect))
        (#_moveTo (gs:rect-left rect) (gs:rect-bottom rect))
        (#_lineTo (gs:rect-right rect) (1- (gs:rect-top rect)))))
     ;; Check is off
     (t
      (render (gs:node-fillColor theActor) theActor (gs:node-fillRegion theActor) thePort)))
    ;; Rendering the frame.
    (render (gs:node-frameColor theActor) theActor (gs:node-frameRegion theActor) thePort)
    ;; Do hiliting...
    (when (and (gs:hilited? flags) (gs:inverted? flags))
      (#_invertRgn (gs:node-boundsRegion theActor)))))

(setf (gs:node-drawFunction checkBoxCheck) 'checkBoxCheck-draw)

(setf (checkColor checkBox) red)
(setf (checkStyle checkBox) 'X)

;;; Because of the possibility of roundoff errors in the computation of the boundsRect,

(define-handler actorTextSize (checkbox &key theText)
  (declare (ignore-if-unused theText))
  (sk8-multival-bind (h v) (call-next-method)
    (sk8-multivals h (1+ v))))

(define-handler resized (checkbox)
  (let ((theCheck (car (contents me))))
    (when theCheck
      (if (not (string= (text me) ""))
        (setf (v theCheck) (/ (height me) 2))
        (setBoundsRect theCheck 0 0 (width me) (height me))))))

(define-handler (setf checkSize) (checkSize checkbox)
  (setf (slot-value me 'checkSize) (gs:f.round checkSize))
  (when (checked me)
    (forceRedraw me)))

(define-handler (setf checked) (boolean checkBox)
  (unless (eq boolean (checked me))
    (setf (slot-value me 'checked) boolean)
    (forceRedraw me))
  boolean)

(define-handler check (checkBox)
  (setf (checked me) (not (checked me))))

;;; Implementing the autohighlight special behaviour!

(setf (autohighlight checkbox) t)
(setf (inverts checkbox) nil)

(define-handler (setf highlight) (boolean checkbox)
  (if boolean
    (setFramesize (car (contents me)) 2 2)
    (setFramesize (car (contents me)) 1 1))
  (call-next-method))

(define-handler mouseup (checkbox)
  (check me))

(setBoundsRect checkbox 0 0 15 15)
(setf (text checkbox) "Untitled")

#|
	Change History (most recent last):
	2	5/26/93	Hernan	Reimplemented. The check is no longer a polygon. 
				Has text associated to it. It is now a label to allow
				for automatic resizing when the text changes.
	3	5/27/93	hernan	The X now clips to the intersection of the fill and
				the draw region.
	4	6/1/93	Brian Roddy	If the text is nil the check is big
	5	6/1/93	Brian Roddy	set the text to nil
	6	6/4/93	Hernan	(setf checked) now returns the boolean value passed.
	7	6/8/93	Hernan	The check of the checkbox now stays always at its
				horizontal position, with its vertical position being
				the center of its container.
	8	6/11/93	Brian Roddy	Made the Size and text come at the end so they update properly and look good in the browser...
	9	6/25/93	Hernan	The Great Renaming of 93.
	15	10/1/93	hernan	==============================
								= Sad but true: the node is gone!!! =
								==============================
	16	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	17	10/27/93	chip	changed checkBoxCheck-draw to support a :MIXED value for the CHECKED property, which fills in the check area with GrayPattern
	18	10/27/93	chip	added the MultiObjectStateCheckBox actor
	19	10/27/93	chip	added nifty note about ports (with respect to the multiObjectStateCheckBox's functionality)
	20	10/29/93	hernan	Making multistate stuff only work for multistate
				checkbox.
	21	11/2/93	rod	
	22	11/23/93	hernan	Added 1 to the vertical text size of the checkbox
				so that roundoff errors do not cause the check's 
				edge to dissapear under its container.
	23	1/7/94	sidney	big mf/ss merge
	24	1/11/94	hernan	self -> me
	25	2/12/94	kleiman	name changes
	26	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	27	2/25/94	hernan	Using symbols instead of keywords for options!!!
	28	3/3/94	Hernan	The great handler argument name renaming of 94!
	29	3/3/94	kleiman	properties -> addproperty
	30	3/3/94	kleiman	private properties declared via makeprivateproperty
	31	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	32	3/17/94	Hernan	Ruben wants text for the default checkbox.
	33	3/18/94	chip	Fixed glitch in (setf objects) of MultiObjectStateCheckbox
	34	3/21/94	Hernan	Making the checkbox resizable.
	35	4/6/94	Hernan	Making Checkbox's text be "Untitled".
	36	4/11/94	Hernan	Fixing the resized method of the checkbox to 
				place the checkbox's text correctly. The checkbox
				no longer resizes.
	37	4/12/94	Hernan	Avoiding use of contents when not necessary.
	38	5/16/94	Hernan	Removing duplicate property of multiobject...
	39 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	40 	11/18/94	Hernan  	CheckStyle is turned into a symbol.
	2  	 1/17/96	sidney  	ss::!coerce -> sk8::as
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
