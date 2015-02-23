(in-package :SK8Development)

(provide "RADIOBUTTON")

(require "LABEL" "objects;Widgets:Label")
(require "OVAL" "objects;Shapes:Oval")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




#| Modification History

02-22-93 ruben inheritsFrom? -> inheritsFrom
02-02-93 ruben totally overhauled
01-22-93 ruben id -> objectName

|#

(new label :objectname "RadioButton"
     :properties `((checkColor :inherit :value ,black))
     :undisposable t
     :prototype t
     :project sk8)

(setFrameSize RadioButton 0 0)
(setf (text RadioButton) "Untitled")
(setf (textSize radioButton) 12) ;; Forces recomputation of size!
(setf (textLocation RadioButton) 'centerleft)
(setf (textHOffset RadioButton) 16)
(setf (mouseSensitivity RadioButton) 'opaque)
(setf (resizable radiobutton) t)

(define-handler resized (radioButton)
  (setf (v (innerButton me)) (/ (height me) 2)))

(new oval :objectname "RadioButtonButton" :project sk8)
(setf (container RadioButtonButton) RadioButton)
(setBoundsRect radioButtonButton 2 2 14 14)

(new oval :objectname "RadioButtonHighlight" :project sk8)
(setf (container RadioButtonHighlight) radioButtonButton)
(setf (fillColor RadioButtonHighlight) white)
(setf (frameColor RadioButtonHighlight) black)
(setBoundsRect RadioButtonHighlight 3 3 9 9)
(setFrameSize RadioButtonHighlight 0 0)

;;; innerButton -- the oval inside the radioButton. Not necessarily the first item.

(define-handler innerButton (RadioButton)
  (dovector (c (gs:node-contents me))
    (when (inheritsFrom c RadioButtonButton)
      (return-from innerButton c))))
  
;;; highlightArea -- this is the part that highlights when a radiobutton
;;;           is checked or unchecked
;;;

(define-handler highlightArea (RadioButton)
  (car (contents (innerButton me))))

;;; Implementing the autohighlight special behaviour!

(setf (autohighlight radiobutton) t)
(setf (inverts radiobutton) nil)

(define-handler (setf highlight) (boolean radioButton)
  (if boolean
    (unless (up mouse)
      (setFramesize (innerButton me) 2 2))
    (setFramesize (innerButton me) 1 1))
  (call-next-method))

(define-handler mouseup (radioButton)
  (check me))

;;; Used to alternate states.

(define-handler toggleState (radioButton)
  (if (checked me)
    ;; Unchecking!
    (setf (fillcolor (highlightArea me)) (fillcolor (innerButton me)))
    ;; Checking!
    (setf (fillcolor (highlightArea me)) (checkColor me))))

;;; check -- checks the radio button and unchecks the radio buttons
;;;        in its cluster, if any. If there are no buttons in its cluster,
;;;        it changes its highlight color.
;;;

(define-handler check (RadioButton)
  (let* ((cluster (container me))
         (things (and cluster (contents cluster))))
    ;; Get rid of non radioButtons.
    (unless (every #'(lambda (b) (inheritsFrom b RadioButton)) things)
      (setf things (remove-if-not #'(lambda (b) (inheritsFrom b RadioButton)) things)))
    (withActorLocked (me)
      ;; Uncheck the previously checked button.
      (when things
        (dolist (aRadio things)
          (when (checked aRadio)
            (toggleState aRadio))))
      ;; Check myme!
      (toggleState me))))

(define-handler (setf checked) (checked RadioButton)
  (unless (eq checked (checked me))
    (let ((cluster (container me)))
      (toggleState me)
      (when cluster
        (if checked
          ;; Uncheck previously selected radiobutton.
          (gs:docontents (c cluster)
            (when (and (neq c me)
                       (inheritsFrom c radioButton)
                       (checked c))
              (toggleState c)
              (return)))
          ;; Check first button we find in contents.
          (gs:docontents (c cluster)
            (when (and (neq c me)
                       (inheritsFrom c radioButton)
                       (not (checked c)))
              (toggleState c)))))
      checked)))
       
;;; checked -- tells you whether the radio button is checked or not.
;;;

(define-handler checked (RadioButton)
  (eq (fillcolor (highlightarea me)) (checkColor me)))

(setSize radioButton 80 16)
(resized radioButton)

#|
	Change History (most recent last):
	2		5/24/93	Hernan	The wills of the gods are inscrutable to man.
	3		5/25/93	Hernan	RadioButtons now look like macbuttons and
							hilite properly!
	4		5/26/93	Hernan	CheckColor property added. Can be set to any renderer.
	5		5/26/93	Hernan	Radiobutton is now a child of label. This automates
							the resizing of the button when the text changes.
	6		5/27/93	hernan	Fixed typo.
	7		6/4/93	Hernan	Initialized the radioButton so that the oval is in 
							the proper place and the radioButton is resized
							to show all its text.
	8		6/22/93	Hernan	(1) set its textSize at beginning of the file to 
							recompute the button's height.
							(2) If the mouse is up already, do not hilite 
							in set hilite.
	14		1/11/94	hernan	self -> me
	15		1/31/94	rod	Changed text of default button to "Option",
							it looks nice in the swatches and Lynn will 
							stop complaining.  I hope.
	15		1/31/94	rod	
	15		1/31/94	rod	
	16		2/12/94	kleiman	name changes
	17		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	18		2/25/94	hernan	Using symbols instead of keywords for options!!!
	19		3/21/94	Hernan	Making the radiobutton resizable.
	20		4/6/94	Hernan	Making button's text be "Untitled".
	21		4/12/94	Hernan	Avoiding use of contents when not necessary.
	22		4/18/94	Hernan	Fixing (seft checked) to uncheck or check a button
							when the check status of a radio button changes.
	23		5/4/94	rod	Making radiobuttoncluster NOT a prototype
	24		5/16/94	Hernan	Getting rid of radiobuttoncluster (was replace by
							straight containment).
	25 	 9/16/94	Hernan  	Made the default size be such that all the text is
							visible.
	26 	11/30/94	Hernan  	No longer relies on the button being item 1 in its
							contents.
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3   4/16/96Build   removing reference to nthItem
|# ;(do not edit past this line!!)
