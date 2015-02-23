(in-package :SK8Development)

(provide "LABEL")
(require "RECTANGLE")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; __________________________________________________________________________
;;;                              The SK8 LABEL object.
;;; __________________________________________________________________________

#| Modification History

04-05-93 hernan the label decides how it resizes itself using the textLocation
                property
01-22-93 ruben id -> objectName
10-03-92 ruben argument changes
09-20-92 hernan changing this to work with MF VI
08/07/92 hernan wrote this.

|#


(new rectangle :objectname "Label"
     :undisposable t
     :prototype t
     :project sk8)

(setf (private Label) nil)  ; in public api

(setf (text label) "Untitled")
(setf (fillcolor label) transparent)
(setFrameSize label 0 0)
(setf (resizable label) nil)

;;; 1.0
;;; BIND-LABEL-TEXT -- resizes the label's rectangle so that it can show all its text.
;;;                 Resizes according to the textLocation. Uses actorTextSize!

(defun bind-label-text (thelabel)
  (sk8-multival-bind (x y) (actorTextSize theLabel)
    (sk8-multival-bind (fh fv) (framesize theLabel)
      (incf x (textHOffset theLabel))
      (incf x fh)
      (incf y fv)
      (SK8-multival-bind (ll tt rr bb) (boundsRect theLabel :physical t)
        (case (textLocation theLabel)
          ((center topCenter bottomCenter)
           (if (gs:node-container theLabel)
             ;; If at all possible, set the size logically (since that preserves the location).
             (sk8-multival-bind (hscale vscale) (physicalScale (gs:node-container theLabel))
               (setf x (/ x hScale) y (/ y vscale))
               (setSize theLabel x y))
             (setSize theLabel x y :physical t)))
          ((topleft centerLeft bottomLeft)
           (setBoundsRect theLabel ll tt (+ ll x) (+ tt y) :physical t))
          ((topright centerRight bottomRight)
           (setBoundsRect theLabel (- rr x) (- bb y) rr bb :physical t) t))))))
                 
;;; 1.0
;;; (SETF TEXT) -- sets the text of self to text, making sure that self is big enough
;;;             to show all the text.

(define-handler (setf text) (theText label &key)
  (declare (ignore theText))
  (prog1 
    (call-next-method)
    (bind-label-text me)))

;;; 1.0
;;; (SETF TEXTFONT) -- sets the text font and recomputes the bounds rect.

(define-handler (setf textfont) (theFont label &key start end)
  (declare (ignore thefont start end))
  (prog1 
    (call-next-method)
    (bind-label-text me)))

;;; 1.0
;;; (SETF TEXTSIZE) -- sets the size of the text of self to size. When the actor has text, it
;;;                makes itself dirty to show the changes.

(define-handler (setf textSize) (size label &key start end)
  (declare (ignore size start end))
  (prog1 
    (call-next-method)
    (bind-label-text me)))

;;; 1.0
;;; (SETF TEXTSTYLE) -- sets the style of the actor's text to combinations based on the
;;;                  arguments. As usual, if the actor has any text, it makes
;;;                  itself dirty in order to show the changes.      

(define-handler (setf textStyle) (styleList label &key)
  (declare (ignore stylelist))
  (prog1 
    (call-next-method)
    (bind-label-text me)))

;;; Since bind-label-text now takes into account the framesize, this is required.

(define-handler setFrameSize (label h v &key physical)
  (declare (ignore h v physical))
  (prog1
    (call-next-method)
    (bind-label-text me)))

(bind-label-text label)

#|
	Change History (most recent last):
	2	5/26/93	Hernan	The label now deals with text offsets. This is
				helpful to allow labels to contain other actors 
				which do not get covered by the text.
	3	6/8/93	Hernan	Found the culprit for the font chaning problem!
				bind-label-text now encloses its font munging 
				stuff using ccl's temporary port.
	4	6/22/93	Hernan	The label now calls actorTextSize from the actor file.
	5	6/25/93	Hernan	The Great Renaming of 93.
	11	11/2/93	rod	Fixed it to include the frame size when setting 
				the size.  Removed changing height, as this 
				doesn't work properly.  Better to always set both
				height and width.
	12	11/23/93	hernan	Setting the size of the default label to the right
				size for showing the label's text.
	13	11/30/93	hernan	Making the label's resizable be false. Also 
				removed the resize method of label. Now you can
				set the resizable of your label to true and resize
				it with the UI.
	14	1/10/94	hernan	Fonts just became objects!!!
	15	1/11/94	hernan	self -> me
	16	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	17	2/25/94	hernan	Using symbols instead of keywords for options!!!
	18	4/5/94	kleiman	code review
				removed all compiler warnings
	19	4/6/94	Hernan	Making the redefined methods return the right
				things.
	20	7/1/94	Hernan	1169265: removed silly methods that did nothing.
	21 	 4/ 4/95	Hernan  	Fixed bind-label-text to call setSize with logical because
							that preserves the location of the label.
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
