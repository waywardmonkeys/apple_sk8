;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)

(provide "DIALOGBOX")

(require "RECTANGLE")
(require "LABEL" "objects;Widgets:Label")
(require "EDITTEXT" "objects;EditText:EditText")
(require "ROUNDRECT" "objects;Shapes:RoundRect")
(require "STRINGSANDCHARS" "functions;Strings And Chars")
(require "MODALDIALOGMODE" "objects;Dialogs:ModalDialogMode")

(defvar okChars (list #\Delete #\Space #\Escape #\" 
                        #\UpArrow #\downArrow #\BackArrow #\ForwardArrow 
                        #\enter #\Newline #\tab))

(define-sk8-function OkSK8Character nil (theChar)
  (or (AlphaNumeric thechar)
      (memq theChar okChars)))

;;;This File contains the raw objects used in the dialogs.  
;;;These dialogs are used in the SK8 Browser and ui halos for creation of
;;;handlers/properties/objects...

(new rectangle :objectname "DialogBox" :project sk8)
(setf (windowstyle dialogbox) 'doubleedgebox)
(setf (fillcolor DialogBox) white)
(setFramesize DialogBox 0 0)
(setf (framecolor DialogBox) gray)

(define-handler mouseDown (DialogBox)
  (if (eq (eventactor) me)
    (drag me :live t)))

(new label :objectName "DialogBoxLabel" :project sk8)

(define-handler enteringStage (DialogBox)
  (call-next-method)
  (setf (cursor stage) standardcursor))

(define-handler leavingStage (DialogBox)
  (if (eq (project me) sk8)
    (dolist (p (localproperties me))
      (unless (mf::creation-relation-p me p)
        (SK8::setValue p me nil)))))

;;;-------------------------------------------------------------------------------------------

(new rectangle :objectname "DialogBoxDisplayRectangle" :project sk8)
(setf (framecolor DialogBoxDisplayRectangle) black)
(setf (mousesensitivity DialogBoxDisplayRectangle) 'transparent)
(setf (textcolor DialogBoxDisplayRectangle) black)
(setf (fillcolor DialogBoxDisplayRectangle) graytone60)


(new editText :objectname "DialogBoxEditText" :project sk8)
(setframeSize DialogBoxEditText 1 1)
(setf (framecolor DialogBoxEditText) black)

(new DialogBoxEditText :objectname "DialogBoxDisplayEditText" :project sk8)
(setframeSize DialogBoxDisplayEditText 0 0)
(setf (mouseSensitivity DialogBoxDisplayEditText) 'transparent) ;; HEW
(locktext DialogBoxDisplayEditText)

;;;-------------------------------------------------------------------------------------------

(new roundRect :objectname "DialogBoxButton" :project sk8
     :properties '((enabled :value t)))

(setf (fillcolor DialogBoxButton) white)
(setf (autoHighlight DialogBoxButton) t)
(setf (roundedness DialogBoxButton) '(8 8))
;;;(setf (inverts DialogBoxButton) nil)

(define-handler enteringstage (DialogBoxButton)
  (when (highlight me) (setf (highlight me) nil))
  (call-next-method))

(define-handler (setf enabled) (boolean DialogBoxButton)
  (unless (eq boolean (enabled me))
    ;; Set the slot.
    (setf (slot-value me 'enabled) boolean)
    ;; Change the attributes.
    (withActorLocked (me)
      (if boolean
        (setf (textColor me) Black
              (framecolor me) Black
              (autohighlight me) t)
        (setf (textColor me) LightGray
              (framecolor me) Gray
              (autohighlight me) nil)))))


(setsize DialogBoxButton 70 18)
;;;-------------------------------------------------------------------------------------------

(new DialogBoxButton :objectname "DialogBoxHighlightedButton" :project sk8)
(setFrameSize DialogBoxHighlightedButton 3 3)
(setf (mouseSensitivity DialogBoxHighlightedButton) 'opaque) ;; HEW
(setf (roundedness DialogBoxHighlightedButton) '(15 15))

(new DialogBoxButton :objectname "DialogBoxHighlightedButtonInnard" :project sk8)
(setFrameSize DialogBoxHighlightedButtonInnard 1 1)
(setf (container DialogBoxHighlightedButtonInnard) DialogBoxHighlightedButton)
(setf (text DialogBoxHighlightedButtonInnard) nil)
(setf (fillcolor DialogBoxHighlightedButtonInnard) Transparent)
(tagpart DialogBoxHighlightedButton DialogBoxHighlightedButtonInnard 'innerRect)

(define-handler resized (DialogBoxHighlightedButton)
  (call-next-method)
  (sk8-multival-bind (ll tt rr bb) (boundsRect me :physical t)
    (setBoundsrect (innerRect me) (+ ll 4) (+ tt 4) (- rr 4) (- bb 4) :physical t)))

(define-handler keydown (DialogBoxHighlightedButton theChar)
  (if (or (eq theChar #\Return) (eq theChar #\Enter))
    (click me)))

(define-handler click (DialogBoxHighlightedButton)
  (sk8-multival-bind (hh vv) (mouseloc stage) 
    (when (neq (container (actorathvcoordinates hh vv)) me)
      (setf (Highlight me) t)
      (sleep 0.05)
      (setf (Highlight me) nil)
      (sleep 0.05))))

(define-handler (setf Highlight) (boolean DialogBoxHighlightedButton)
  (setf (inverts me) nil)
  (call-next-method)
  (setf (Highlight (car (contents me))) boolean))

(define-handler text (DialogBoxHighlightedButton)
  (text (car (contents me))))

(define-handler (setf text) (boolean DialogBoxHighlightedButton)
  (call-next-method "" me)
  (copytextproperties (car (contents me)) me)
  (setf (text (car (contents me))) boolean))

(setsize DialogBoxHighlightedButton 78 26)

;;;-------------------------------------------------------------------------------------------
(new DialogBoxButton :objectname "DialogBoxCancelButton" :project sk8)
(setf (text DialogBoxCancelButton) "Cancel")
(setSize DialogBoxCancelButton 55 20)
(setLocation DialogBoxCancelButton 260 90)

(define-handler click (DialogBoxCancelButton)
  (abortevent))

;;;------------------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------------------


(dolist (i (list DialogBoxDisplayRectangle DialogBoxButton DialogBoxCancelButton 
                 DialogBoxEditText DialogBoxDisplayEditText
                 DialogBoxHighlightedButton DialogBoxLabel))
  (setf (textStyle i) '(plain))
  (setf (textsize i) 12)
  (setf (textfont i) ChicagoFOnt)
  )

#|
	Change History (most recent last):
	2	5/25/93	Brian Roddy	mouseenter on dialogboxbutton goes to the 
				standardCursor
	3	6/15/93	Brian Roddy	
	4	6/25/93	Brian Roddy	
	10	9/28/93	rod	
	11	9/29/93	rod	
	12	10/1/93	rod	
	13	10/11/93	rod	
	14	11/8/93	rod	
	15	11/15/93	rod	
	16	11/19/93	hernan	
	18	1/14/94	rod	
	19	2/12/94	kleiman	name changes
	20	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	21	2/25/94	hernan	Using symbols instead of keywords for options!!!
	22	2/26/94	rod	
	23	3/9/94	rod	
	24	3/9/94	Brian	
	25	3/21/94	kleiman	setvalue -> sk8::setValue
	26	5/6/94	rod	Changing buttons to be more standard.
	27	6/3/94	rod	
	28	6/23/94	rod	Getting rid of keyword property values.  No
				longer need cancel stuff in cancel button.
	29	6/23/94	rod	
	30	6/27/94	rod	
	31	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	32	7/13/94	rod	1174282
	33 	 8/24/94	Hernan  	1182780: Defining enabled and set enabled of 
							dialogBoxButton.
	34 	 9/ 2/94	rod     	
	35 	10/12/94	chip    	DialogBox's leavingStage handler now checks creationRelations rather than tags
	36 	12/21/94	rod     	Making dialogs white.
	37 	12/22/94	rod     	
	38 	 3/ 3/95	rod     	
	39 	 3/ 6/95	rod     	
	40 	 3/ 8/95	rod     	
	41 	 3/ 9/95	rod     	
	42 	 4/12/95	rod     	
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
