;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8dev)

(provide "MESSAGETOUSER")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "MODALDIALOGMODE" "objects;Dialogs:ModalDialogMode")
(require "USERDIALOGS" "functions;User Dialogs")

;;; _____________________________________________________________________________
;;;                             MessageToUser
;;; _____________________________________________________________________________

(new DialogBox :objectname "MessageToUserDialogBox" :project sk8)
(setlocation MessageToUserDialogBox 333 205)
(setSize MessageToUserDialogBox 368 105)
(addproperty MessageToUserDialogBox 'items)
(new DialogBoxDisplayEditText :project sk8 ::objectName "MessageToUserDialogBoxDisplayer")
(setf (container MessageToUserDialogBoxDisplayer) MessageToUserDialogBox)

(new DialogBoxHighlightedButton :objectname "MessageToUserDialogBoxButton" :project sk8
     :text "OK")
(setf (container MessageToUserDialogBoxButton) MessageToUserDialogBox)

(define-handler click (MessageToUserDialogBoxButton)
  (call-next-method)
  (setf (container MessageToUserDialogBox) nil)
  (exitModalState nil))

(define-handler EnteringStage (MessageToUserDialogBox)
  (call-next-method)
  (setf (keyTarget me) MessageToUserDialogBoxButton))

(define-handler (setf container) :after (theval MessageToUserDialogBox)
                (if (and theval (items me))
                  (ed-beep)))

(define-handler resized (MessageToUserDialogBox)
  (withActorLocked (me)
    (call-next-method)
    (sk8::sk8-multival-bind (x y) (size me)
      (sk8::sk8-multival-bind (hact vact) (actortextsize MessageToUserDialogBoxButton)
        (declare (ignore vact))
        (unless (> hact 50) (setf hact 50))
        (setBoundsRect MessageToUserDialogBoxDisplayer 7 7 (- x 7) (- y 35))
        (setBoundsRect MessageToUserDialogBoxButton 
                       (- x hact 15) (- y 33)
                       (- x 7) (- y 7))))))

(resized MessageToUserDialogBox)

(define-sk8-function MessageToUser nil (message &key 
                                                   (beep nil)
                                                   (oktext "OK")
                                                   (textFont ChicagoFont)
                                                   (textsize 12)
                                                   (width 300)
                                                   (height nil)
                                                   (h nil)
                                                   (v nil)
                                                   ((:system UseSystem) nil))
  (unless (and h v)
    (sk8::sk8-multival-bind (hh vv) (mainMonitorcenter)
      (if (not h) (setf h hh))
      (if (not v) (setf v vv))))
  (if UseSystem
    (progn
      (unless height (setf height 100))
      (when beep (beep))
      (MacMessageToUser message :oktext oktext :width width :height height :top (- v (round height 2)) :left (- h (round width 2))))
    (progn
      (setlocation MessageToUserDialogBox h v)
      (setf (textsize MessageToUserDialogBoxDisplayer) textsize)
      (setf (textfont MessageToUserDialogBoxDisplayer) textfont)
      (setf (text MessageToUserDialogBoxButton) oktext)
      (setf (text MessageToUserDialogBoxDisplayer) message)
      (setf (items MessageToUserDialogBox) beep)
      (unless height 
        (sk8-multival-bind (hh vv) (actortextsize MessageToUserDialogBoxDisplayer)
          (declare (ignore hh))
          (setf height (min 340 (+ vv 45)))))
      (setSize MessageToUserDialogBox width height)
      (resized MessageToUserDialogBox)
      (modalDialog MessageToUserDialogBox))))


#|
	Change History (most recent last):
	1	9/28/93	rod	
	2	9/29/93	kleiman	
	3	10/4/93	kleiman	Nothing! Forget it.
	4	10/8/93	rod	
	5	10/11/93	rod	Added Remove Property and Remove Handler Dialogs
	6	10/27/93	rod	
	7	11/1/93	rod	
	8	11/1/93	rod	
	9	11/1/93	rod	
	10	11/1/93	rod	
	12	11/8/93	rod	
	13	11/8/93	rod	
	14	11/8/93	rod	
	15	11/8/93	kleiman	
	16	11/17/93	rod	
	17	11/19/93	rod	
	18	12/2/93	rod	Added RemoveHandler to the removeHandlerDialog
				now that that works!
	19	12/2/93	rod	
	20	12/2/93	rod	
	21	12/2/93	kleiman	removeProperty args changed
	22	12/2/93	kleiman	removeProperty args changed
	23	12/3/93	rod	
	24	12/3/93	rod	
	25	12/10/93	kleiman	
	26	12/17/93	till	#.'s be gone: doubleclick, keyDown
	28	2/12/94	kleiman	name changes
	29	2/18/94	rod	
	31	2/22/94	kleiman	The Final Renaming for Alpha! (yeah, right...)
	32	2/23/94	kleiman	addproperty inheritable -> propagatedValue
	33	2/25/94	rod	
	34	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	35	2/26/94	rod	
	36	2/28/94	rod	Goodbye currentProject
	37	2/28/94	rod	objectstring stuff
	38	2/28/94	hernan	Avoiding calling dispose directly.
	39	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	40	3/7/94	rod	
	41	3/9/94	rod	Doing Project Switching and Reference Clearing.
	42	3/10/94	rod	
	43	3/10/94	rod	
	44	3/10/94	rod	
	45	3/10/94	rod	
	46	3/10/94	rod	
	47	3/10/94	rod	
	48	3/16/94	rod	
	49	3/22/94	rod	
	50	3/25/94	Hernan	Locking the text of the editText used by the 
				messageToUserDialogBox.
	51	3/30/94	rod	
	52	3/30/94	rod	
	53	4/1/94	rod	
	54	4/1/94	rod	Now having a global variable to specify the editor prototype.
	55	4/5/94	kleiman	code review: various objects marked private
	56	4/12/94	kleiman	removeFunctionDialog works with actual Function
				object instead of symbol only
	57	4/13/94	rod	Making sure objectnamedialog clears old name.
	58	4/13/94	rod	Objectnamedialog checks if name already in use.
	59	5/4/94	rod	Locking text of certain edit texts.
	60	5/4/94	rod	Making selectfromcollection picker have a nice
				default size.
	61	5/6/94	rod	
	62	5/6/94	rod	
	63	5/6/94	rod	
	64	5/9/94	rod	
	65	6/3/94	rod	
	66	6/8/94	kleiman	Problem 1166987
	67	6/10/94	kleiman	Problem 1166589: label and menus removed;
				click of NHDBCreateAddHandler and EnteringStage
				of AddHandlerDialogBox modified
	68	6/13/94	rod	1167702:  Fixing TagDialog Menu
	69	6/13/94	rod	1167903:  Added a private checkbox to the
				add variable/constant dialog.
	70	6/16/94	rod	1168903:  Inheritable -> Propagating
	71	6/17/94	rod	
	72	6/23/94	rod	
	73	6/27/94	rod	
	74	6/29/94	chip	case-saving-intern --> intern-symbol/intern-legal-varName/intern-legal-handlerName (for radar #107741)
	75	7/11/94	rod	1173469: fixing allowEmptyStrings option.
	76	7/13/94	rod	1174021
	77	7/14/94	rod	
	78	7/29/94	rod	
	79	8/1/94	rod	Fixing ObjectNameDialog so it can take more 
				than one object at a time.
	80	8/1/94	rod	Fixing bug regarding setting objectnames to false.
	81	8/2/94	rod	Fixing bug with non-activated button in
				get answer from user.
	82	8/2/94	rod	Adding "Unname" button to objectname dialog.
	83 	 8/31/94	rod     	
	84 	 8/31/94	Hernan  	Chicago -> ChicagoFont.
	85 	 8/31/94	Hernan  	Geneva -> GenevaFont
	86 	 9/ 2/94	rod     	
	87 	 9/12/94	rod     	
	88 	 9/12/94	rod     	
	89 	 9/16/94	rod     	
	90 	 9/20/94	rod     	Request to have objectnamer default to 
							objectname if one exists.
	91 	 9/20/94	rod     	
	92 	 9/21/94	rod     	
	93 	 9/28/94	rod     	1180093:  Fixing bug that objects can't have their names set to themselves again.
	94 	11/17/94	rod     	FIxing stupid bug in with handlers.  I can't count 
							to 4.
	95 	11/17/94	rod     	
	96 	11/17/94	rod     	Fixing addhandler to handle "x for mods".
	97 	11/17/94	rod     	removing leftover print statement.
	98 	12/ 3/94	rod     	Removing duplicates from handler menu popup.
	99 	12/ 3/94	rod     	Fixing error message.
	100	12/22/94	rod     	
	101	 1/16/95	rod     	making objectnamedialog send the appropriate
							updates.
	102	 1/18/95	rod     	Adding clear all references
	103	 1/18/95	rod     	
	104	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	105	 2/14/95	rod     	
	106	 2/14/95	rod     	Fixing watch cursors on the dispose dialog.
	107	 3/ 7/95	rod     	
	108	 3/ 8/95	rod     	Cleaning up edittexts and using prototypes.
	109	 3/13/95	sidney  	Cleaning up clearallreferences
	110	 3/23/95	rod     	Prettying up error message for dispose dialog.
	111	 3/23/95	rod     	
	112	 3/24/95	rod     	
	113	 4/ 7/95	rod     	Checking for a reserved word.
	113	 4/ 7/95	rod     	
	113	 4/ 7/95	rod     	
	114	 4/12/95	rod     	
	115	 4/13/95	rod     	adding alphabeticaldisplay option to 
							selectfromcollectiondialog.
	116	 4/17/95	rod     	Adding extra dialog about clear all references
							to make it painfully clear.
	117	 4/25/95	rod     	
	2  	12/11/95	Brian   	fixing call to ss package
	3  	12/18/95	sidney  	add sk8::condition and other overridden symbols to reserved word list
	4  	 1/17/96	Hernan  	Folding in the new compiler API.
	5  	 2/12/96	Brian   	Making the addhandler dialog add the handler when
						a menu item is chosen.
	6  	 2/15/96	Brian   	Adding Sk8 character checking.
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
