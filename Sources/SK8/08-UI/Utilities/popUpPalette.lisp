;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

;; (setCurrentProject ui)

#| Modification History

01-22-93 ruben id -> objectName

|#

;; THE PALETTE

(new rectangle :objectName "PopUpPalette" :project ui)
(setFrameSize popUpPalette 2 2)
(setSize popUpPalette 60 60)
(setf (fillColor popUpPalette) graytone80)
(setf (frameColor popUpPalette) black)
(setf (floating popUpPalette) t)

;; THE EVENT MODE
;; Tracks the different objects, returns the one that was selected.

(new eventMode :objectName "PopUpEventMode" :project ui
     :properties '((currentSelection) (defaultItem) (currentMenu)))

(define-handler handleIdle (popUpEventMode)
  (let ((obj (finddropobject (car (mouseloc stage)) (cadr (mouseloc stage))))
        (lvmenu (currentMenu me))
        (curItem (currentSelection me))
        (otherItem (defaultItem me)))
    (if (eq (sk8::window obj) lvmenu)
      ;; we are in the menu
      (when (and (neq obj lvmenu)
                 (neq obj curItem))
        (withActorLocked (lvMenu)
          (setf (Highlight curItem) nil)
          (setf (Highlight obj) t)
          (setf (currentSelection me) obj)))
      ;; we are off the menu
      (when (neq curItem otherItem) 
        (withActorLocked (lvmenu)
          (setf (Highlight curItem) nil)
          (setf (Highlight otherItem) t)
          (setf (currentSelection me) otherItem))))
    obj))

(define-handler handleMouseUp (popUpEventMode)
  ;;(exitMode me)
  (let ((currentItem (currentSelection me)))
    (exitModalState (ValueToReturn currentItem))
    )
  t)

(new rectangle :objectname "PopUpPaletteRect" :project ui
     :properties '(ValueToReturn))
(setSize PopUpPaletteRect 24 24)
(setframesize PopUpPaletteRect 0 0)

(new object :objectname "PopUpPaletteItem" :project ui
     :properties '(ColorToUse ValueToReturn))

(defun selectFromPopUpPalette (paletteitems &key (x (sk8::eventH))
                                       (y (sk8::eventV))
                                       (currentItem nil))
  (let* ((numitems (length paletteitems))
         (numER (length (gs:node-contents PopUpPalette)))
         existingRects theContents)
    (unless currentItem
      (setq currentItem (and paletteitems (car paletteitems))))
    (dotimes (i (- numitems numER)) (new PopUpPaletteRect :project ui :container popUpPalette))
    (setf numER (length (gs:node-contents PopUpPalette)))
    (dotimes (i (- numER numitems)) 
      (setf (container (car (contents PopUpPalette))) nil)
      )
    (setf ExistingRects (gs:node-contents PopUpPalette))
    (dotimes (i numitems)
      (if (eq (elt PaletteItems i) currentItem) (setf currentItem (elt ExistingRects i)))
      (setf (fillcolor (elt ExistingRects i)) (ColorToUse (elt PaletteItems i)))
      (setf (ValueToReturn (elt ExistingRects i)) (elt PaletteItems i)))
    (dolist (i (contents PopUpPalette))
      (setf (Highlight i) nil))
    (setf (Highlight currentItem) t)
    (setf (currentSelection popUpEventMode) currentItem)
    (setf (defaultItem popUpEventMode) currentItem)
    (setf (currentMenu popUpEventMode) PopUpPalette)
    ;; Let's save the contents.
    (setf theContents (contents PopUpPalette))
    (tile theContents :relativeactor PopUpPalette :boundsrect (list 2 2 0 0) :hspacing 2 :vspacing 2)
    (sk8-multival-bind (ll tt rr bb) (actorsBounds theContents)
      (setSize popuppalette (- rr ll -8) (- bb tt -8) ))
    (tile theContents :relativeactor PopUpPalette :boundsrect (list 2 2 0 0) :hspacing 2 :vspacing 2)
    (setLocation PopUpPalette x y :physical t)      
    (bringUp PopUpPalette)
    (prog1 (enterModalState popUpEventMode)
      (setf (container PopUpPalette) nil))))
  
  
#|

(new PopUpPaletteItem :objectname "testa" :project ui :colorToUse red :valuetoReturn :cornertocorner)
(new PopUpPaletteItem :objectname "testb" :project ui :colorToUse blue :valuetoReturn :cornertocorner)
(new PopUpPaletteItem :objectname "testx" :project ui :colorToUse black :valuetoReturn :centerout)
(new PopUpPaletteItem :objectname "testy" :project ui :colorToUse black :valuetoReturn :centerout)
(new PopUpPaletteItem :objectname "testz" :project ui :colorToUse black :valuetoReturn :centerout)
(new PopUpPaletteItem :objectname "testq" :project ui :colorToUse black :valuetoReturn :centerout)

(selectFromPopUpPalette (list testa testb))

|#
#|
	Change History (most recent last):
	2	6/10/93	Brian Roddy	
	3	6/15/93	Brian Roddy	
	1	6/24/93	Brian Roddy	
	7	10/15/93	rod	
	9	2/12/94	kleiman	name changes
	6  	 2/27/97	Hernan  	The Final Renaming for Alpha! (yeah, right...)
						133/5/94rodwindow -> sk8::window.
						122/28/94hernanAvoiding calling dispose directly.
						144/13/94HernanAvoiding use of contents when possible.
						157/6/94Hernan1169269: replacing the eventX variables with
						functions.
						2   2/14/96Brian   removing mf::docontents
						2   4/12/96Hernan  Fixing method congruency problems.
						3   4/16/96brian   removing (nthitem call.
						4   9/30/96sidney  eventmode handlers must now return T to indicate that they handled the event
						5  12/17/96Brian Roddy
|# ;(do not edit past this line!!)
