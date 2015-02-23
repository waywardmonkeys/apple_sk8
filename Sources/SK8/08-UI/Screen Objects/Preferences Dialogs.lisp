;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :UIDev)

(new imageRenderer :objectname "HaloPrefsPict" :project ui)
(new qdPicture :objectName "HaloPrefsPictRSRC" :project ui)
(setf (file HaloPrefsPictRSRC) (file ui))
(setf (resourceid HaloPrefsPictRSRC) 130)
(setf (media HaloPrefsPict) HaloPrefsPictRSRC)
(setf (renderstyle HaloPrefsPict) 'renderUnstretched)

(new dialogbox :objectname "HaloPreferencesDialog" :Project ui)
;;(setf (fillcolor HaloPreferencesDialog) white)
(addproperty HaloPreferencesDialog 'item)
(define-handler resized (HaloPreferencesDialog)
  (let (hSize vSize)
    (declare (special hSize vSize))
    (sk8-multival-setf (hSize vSize) (size me))
    (mapc #'bestsize (contents me))
    ))


(new uibiglabel :objectname "HaloPrefsTitle" :Project ui)
(setf (fillcolor HaloPrefsTitle) (fillcolor HaloPreferencesDialog))
(setf (text HaloPrefsTitle) "Selection Control Preferences:")
(setf (textsize HaloPrefsTitle) 12)
(setf (textfont HaloPrefsTitle) ChicagoFont)
(setf (container HaloPrefsTitle) HaloPreferencesDialog)
(define-handler bestsize (HaloPrefsTitle)
  (setf (left me :resizing nil) 7)
  (setf (top me :resizing nil) 10)
  )


(new radiobutton :objectname "HPrefsRadioButton" :Project ui)
(addproperty HPrefsRadioButton 'value)
(setf (textfont HPrefsRadioButton) ChicagoFont)
(setf (textcolor HPrefsRadioButton) Black)
(setf (textsize HPrefsRadioButton) 12)
(define-handler check (HPrefsRadioButton)
  (setf (item HaloPreferencesDialog) (value me))
  (call-next-method)
  )

(new HPrefsRadioButton :objectname "EditOffRadioButton" :Project ui)
(setf (container EditOffRadioButton) HaloPreferencesDialog)
(setf (value EditOffRadioButton) t)
(setf (text EditOffRadioButton) "Active Objects & Edit Control")
(define-handler bestsize (EditOffRadioButton)
  (setf (left me :resizing nil) 15)
  (setf (top me :resizing nil) 35)
  )
(define-handler check (EditOffRadioButton)
  (withactorlocked (me)
    (setf (text HPHaloInfo) 
          "Button is active.  Select with the Selection Tool or by Control Clicking.")
    (setf (checked HPDragFrame) t)
    (setf (checked HPObjectTitle) t)
    (setf (checked HPResizeHandles) t)
    (show SCHiBox)
    (call-next-method)
    ))


(new HPrefsRadioButton :objectname "EditOnRadioButton" :Project ui)
(setf (container EditOnRadioButton) HaloPreferencesDialog)
(setf (value EditOnRadioButton) t)
(setf (text EditOnRadioButton) "Edit Control Only")
(define-handler bestsize (EditOnRadioButton)
  (setf (left me :resizing nil) 245)
  (setf (top me :resizing nil) 35)
  )
(define-handler check (EditOnRadioButton)
  (withactorlocked (me)
    (setf (text HPHaloInfo) 
          "Button is not active.  Select by clicking.  Drag with mousedown.")
    (setf (checked HPDragFrame) nil)
    (setf (checked HPObjectTitle) nil)
    (setf (checked HPResizeHandles) t)
    (hide SCHiBox)
    (call-next-method)
    ))


(new label :objectname "HaloPrefsShow" :Project ui)
(setf (fillcolor HaloPrefsShow) (fillcolor HaloPreferencesDialog))
(setf (text HaloPrefsShow) "Show:")
(setf (container HaloPrefsShow) HaloPreferencesDialog)
(define-handler bestsize (HaloPrefsShow)
  (setf (left me :resizing nil) 7)
  (setf (top me :resizing nil) 220)
  )


(new checkbox :objectname "HPResizeHandles" :Project ui)
(setf (container HPResizeHandles) HaloPreferencesDialog)
(setf (text HPResizeHandles) "Resizing Handles")
(define-handler bestsize (HPResizeHandles)
  (setf (left me :resizing nil) 15)
  (setf (top me :resizing nil) 240)
  )

(new checkbox :objectname "HPDragFrame" :Project ui)
(setf (container HPDragFrame) HaloPreferencesDialog)
(setf (text HPDragFrame) "Drag Frame")
(define-handler bestsize (HPDragFrame)
  (setf (left me :resizing nil) 15)
  (setf (top me :resizing nil) 260)
  )

(new checkbox :objectname "HPObjectTitle" :Project ui)
(setf (container HPObjectTitle) HaloPreferencesDialog)
(setf (text HPObjectTitle) "Object Title")
(define-handler bestsize (HPObjectTitle)
  (setf (left me :resizing nil) 15)
  (setf (top me :resizing nil) 280)
  )

(new rectangle :objectname "SCHiBox" :Project ui)
(setf (fillcolor SCHiBox) (fillcolor HaloPreferencesDialog))
(setf (framesize SCHiBox) '(0 0))
(setf (mousesensitivity SCHiBox) 'transparent)
(setf (container SCHiBox) HaloPreferencesDialog)
(setboundsrect SCHiBox 200 200 500 500)
(sendtoback SCHiBox)

(new label :objectname "HaloPrefsSelHi" :Project ui)
(setf (fillcolor HaloPrefsSelHi) (fillcolor HaloPreferencesDialog))
(setf (text HaloPrefsSelHi) "Outline Item to be Selected:")
(setf (container HaloPrefsSelHi) SCHiBox)
(setf (left HaloPrefsSelHi :resizing nil) 0)
(setf (top HaloPrefsSelHi :resizing nil) 20)

(new radioButton :objectname "SCHOnKey" :Project ui)
(setf (container SCHOnKey) SCHiBox)
(setf (text SCHOnKey) "When Spacebar is Pressed")
(setf (left SCHOnKey :resizing nil) 15)
(setf (top SCHOnKey :resizing nil) 40)

(new radioButton :objectname "SCHAlways" :Project ui)
(setf (container SCHAlways) SCHiBox)
(setf (text SCHAlways) "Always")
(setf (left SCHAlways :resizing nil) 15)
(setf (top SCHAlways :resizing nil) 60)

(new rectangle :objectname "HPHaloPict" :Project ui)
(setf (container HPHaloPict) HaloPreferencesDialog)
(setf (framesize HPHaloPict) '(0 0))
(setf (mousesensitivity HPHaloPict) 'transparent)
(setf (fillcolor HPHaloPict) HaloPrefsPict)
(define-handler bestsize (HPHaloPict)
  (setboundsrect me 38 70 260 202)  )
(sendtoback HPHaloPict)

(new edittext :objectname "HPHaloInfo" :Project ui)
(setf (container HPHaloInfo) HaloPreferencesDialog)
(setf (framesize HPHaloInfo) '(0 0))
(setf (fillcolor HPHaloInfo) (fillcolor HaloPreferencesDialog))
(setf (textsize HPHaloInfo) 9)
(setf (textfont HPHaloInfo) "Geneva")
(locktext HPHaloInfo)
(define-handler bestsize (HPHaloInfo)
  (setboundsrect me 262 109 393 202)  )
(bringtofront HPHaloPict)


(check EditOffRadioButton)

;;;============================================================
;;;============================================================
;;;===The Buttons
;;;(resized HaloPreferencesDialog)

(new DialogBoxCancelButton :objectname "HaloPrefsCancel" :Project ui)
(setf (container HaloPrefsCancel) HaloPreferencesDialog)

(define-handler bestsize (HaloPrefsCancel)
  (sk8-multival-bind (hh vv) (size HaloPreferencesDialog)
    (setboundsrect me (- hh 168) (- vv 29) (- hh 98) (- vv 11))))


(new DialogBoxHighlightedButton :objectname "HaloPrefsOKButton" :Project ui)
(setf (container HaloPrefsOKButton) HaloPreferencesDialog)
(setf (text HaloPrefsOKButton) "OK")

(define-handler bestsize (HaloPrefsOKButton)
  (sk8-multival-bind (hh vv) (size HaloPreferencesDialog)
    (setboundsrect me (- hh 88) (- vv 33) (- hh 10) (- vv 7))))
(define-handler click (HaloPrefsOKButton)
  (if (and (checked EditOffRadioButton) (eq uieditmode (cadr (currenteventmodes))))
    (sk8dev::eval-in-new-thread "Halo Prefs OK Edit Off" `(exitmode uieditmode))
    (if (and (checked EditOnRadioButton) (neq uieditmode (cadr (currenteventmodes))))
      (sk8dev::eval-in-new-thread "Halo Prefs OK Edit On" `(entermode uieditmode))))
  (when (and (memq pickactormode (currenteventmodes)) (checked EditOffRadioButton) (neq *AlwaysHighlightOnSelect* (checked SCHAlways)))
    (setf (container pickactormoderect) (if (checked SCHAlways) stage nil))
    (setf *AlwaysHighlightOnSelect* (checked SCHAlways))
    )
  (sk8dev::eval-in-new-thread "Halo Prefs OK" `(let ((xx (selecteditems selectionhalo)))
                   (deselect selectionhalo)
                   (when ,(checked EditOffRadioButton) (setf *AlwaysHighlightOnSelect* ,(checked SCHAlways)))
                   (setf (showsdragger SelectionHalo) ,(checked HPDragFrame))
                   (setf (showstitle SelectionHalo) ,(checked HPObjectTitle))
                   (setf (showsresizer SelectionHalo) ,(checked HPResizeHandles))
                   (select xx)))
  (exitmodalstate t)
  )


;;(setf (container HaloPreferencesDialog) stage)
(setf (container HaloPreferencesDialog) nil)
(setf (size HaloPreferencesDialog) '(420 340))
(resized HaloPreferencesDialog)


(defun HaloPrefsDialog nil ()
       (setf (location HaloPreferencesDialog) (mainmonitorcenter))
       (if *AlwaysHighlightOnSelect*
         (check SCHAlways)
         (check SCHOnKey)
         )
       (if (eq uieditmode (activeMode))
         (check EditOnRadioButton)
         (check EditOffRadioButton))
       (setf (checked HPDragFrame) (showsdragger SelectionHalo))
       (setf (checked HPObjectTitle) (showstitle SelectionHalo))
       (setf (checked HPResizeHandles) (showsresizer SelectionHalo))
       (modaldialog HaloPreferencesDialog))

;;;______________________________________________________________________________________
;;;______________________________________________________________________________________
;;;______________________________________________________________________________________
;;;______________________________________________________________________________________
;;;______________________________________________________________________________________
;;;______________________________________________________________________________________



(new imageRenderer :objectname "WindowPrefsPict" :project ui)
(new qdPicture :objectName "WindowPrefsPictRSRC" :project ui)
(setf (file WindowPrefsPictRSRC) (file ui))
(setf (resourceid WindowPrefsPictRSRC) 131)
(setf (media WindowPrefsPict) WindowPrefsPictRSRC)
(setf (renderstyle WindowPrefsPict) 'renderUnstretched)

(new dialogbox :objectname "WindowPreferencesDialog" :Project ui)
;;(setf (fillcolor WindowPreferencesDialog) white)
(addproperty WindowPreferencesDialog 'item)
(addproperty WindowPreferencesDialog 'styles)
(define-handler resized (WindowPreferencesDialog)
  (let (hSize vSize)
    (declare (special hSize vSize))
    (sk8-multival-setf (hSize vSize) (size me))
    (mapc #'bestsize (contents me))
    ))


(new uibiglabel :objectname "WindowPrefsTitle" :Project ui)
(setf (fillcolor WindowPrefsTitle) (fillcolor WindowPreferencesDialog))
(setf (text WindowPrefsTitle) "Window Style Preferences:")
(setf (container WindowPrefsTitle) WindowPreferencesDialog)
(define-handler bestsize (WindowPrefsTitle)
  (setf (left me :resizing nil) 7)
  (setf (top me :resizing nil) 10)
  )


(new rectangle :objectname "WPWindowPict" :Project ui)
(setf (container WPWindowPict) WindowPreferencesDialog)
(setf (framesize WPWindowPict) '(1 1))
(setf (framecolor WPWindowPict) uirectangleoutbevel)
(setf (mousesensitivity WPWindowPict) 'transparent)
(setf (fillcolor WPWindowPict) WindowPrefsPict)
(define-handler bestsize (WPWindowPict)
  (if (styles WindowPreferencesDialog)
    (setboundsrect me 20 40 362 235)
    (setboundsrect me 20 40 362 155)
    ))
(sendtoback WPWindowPict)


(new rectangle :objectname "WPWindowButton" :Project ui)
(setf (framesize WPWindowButton) '(0 0))
(setf (fillcolor WPWindowButton) transparent)
(addproperty WPWindowButton 'styles)
(define-handler mousedown (WPWindowButton)
  (withactorlocked (me)
    (when (item WindowPreferencesDialog)
      (setframesize (item WindowPreferencesDialog) 0 0)
      (setf (fillcolor (item WindowPreferencesDialog)) transparent))
    (setf (item WindowPreferencesDialog) me)
    (setf (fillcolor me) slightlylighter)
    (setframesize me 3 3)))

(new WPWindowButton :project ui :styles uiGreentextures :container WPWindowPict)
(new WPWindowButton :project ui :styles uiBluetextures :container WPWindowPict)
(new WPWindowButton :project ui :styles uiyellowtextures :container WPWindowPict)
(new WPWindowButton :project ui :styles uiGraytextures :container WPWindowPict)
(new WPWindowButton :project ui :styles uiPinktextures :container WPWindowPict)
(setf (styles (new WPWindowButton :project ui :container WPWindowPict)) :macstyle)

(dolist (i (contents WPWindowPict)) (setsize i 66 85))
(dolist (i (contents WPWindowPict)) (setf (framecolor i) framescore))
(dolist (i (contents WPWindowPict)) (setf (fillcolor i) transparent))

(tile (reverse (contents WPWindowPict)) :columns 5 :hspacing 0 :vspacing 20 :boundsrect '(9 23 0 0))
(setboundsrect (car (contents WPWindowPict)) 0 0 0 -20 :relative t)


;;;============================================================
;;;============================================================
;;;===The Buttons
;;;(resized WindowPreferencesDialog)

(new DialogBoxCancelButton :objectname "WindowPrefsCancel" :Project ui)
(setf (container WindowPrefsCancel) WindowPreferencesDialog)

(define-handler bestsize (WindowPrefsCancel)
  (sk8-multival-bind (hh vv) (size WindowPreferencesDialog)
    (setboundsrect me (- hh 168) (- vv 29) (- hh 98) (- vv 11))))


(new DialogBoxHighlightedButton :objectname "WindowPrefsOKButton" :Project ui)
(setf (container WindowPrefsOKButton) WindowPreferencesDialog)
(setf (text WindowPrefsOKButton) "OK")

(define-handler bestsize (WindowPrefsOKButton)
  (sk8-multival-bind (hh vv) (size WindowPreferencesDialog)
    (setboundsrect me (- hh 88) (- vv 33) (- hh 10) (- vv 7))))
(define-handler click (WindowPrefsOKButton)
  (if (eq (styles (item WindowPreferencesDialog)) :macstyle)
    (unless (macstyleinterface ui) (sk8dev::eval-in-new-thread "Window Macstyle Prefs OK" '(setf (MacStyleInterface ui) t)))      
    (sk8dev::eval-in-new-thread "Window Prefs OK"
                                `(sk8dev::withlockedcursor animatedClock 
                                   (setf (MacStyleInterface ui) nil)      
                                   (loadtexture ,(styles (item WindowPreferencesDialog))))))
  (exitmodalstate t)
  )


;;(setf (container WindowPreferencesDialog) stage)
(setf (container WindowPreferencesDialog) nil)
(setf (size WindowPreferencesDialog) '(400 300))
(resized WindowPreferencesDialog)

(define-handler bestsize (WindowPreferencesDialog)
  (if (styles me)
    (setsize me 400 285)
    (setsize me 400 210))
  (resized me))

(defun WindowPrefsDialog ()
  (setf (styles WindowPreferencesDialog) (or (MacStyleInterface ui) (optionkeydown)))
  (bestsize WindowPreferencesDialog)
  (setf (location WindowPreferencesDialog) (mainmonitorcenter))
  (if (MacStyleInterface ui)
    (mousedown (car (contents WPWindowPict)))
    (dolist (i (cdr (contents WPWindowPict))) 
      (if (eq (media UITopLeft) (car (media (styles i)))) (mousedown i))))
  (modaldialog WindowPreferencesDialog))


;;;(progn (sleep 1) (WindowPrefsDialog))




#|
	Change History (most recent last):
	1	5/26/94	rod	
	2	6/1/94	rod	
	3	6/1/94	rod	Minor fixes
	4	6/4/94	rod	
	5	6/16/94	rod	
	6  	 8/31/94	Hernan  	Chicago -> ChicagoFont.
	7  	12/16/94	rod     	Changing wording.
	8  	 1/16/95	rod     	making the selection dialog work nicely if the
							user is already in pickactormode.
	9  	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	10 	 2/17/95	rod     	fixing size of dialog.
	2  	 7/10/95	Brian Roddy	removing sk8dev::eval-in-new-threads
	3  	 2/14/96	Brian   	removing mf::docontents
	2  	11/12/96	sidney  	added a name argument to eval-in-new-thread
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
