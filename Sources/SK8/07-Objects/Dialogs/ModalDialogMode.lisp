;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :SK8Development)

(provide "MODALDIALOGMODE")

(SK8-declare-syms :SK8 :public ; Updated  3-24-94  12:35 am
                  SK8::DISABLEDMENUS)

#| Modification History

03-10-93 brian -> nesting of modal dialogs now allowed.
01-22-93 ruben id -> objectName

|#

;;;______________________________________________________________________
;;;                                   modalDialog
;;;                    IMPLEMENTS A MODAL DIALOG USING EVENT MODES...
;;;______________________________________________________________________

(new eventMode
     :objectname "ModalDialogMode"
     :undisposable t
     :properties '((theDialog :value nil)
                   (canceled :value nil)
                   (returnValue :value nil)
                   (savedCursor :value nil)
                   (disabledMenus :value nil))
     :project sk8)

;; event handlers. They return T if they handle the event, NIL to let normal event processing happen

(define-handler handleActivate (modalDialogMode)
  t)

(define-handler handleMouseDown (modalDialogMode)
  (when (neq (sk8::window (gs:object-under-mouse)) (theDialog me))
    (ed-beep)
    t))

(define-handler handleMouseUp (modalDialogMode)
  (neq (sk8::window (gs:object-under-mouse)) (theDialog me)))

(define-handler handleKeyDown (modalDialogMode)
  (let ((chr (CurrentEventKey)))
    (if (and chr (char= chr #\escape))
      (abortevent)
      (call-next-method))
    ))

;;; me = a modalDialogMode.

(defun disable-menus (me)
  (when (sk8::menubar stage)
    (let ((theMenus (menus (sk8::menubar stage))))
      (gs:withLockedMenubar 
        (dolist (c theMenus)
          (when (enabled c) 
            (if (string-equal (text c) "Edit")
              (pushnew (disable-non-standard-menu-items c) (disabledMenus me))
              (progn (setf (enabled c) nil)
                     (pushnew c (disabledMenus me)))))))))
  ;; Disable the apple menu.
  (menu-disable *apple-menu*))

(define-handler activate (modalDialogMode)
  (unless (inheritsFrom (previousEventMode) modalDialogMode)
    (setf (savedCursor me) (cursor Stage))
    (disable-menus me))
  (bringUp (theDialog me)))

(defun reenable-menus (theMode)
  (when (disabledMenus theMode)
    (gs:withLockedMenubar 
      (dolist (c (disabledMenus theMode))
        (if (listp c)
          (dolist (mItem c)
            (setf (enabled mItem) t))
          (setf (enabled c) t)))))
  ;; Enable the apple menu.
  (menu-enable *apple-menu*))

(define-handler deactivate (modalDialogMode)
  (setf (container (theDialog me)) nil)
  (let ((oldcursor (savedCursor me)))
    (when oldcursor (setf (cursor Stage) oldcursor)))
  (reenable-menus me)
  (setf (disabledMenus me) nil))

(define-sk8-function modalDialog nil (dialog)
  (let ((TheMode (new ModalDialogMode :project (project dialog))))
    (setf (theDialog TheMode) dialog)
    (unwind-protect
      (progn
        (setf (floating dialog) t)
        (enterModalState TheMode))
      (setf (floating dialog) nil))
    ))

#|
	Change History (most recent last):
	7	1/11/94	hernan	self -> me
	8	2/12/94	kleiman	name changes
	9	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	10	2/21/94	hernan	window -> sk8::window.
	11	2/28/94	hernan	No need to dispose the mode at the end since
				it just goes away!
	12	3/18/94	Hernan	Just cleaned up the code a bit.
	13	3/18/94	Hernan	Ooops! Left over a print statement.
	14	3/24/94	Hernan	When a modal dialog comes up, we disable all
				menus in the menubar when the menubar is on
				the stage (if it is on its own window this is not
				necessary since the dialog mode will not let users
				get to it).
	15	3/24/94	Hernan	If modal dialogs are nested, this menu disabling
				stuff should not be repeated.
	16	3/24/94	Hernan	Ooops. Forgot to remove a print statement.
	17	6/27/94	rod	Making dialogs float when put on stage and
				not float when they are taken off.
	18	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	19	8/10/94	Hernan	1173788: disable-menus should also disable the
				apple menu.
	20 	 8/31/94	rod     	
	21 	 9/ 9/94	sidney  	modaldialog saves and restores cursor
	22 	 9/20/94	Hernan  	1186830: we make the dialog floating before we
							ever add it to the Stage. It then leaves the Stage
							and then stops being floating.
	23 	 9/22/94	rod     	Added escape key to abort.
	24 	 9/27/94	Hernan  	Using the object under the mouse instead of the
							eventActor to check things while in the middle of
							the eventmode.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 9/30/96	sidney  	eventmode handlers must now return T to indicate that they handled the event
	5  	 2/27/97	Hernan  	
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
