;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

;; PickActorMode
;; A mode that will pick an actor.

(new eventMode :objectName "PickActorMode" :project ui
     :properties '(cursor
                   EnteredWithControlKey
                   FrontWindow
                   MouseLoc
                   SelectedItem
                   ItemOver))

(setf (cursor PickActorMode) cursorCrossHair)

(new halo :objectName "PickActorModeRect" :project ui)
(setf (insetSize PickActorModeRect) '(3 3))
(Setf (fillColor PickActorModeRect) warningcolor)
(setf (mouseSensitivity PickActorModeRect) 'transparent)
(setf (floating PickActorModerect) t)
(setf (colordepth PickActorModerect) nil)


(defparameter *AlwaysHighlightOnSelect* nil)

;;;
;;;  Should replace this based on the args to userpickactor...
;;;

(defun OKTOSELECT? (tla)
  (progn
    (setf tla (sk8::window tla))
    (or
     (and (eq tla stage) (gs:findbackground))
     (or (neq (project tla) ui)  ; ;;(memq (project tla) (okprojects (targetproject ui)))
         (and SS::!*lisp-dev-mode* (optionkeydown) (eq (targetproject ui) sk8))))))

;; ________________________________________________________
;; Activate and Deactivate
;; Do basic setup...

(define-handler activate (PickActorMode)
  (moveoffstage PickActorModeRect)
  (when *AlwaysHighlightOnSelect*
    (setf (busy selectionhalo) t)
    (setf (container PickActorModeRect) stage))
  (setf (SelectedItem me) nil)
  (if (eq (currenttool drawpalette) SelectTool) 
    (setf (cursor PickActorMode) cursorSelectionCrossHair)
    (setf (cursor PickActorMode) cursorCrossHair))
  (setf (ItemOver me) nil)
  (unless (eq (currenttool drawpalette) selecttool)
    (moveoffstage selectionhalo))
  )

(define-handler deactivate (PickActorMode)
  (setf (container PickActorModeRect) nil)
  (when (frontwindow me) (unlock (frontwindow me)))
  (setf (frontwindow me) nil)
  (setf (cursor stage) standardcursor)
  (setf (enteredWithControlKey me) nil)
  (setf (busy selectionhalo) nil)
  )

;;; We do not redefine handleUpdate...

(define-handler eventTick (PickActorMode))

;; ________________________________________________________
;; handleIdle
;; Here we do tracking and hilight our selecteditem....
(define-handler handleIdle (PickActorMode)
  (setf (cursor stage) (cursor me))
  (if (and (enteredwithcontrolkey me) (not (controlkeydown)))
    (progn
      (setf (selecteditem me) 'NothingClicked)
      (exitModalState 'NothingClicked))
    (sk8-multival-bind (xx yy) (mouseloc stage)
      (let ((curActor (actorAtHvCoordinates xx yy))
            (ItemOver (ItemOver me)))
        (if (eq curActor PickActorModeRect) 
          (moveoffstage PickActorModeRect)
          (let ((x (selecteditem me)))
            (when (neq curActor ItemOver)
              (if (OKTOSELECT? curActor)
                (progn
                  (setf (selectedItem me) curActor)
                  (setf (ItemOver me) curActor))
                (progn
                  (setf (selectedItem me) nil)
                  (setf (ItemOver me) nil))))
            (when (neq x (selecteditem me)) (surroundobject PickActorModeRect (selecteditem me)))
            )))))
  t)

;; ________________________________________________________
;; HANDLEMOUSEDOWN
;; Exits the mode

(define-handler handleMouseDown (PickActorMode)
  (setf (MouseLoc me) (list (eventh) (eventv)))
  (handleidle me) ;;; just to make sure we are over the right actor.
  (exitmodalstate (selecteditem me))
  t
  )

;; ________________________________________________________
;; handleMouseUp
;; Should never be encountered, but if it is, it is ignored.

(define-handler handleMouseUp (PickActorMode)
  (declare (ignore me))
  t)

;; ________________________________________________________
;; KeyEvents
;; Keys can be used to give hints to the tracking mechanism...

(define-handler handleautoKey (PickActorMode)
  (handleKeyDown me))

(define-handler handleKeyUp (PickActorMode)
  (declare (ignore me))
  t)

(define-handler handleKeyDown (PickActorMode)
  (let* ((thekey (gs:getEventKey))
         (theActor (selectedItem me))
         newActor)
    (if (eq TheKey #\delete)
      (let ( Thetool)
        (when (and  (currentTool DrawPalette) (eq (container(currentTool DrawPalette)) userpalette))
          (setf thetool (currenttool DrawPalette)))
        (when (and thetool (not (objectname thetool)))
          (setf (stuckDown thetool) nil)
          (setf (inputobjects userpalette) (delete (actortodraw thetool) (inputobjects userpalette)))
          (setf (selectedItem me) 'NothingClicked)
          (exitModalState 'NothingClicked)))
      (progn
        (unless (eq (container pickactormoderect) stage) 
          (setf (container pickactormoderect) stage))
        (when (or theActor (eq thekey #\Escape))
          (case theKey
            (#\upArrow
             ;; select container
             (setq newActor (container theActor))
             (if (eq newActor stage)
               (setq newActor nil)))
            (#\DownArrow
             ;; select first item in contents
             (let* ((theItem theActor)
                    (theProject (or (project theItem) (targetproject ui))))
               (setq newActor 
                     (if (eq theactor stage)
                       (car (delete-if-not #'(lambda (i) (eq (project i) theProject)) (contents stage)))
                       (car (contents theActor))))))
            (#\ForwardArrow
             ;; select next object
             (let* ((theItem theActor)
                    (theProject (project theItem)))
               (setf newActor 
                     (find-next-sibling-that theItem #'(lambda (i) (and (neq i pickactormoderect)
                                                                        (eq (project i) theProject))))))
             )
            (#\BackArrow
             ;; select previous object
             (let* ((theItem theActor)
                    (theProject (project theItem)))
               (setf newActor 
                     (find-prev-sibling-that theItem #'(lambda (i) (eq (project i) theProject)))))
             )
            (#\Escape                         ;; get out of the mode
             (setf (selectedItem me) 'NothingClicked)
             (exitModalState 'NothingClicked))
            (#\Return                       ;;;; Enter or return act like mousedown...
             (handlemousedown me))
            (#\Enter
             (handlemousedown me)))
          (when newActor
            (setf (selecteditem me) newactor)
            (surroundobject PickActorModeRect (selecteditem me))
            (handleidle me)))))
    t))

;; ________________________________________________________
;; handleOther
;; Make sure we have no spurious events...

(define-handler handleOther (PickActorMode)
  (declare (ignore me))
  t)

;;MCL3.0
(defun userPickActor ()
  (let* ((result (enterModalState PickActorMode))
         (xx (car (mouseloc PickActorMode)))
         (yy (cadr (mouseloc PickActorMode)))
         theactor)
    (#_FlushEvents #$everyEvent 0)
    (deactivate PickActorMode)  ;;; This is just to make sure it gets called!!!
    (cond
     ((eq result nil)
      (setf theactor (actorAtHVCoordinates xx yy))
      (if (or (eq theactor selectionhalo)
              (eq (container theactor) selectionhalo)
              (inheritsfrom theactor DrawPaletteTools))
        (mousedown theactor))
      (sk8-multivals nil nil nil))
     ((and (neq result 'NothingClicked) (or (eq result stage) (is-a result actor)))
      (when (and (neq result stage) (not (memq (project (sk8::window result)) (okprojects (targetproject ui)))))
        (process-run-function
         '(:name "Setting Target Project" :background-p t)
         #'(lambda (proj)
             (setf (targetproject ui) proj))
         (project (sk8::window result))))
      (sk8-multivals result xx yy))
     (t 
      (sk8-multivals nil nil nil))
     )))

#|
	Change History (most recent last):
	1	10/1/93	rod	
	2	10/1/93	rod	Removed Node References and Added With-fast-slots.
	3	10/6/93	hernan	Use object-under-mouse to get the current
				object during event modes.
	4	10/19/93	rod	Simplified by calling next method in handlepointerdown
	5	10/19/93	rod	
	8	11/6/93	rod	
	9	11/19/93	rod	
	10	11/19/93	rod	
	11	11/19/93	hernan	
	12	11/19/93	kleiman	
	13	11/23/93	rod	
	14	11/29/93	rod	
	15	11/29/93	rod	Fixed bug due to 'NothingClicked not being sent
				properly.
	16	12/6/93	rod	
	17	12/8/93	rod	
	19	2/12/94	kleiman	name changes
	20	2/18/94	rod	Removed extraneous calls to surroundobject in
				handleidle as this conses.
	21	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	22	2/21/94	hernan	window -> sk8::window.
	23	2/25/94	rod	
	24	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	24	2/25/94	hernan	
	25	2/26/94	rod	
	26	3/10/94	rod	
	27	3/11/94	rod	
	28	3/16/94	rod	Fixing various problems...
	29	3/17/94	rod	various fixups...
	30	3/29/94	rod	
	31	3/29/94	rod	Making userpickactor a bit more robust.
	32	3/30/94	Hernan	
	33	4/13/94	Hernan	Avoiding use of contents when it is not necessary.
	34	5/4/94	rod	
	35	5/4/94	rod	
	36	6/3/94	rod	
	37	6/16/94	rod	
	38	7/20/94	rod	Fixing selection bug.
	39 	 8/23/94	rod     	
	40 	 8/23/94	rod     	
	41 	11/ 1/94	rod     	giving selection a unique cursor.
	42 	12/16/94	rod     	
	2  	 4/16/96	brian   	removing (nthitem call.
	3  	 9/30/96	sidney  	eventmode handlers must now return T to indicate that they handled the event
	4  	11/12/96	sidney  	name formerly anonymous process
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
