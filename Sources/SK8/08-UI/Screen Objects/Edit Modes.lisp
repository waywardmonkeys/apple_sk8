;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :UIDevelopment)

(new eventMode :objectName "UIEditMode" :project ui)


;;; We do not redefine handleUpdate...

;;; We do not redefine eventTick...


(define-handler handleIdle (UIEditMode)
  (if (eq (project (sk8dev::eventWindow)) ui)
    (call-next-method)
    t))

;; ________________________________________________________
;; HANDLEMOUSEDOWN
;; Exits the mode
;;;***** THIS IS KLUDGED LIKE CRAZY INVESTIGATE BUGS IN WINDOIDS AND ACTIVATION!!!!
(define-handler handleMouseDown (UIEditMode)
  (sk8-multival-bind (hh vv) (mouseloc stage)
    (let* ((act (actorathvcoordinates hh vv))
           (conts (sk8::containers act)))
      (cond
       ((eq act stage) (deselect selectionhalo) t)
       ((eq (project act) ui)
        (if (eq (sk8dev::eventWindow) selectionhalo)
          nil  ;; return nil to use normal mousedown handling of window under mouse
          (call-next-method)))
       (t                    ;;;(or (sleep (/ (#_GetDblTime) 70)) (down mouse)) 
        (if (or (memq act (selecteditems selectionhalo))
                (some #'(lambda (x) (memq x conts))
                      (selecteditems selectionhalo)))
          (setf act (selecteditems selectionhalo)))
        (unless (listp act) (setf act (list act)))
        (moveoffstage selectionhalo)
        (sendupdateevent (car act))
        (drag (car act) :otheractors (cdr act) :live t)
        (select act :extend (shiftkeydown))
        t)
       ))))

;; ________________________________________________________
;; handleMouseUp

(define-handler handleMouseUp (UIEditMode)
  (sk8-multival-bind (hh vv) (mouseloc stage)
    (let* ((act (actorathvcoordinates hh vv)))
      (if (eq (project act) ui)
        (call-next-method)
        t))))

;; ________________________________________________________
;; KeyEvents

(define-handler handleautoKey (UIEditMode)
  (handleKeyDown me))

(define-handler handleKeyUp (UIEditMode)
  (if (eq (project (sk8dev::eventWindow)) ui)
    (call-next-method)
    t))

(define-handler handleKeyDown (UIEditMode)
  (if (eq (currenteventkey) #\escape)
    (progn
      (exitmode me)
      t)
    (if (eq (project (sk8dev::eventWindow)) ui)
      (call-next-method)
      t)))

;; ________________________________________________________
;; handleOther
;; Make sure we have no spurious events...

(define-handler handleOther (UIEditMode)
  (if (eq (project (sk8dev::eventWindow)) ui)
    (call-next-method)
    t))



#|
	Change History (most recent last):
	1	2/25/94	rod	
	2	3/21/94	rod	
	3	3/21/94	rod	
	4	3/22/94	rod	
	5	4/18/94	rod	Clean UP
	6	4/26/94	rod	Fixing halo in edit Mode...
	7	4/26/94	rod	
	8	5/26/94	rod	
	9  	 9/21/94	rod     	
	2  	 8/ 9/95	Brian   	
	2  	 9/30/96	sidney  	eventmode handlers must now return T to indicate that they handled the event
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
