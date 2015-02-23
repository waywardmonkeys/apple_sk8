;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "UNDOABLESETLOG")

(require "ANIMATEDCURSOR" "objects;Media:AnimatedCursor")
(require "YESORNODIALOGBOX" "objects;Dialogs:YesOrNoDialogBox")
(require "OBJECTDATARECT" "objects;Widgets:ObjectDataRect")

(define-handler undo :hidden (object) 
  )

(define-handler setInform (project objs propertyname) 
  (declare (ignore objs propertyname)))

(define-handler clearreferences (project &key ((:objects theobjects))) 
  (declare (ignore theobjects))
  )

(new Object :objectname "UndoableSetLog" :project sk8
     :properties '(objectlist propertyname valuelist))

(define-sk8-function UndoableSet nil (PropertyName objs value &key confirmation)
  (when (or (not confirmation)
            (YesOrNoDialog (if (= (length objs) 1)
                             (concatenate 'string "Set the " (simpleobjectstring PropertyName) 
                                          " of " (objectstring (car objs) :project (project (car objs))) " to " 
                                          (objectstring value :project (project (car objs))) "?")
                             (concatenate 'string "Set the " (simpleobjectstring PropertyName) 
                                          " of every item in " (objectstring objs :project (project (car objs))) " to " 
                                          (objectstring value :project (project (car objs))) "?"))
                           :cancel nil))
    (let (setter curval)
      (unless (listp objs)
        (setf objs (list objs)))
      (setf (objectlist UndoableSetLog) objs)
      (setf (PropertyName UndoableSetLog) PropertyName)
      (if (listp PropertyName)
        (progn
          (unless (= (length propertyname) (length value)) (error "There must be the same number of propertynames as there are object lists"))
          (setf (valuelist UndoableSetLog) nil)
          (withlockedcursor animatedclock
            (dolist (prop propertyname)
              (tickeventclock)
              (setf curval (pop value))
              (setf setter (or (gethash prop ccl::%setf-function-names%) 
                               (gethash prop ccl::%setf-methods%)))
              (push (mapcar #'(lambda (x) (funcall prop x)) objs) (valuelist UndoableSetLog))
              (mapcar #'(lambda (x) (funcall setter curval x)) objs)
              (sk8::setInform system objs prop))
            (setf (valuelist UndoableSetLog) (nreverse (valuelist UndoableSetLog)))
            ))
        (withlockedcursor animatedclock
          (setf setter (or (gethash PropertyName ccl::%setf-function-names%) 
                           (gethash PropertyName ccl::%setf-methods%)))
          (setf (valuelist UndoableSetLog) (mapcar #'(lambda (x) (progn (tickeventclock) (funcall PropertyName x))) objs))
          (mapcar #'(lambda (x) (funcall setter value x)) objs)
          (sk8::setInform system objs propertyname)))
      t
      )))

(define-sk8-function UndoLastSet nil ()
  (when (and (objectlist UndoableSetLog) (propertyName UndoableSetLog) (valuelist UndoableSetLog))
    (let* ((objs (objectlist UndoableSetLog))
           (propertyName (propertyName UndoableSetLog))
           (valuelist (sk8dev::valuelist UndoableSetLog))
           setter)
      (if (listp PropertyName)
        (progn
          (setf (valuelist UndoableSetLog) nil)
          (withlockedcursor animatedclock
            (dolist (prop propertyname)
              (setf setter (or (gethash prop ccl::%setf-function-names%) 
                               (gethash prop ccl::%setf-methods%)))
              (tickeventclock)
              (push (mapcar #'(lambda (x) (funcall prop x)) objs) (valuelist UndoableSetLog) )
              (mapcar #'(lambda (x y) (funcall setter y x)) objs (pop valuelist))
              (sk8::setInform system objs prop))
            (setf (valuelist UndoableSetLog) (nreverse (valuelist UndoableSetLog)))
            ))
        (withlockedcursor animatedclock
          (setf setter (or (gethash PropertyName ccl::%setf-function-names%) 
                           (gethash PropertyName ccl::%setf-methods%)))
          (setf (valuelist UndoableSetLog) (mapcar #'(lambda (x) (progn (tickeventclock) (funcall PropertyName x))) objs))
          (mapcar #'(lambda (x y) (funcall setter y x)) objs valuelist)
          (sk8::setInform system objs propertyname))))))

(defvar *miscelaneous-things-to-be-cleared-by-UndoableSetLog* nil)

(pushnew ObjectDataRect *miscelaneous-things-to-be-cleared-by-UndoableSetLog*)

(define-handler clearreferences (UndoableSetLog &key ((:objects theobjects)))
  (when theobjects
    (dolist (i (eventlisteners system)) (clearreferences i :objects theobjects)))
  (setf (objectlist me) nil)
  (setf (propertyname me) nil)
  (setf (valuelist me) nil)
  (setf (object objectdatarect) nil)
  (dolist (i *miscelaneous-things-to-be-cleared-by-UndoableSetLog*)
    (clearReferences i)))

#|

  (setf (objectlist UndoSlotLog) nil)
  (setf (propertyname UndoSlotLog) nil)
  (setf (valuelist UndoSlotLog) nil)

(new Object :objectname "UndoSlotLog" :project sk8
     :properties '(objectlist propertyname valuelist))
(defun LogSlots (objs propertyname)
  (setf (objectlist UndoSlotLog) objs)
  (setf (propertyName UndoSlotLog) propertyname)
  (setf (valuelist UndoSlotLog) (mapcar #'(lambda (x) (slot-value x propertyname)) objs)))
(defun UndoSlotSet ()
  (when (and (objectlist UndoSlotLog) (propertyName UndoSlotLog) (valuelist UndoSlotLog))
    (let* ((objs (objectlist UndoSlotLog))
           (propertyName (propertyName UndoSlotLog))
           (valuelist (valuelist UndoSlotLog)))
      (setf (valuelist UndoSlotLog) (mapcar #'(lambda (x) (slot-value x propertyname)) objs))
      (mapcar #'(lambda (x y) (setf (slot-value x propertyname) y)) objs valuelist)
      )))

|#
#|
	Change History (most recent last):
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	5  	 7/ 7/96	sidney  	changes for native PPC build
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
