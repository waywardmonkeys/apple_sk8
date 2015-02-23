;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "MULTIOBJECTSTATECHECKBOX")

(require "CHECKBOX" "objects;Widgets:Checkbox")

;;______________________________________________________________________
;;______________________________________________________________________
;; MULTIOBJECTSTATECHECKBOX
;; This is a checkBox that links to a given boolean property on multiple objects.  Since the
;; state of the property may differ across the multiple objects, the checkBox may include
;; a third CHECKED state: :mixed.
;;
;; Usage:  First, the property is specified in the STATEPROPERTY property, then the
;; collection of objects is specified in the OBJECTS property.


#| ;*** NOTE!!!
The functionality this checkBox gives us should eventually be implemented by ports.
That is: a SINGLE property, <p1>, in an object ported to some property, <pj>, in
MULTIPLE objects (where there may be multiple, distinct <pj>'s, though the common
case would be the SAME property in all the multiple objects).

The port would have to "remember" the original value of each <pj> property (so that,
when all the <pj> properties don't start out with the same value -- i.e. they're
"mixed" -- the port can restore them all to their original state).  It would also
have to generate a "mixed" output value on the single object side whenever the values
of all the <pj> properties were not the same.
|#


(new CheckBox :objectName "MultiObjectStateCheckbox" :project SK8
     :properties '((stateProperty :value nil)
                   (objects :value nil)
                   (originalObjectStates :value
                    #.(make-array 0 :element-type 'bit :fill-pointer t))))

(setf (private MultiObjectStateCheckbox :property 'originalObjectStates) t)

(define-handler check (MultiObjectStateCheckbox)
  (let ((checked (checked me)))
    (setf (checked me) (cond
                        ;; Mixed to on
                        ((eq checked 'mixed) t)
                        ;; On to off
                        (checked nil)
                        ;; Off to mixed
                        (t 'mixed)))))

(define-handler (setf checked) (checkState MultiObjectStateCheckbox &key (update t))
  (let* ((originalObjectStates (originalObjectStates me))
         (allowsMixed? (not (eql 0 (length originalObjectStates))))
         (objs (objects me))
         (stateProperty (stateProperty me)))
    ;; Ensure checkState is a legal value
    (when (and checkState (or (neq checkState 'mixed) (not allowsMixed?)))
      (setq checkState t))
    ;; Update the object properties linked to this checkbox
    (when update
      (if (eq checkState 'mixed)
        (dovector (flag originalObjectStates) (setPropertyValue (pop objs) stateProperty (eql 1 flag)))
        (dolist (obj objs) (setPropertyValue obj stateProperty checkState))))
    ;; Update the graphical state and stored state
    (call-next-method checkState me)))


(define-handler (setf objects) (objects MultiObjectStateCheckbox)
  (setq objects (if (listp object)
                  objects
                  (list objects)))
  (setf (slot-value me 'objects) objects)
  (let ((stateProperty (stateProperty me))
        (originalObjectStates (originalObjectStates me))
        (numObjects (list-length objects))
        (objs objects)
        (allTrue? t)
        (allFalse? t))
    ;; Make the array's size correspond to the number of objects
    (when (> numObjects (array-dimension originalObjectStates 0))
      (setf originalObjectStates (make-array numObjects :element-type 'bit :fill-pointer t)
            (originalObjectStates me) originalObjectStates))
    (setf (fill-pointer originalObjectStates) numObjects)
    ;; Make the array's contents correspond to the objects' states
    (dotimes (i numObjects)
      (cond
       ((funcall stateProperty (pop objs))
        (setq allFalse? nil)
        (setf (aref originalObjectStates i) 1))
       (t
        (setq allTrue? nil)
        (setf (aref originalObjectStates i) 0))))
    ;; If all the objects' states match, empty out the originalObjectStates to indicate that
    (when (or allFalse? allTrue?) (setf (fill-pointer originalObjectStates) 0))
    ;; Make the check state match the overall state
    (setf (checked me :update nil) (cond (allTrue? t)
                                         (allFalse? nil)
                                         (t 'mixed)))
    objects))




#|
	Change History (most recent last):
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
