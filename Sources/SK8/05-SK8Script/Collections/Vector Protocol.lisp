;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;-----------------------------------------------------------------------------
; Vector PROTOCOL DEFINITION
;-----------------------------------------------------------------------------
#|
Vector = {ContentMutableOrderedCollection + Discrete}
|#


#|

(defun removeVectorSubsequence (vect subStart subEnd dependentStates)
  (when dependentStates (updateVectorDependentStates subStart subEnd (- subStart subEnd) dependentStates))
  (shiftVectorEnd vect subEnd subStart))

(defun insertIntoVector (vect index newItem dependentStates)
  (when dependentStates (updateVectorDependentStates index index 1 dependentStates))
  (setq vect (shiftVectorEnd vect index (1+ index)))
  (setf (aref vect index) newItem)
  vect)

(define-handler insertInto (Vector newItem &optional dependentStates)
  (let ((newMe (insertIntoVector me (length me) newItem dependentStates)))
    (unless (eq newMe me)
      ;; Return {oldCollection . newCollection} to indicate change of eq'ness
      (cons me newMe))))

(define-handler insertBeforeCurrentItem (Vector state newItem &optional dependentStates)
  (let ((newMe (insertIntoVector me (or state (length me)) newItem dependentStates)))
    (unless (eq newMe me)
      ;; Return {oldCollection . newCollection} to indicate change of eq'ness
      (cons me newMe))))

(define-handler insertAfterCurrentItem (Vector state newItem &optional dependentStates)
  (let ((newMe (insertIntoVector me (if state (1+ state) 0) newItem dependentStates)))
    (unless (eq newMe me)
      ;; Return {oldCollection . newCollection} to indicate change of eq'ness
      (cons me newMe))))

(define-handler removeCurrentItem (Vector state &optional dependentStates)
  (let ((newMe (removeVectorSubsequence me state (1+ state) dependentStates)))
    (unless (eq newMe me)
      ;; Return {oldCollection . newCollection} to indicate change of eq'ness
      (cons me newMe))))

|#



;-----------------------------------------------------------------------------
; HANDLER DEFINITIONS
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Collection
;-----------------------------------------------------------------------------

#| collect (collectionType, {elt1, elt2 ... eltn}) => [Collection]
 ----------------------------------------------------------
   Required Handler

   Creates a 1-dimensional array (Vector), initialized with the
   objects provided.
|#
(defmethod collect ((me (eql Vector))
                           &rest objects)
  (apply #'cl::vector objects))

;;-----------------------------------------------------------------------------

;;initialVisitState is inherited from array
;;succeedingVisitState is inherited from array

(define-handler elementAtVisitState (Vector stateObj)
  (let ((n (1+ stateObj)))
    (when (<= n (length me))
      (aref me (1- n)))))


;-----------------------------------------------------------------------------
; ContentMutable
;-----------------------------------------------------------------------------


(define-handler setElementAtVisitState (Vector stateObj newVal)
  (setf (aref (the vector me) stateObj) newVal)
  me)

;-----------------------------------------------------------------------------
; Ordered
;-----------------------------------------------------------------------------

(define-handler indexAtVisitState (Vector stateObj)
  (1+ stateObj))

(define-handler visitStateAtIndex (Vector indexObj)
  (1- indexObj))


#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
