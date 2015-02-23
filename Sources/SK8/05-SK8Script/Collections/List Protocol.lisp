;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;-----------------------------------------------------------------------------
; List PROTOCOL DEFINITION
;-----------------------------------------------------------------------------
#|
   List = {StructureMutableOrderedCollection+Discrete}

   The List state handler is a cons whose head is the index
   corresponding to the state and whose tail is the nthcdr
   corresponding to that index
|#

;-----------------------------------------------------------------------------
; Collection
;-----------------------------------------------------------------------------
(defmethod collect ((me (eql List))
                           &rest objects)
  (copy-list objects))

(define-handler initialVisitState (List)
  me)

(define-handler succeedingVisitState (List stateObj)
  (cdr stateObj))

(define-handler elementAtVisitState (List stateObj)
  (car stateObj))

(define-handler setElementAtVisitState (List stateObj newVal)
  (setf (car stateObj) newVal)
  me)

(define-handler removeVisitState (List stateObj)
  (cond
   ((null stateObj)
    me)
   ((eq stateObj me)
    (cdr (the list me)))
   (t
    (let ((prevState me)
          (curState (cdr (the list me))))
      (loop
        (when (eq curState stateObj) (return))
        (unless curstate (error "item not found"))
        (setq prevState curState
              curState (cdr curState)))
      (setf (cdr (the list prevState)) (cdr curState))
      me))))


(define-handler insertAtVisitState (List 
                                             stateObj
                                             obj
                                             &key (after nil))
  (cond
   ((null me)               ;;;empty list, so we need to make a new list
    (cons obj nil))
   ((and after stateObj)    ;;;after state is easy, we just set the cdr of the state
    (setf (cdr (the cons stateObj)) (cons obj (cdr stateObj)))
    me)
   ((or after (eq stateObj me)) ;;this is the insert at the front
    (let ((hd (car me)))
      (setf (car me) obj)
      (rplacd me (cons hd (cdr me))))
    me)
   ((null stateObj)             ;;;before an empty state means append to the end..
    (nconc me (list obj))
    me)
   (t                        ;;;otherwise find state and then shove it right before it.
    (let* ((prevState me)
           (st (cdr (the list me))))
      (loop
        (when (eq st stateObj) (return))
        (setq prevState st
              st (cdr st)))
      (setf obj (cons obj stateObj)
            (cdr prevState) obj)
      me))))

(define-handler indexAtVisitState (List stateObj)
  (1+ (search me stateObj :test #'eq)))

(define-handler visitStateAtIndex (List indexObj)
  (nthcdr (1- indexObj) me))

(define-handler isFinalVisitState (List stateObj)
  (equal stateObj (last me)))


#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 5/ 2/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
