;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  4-20-96  12:18 pm
                  sk8::TABLE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The Table object
;;;

(new Collection :objectname "Table" :project SK8
     :properties '(data)) ; *** 'data' should be a "private" property!!!

(define-handler keys (Table)
  (mapcar #'car (data me)))

;; For completeness we'll also provide one for HASHTABLE
(defmethod keys ((tbl hash-table) &key)
  (let ((result (list)))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (setq result (nconc result (list key))))
             tbl)
    result))


#|
*** NEED TO MAKE hash-table ITSELF FOLLOW THE COLLECTION PROTOCOL (SINCE SOME TABLES -- E.G.
    PROJECT'S OBJECTTABLE) ARE DIRECTLY hash-tables, NOT Tables!!!

Table           - has an a-list as its guts, and uses = as its test
   TypeTable    - has an a-list as its guts, and uses is-a as its test, + keeps sorted most-specific 1st
   ObjectTable  - has either an a-list or a hash-table as its guts, and uses EQ as its test


Tables of the following form

   {'sym1': 43, 'sym2': "Boo"}

have special case reading/writing so that they're shown/read like an AppleScript "record":

   {sym1: 43, sym2: "Boo"}

|#




#|




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The basic portion of the Collection Protocol for Table...
;;;

(define-handler initialState (Table)
  (data me))
(define-handler nextState (Table state)
  (cdr state))
(define-handler finalState (Table)
  (last (data me)))
(define-handler currentItem (Table state)
  (cdr (car state)))

;; An addition to the collection protocol to permit listing a Table's keys
(define-handler currentKey (Table state)
  (car (car state)))

;;; inherits copyState and stateEqual from Collection

(define-handler stateGreaterThan (Table state1 state2)
  (stateGreaterThan (data me) state1 state2))

;;; inherits collectionStructureShared from Collection

(define-handler numItems (Table fromState toState)
  (numItems (data me) fromState toState))
(define-handler empty (Table)
  (null (data me)))

;;; inherits containsSubcollection and extractSubcollection from Collection

;;; inherits positionOfItem and stateOfItem from Collection

(define-handler nthItem (Table n)
  (dolist (entry (data me))
    (when (sk8::= n (car entry)) (return (cdr (the cons entry))))))
(define-handler nthState (Table n startState)
  (doheads (state (if startState (cdr startState) (data me)))
    (when (sk8::= n (car (car state))) (return state))))
(define-handler nthPreviousState (Table n fromState)
  (nthPreviousState (data me) n fromState))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The destructive modifiers portion of the Collection Protocol for Table...
;;;

(define-handler (setf nthItem) (newItem Table n &optional dependentStates)
  ;; No states require updating because none of the conses have changed
  (declare (ignore dependentStates))
  ;; Try to find the existing entry for n, and if it's found replace its data with newItem
  (unless (dolist (entry (data me))
            (when (sk8::= n (car entry))
              (setf (cdr (the cons entry)) newItem)
              (return t)))
    ;; If no existing entry, add a new one
    (push (cons n newItem) (data me)))
  ;; Return FALSE to indicate no change of eq'ness
  nil)

(define-handler (setf currentItem) (newItem Table state &optional dependentStates)
  ;; No states require updating because none of the conses have changed
  (declare (ignore dependentStates))
  (setf (cdr (car state)) newItem)
  ;; Return FALSE to indicate no change of eq'ness
  nil)

;;; Insertions make no sense in Tables, so none of the insertion handlers are implemented

(define-handler removeCurrentItem (Table state &optional dependentStates)
  (let ((changeInfo (removeCurrentItem (data me) state dependentStates)))
    (when changeInfo (setf (data me) (cdr changeInfo))))
  ;; Return FALSE to indicate no change of eq'ness
  nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The ObjectTable object
;;;

(new Table :objectname "ObjectTable" :project SK8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The basic portion of the Collection Protocol for ObjectTable...
;;;

(define-handler nthItem (ObjectTable n)
  (dolist (entry (data me))
    (when (eq n (car entry)) (return (cdr (the cons entry))))))
(define-handler nthState (ObjectTable n startState)
  (doheads (state (if startState (cdr startState) (data me)))
    (when (eq n (car (car state))) (return state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The destructive modifiers portion of the Collection Protocol for ObjectTable...
;;;

(define-handler (setf nthItem) (newItem ObjectTable n &optional dependentStates)
  ;; No states require updating because none of the conses have changed
  (declare (ignore dependentStates))
  ;; Try to find the existing entry for n, and if it's found replace its data with newItem
  (unless (dolist (entry (data me))
            (when (eq n (car entry))
              (setf (cdr (the cons entry)) newItem)
              (return t)))
    ;; If no existing entry, add a new one
    (push (cons n newItem) (data me)))
  ;; Return FALSE to indicate no change of eq'ness
  nil)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The TypeTable object
;;;

(new Table :objectname "TypeTable" :project SK8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The basic portion of the Collection Protocol for TypeTable...
;;;

;#####*** THESE SEARCHES CAN BE SHORTENED, SINCE THE LIST IS SORTED BY TYPE-SPECIFICITY!!!
(define-handler nthItem (TypeTable n)
  (dolist (entry (data me))
    (when (is-a n (car entry)) (return (cdr (the cons entry))))))
(define-handler nthState (TypeTable n startState)
  (doheads (state (if startState (cdr startState) (data me)))
    (when (is-a n (car (car state))) (return state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The destructive modifiers portion of the Collection Protocol for TypeTable...
;;;

(define-handler (setf nthItem) (newItem TypeTable n &optional dependentStates)
  ;; No states require updating because none of the conses have changed
  (declare (ignore dependentStates))
  ;; Try to find the existing entry for n, and if it's found replace its data with newItem
  (unless (dolist (entry (data me))
            (when (eq n (car entry))
              (setf (cdr (the cons entry)) newItem)
              (return t)))
    ;##### ACK!  OPTIMIZE THIS!
    ;; If no existing entry, add a new one in the correct place
    (push (cons n newItem) (data me))
    (setf (data me) (sort (data me) #'is-a :key #'car)))
  ;; Return FALSE to indicate no change of eq'ness
  nil)



|#



;-----------------------------------------------------------------------------
; Collection
;-----------------------------------------------------------------------------

#| collect (collectionType, {pair1, pair2 ... pairn}) => [Table]
 ----------------------------------------------------------
   Required Handler

   Creates a SImpleTable, initialized with the
   key/value pairs provided.
|#
(defmethod collect ((me (eql Table))
                           &rest key-value-pairs)
  (let ((tableObj (new Table :project sk8)))
    (dolist (pr key-value-pairs)
      (let ((key (car pr))
            (val (cdr pr)))
        (setElementAtVisitState tableObj (visitStateAtKey tableObj key) val)))
    tableObj))

(define-handler initialVisitState (Table)
  (data me))

(define-handler succeedingVisitState (Table stateObj)
  (cdr stateObj))

(define-handler elementAtVisitState (Table stateObj)
  (cdr (car stateObj)))


;-----------------------------------------------------------------------------
; ContentMutable
;-----------------------------------------------------------------------------

(define-handler setElementAtVisitState (Table
                                                  stateObj
                                                  newVal)
  (setf (cdr (car stateObj)) newVal)
  me)

;-----------------------------------------------------------------------------
; StructureMutable
;-----------------------------------------------------------------------------

(define-handler removeVisitState (Table stateObj)
  (setf (data me) (removeVisitState (data me) stateObj))
  me)

;-----------------------------------------------------------------------------
; Keyed
;-----------------------------------------------------------------------------

(define-handler keyAtVisitState (Table stateObj)
  (declare (ignore me))
  (car (car stateObj)))

;;test me*****
(define-handler visitStateAtKey (Table keyObj)
  (block searching
    (let ((end-st (last (data me)))
          (key nil))
      (do* ((st (initialVisitState me)
                (succeedingVisitState me st)))
           ((equal st end-st) 
            (setq key (car (car st)))
            (when (equal keyObj key)
              (return-from searching st)))
        (setq key (car (car st)))
        (when (equal keyObj key)
          (return-from searching st))))))

(define-handler isFinalVisitState (Table stateObj)
  (equal stateObj (last (data me))))



;-----------------------------------------------------------------------------
; TXStructureMutableKeyedCollection
;-----------------------------------------------------------------------------

(define-handler setElementAtKey (Table
                                          keyObj
                                          newVal)
  (unless (dolist (entry (data me))
            (when (sk8::= keyObj (car entry))
              (setf (cdr (the cons entry)) newVal)
              (return t)))
    ;; If no existing entry, add a new one
    (push (cons keyObj newVal) (data me)))
  me)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The ObjectTable object
;;;

(new Table :objectname "ObjectTable" :project SK8)

#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 5/ 2/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
