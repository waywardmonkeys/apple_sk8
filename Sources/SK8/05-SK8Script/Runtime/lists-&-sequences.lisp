;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :SK8Development)

(define-sk8-function sk8:removeDuplicates nil (coll)
  (unless (is-a coll collection) (error "~A is not a collection" coll))
  (setf coll (sk8:copy coll))
  (let ((currentState (initialVisitState coll))
        (count 1)
        currentItem)
    (when currentState
      (loop
        (if (isFinalVisitState coll currentState) (return))
        (setf currentItem (elementAtVisitState coll currentState))
        (when (positionOfItem coll currentItem :start count)
          (setf coll (removeVisitState coll currentState))
          )
        (incf count)
        (setf currentState (succeedingVisitState coll currentState))))
    coll))

(define-sk8-function sk8:reverse nil (coll)
  (let* ((newColl (sk8:copy coll))
         (currentState1 (initialVisitState coll))
         (currentState2 (initialVisitState newColl))
         its)
    (when currentState1
      (loop
        (push (elementAtVisitState coll currentState1) its)
        (if (isFinalVisitState coll currentState1) (return))
        (setf currentState1 (succeedingVisitState coll currentState1)))
      (loop
        (setf newColl (setElementAtVisitState newColl currentState2 
                                              (pop its)))
        (if (isFinalVisitState newColl currentState2) (return))
        (setf currentState2 (succeedingVisitState newColl currentState2)))
      )
    newColl))

(define-sk8-function sk8:intersection nil (coll1 coll2)
  (unless (is-a coll1 collection) (error "~A is not a collection" coll1))
  (unless (is-a coll2 collection) (error "~A is not a collection" coll2))
  (let ((currentState (initialVisitState coll2))
        (newColl nil)
        currentItem)
    (when currentState
      (loop
        (if (isFinalVisitState coll2 currentState) (return))
        (setf currentItem (elementAtVisitState coll2 currentState))
        (when (positionOfItem coll1 currentItem)
          (setf newColl (insertAtEnd currentItem newColl)))
        (setf currentState (succeedingVisitState coll2 currentState))))
    newColl))

;;; Will later specialize to work on Sets and Bags
;;;
(define-sk8-function sk8:union nil (coll1 coll2)
  (unless (is-a coll1 collection) (error "~A is not a collection" coll1))
  (unless (is-a coll2 collection) (error "~A is not a collection" coll2))
  (let (currentState
        (newColl nil)
        currentItem)
    (setf currentState (initialVisitState coll1))
    (when currentState
      (loop
        (setf newColl (insertAtEnd (elementAtVisitState coll1 currentState) newColl))
        (if (isFinalVisitState coll1 currentState) (return))
        (setf currentState (succeedingVisitState coll1 currentState))))
    (setf currentState (initialVisitState coll2))
    (when currentState
      (loop
        (setf currentItem (elementAtVisitState coll2 currentState))
        (unless (positionOfItem coll1 currentItem)
          (setf newColl (insertAtEnd currentItem newColl)))
        (if (isFinalVisitState coll2 currentState) (return))
        (setf currentState (succeedingVisitState coll2 currentState))))
    newColl))

(define-sk8-function sk8:difference nil (coll1 coll2)
  (unless (is-a coll1 collection) (error "~A is not a collection" coll1))
  (unless (is-a coll2 collection) (error "~A is not a collection" coll2))
  (let (currentState
        (newColl nil)
        currentItem)
    (setf currentState (initialVisitState coll1))
    (when currentState
      (loop
        (setf currentItem (elementAtVisitState coll1 currentState))
        (unless (positionOfItem coll2 currentItem)
          (setf newColl (insertAtEnd currentItem newColl)))
        (if (isFinalVisitState coll1 currentState) (return))
        (setf currentState (succeedingVisitState coll1 currentState))))
    (setf currentState (initialVisitState coll2))
    (when currentState
      (loop
        (setf currentItem (elementAtVisitState coll2 currentState))
        (unless (positionOfItem coll1 currentItem)
          (setf newColl (insertAtEnd currentItem newColl)))
        (if (isFinalVisitState coll2 currentState) (return))
        (setf currentState (succeedingVisitState coll2 currentState))))
    newColl))

;;; Refine with further arguments later as we augment the overall Find system
;;; with a database
;;;
(define-sk8-function searchThing nil (thingToSearchFor coll)
  (let ((currentState (initialVisitState coll)))
    (when currentState
      (loop
        (when (containsSubcollection coll thingToSearchFor currentState nil)
          (return-from searchthing t))
        (if (isFinalVisitState coll currentState) (return))
        (setf currentState (succeedingVisitState coll currentState))))
    nil))

;;______________________________________________________________________________________

(define-sk8-function sk8:minimum nil (coll)
  (let* ((currentState (initialVisitState coll))
         currentItem
         currentBest)
    (when currentState
      (setf currentBest (elementAtVisitState coll currentState))
      (loop
        (setf currentItem (elementAtVisitState coll currentState))
        (if (sk8:< currentItem currentBest) (setf currentBest currentItem))
        (if (isFinalVisitState coll currentState) (return))
        (setf currentState (succeedingVisitState coll currentState))))
    currentBest))

(define-sk8-function sk8:maximum nil (coll)
  (let* ((currentState (initialVisitState coll))
         currentItem
         currentBest)
    (when currentState
      (setf currentBest (elementAtVisitState coll currentState))
      (loop
        (setf currentItem (elementAtVisitState coll currentState))
        (if (sk8:> currentItem currentBest) (setf currentBest currentItem))
        (if (isFinalVisitState coll currentState) (return))
        (setf currentState (succeedingVisitState coll currentState))))
    currentBest))


#|
	Change History (most recent last):
	2		 6/25/93	chip	published the names of the handlers made by defmethod
	8  	 9/ 1/94	chip    	publish-project-symbol --> publish-global/function-symbol where necessary (radar #1183935)
	2  	 4/10/96	Hernan  	define-sk8-function->defun
	3  	 4/16/96	Brian   	
	4  	 4/22/96	Brian   	
	5  	 5/ 2/96	Brian   	
	6  	 5/ 7/96	sidney  	update for some package changes
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
