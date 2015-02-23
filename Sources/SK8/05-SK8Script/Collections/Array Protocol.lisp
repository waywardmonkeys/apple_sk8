;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

#|
Array = {ContentMutableOrderedCollection + Discrete}

|#

;-----------------------------------------------------------------------------
; PRIVATE FUNCTIONS
;-----------------------------------------------------------------------------
(defun place-value (dimensions index)
  (let ((limit (1- (length dimensions))))
    (cond 
     ((< -1 index limit)
      (* (elt dimensions index) (place-value dimensions (1+ index))))
     ((= index limit)
      (elt dimensions index))
     (t (error "PLACE-VALUE: marker out of bounds")))))

;;; WARNING: indexes are assumed to be SK8-style
;;; 1-based indexes!
(defun row-major-index (dimensions indexes marker)
  (let ((limit (1- (length indexes))))
    (cond 
     ((< -1 marker limit)
      (+ (* (1- (elt indexes marker))
            (place-value dimensions (1+ marker)))
         (row-major-index dimensions indexes (1+ marker))))
     ((= marker limit)
      (1- (elt indexes marker)))
     (t (error "ROW-MAJOR-INDEX: marker out of bounds")))))

(defun row-major-to-index (dimensions scalar)
  (let* ((len (length dimensions))
         (indexData (make-array len :initial-element nil))
         (remainder scalar))
    (dotimes (i (1- len))
      (multiple-value-bind (fac rem)
                           (truncate remainder
                                     (place-value dimensions (1+ i)))
        (setq remainder rem)
        (setf (elt indexData i) (1+ fac))))
    (setf (elt indexData (1- len)) (1+ remainder))
    indexData))

;-----------------------------------------------------------------------------
; Collection
;-----------------------------------------------------------------------------

(defmethod collect ((me (eql Array)) &rest objects)
  (make-array (car objects) 
              :initial-element (cadr (memq :initial-element objects))
              :adjustable t))

(define-handler initialVisitState (Array)
  (unless (eq 0 (array-total-size me)) 0))

(define-handler succeedingVisitState (Array stateObj)
  (and stateObj
       (unless (eql (incf stateObj) (array-total-size me))
         stateObj)))

(define-handler elementAtVisitState (Array stateObj)
  (row-major-aref (the array me) stateObj))

(define-handler setElementAtVisitState (Array stateObj newVal)
  (setf (row-major-aref me stateObj) newVal)
  me)

(define-handler indexAtVisitState (Array stateObj)
  (row-major-to-index (array-dimensions me)
                      stateObj))

(define-handler visitStateAtIndex (Array indexObj)
  (let* ((dimensions (array-dimensions me)))
    (row-major-index dimensions indexObj 0)))

(define-handler isFinalVisitState (Array stateObj)
  (let ((state (1- (array-total-size me))))
    (eql stateObj (unless (eql -1 state) state))))

#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 5/ 2/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
