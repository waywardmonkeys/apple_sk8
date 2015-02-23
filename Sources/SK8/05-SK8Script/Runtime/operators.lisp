;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  A function used only during the build:
;;;
(defun define-operator-synonym (synonymName realFunction)
  ;; Not published as function so the real function only shows up once (in the functions of SK8)
  (mf::publish-project-symbol synonymName mf::*SK8-package*)
  (setf (symbol-function synonymName) realFunction))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SK8::= (object1 object2 &key exactMatch)
  (cond
   ((eq object1 object2) t)
   ((null object1) (null object2))
   ((null object2) (null object1))
   (t (equalTo object1 object2 :exactMatch exactMatch))))

(define-operator-synonym 'equals #'SK8::=)


(defun <> (object1 object2 &key exactMatch)
  (not (SK8::= object1 object2 :exactMatch exactMatch)))

(define-operator-synonym '­ #'SK8::<>)



(defun SK8::> (object1 object2)
  (cond
   ((numberp object1)
    (> object1 object2))
   ((and (stringp object1) (stringp object2))
    (when (string-greaterp object1 object2) t))
   (t
    (greaterThan object1 object2))))


(defun SK8::< (object1 object2)
  (cond
   ((numberp object1)
    (< object1 object2))
   ((and (stringp object1) (stringp object2))
    (when (string-lessp object1 object2) t))
   (t
    (greaterThan object2 object1))))


(defun SK8::>= (object1 object2)
  (not (SK8::< object1 object2)))

;;(define-operator-synonym '³ #'SK8::>=)


(defun SK8::<= (object1 object2)
  (not (SK8::> object1 object2)))

;;(define-operator-synonym '² #'SK8::<=)



(defun as (thing type &key ((:project proj)))
  (asType type thing :project proj))

;;;______________________________________________________________________________________________________

;;;  From the item at fromState (or the 1st item, if fromState is false), to the item BEFORE toState (or the last item, if toState is false)
;;;
(define-handler containsSubcollection (Collection subCollection fromState toState &key exactMatch)
  (let (test)
    (if exactMatch
      (setf test #'eq)
      (setf test #'sk8::=))
    (let ((subState (initialVisitState subcollection)))
      (loop
        (when (null subState) (return-from containsSubcollection t))
        (when (eq fromState toState)
          (return-from containsSubcollection nil))
        (unless (funcall test (elementAtVisitState me fromState) (elementAtVisitState subCollection subState)) 
          (return-from containsSubcollection nil))
        (setq subState (succeedingVisitState subCollection subState))
        (setf fromState (succeedingVisitState me fromState)))
      (null subState))))

(define-handler positionOfItem (Collection item &key start end exactMatch)
  (let ((test (if exactMatch
                #'(lambda (a b) (sk8::= a b :exactMatch t))
                #'sk8::=))
        (index 1)
        (startState (if start (Collectionnthstate me start) (initialVisitState me)))
        (endState (when end (Collectionnthstate me (1+ end))))
        ourtest
        )
    (if (or exactMatch (not (is-a item text)) (eql 0 (sk8::length item)))
      ;; Don't do sub-collection matching -- just do item matching
      (setf ourTest #'(lambda (x) (funcall test (elementAtVisitState me x) item)))
      ;; Try both item matching and sub-collection matching
      (setf ourTest #'(lambda (x) (or (funcall test (elementAtVisitState me x) item)
                                      (containsSubcollection me item x endState)))))
    (loop
      (when (eq startState endState)
        (return-from positionOfItem nil))
      (when (funcall ourTest startState)
        (return-from positionOfItem index))
      (incf index)
      (setf startState (succeedingVisitState me startState)))
    nil))


(define-handler containsSubcollection (String subString fromState toState &key exactMatch)
  (when subString
    (if fromState
      (when (listp fromState) (setq fromState (car (the list fromState))))
      (setq fromState 0))
    (setq toState (if toState
                    (min (sk8::length me)
                         (+ fromState (sk8::length subString))
                         (if (listp toState) (car (the list toState)) toState))
                    (min (sk8::length me)
                         (+ fromState (sk8::length subString)))))
    (if exactMatch
      (string= me subString :start1 fromState :end1 toState)
      (string-equal me subString :start1 fromState :end1 toState))))

(define-handler positionOfItem (String item &key start end  exactMatch)
  (declare (ignore exactMatch))
  (setq start (if start (1- start) 0))
  (if (characterp item)
    (let ((pos (position item me :test #'CHAR-EQUAL :start start :end end)))
      (when pos (1+ (- pos start))))
    (unless (eql 0 (sk8::length item))
      (let ((pos (search item me :start2 start :end2 end :test #'CHAR-EQUAL)))
        (when pos (1+ (- pos start)))))))



(defun contains (coll item &key exactMatch)
  (when (positionOfItem coll item :exactMatch exactMatch)
    t))


(defun startsWith (coll item &key test exactMatch)
  (let ((collStartState (initialVisitState coll)))
    (when collStartState
      (unless test (setq test #'SK8::=))
      (or
       ;; Either the given item matches the 1st item of coll...
       (funcall test item (elementAtVisitState coll collStartState))
       ;; ...or the given item is a non-empty coll nested at the start of coll
       (and (sk8::is-a item collection)
            (containsSubcollection coll item nil nil
                                   :exactMatch exactMatch))))))

(defun endsWith (coll item &key test exactMatch)
  (let* ((collLength (sk8::length coll))
         (collEndState (collectionnthstate coll collLength)))
    (when collEndState
      (unless test (setq test #'SK8::=))
      (or
       ;; Either the given item matches the last item of coll...
       (funcall test item (elementAtVisitState coll collEndState))
       ;; ...or the given item is a non-empty coll nested at the end of coll
       (when (is-a item collection)
         (let ((itemLength (SK8::length item)))
           (unless (eql itemLength 0)
             (let* ((loc (1+ (- collLength itemLength)))
                    (collStartState (and (> loc 0) (collectionnthstate coll loc))))
               (when collStartState
                 (containsSubcollection coll item collStartState nil
                                        :exactMatch exactMatch)))))))))
  )



;;___________________________________________________________________________________________________

(defun ^ (baseNumber powerNumber)
  (expt baseNumber powerNumber))


(declare-not-global (number)
  (defun SK8::/ (number divisor)
    (// number divisor)))

(define-operator-synonym 'Ö #'SK8::/)


(declare-not-global (number)
  (defun div (number divisor)
    (truncate number divisor)))


(declare-not-global (number)
  (defun SK8::mod (number divisor)
    (rem number divisor)))


(mf::publish-function-symbol '* mf::*SK8-package*) ; simply shared as is
(mf::publish-function-symbol '+ mf::*SK8-package*) ; simply shared as is
(mf::publish-function-symbol '- mf::*SK8-package*) ; simply shared as is


(CCL::can-constant-fold '(&))
(defun & (&rest things)
  (declare (list things) (dynamic-extent things))
  (unless things (return-from & nil))
  (let ((anything? nil)
        (anyText? nil)
        (allVectors? t)
        (proj nil)
        (previous nil)
        (remainingThings things)
        thing)
    (loop
      (when (setq thing (car remainingThings))
        (setq anything? t)
        (cond
         ((eq thing :PROJECT)
          (unless previous (return-from & nil))
          (setq proj (second remainingThings))
          (setf (cdr (the list previous)) nil)
          (return))
         ((vectorp thing)
          (when (stringp thing)
            (setq anyText? t)
            (setq allVectors? nil)))
         ((mf::!textp thing)
          (setq anyText? t
                allVectors? nil))
         (t
          (setq allVectors? nil))))
      (setq previous remainingThings)
      (unless (setq remainingThings (cdr remainingThings)) (return)))
    (when anything?
      (prog1 ; to tell MCL this doesn't return multiple-values (makes it a bit faster)
        (cond
         (allVectors?
          (doheads (thingHead things)
            (declare (list thingHead))
            (when (car thingHead)
              (unless (CCL::sequencep (car thingHead))
                (setf (car thingHead) (asType (car thingHead) Vector)))))
          (apply #'concatenate 'vector things))
         (anyText?
          (doheads (thingHead things)
            (declare (list thingHead))
            (when (car thingHead)
              (unless (stringp (car thingHead))
                (setf (car thingHead) (objectAsString (car thingHead) :project proj)))))
          (apply #'concatenate 'string things))
         (t
          (doheads (thingHead things)
            (declare (list thingHead))
            (unless (CCL::sequencep (car thingHead))
              (setf (car thingHead) (objectAsList (car thingHead)))))
          (apply #'concatenate 'list things)))))))



(mf::publish-function-symbol 'AND mf::*SK8-package*) ; simply shared as is
(mf::publish-function-symbol 'OR mf::*SK8-package*) ; simply shared as is


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun containsq (coll item)
  (contains coll item :test #'eq :exactMatch t))

(defun startsWithq (coll item)
  (startsWith coll item :test #'eq :exactMatch t))

(defun endsWithq (coll item)
  (endsWith coll item :test #'eq :exactMatch t))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-handler name (Object)
  (objectName me))




#|
	Change History (most recent last):
	1  	10/20/94	chip    	new file (pulled operators out of "runtime" file)
	2  	10/24/94	chip    	moved remaining operators from runtime file
	3  	10/31/94	chip    	new definition of MOD (for radar #1196467)
	4  	10/31/94	chip    	oops; wrong definition of mod
	5  	11/ 7/94	chip    	oops; the actions in the "(if (MF::!textp coll) ..." of startsWith & endsWith were reversed -- fixed now.
	6  	11/11/94	chip    	fixed bogus reference to SK8::containedBy
	7  	11/11/94	chip    	removed extraneous let-binding from SS::CONTAINEDBY; cleaned up equals, < and >
	8  	11/16/94	chip    	major cleanup + added all the official functions corresponding to the operators
	9  	11/23/94	chip    	fixed startsWith and endsWith's calls to containsSubcollection (radar #1201459)
	2  	 1/17/96	sidney  	ss::!coerce deleted
	3  	 4/19/96	Brian   	Cleaning out old stuff.
	4  	 4/22/96	Brian   	
	5  	 4/22/96	Brian   	
	6  	 5/ 2/96	Brian   	
	7  	 7/18/96	Brian   	removing print statement
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
