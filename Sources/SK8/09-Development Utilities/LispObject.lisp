;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :special
                  SK8::LispRef
                  SK8::WeakReference
                  )

(SK8-declare-syms :SK8 :public
                  SK8::data
                  SK8::referencesFrom
                  SK8::referencesTo
                  SK8::getLispRef
                  )

(new Object
     :objectName "WeakReference"
     :project SK8)

(addProperty WeakReference 'data)

(define-handler initialize (WeakReference original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (setValue 'data me (ccl::%cons-population nil))
  (sk8-return-nothing))

(define-handler (setf data) (newValue WeakReference)
  (setf (ccl::population-data (getValue 'data me)) (list newValue))
  newValue)

(define-handler data (WeakReference)
  (let ((data (getValue 'data me)))
    (and data (first (ccl::population-data data)))))

#| TESTS

(data (new WeakReference :project SK8 :data '(a 2 3)))

|#

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(defvar *LispRefs* (make-hash-table :weak :key :test #'eq))

(defun getLispRef (data)
  (let ((existing-LispRef (gethash data *lispRefs*)))
    (or existing-LispRef
        (setf (gethash data *lispRefs*)
              (new LispRef :project SK8 :data data)))))

(new WeakReference
     :objectName "LispRef"
     :project SK8)

(define-handler print-object (LispRef theStream)
  (if (getValue 'data me)
    (let ((*print-length* 3)
          (*print-level* 2))
      (format theStream "the LispRef ~s" (data me)))
    (call-next-method)))

(define-handler writeObject (LispRef theStream rereadably)
  (if (getValue 'data me)
    (let ((*print-length* 3)
          (*print-level* 2))
      (format theStream "~A~s" (if rereadably
                                 "the LispRef "
                                 "")
              (data me)))
    (call-next-method)))

(define-handler dontSave (LispRef)
  nil)


(defun ensure-is-sk8object (obj)
  (if (typep obj 'sk8::object)
    obj
    (getLispRef obj)))

#|
(ensure-is-sk8object QuickTimeMovie)
(ensure-is-sk8object (make-hash-table))
|#

(defun doReferencesFrom (me)
  (let ((theRefs '()))
    (ccl::map-references-from #'(lambda (obj)
                                  (pushnew obj theRefs))
                              me)
    (loop for eachRefCons on theRefs
          doing (setf (car eachRefCons) (ensure-is-sk8object (car eachRefCons))))
    theRefs))

(defmethod referencesFrom (me)
  (referencesFrom (getLispRef me)))

(define-handler referencesFrom (Object)
  (doReferencesFrom me))

(define-handler referencesFrom (LispRef)
  (doReferencesFrom (data me)))


(define-handler referencesTo (Object)
  (let ((theRefs '()))
    (ccl::map-references-to #'(lambda (obj)
                                (pushnew (ensure-is-sk8object obj) theRefs))
                            me)
    theRefs))

(define-handler referencesTo (LispRef)
  (let ((theRefs '()))
    (ccl::map-references-to #'(lambda (obj)
                                (pushnew (ensure-is-sk8object obj) theRefs))
                            (data me))
    theRefs))

#| TESTS

(print-object LispRef *standard-output*)
(writeObject LispRef *standard-output* nil)
(writeObject (getLispRef '(a 2 3)) *standard-output* nil)
(writeObject (getLispRef '(a 2 3)) *standard-output* t)

(new Object :project SK8 :objectName "junk")
(new WeakReference :project SK8 :objectName "junkRef" :data sk8::junk)
(ccl::map-references-to #'print sk8::junk)
(referencesTo SK8::junk)

|#


#|
	Change History (most recent last):
	1  	 7/ 5/95	dy      	
	2  	 7/ 5/95	dy      	added writeObject
	3  	 7/ 6/95	Till    	referencesFrom and referenceTo of LispRef were wrong - call-next-method was not the way to do it
	4  	 7/ 6/95	Till    	
	5  	 7/ 7/95	dy      	change name of LispObject to LispRef; new WeakReference object
	6  	 7/17/95	dy      	New getLispRef function, used everywhere.
						WeakReference allocates new population in initialize; population-data always a 1-element list.
						Reuse LispRef objects with *LispRefs* global
						New dontSave of LispRef handler returning True.
						More tests.
	7  	 7/17/95	till    	Part of the code was mangled.
	3  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
