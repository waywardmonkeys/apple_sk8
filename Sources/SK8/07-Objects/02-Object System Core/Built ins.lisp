;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

(defmethod ps::owning-project ((me t)) nil)

(defmethod ps::owning-project ((me Object)) (project me))

(defmethod ps::owning-project ((me Project)) me)

(defun // (n1 n2)
  (let ((q (/ n1 n2)))
    (if (ratiop q)
      (float q)
      q)))


;; from built-in-types

(defmethod SK8:project ((me t) &key) nil)

(defmethod SK8:knownChildren ((me t) &key) nil)

(ccl::make-built-in-class 'SK8-function)
(sk8dev::make-clos-class-shadow 'SK8-function "Function" object)
(setf (ccl::type-predicate 'SK8-function) 'functionp)

(defun handlerp (thing)
  (typep thing 'standard-method))

(ccl::make-built-in-class 'SK8-handler)
(sk8dev::make-clos-class-shadow 'SK8-handler "Handler" object)
(setf (ccl::type-predicate 'SK8-handler) 'handlerp)

;;; get-handler-object-from-lfun used to be defined in mf-kernel;macros.lisp (did it used to be a macro?) and it used to look up the object in
;;;  the lfun-info of the lfun. That caused problems with storing, because the class referred to the handler which referred to the lfun which referred to
;;;  the object. It's a lot simpler to be able to store the class before dealing with any reference to the object.
;;; Since the object was recorded inthe lfun-info only for handlers, not accessor methods, this function could be used to distinguish between handler and
;;;  accessor. It would be nice not to have to carry this historical baggage, but for now, this function is being written to check for accessors. The function
;;;  following this, more-sure-get-handler-from-lfun, works for accessors too. This is about three times slower than getting the object straight from the
;;;  lfun, but it is simpler and maybe we can get rid of everything that we store in the lfun eventually, which would be nice.

(defun get-handler-object (handler)
  (let* ((specializers (method-specializers handler))
         (arg1Class (pop specializers))
         (dispatchClass (cond
                         ((listp arg1Class) (return-from get-handler-object (second arg1Class))) ; eql method
                         ((eq arg1Class CCL::*t-class*) (if specializers (first specializers) arg1Class))
                         (t arg1Class))))
    (declare (list specializers))
    (cond ((listp dispatchClass) (second dispatchClass)) ; eql method
          ((eq dispatchClass CCL::*t-class*) SK8:Object)
          ((clos-class-shadow-p dispatchClass) dispatchClass)
          ((eq dispatchClass CCL::*standard-method-class*) SK8:Handler)
          ((eq dispatchClass CCL::*function-class*) SK8:Function)
          (t (safe-class-owner dispatchClass)))))

(defun get-handler-object-from-lfun (lfun)
  (let ((name (function-name lfun)))
    (when (typep name 'standard-method)
      (get-handler-object name))))

;; was (define-handler SK8:private (SK8:Handler)
(defmethod SK8:private ((me standard-method) &key)
  (let ((obj (SK8:object me)))
    (unless obj (error "~a isn't a valid handler" me))
    ;; since sk8script no longer allows :before and :after methods, any such were written in Lisp and are private
    (or (not (null (method-qualifiers me)))
        (private-property-p obj (SK8:name me)))))

;; was (define-handler (setf SK8:private) (boolean SK8:Handler)
(defmethod (setf SK8:private) (boolean (me standard-method) &key)
  (let ((obj (SK8:object me)))
    (unless obj (error "~a isn't a valid handler" me))
    (if boolean
      (make-private-property obj (SK8:name me))
      (make-public-property obj (SK8:name me)))))


(new object :objectname "Collection" :project sk8:sk8)
(new object :objectname "Text" :project sk8:sk8)

(defparameter *collection-object* SK8:collection)
(defparameter *collection-class* (class-of SK8:collection))
(defparameter *text-object* sk8:text)
(defparameter *text-class* (class-of sk8:text))


;;; For use by MAYBE-MIRROR-METHOD-FOR-PSEUDO-CHILDREN
;;;
;;; this variable is used as a lookup table by define-handler, but since none of the types being referenced here existed earlier,
;;;  there should be no problem with it having been nil up until now.
;;;
;;; Each sublist is of the form  (<object>  <objectPseudoChildrenList>  <objectPrecedenceList>)
;;; Note that the <objectPrecedenceList> must only go up to the least specific object that has any pseudo-children
;;;
(setf *object-pseudo-children-and-precedence-lists*
      (list (list *Collection-object*
                  (list (find-class 'list) (find-class 'array))
                  nil)
            
            (list *Text-object*
                  (list (find-class 'string))
                  (list *Collection-object*))
            
            (list (find-class 'array)
                  (list (find-class 'vector))
                  (list *Collection-object*))
            
            (list (find-class 'vector)
                  (list (find-class 'string))
                  (list (find-class 'array) *Collection-object*))
            
            (list (find-class 'string)
                  nil
                  (list *Text-object* (find-class 'vector) (find-class 'array) *Collection-object*))
            
            (list (find-class 'list)
                  nil
                  (list *Collection-object*))))

(defmethod !collectionp ((me t)) (declare (ignore me)) nil)
(defmethod !collectionp ((me SK8:List)) (declare (ignore me)) t)
(defmethod !collectionp ((me SK8:Array)) (declare (ignore me)) t)
(defmethod !collectionp ((me hash-table)) (declare (ignore me)) t)

(define-handler !collectionp :hidden (SK8:Collection) (declare (ignore me)) t)


(define-handler sk8:typeSatisfied (SK8:Collection specific)
  (if (eq me SK8:Collection)
    (if (or (!collectionp specific)
                  ;;************This second half of the or statement is a kludge, because handlers aren't being defined on their "parent" sealed object
            (memq specific (list sk8::string sk8::list sk8::array)) )
      t nil)
    (call-next-method)))

(defmethod !textp ((me t)) (declare (ignore me)) nil)
(defmethod !textp ((me SK8:String)) (declare (ignore me)) t)
(define-handler !textp :hidden (SK8:Text) (declare (ignore me)) t)

(define-handler sk8:typeSatisfied (SK8:Text specific)
  (if (eq me sk8:text)
    (!Textp specific)
    (call-next-method)))

(eval `(progn
(sk8dev::make-clos-class-shadow  'symbol "Symbol" SK8:Object)
(sk8dev::make-clos-class-shadow  'character "Character" SK8:Object)
(sk8dev::make-clos-class-shadow  'number "Number" SK8:Object)
(sk8dev::make-clos-class-shadow  'complex "Complex" SK8:number)
(sk8dev::make-clos-class-shadow  'real "Real" SK8:number)
(sk8dev::make-clos-class-shadow  'float "Float" SK8:real)
(sk8dev::make-clos-class-shadow  'short-float "SmallFloat" SK8:real)
(sk8dev::make-clos-class-shadow  'double-float "BigFloat" sk8:float)
(sk8dev::make-clos-class-shadow  'integer "Integer" SK8:real)
(sk8dev::make-clos-class-shadow  'fixnum "SmallInteger" SK8:integer)
(sk8dev::make-clos-class-shadow  'bignum "BigInteger" SK8:integer)
(sk8dev::make-clos-class-shadow 'array "Array" SK8:Collection)
(sk8dev::make-clos-class-shadow 'vector "Vector" SK8:Array)
(sk8dev::make-clos-class-shadow 'string "String" SK8:Text SK8:Vector)
(sk8dev::make-clos-class-shadow 'list "List" SK8:Collection)
))

#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/10/96	sidney  	changed build order of some things
	4  	 9/23/96	Brian   	Adding kludge in typesatisfied of collection
						see note there.
	5  	 9/27/96	Brian   	
	6  	11/22/96	Brian   	Making shadow classes be evaluated in order, 
						because the code relies on successively building
						up a known children list from state.
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
