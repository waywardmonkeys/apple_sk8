(in-package :SK8Script)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

;; new, slimmed down version of the sk8-multivalue stuff

(SK8-declare-syms :SK8 :private
                  SK8::ignore-SK8-multivals SK8::SK8-multivals SK8::SK8-multivals-from-list
                  SK8::SK8-multival-setf SK8::SK8-multival-bind SK8::with-recoverable-list
                  SK8::maybe-recover-list SK8::SK8-destructuring-setf
                  SK8::SK8-destructuring-bind SK8::definitely-recover-list)

(eval-when (compile eval load)
  (import '(SK8::SK8-return-nothing 
            SK8::ignore-SK8-multivals SK8::SK8-multivals SK8::SK8-multivals-from-list
            SK8::sk8-multivals-sequence
            SK8::SK8-multival-setf SK8::SK8-multival-bind SK8::with-recoverable-list
            SK8::maybe-recover-list SK8::SK8-destructuring-setf
            SK8::SK8-destructuring-bind SK8::definitely-recover-list)
          :SK8Script)
  (import '(SK8::SK8-return-nothing 
            SK8::ignore-SK8-multivals SK8::SK8-multivals SK8::SK8-multivals-from-list
            SK8::sk8-multivals-sequence
            SK8::SK8-multival-setf SK8::SK8-multival-bind SK8::with-recoverable-list
            SK8::maybe-recover-list SK8::SK8-destructuring-setf
            SK8::SK8-destructuring-bind SK8::definitely-recover-list)
          :MF)
  (import '(SK8::SK8-return-nothing 
            SK8::ignore-SK8-multivals SK8::SK8-multivals SK8::SK8-multivals-from-list
            SK8::sk8-multivals-sequence
            SK8::SK8-multival-setf SK8::SK8-multival-bind SK8::with-recoverable-list
            SK8::maybe-recover-list SK8::SK8-destructuring-setf
            SK8::SK8-destructuring-bind SK8::definitely-recover-list)
          :UIDev)
  )

(defmacro SK8-return-nothing () '*undefined*)

(defmacro ignore-SK8-multivals (form)
  (unless (and (listp form)
               (symbolp (first form)))
    (error "given form must be a function-call form"))
  form)

(defmacro SK8-multivals (&rest arguments)
  `(list ,@arguments))

(defmacro SK8-multivals-from-list (theList)
  theList)

(defmacro sk8-multivals-sequence (num sequence)
  "All of the elements of sequence are returned as multiple values.
If you know it's a list, use SK8-multivals-from-list instead."
  (declare (ignore num))
  `(coerce ,sequence 'list))

(defmacro SK8-multival-setf ((&rest places) form)
  (cond
   
   ((and (listp form) (eq (first (the list form)) 'LIST))
    (let* ((temp-vars nil)
           (place-sets nil)
           v err)
      (pop form)
      (unless (eql (list-length form) (list-length places))
        (setq err `((error "Can't destructure ~a values into ~a places"
                           ,(list-length form) ,(list-length places)))))
      (dolist (p places)
        (setq v (gensym))
        (push (list v (pop form)) temp-vars)
        (push p place-sets)
        (push v place-sets))
      `(let* ,(nreverse temp-vars)
         ,@err
         (setf ,@(nreverse place-sets)))))
   
   ((null (cdr places))
    `(setf ,(first places) (first ,form)))
   
   (t
    `(multiple-value-setq ,places (values-list ,form)))))

(defmacro SK8-multival-bind ((&rest vars) form &body forms)
  (dolist (v vars) (unless (symbolp v) (error "~A is not a symbol" v)))
  `(destructuring-bind ,vars ,form ,@forms))

(defun !bad_number_of_values_err (expectedn gotn)
  (error "can't destructure a collection of ~a values into ~a places" gotn expectedn))

(defun !collection_destructure_num_err (theList currentCons moreValsExpected)
  (when (eq theList nil) (incf moreValsExpected))
  (error "cannot destructure the ~a items of ~a into ~a places"
         (length theList) (SK8::objectString theList) (+ (- (length theList) (length currentCons)) moreValsExpected)))

(defun !collection_destructure_badlist (theList)
  (error "cannot destructure the improper list ~a" (SK8::objectString theList)))

(defun !collection_destructure_notcollection (obj)
  (error "~a cannot be destructured" (SK8::objectString obj)))

(defmacro SK8-destructuring-setf ((&rest places) form)
  `(SK8-multival-setf ,places ,form))

(defmacro SK8-destructuring-bind ((&rest vars) form &body forms)
  (dolist (v vars) (unless (symbolp v) (error "~A is not a symbol" v)))
  `(destructuring-bind ,vars ,form ,@forms))

(defmacro with-recoverable-list (form) form)

(defmacro maybe-recover-list (list-argument) list-argument)

(defmacro definitely-recover-list (list-argument) list-argument)




#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
