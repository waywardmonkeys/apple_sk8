;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

;;;========================================================
(export 'when-unwind)

(defmacro when-unwind (protected-form &body cleanup-forms) 
  "evaluates protected-form and returns its result if exits normally.
If there is a non-local exit during the execution of protected-form,
when-unwind executes the cleanup-forms."
  (let ((unwinding (gensym)))
    `(let ((,unwinding t))
       (unwind-protect (multiple-value-prog1 ,protected-form (setf ,unwinding nil))
         (when ,unwinding (progn ,@cleanup-forms))))))

#| TESTS
(when-unwind (error "hah")
           (princ "clean ")
           (princ "up") 
           (terpri)
           "bombed")

(when-unwind t
           (princ "clean ")
           (princ "up") 
           (terpri))

(when-unwind nil
           (princ "clean ")
           (princ "up") 
           (terpri))
|#

;;;========================================================

(export 'mklist)

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;;========================================================

(export 'xor)

(defun xor (&rest args)
  "The exclusive 'or' of all the arguments.
Yields nil for no arguments, t for one argument."
  (= 1 (mod (count nil args :test-not #'eq) 2)))

#| All should yield t:
(not (xorf))
(xorf 'a)
(xorf nil 'b)
(xorf 'b  nil)
(not (xorf 'a  'b))
|#

;;;========================================================

(export 'with-preserved-values)

(defmacro with-preserved-values (thing-spec-list &body body)
  "Each thing-spec is a list consisting of a thing
and an optional temporary value to be stored there.
Executes body inside an unwind-protect context which preserves
the original values of the things listed in thing-spec-list.
Example:
  (setf height 1)
  (setf (width foo) 2)
  (with-preserved-values ((height)
                          ((width foo) 9))
    (print height) ; -> 1
    (print (width foo)) ; -> 9
    (setf height 22))
  (print height) ; -> 1
  (print (width foo)) ; -> 2"

  (if (= (length thing-spec-list) 1)
    (let ((oldValue (gensym)))
      (destructuring-bind ((thing &optional (value nil valueSet)))
                          thing-spec-list
        (if valueSet
          `(let ((,oldValue ,thing))
             (setf ,thing ,value)
             (unwind-protect
               (progn ,@body)
               (setf ,thing ,oldvalue)))
          `(let ((,oldValue ,thing))
             (unwind-protect
               (progn ,@body)
               (setf ,thing ,oldvalue))))))
    
    (let ((numOldValues (length thing-spec-list))
          (oldValues (gensym)))
      `(let ((,oldValues (make-array ,numOldValues)))
         (unwind-protect
           ,(append '(progn)
                    (loop for thing-spec in thing-spec-list
                          for ind from 0
                          collect (destructuring-bind (thing &optional (value nil valueSet))
                                                      thing-spec
                                    (if valueSet
                                      `(progn
                                         (setf (aref ,oldValues ,ind) ,thing)
                                         (setf ,thing ,value))
                                      `(setf (aref ,oldValues ,ind) ,thing))))
                    body)
           ,(append '(progn)
                    (loop for thing-spec in thing-spec-list
                          for ind from 0
                          collect (destructuring-bind (thing &optional value)
                                                      thing-spec
                                    (declare (ignore value))
                                    `(setf ,thing (aref ,oldValues ,ind))))))))))

#| Testing
(let ((pv1 3)
      (pv2 4))
  (progn
    (print "Should print 1 2 3 4 5 6 7")
    (ignore-errors
     (with-preserved-values ((pv1) (pv2 2))
       (setf pv1 1)
       (print pv1)
       (print pv2)
       (setf pv2 22)
       (error "hah!")))
    (print pv1)
    (print pv2)
    (setf pv1 7)
    (print (with-preserved-values ((pv1 5))
             (print pv1)
             (setf pv1 6)))
    (print pv1)
    (values)))

|#

;;;========================================================

(export 'with-debugging-features)

(defmacro with-debugging-features ((features) &body body)
  "Set features to :best or leave it nil."
  ;; We can extend this later if we want.
  (if (eq (eval features) :best)
    `(locally
       (declare '(optimize (debug 3))); Enable useful backtraces; turn off tail recursion elimination.
       (with-preserved-values (((getf cl-user::*sk8-features* :save-definitions) t)
                                       (ccl::*save-definitions* t)) ; Enable stepping
         ,@body))
    `(progn ,@body)))
#| TESTS
(with-debugging-features ((if t   :best nil)) (oh))
(with-debugging-features ((if nil :best nil)) (oh))
|#

#|
	Change History (most recent last):
	1  	 2/13/95	dy      	Replace when-unwind.lisp, xor.lisp, add mklist function
	2  	 2/13/95	dy      	move when-unwind to utility-macros.lisp
	3  	 3/ 2/95	dy      	with-preserved-values and with-debuggin-features macros moved here
	4  	 3/ 2/95	dy      	put cl-user:: in front of *sk8-features*
	2  	11/28/95	dy      	new: with-temporary-ptr, with-temporary-handle
	3  	11/29/95	dy      	new: with-use-res-file, with-open-and-use-resource-file
	4  	11/29/95	dy      	export with-use-res-file, with-open-and-use-resource-file
	5  	11/29/95	dy      	rename with-use-res-file -> with-use-resource-file
	6  	11/29/95	dy      	#+SK8 sk8dev::T_UseResFile
	7  	11/29/95	dy      	move with-use-resource-file & with-open-and-use-resource-file to macros-traps.lisp in macframes
	8  	 1/ 4/96	dy      	new: with-use-resource-file and with-open-and-use-resource-file;
						back out the #+SK8 stuff
	9  	 1/10/96	Hernan  	Aaaaaaargggggggh!!! Cannot use the sk8dev package
						because it does not exist yet. Moving definition of with-
						use-resource-file to Real Build P1 Work.
	10 	 5/ 7/96	Hernan  	Removing stuff that uses T_ things which are NOT defined
						as part of Build Part 1...
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
