;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: SK8DEV -*-

(in-package :sk8dev)

;;; _______________________________ 
;;; Make-load-forms needed for storing projects in fasl format
;;; _______________________________ 

;; This particular make-load-form uses the split-lfun function to extract the contents of an lfun into a form
;; that can be written out and read back in. One component, the immediates, can contain circular references to the lfun.
;; That is dealt with by recreating the lfun in two steps, using dummy immediates in the first step

(defun create-blank-method-object-with-dummy-lfun (methodclassname)
  (ccl::%cons-method nil nil nil nil
                     (or (and methodclassname (find-class methodclassname nil))
                         ccl::*standard-method-class*)))

;;; add-method-to-class -- adds a method to class
;;;  method - an anonymous method object that is as of yet just filled with nil elements
;;;  lfun, name, qualifiers, specializers - the elements to fill in to the method object
;;;  Since this is being called only for SK8 handlers, specializers contains the class object that this method gets attached to
;;;
;;; Returns the method
;;; This should only be called when we are loading a method on a SK8 class

(defun add-method-to-class (method name qualifiers specializers lfun)
  (debug-msg "~%Loading method ~a..." name)
  (setf (CCL::%method-function method) lfun
        (CCL::%method-qualifiers method) qualifiers
        (CCL::%method-specializers method) specializers
        (CCL::%method-name method) name)
  (ccl::lfun-name (ccl::closure-function lfun) method)
  (let* ((fbinding (fboundp name))
         (gf (if (functionp fbinding) fbinding))
         (already-gf (and gf (typep gf 'ccl::standard-generic-function))))
    (unless already-gf
      (debug-msg "~%MAKING GENERIC FUNCTION ~s" name)
      (setq gf (ccl::make-gf name (ccl::lfun-bits (ccl::closure-function lfun)))))
    (let ((oldmethod (when fbinding (ignore-errors (find-method gf qualifiers specializers nil)))))
      (ccl::%add-method method gf t)
      (unless already-gf
        (when fbinding
          (ccl::forget-encapsulations name))
        (ccl::%fhave name gf))
      (when oldmethod (ccl::%move-method-encapsulations-maybe oldmethod method)))
    method))

(defmethod make-load-form ((me standard-method))
  (declare (special sk8dev::*FASL-CodeGenerator*))
  (let ((name (method-name me))
        (specializers (method-specializers me))
        (qualifiers (method-qualifiers me))
        (lfun (method-function me))
        (myclassname  (class-name (class-of me)))
        objclass obj) ;  objclassname)
    ;; determine if this method is a sk8 object's handler or accessor
    (setf objclass
          (cond ((and (listp name) (eq (car name) 'setf)) (second specializers))
                ((and name (symbolp name)) (first specializers)))
          ; objclassname (when objclass (class-name objclass))
          )
    (if (and objclass
             ; (or (null objclassname) (not (symbolp objclassname)))
             (boundp 'sk8dev::*FASL-CodeGenerator*)  ;; only do this when we are generating code
             sk8dev::*FASL-CodeGenerator*
             (setf obj (mf::safe-class-owner objclass)))  ;; its a sk8 thing
      (values
       `(create-blank-method-object-with-dummy-lfun
         ',(when (symbolp myclassname) myclassname))
       `(add-method-to-class
         ,me
         ',name
         ',qualifiers
         (substitute (mf::maybe-make-own-object-class ,(sk8dev::gen-value-string obj sk8dev::*FASL-CodeGenerator*))
                     '$%objclass%$
                     ',(substitute '$%objclass%$ objclass specializers))
         ,lfun
         ))
      ;; This should never happen, and it is now an error since there is no next method. 
      (error "Cannot save a method which is not a SK8 handler.")
      )))

;; based on join-lfun-with-dummy-immediates in WOOD

(defun generate-dummy-lfun-immediates (icode linkmap)
  (let* ((imm-count (ash (length linkmap) -1))
         (indices (make-array imm-count))
         (max-index -1))
    (declare (fixnum imm-count))
    (dotimes (i imm-count)
      (let ((index (aref icode (1+ (ash (aref linkmap (+ i i)) -1)))))
        (when (> index max-index) (setq max-index index))
        (setf (aref indices i) index)))
    (let ((imms (make-array (1+ max-index) :initial-element '*%dummy-imm%*)))
      (values imms
              indices))))

;; this is what allows lfun to appear in make-load-form of standard-method

(defmethod make-load-form ((me method-function))
  (multiple-value-bind (imms icode linkmap bits attrib fasl-version)
                       (ccl::split-lfun me #+ccl-3 t) ;; argument added in MCL3.0 version
    (multiple-value-bind (dummy-imms indices)
                         (generate-dummy-lfun-immediates icode linkmap)
      (values `(ccl::join-lfun ,dummy-imms ,icode ,linkmap ,bits ,attrib ,fasl-version)
              `(ccl::%patch-lfun-immediates ,me ',imms ',indices)))))


#|
	Change History (most recent last):
	2  	 5/20/96	sidney  	Add make load forms to build
	3  	 8/ 1/96	Hernan  	In make-load-form of standard-method, removed one of
						the checks of whether a method is a SK8 handler. This 
						change was required because the way handlers are
						represented has changed since porting to the PPC.
	4  	10/17/96	sidney  	remove a debugging print that had been checked in
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
