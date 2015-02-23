;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;; bootstrapping/testing code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bootstrap OBJECT object:
(setf *temp-hash-table* (make-hash-table :test #'eq :weak :value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (boundp '*build-in-progress-p*) *build-in-progress-p*  ;; hack to help our build process
             (constantp 'object) (eq object *undefined*))
    (let ((ccl::*warn-if-redefine-kernel* nil))
      (makunbound 'object))))
(defvar object)
(setf Object (make-instance 'object))
(setf (slot-value object 'SK8::sk8_id) 'object)
(setf (slot-value object 'SK8::sk8_flags)
      (logior (ash 1 %flags-named-object?)
              (ash 1 %flags-prototype-object?)
              (slot-value object 'SK8::sk8_flags)))
(set-class-owner (class-of object) object)
(mapc #'(lambda (p) (make-private-property sk8:object p)) *mf-internal-format-slot-names*)   
(setf (gethash 'object *temp-hash-table*) object)  ; RECORD-OBJECT

;; Bootstrap PROJECT object:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (boundp '*build-in-progress-p*) *build-in-progress-p*  ;; hack to help our build process
             (constantp 'project) (eq project *undefined*))
    (let ((ccl::*warn-if-redefine-kernel* nil))
      (makunbound 'project))))
(defvar project)
(setf project (make-instance 'project))
(setf (slot-value project 'SK8::sk8_id) 'project)
(setf (slot-value project 'SK8::sk8_flags)
      (logior (ash 1 %flags-named-object?)
              (ash 1 %flags-prototype-object?)
              (slot-value project 'SK8::sk8_flags)))
(set-class-owner (class-of project) project)
(setf (slot-value project 'file) nil)
;; these next two are to make sure that the properties are of the expected type so inspectors or whatever don't crash on the prototype project object
(setf (slot-value project 'objecttable) (make-hash-table :size 0 :test #'eq :weak :value))
(setf (slot-value project 'package) (find-package :mf))
(setf (gethash 'project *temp-hash-table*) project)      ; RECORD-OBJECT

;; Bootstrap SK8 project object:
(defclass sk8:sk8 (sk8:project)
  ((sk8:systemLog :initform nil)))

(defmethod sk8:systemLog (:method-class SK8-reader-method) ((me sk8:sk8) &key)
           (slot-value me 'sk8:systemLog))

(defmethod (setf sk8:systemLog) (:method-class SK8-writer-method) (value (me sk8:sk8) &key)
           (setf (slot-value me 'sk8:systemLog) value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (boundp '*build-in-progress-p*) *build-in-progress-p*  ;; hack to help our build process
             (constantp 'sk8) (eq sk8 *undefined*))
    (let ((ccl::*warn-if-redefine-kernel* nil))
      (makunbound 'sk8))))
(defvar sk8)
(setf sk8 (make-instance 'sk8))
(setf (slot-value sk8 'SK8::sk8_id) 'sk8)
(setf (slot-value sk8 'SK8::sk8_flags)
      (logior (ash 1 %flags-named-object?)
              (slot-value sk8 'SK8::sk8_flags)))
(set-class-owner (class-of sk8) sk8)
(setf (slot-value project 'file) nil)
(setf (slot-value project 'requiredProject) sk8)
(setf (slot-value sk8 'requiredProject) nil)
(setf (slot-value sk8 'requiringProjects) nil)
(setf (slot-value object 'knownchildren) (list project))
(setf (slot-value project 'knownchildren) (list sk8))
;;(setf (ccl::package-project *sk8-package*) *sk8-project*)
(setf (gethash 'sk8 *temp-hash-table*) sk8)      ; RECORD-OBJECT
;;(mapc #'(lambda (p) (make-private-property *project* p)) '(sk8::package sk8::objectTable))
(setf (slot-value sk8 'file) (namestring (full-pathname "ccl:SK8 Temporary Files;SK8"))) ; turned into a file object later in load
(setf (slot-value sk8 'package) (find-package :sk8))
(setf (slot-value sk8 'objectTable) *temp-hash-table*)

(setf (slot-value object 'project) sk8)
(setf (slot-value project 'project) sk8)
(setf (slot-value sk8 'project) sk8)

;;(defun timetest (n) (dotimes (i n) (new object :project sk8 :properties '(a b))))
;;(defun timetest2 (n) (dotimes (i n) (new object :project sk8)))
;;(defvar *test3var* 0)
;;;;(defun timetest3 (n) (dotimes (i n) (new object :project sk8 :properties '(a b)
;;                 :objectname (ccl::%str-cat "FOOO" (ccl::%integer-to-string (incf *test3var*))))))


#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/ 9/96	sidney  	suppress error in makunbound constants
	3  	 2/27/97	sidney  	light editing of comments to prepare for publication
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
