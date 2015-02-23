;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

; FILE          "CodeGenerator-class.lisp"
; IMPLEMENTS     Class for all CodeGenerators. 
; AUTHOR         Ken Dickey
; DATE           1994 August 3
; COPYRIGHT (c)  1994 Apple Computer, Inc. ; All rights reserved
; NOTES          See "Project-Walker.lisp"
; STATUS         Released              

(in-package :SK8Dev)

;; ---------  Protocol for all Code Generators  ---------

(defclass CodeGenerator () 
  ((project      :initform 'nil)
   (order-table  :accessor order-table)
   (libs-info    :accessor libs-info)
   (outp         :accessor outp)
   (warnp        :accessor warnp)
   (splash       :accessor splash)
   (sound        :accessor sound)
   (no-interact? :accessor no-interact?)
   (allConsItems :accessor allConsItems :initform '())
   (interEqDependencies :accessor interEqDependencies :initform '())
   (specialPropTable :accessor specialPropTable :initform '())
   (EqVectorIDTable :accessor EqVectorIDTable :initform '())
   (ConsTables :accessor ConsTables :initform '())
   ;; anonymous objects are kept in a vector during object creation
   ;; phase in order to generate code for them
   (anonymous-object-alloction-high-water 
    :accessor anonymous-object-high-water-index
    :initform '1) ; NB: MCL "insert" leaves "false" in slot 1; 1st SK8 index is "1"
   (out-of-project-objects :accessor out-of-project-objects :initform '())
   (localCreationRelations-objects 
    :accessor localCreationRelations-objects 
    :initform '())
  )
)

(defmethod project (:method-class mf::sk8-reader-method) ((me CodeGenerator) &key)
           (slot-value me 'project))

(defmethod (setf project) (:method-class mf::sk8-writer-method) (newValue (me CodeGenerator) &key)
           (setf (slot-value me 'project) newValue))


(defclass CodeGenerator-SK8-object-info-class ()
  ((sk8-object            :accessor sk8-object
                          :initarg :sk8-object)
   (creation-order-number :accessor creation-order-number
                          :initarg :creation-order-number)
   ;; when a slot-value is being set for an object which has not yet
   ;; been created, the setter info is queued on the object which needs
   ;; to be created.  After the object is created, the delayed slot
   ;; setting code gets generated.
   (setter-queue           :accessor object-setter-queue
                           :initform 'nil)
   ;; When an object has no object name, it is given an index in
   ;; an anonymous-object-vector, which only exists during object
   ;; creation and initialization time.
   (anonymous-object-vector-index :accessor anonymous-object-vector-index
                                  :initform 'nil)
   ;; We need to track delayed slot setting
   (delayed-setter-slots :accessor delayed-setter-slots
                         :initform 'nil)
  )
)

;; For delayed set! info

(defstruct delayed-setter-info
    object-to-be-updated
    property-name-of-slot-to-be-set
    value-of-slot-to-be-set
    use-set-instead-of-set!?
)


;;                                  --- E O F ---

#|
	Change History (most recent last):
	1  	 9/26/94	kend    	Generic code generator abstractions to be subclassed by specific {cross} code generators.
	2  	10/12/94	kend    	Made generic to SK8 and SX
	3  	10/13/94	sidney  	check in to clear a sourceserver glitch with the file
	4  	11/ 4/94	kend    	Save info on objects with localCreationRelations
	5  	 1/24/95	sidney  	more tweaking to suport save as text of strange objects
	6  	 3/18/95	sidney  	1227949: support changes to allow menu objects to saved and loaded right
	7  	 3/21/95	sidney  	no more top-level-actors field in codegenerator
	2  	 8/ 4/95	Hernan  	Adding properties for new eq stuff
	3  	 8/ 7/95	Hernan  	
	4  	 8/ 9/95	Brian   	
	3  	 2/27/97	Hernan  	Fixing method congruency problems.
|# ;(do not edit past this line!!)
