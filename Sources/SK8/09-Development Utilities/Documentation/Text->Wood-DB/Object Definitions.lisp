;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)

;;; CLOS class definitions for the objects we use to store information
;;; about objects, handlers, properties, functions, etc. 

(defclass object_ ()
  ((name :accessor object_name :initarg :name :initform nil)
   (info :accessor object_info :initarg :info :initform nil)
   (owner :accessor object_owner :initarg :owner :initform nil)
   (description :accessor object_description :initarg :description :initform nil)
   (example :accessor object_example :initarg :example :initform nil)
   (seealso :accessor object_seealso :initarg :seealso :initform nil)
   (properties :accessor object_properties :initarg :properties :initform nil)
   (handlers :accessor object_handlers :initarg :handlers :initform nil))
  )

(defclass argument_ ()
  ((name :accessor argument_name :initarg :name :initform nil)
   (type :accessor argument_type :initarg :type :initform nil)
   (required :accessor argument_required :initarg :required :initform nil)
   (description :accessor argument_description :initarg :description :initform nil))
  )

(defclass handler_ ()
  ((name :accessor handler_name :initarg :name :initform nil)
   (info :accessor handler_info :initarg :info :initform nil)
   (description :accessor handler_description :initarg :description :initform nil)
   (arguments :accessor handler_arguments :initarg :arguments :initform nil)
   (example :accessor handler_example :initarg :example :initform nil)
   (seealso :accessor handler_seealso :initarg :seealso :initform nil))
  )

(defclass property_ ()
  ((name :accessor property_name :initarg :name :initform nil)
   (info :accessor property_info :initarg :info :initform nil)
   (description :accessor property_description :initarg :description :initform nil)
   (getter_description :accessor property_getter_description :initarg :getter_description :initform nil)
   (getter_arguments :accessor property_getter_arguments :initarg :getter_arguments :initform nil)
   (setter_description :accessor property_setter_description :initarg :setter_description :initform nil)
   (setter_arguments :accessor property_setter_arguments :initarg :setter_arguments :initform nil)
   (example :accessor property_example :initarg :example :initform nil)
   (seealso :accessor property_seealso :initarg :seealso :initform nil))
  )

(defclass function_ ()
  ((name :accessor function_name :initarg :name :initform nil)
   (info :accessor function_info :initarg :info :initform nil)
   (syntax :accessor function_syntax :initarg :syntax :initform nil)
   (description :accessor function_description :initarg :description :initform nil)
   (arguments :accessor function_arguments :initarg :arguments :initform nil)
   (example :accessor function_example :initarg :example :initform nil)
   (seealso :accessor function_seealso :initarg :seealso :initform nil))
  )

(defclass constant_ ()
  ((name :accessor constant_name :initarg :name :initform nil)
   (info :accessor constant_info :initarg :info :initform nil)
   (description :accessor constant_description :initarg :description :initform nil)
   (example :accessor constant_example :initarg :example :initform nil)
   (seealso :accessor constant_seealso :initarg :seealso :initform nil))
  )

(defclass variable_ ()
  ((name :accessor variable_name :initarg :name :initform nil)
   (info :accessor variable_info :initarg :info :initform nil)
   (description :accessor variable_description :initarg :description :initform nil)
   (example :accessor variable_example :initarg :example :initform nil)
   (seealso :accessor variable_seealso :initarg :seealso :initform nil))
  )


#|
	Change History (most recent last):
	1  	 2/ 5/96	Hernan  	New file. Moved here the class definitions used in parsing
						the doc files. The reason for the move was to separate 
						them from Wood (who is going away).
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
