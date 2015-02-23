(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :public ; Updated  8-04-94   2:23 pm
                  SK8::CONSTANTDOCUMENTATION SK8::FUNCTIONDOCUMENTATION SK8::HANDLERDOCUMENTATION
                  SK8::OBJECTDOCUMENTATION SK8::PROPERTYDOCUMENTATION SK8::VARIABLEDOCUMENTATION)

;;; Expects a sk8 object.

(define-sk8-function objectDocumentation nil (theObject &key (inherited nil))
  (cl-user::with-help-file
   (let (object_ ok)
     (dolist (i (if inherited (ancestors theobject) (cons theobject (ancestors theobject))))
       (when (setf object_ (cl-user::fetch-object (string-upcase (objectName i))))
         (setf ok i)
         (return)))
     (if object_
       (sk8-multivals 
        ok
        (cl-user::object_description object_)   ;; description
        (cl-user::object_example object_)       ;; example
        (cl-user::object_seeAlso object_))      ;; see also
       (sk8-multivals nil "" "" "")
       ))))

;;; Given a list of argument_ objects it returns a list of lists of the form:
;;; ((name, type, required,description) ...)
;;; Eact item in the inner list is a string.

(defun arguments-to-strings (theArgs)
  (let ((result nil))
    (dolist (c theArgs (reverse result))
      (push (list (cl-user::argument_name c)
                  (cl-user::argument_type c)
                  (cl-user::argument_required c)
                  (cl-user::argument_description c)) 
            result))))

;;; Expects a SK8 object and a symbol (the property name).

(defmacro tis-a-property? (theObj theSymbol)
  `(and (some #'(lambda (x) (inheritsFrom ,theObj x))  (implementors ,theSymbol))
        (some #'(lambda (x) (inheritsFrom ,theObj x)) (implementors (setter ,theSymbol)))))

(defmacro tis-a-local-property? (theObj theSymbol)
  `(and (memq ,theObj  (implementors ,theSymbol))
        (memq ,theObj (implementors (setter ,theSymbol)))))

;;; LISTEN: if something has a getter AND a setter, it is a property!!!

(define-sk8-function propertyDocumentation nil (theObject theProperty &key (inherited nil))
  (cl-user::with-help-file
   (let (object_ property_ ok)
     (if (not (tis-a-property? theObject theProperty)) ;; (not (memq theProperty (properties theobject)))
       (error "Can't get documentation, because ~a is not a property of ~a" theproperty theobject))
     ;; Now find the object that really implements this.
     (dolist (i (if inherited (ancestors theobject) (cons theobject (ancestors theobject))))
       (when (and (tis-a-local-property? i theProperty)
                  (setf object_ (cl-user::fetch-object (string-upcase (objectName i))))
                  (setf property_ 
                        (cl-user::fetch-property object_
                                                 (pretty-symbol-name theProperty))))
         (setf ok i)
         (return)))
     (if ok
       ;; Return the arguments as strings.
       (sk8-multivals 
        ok
        (cl-user::property_description property_)                             ; Desc.
        (cl-user::property_getter_description property_)                      ; Getter desc.
        (arguments-to-strings (cl-user::property_getter_arguments property_)) ; Getter args.
        (cl-user::property_setter_description property_)                      ; Setter desc.
        (arguments-to-strings (cl-user::property_setter_arguments property_)) ; Setter args.
        (cl-user::property_example property_)                                 ; Example.
        (cl-user::property_seeAlso property_))                                ; SeeAlso.
       (sk8-multivals nil "" "" "" "" "" "" "" )
       ))))

;;; Expects a handler object.

(define-sk8-function handlerDocumentation nil (theHandler &key (inherited nil))
  (let ((handlerName (name theHandler))
        (theobject (object theHandler))
        object_ handler_ ok args
        )
    (if (listp handlerName)
      (setf handlerName (cadr handlerName)))
    (cl-user::with-help-file
     (dolist (i (if inherited (ancestors theobject) (cons theobject (ancestors theobject))))
       (when (and (memq handlerName (mapcar #'name (handlers i)))
                  (setf object_ (cl-user::fetch-object (string-upcase (objectName (object theHandler)))))
                  (setf handler_  (cl-user::fetch-handler object_
                                                          (pretty-symbol-name handlerName))))
         (setf ok i)
         (return)))
     (if ok
       (progn
         (setf args (cl-user::handler_arguments handler_))
         (sk8-multivals (car (remove-if-not #'(lambda (x) (eq (name x) handlername)) (handlers ok)))
                        (cl-user::handler_description handler_) ;; Desc.
                        (arguments-to-strings args)             ;; Args.
                        (cl-user::handler_example handler_)     ;; Example.
                        (cl-user::handler_seeAlso handler_))    ;; SeeAlso.
         )
       (sk8-multivals nil "" "" "" "")))))

(define-sk8-function functionDocumentation nil (thefunction)
  (cl-user::with-help-file
   (let ((functionName (name thefunction)) function_ args)
     (setf function_ 
           (cl-user::fetch-function (pretty-symbol-name functionName)))
     (if function_
       (progn
         ;; Return the arguments as strings.
         (setf args (cl-user::function_arguments function_))
         (sk8-multivals (cl-user::function_description function_) ;; Desc.
                        (arguments-to-strings args)             ;; Args.
                        (cl-user::function_example function_)     ;; Example.
                        (cl-user::function_seeAlso function_)))    ;; SeeAlso.
       (sk8-multivals "" "" "" "")
       ))))


(define-sk8-function constantDocumentation nil (theconstant)
  (cl-user::with-help-file
   (let (constant_)
     (setf constant_ 
           (cl-user::fetch-constant theconstant))
     (if constant_
       (sk8-multivals (cl-user::constant_description constant_) ;; Desc.
                      (cl-user::constant_example constant_)     ;; Example.
                      (cl-user::constant_seeAlso constant_))    ;; SeeAlso.
       (sk8-multivals "" "" "")
       ))))



(define-sk8-function variableDocumentation nil (thevariable)
  (cl-user::with-help-file
   (let (variable_)
     (setf variable_ 
           (cl-user::fetch-variable thevariable))
     (if variable_
       (sk8-multivals (cl-user::variable_description variable_) ;; Desc.
                      (cl-user::variable_example variable_)     ;; Example.
                      (cl-user::variable_seeAlso variable_))    ;; SeeAlso.
       (sk8-multivals "" "" "")
       ))))


#|
	Change History (most recent last):
	1	8/3/94	Hernan	New file. Implements high level access to the 
				WOOD documentation dbase.
	2	8/4/94	rod	Adding functions for getting docs on functions,
				constants, and variables.
	3	8/5/94	rod	
	4	8/5/94	rod	
	5	8/5/94	rod	
	6	8/22/94	rod	
	7  	 8/23/94	rod     	
	8  	 2/13/95	Hernan  	Created the macro tis-a-property? which returns
							true when the symbol given names a getter and
							a setter that are both implemented by the object
							in question. This ensures that things defined as
							properties (which are documented as properties)
							can be viewed in the doc window.
	9  	 3/22/95	Hernan  	1230871: tis-a-property? should take into account
							inherited handlers.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
