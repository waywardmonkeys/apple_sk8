;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; we have the basics, now for some mre exported API functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AddProperty RemoveProperty GetPropertyValue SetPropertyValue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; very implementation-dependent object stuff
(defun obsolete-class-and-subclasses (class)
  (let ((subclasses (ccl::%class-subclasses class)))
    (dolist (subclass (if (listp subclasses) subclasses (CCL::population-data subclasses)))
      (obsolete-class-and-subclasses subclass)))
  ;; Mark the class as obsolete
  (setf (ccl::%class-cpl class) nil)
  ;; Mark its instances as obsolete
  (ccl::make-instances-obsolete class))

;;; addProperty -- adds a new property to an object
;;;   propertyName - a symbol
;;;   private - if non-nil, make the property private
;;;   propagatedValue - if non-nil, then make the property propagated by its descendants
;;;   initialValue - if non-nil, then set the initial value of the property to it (else set to false)
;;;
(defmethod addProperty ((me Object) name &key
                          (type nil)
                          (private nil)
                          (propagatedValue nil)
                          (initialValue nil))
  (declare (ignore type))  ;; We ignore this for now.  This type is used in conversion to typed languages.
  (require-type name 'symbol)
  (unless (slot-exists-p me name)
    (let* ((class (class-of me))
           (owner (class-owner class)))
      (when (assq name (CCL::%class-local-instance-slotds class))
        (error "Property ~a can't be added because it already exists in ~a" name me))
      
      (when (position #\space (symbol-name name))   ;;to prevent users calling "addproperty objectXXX, 'bogusprop   '"
        (error "Property '~a' can't be added because it has spaces in it's name" name))
      
      (when (and (fboundp name) (not (ccl::standard-generic-function-p (fboundp name))))
        (error "Property '~a' can't be added to ~a because it is already defined as a function" name me))

      (when propagatedValue
        (setq initialValue (make-dynamic-slotvalue-inheriter me initialValue)))
      
      (without-interrupts
       (cond
        ;; The object already owns its class; just add the slot to its class...
        ((eq owner me)
         (obsolete-class-and-subclasses class)
         (push (ccl::%cons-slotd name nil nil nil) (ccl::%class-local-instance-slotds class))
         (CCL::initialize-class class t))
        ;; The object DOESN'T own its class; make a new class with the additional property...
        (t
         (setq class (make-own-object-class me :newProperty name))))
       ;; Create accessor methods
       (add-SK8-accessors class name propagatedValue)
       ;; Store the given value (this object's descendants get the new value when they're updated)
       (setf (slot-value me name) initialValue))
      (setf (slot-value me 'SK8::sk8_flags) (logior (ash 1 %flags-specialized-object?) (slot-value me 'SK8::sk8_flags)))  ;; (specialized-object! me)
      (if private
        (make-private-property me name)
        (publish-project-symbol name (project me)))))
  name)

;;; Looks through the parents for one that has the given property
;;;  - if one is found, returns its property value (NOTE: this may be a "dynamic-slotvalue-inheriter" object!)
;;;  - otherwise, returns the 'default' value
;;;
(defun find-parent-slot-value (obj property &optional (default (ccl::%slot-missing-marker)) returnParent?)
  (do-SK8-parents (parent obj)
    (let ((val (slot-value-no-error parent property)))
      (unless (eq val (ccl::%slot-missing-marker))
        (return-from find-parent-slot-value (if returnParent? parent val)))))
  ;; If we get to this point, there's no hope
  default)

;;; This is called when an obsolete instance is updated (e.g. after adding a slot or changing parents),
;;; in order to get the right initial values for any new slots.
;;;
(defun update-new-slots-from-parents (obj)
  (dolist (slot (class-instance-slots (class-of obj)))
    (setq slot (car slot))
    ;; We use the high-level slot-access functions here in case the slot-access to the parent causes further updates!
    (when (eq (slot-value-no-error obj slot) (CCL::%unbound-marker-8))
      (setf (slot-value obj slot) (find-parent-slot-value obj slot nil)))))

;; these are called by MCL when an instance is updated because its class has changed.
;; They are intended to be a user hook for purposes like this
;; Here we use the hook to propogate properties from new parents
(defmethod update-instance-for-redefined-class ((me SK8:Object) addedSlots removedSlots propList &rest initArgs)
  (declare (ignore addedSlots removedSlots propList initArgs))
  (update-new-slots-from-parents me)
  (call-next-method))

(defmethod update-instance-for-different-class ((oldObject SK8:Object) (me SK8:Object) &rest initArgs)
  (declare (ignore oldObject initArgs))
  (update-new-slots-from-parents me)
  (call-next-method))

;;; Differs from find-local-handler mainly in that it will find a method even if it's a mirror for some other class's method
;;;
(defun find-primary-method-on-obj (genericFunction classOrObj)
  (when (ccl::standard-generic-function-p genericFunction)
    (let ((class (if (ccl::classp classOrObj) classOrObj (class-of classOrObj))))
      (dolist (m (ccl::%gf-methods genericFunction))
        (when (and (memq class (method-specializers m)) (null (method-qualifiers m)))
          (return m))))))

(defun hidden-handler-p (method)
  (null (getf (CCL::%lfun-info (ccl::closure-function (method-function method)))
              :SK8-handler)))

(defun set-sk8-handler-p (method)
  (let ((plist (CCL::%lfun-info (ccl::closure-function (method-function method)))))
    (when plist
      (setf (getf (cddr plist)
                  :SK8-handler) t))))

(defun remove-SK8-accessors (class propertyName)
  (let* ((setterName (CCL::setf-function-name propertyName))
         (getterGF (symbol-function propertyName))
         (setterGF (symbol-function setterName))
         (getter (find-primary-method-on-obj getterGF class))
         (setter (find-primary-method-on-obj setterGF class)))
    (when (and getter (hidden-handler-p getter))
      (remove-method getterGF getter))
    (when (and setter (hidden-handler-p setter))
      (remove-method setterGF setter))))

;;; removeProperty -- removes propertyName from object
;;;
(defmethod removeProperty ((me Object) name &key)
  (require-type name 'symbol)
  (when (slot-exists-p me name)
    (let* ((class (class-of me))
           (localSlots (CCL::%class-local-instance-slotds class)))
      ;; Error checking:
      (unless (assq name localSlots)
        (error "Property ~a can't be removed because it's not local to ~a" name me))
      (when (memq name *mf-internal-format-slot-names*)
        (error "Reserved property '~a' cannot be removed from object ~s" name me))
      (without-interrupts
       ;; Remove the associated accessors
       (remove-SK8-accessors class name)
       ;; deal with ports later
       #+ignore
       (progn
         ;; Remove any attached ports
         (dolist (p (gethash me *input-port-hash-table*))
           (and (eq me (portObject p))
                (eq name (portProperty p))
                (SK8:detachPort p)))
         (dolist (p (gethash me *output-port-hash-table*))
           (and (eq me (portObject p))
                (eq name (portProperty p))
                (detachPort p)))
         )
       ;; Update the class
       (obsolete-class-and-subclasses class)
       (setf (ccl::%class-local-instance-slotds class)
             (delete name localSlots :test #'eq :key #'car))
       (CCL::initialize-class class t)
       ;; Refresh obj's internal creationRelations info if necessary
       (maybe-update-creationRelations-due-to-removed-slots me name))
      t)))

;;; getPropertyValue -- this is the same functionality as "get the prop of me", except that it allows
;;;   the following:
;;;        set p to 'someProperty'
;;;        set x to getpropertyvalue(me,p)
;;;        if (the someProperty of me) = x then beep -- this should be the case!
;;;
(defmethod getPropertyValue ((me Object) property &key)
  (funcall property me))

;;; setPropertyvalue -- same idea as getPropertyValue, except that you can use this to
;;;    set a property value
;;;
(defmethod setPropertyValue ((me Object) property value &key)
  (funcall (ccl::setf-function-name property) value me))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapAncestors Ancestors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mapancestors1 (parents fun)
  (declare (special *ancestors*))
  (if (and parents (listp parents))
    (dolist (p parents)
      (unless (memq p *ancestors*)
        (funcall fun p)
        (push p *ancestors*))
      (mapancestors1 (parents p) fun))))

(defmethod mapAncestors ((me Object) &key ((:function fun) nil))
  (when fun
    (let ((*ancestors* nil))
      (declare (special *ancestors*))
      (mapancestors1 (parents me) fun))))

(defmethod ancestors ((me Object) &key)
  (let (result)
    (mapancestors me :function #'(lambda (obj) (setq result (nconc result (cons obj nil)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; should discard not be part of the core object system?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equalTo and greaterThan can be in an addon to the core that also deals with
;; built-in types that act like objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inheritedProperties inheritedVirtualProperties localProperties localVirtualProperties
;; properties virtualProperties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Need to build one of these for each object with virtual
;;; properties.  See the file editableproperties.lisp.
(defmethod sk8:localVirtualProperties ((me sk8:Object) &key)
  (declare (special sk8:Object))
  (when (eq me sk8:object)
    '(sk8:ancestors sk8:project sk8:properties
      sk8:parents sk8:prototype
      sk8:knowndescendants sk8:objectname)))

(defmethod sk8:localVirtualProperties ((me sk8:project) &key)
  (declare (special sk8:project))
  (when (eq me sk8:project)
    '(sk8:constants sk8:functions sk8:globals
      sk8:menus sk8:objects sk8:windows)))

(defmethod sk8:inheritedVirtualProperties ((me sk8:Object) &key)
  (let (props)
    (dolist (each (sk8:parents me))
      (setq props (union props (sk8:virtualproperties each))))
    props))

(defmethod sk8:virtualProperties ((me sk8:Object) &key)
  (union (localvirtualproperties me)
         (inheritedvirtualproperties me)))

(defmethod sk8:inheritedRealProperties ((me sk8:Object) &key)
  (set-difference (sk8:realProperties me)
                  (sk8:localRealProperties me)))

(defmethod sk8:localRealProperties ((me sk8:Object) &key)
  (set-difference
   (mapcar #'car (class-direct-instance-slots (class-of me)))
   *mf-internal-format-slot-names*)
  )

;;; localProperties -- returns a list of all the property names in the object (list of symbols)
;;;   this includes the real and the virtual properties
;;;
(defmethod sk8:localProperties ((me sk8:Object) &key)
  (let ((realProps (sk8:localRealProperties me)))
    (nconc realProps
           (remove realProps (sk8:localVirtualProperties me)
                   :test #'(lambda (reals vp) (memq vp reals))))))

;;; realProperties -- returns all real properties of object (these are the properties
;;;   that have space allocated on the object)
;;;
(defmethod sk8:realProperties ((me sk8:Object) &key)
  (set-difference
   (mapcar #'car (class-instance-slots (class-of me)))
   mf::*mf-internal-format-slot-names*)
  )

;;; inheritedProperties -- returns the inherited properties of object
;;;

(defmethod sk8:inheritedProperties ((me sk8:Object) &key)
  (let ((realProps (sk8:inheritedRealProperties me)))
    (nconc realProps
           (remove realProps (sk8:inheritedVirtualProperties me)
                   :test #'(lambda (reals vp) (memq vp reals))))))

;;; property -- query whether property belongs to object
;;;

(defmethod sk8:realProperty ((me sk8:Object) name &key)
  (when (slot-exists-p me name)
    t))

(defmethod sk8:realProperty ((me T) name &key)
  (declare (ignore me name))
  nil)

;;; properties -- returns all properties of the object
;;;
(defmethod sk8:properties ((me sk8:Object) &key)
  (let ((realProps (sk8:realProperties me)))
    (nconc realProps
           (remove realProps (sk8:virtualProperties me)
                   :test #'(lambda (reals vp) (memq vp reals))))))

(defmethod sk8:properties ((me t) &key)
  (declare (ignore me))
  '(sk8:knownChildren sk8:parents))

;;; property -- returns true if property is a property of object
;;;
(defmethod sk8:property ((me sk8:Object) name &key)
  (when   ;; this should return t or nil only
    (or (slot-exists-p me name)
        (memq name (sk8:localVirtualProperties me))
        (progn
          (sk8:mapAncestors me :function #'(lambda (obj)
                                             (when (memq name (sk8:localVirtualProperties obj))
                                               (return-from sk8:property t))))
          nil))
    t))

(defmethod sk8:property ((me t) name &key)
  (when
    (memq name (sk8:properties me))
    t))

(defmethod sk8:localProperty ((me sk8:Object) name &key)
  (when (member name (slot-value (class-of me) 'ccl::direct-slots)
                :test #'eq :key #'car)
    t))

(defmethod sk8:localProperty ((me t) name &key)
  (declare (ignore me))
  nil)

(defmethod sk8:inheritedProperty ((me sk8:Object) name &key)
  (and (slot-exists-p me name)
       (not (sk8:localProperty me name))))

(defmethod sk8:inheritedProperty ((me t) name &key)
  (when
    (memq name (sk8:properties me))
    t))

;;; propertiesWithValue returns a list of the names of properties of an object that have a given value
;;;
(defmethod sk8:propertiesWithValue ((me sk8:Object) value  &key (test #'eql) (inherited? t) (local? t) (real? t) (virtual? t))
  (ignore-errors 
   (let* ((reals (when real?
                   (if inherited?
                     (if local?
                       (sk8:realProperties me)
                       (sk8:inheritedRealProperties me))
                     (when local? (sk8:localRealProperties me)))))
          (virts (when virtual?
                   (if inherited?
                     (if local?
                       (sk8:virtualProperties me)
                       (sk8:inheritedVirtualProperties me))
                     (when local? (sk8:localVirtualProperties me)))))
          (props (union reals virts)))
     (remove value props :key #'(lambda(prop) (funcall prop me)) :test-not test))))

(defmethod sk8:dontSave ((me sk8:object) &key)
  (declare (ignore me))
  nil)

(defmethod sk8::dontSave ((thing t) &key)
  (declare (ignore thing))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  preserve, restore   For Object do nothing, just act as base virtual function
;;    will need something more for Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod preserve ((me Object) &key)
  (declare (ignore me)))

(defmethod restore ((me Object) &key)
  (declare (ignore me))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handler stuff
;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sk8:typeSatisfied ((me SK8:Object) specific &key)
  (cond ((eq me specific) t)
        ((eq me SK8:Object)
         (or (not (ccl::standard-instance-p specific))
             (typep specific (class-of me))))
        (t (let ((mytype (class-of me)))
             (and (neq mytype (class-of specific))  ;; instances of same class are unspecialized children of common parent
                  (typep specific (class-of me)))))))

(defmethod sk8:typeSatisfied ((me ccl::built-in-class) specific &key)
  (typep specific (class-name me)))

(defmethod sk8:typeSatisfied ((me t) specific &key)
  (typep specific (class-of me)))

(defun SK8dev::is-a (specific generic)
  (sk8:typesatisfied generic specific))

(defun sk8:inheritsfrom (specific generic)
  (sk8:typesatisfied generic specific))

(defmethod sk8::ensureType ((me t) type &key
                               (warning "is not of the expected type"))
  (declare (ignore warning))
  (if (consp type)
    (if (some #'(lambda (ancestor) (sk8:inheritsfrom me ancestor))
              type)
      me
      (SK8:SK8-error sk8:TypeMismatchError :object me :expectedType type))
    (if (sk8:inheritsFrom me type)
      me
      (SK8:SK8-error sk8:TypeMismatchError :object me :expectedType type))))

(defun sk8:nameAlreadyInUse (theString theProj)
  (let ((theSymbol (find-symbol (string-upcase theString) (sk8::package theProj))))
    (and theSymbol 
         (boundp theSymbol) 
         (sk8:inheritsFrom (symbol-value theSymbol) object)
         ;; This case ensures that the symbol we found is not a variable
         ;; but an actual object.
         (string-equal (sk8:objectName (symbol-value theSymbol)) theString))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method Information Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun second-arg-dispatch-name-p (name)
  (or (listp name)
      (and (symbolp name)
           (>= (the fixnum (length (setq name (symbol-name name)))) 5)
           (string= "WITH " name :end2 5))))

;;; make-internal-function-name -- returns a valid internal function name for use by the store
;;;  name - either a string, a cons or a symbol
;;;       If string: then it should have a valid SK8 function name (e.g., "foo" or "set foo")
;;;       If cons: then it should be of the form (setf name), where name is a symbol
;;;       If symbol: then it should be the function name. This is kludged up a bit for setf functions to deal with what happens when a project has been deleted
;;;  object - if provided, the name is assumed to represent a handler name (rather than a function name)
;;;
;;;  Note: internally, a function name is either (setf name) or name.
;;;
(defun make-internal-function-name (name &optional obj)
  (cond ((consp name)
         name)
        ((symbolp name)
         (if (eq (symbol-package name) ccl::*setf-package*)
           (let ((fun (fboundp name)))
             (when fun
               (let* ((funname (function-name name))
                      (funsym (when (consp funname) (second funname))))
                 (when (and funsym (symbolp funsym) (null (symbol-package funsym)))  ;; package was deleted
                   (fmakunbound name))))  ;; this helps clean things up
             (let* ((symstring (symbol-name name))  ;; it is now of the form package::symbolname
                    (wherecol (search "::" symstring))
                    (package (or (and wherecol (find-package (subseq symstring 0 wherecol)))
                                 (and obj
                                      (if (sk8:inheritsFrom obj sk8:Project)
                                        (sk8::package obj)
                                        (sk8::package (sk8:project obj)))))))
               (list 'cl::setf (intern (if wherecol
                                         (subseq symstring (+ 2 wherecol))
                                         symstring) package))))
           name))
        ((stringp name)
         (let* ((p (search "set " name))
                (package (if obj
                           (if (sk8:inheritsFrom obj sk8:Project)
                             (sk8::package obj)
                             (sk8::package (sk8:project obj)))))
                (sym (if p
                       (find-symbol (string-upcase (subseq name (1+ (search " " name)))) (or package *package*))
                       (find-symbol (string-upcase name) (or package *package*)))))
           (unless sym (error "Can't find ~:[function~;handler~] ~a" obj name))
           (if p
             (list 'cl::setf sym)
             sym)))
        (t (error "Invalid function name ~s" name))))

;;;  Mirrors sourceMethod onto destClass
;;;
(defun mirror-method (genericFunction sourceMethod destClass)
  (when (symbolp destClass) (setq destClass (find-class destClass)))
  (let* ((specializers (method-specializers sourceMethod))
         (dispatchArgNum (position ccl::*T-class* specializers :test #'neq))
         (destClassSpecializers (make-list (length specializers) :initial-element ccl::*T-CLASS*))
         (sourceMethodFunction-maybeWrapper (method-function sourceMethod))
         (sourceMethodFunction sourceMethodFunction-maybeWrapper)
         destClass-newMethod)
    (setf (elt destClassSpecializers dispatchArgNum) destClass)
    (setq destClass-newMethod (make-instance (class-of sourceMethod)
                                :function sourceMethodFunction
                                :specializers destClassSpecializers
                                :name (method-name sourceMethod)))
    (add-method genericFunction destClass-newMethod)))

;;;  Returns true if this method is a mirror-method for some other class's method
;;;
(defun mirror-method-p (method)
  (neq method (CCL::lfun-name (ccl::closure-function (method-function method)))))

;;; find-local-handler -- returns the local handler of the given name for the given object
;;;
(defun find-local-handler (name obj &key qualifier)
  (declare (ignore qualifier)) ; since we no longer support befores & afters
  (setq name (make-internal-function-name name obj))
  (when (CCL::standard-instance-p obj)
    (let* ((generic-function (fboundp name))
           (arg2-dispatch-p (second-arg-dispatch-name-p name))
           (clos-class-shadow-p (clos-class-shadow-p obj))
           (class (cond (clos-class-shadow-p obj)
                        ((eq obj Object) CCL::*t-class*)
                        (t (class-of obj))))
           specializers)
      (when (ccl::standard-generic-function-p generic-function)
        (dolist (handler (CCL::%gf-methods generic-function))
          (setq specializers (CCL::%method-specializers handler))
          (when (eq class (if arg2-dispatch-p (second specializers) (first specializers)))
            (return-from find-local-handler (unless (and clos-class-shadow-p (mirror-method-p handler)) handler))))))))


;;; find-applicable-handler
;;;   Returns the first applicable handler of the given name for the given object (may be the local handler),
;;;   i.e. the most specific handler that would be invoked if the named generic-function were called with
;;;   the given object, or NIL if there's no applicable handler
;;;
(defun find-applicable-handler (name obj)
  (setq name (make-internal-function-name name obj))
  (let* ((clos-class-shadow-p (clos-class-shadow-p obj))
         (baseClass (cond (clos-class-shadow-p obj)
                          (t (class-of obj))))
         (classes (class-precedence-list baseClass)))
    (dolist (class classes)
      (dolist (handler (CCL::specializer-direct-methods class))
        (unless (or (hidden-handler-p handler)
                    ;; why should we exclude mirrored handlers? was: (and clos-class-shadow-p (mirror-method-p handler))
                    )
          (when (equal (method-name handler) name)
            (return-from find-applicable-handler handler)))))))


;;; find-inherited-handler -- returns the first handler of the given name inherited by object
;;;
(defun find-inherited-handler (name obj)
  (unless (eq obj SK8:Object)
    (setq name (make-internal-function-name name obj))
    (let* ((clos-class-shadow-p (clos-class-shadow-p obj))
           (baseClass (cond (clos-class-shadow-p obj)
                            (t (class-of obj))))
           (classes (cdr (class-precedence-list baseClass))))
      ;; mirrored methods are found in the local list, but are treated like they are inherited
      (when clos-class-shadow-p
        (dolist (handler (CCL::specializer-direct-methods baseClass))
          (unless (or (hidden-handler-p handler)
                      (not (mirror-method-p handler)))
            (when (equal (method-name handler) name)
              (return-from find-inherited-handler handler)))))
      (dolist (class classes)
        (dolist (handler (CCL::specializer-direct-methods class))
          (unless (hidden-handler-p handler)
            (when (equal (method-name handler) name)
              (return-from find-inherited-handler handler))))))))


;;; find-handler - returns the handler that meets the criteria
;;;
(defun find-handler (name obj &key (local-p t) (inherited-p nil))
  (cond (local-p (find-local-handler name obj))
        (inherited-p (find-applicable-handler name obj))))

;;; get-handlers -- returns all handler objects that meet criteria
;;;  local-p - return local handlers
;;;  inherited-p - return inherited handlers
;;;
;;; Deals with inheritance by only returning the first method of any one name that is found
;;; That ignores the possibility of specialization of a handler on more than one argument, but
;;; the whole concept of a handler belonging to one object makes the same assumption.
;;;
(defun get-handlers (obj &key (local-p t) (inherited-p t))
  (let* ((clos-class-shadow-p (clos-class-shadow-p obj))
         (baseClass (cond (clos-class-shadow-p obj)
                          ((eq obj sk8:Object) CCL::*t-class*)
                          (t (class-of obj))))
         (classes (when inherited-p (cdr (class-precedence-list baseClass))))  ;; this doesn't find pseudoparents, but methods inherited from them show up as mirrored methods
         (handlers nil)
         (handlerNames nil)
         handlerName)
    (when (or local-p (and inherited-p clos-class-shadow-p))
      (dolist (handler (CCL::specializer-direct-methods baseClass))
        (unless (or (hidden-handler-p handler)
                    (and (eq baseClass CCL::*t-class*)
                         (neq (object handler) sk8::object))
                    (if (mirror-method-p handler) ;; mirror method looks local but should be treated as inherited
                      (not inherited-p)
                      (not local-p)))
          (setq handlerName (method-name handler))
          (unless (member handlerName handlerNames :test #'equal)
            (push handler handlers)
            (push handlerName handlerNames))))
      )
    (dolist (class classes)
      (dolist (handler (CCL::specializer-direct-methods class))
        (unless (or (hidden-handler-p handler)
                    (and (eq class CCL::*t-class*)
                         (neq (object handler) sk8::object)))
          (setq handlerName (method-name handler))
          (unless (member handlerName handlerNames :test #'equal)
            (push handler handlers)
            (push handlerName handlerNames)))))

    handlers))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;
;; addparent, etc. and stuff for it
;;;;;;;;;;;;

(defun merge-flags-from-parents (obj)
  (let ((flags (slot-value obj 'SK8::SK8_flags)))
    (do-SK8-parents (parent obj)
      (setq flags (merge-SK8-flags flags (slot-value parent 'SK8::SK8_flags))))
    (setf (slot-value obj 'SK8::SK8_flags) flags)))

;;; change-supers -- changes the superclasses of the given class
;;;   obj - the object whose class's superclasses are to be changed
;;;   oldSuperclass - existing superclass to remove (might not be any)
;;;   newSuperclass - superclass to add (might not be any)
;;;
(defun change-supers (obj oldSuperclass newSuperclass)
  (let ((targetClass (class-of obj)))
    (require-type targetClass 'ccl::std-class)
    (unless (eq targetClass ccl::*built-in-class-class*) ; built-in classes should not be messed with like this
      
      (without-interrupts
       (obsolete-class-and-subclasses targetClass)
       (dolist (sup (ccl::%class-local-supers targetClass))
         (when (typep sup 'class)          ; might be a symbol from earlier forward ref
           (let ((subs (ccl::%class-subclasses sup)))
             (if (listp subs)
               (setf (ccl::%class-subclasses sup) (nremove targetClass subs))
               (setf (ccl::population-data subs) (nremove targetClass (ccl::population-data subs))))))) ; initialize-class rectifies this
       (setf (ccl::%class-local-supers targetClass)
             (if (and oldSuperclass newSuperclass)
               (setf (ccl::%class-local-supers targetClass)
                     (nsubstitute newSuperclass oldSuperclass (ccl::%class-local-supers targetClass)))
               (if oldSuperclass
                 (setf (ccl::%class-local-supers targetClass)
                       (delete oldSuperclass (ccl::%class-local-supers targetClass)))
                 (push newSuperclass (ccl::%class-local-supers targetClass)))))
       (unless (ccl::%class-local-supers targetClass)
         (push (class-of sk8:Object) (ccl::%class-local-supers targetClass)))
       
       (CCL::initialize-class targetClass t)
       
       ;; Update obj's flags
       (merge-flags-from-parents obj)
       
       ;; Update obj's set of ports if necessary
       (when newSuperclass (SK8Dev::update-ports-from-parents obj))
       
       ;; Refresh obj's internal creationRelations info if necessary
       (unless (maybe-update-creationRelations-due-to-removed-slots obj targetClass oldSuperclass)
         (when (or (and oldSuperclass (%object-has-creationRelations? (class-owner oldSuperclass)))
                   (and newSuperclass (%object-has-creationRelations? (class-owner newSuperclass))))
           (refresh-internal-creationRelations-cache obj)))))))


;;; This is like change-supers, exept that the caller specifies exactly the sequence of
;;; parents that will result. This allows setting the ordering of parents.
;;; NOTE!  Since shuffle-supers is called by add-class-to-object, the superclasses may contain a "non-SK8" class (so don't assume otherwise)!
;;;
(defun shuffle-supers (obj newSuperclasses)
  (let ((targetClass (class-of obj)))
    (require-type targetClass 'ccl::std-class)
    (unless (eq targetClass ccl::*built-in-class-class*) ; built-in classes should not be messed with like this
      (let ((oldSuperclasses (copy-list (ccl::%class-local-supers targetClass))))
        (without-interrupts
         (obsolete-class-and-subclasses targetClass)
         ;; remove class from the lists of subclasses held by its supers
         (dolist (sup (ccl::%class-local-supers targetClass))
           (if (typep sup 'class)          ; might be a symbol from earlier forward ref
             (let ((subs (ccl::%class-subclasses sup)))
               (if (listp subs)
                 (setf (ccl::%class-subclasses sup) (nremove targetClass subs))
                 (setf (ccl::population-data subs) (nremove targetClass (ccl::population-data subs))))))) ; initialize-class rectifies this
         (setf (ccl::%class-local-supers targetClass) (copy-list newSuperclasses))
         (CCL::initialize-class targetClass t)
         
         ;; Update obj's flags
         (merge-flags-from-parents obj)
         
         ;; Update obj's set of ports if necessary
         (SK8Dev::update-ports-from-parents obj)
         
         ;; Refresh obj's internal creationRelations info if necessary
         (let ((removedSuperclasses (delete newSuperclasses oldSuperclasses :test #'(lambda (news old) (memq old news))))
               o)
           (unless (maybe-update-creationRelations-due-to-removed-slots obj targetClass removedSuperclasses)
             (when (or (dolist (c removedSuperclasses) (when (and (CCL::standard-instance-p (setq o (class-owner c)))
                                                                  (%object-has-creationRelations? o)) (return t)))
                       (dolist (c newSuperclasses)     (when (and (CCL::standard-instance-p (setq o (class-owner c)))
                                                                  (%object-has-creationRelations? o)) (return t))))
               (refresh-internal-creationRelations-cache obj)))))))))

;;; addParent -- adds a parents to object
;;;   object - the object to which to add new parent
;;;   parent - new parent for object
;;;
(defmethod sk8:addParent ((me sk8:Object) parent &key)
  (declare (ftype t sk8:addedMeAsParent))
  (ensure-real-sk8-object me '("Can’t change the parents of "))
  (ensure-real-sk8-object parent '("Can’t add " " as a parent"))
  (let ((oldParents (sk8:parents me))
        newParentClass)
    (when (memq parent oldParents) (error "~a is already a parent of ~a" parent me))
    (when (sk8:inheritsfrom me parent) (error "~a is already an ancestor of ~a" parent me))
    (without-interrupts
     (setq newParentClass (maybe-make-own-object-class parent))
     (maybe-make-own-object-class me)
     (change-supers me nil newParentClass)
     (when (logbitp %flags-named-object? (SK8::sk8_flags me))
       (push me (slot-value parent 'sk8:knownchildren)))
     (when (find-handler 'sk8:addedMeAsParent parent)
       (sk8:addedMeAsParent parent me oldParents)
       (map-all-descendants me #'(lambda (obj) (sk8:addedMeAsParent parent obj oldParents))))
     (sk8:parents me))))


;;; remove-parent -- removes parent from inheritance hierarchy of object
;;;  object - the object
;;;  parentToRemove - the parent to remove from inheritance hierarchy of object
;;;  Use maybe-siblings-too to remove parent of unnamed child that may share class with siblings
;;; This will call removingmeasparent for the object and its descendants if the parent being removed is
;;; recognized as a parent of the object. If it isn't, this will still make sure that the class is removed
;;; from the superclasses of the object and that the object is not on knownchildren of the supposed parent
;;;
(defun remove-parent (obj parentToRemove maybe-siblings-too)
  (when (typep obj 'SK8:Object)
    (let ((objectClass (class-of obj))
          (parentClass (class-of parentToRemove)))
      (when (memq parentClass (class-direct-superclasses objectClass))
        (unless maybe-siblings-too
          (maybe-make-own-object-class obj))
        (let ((parents (SK8:parents obj)))
          (when (memq parentToRemove parents)
            (setf parents (delete parentToRemove parents))
            (when (find-handler 'SK8:removingMeAsParent parentToRemove)
              (SK8:removingMeAsParent parentToRemove obj parents)
              (map-all-descendants obj #'(lambda (o) (SK8:removingMeAsParent parentToRemove o parents))))))
        (change-supers obj parentClass nil)
        (setf (slot-value parentToRemove 'SK8:knownchildren)
              (delete obj (SK8:knownchildren parentToRemove))))
      t)))


;;; removeParent -- removes parent from a macframes object
;;;   object - the object from which to remove parents
;;;   parent - the parent to remove from the object
;;;
(defmethod sk8:removeParent ((me sk8:Object) parent &key)
  (ensure-real-sk8-object me '("Can’t change the parents of "))
  (remove-parent me parent nil))


;;; changeParents -- replaces entire set of parents of an object with a new set of parents
;;; This function allows the caller to specify resulting order of the parents, and makes the
;;; change all at once so that the settings of any properties still existing after the change
;;; are not lost.
;;;   object -- the object whose parents are being changed
;;;   newParents -- the new set of parents for the object
;;;
(defmethod sk8:changeParents ((me sk8:Object) newParents &key)
  (declare (ftype t sk8:addedMeAsParent sk8:removingMeAsParent))
  (ensure-real-sk8-object me '("Can’t change the parents of "))
  (unless newParents (setf newParents (list sk8:Object)))  ;; everything has to have at least Object as a parent
  (let* ((oldParents (sk8:parents me))
         (removedParents (set-difference oldParents newParents :test #'eq))
         (addedParents (set-difference newParents oldParents :test #'eq))
         (named? (logbitp %flags-named-object? (SK8::sk8_flags me))))
    (dolist (p addedParents)
      (ensure-real-sk8-object p '("Can’t add " " as a parent"))
      (maybe-make-own-object-class p))
    (maybe-make-own-object-class me)
    (dolist (p removedParents)
      (when (find-handler 'sk8:removingMeAsParent p)
        (sk8:removingMeAsParent p me newParents)
        (map-all-descendants me #'(lambda (obj) (sk8:removingMeAsParent p obj newParents))))
      (setf (slot-value p 'sk8:knownchildren) (delete me (slot-value p 'sk8:knownchildren))))
    (shuffle-supers me (mapcar #'class-of newParents))
    (dolist (p addedParents)
      (when named? (push me (slot-value p 'sk8:knownchildren))))
    (dolist (p addedParents)
      (when (find-handler 'sk8:addedMeAsParent p)
        (sk8:addedMeAsParent p me oldParents)
        (map-all-descendants me #'(lambda (obj) (sk8:addedMeAsParent p obj oldParents))))))
  (sk8:parents me))

(defmethod (setf sk8:parents) (parents (me sk8:Object) &key)
  (sk8:changeParents me parents))

;;; This should be enhanced to find the appropriate oldParent
;;; if oldParent is t.
;;;
(defmethod SK8:replaceParent ((me sk8:Object) oldParent newParent &key)
  (cond
   ((null newParent) (sk8:removeParent me oldParent))
   ((null oldParent) (sk8:addParent me newParent))
   (t
    (let* ((pars (sk8:parents me))
           (oldpos (position oldParent pars)))
      (when (memq newParent pars) (error "~a is already a parent of ~a" newParent me))
      (when (sk8:inheritsfrom me newParent) (error "~a is already an ancestor of ~a" newParent me))
      (if oldPos
        (progn (setf (nth oldPos pars) newParent)
               (sk8:changeParents me pars))
        (sk8:addParent me newParent))))))

(defun SK8:SpliceNewParent (Kids &key (ObjectName nil))
  (unless (listp kids)
    (setf kids (list kids)))
  (unless objectname
    (SK8:sk8-error SK8:GeneralProgrammaticError
               :strings '("An ObjectName for the new parent must be specified.")))
  (let* ((proj (sk8:project (car kids)))
         (pars (sk8:parents (car kids)))
         (orderedParents (reverse pars))
         theguy)
    (unless (every #'(lambda (x) (eq x proj)) (mapcar #'sk8:project kids)) 
      (error "All children recieving the new parent need to be in the same project"))
    (unless (every #'(lambda (x) (equal x pars)) (mapcar #'sk8:parents kids)) 
      (error "All children recieving the new parent need to have the same set of parents."))
    (setf theguy (new (car orderedParents) :project proj :objectname objectname))
    (dolist (i (cdr orderedParents)) (sk8:addparent theguy i))
    (dolist (i kids) (sk8:addparent i theguy))
    (dolist (i orderedParents)
      (dolist (k kids) 
        (sk8:removeparent k i)))
    theguy))


#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/10/96	Hernan  	Fixing find-handler to return the local handler when both
						local-p and inherited-p are t. Also changed the default 
						values of these keyword args to be t and nil.
	3  	 5/10/96	sidney  	store project in project function
	4  	 5/13/96	sidney  	
	5  	 5/23/96	sidney  	
	6  	 7/ 7/96	sidney  	changes for native PPC build
	7  	 7/26/96	Hernan  	Need  to define dontSave on t, otherwise it is not defined
						for built in types.
	8  	 8/ 5/96	Brian   	Adding stub for a "type" keyword argument to addproperty.
	9  	 8/12/96	Hernan  	Added an option to find-parent-slot-value to actually find
						the parent that defines the slot.
	10 	 9/11/96	sidney  	fix inheritance checking for built-in class objects, such as Handler
	11 	10/10/96	sidney  	package was wrong for localvirtualproperties symbols of Project
	12 	10/18/96	sidney  	use correct package when referring to SK8 errors
	13 	11/25/96	Brian   	Fixing getting the localhandlers of object now
						that t-class seems to have every handler
						associated with it.
	14 	12/12/96	Hernan  	AddProperty checks before adding a property that is already a plain function (not a generic function).
	15 	 1/30/97	sidney  	typesatisfied (and so inheritsfrom) was saying that unspecialized children of same parent inherit from each other
	16 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
