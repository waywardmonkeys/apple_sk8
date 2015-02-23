;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;; initialize -- called by new/copy after original has been cloned
;;;  original - object originally cloned
;;;  isNew - t = me is a child, nil = me is a sibling of original
;;;
;;; This is defined on T instead of Object because sometimes it gets called on arbitrary stuff and it's easier to just ignore the call
(defmethod SK8:Initialize ((me t) original isNew initArgs &key)
  (declare (ignore me original isNew initargs)))

(defun simple-new (originalObject targetProject)
  (unless targetProject (setq targetProject (project originalObject)))
  (let ((objectFlags (SK8::SK8_flags originalObject)))
    (if (%flags-has-ports-or-creationRelations? objectFlags)
      ;; The object has creationRelations or ports; make the objects in the creationGroup and enforce the relations...
      (with-creationRelationsInfo (crinfo)
        (let ((newObject (correspondenceInfo-newObject
                          (make-object-and-creationGroup originalObject crinfo targetProject T *start-depth*))))
          (finish-relations-and-initialize-object crinfo originalObject newObject NIL targetproject T)))
      ;; The object is nice and simple; just create and initialize it...
      (let ((newObject (new-object-and-low-level-init originalObject targetProject)))
        (Initialize newObject originalObject t nil)
        newObject))))

(defun simple-copy (initArgs originalObject targetProject name)
  (unless targetProject (setq targetProject (project originalObject)))
  (let ((objectFlags (SK8::SK8_flags originalObject)))
    (if (%flags-has-ports-or-creationRelations? objectFlags)
      ;; The object has creationRelations or ports; make the objects in the creationGroup and enforce the relations...
      (with-creationRelationsInfo (crinfo)
        (let ((newObject (correspondenceInfo-newObject
                          (make-object-and-creationGroup originalObject crinfo targetProject NIL *start-depth*))))
          (finish-relations-and-initialize-object crinfo originalObject newObject initArgs targetproject NIL)))
      ;; The object is nice and simple; just create and initialize it...
      (let ((newObject (copy-object-and-low-level-init originalObject targetProject name)))
        (Initialize newObject originalObject nil initArgs)
        newObject))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;This function searches the raw initargs for the project keyword in order to aid
;;;load-initializerarguments in determing which package to intern all the other
;;;keywords.
(defun find-project-from-raw-initargs (initializerArguments)
  (cond
   ((simple-vector-p initializerArguments)
    (let ((i (length initializerArguments)))
      (declare (fixnum i))
      (unless (logbitp 0 i) 
        (loop
          (when (eql i 0) (return nil))
          (decf i 2)
          (when (eq (svref initializerArguments i) :project)
            (return (svref initializerArguments (1+ i)))
            )))))
   (t nil))
  )

(defun load-initializerArguments (originalObject initializerArguments conses)
  (unless (simple-vector-p initializerArguments) (invalid-initializerArguments-error initializerArguments))
  (let ((pkg (package  (or (find-project-from-raw-initargs initializerArguments) (project originalObject))  ))
        (i 0)
        cons)
    (declare (fixnum i))
    (loop
      (unless conses (return))
      (setf (svref initializerArguments i) (intern (symbol-name (svref initializerArguments i)) pkg)
            i (1+ i)
            cons conses
            conses (cdr conses)
            (car (the cons cons)) (svref initializerArguments i)
            (cdr (the cons cons)) nil
            (svref initializerArguments i) cons
            i (1+ i)))))

(defmacro with-initializerArguments ((originalObject initializerArgumentsVar) &body body)
  (require-type initializerArgumentsVar 'symbol)
  (let ((conses (gensym)))
    `(let ((,conses (make-list (the fixnum (ash (the fixnum (length ,initializerArgumentsVar)) -1)))))
       (declare (dynamic-extent ,conses))
       (load-initializerArguments ,originalObject ,initializerArgumentsVar ,conses)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  High-level API for accessing & processing initializerArguments
;;;

(defun initializerArgument (initializerArguments argumentSymbol &key default (use t))
  (cond
   ((null initializerArguments) default)
   ((simple-vector-p initializerArguments)
    (let ((i (length initializerArguments)))
      (declare (fixnum i))
      (when (logbitp 0 i) (invalid-initializerArguments-error initializerArguments))
      (loop
        (when (eql i 0) (return default))
        (decf i 2)
        (when (eq (svref initializerArguments i) argumentSymbol)
          (let ((valueHolder (svref initializerArguments (1+ i))))
            (when use (setf (cdr valueHolder) t))
            (return (car (the cons valueHolder))))))))
   ;; This case is provided so that a function can be handed a symbol-keyed ObjectTable (i.e. a "record")
   ;; in place of real initializer arguments
   ;; I think that is a bogus idea. This is at much too low a level to have to know about ObjectTable objects!!!
   #+ignore
   ((is-a initializerArguments ObjectTable)
    (let ((nthState (nthState initializerArguments argumentSymbol nil)))
      (if nthState
        (currentItem initializerArguments nthState)
        default)))
   (t (invalid-initializerArguments-error initializerArguments))))

(defun processUnusedInitializerArguments (obj initializerArguments)
  (cond
   ((null initializerArguments) nil)
   ((not (simple-vector-p initializerArguments)) (invalid-initializerArguments-error initializerArguments))
   (t
    (let ((len (length initializerArguments))
          (i 0)
          valueHolder)
      (declare (fixnum len i))
      (when (logbitp 0 i) (invalid-initializerArguments-error initializerArguments))
      (loop
        (when (eql i len) (return))
        (setq valueHolder (svref initializerArguments (incf i)))
        (when (null (cdr valueHolder))
          (setf (cdr valueHolder) t) ;; this lets this function be called twice without using the same args twice
          (funcall (CCL::setf-function-name (svref initializerArguments (the fixnum (1- i))))
                   (car (the cons valueHolder))
                   obj))
        (incf i))))))

(defun sk8::raiseUnsettablePropertyError (property theObject)
  (SK8:sk8-error SK8:GeneralProgrammaticError
             :strings '("You can't set the " " property of " ".")
             :objects (list property theObject)))

(defun SK8::raiseInsufficientInitializersError (original
                                                       requiredInitializers
                                                       &key
                                                       (argumentsToCheck nil argumentsToCheckSet))
  (when (or (not argumentsToCheckSet)
            (dolist (property requiredInitializers nil)
              (when (eq (initializerArgument argumentsToCheck property :use nil :default *undefined*) *undefined*)
                (return t))))
    (SK8:sk8-error SK8:GeneralProgrammaticError
               :strings '("Insufficient initializers for a new child of "
                          ".  Required initializers are: " ".")
               :objects (list original requiredInitializers))))

(defun ensure-usable-objectName (objectName package &key existingObject)
  (setq objectName (intern-legal-varName objectName package T T))
  (when (boundp objectName)
    (let ((value (symbol-value objectName)))
      (when (and (neq value *Undefined*)
                 (or (null existingObject) (neq value existingObject)))
        (illegal-objectName-error objectName))))
  objectName)


(defvar *mf-internal-format-slot-names* '(sk8::sk8_id
                                                sk8::sk8_flags
                                                sk8:project))

(defun ensure-legal-property-name (propertyName)
  (when
    (or (eq propertyName nil) (eq propertyName t)
        (memq propertyName *mf-internal-format-slot-names*)  ;; was used to check for sk8_id, sk8_flags and project
        )
    (illegal-propertyName-error propertyName :reserved t))
  propertyName)

(defun parents-classes (allParents)
  (if (listp allParents)
    
    (let ((parentClasses nil)
          class)
      (dolist (parent allParents)
        (setq class (class-of parent))
        (when (or (eq class CCL::*built-in-class-class*)
                  (not (typep class 'ccl::std-class)))
          (invalid-parent-error parent))
        (push class parentClasses))
      (nreverse parentClasses))
    (let ((class (class-of allParents)))
      (when (or (eq class CCL::*built-in-class-class*)
                (not (typep class 'ccl::std-class)))
        (invalid-parent-error allParents))
      (list class))))

(defun check-and-normalize-prop-specs (properties parentClasses)
  (let ((existingPropertiesSpecs nil)
        (newPropertiesSpecs nil)
        propName thing optionFlag optionFlags valueCons)
    (dolist (propSpec properties)
      (cond
       
       ((listp propSpec)
        (setq propName (ensure-legal-property-name (first (the list propSpec))))
        (when (or (assq propName newPropertiesSpecs)
                  (assq propName existingPropertiesSpecs))
          (duplicate-propertyName-error propName))
        (setq valueCons nil
              optionFlags 0)
        (loop
          (unless (setq propSpec (cdr (the list propSpec))) (return))
          (setq thing (car propSpec))
          (cond
           ((or (eq thing *property-spec-value-marker*)
                (eq thing *OLD-property-spec-value-marker*))
            (setq propSpec (cdr (the list propSpec)))
            (setq valueCons (list (car propSpec))))
           ((setq optionFlag (cdr (or (assq thing *property-spec-options*)
                                      (assq thing *OLD-property-spec-options*))))
            (setq optionFlags (logior optionFlags optionFlag)))
           (t
            (invalid-property-option-error thing propName))))
        (setq propSpec (list* propName optionFlags valueCons)))
       
       (t
        (setq propSpec (list (ensure-legal-property-name propSpec) 0))))
      
      (if (dolist (c parentClasses)
            (when (assq propName (CCL::%class-local-instance-slotds c)) (return t)))
        (push propSpec existingPropertiesSpecs)
        (push propSpec newPropertiesSpecs)))
    
    (setq newPropertiesSpecs (nreverse newPropertiesSpecs))
    (values (nconc existingPropertiesSpecs newPropertiesSpecs) newPropertiesSpecs)))

;;; maybe-make-own-class -- makes own class if the class isn't already owned
;;;
(defun maybe-make-own-object-class (obj)
  (let ((class (class-of obj)))
    (if (eq (class-owner class) obj)
      class
      (make-own-object-class obj))))

;;; ensure-parents-have-own-class -- makes own class for parents which have shared classes
;;;   parents - a parent or a list of parents
;;;   multiple - if non-nil, then parents is a list of parents
;;;
(defun ensure-parents-have-own-class (parents multiple)
  (if multiple
    (dolist (p parents)
      (maybe-make-own-object-class p))
    (maybe-make-own-object-class parents)))


(defun slot-value-no-error (obj slot-name)
  (if (ccl::standard-object-p obj)
    (ccl::%slot-value obj slot-name)
    (ccl::%slot-missing-marker)))

;;; Initialize all the properties (either from the given properties specs or from the parents)
;;;
(defun initialize-all-properties (obj objClass parents givenPropertiesSpecs)
  (let ((multipleParents? (listp parents))
        propName propSpec propOptions value)
    (dolist (slotd (CCL::class-instance-slots objClass))
      (setq propName (first slotd))
      ;; Don't bash the low-level properties
      (unless (or
               (eq propName 'sk8:knownChildren)
               (memq propName *mf-internal-format-slot-names*)
               )
        (setq propSpec (cdr (assq propName givenPropertiesSpecs))
              propOptions (if propSpec (pop propSpec) 0))
        (cond
         (propSpec
          (setq value (car propSpec)))
         (multipleParents?
          (dolist (parent parents)
            (unless (eq (setq value (slot-value-no-error parent propName)) (ccl::%slot-missing-marker))
              (return))))
         (t
          (setq value (slot-value-no-error parents propName))))
        (when (eq value (ccl::%slot-missing-marker)) (setq value nil))
        (if (logbitp 0 propOptions)
          (setf (slot-value obj propName) (or (when (dynamic-slotvalue-inheriter-p value) value)
                                              (make-dynamic-slotvalue-inheriter obj value)))
          (setf (slot-value obj propName) value))))
    (let ((flags (slot-value obj 'SK8::sk8_flags)))
      (if multipleParents?
        (dolist (p parents) (setq flags 
                                  (logior flags (logand (slot-value p 'SK8::sk8_flags) %inheritable-SK8-flags-mask))))
        (setq flags (logior flags (logand (slot-value parents 'SK8::sk8_flags) %inheritable-SK8-flags-mask))))
      (setf (slot-value obj 'SK8::sk8_flags) flags))))

;;; Initialize just those properties for which initial values are given (or property options are changed) in the properties specs
;;;
(defun initialize-given-properties (obj givenPropertiesSpecs)
  (let (propName propOptions)
    (dolist (propSpec givenPropertiesSpecs)
      (setf propName (pop propSpec)
            propOptions (pop propSpec))
      (cond
       ((logbitp 0 propOptions)
        (cond (propSpec
               (setf (slot-value obj propName) (make-dynamic-slotvalue-inheriter obj (car propSpec))))
              ((not (dynamic-slotvalue-inheriter-p (setq propSpec (slot-value obj propName))))
               (setf (slot-value obj propName) (make-dynamic-slotvalue-inheriter obj propSpec)))))
       (propSpec
        (funcall (CCL::setf-function-name propName) (car propSpec) obj))))))

(defun maybe-object-flags (obj)
  (let ((flags (slot-value-no-error obj 'SK8::SK8_flags)))
    (unless (eq flags (ccl::%slot-missing-marker))
      flags)))

;;; Similar to simple-new & make-object-and-creationGroup combined, but more general.  This function is for use only by
;;; general-new & general-copy, when just a single parent is given.
;;;

(defun general-new-make-object (allParents mainParent newClass givenPropertiesSpecs newProps? initArgs oid targetProject skipInitialize?)
  (if (eq allParents mainParent)
    ;; Just one parent...
    (let* ((objectFlags (maybe-object-flags mainParent))
           (newObject (make-object-and-low-level-init* (unless newProps? mainParent) newClass oid targetProject)))
      (if newProps?
        ;; Must initialize all the properties (either from the given properties specs or from the parents)
        (initialize-all-properties newObject newClass allParents givenPropertiesSpecs)
        ;; Must initialize just those properties for which initial values are given in the properties specs
        (when givenPropertiesSpecs (initialize-given-properties newObject givenPropertiesSpecs)))
      (unless skipInitialize?
        (if (%flags-has-ports-or-creationRelations? objectFlags)
          ;; The object has creationRelations or ports; make the objects in the creationGroup and enforce the relations...
          (with-creationRelationsInfo (crinfo)
            (let ((correspondenceInfo (make-correspondenceInfo mainParent newObject *start-depth*)))
              (insert-into-crinfo-objectsList correspondenceInfo crinfo)
              (make-creationGroup mainParent objectFlags correspondenceInfo crinfo targetProject T *start-depth*)
              (finish-relations-and-initialize-object crinfo mainParent newObject initArgs targetProject T)))
          ;; The object has no creationRelations or ports; just initialize it...
          (initialize newObject mainParent t initArgs))
        )
      newObject)
    ;; Multiple parents...
    (let* ((newObject (make-object-and-low-level-init* nil newClass oid targetProject))
           (creationGroup? nil)
           objectFlags)
      ;; Must unconditionaly initialize all the properties (either from the given properties specs or from the parents)
      (initialize-all-properties newObject newClass allParents givenPropertiesSpecs)
      ;; Determine whether the new object has a creation group
      (dolist (parent allParents)
        (unless (setq objectFlags (maybe-object-flags parent))
          (invalid-parent-error parent)) ;*** REDUNDANT?  LOOK AT CALLER OF THIS FUNCTION.
        (when (%flags-has-ports-or-creationRelations? objectFlags)
          (setq creationGroup? t)))
      (unless skipInitialize?
        (if creationGroup?
          ;; The object has creationRelations or ports; make the objects in the creationGroup and enforce the relations...
          (with-creationRelationsInfo (crinfo)
            (let (correspondenceInfo)
              (dolist (parent allParents)
                (setq objectFlags (maybe-object-flags parent))
                (setq correspondenceInfo (make-correspondenceInfo parent newObject *start-depth*))
                (insert-into-crinfo-objectsList correspondenceInfo crinfo)
                (make-creationGroup parent objectFlags correspondenceInfo crinfo targetProject T *start-depth*))
              (finish-relations-and-initialize-object crinfo mainParent newObject initArgs targetProject T)))
          ;; The object has no creationRelations or ports; just initialize it...
          (initialize newObject mainParent T initArgs))
        )
      newObject)))

(defun name-new-object (newObject private? objectName targetProject package)
  (unrecord-object newObject)
  (record-object newObject objectName targetProject)
  ;; to be completed
  (publish-project-symbol objectName targetProject)
  
  (setf (SK8::sk8_id newObject) objectName)
  (let ((flags (logior (ash 1 %flags-named-object?) (SK8::sk8_flags newObject))))
    (setf (SK8::sk8_flags newObject)
          (if private?
            (logior (ash 1 %flags-private-object?) flags)
            (logand (lognot (ash 1 %flags-private-object?)) flags))))
  (when (and (boundp '*build-in-progress-p*) *build-in-progress-p*
             (constantp objectname) (eq (symbol-value objectname) *undefined*))
    (let ((ccl::*warn-if-redefine-kernel* nil))
      (makunbound objectname)))
  (ccl::%defparameter objectName newObject) ;; ??? (CCL::%defconstant objectName newObject)
  
  (do-SK8-parents (parent newObject)
                  (pushnew newObject (slot-value parent 'knownChildren) :test #'eq)))

;;; Use object-class-p to check if an object is actually an object instance.
;;; Hard to believe, but this crazy thing is several times faster than typep.
(defmethod object-class-p ((obj SK8::Object))
  t)

(defmethod object-class-p ((obj t))
  nil)

(defmethod ensure-real-SK8-object ((me Object) strings)
  (declare (ignore strings))
  me)

(defmethod ensure-real-SK8-object ((me T) strings)
  (SK8:SK8-error SK8:GeneralError :strings strings :objects (list me)))

(defun pretty-symbol-name (sym)
  (cond
   ((eq sym nil) "False")
   ((eq sym t) "True")
   ;; Special-case for setter symbols
   ((eq (symbol-package sym) CCL::*SETF-PACKAGE*)
    (let* ((set-sym-name (symbol-name sym))
           (colpos (search "::" set-sym-name))
           (pkgname (when colpos (subseq set-sym-name 0 colpos)))
           (symname (if colpos (subseq set-sym-name (+ 2 colpos)) set-sym-name))
           (symsym (when pkgname (find-symbol symname pkgname))))
      (concatenate 'string "set " (or (when symsym (get symsym :original-case))
                                      (string-downcase symname)))))
   (t
    (or (get sym :original-case)
        (string-downcase (symbol-name sym))))))


(defmethod objectName ((me Object) &key)
  (when (slot-boundp me 'SK8::sk8_flags)  ;; Bullet-proofing for objects in the middle of creation by the store
    (let ((flags (slot-value me 'SK8::SK8_flags)))
      (when (logbitp %flags-named-object? flags)
        (pretty-symbol-name (slot-value me 'SK8::SK8_id))))))

(defmethod objectName ((me t) &key)
  nil)

;; these next two functions are used too many places to get rid of, just define them simply here
(defun sk8:objectString (thing &key ((:project proj)))
  (if (sk8:objectname thing)
    (sk8:objectname thing)
    (format nil "Unnamed thing in ~a" (or (sk8:objectname proj) "some project"))))

(defun sk8:simpleobjectString (thing &key ((:project proj)))
  (if (sk8:objectname thing)
    (sk8:objectname thing)
    (format nil "Unnamed thing in ~a" (or (sk8:objectname proj) "some project"))))


;;; (setf objectName) -- changes the object objectName to another one or makes the object unnamed
;;;  objectName - a string or false (if false, then the object becomes an unnamed object)
;;; force - T to suppress a confirmation dialog when changing the name of an object in the SK8 project
;;;       :NEWPUBLIC and :NEWPRIVATE is used only internally by the object creation system when a new object is created
;;;
(defmethod (setf objectName) (objectName (me Object) &key force)
  (let* ((proj (project me))
         (package (package proj)))
    (case force
      (:NEWPUBLIC (return-from objectname (name-new-object me nil objectName proj package)))
      (:NEWPRIVATE (return-from objectname (name-new-object me t objectName proj package))))
    (ensure-real-sk8-object me '("Can’t set the objectName of "))
    (when objectName
      (unless (stringp objectName)
        (SK8:Sk8-error SK8:GeneralError :strings '("objectName (" ") for object " " should have been a string!") :objects (list objectName me)))
      )
    (let ((named? (logbitp %flags-named-object? (SK8::sk8_flags me)))
          (current (SK8::SK8_id me)))
      (when objectName
        ;; The existingObject arg allows an object to change its name to itself
        (setq objectName (ensure-usable-objectName objectName package :existingObject me)))
      
      (without-interrupts
       (when (and named? (boundp current))
         (makunbound current))
       (setf (SK8::sk8_flags me)
             (if objectName
               (logior (ash 1 %flags-named-object?) (SK8::sk8_flags me))    ;; was (named-object! me objectName)
               (logand (lognot (ash 1 %flags-named-object?)) (SK8::sk8_flags me))))
       (unrecord-object me)
       (if objectname
         (progn
           (publish-project-symbol objectName proj)
           (when (and (boundp '*build-in-progress-p*) *build-in-progress-p*  ;; hack to help our build process
                      (constantp objectName) (eq (symbol-value objectName) *undefined*))
             (let ((ccl::*warn-if-redefine-kernel* nil))
               (makunbound objectName)))
           (ccl::%defparameter objectName me) ;; ??? (ccl::%defconstant objectName me)
           (unless named?    ;; add to knownchildren of parents if it wasn't named before
             (dolist (parent (parents me))
               (pushnew me (slot-value parent 'knownchildren)))))
         (progn
           (setf objectname (get-new-object-oid))
           (when named?    ;; remove from knownchildren of parents if it was named before
             (dolist (parent (parents me))
               (setf (slot-value parent 'knownchildren)
                     (delete me (slot-value parent 'knownchildren)))))))
       (setf (SK8::SK8_id me) objectName)
       (let ((class (class-of me)))
         (when (eq (class-owner class) me)     ;; have own class, so hack the class name to correspond to objectname
           (let ((oldclassname (class-name class)))
             (setf (class-name class) (make-symbol (makeup-class-name me)))
             (when (find-class oldclassname nil)
               (setf (find-class oldclassname) nil))
             (setf (find-class (class-name class)) class))))
       (record-object me objectName proj)))
    (when (and objectName (symbolp objectname))
      (pretty-symbol-name objectName))))

;;; print-object -- how sk8 objects are printed back
;;;   This goes to some pains to check that the object is consistent so that
;;;   we can print during debugging.
;;;
(defmethod print-object ((me Object) strm)
  (let ((id (when (slot-boundp me 'SK8::sk8_id) (slot-value me 'SK8::sk8_id))))  ; safe call in case it is temporarily unbound (during creation)
    (if id
      (if (and (symbolp id) (logbitp %flags-named-object? (slot-value me 'SK8::SK8_flags)))
        (prin1 id strm) ;; ??? (write-string (pretty-symbol-name id) strm)
        (let ((namedAncestor nil)
              (parent? t))
          (ignore-errors
           (setq namedAncestor (baseParent me))
           (loop
             (when (objectName namedAncestor) (return))
             (setq namedAncestor (baseParent namedAncestor)
                   parent? nil)))
          (if namedAncestor
            (format strm "[a ~:[descendant~;child~] of ~a]" parent? (objectName namedAncestor))
            (call-next-method))))
      (call-next-method))))

;;; print-object -- prints sk8-generated classes with SK8 object information
;;;
(defmethod print-object ((me standard-class) s)
  (let* ((owner (class-owner me))
         (name (when (and (CCL::standard-object-p owner) (slot-exists-p owner 'SK8::sk8_id) (slot-boundp owner 'SK8::sk8_id))
                 (slot-value owner 'SK8::sk8_id))))
    (if name
      (format s "<SK8-CLASS for ~s>" name)
      (call-next-method))))

(defun general-new (originalObject &rest initArgsL &aux initArgs)
  (declare (dynamic-extent initArgsL))
  (setf initArgs  (apply #'vector initARgsL))
  (with-initializerArguments (originalObject initArgs)
    (let* ((targetProject (or (initializerArgument initArgs 'sk8:project) (no-targetProject-error)))
           (otherParents (initializerArgument initArgs 'sk8:otherParents))
           (properties (initializerArgument initArgs 'sk8:properties))
           (objectName (initializerArgument initArgs 'sk8:objectName))
           (private? (initializerArgument initArgs 'sk8:privateObject))
           (skipInitialize? (initializerArgument initArgs 'sk8:skipInitialization))
           (package (package targetProject))
           (allParents (if otherParents
                         (append (if (listp otherParents) otherParents (list otherParents))
                                 (list originalObject))
                         originalObject))
           (parentClasses nil)
           (newClass nil)
           (oid (get-new-object-oid))
           givenPropertiesSpecs
           newPropertiesSpecs
           newObject
           needsOwnClass?)
      
      ;; Process objectName if given
      (when objectName
        (setq objectName (ensure-usable-objectName objectName package)))
      
      ;; Make sure the parents can have children
      (ensure-parents-have-own-class allParents otherParents)
      ;; Normalize and categorize properties specs if given
      (when properties
        (setq parentClasses (parents-classes allParents))
        (multiple-value-setq (givenPropertiesSpecs newPropertiesSpecs)
          (check-and-normalize-prop-specs properties parentClasses)))
      
      ;; If the object has new properties or multiple parents, it needs its own class
      (when (or newPropertiesSpecs otherParents) (setf needsOwnClass? t))
      (cond
       (needsOwnClass?
        (without-interrupts ; so can't abort before the new class is marked as owned!
         (setq newClass (parse-object-definition
                         (or objectName
                             (makeup-symbol-from-oid (car (or parentClasses (parents-classes (car allParents)))) oid))
                         NIL targetProject allParents properties private?))
         (set-class-owner newClass :OWNER-NOT-YET-CREATED)))
       ;; Otherwise, only make a new class if there isn't already an unowned one available
       ((null (setq newClass (simple-find-unowned-class (class-of originalObject))))
        (setq newClass (parse-object-definition nil NIL targetProject allParents properties private?))))
      
      ;; Create the new object
      (setq newObject (general-new-make-object allParents originalObject newClass givenPropertiesSpecs
                                               newPropertiesSpecs initArgs oid targetProject skipInitialize?))
      (set-class-owner newClass newObject needsOwnClass?)
      ;; If the new object is specialized, mark it as such and make it own its class
      (when (and needsOwnClass? newPropertiesSpecs)
        (setf (SK8::sk8_flags newObject)
              (logior (ash 1 %flags-specialized-object?) (SK8::sk8_flags newObject))))
      ;; Process any remaining unused init-args
      (processUnusedInitializerArguments newObject initArgs)
      ;; The creation succeeded, so name the object if a name was given
      (when objectName
        (setf (objectname newObject :force (if private? :NEWPRIVATE :NEWPUBLIC)) objectName))
      newObject)))

(defun general-copy (originalObject &rest initArgsL &aux initArgs)
  (declare (dynamic-extent initArgs))
  (setf initArgs  (apply #'vector initARgsL))
  (with-initializerArguments (originalObject initArgs)
    (when (initializerArgument initArgs 'sk8:otherParents)
      (SK8:SK8-error SK8:GeneralError :strings '("Can’t add parents while copying an object")))
    (when (initializerArgument initArgs 'sk8:properties)
      (SK8:SK8-error SK8:GeneralError :strings '("Can’t add properties while copying an object")))
    (let* ((targetProject (or (initializerArgument initArgs 'sk8:project) (project originalObject)))
           (objectName (initializerArgument initArgs 'sk8:objectName))
           (private? (initializerArgument initArgs 'sk8:privateObject))
           (package (package targetProject))
           newObject)
      
      ;; Process objectName if given
      (when objectName
        (setq objectName (ensure-usable-objectName objectName package)))
      
      ;; Create the new object
      (setq newObject (simple-copy initArgs originalObject targetProject (when objectname (string objectname))))
      
      ;; Process any remaining unused init-args
      (processUnusedInitializerArguments newObject initArgs)
      
      ;; The creation succeeded, so name the object if a name was given
      (when objectName
        (setf (objectname newObject :force (if private? :NEWPRIVATE :NEWPUBLIC)) objectName))
      
      newObject)))

;; The general purpose New and Copy functions. Dispatch to a simple fast or a slower more general routine.

(defmethod New ((me Object) &rest initArgs &key &allow-other-keys)
  (cond ((null initArgs) (simple-new me nil))
        ((and (eq (car initargs) :PROJECT) (null (cddr initargs)))
         (simple-new me (cadr initargs)))
        (t (apply #'general-new me initargs))))

(defmethod SK8:Copy ((me Object) &rest initArgs &key &allow-other-keys)
  (cond ((null initArgs) (simple-copy nil me nil nil))
        ((and (eq (car initargs) :PROJECT) (null (cddr initargs)))
         (simple-copy nil me (cadr initargs) nil))
        (t (apply #'general-copy me initargs))))



#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/10/96	sidney  	record source file info when defining a named object
	3  	 5/13/96	sidney  	
	4  	 5/20/96	sidney  	wasn't setting the proper flag when setting objectname of an object
	5  	 5/23/96	sidney  	
	6  	 5/23/96	sidney  	suppress error in makunbound constant
	8  	 7/19/96	sidney  	slot-value-no-error was getting an error for a non-nil non-object argument
	9  	 7/25/96	sidney  	give classes of named sk8 objects meaningful names
	10 	 7/26/96	sidney  	give classes of unnamed sk8 objects meaningful names
	11 	10/18/96	sidney  	fix wrong arguments to a call of sk8-error
	12 	10/18/96	sidney  	use correct package when referring to SK8 errors
	13 	 1/30/97	sidney  	bug with new ... with otherparents ... when no new properties are defined
	14 	 2/27/97	sidney  	light editing of comments to prepare for publication
	15 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
