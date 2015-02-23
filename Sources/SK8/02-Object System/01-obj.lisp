;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

(require :lispequ)  ; for population-data


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a weak hash table only really acts weak if we explicitly clear its cache, so we need a list of all of them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *all-weak-hash-tables* nil)

(defun clear-all-caches ()
  (clear-clos-caches)
  (setf common-lisp::* nil
        common-lisp::** nil
        common-lisp::*** nil
        common-lisp::/ nil
        common-lisp::// nil
        common-lisp::/// nil
        ccl::@ nil))

;;;;;;;;;;;;;;;;;;
;;; Class Descriptors
;;;;;;;;;;;;;;;;;;


;;; The calls to initialize-class-and-wrapper are ABSOLUTELY necessary (read: without them you'll crash into Macsbug).
;;; This might be considered a bug in MCL: when you supply a class for the creation of a method, MCL doesn't initialize the
;;; class wrapper (thereby creating an inconsistent method if the wrapper hasn't already been initialized).
;;;

(CCL::initialize-class-and-wrapper
 (defclass SK8-accessor-method (standard-method) ()))
(CCL::initialize-class-and-wrapper
 (defclass SK8-reader-method (SK8-accessor-method) ()))
(CCL::initialize-class-and-wrapper
 (defclass SK8-writer-method (SK8-accessor-method) ()))

(defvar *SK8-accessor-method-class* (find-class 'SK8-accessor-method))

;; classes defined in this file: Object, Project

;; public accessors defined with the classes:
;;    knowChildren, Project
;;    File, requiredProject, requiringProjects, Version, Package, ObjectTable, Preferences
;; (what about the others?)

;; external functions defined in this file
;;;  BaseParent Initialize New ObjectName (setf ObjectName) Parents Copy GetValue SetValue


;;; object -- All MacFrames objects inherit from this class:
;;;
;;; sk8_id - Object id. If the object is named, then this is a symbol whose symbol-value is the object.
;;;                If the object is unnamed, then this is a unique local id for the object.
;;; sk8_flags - A set of flags for the object.
;;;   BIT     USE
;;;    0           is it a named object?
;;;    1           is it a private object?
;;;    2           is it a undisposable object?
;;;    3           is it a prototype object?
;;;    4           mark bit. This is a general-purpose bit for use by mark algorithms. It should always be cleared before each use.
;;;    5 - 31   Unused bits (reserved)
;;;

(defclass Object ()
  (
   (knownchildren  :initform nil)   ; list of objects which inherit from this object
   (project        :initform nil)   ; the project to which this object belongs
   (SK8::SK8_flags     :accessor SK8::SK8_flags     :initform #b0)
   (SK8::SK8_id        :accessor SK8::SK8_id        :initform nil)))

(defmethod knownchildren (:method-class SK8-reader-method) ((me Object) &key)
           (slot-value me 'knownchildren))

(defmethod (setf knownchildren) (value (me Object) &key)
  (setf (slot-value me 'knownchildren) value))

(defmethod project (:method-class SK8-reader-method) ((me Object) &key)
           (slot-value me 'project))

(defmethod (setf project) (value (me Object) &key)
  (setf (slot-value me 'project) value))


;;; *macframes-version* -- user-visible version number
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *sk8-features* '(:sk8-version 2)))

(defvar *MACFRAMES-VERSION*       ; SK8 version number
  (getf *sk8-features* :sk8-version))

(defvar *MACFRAMES-PATCH* nil)     ; MacFrames version loaded maximum patch number

;;; project -- The class used by the SK8 Project object
;;;
(defclass Project (Object)
  ((sk8:file :initform nil)
   (sk8:requiredProject :initform nil)
   (sk8:requiringProjects :initform nil)
   (sk8:version :initform #.(princ-to-string (getf *sk8-features* :sk8-version))) ; *** sk8-major-version
   (sk8::package :initform nil)
   (sk8:objectTable :initform nil)
   (sk8:preferences :initform nil)
   (sk8:swapfile :initform nil)
   (sk8::xenoid-typedef-num :accessor sk8::xenoid-typedef-num :initform 0)
   (sk8::function-record-table :accessor sk8::function-record-table :initform nil)
   (sk8::handler-record-table :accessor sk8::handler-record-table :initform nil)
   (sk8::variable-record-table :accessor sk8::variable-record-table :initform nil)
   (sk8::constant-record-table :accessor sk8::constant-record-table :initform nil)
   ))

(defmethod sk8:file (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8:file))

(defmethod (setf sk8:file) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8:file) value))

(defmethod sk8:requiredProject (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8:requiredProject))

(defmethod (setf sk8:requiredProject) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8:requiredProject) value))

(defmethod sk8:requiringProjects (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8:requiringProjects))

(defmethod (setf sk8:requiringProjects) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8:requiringProjects) value))

(defmethod sk8:version (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8:version))

(defmethod (setf sk8:version) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8:version) value))

(defmethod sk8::package (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8::package))

(defmethod (setf sk8::package) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8::package) value))

(defmethod sk8:objectTable (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8:objectTable))

(defmethod (setf sk8:objectTable) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8:objectTable) value))

(defmethod sk8:preferences (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8:preferences))

(defmethod (setf sk8:preferences) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8:preferences) value))

(defmethod sk8:swapfile (:method-class SK8-reader-method) ((me Project) &key)
           (slot-value me 'sk8:swapfile))

(defmethod (setf sk8:swapfile) (:method-class SK8-writer-method) (value (me Project) &key)
           (setf (slot-value me 'sk8:swapfile) value))

;; name the bits in the SK8_flags

(defconstant %flags-named-object? 0)
(defconstant %flags-private-object? 1)
;;(defconstant %flags-not-simple-instantiable? 2)
(defconstant %flags-prototype-object? 3)
(defconstant %flags-marked-object? 4)
(defconstant %flags-specialized-object? 5)
;;(defconstant %flags-virtual-type? 6)
(defconstant %flags-has-creationrelations? 7)
(defconstant %flags-has-ports? 8)

(defconstant %uninheritable-SK8-flags-mask (logior
                                                 (ash 1 %flags-named-object?)
                                                 (ash 1 %flags-prototype-object?)
                                                 (ash 1 %flags-marked-object?)
                                                 (ash 1 %flags-specialized-object?)
                                                 (ash 1 %flags-has-ports?)))

(defconstant %inheritable-SK8-flags-mask (lognot %uninheritable-SK8-flags-mask))

(defun %flags-has-ports-or-creationRelations? (bits)
  (declare (fixnum bits))
  (or (logbitp %flags-has-ports? bits) (logbitp %flags-has-creationRelations? bits)))

(defun %flags-has-creationRelations? (bits)
  (declare (fixnum bits))
  (logbitp %flags-has-creationRelations? bits))

#|
(defun %flags-not-simple-instantiable? (bits)
  (declare (fixnum bits))
  (logbitp %flags-not-simple-instantiable? bits))
|#

(defun %object-has-creationRelations? (obj)
  (logbitp %flags-has-creationRelations? (slot-value obj 'SK8::SK8_flags)))

(defun named-object? (obj)
  (logbitp %flags-named-object? (slot-value obj 'SK8::SK8_flags)))

(defun set-%object-has-creationRelations? (obj)
  (setf (slot-value obj 'SK8::SK8_flags)
        (logior (ash 1 %flags-has-creationRelations?) (slot-value obj 'SK8::SK8_flags))))

(defun set-%object-has-ports? (obj)
  (setf (slot-value obj 'SK8::SK8_flags)
        (logior (ash 1 %flags-has-ports?) (slot-value obj 'SK8::SK8_flags))))

(defun clear-%object-has-ports? (obj)
  (setf (slot-value obj 'SK8::SK8_flags)
        (logand (lognot (ash 1 %flags-has-ports?)) (slot-value obj 'SK8::SK8_flags))))

(defun clear-%object-has-creationRelations? (obj)
  (setf (slot-value obj 'SK8::SK8_flags)
        (logand (lognot (ash 1 %flags-has-creationRelations?)) (slot-value obj 'SK8::SK8_flags))))

;;; *property-characteristics* -- table contains pdata objects for properties
;;;

(defvar *property-characteristics* (make-hash-table :test #'eq))

;;; get-property-info -- returns property information for property with given name (a symbol)
(defmacro get-property-info (name)
  `(gethash ,name *property-characteristics*))

;;; obsolete-property-info -- obsoletes property information for property
(defmacro obsolete-property-info (name)
  `(remhash ,name *property-characteristics*))

;;; make-property-info-entry -- creates an entry for property information
;;;     initializes private property info (list of objects)
;;;
(defun make-property-info-entry (name initiaList)
  (setf (gethash name *property-characteristics*) (ccl::%cons-population initiaList)))

(defun make-private-property (obj property)
  (if (not (private-property-p obj property))
    (let ((entry (get-property-info property)))
      (if entry
        (setf (ccl::population-data entry) (cons obj (ccl::population-data entry)))
        (make-property-info-entry property (list obj)))))
  property)

;;; make-public-property -- undoes the effect of make-private-property
;;;

(defun make-public-property (obj property)
  (let ((entry (get-property-info property)))
    (when entry    ;; if no entry, then this is already public (not private)
      (let ((objects (ccl::population-data entry)))
        (if (cdr objects)
          (setf (ccl::population-data entry) (delete obj objects))
          ;; NOTE: If there were other slots in the pdata entry, we'd have to ensure
          ;;       that they're all empty too before obsoleting the entry.
          (obsolete-property-info property)))))
  property)

;;; private-property-p -- is this property private for the given object?
;;;   If private, returns the object for which the property was declared as private
;;;

(defun private-property-p (obj property &optional (entry (get-property-info property)))
  (when entry
    (setq entry (ccl::population-data entry))
    (or (and (memq obj entry)
             object)
        (let ((class (class-of obj)))
          (dolist (o entry)
            (when (ccl::subclassp class (class-of o))
              (return o)))))))


(defmethod sk8:prototype (me &key)
  (declare (ignore me))
  nil)

(defmethod sk8:prototype ((me sk8:Object) &key)
  (logbitp %flags-prototype-object?
           (slot-value me 'SK8::sk8_flags)))

(defmethod (setf sk8:prototype) (boolean (me sk8:Object) &key)
  (setf (slot-value me 'SK8::sk8_flags)
        (deposit-field (if boolean -1 0)
                       (byte 1 %flags-prototype-object?)
                       (slot-value me 'SK8::sk8_flags))))

(defmethod sk8:private (me &key property)
  (declare (ignore me property))
  nil)

(defmethod sk8:private ((me sk8:Object) &key property)
  (and (if property
         (private-property-p me property)
         (logbitp %flags-private-object?
                  (slot-value me 'SK8::sk8_flags)))
       t))

(defmethod (setf sk8:private) (boolean (me sk8:Object) &key property)
  (if property
    (if boolean
      (make-private-property me property)
      (make-public-property me property))
    (setf (slot-value me 'SK8::sk8_flags)
          (deposit-field (if boolean -1 0)
                         (byte 1 %flags-private-object?)
                         (slot-value me 'SK8::sk8_flags)))))

(defmethod sk8:specialized (me &key)
  (declare (ignore me))
  nil)

(defmethod sk8:specialized ((me ccl::built-in-class) &key)
  (declare (ignore me))
  t)

(defmethod sk8::specialized ((me sk8:Object) &key)
  (logbitp %flags-specialized-object?
           (slot-value me 'SK8::sk8_flags)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Low-level object-allocation functions...
;;;

(defun %maybe-forwarded-non-obsolete-instance (instance)
  (let* ((wrapper (CCL::%instance-class-wrapper instance)))
    (if (and (eql 0 (CCL::%wrapper-instance-slots wrapper))
             (CCL::%forwarding-forwarded-p (CCL::%wrapper-forwarding-info wrapper)))
      (CCL::%forwarded-instance instance)
      instance)))

;*** MAYBE FOLD THIS INTO make-object-and-low-level-init* (SINCE THAT'S THE ONLY CALLER!)
;;; WARNING!!!  If modelObject is non-nil, modelObject and class MUST BE SLOT-WISE CONGRUENT!!!
;;;
(defun SK8-make-instance (class modelObject nameOrID project)
  (flet ((change-class-in-place (obj newclass)
           (let ((wrapper (or (ccl::%class-own-wrapper newclass)
                              (ccl::initialize-class-and-wrapper newclass))))
             (setf (ccl::%svref obj ccl::%instance-class-wrapper) wrapper))
           obj))
    (let ((newflags (ash 1 %flags-private-object?))
          instance oldflags)
      (if modelObject
        (setf instance (change-class-in-place (ccl::copy-uvector (%maybe-forwarded-non-obsolete-instance modelObject)) class)
              oldflags (slot-value instance 'SK8::SK8_flags)
              newflags (if oldflags
                         (logior newflags
                                 (logand oldflags %inheritable-SK8-flags-mask))
                         newflags))
        (setf instance (allocate-instance class)))
      (setf (slot-value instance 'knownChildren) nil
            (slot-value instance 'project) project
            (slot-value instance 'SK8::SK8_flags) newflags
            (slot-value instance 'SK8::SK8_id) nameOrID)
      instance)))

;;; *last-object-oid* -- oid for SK8 objects
;;;    Each newly created SK8 object gets an oid which is monotonically increasing
;;;    as the SK8 session progresses. This id is used to sort SK8 objects
;;;    according to creation order. This is important for efficient re-loading of objects
;;;    when saved in a textual format.
;;;

(defvar *last-object-oid* 0)

(defun get-new-object-oid ()
  (declare (special *last-object-oid*))
  (incf *last-object-oid*))

;;; record-object -- records object into package/project object hash table
;;;    object - object to record
;;;    name - the object name (a symbol)
;;;    project - project in which to record (an object)
;;;

(defun record-object (obj id proj)
  (setf (gethash id (objectTable proj)) obj))

;;; unrecord-object -- unrecords object from package/project object hash table
;;;   object - object to unrecord
;;;    package - package in which to record
;;;

(defun unrecord-object (obj)
  (remhash (SK8::sk8_id obj)
           (objectTable (sk8:project obj))))

;;; WARNING!!!  If originalObject is non-nil, originalObject and newClass MUST BE SLOT-WISE CONGRUENT!!!
;;;
(defun make-object-and-low-level-init* (originalObject newClass oid targetProject)
  (let ((newObject (SK8-make-instance newClass originalObject oid targetProject)))
    (record-object newObject oid targetProject)
    newObject))


;; class owner information is stored in a class property :owner
;; specialized objects have their own class and class owner poinnts to them
;; unspecialized objects that share their class have the class owner point to a population of the ones that share that class

;; Note: The original code used standard-type where here we use ccl::std-type
;; Probably either would work. standard-type is Common Lisp, ccl::std-type is not but typep is much faster
;; Should we use standard-type for portability? If we do shlould we have an implementation dependent speedup of its typep?

(defun class-owner (class)
  (ccl::%class-get (require-type class 'ccl::std-class) :owner))

;; not only will this not produce an error on non-instance classes,
;; it also will not return the unnamed objects that share classes
(defun safe-class-owner (class)
  (when (typep class 'ccl::std-class)
    (let ((obj (ccl::%class-get class :owner)))
      (when (CCL::standard-object-p obj) obj))))

;; fast way of checking for population list, implementation dependent
(defmacro populationdata? (x)
  `(and (uvectorp ,x) (ccl::uvector-subtype-p ,x
                                              #+ppc-target #.ppc::subtag-weak
                                              #-ppc-target #.ccl::$v_weakh)))

(defun set-class-owner (class owner &optional (owner? t))
  (require-type class 'ccl::std-class)
  (when owner  ;; if this really is an object and it is of a different class, remove it as class owner of its current class
    (let ((oldclass (class-of owner)))
      (when (and (typep oldclass 'ccl::std-class) (neq oldclass class))
        (let ((ownerdata (ccl::%class-get oldclass :owner)))
          (if (eq ownerdata owner)
            (ccl::%class-remprop oldclass :owner)
            (when (populationdata? ownerdata)
              (let ((owners (delete owner (ccl::population-data ownerdata))))
                (if owners
                  (setf (ccl::population-data ownerdata) owners)
                  (ccl::%class-remprop oldclass :owner)))))))))
  (if owner?
    (progn
      (ccl::%class-remprop class :owner)
      (when owner (ccl::%class-put class :owner owner)))
    (let ((curown (class-owner class)))
      (if (populationdata? curown)
        (when owner (push owner (ccl::population-data curown)))
        (progn
          (when curown
            (ccl::%class-remprop class :owner))
          (when owner
            (ccl::%class-put class :owner (ccl::%cons-population (list owner)))))))
    ))


;; find the class that unspecialized children of a parent share.
;; Non-sk8 objects that are grafted on to a sk8 parent don't count
(defun simple-find-unowned-class (parentClass)
  (dolist (childClass (CCL::class-direct-subclasses parentClass))
    (when (and
           (typep childClass 'ccl::std-class)
           (not (CCL::standard-object-p (CCL::%class-get childClass :owner)))
           )
      (return childClass))))

;;; find-unowned-class -- for multiple parents
;;;   finds a class which is not owned by any object (i.e., which is sharable) which
;;;   has the same parents as PARENTS
;;;
;; here's a version that doesn't call the children function, but uses the class inheritance instead
(defun find-unowned-class (parentObjs)
  (let ((parentClass (if (listp parentObjs)
                       (unless (cdr parentObjs) (class-of (car parentObjs)))
                       (class-of parentObjs))))
    (if parentClass  ;; single parent?
      (dolist (class (ccl::class-direct-subclasses parentClass))
        (unless (cdr (ccl::class-direct-superclasses class))  ;; child must have single parent if it is to match
          (unless (or
                   (not (typep class 'ccl::std-class))
                   (CCL::standard-object-p (class-owner class)))
            (return class))))
      (let* ((parent-classes (mapcar #'class-of parentObjs))
             (baseparentClass (car (last parent-classes))))
        (dolist (child-class (ccl::class-direct-subclasses baseparentClass))
          (when (and (typep child-class 'ccl::std-class)  ;; not a built-in class, could be a sk8 class
                     (equal (class-direct-superclasses child-class)
                            parent-classes)
                     (not (CCL::standard-object-p (class-owner child-class))))
            (return child-class)))))))

;;; Returns true if the given method is an accessor-method (either made by MCL or by SK8)
;;;
(defun accessor-method-p (meth)
  (let ((methodClass (class-of meth)))
    (or (CCL::subclassp methodClass CCL::*accessor-method-class*)
        (CCL::subclassp methodClass *SK8-accessor-method-class*))))

;;; NOTE: This simply ignores "non-objects"
;;;

(defun map-all-descendants (parent fcn)
  (let ((ancestorclass (class-of parent)))
    (unless (eq ancestorclass CCL::*built-in-class-class*)
      (labels ((fast-map-all-descendants-internal (myclass fcn)
                 (dolist (childclass (ccl::class-direct-subclasses myclass))
                   (let ((ownobj (class-owner childclass)))
                     (when ownobj
                       (if (populationdata? ownobj)
                         (mapc fcn (ccl::population-data ownobj))
                         (let ((childpars (ccl::class-direct-superclasses childclass)))
                           (when (or (null (cdr childpars)) ;; if only one parent, can't be a repetition
                                     (eq myclass
                                         (find ancestorclass childpars
                                               :test #'(lambda(a b)
                                                         (find a (cdr (ccl::%inited-class-cpl b)))))))
                             (funcall fcn ownobj)
                             (fast-map-all-descendants-internal childclass fcn)))))))))
        (fast-map-all-descendants-internal ancestorclass fcn)))))

;; THIS LOOKS LIKE SOMETHING THAT CAN BE OPTIMIZED -- it really is supposed to be just known descendants?
(defmethod sk8dev::mapKnownDescendants ((me Object) func &optional proj &key)
  (let ((funct (if proj #'(lambda(child)
                            (when (eq (sk8::project child) proj)
                              (funcall func child)))
                   func)))
    (map-all-descendants me funct)))

;;;  mapKnownDescendants
;;;  maps function across all descendants of parent in project
;;;  obj - the root, could be a list of roots
;;;  func - a function that takes the descendant object as its only argument
;;;  proj - optionally a project: if provided, function will be called
;;;         only on descendants in the given project.
;;;  (it can skip over descendants of another project)
;;;
(defmethod sk8:KnownDescendants ((me sk8:Object) &optional proj &key)
  (let ((results nil))
    (sk8:mapknowndescendants
     me
     #'(lambda(obj) (push obj results)))
    results))


;;; Maps, in order from descendant to ancestor, the descendants-branch leading down from ancestor to descendant (includes descendant but not ancestor)
;;;
(defun map-descendants-branch (ancestor descendant fcn)
  (unless (is-a descendant ancestor)
    (SK8::SK8-error SK8::GeneralError
                    :strings '(nil " is not a descendant of ")
                    :objects (list descendant ancestor)))
  (let ((ancestorClass (class-of ancestor))
        (descendantClass (class-of descendant)))
    (when (eq (CCL::%instance-class-wrapper descendantClass) CCL::*standard-class-wrapper*)
      (funcall fcn descendant)
      (dolist (superclass (cdr (CCL::%inited-class-cpl descendantClass)))
        (when (eq superclass ancestorClass) (return))
        (when (CCL::subclassp superclass ancestorClass)
          (funcall fcn (class-owner superclass)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  dynamic-slotvalue-inheriter objects...
;;;
;;; These used to use a bogus uvector type, but that caused a different problem with every new version of MCL
;;; By using a typed struct, it costs an extra cell per value, but it works

(defstruct (dynamic-slotvalue-inheriter (:conc-name %slotvalue-inheriter-)
                                        (:copier nil)
                                        (:predicate dynamic-slotvalue-inheriter-p)
                                        (:constructor make-dynamic-slotvalue-inheriter (valueOwner value))
                                        (:print-function (lambda (me strm k)
                                                           (declare (ignore k))
                                                           (format strm "#<dynamic-slotvalue-inheriter of ~s (value: ~s)>"
                                                                   (%slotvalue-inheriter-valueOwner me)
                                                                   (%slotvalue-inheriter-value me)))))
  valueOwner value)

;;; makePropertyPropagate -- makes the property one whose values propagate to
;;;   the knownchildren (and so on) down the inheritance hierarchy.
;;;
(defmethod sk8:makePropertyPropagate ((me sk8:object) property)
    (if (and (slot-exists-p me property) (not (find-handler property me)))
      (unless (dynamic-slotvalue-inheriter-p (slot-value-no-error me property))
        (without-interrupts
         (localize-and-set-propagatable-value me property (slot-value me property) nil)
         (add-SK8-accessors (class-of me) property t)
         t))
      (error "Cannot make '~a' propagatable because it is not a real property of ~a"
             property me)))

;;; makePropertyNotPropagate -- undoes effect of  makePropertyPropagate
;;;
(defmethod sk8:makePropertyNotPropagate ((me sk8:object) property)
  (when (dynamic-slotvalue-inheriter-p (slot-value-no-error me property))
    (without-interrupts
     (unlink-propagatable-values me property)
     (add-SK8-accessors (class-of me) property nil)
     t)))

;;; propagatableValue -- test to see if the property's value can propagate
;;;
(defmethod sk8:propagatableValue ((me sk8:Object) property)
  (when (dynamic-slotvalue-inheriter-p (slot-value-no-error me property))
    t))

;;; Works only with real SK8 objects (standard-instances) and only maps over the real SK8 object parents
;;;
(defmacro do-SK8-parents ((parentVar SK8object) &body body)
  (let* ((parent-classes (gensym))
         (parent-class (gensym)))
    `(let ((,parent-classes (CCL::%class-local-supers (class-of ,SK8object)))
           ,parentVar)
       (dolist (,parent-class ,parent-classes)
         (when (CCL::standard-object-p (setq ,parentVar (class-owner ,parent-class)))
           ,@body)))))

;;; parents virtual property. 
;;;
(defmethod parents ((me Object) &key)
  (delete nil (mapcar #'(lambda(class) (let ((parentobj (class-owner class)))
                                         (when (ccl::standard-object-p parentobj)
                                           parentobj)))
                      (CCL::%class-local-supers (class-of me)))))

;;; baseParent -- returns the base parent for an object
;;;

;;; Dispatch on ANYTHING (instead of just Objects), so that as long as the thing knows how to return
;;; its parents, we can get its baseParent.
;;;
(defmethod baseParent ((me T) &key)
  (car (last (parents me))))

;;; For real SK8 objects, we find the baseParent more efficiently (no consing)
;;;
(defmethod baseParent ((me Object) &key)
  (let (lastParent)
    (do-SK8-parents (p me)
                    (setq lastParent p))
    lastParent))

;;; Looks through the parents for one for which the given property is dynamically propagatable
;;;  - if one is found, returns its "dynamic-slotvalue-inheriter" object
;;;  - otherwise, returns nil
;;;
(defun find-parent-dynamic-inheriter (obj property)
  (do-SK8-parents (parent obj)
    (let ((inheriter (ccl::%slot-value parent property)))
      (when (dynamic-slotvalue-inheriter-p inheriter) (return inheriter)))))

;;; Stores a new "dynamic-slotvalue-inheriter" holding the given value into the given property of the given object and,
;;; if no-descendants? is non-nil, propagates that "dynamic-slotvalue-inheriter" to all the object's descendants that
;;; don't already have their own localized "dynamic-slotvalue-inheriter"s.  Returns the given value.
;;;
(defun localize-and-set-propagatable-value (obj property value old-dynamic-inheriter &optional no-descendants?)
  (let ((new-dynamic-inheriter (make-dynamic-slotvalue-inheriter obj value)))
    (without-interrupts  ;; really without interrupts???
     (when (slot-exists-p obj property)
       (unless no-descendants?
         (unless old-dynamic-inheriter
           (setf old-dynamic-inheriter (let ((v (slot-value obj property))) 
                                         (when (dynamic-slotvalue-inheriter-p v) v)))
           (unless old-dynamic-inheriter
             (setq old-dynamic-inheriter (find-parent-dynamic-inheriter obj property))))
         (if old-dynamic-inheriter
           ;; There was already a dynamic-inheriter, so we must only update descendants holding that one
           (flet ((slot-setter (o)
                    (when (eq (slot-value o property) old-dynamic-inheriter)
                      (setf (slot-value o property) new-dynamic-inheriter))))
             (declare (dynamic-extent #'slot-setter))
             (map-all-descendants obj #'slot-setter))
           ;; There was NOT already a dynamic-inheriter, so we update all descendants
           (flet ((slot-setter (o)
                    (let ((wrapper (CCL::%instance-class-wrapper o)))
                      ;; Set the slot's value when either (1) the object is NOT obsolete or (2) it is obsolete because this
                      ;; property was just added.  This leaves alone descendants that locally redefine the property (which
                      ;; is important in the case where a new property appeared due to an added parent).
                      (when (or (not (eql 0 (CCL::%wrapper-hash-index wrapper)))
                                (not (find property (CCL::%forwarding-instance-slots
                                                     (CCL::%wrapper-forwarding-info wrapper)) :test #'eq)))
                        (setf (slot-value o property) new-dynamic-inheriter)))))
             (declare (dynamic-extent #'slot-setter))
             (map-all-descendants obj #'slot-setter))))
       (setf (slot-value obj property) new-dynamic-inheriter)))
    value))

;;; If the given property of the given object is dynamically propagatable, its "dynamic-slotvalue-inheriter" is replaced by
;;; its simple slot-value, and likewise for that property of all the object's descendants
;;;
(defun unlink-propagatable-values (obj property)
  (without-interrupts
   (when (slot-exists-p obj property)
     (let ((val (slot-value obj property)))
       (when (dynamic-slotvalue-inheriter-p val)
         (setf (slot-value obj property) (%slotvalue-inheriter-value val))
         ;; There was NOT already a dynamic-inheriter, so we update all descendants
         (flet ((slot-unlinker (o)
                  (when (slot-exists-p o property)
                    (let ((val (slot-value o property)))
                      (when (dynamic-slotvalue-inheriter-p val)
                        (setf (slot-value o property) (%slotvalue-inheriter-value val)))))))
           (declare (dynamic-extent #'slot-unlinker))
           (map-all-descendants obj #'slot-unlinker)))))))

;;; PROPAGATE-VALUE
;;;   Forces the propagation of the object's property value down the knownchildren inheritance hierarchy.
;;;   This is FULLY GENERAL; i.e. if the slot doesn't exist, it'll call the accessors.
;;;
;;;  NOTE: either 'from' or 'to' (or both) must be provided
;;;   from - if provided, this is the object from wihch the propagation starts (the source);
;;;         otherwise the source is taken to be the ancestor of <to> from which its parent inherits the given property value
;;;   to     - if provided, the propagation goes only down the inheritance path from the source to this object, and stops at this object;
;;;         otherwise the value is propagated from the source to all of its descendants
;;;
(defun sk8:propagatevalue (property &key from to)
  (flet ((missing-property (obj prop)
           (SK8::SK8-error SK8::GeneralError
                           :strings '(nil " has no " " property")
                           :objects (list obj prop))))
    (cond
     ;; Propagate value down from TO's parent (or from whomever the parent gets the value)
     ((null from)
      (unless to (error "Must specify either from or to in order to propagate a property value"))
      (let ((value (find-parent-slot-value to property)))
        (if (eq value (ccl::%slot-missing-marker))
          (missing-property to property)
          (if (slot-exists-p to property)
            (setf (slot-value to property) value)
            (progn
              (when (dynamic-slotvalue-inheriter-p value)
                (setq value (%slotvalue-inheriter-value value)))
              (funcall (CCL::setf-function-name property) value to))))))
     ;; Propagate value starting at FROM...
     (t
      (let ((failures nil)
            (value (slot-value-no-error from property)))
        (when (eq value (ccl::%slot-missing-marker))
          (multiple-value-bind (propertyValue err?) (ignore-errors (funcall property from))
            (when err? (missing-property from property))
            (setq value propertyValue)))
        (flet ((mapper (o)
                 (if (slot-exists-p o property)
                   (setf (slot-value o property) value)
                   (push o failures))))
          (declare (dynamic-extent #'mapper))
          (if to
            ;; Propagate value starting at FROM, only along the descendants branch down to TO
            (map-descendants-branch from to #'mapper)
            ;; Propagate value starting at FROM, to all its descendants
            (map-all-descendants from #'mapper)))
        (when failures
          (let ((setterName (CCL::setf-function-name property)))
            (when (dynamic-slotvalue-inheriter-p value)
              (setq value (%slotvalue-inheriter-value value)))
            (dolist (child failures)
              (funcall setterName value child)))))))))

;;; sets the actual property value (without calling the setter handler)
;;;
(defun setValue (property obj value)
   (when (slot-exists-p obj property)   ;; necessary for back compatibility
    (let ((inheriter (and (slot-boundp obj property)
                          (slot-value obj property))))
      (if (dynamic-slotvalue-inheriter-p inheriter)
        (if (eq (%slotvalue-inheriter-valueOwner inheriter) obj)
          (setf (%slotvalue-inheriter-value inheriter) value)
          (localize-and-set-propagatable-value obj property value inheriter))
        (setf (slot-value obj property) value)))))

;;; gets the actual property value (without calling the getter handler)
;;;
(defun getValue (property obj)
  (let ((value (slot-value obj property)))
    (if (dynamic-slotvalue-inheriter-p value)
      (%slotvalue-inheriter-value value)
      value)))

;;;;; should 'me be in the SK8 package, or exported, or what?

;;; add-SK8-accessors -- makes appropiate SK8 accessor methods
;;;  class - the CLOS class on which to define the accessor
;;;  propertyName - the name of the property (a symbol) for which to make an accessor
;;;  propagatableValue? - if true, then make propagatable-value accessors
;;;  justSetterOrGetter - if nil, do both; if :getter just do getter; if :setter just do setter
;;;
(defun add-SK8-accessors (class propertyName propagatableValue? &optional justSetterOrGetter)
  (unless (eq justSetterOrGetter :setter)
    (if propagatableValue?
      (eval `(defmethod ,propertyName (:method-class SK8-reader-method) ((me ,class) &key)
                        (getValue ',propertyName me)))
      (eval `(defmethod ,propertyName (:method-class SK8-reader-method) ((me ,class) &key)
                        (slot-value me ',propertyName)))))
  (unless (eq justSetterOrGetter :getter)
    (if propagatableValue?
      (eval `(defmethod (setf ,propertyName) (:method-class SK8-writer-method) (newValue (me ,class) &key)
                        (setValue ',propertyName me newValue)))
      (eval `(defmethod (setf ,propertyName) (:method-class SK8-writer-method) (newValue (me ,class) &key)
                        (setf (slot-value me ',propertyName) newValue))))))

;;; CREATE-CLASS -- creates a new CLOS class with a gensym for a name so it can be GCd (minimizes extra work not necessary for SK8 classes)
;;;  superclasses - list of superclasses (create-class--actually %defclass) makes a copy of it)
;;;  slots-info - list of (property-name inherits-p) items, one for each slot in the class that is to be created
;;;  slots - list of slot definitions
;;;  reuse-class - if non-nil, it is a class object that will be reused for this class, so that its instances will automatically be converted
;;;  classname - string to use for class name
;;;
(defun create-class (superclasses slots-info slots reuse-class classname)
  (let ((ccl::*suppress-compiler-warnings* t)
        (*save-definitions* nil)
        (*save-local-symbols* nil)
        (*fasl-save-local-symbols* nil)
        (*save-doc-strings* nil)
        (*record-source-file* nil)
        (newclassname (if reuse-class
                        (class-name reuse-class)
                        (make-symbol (or classname "sk8-class")))))
    (let ((class (ccl::%defclass newclassname superclasses slots nil nil nil nil)))
      (dolist (slot-info slots-info)
        (let ((property-name (first slot-info))
              (propagates-value-p (second slot-info)))
          (add-SK8-accessors class property-name propagates-value-p)))
      class)))


#+ppc-target
(defun %set-nth-immediate (lfun-vect i val &aux (n (ccl::%count-immrefs lfun-vect)))
  (declare (fixnum i n))
  (unless (and (>= i 0) (< i n))
    (ccl::report-bad-arg i `(integer 0 ,n)))
  (ccl::%svset lfun-vect (the fixnum (1+ i)) val))

#-ppc-target
(defun %set-nth-immediate (lfun-vect n val)
  (let ((offset (+ 3 (CCL::%ilsr 1 (CCL::%immediate-offset lfun-vect n))))
        (adrs (%address-of val)))
    (without-interrupts ; to prevent any possibility of GC (which would change val's address!)
     (setf (uvref lfun-vect (1+ (the fixnum offset)))  (point-h adrs)
           (uvref lfun-vect offset)                    (point-v adrs)))
    val))

#-ppc-target
(defun deep-copy-lfun (lfun)
  (labels ((deep-copy-lfun* (lfun)
             (let* ((source-vector (CCL::%lfun-vector lfun))
                    (size (uvsize source-vector))
                    (dest-vector (CCL::make-uvector size CCL::$v_nlfunv))
                    (new-lfun (CCL::%LFUN-VECTOR-LFUN dest-vector))
                    (num-immrefs (CCL::%count-immrefs source-vector))
                    (lfun-bits (CCL::lfun-attributes lfun))
                    (lfun-info-index (when (CCL::%ilogbitp CCL::$lfatr-symmap-bit lfun-bits)
                                       (CCL::%i- num-immrefs
                                                 (if (CCL::%ilogbitp CCL::$lfatr-noname-bit lfun-bits)
                                                   1
                                                   2)))))
               ;; First copy the vector verbatim
               (dotimes (i size) (setf (uvref dest-vector i) (uvref source-vector i)))
               
               ;; Now put a COPY of the lfun-info list into the dest-vector's lfun-info
               (when lfun-info-index
                 (%set-nth-immediate dest-vector lfun-info-index (copy-list (CCL::%lfun-info lfun))))
               
               ;; Now copy all the nested lfuns and prepare them to be init'ed by !init_lfun_links_&_cache_breakpts
               (let (nestedLfun nestedLfunName)
                 (dotimes (i (1- (the fixnum num-immrefs)))
                   (when (CCL::LFUNP (setq nestedLfun (CCL::%NTH-IMMEDIATE dest-vector i)))
                     (setq nestedLfunName (CCL::LFUN-VECTOR-NAME (CCL::%LFUN-VECTOR nestedLfun)))
                     (when (functionp nestedLfunName)
                       ;; Copy the nested lfun (recursively)
                       (setq nestedLfun (deep-copy-lfun* nestedLfun))
                       (%set-nth-immediate dest-vector i nestedLfun)
                       ;; Reset the new nested lfun's name
                       (CCL::LFUN-VECTOR-NAME (CCL::%LFUN-VECTOR nestedLfun) nil)))))
               new-lfun)))
    
    (deep-copy-lfun* lfun)))

#+ppc-target
(defun deep-copy-lfun (lfun)
  (labels ((deep-copy-lfun* (lfun)
             (let ((new-lfun (ccl::copy-uvector lfun)) ;; First copy the vector verbatim
                   (num-immrefs (CCL::%count-immrefs lfun))
                   (lfun-info (ccl::%lfun-info lfun)))
               ;; Now put a COPY of the lfun-info list into the new lfun's lfun-info
               (when lfun-info
                 (setf (ccl::%svref lfun (ccl::%i- (uvsize lfun) 3)) (copy-list lfun-info)))
               ;; Now copy all the nested lfuns and prepare them to be init'ed by !init_lfun_links_&_cache_breakpts
               ;; (I don't know why this is here or when the nested lfun has a name that is functionp, but this should work
               (let (nestedLfun nestedLfunName)
                 (dotimes (i (1- (the fixnum num-immrefs)))
                   (when (CCL::LFUNP (setq nestedLfun (CCL::%NTH-IMMEDIATE new-lfun i)))
                     (setq nestedLfunName (CCL::LFUN-VECTOR-NAME (CCL::%LFUN-VECTOR nestedLfun)))
                     (when (functionp nestedLfunName)
                       ;; Copy the nested lfun (recursively)
                       (setq nestedLfun (deep-copy-lfun* nestedLfun))
                       (%set-nth-immediate new-lfun i nestedLfun)
                       ;; Reset the new nested lfun's name
                       (CCL::LFUN-VECTOR-NAME (CCL::%LFUN-VECTOR nestedLfun) nil)))))
               new-lfun)))
    (deep-copy-lfun* lfun)))

;;; copy-handler -- copies a handler from one object to another
;;;   name - the name of the handler
;;;   old-class - the class in which to find the handler
;;;   new-class - the class into which to copy the handler
;;;
(defun copy-handler (name old-class new-class)
  (let* ((generic (symbol-function name))
         (dispatch-argnum nil)
         (source-method nil)
         new-lfun
         new-method
         num-required-args
         specializers)
    (dolist (meth (generic-function-methods generic))
      (setq specializers (method-specializers meth))
      (when (and (setq dispatch-argnum (position ccl::*T-CLASS* specializers :test-not #'eq))
                 (eq (elt specializers dispatch-argnum) old-class))
        (setq source-method meth)
        (setq num-required-args (length specializers))
        (return)))
    (when source-method
      (setf new-lfun (deep-copy-lfun (method-function source-method))
            specializers (make-sequence 'list num-required-args :initial-element ccl::*T-CLASS*)
            (elt specializers dispatch-argnum) new-class
            new-method (make-instance (class-of source-method)
                         :function new-lfun
                         :specializers specializers
                         :name name))
      (ccl::lfun-name (ccl::closure-function new-lfun) new-method)
      (add-method generic new-method)
      new-method)))


(defun makeup-symbol-from-oid (original-class oid)
  (let ((parname (symbol-name (class-name original-class))))
    (concatenate 'string (subseq parname 0 (position #\- parname)) "-" (write-to-string oid))))

(defun makeup-class-name (obj)
  (let ((id (sk8_id obj)))
    (if (symbolp id)
      (symbol-name id)
      (makeup-symbol-from-oid (class-of (baseparent obj)) id))))

;;; make-own-object-class -- makes a new class for object
;;;    newProperty - name of property to add when making the new class (or nil if no change in properties)
;;;
;;; NOTE: this function doesn't need to worry about updating any children of the given object, because
;;;      make-own-object-class is NEVER called with an object that already owns its class, therefore
;;;      this object can't have any "real object" children (since spawning a child causes an object to
;;;      own its class)
;;;
(defun make-own-object-class (obj &key newProperty)
  (let* ((oldClass (require-type (class-of obj) 'ccl::std-class))
         (oldLocalSlots (CCL::%class-local-instance-slotds oldClass))
         (newLocalSlots oldLocalSlots)
         newClass)
    ;; Adding the new property if necessary
    (when newProperty
      (when (slot-exists-p obj newProperty)
        (error "Property ~a can't be added to ~a because it ~:[already has it~;is inherited from an ancestor~]."
               newProperty obj (assq newProperty oldLocalSlots)))
      (push (CCL::%cons-slotd newProperty nil nil nil) newLocalSlots))
    ;; Make the new class
    (setq newClass (create-class (copy-list (CCL::%class-local-supers oldClass))
                                 nil newLocalSlots nil
                                 (makeup-class-name obj)))
    ;; Copy all the old accessor functions
    (dolist (slot oldLocalSlots)
      (copy-handler (car slot) oldClass newClass))
    ;; Hook it up to the given object
    (set-class-owner newClass obj)
    (change-class obj newClass)
    newClass))

(defvar *property-spec-value-marker* 'value)
(defvar *OLD-property-spec-value-marker* :value)
(defvar *property-spec-options* '((propagatedValue  . 1)
                                      (private  . 2)))
(defvar *OLD-property-spec-options* '((:inherit  . 1)
                                           (:private  . 2)))

(defun mark-sym-global (sym)
  (setf (get sym 'SK8-global) t))

(defun mark-sym-not-global (sym)
  (remprop sym 'SK8-global))

(defun mark-sym-function (sym)
  (setf (get sym 'SK8-function) t))

(defun mark-sym-not-function (sym)
  (remprop sym 'SK8-function))

(defun global-sym-p (sym)
  (get sym 'SK8-global))

(defun function-sym-p (sym)
  (get sym 'SK8-function))

;;; publish-project-symbol -- publishes a symbol which should be considered bound to a handler (method)
;;;
;;; Second argument is either the package of a project, or a project, or a list of either
;;; I want to require the argument to be a package, but have not completed making sure that all calls to this function
;;; that use a project argument have been modified. Until then, there's code here to deal with the different forms of argument

(defun publish-project-symbol (sym myproject)
  (when (packagep myproject)
    (dolist (i (sk8::knowndescendants sk8::project))
      (when (eq (package i) myproject)
        (setf myproject i)))
    (if (packagep myproject) (error "Can't find project for package ~a." myproject)))
  (if (listp myproject)
    (dolist (p myproject)
      (publish-project-symbol sym p))
    (let ((mypkg (if (packagep myproject) myproject (package myproject))))
      (unless (eq (symbol-package sym) mypkg)
        (import sym mypkg))
      (export sym mypkg)
      (let ((projs (requiringProjects myproject)))
        (when projs
          (publish-project-symbol sym projs))))))

;;; These two are same as the above function, but also mark the symbol as a "global"/"function"
;;;
(defun publish-global-symbol (sym fromPackage)
  (mark-sym-global sym)
  (publish-project-symbol sym fromPackage))

(defun publish-function-symbol (sym myProject)
  (mark-sym-function sym)
  (publish-project-symbol sym myProject))

;;; parse-object-definition -- returns the object's class
;;;       If necessary, it creates new class (with new slot definitions and accessor methods)
;;;  class - candidate class for object
;;;  parents - object's parents
;;;  specs - list of SK8 property specifications for new properties for this object
;;;       A list of SK8 property specs may be provided to NEW as its :properties keyword
;;;       A property spec may be a symbol or a list:
;;;  private-object-p -- non-nil if all properties of the object should be private
;;;
;;;          If the spec is a symbol, then it's the name of a new property. The default initial value of
;;;          the property is false, the property is public and its value is not inheritable.
;;;
;;;          If a list, then the first element must be the property name. The following optional
;;;          elements may include (in any order) the keyword :inherit (which means that the
;;;          property value should be inheritable), the keyword :private (which means that
;;;          the property is private to this object (i.e., is not "visible" to projects that
;;;          use the project in which this object exists), and the keyword :value which MUST
;;;          be followed for some value: this value is the default initialization value for this
;;;          property (not only for this object, but also for descendant objects). If the :value
;;;          keyword is not provided, then the default initialization value is false (SK8 properties
;;;          are never unbound in the Common Lisp sense).
;;;

(defun parse-object-definition (name class targetProject parents specs private-object-p)
  (declare (special *property-spec-value-marker* *OLD-property-spec-value-marker*))
  (let* (property-name
         (create-class (null class))        ; object needs its own new class
         (multiple (consp parents))
         (parents-class (if (not multiple) (class-of parents)))
         (parents-slots (if parents-class (class-instance-slots parents-class)))
         (superclasses (if multiple         ; list of superclasses for new class
                         (mapcar #'class-of parents)
                         (list parents-class)))
         slots            ; list of new slot definitions
         value            ; initial value of slot
         inherits-value   ; this property is specified to inherit values
         slot             ; a slot definition
         listp
         accessors
         )
    
    (dolist (spec specs)
      (setq property-name (if (setq listp (listp spec))
                            (car spec)
                            spec))
      (setq value (when listp
                    (cadr (or (memq *property-spec-value-marker* spec)
                              (memq *OLD-property-spec-value-marker* spec)))))
      
      ;; Property defined as inheritable?
      (setq inherits-value nil)
      (when (and listp
                 (or (memq 'propagatedValue spec)
                     (memq :inherit spec)))
        (setq inherits-value t)
        )
      
      ;; Is this a new slot?
      (if multiple
        (dolist (parent parents (setq slot nil))
          (if (setq slot (assq property-name (class-instance-slots (class-of parent))))
            (return nil)))
        (setq slot (assq property-name parents-slots)))
      
      ;; Publish public property?
      (when (and (not private-object-p)
                 listp
                 (not (or (memq 'private spec)
                          (memq :private spec))))
        (publish-project-symbol property-name targetProject))
      
      ;; Build slot definitions and accessor forms for a new property:
      (cond (slot
             (unless (equal value (caadr slot)) ; -> SK8Script-equal
               ;; Object's initial value is different for this slot (unfortunately, this requires a new class for it)
               (push (ccl::%cons-slotd property-name (list value) nil nil) slots)  
               (setq create-class t)))
            
            (t ; object needs a new slot
             (setq create-class t)
             (push (list property-name inherits-value) accessors)
             
             (push (ccl::%cons-slotd property-name (list value) nil nil) slots))))
    ;; Is a new class required?
    (when create-class
      (setf class (create-class superclasses (nreverse accessors) (nreverse slots) nil (when name (string name)))))
    
    class))

;;;  Make a simple (unspecialized) new object; don't call initialize on it
;;;
(defun new-object-and-low-level-init (originalObject targetProject)
  (let ((originalClass (class-of originalObject)))
    (unless (eq (class-owner originalClass)
                originalObject)
      (setq originalClass (make-own-object-class originalObject)))
    ;; Find or create an appropriate class for object:
    (let ((shareableChildClass (simple-find-unowned-class originalClass))
          (oid (get-new-object-oid)))
      (unless shareableChildClass
        (setq shareableChildClass (parse-object-definition (makeup-symbol-from-oid originalClass oid) NIL targetProject originalObject NIL T)))
      (let ((obj (make-object-and-low-level-init*
                  originalObject shareableChildClass oid targetProject)))
        (set-class-owner shareableChildClass obj nil)
        obj))))

;;; copy-method-to-object -- copies a method into an object
;;;  method - the CLOS method object
;;;  object - object into which to copy the method
;;;

(defun copy-method-to-object (method obj)
  (let* ((name (method-name method))
         (class (class-of obj))
         (new-lfun (deep-copy-lfun (method-function method)))
         (specializers (method-specializers method))
         (new-specializers (make-sequence 'list (length specializers) :initial-element ccl::*T-CLASS*))
         new-method)
    (setf (elt new-specializers (position ccl::*T-CLASS* specializers :test-not #'eq)) class
          new-method (make-instance (type-of method)
                       :function new-lfun
                       :specializers new-specializers
                       :qualifiers (method-qualifiers method)
                       :name name))
    (ccl::lfun-name (ccl::closure-function new-lfun) new-method)
    (add-method (symbol-function name) new-method)
    new-method))

;;; copy-local-handlers -- copies the local handlers of one object into another object
;;;                   Also sets up a similar copy of the handlers in the store.
;;; from - the object from which to copy the handlers
;;; to - the object to which to copy the handlers
;;;
(defun copy-local-handlers (from to)
  (let ((from-class (require-type (class-of from) 'ccl::std-class)))
    (when
      (logbitp %flags-specialized-object? (SK8::sk8_flags from))
      (require-type (class-of to) 'ccl::std-class)
      (dolist (handler (ccl::specializer-direct-methods from-class))
        (setq handler (copy-method-to-object handler to))
        ))))

;;;  Make a simple (unspecialized) object copy; don't call initialize on it
;;;
(defun copy-object-and-low-level-init (originalObject targetProject name)
  (let* ((originalClass (class-of originalObject))
         (owner? (let ((ownr (class-owner originalClass)))
                   (when (CCL::standard-object-p ownr) ownr)))
         (oid (get-new-object-oid))
         (newClass (if owner?
                     (create-class (CCL::%class-local-supers originalClass) nil
                                   (copy-list (cdr (the list (CCL::%class-direct-slots originalClass)))) nil
                                   (or name (makeup-symbol-from-oid originalClass oid)))
                     originalClass))
         (copy (make-object-and-low-level-init* originalObject newClass oid targetProject))
         (i (uvsize copy))
         inheriter)
    (declare (fixnum i))
    ;; Localize any dynamically propagatable slots that were local to the original
    (loop
      (when (eql (decf i) 0) (return))
      (when (and (setq inheriter (let ((v (CCL::%svref copy i))) (when (dynamic-slotvalue-inheriter-p v) v)))
                 (eq originalObject (%slotvalue-inheriter-valueOwner inheriter)))
        (setf (CCL::%svref copy i)
              (make-dynamic-slotvalue-inheriter copy (%slotvalue-inheriter-value inheriter)))))
    ;; set its owner and copy any handlers it has
    (set-class-owner newClass copy owner?)
    (when owner?
      (copy-local-handlers originalObject copy))
    copy))


;;; Lists and arrays are the only things copied; everything else is left EQ.
;;;
(defun copy-non-SK8-object (obj)
  (cond
   ((listp obj) (copy-list obj))
   ((ccl::sequencep obj)
    (if (array-has-fill-pointer-p obj)
      (let ((fill (fill-pointer obj))
            (size (array-total-size obj)))
        (setf (fill-pointer obj) size)
        (let ((newobj (make-array size :adjustable (adjustable-array-p obj)
                                  :initial-contents obj
                                  :fill-pointer fill)))
          (setf (fill-pointer obj) fill)
          newobj))
      (copy-seq obj)))
   ((arrayp obj)
    (multiple-value-bind (realobj offset) (displaced-array-p obj)
      (if realobj
        (make-array (array-dimensions obj)
                    :adjustable (adjustable-array-p obj)
                    :displaced-to realobj :displaced-index-offset offset)
        ;; the following never gets evaluated in current implementation of MCL (non-sequence arrays are implemented as displaced vectors), but is here for completeness
        (let ((newobj (make-array (array-dimensions obj)  :adjustable (adjustable-array-p obj))))
          (dotimes (i (array-total-size obj))
            (setf (row-major-aref newobj i) (row-major-aref obj i)))
          newobj))))
   ;; It may be useful to implement here something that copies some other types of objects,
   ;; such as hash-table, regions (at least those created with (T_NewRgnGC), other types of macptrs
   (t obj)))

;;; Only works on vectors & arrays of element-type t; just returns NIL if original isn't one of those.  This is used only by
;;; process-plural-simpleRelation & process-plural-creationRelation.
;;;
(defun copy-object-vector-or-array (original)
  (when (arrayp original)
    (let ((newobj (copy-non-sk8-object original))
          (size (if (vectorp original)
                  (length original)
                  (array-total-size original))))
      (multiple-value-bind (realobj offset) (displaced-array-p newobj)
        (if realobj
          (values newobj realobj offset (+ offset size))
          (values newobj newobj 0 size))))))

;;; setterp -- given an accessor (a symbol) and an object, 
;;;         will tell us if the accessor can be used to set a property
;;;         of the object.  Returns the setter if so.
;;;   name - a symbol
;;;   obj - a SK8 object
;;;
(defun sk8::setterp (name obj)
  (let ((setterName (gethash name CCL::%setf-function-names%)))
    (when setterName
      (let ((setterFunction (fboundp setterName))
            (objectCPL (CCL::%class-precedence-list (class-of obj))))
        (when setterFunction
          (if (typep setterFunction 'standard-generic-function)
            (dolist (method (CCL::%gf-methods setterFunction))
              (when (memq (second (CCL::%method-specializers method)) objectCPL)
                (return setterName)))
            setterName))))))


#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/10/96	Hernan  	In sk8::private, it should be private-property-p (not private-propertyp).
	3  	 5/10/96	sidney  	get some packages correct
						define setterp function
	4  	 5/20/96	sidney  	some package changes
	5  	 5/21/96	sidney  	package oops
	6  	 6/13/96	sidney  	
	7  	 7/ 7/96	sidney  	changes for native PPC build
	8  	 7/19/96	sidney  	rewrite deep-copy-lfun to work with PPC MCL 3.9
	9  	 7/25/96	sidney  	give classes of named sk8 objects meaningful names
	10 	 7/26/96	sidney  	give classes of unnamed sk8 objects meaningful names
	11 	 8/23/96	Brian   	
	12 	11/22/96	Brian   	Fixing private to always take a private keyword.
	13 	 2/27/97	sidney  	light editing of comments to prepare for publication
	14 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
