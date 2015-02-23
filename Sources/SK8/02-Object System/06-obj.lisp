;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define-handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make MCL meta-. find MacFrames handlers
;;;
(rplacd (cdr (assq 'method ccl::*define-type-alist*)) (list "ine-handler" "ine-system-handler"))
;;; *** still need to make ccl::search-method-classes (in fred-additions.lisp) return T

;; only do the simple case for now of an object name, not an object or list
;;  That makes it easy, it just maps into a defmethod on the object class
;; The only trick is making the object class name dynamic... Hmmm...
;;
;; syntax is (define-handler handler-name [qualifier]* (object-name [arg]*) body)
;; qualifier is :private, :hidden. Others are not supported anymore
;; We no longer support a breakpoint vector in there either
;; We no longer support the special WITH syntax

(defmacro SK8:define-handler (handler-name option-or-args &body body &environment env)
  (let ((private-p nil)
        (hidden-p nil)
        (lambda-list option-or-args)
        (ok-p t)
        (quallist nil)
        (obj nil)
        (objobj nil)
        (classname nil)
        (funvar (make-symbol "function"))
        (methvar (make-symbol "method")))
    (loop
      (if (consp lambda-list)
        (return)
        (case lambda-list
          (:private (if private-p
                      (setf ok-p nil)
                      (setf private-p t)))
          (:hidden (if hidden-p
                     (setf ok-p nil)
                     (setf hidden-p t)))
          ((:before :after :around) (if quallist
                                      (setf ok-p nil)
                                      (push lambda-list quallist)))
          (t (setf ok-p nil))))
      (unless ok-p (error "Bad lambda list in handler named ~s" handler-name))
      (unless body (return))
      (setf lambda-list (pop body)))
    ;; force the lambda-list to have &key so any subsequent method may add its own keyword arguments
    (unless (or hidden-p (member '&key lambda-list))
      (let ((auxpos (position '&aux lambda-list)))
        (setf lambda-list 
              (if auxpos
                (append (subseq lambda-list 0 auxpos) (list '&key) (nthcdr auxpos lambda-list))
                (append lambda-list (list '&key))))))
    (let ((setterp (when (listp handler-name)
                     (or (and (eq (car handler-name) 'SETF)
                              (symbolp (cadr handler-name))
                              (null (cddr handler-name)))
                         (error "Inappropriate handler name")))))
      (if setterp
        (setf obj (second lambda-list)
              lambda-list  (list* (first lambda-list) 'me (cddr lambda-list)))
        (setf obj (first lambda-list)
              lambda-list (cons 'me (cdr lambda-list))))
      (case obj
        (sk8:object  ;; special hack for (define-handler foo (object...)) so it works for shadow classes too
         (setf objobj T classname T))
        (sk8:Function (setf objobj T classname ''Function))  ;; special hacks for function and handler pseudo-objects
        (sk8:Handler (setf objobj T classname ''standard-method))
        (T
         (if (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'built-in-class)) ;; special hack for built-in class pseudo-objects
           (setf objobj obj)
           (setf objobj (list 'class-of obj)))
         (setf classname (list 'class-name objobj))))
      ;; unfortunately we have to use the macro expansion of defmethod to get the class name at run time
      (multiple-value-bind (function-form specializers-form qualifiers method-class documentation specializers)
                           (ccl::parse-defmethod handler-name `(,@quallist ,lambda-list ,@body) env)
        (if setterp
          (setf specializers (list* (first specializers) :SK8OBJECT (cddr specializers))  ;; hack to make warning messages look ok
                specializers-form (list* (first specializers-form) (second specializers-form)
                                         classname (cdddr specializers-form)))
          (setf specializers (cons :SK8OBJECT (cdr specializers))
                specializers-form (list* (first specializers-form)
                                         classname (cddr specializers-form))))
        `(progn
           (eval-when (:compile-toplevel)
             (ccl::note-function-info ',handler-name nil ,env))
           ,@(when (and (consp objobj) (eq (first objobj) 'class-of))
               (list (list 'ensure-real-sk8-object (second objobj)
                           (format nil "Cannot define a handler (~s) on a non-object (~s)" handler-name obj))
                     (list 'maybe-make-own-object-class (second objobj))))
           (compiler-let ((ccl::*nx-method-warning-name*
                           (list ',handler-name
                                 ,@(mapcar #'(lambda (x) `',x) qualifiers)
                                 ',specializers)))
             (let* ((,funvar ,function-form)
                    (,methvar
                     (ccl::%defmethod ,funvar
                                      ,specializers-form
                                      ',qualifiers
                                      ,@(if (or method-class documentation) `(',method-class))
                                      ,@(if documentation `(,documentation)))))
               ,@(when (eq objobj obj) ;; when a built-in-class pseudo-object
                   `((ccl::%defmethod ,funvar
                                     ,(list* (first specializers-form) `(list 'eql ,obj)
                                              (cddr specializers))
                                     ',qualifiers
                                     ,@(if (or method-class documentation) `(',method-class))
                                     ,@(if documentation `(,documentation)))))
               ,@(unless hidden-p
                   `((set-sk8-handler-p ,methvar)
                     ,@(when (symbolp handler-name)
                         (if private-p
                           `((make-private-property ,obj ',handler-name))
                           `((when (private-property-p ,obj ',handler-name)
                               (make-public-property ,obj ',handler-name))
                             (publish-project-symbol ',handler-name (sk8:project ,obj)))))))
               (maybe-update-pseudo-descendant-methods (method-generic-function ,methvar) ,obj)
               ,methvar)
             ))))))

(defun update-pseudo-child-method (genericFunction destClass)
  (declare (special *object-pseudo-children-and-precedence-lists*))
  (let ((existingDestClassMethod (find-primary-method-on-obj genericFunction destClass))
        (destPrecedenceList (third (assq destClass *object-pseudo-children-and-precedence-lists*)))
        (fromMethod nil))
    (if (null existingDestClassMethod)
      
      ;; There's no existingDestClassMethod.  Walk up the precedence list...
      (dolist (ancestor destPrecedenceList)
        (unless (clos-class-shadow-p ancestor) (setq ancestor (class-of ancestor)))
        (when (setq fromMethod (find-primary-method-on-obj genericFunction ancestor))
          ;; Stop when we find a method; mirror that method UNLESS destClass truly inherits from ancestor
          (unless (ccl::subclassp destClass ancestor)
            (mirror-method genericFunction fromMethod destClass))
          (return)))
      
      ;; There IS an existingDestClassMethod...
      (let* ((existingDestClassLfun (method-function existingDestClassMethod))
             (existingDestClassMethodObj (get-handler-object-from-lfun existingDestClassLfun)))
        ;; When the existingDestClassMethod actually belongs to the destClass, nothing needs updating
        (unless (eq existingDestClassMethodObj destClass)
          ;; Walk up the precedence list -- if no fromMethod is found, just remove the existingDestClassMethod
          (dolist (ancestor destPrecedenceList  (remove-method genericFunction existingDestClassMethod))
            (unless (clos-class-shadow-p ancestor) (setq ancestor (class-of ancestor)))
            (when (setq fromMethod (find-primary-method-on-obj genericFunction ancestor))
              ;; Stop when we find a method
              (cond
               ;; When destClass truly inherits from ancestor, remove the existingDestClassMethod
               ((ccl::subclassp destClass ancestor)
                (remove-method genericFunction existingDestClassMethod))
               ;; Otherwise, mirror that method UNLESS the existingDestClassMethod already mirrors it
               ((neq existingDestClassLfun (method-function fromMethod))
                (mirror-method genericFunction fromMethod destClass)))
              (return))))))))

;;; Called after [re]defining a handler, and after removing a handler
;;;
(defun maybe-update-pseudo-descendant-methods (genericFunction obj)
  (declare (special *object-pseudo-children-and-precedence-lists*))
  (when (clos-class-shadow-p obj) (update-pseudo-child-method genericFunction obj))
  (let ((pseudoChildrenClasses (second (assq obj *object-pseudo-children-and-precedence-lists*))))
    ;; Attempt to update each of the pseudo-children
    (dolist (pseudoChildClass pseudoChildrenClasses)
      (maybe-update-pseudo-descendant-methods genericFunction pseudoChildClass))))

;;; delete-handler -- deletes handler
;;;   handler - name of handler (either a symbol or a (setf name) cons) or the handler object itself
;;;   object - object from which to delete handler. If object is not supplied, then all handlers of given name are deleted
;;;   type - handler type is either :after or :before

(defun delete-handler (handler &key object type (register t))
  (unless (memq type *valid-macframes-handler-types*)
    (error "Invalid handler type ~s... must be one of ~a" type *valid-macframes-handler-types*))
  (let* ((method-p (ccl::standard-method-p handler))
         (function-name (if method-p
                          (method-name handler)
                          handler))
         (setter (listp function-name))
         (sym (if setter (cadr function-name) function-name))
         (function (if method-p
                     (method-generic-function handler)
                     (fboundp handler)))
         (lfun (if (handlerp handler) (method-function handler))))
    
    (unless method-p
      (unless object (error "You must supply an object to find handler ~a of type ~a" function-name type))
      (setq handler (find-local-handler handler object)))
    
    (labels ((remove-handler (f h obj)
               (let* ((normalClass (and (ccl::standard-instance-p obj) (not (clos-class-shadow-p obj)) (class-of obj)))
                      (slot-exists-p (and normalClass (assq sym (class-direct-instance-slots normalClass)))))
                 (remove-method f h)
                 (maybe-update-pseudo-descendant-methods function obj)
                 (when slot-exists-p
                   (add-SK8-accessors normalClass sym
                                      (let ((v (slot-value-no-error obj sym)))
                                        (when (dynamic-slotvalue-inheriter-p v) v))
                                      (if setter :setter :getter)))
                 (when register
                   (PS::purgeHandler function-name obj type)))))
      (declare (dynamic-extent #'remove-handler))
      
      (when function
        
        (if object
          (if handler
            (remove-handler function handler object)
            (error "Object ~s does not have handler ~a" object function-name))
          (if handler
            (remove-handler function handler (get-handler-object-from-lfun lfun))
            (let (lfun)
              (dolist (h (ccl::%gf-methods function))
                (setq lfun (method-function h))
                (remove-handler function h (get-handler-object-from-lfun lfun))))))
        
        (unless (ccl::%gf-methods function)
          (if (and (symbolp function-name)
                   (getf (symbol-plist function-name) ':multifbound))
            (fmakunboundmultiple function-name function)
            (fmakunbound function-name))))
      t)))

(defun fmakunboundmultiple (sym function &optional (name (symbol-name sym)))
  (when (and sym
             (fboundp sym)
             (eq function (symbol-function sym)))
    (fmakunbound sym)
    (mark-sym-not-function sym)
    (dolist (p (sk8::requiringProjects (ccl::package-project (symbol-package sym))))
      (fmakunboundmultiple (find-symbol name (sk8::package p))
                           function))))


(defun delete-function (name &key (project nil) (register t))
  (when (ccl::standard-generic-function-p (fboundp name))
    (error "~a is a handler, not a function!" name))
  (let ((p (get-function-project (symbol-function name))))
    (when (or (null project)
              (if p
                (eq project p)
                (error "SK8 ERROR: Can't find function ~a's project!" name)))
      (cond
       ((and (symbolp name)
             (get name :multifbound))
        (fmakunboundmultiple name (fboundp name)))
       (t
        (fmakunbound name)
        (mark-sym-not-function name)))
      (if register (PS::purgeFunction project name))
      t)))

(defun delete-variable (name project &key constant (register t))
  (let (value)
    (unless project (error "SK8 ERROR: Can't find ~:[variable~;constant~] ~a's project!" constant name))
    (when constant
      (unless (constant-symbol-p name) (error "~a is not a constant" name))
      (unless (boundp name) (error "a is not a bound constant" name))
      (if (and (typep (setq value (symbol-value name)) 'sk8::object)
               (mf::named-object? value)
               (eq (sk8::sk8_id value) name))
        (error "You cannot remove of constant ~a because it is bound to object ~-1*~a. Unname the object if you want to remove this constant."
               name)))
    (let ((ccl::*warn-if-redefine-kernel* nil))
      (makunbound name))
    (mark-sym-not-global name)
    (when register (PS::purgeGlobal project name :constant constant))
    t))

(defun delete-constant (name project &key (register t))
  (delete-variable name project :constant t :register register))

;; make-load-form (CLtL, pg 659) is called by the Common Lisp routines that write objects out to FASL files
;; Define it on an object to tell MCL how to deal with the object
;; It returns two values, each of which is a list whose first element is a symbol that can be applied to the rest
;; The first value when applied should allocate storage for the object, returning the allocated object
;; The second value, when applied, has the side effect of filling in the contents of the object
;; The two stage process allows it all to work when there is a circular reference

;;; MAKE-LOAD-FORM for SK8 objects.
;;;
(defmethod make-load-form ((me sk8:object))
  (let ((myId (sk8::sk8_id me)))
    (when (and myId (symbolp myId))
      `(symbol-value ',myId))))

(defmethod make-load-form ((me built-in-class))
  `(find-class ',(slot-value me 'ccl::name) t))

;;; load form for a class object looks up its name if it has one, else looks up the name of the associated
;;; SK8 object if it has one, otherwise we can't do it. Notice how this has much the same code as make-load-form
;;; of a SK8 object in the second case, since it is doing the same thing then getting class-of the result.
;;;
(defmethod make-load-form ((me standard-class))
  (let ((name (slot-value me 'ccl::name)))
    (if (symbol-package name)
      `(find-class ',name t)
      (let* ((myowner (safe-class-owner me))
             (myObjId (and myowner (sk8::sk8_id myowner))))
        (if (and myObjId (symbolp myObjId))
          (when (eq myowner (sk8:project myowner))
            `(class-of (symbol-value ',myObjId)))
          (error "Can't compile reference to anonymous class ~s without owner object" me))))))

;; built-in object types
;; list array vector

(defmethod clos-class-shadow-p ((me ccl::built-in-class)) (declare (ignore me)) t)
(defmethod clos-class-shadow-p (me) (declare (ignore me))  nil)
;; default case for knownchildren
(defmethod knownchildren ((me ccl::built-in-class) &key) (declare (ignore me)) nil)

(defun make-macro-list-from-list (l)
  (let ((res '(list)))
    (dolist (i l)
      (setf res (append res (list i))))
    res))
;;(make-macro-list-from-list (list vector array))

(defmacro sk8dev::make-clos-class-shadow (class-name desired-name &rest parent-names)
  (when (and (listp class-name) (eq (car class-name) 'quote))
    (setf class-name (second class-name)))
  (let ((desired-symbol (find-symbol (string-upcase desired-name) :sk8)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (when (and (boundp '*build-in-progress-p*) *build-in-progress-p*
                    (constantp ',desired-symbol) (eq ,desired-symbol *undefined*))
           (let ((ccl::*warn-if-redefine-kernel* nil))
             (makunbound ',desired-symbol))))
       (defparameter ,desired-symbol (find-class ',class-name t))
       (defmethod sk8:objectName ((me (eql ,desired-symbol)) &key) ,desired-name)
       (defmethod sk8::sk8_id ((me (eql ,desired-symbol))) ',desired-symbol)
       (defmethod sk8::sk8_flags ((me (eql ,desired-symbol))) nil)
       (defmethod sk8:parents ((me (eql ,desired-symbol)) &key)
         (mapcar #'(lambda (p)
                     (let ((pp (and (symbolp p) (boundp p) (symbol-value p))))
                       (if (or (object-class-p pp) (clos-class-shadow-p pp))
                         pp
                         (error "~a is not a SK8 object" p))))
                 ',parent-names))
       (defmethod sk8:parents ((me ,class-name) &key) '(,desired-symbol))
       (defmethod sk8:knownchildren ((me (eql ,desired-symbol)) &key) nil)
       (defmethod sk8:project ((me (eql ,desired-symbol)) &key) sk8)
       ,@(let ((res nil))
           (dolist (p parent-names (nreverse res))
             (push
              `(if (object-class-p ,p)
                 (push ,desired-symbol (slot-value ,p 'sk8:knownchildren))
                 (defmethod sk8:knownchildren ((me (eql ,p)) &key) 
                   ,(make-macro-list-from-list (remove-duplicates (cons desired-symbol (copy-list (sk8:knownchildren (symbol-value p))))))))
              res)))
       ,desired-symbol)))

(defvar *SK8-PACKAGE* (find-package :sk8))

(defmacro SK8:define-sk8-var (name &key (register nil) (project SK8:SK8) initial-value private?
                                      varValueType handlerobject handlernamesymbol
                                      handlerqualifier documentation)
  (declare (ignore documentation handlerqualifier handlernamesymbol handlerobject
                   varvaluetype register))
  `(progn
     ,(unless private?
        `(publish-project-symbol ',name ,project))
     (defparameter ,name ,initial-value)))


;; get rid of this when #~ is removed from all the sources
(set-dispatch-macro-character #\# #\~ #'(lambda (stream ch n)
                                          (declare (ignore ch n))
                                          (read stream t nil t))
                              *readtable*)

(defun SK8::define-sk8-var* (name &key register ((:project proj) nil) initial-value private? locked
                                      varValueType handlerobject handlernamesymbol
                                      handlerqualifier documentation)
  (unless proj (error "No project provided!"))
  (mark-sym-global name)
  (unless private?
    (publish-project-symbol name proj))
  (ccl::%defparameter name initial-value)
  (when register
    (ps::saveglobal project name initial-value private? locked varValueType handlernamesymbol handlerobject handlerqualifier documentation :constant nil))
  t)

(defmacro sk8:define-system-handler (handler-name option-or-args &body body)
  `(sk8:define-handler ,handler-name ,option-or-args ,@body))

(defun record-project-object-in-lfun (closure proj)
  (let ((plist (ccl::%lfun-info (ccl::closure-function closure))))
    (setf (getf (cddr plist) 'projectObject) proj)))

(defun get-function-project (closure)
  (getf (ccl::%lfun-info (ccl::closure-function closure)) 'projectObject))

(defmacro sk8:define-sk8-function (name private? &rest r)
  `(prog1
     (defun ,name ,@r)
     ,(if private?
        `(mark-sym-function ',name)
        `(publish-function-symbol ',name SK8:SK8))
     (record-project-object-in-lfun (symbol-function ',name) SK8:SK8)))

(defmacro sk8:define-sk8-constant (name val &rest r)
  (declare (ignore r))
  (require-type name 'symbol)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (when (and (boundp '*build-in-progress-p*) *build-in-progress-p*
                  (constantp ',name) (eq (locally (declare (special ,name)) ,name) *undefined*))
         (let ((ccl::*warn-if-redefine-kernel* nil))
           (makunbound ',name))))
     (defconstant ,name ,val)))

;; functional form of define-sk8-constant macro with extra runtime stuff to declare symbol as public
(defun SK8::define-sk8-constant* (name value &key register ((:project proj) (ccl::package-project *package*))
                                          private? locked handlernamesymbol
                                          handlerobject handlerqualifier documentation)
  (mark-sym-global name)
  (unless private?
    (publish-project-symbol name proj))
  (when (and (boundp '*build-in-progress-p*) *build-in-progress-p* (constantp name) (eq (symbol-value name) *undefined*))
    (let ((ccl::*warn-if-redefine-kernel* nil))
      (makunbound name)))
  (CCL::%DEFCONSTANT name value)
  (when register
    (ps::saveglobal project name value private? locked nil handlernamesymbol handlerobject handlerqualifier documentation :constant t)))

(defmacro sk8::define-project-function (proj name private? &rest optionsAndLambdaListAndBody)
  (unless name (error "Name arg missing!"))
  (let (;;(old-function (make-symbol "OLD-FUNCTION"))
        ;;(breakpointVector nil)
        lambdaList
        item)
    (loop
      (cond
       ((listp (setq item (pop optionsAndLambdaListAndBody)))
        (setq lambdaList item)
        (return))
       ;;((vectorp item)
       ;; (setq breakpointVector item))
       ))
    `(progn
       (defun ,name ,lambdaList ,@optionsAndLambdaListAndBody)
       ,(if private?
          `(mark-sym-function ',name)
          `(publish-function-symbol ',name ,proj))
       (record-project-object-in-lfun (symbol-function ',name) ,proj)
       ',name)
    ))

(defmacro sk8dev::doheads (varinit &body body)
  (let ((var (first varinit))
        (init (second varinit)))
    `(do ((,var ,init ,(list 'cdr var)))
         ((null ,var))
       ,@body)))

(defmacro sk8dev::declare-not-global (args &body body)
  (declare (ignore args))
  (cons 'progn body))

(defmacro dovector-in-reverse ((element-var vector-form) &body body)
  (let* ((v (gensym "VECTOR-"))
         (index (gensym "INDEX-")))
    `(let* ((,v ,vector-form)
            (,index (length ,v))
            ,element-var)
       (loop
         (when (minusp (setq ,index (ccl::%i- ,index 1)))
           (return))
         (setq ,element-var (aref ,v ,index))
         ,@body))))

(defvar *sk8-major-version* 1)

(defvar *sk8-minor-version* 4) ;; set to 4 for MCL 3.0, WOOD 0.91 version

(defvar *sk8-little-version* 0)

;;; *quit-functions* -- List of argless functions to evaluate before MacFrames QUIT is executed
;;;

(defvar *quit-functions* nil)

(push #'(lambda() (dolist (f *quit-functions*)
                    (funcall f))) ccl::*lisp-cleanup-functions*)

;;; "Stubs" for a currently obsolete property of object 
;;;
(defmethod sk8:undisposable ((me SK8:Object) &key)
  (declare (ignore me))
  nil)

(defmethod (setf SK8:undisposable) (boolean (me SK8:Object))
  (declare (ignore boolean me))
  nil)


#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/10/96	sidney  	define define-handler in a way that the editor knows how to indent
						add project info in project functions and provide function to find it
	3  	 5/20/96	sidney  	
	4  	 5/20/96	sidney  	suppress error when makunbound a constant
	5  	 7/ 7/96	sidney  	changes for native PPC build
	6  	 9/30/96	sidney  	define-handler on a built in class should also define an eql handler on the class object
	7  	10/21/96	sidney  	make the cleanup functions do something again
	8  	11/22/96	Brian   	Fixing make-clos-class-shadow to handle knownchildren correctly.
	9  	12/16/96	Brian Roddy	Making define-sk8-var* always return true.
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
