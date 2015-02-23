;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;;;;;;;; from macframes;kernel;object-creation.lisp, just the creation relation parts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The internal representation of creationRelations:
;;;
;;;    The ":hidden" handler 'internal-creationRelations' returns an object's creationRelations -- a list of <creationRelation-chain>s.  Each
;;;    <creationRelation-chain> is of the form (which is tree-like, in order to be able to "left-factor" relation chains):
;;;
;;;       ( [<relationInfo>]+  [ ( [<creationRelation-chain>]+ ) ] )
;;;
;;;    Each of the traversal-paths from start to leaf may contain exactly one occurrence of the marker %begin-simple-relations% (in place
;;;    of a <relationInfo>; relations coming before the marker are "creation" relations and relations coming after the marker are "simple"
;;;    relations, i.e. relations that are maintained, but that don't force creation of new objects.
;;;
;;;    Each <relationInfo> is simply of the form:
;;;
;;;       ( <relationName> . <plural?> )
;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Low-level creationRelations access API...
;;;

(defvar *internal-localCreationRelations-table* (make-hash-table :test #'eq :weak t))
(push *internal-localCreationRelations-table* *all-weak-hash-tables*)


;;;  This method is the default "entry" in the internal-creationRelations "cache"
;;;
(defmethod internal-creationRelations ((me t))
  (declare (ignore me))
  nil)


(defun relationChain-starts-with? (propertyOrProperties relationChain)
  (when (listp (setq relationChain (car relationChain)))
    (if (listp propertyOrProperties)
      (memq (car (the list relationChain)) propertyOrProperties)
      (eq (car (the list relationChain)) propertyOrProperties))))


(defun refresh-internal-creationRelations-cache (affectedObject &key removedSlots)
  ;; If the has-creationRelations? flag must change, change it for affectedObject and all its descendants
  (if (or (gethash affectedObject *internal-localCreationRelations-table*) ; (must check the table and the method since
          (internal-creationRelations affectedObject))                     ;  refresh may be due to a change in parents)
    (unless (%object-has-creationRelations? affectedObject)
      (set-%object-has-creationRelations? affectedObject)
      (map-all-descendants affectedObject #'set-%object-has-creationRelations?))
    (when (%object-has-creationRelations? affectedObject)
      (clear-%object-has-creationRelations? affectedObject)
      (map-all-descendants affectedObject #'clear-%object-has-creationRelations?)))
  ;; Collect all the affected objects (and their corresponding localCreationRelations)
  (let ((affectedObjectsAndRelations nil))
    (flet ((collector (obj sk8::localCreationRelations)
             (when (or (eq obj affectedObject) (is-a obj affectedObject))
               (push (cons obj sk8::localCreationRelations) affectedObjectsAndRelations))))
      (declare (dynamic-extent #'collector))
      (maphash #'collector *internal-localCreationRelations-table*)
      ;; If the affectedObject has no local creationRelations, it must need an update due to a change in parents
      (when (and (not (assq affectedObject affectedObjectsAndRelations))
                 (neq affectedObject Object))
        (push (cons affectedObject nil) affectedObjectsAndRelations))
      ;; When slots have been removed, remove any relationChains associated with those slots
      (when removedSlots
        (let (relations)
          (dolist (objectAndRelations affectedObjectsAndRelations)
            (declare (cons objectAndRelations))
            (setq relations (delete removedSlots (cdr objectAndRelations) :test #'relationChain-starts-with?))
            (setf (cdr objectAndRelations) relations)
            (if relations
              (setf (gethash (car objectAndRelations) *internal-localCreationRelations-table*) relations)
              (remhash (car objectAndRelations) *internal-localCreationRelations-table*)))
          (setq affectedObjectsAndRelations (delete-if #'null affectedObjectsAndRelations :key #'cdr))))
      ;; Empty out all the affected methods from #'internal-creationRelations
      (let ((gf #'internal-creationRelations)
            class obj)
        (dolist (m (generic-function-methods gf))
          (setq class (first (method-specializers m)))
          (unless (eq class CCL::*t-class*)
            (setq obj (class-owner class))
            (when (and (CCL::standard-object-p obj)
                       (or (eq obj affectedObject)
                           (assq obj affectedObjectsAndRelations)
                           (is-a obj affectedObject)))
              (remove-method gf m)))))
      ;; Insert recomputed methods...
      (let ((*record-source-file* nil)
            (*save-local-symbols* t)  ;;*** this has to be t so we have something in the lfun-info!!!
            (*save-definitions* nil)
            obj obj-relations ancestor-relations)
        (dolist (objectAndRelations affectedObjectsAndRelations)
          (declare (cons objectAndRelations))
          (setq obj (car objectAndRelations))
          (setq obj-relations (nreverse (copy-list (cdr objectAndRelations))))
          (dolist (ancestor (cdr (class-precedence-list (class-of obj))))
            (when (and (neq ancestor CCL::*t-class*)
                       (CCL::standard-object-p (setq ancestor (class-owner ancestor))))
              (when (eq ancestor Object) (return))
              (when (setq ancestor-relations (internal-localCreationRelations ancestor))
                (dolist (relation ancestor-relations)
                  (pushnew relation obj-relations :test #'equal)))))
          ;*** It would be nicer if someday we could make this work without invoking eval!!!
          (eval `(SK8::define-handler internal-creationRelations :hidden (,obj)
                                      (declare (ignore-if-unused me))
                                      ',(nreverse obj-relations))))))))


(defun internal-localCreationRelations (obj)
  (gethash obj *internal-localCreationRelations-table*))


(defun (setf internal-localCreationRelations) (relations obj)
  ;; Register the new relations with the object
  (if relations
    (setf (gethash obj *internal-localCreationRelations-table*) relations)
    (remhash obj *internal-localCreationRelations-table*))
  ;; Since the caching is done with methods, the object must be made "specialized" before getting a method
  (maybe-make-own-object-class obj)
  ;; Refresh the internal creationRelations cache (and flags)
  (refresh-internal-creationRelations-cache obj)
  relations)


(defun restore-saved-creationRelations (objectsAndRelations)
  (when objectsAndRelations
    (dolist (objectAndRelations objectsAndRelations)
      (setf (internal-localCreationRelations (car objectAndRelations))
            (cdr (the cons objectAndRelations))))
    ))


(defun maybe-update-creationRelations-due-to-removed-slots (affectedObject objClass-or-removedSlot
                                                                                    &optional removedSuperclasses)
  (let ((removedSlots nil))
    (if (symbolp objClass-or-removedSlot)
      ;; The removedSlot was specified
      (setq removedSlots objClass-or-removedSlot)
      ;; The objClass & removedSuperclasses were specified; we need to figure out what slots (if any) were removed
      (when removedSuperclasses
        (let* ((newClass objClass-or-removedSlot)
               (newClassSlots (cdr (CCL::%class-slots newClass))))
          (dolist (oldSuperclass (if (listp removedSuperclasses)
                                   removedSuperclasses
                                   (list removedSuperclasses)))
            (dovector (slotd (cdr (CCL::%class-slots oldSuperclass)))
              (unless (find (car slotd) newClassSlots :key #'car :test #'eq)
                (pushnew (car slotd) removedSlots :test #'eq)))))))
    ;; If some slots were really removed, decide whether any objects with creationRelations are affected
    (when removedSlots
      (let ((trulyAffectedObject nil))
        (flet ((findAffectedObject (o sk8::localCreationRelations)
                 (when (and (or (eq o affectedObject) (is-a o affectedObject))
                            (member removedSlots sk8::localCreationRelations :test #'relationChain-starts-with?)
                            (or (null trulyAffectedObject)
                                (is-a trulyAffectedObject o)))
                   (setq trulyAffectedObject o))))
          (declare (dynamic-extent #'findAffectedObject))
          (maphash #'findAffectedObject *internal-localCreationRelations-table*))
        ;; If any objects with creationRelations are affected, refresh the cache taking into account the removedSlots
        (when trulyAffectedObject
          (refresh-internal-creationRelations-cache trulyAffectedObject :removedSlots removedSlots)
          t)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Support code for converting between internal and "pretty" representations of creationRelations...
;;;

(defun left-factor-creationRelations (relationChains &optional single-if-possible)
  (let ((factoredRelationChains nil)
        relationName factorables)
    (declare (list factoredRelationChains factorables))
    (dolist (chain relationChains)
      (setq relationName (if (listp chain) (first (the list chain)) chain))
      (unless (assq relationName factoredRelationChains)
        (setq factorables (remove relationName relationChains :test #'(lambda (name otherChain)
                                                                        (neq name (if (listp otherChain)
                                                                                    (first (the list otherChain))
                                                                                    otherChain)))))
        (map-into factorables #'(lambda (chain) (if (listp chain) chain (list chain))) factorables)
        (cond
         ((null (cdr factorables))
          (push (first factorables) factoredRelationChains))
         (t
          (setq factorables (delete-if-not #'cdr factorables))
          (cond
           ((null (cdr factorables))
            (push (first factorables) factoredRelationChains))
           (t
            (map-into factorables #'cdr factorables)
            (push (cons relationName (list (left-factor-creationRelations factorables t)))
                  factoredRelationChains)))))))
    (if (null (cdr factoredRelationChains))
      (if single-if-possible (first factoredRelationChains) factoredRelationChains)
      (nreverse factoredRelationChains))))


(defun compute-true-relation-names (relationChains)
  (let (relationName)
    (dolist (chain relationChains)
      (loop
        (unless chain (return))
        (setq relationName (first chain))
        (if (listp relationName)
          (compute-true-relation-names relationName)
          (let* ((str (symbol-name relationName))
                 (len (length str))
                 (end (1- len))
                 (symStart nil)
                 (symEnd nil)
                 (simple? nil)
                 (num %singular%))
            (declare (fixnum len end))
            (when (> len 1)
              (if (eq (char str end) #\*)
                (setq num %plural%
                      symStart 0
                      symEnd end)
                (when (and (eq (char str 0) #\[)
                           (eq (char str end) #\]))
                  (setq simple? t
                        symStart 1
                        symEnd end)
                  (when (eq (char str (the fixnum (1- end))) #\*)
                    (setq num %plural%
                          symEnd (1- symEnd))))))
            (when symStart
              (let ((symPkg (symbol-package relationName)))
                (publish-project-symbol relationName (if (CCL::package-project symPkg)
                                                       symPkg
                                                       (progn (import relationName *SK8-package*) *SK8-package*)))
                (setq relationName (intern-symbol (subseq (pretty-symbol-name relationName) symStart symEnd) symPkg))))
            (setf (first (the list chain))
                  (cons relationName
                        (if simple? (list num) num)))))
        (setq chain (cdr (the list chain)))))))


(defun mark-simpleRelations (relationChains &optional pastFirstSimple?)
  (let (simpleStartPos)
    (loop
      (unless relationChains (return))
      (setq simpleStartPos nil)
      (dolist (relation (first relationChains))
        (if (listp (car relation))
          (mark-simpleRelations relation simpleStartPos)
          (when (consp (cdr relation))
            (when (and (not simpleStartPos) (not pastFirstSimple?))
              (setq simpleStartPos (position relation (first (the list relationChains)) :test #'eq)))
            (setf (cdr (the list relation)) (car (the list (cdr (the list relation))))))))
      
      (when simpleStartPos
        (if (eql simpleStartPos 0)
          (push %begin-simple-relations% (first (the list relationChains)))
          (let ((prevCdr (nthcdr (1- simpleStartPos) '(a b))))
            (push %begin-simple-relations% (cdr prevCdr)))))
      (setq relationChains (cdr (the list relationChains))))))


(defun compute-pretty-relation-names (relationChains &optional pastFirstSimple?)
  (let (pastSimple? relationName)
    (dolist (chain relationChains)
      (setq pastSimple? pastFirstSimple?)
      (dolist (relation chain)
        (cond
         ((eq relation %begin-simple-relations%) (setq pastSimple? t))
         (t
          (setq relationName (car relation))
          (if (listp relationName)
            (compute-pretty-relation-names relation pastSimple?)
            (let ((str (pretty-symbol-name relationName))
                  (symPkg (symbol-package relationName)))
              (declare (fixnum len end))
              (unless (CCL::package-project symPkg) (setq symPkg *SK8-package*))
              (if (eq (cdr (the list relation)) %plural%)
                (setq relationName (intern-symbol (if pastSimple?
                                                    (concatenate 'string "[" str "*]")
                                                    (concatenate 'string str "*"))  *SK8-package*))
                (when pastSimple?
                  (setq relationName (intern-symbol (concatenate 'string "[" str "]")  *SK8-package*))))
              (setf (car (the list relation)) relationName)))))))))


(defun unfactor-creationRelations (relationChains)
  (let ((unfactoredChains nil)
        reverseCurrentChain relationName usedCurrentChain?)
    (dolist (chain relationChains)
      (setq reverseCurrentChain nil
            usedCurrentChain? nil)
      (dolist (relation chain)
        (unless (eq relation %begin-simple-relations%)
          (setq relationName (car relation))
          (cond
           ((listp relationName)
            (setq usedCurrentChain? t)
            (dolist (unfactoredChainTail (unfactor-creationRelations relation))
              (dolist (r reverseCurrentChain) (push r unfactoredChainTail))
              (push unfactoredChainTail unfactoredChains)))
           (t
            (push relationName reverseCurrentChain)))))
      (when (not usedCurrentChain?)
        (push (if (null (cdr reverseCurrentChain))
                (first (the list reverseCurrentChain))
                (nreverse reverseCurrentChain)) unfactoredChains)))
    (nreverse unfactoredChains)))


;;; Must build a new list
(defun pretty-to-internal-creationRelations (relations-list)
  ;; Left factor
  (setq relations-list (left-factor-creationRelations relations-list))
  ;; Turn the symbols into relation representations
  (compute-true-relation-names relations-list)
  ;; Mark chains with %begin-simple-relations% markers where applicable
  (mark-simpleRelations relations-list)
  relations-list)


;;; Must build a new list
(defun internal-to-pretty-creationRelations (relations-list)
  (setq relations-list (copy-tree relations-list))
  ;; Turn the relation representations into "pretty" symbols
  (compute-pretty-relation-names relations-list)
  ;; Un-factor
  (unfactor-creationRelations relations-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some "introspective" capabilites for relations...
;;;

;;; Returns true if this property of the object is a creationRelation (*not* a simpleRelation)
;;;
(defun creation-relation-p (obj property)
  (when (%object-has-creationRelations? obj)
    (let (relation)
      (dolist (relationChain (internal-creationRelations obj))
        (unless (eq (setq relation (first relationChain)) %begin-simple-relations%)
          (when (eq property (first relation)) (return t)))))))



;;; Returns two values:  (1) the related object  (2) the relation through which it points to the given object
;;; or nil if no relation was found
;;;
(defun find-relation (obj &optional noPrivateProperties)
  (flet ((find-creationRelation (fromObject targetObject)
           (let (relation relationName other)
             (dolist (relationChain (internal-creationRelations fromObject))
               (unless (eq (setq relation (first relationChain)) %begin-simple-relations%)
                 (setq relationName (first relation))
                 (setq other (slot-value-no-error fromObject relationName))
                 (when (dynamic-slotvalue-inheriter-p other)
                   (setq other (%slotvalue-inheriter-value other)))
                 (when (cond
                        ((not (plural-relation? relation)) (eq targetObject other))
                        ((listp other)                     (memq targetObject other))
                        ((vectorp other)                   (find targetObject other))
                        (t                                 (SK8::positionOfItem other targetObject)))
                   (return relationName)))))))
    (when (CCL::standard-instance-p obj)
      (let* ((slots (CCL::%MAYBE-FORWARDED-INSTANCE obj))
             (len (when slots (uvsize slots)))
             (i 0)
             other othersFlags othersRelations)
        (declare (fixnum len i))
        (when slots
          (loop
            (when (eql (incf i) len) (return))
            (setq other (CCL::%svref slots i))
            (when (dynamic-slotvalue-inheriter-p other)
              (setq other (%slotvalue-inheriter-value other)))
            (when (and (neq other obj)
                       (setq othersFlags (maybe-object-flags other))
                       (%flags-has-creationRelations? othersFlags)
                       (setq othersRelations (find-creationRelation other obj)))
              ;; If the property that we are going to use is private, do not use it.
              (unless (and noPrivateProperties (sk8::private other :property othersRelations))
                (return-from find-relation (values other othersRelations)))))
          
          (let ((flags (maybe-object-flags obj)))
            (when (and flags (logbitp %flags-has-ports? flags))
              (dolist (oport (SK8Dev::object-output-ports obj))
                (dolist (otherPort (SK8::wiredTo oport))
                  (unless (or (eq otherPort oport) (symbolp otherPort))
                    (when (and (setq other (SK8::portObject otherPort))
                               (neq other obj)
                               (setq othersFlags (maybe-object-flags other))
                               (%flags-has-creationRelations? othersFlags)
                               (setq othersRelations (find-creationRelation other obj)))
                      (return-from find-relation (values other othersRelations)))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Support functions for the make-object-and-creationGroup machinery...
;;;

(defun find-object-correspondenceInfo (originalObject crinfo)
  (let ((objects (creationRelationsInfo-objects crinfo)))
    (declare (list objects))
    (loop
      (unless (setq objects (cdr objects))
        (return nil))
      (when (eq (correspondenceInfo-originalObject (car objects)) originalObject)
        (return (car objects))))))

(defun not-real-property-error (objectCorrespondenceInfo propertyName)
  (let ((originalObject (correspondenceInfo-originalObject objectCorrespondenceInfo)))
    (SK8:SK8-error SK8:GeneralError
                   :strings '(nil " is not a real property of ")
                   :objects (list propertyName originalObject))))

(defun bad-plural-relation-value-error (objectCorrespondenceInfo value relationName)
  (let ((originalObject (correspondenceInfo-originalObject objectCorrespondenceInfo)))
    (SK8:SK8-error SK8:PropertyTypeMismatchError
                   :object value :ownerObject originalObject :propertyName relationName
                   :expectedType (list SK8:List SK8:Array))))

(defun not-simple-instantiable-error (obj targetProject)
  (New obj :project targetProject)
  ;; Since the object is not simple-instantiable, the above call should cause an error.
  ;; But just in case we get here (for some odd reason), raise an error to be sure.
  (SK8:SK8-error SK8:GeneralError
                 :strings '(nil " canÕt be instantiated via creationRelations")
                 :objects (list obj)))

(defun invalid-initializerArguments-error (obj)
  (SK8:SK8-error SK8:GeneralError
                 :strings '(nil " is not a valid initializerArguments collection")
                 :objects (list obj)))


(defun no-targetProject-error ()
  (SK8:SK8-error SK8:GeneralError
                 :strings '("Target project must be given in order to make a new object")
                 :objects nil))

(defun illegal-objectName-error (objectName &key reserved)
  (SK8:SK8-error SK8:GeneralError
                 :strings (if reserved
                            '(nil " canÕt be used as an objectName because itÕs a reserved identifier")
                            '(nil " canÕt be used as an objectName because the global already has a value"))
                 :objects (list objectName)))

(defun illegal-propertyName-error (propertyName &key reserved)
  (SK8:SK8-error SK8:GeneralError
                 :strings (if reserved
                            '(nil " canÕt be used as a property name because itÕs a reserved identifier")
                            '(nil " canÕt be used as a property name because itÕs an illegal identifier"))
                 :objects (list propertyName)))

(defun duplicate-propertyName-error (propertyName)
  (SK8:SK8-error SK8:GeneralError
                 :strings '("Duplicate property name " " in properties argument")
                 :objects (list propertyName)))

(defun invalid-property-option-error (thing propertyName)
  (SK8:SK8-error SK8:GeneralError
                 :strings '("Invalid option " " in specification of " " property")
                 :objects (list thing propertyName)))

(defun invalid-parent-error (obj)
  (SK8:SK8-error SK8:GeneralError
                 :strings (if (typep obj 'built-in-class)
                            '(nil " canÕt be combined with other parents")
                            '(nil " is not a valid parent"))
                 :objects (list obj)))

(defun non-propagatable-property-error (obj propertyName)
  (SK8:SK8-error SK8:GeneralError
                 :strings '("The property " " canÕt be propagatable because it is not a real property of ")
                 :objects (list propertyName obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The actual code for processing/maintaining creation relations (and simple relations)...
;;;

(defun process-singular-creationRelation (crinfo targetProject new? currentDepth fromObjectInfo relationName)
  (let* ((newFromObject (correspondenceInfo-newObject fromObjectInfo))
         (originalRelationValue (slot-value newFromObject relationName))
         (inheriter? (when (dynamic-slotvalue-inheriter-p originalRelationValue) originalRelationValue)))
    (when (eq originalRelationValue (ccl::%slot-missing-marker)) (not-real-property-error fromObjectInfo relationName))
    (when inheriter? (setq originalRelationValue (%slotvalue-inheriter-value originalRelationValue)))
    (let* ((correspondenceInfo (find-or-make-corresponding-object originalRelationValue crinfo targetProject new? currentDepth))
           (newRelationValue (correspondenceInfo-newObject correspondenceInfo)))
      (if inheriter?
        (localize-and-set-propagatable-value newFromObject relationName newRelationValue inheriter? T)
        (setf (slot-value newFromObject relationName) newRelationValue))
      correspondenceInfo)))

(defun process-singular-simpleRelation (crinfo fromObjectInfo relationName)
  (let* ((newFromObject (correspondenceInfo-newObject fromObjectInfo))
         (originalRelationValue (slot-value newFromObject relationName))
         (inheriter? (when (dynamic-slotvalue-inheriter-p originalRelationValue) originalRelationValue)))
    (when (eq originalRelationValue (ccl::%slot-missing-marker)) (not-real-property-error fromObjectInfo relationName))
    (when inheriter? (setq originalRelationValue (%slotvalue-inheriter-value originalRelationValue)))
    (let ((correspondenceInfo (find-object-correspondenceInfo originalRelationValue crinfo)))
      (when correspondenceInfo
        (let ((newRelationValue (correspondenceInfo-newObject correspondenceInfo)))
          (if inheriter?
            (localize-and-set-propagatable-value newFromObject relationName newRelationValue inheriter? T)
            (setf (slot-value newFromObject relationName) newRelationValue))
          correspondenceInfo)))))


;;; Will only work with a list, vector, or array.  Period.
;;;
(defun process-plural-creationRelation (crinfo targetProject new? currentDepth fromObjectInfo relationName remainingChain)
  (let* ((newRelationValueCollection nil)
         (newFromObject (correspondenceInfo-newObject fromObjectInfo))
         (originalRelationValueCollection (slot-value-no-error newFromObject relationName))
         (inheriter? (when (dynamic-slotvalue-inheriter-p originalRelationValueCollection) originalRelationValueCollection))
         correspondenceInfo)
    (when (eq originalRelationValueCollection (ccl::%slot-missing-marker)) (not-real-property-error fromObjectInfo relationName))
    (when inheriter? (setq originalRelationValueCollection (%slotvalue-inheriter-value originalRelationValueCollection)))
    
    (cond
     ;; A list...
     ((listp originalRelationValueCollection)
      (when originalRelationValueCollection
        (setq newRelationValueCollection (copy-list originalRelationValueCollection))
        (let ((current newRelationValueCollection))
          (loop
            ;; Get the object corresponding to the current item, and store it into the new list
            (setq correspondenceInfo (find-or-make-corresponding-object (car current) crinfo targetProject new? currentDepth))
            (setf (car (the list current)) (correspondenceInfo-newObject correspondenceInfo))
            ;; If there's more to the relation chain we're in, process the remainder of the chain on the current item
            (when remainingChain
              (process-creationRelation-chain crinfo targetProject new? currentDepth correspondenceInfo remainingChain))
            (when (null (setq current (cdr (the list current)))) (return))))))
     ;; A vector or array...
     (t
      (multiple-value-bind (newArray baseVector i endi) (copy-object-vector-or-array originalRelationValueCollection)
        (declare (fixnum i endi))
        (unless newArray (bad-plural-relation-value-error fromObjectInfo originalRelationValueCollection relationName))
        (setq newRelationValueCollection newArray)
        (loop
          (when (eql i endi) (return))
          ;; Get the object corresponding to the current item, and store it into the new array
          (setq correspondenceInfo (find-or-make-corresponding-object (CCL::%svref baseVector i) crinfo targetProject new? currentDepth))
          (setf (CCL::%svref baseVector i) (correspondenceInfo-newObject correspondenceInfo))
          ;; If there's more to the relation chain we're in, process the remainder of the chain on the current item
          (when remainingChain
            (process-creationRelation-chain crinfo targetProject new? currentDepth correspondenceInfo remainingChain))
          (incf i)))))
    
    (if inheriter?
      (localize-and-set-propagatable-value newFromObject relationName newRelationValueCollection inheriter? T)
      (setf (slot-value newFromObject relationName) newRelationValueCollection))))

;;; Will only work with a list, vector, or array.  Period.
;;;
(defun process-plural-simpleRelation (crinfo targetProject new? currentDepth fromObjectInfo relationName remainingChain)
  (let* ((newRelationValueCollection nil)
         (newFromObject (correspondenceInfo-newObject fromObjectInfo))
         (originalRelationValueCollection (slot-value-no-error newFromObject relationName))
         (inheriter? (when (dynamic-slotvalue-inheriter-p originalRelationValueCollection) originalRelationValueCollection))
         correspondenceInfo)
    (when (eq originalRelationValueCollection (ccl::%slot-missing-marker)) (not-real-property-error fromObjectInfo relationName))
    (when inheriter? (setq originalRelationValueCollection (%slotvalue-inheriter-value originalRelationValueCollection)))
    
    (cond
     ;; A list...
     ((listp originalRelationValueCollection)
      (when originalRelationValueCollection
        (setq newRelationValueCollection (copy-list originalRelationValueCollection))
        (let ((current newRelationValueCollection))
          (loop
            ;; Get the object corresponding to the current item, and store it into the new list
            (when (setq correspondenceInfo (find-object-correspondenceInfo (car current) crinfo))
              (setf (car (the list current)) (correspondenceInfo-newObject correspondenceInfo))
              ;; If there's more to the relation chain we're in, process the remainder of the chain on the current item
              (when remainingChain
                (process-simpleRelation-chain crinfo targetProject new? currentDepth correspondenceInfo remainingChain)))
            (when (null (setq current (cdr (the list current)))) (return))))))
     ;; A vector or array...
     (t
      (multiple-value-bind (newArray baseVector i endi) (copy-object-vector-or-array originalRelationValueCollection)
        (declare (fixnum i endi))
        (unless newArray (bad-plural-relation-value-error fromObjectInfo originalRelationValueCollection relationName))
        (setq newRelationValueCollection newArray)
        (loop
          (when (eql i endi) (return))
          ;; Get the object corresponding to the current item, and store it into the new array
          (when (setq correspondenceInfo (find-object-correspondenceInfo (CCL::%svref baseVector i) crinfo))
            (setf (CCL::%svref baseVector i) (correspondenceInfo-newObject correspondenceInfo))
            ;; If there's more to the relation chain we're in, process the remainder of the chain on the current item
            (when remainingChain
              (process-simpleRelation-chain crinfo targetProject new? currentDepth correspondenceInfo remainingChain)))
          (incf i)))))
    
    (if inheriter?
      (localize-and-set-propagatable-value newFromObject relationName newRelationValueCollection inheriter? T)
      (setf (slot-value newFromObject relationName) newRelationValueCollection))))

(defun process-creationRelation-chain (crinfo targetProject new? currentDepth fromObjectInfo relationChain)
  (declare (fixnum currentDepth))
  (let (relation relationName)
    (loop
      (setq relation (car relationChain)) ; do a safe car to ensure we've got a valid cons cell
      (locally (declare (list relationChain))
        (setq relationChain (cdr relationChain))
        ;; The %begin-simple-relations% marker means the remaining portion of this chain has to wait till later
        (when (eq %begin-simple-relations% relation)
          (push relationChain (correspondenceInfo-pendingRelationChains fromObjectInfo))
          (return))
        ;; If we've got a nested list, this (final) entry is a conjunction of subchains
        (when (listp (setq relationName (car relation)))
          (process-creationRelation-chains crinfo targetProject new? currentDepth fromObjectInfo relationName)
          (return))
        ;; We've got a plural relation; process-plural-creationRelation recurses to finish processing it
        (when (plural-relation? relation)
          (process-plural-creationRelation crinfo targetProject new? currentDepth fromObjectInfo relationName relationChain)
          (return))
        ;; We've got a singular relation; process it and continue!
        (setq fromObjectInfo (process-singular-creationRelation crinfo targetProject new? currentDepth fromObjectInfo relationName))
        (unless relationChain (return))
        (incf currentDepth)))))

(defun process-simpleRelation-chain (crinfo targetProject new? currentDepth fromObjectInfo relationChain)
  (declare (fixnum currentDepth))
  (let (relation relationName)
    (loop
      (setq relation (car relationChain)) ; do a safe car to ensure we've got a valid cons cell
      (locally (declare (list relationChain))
        (setq relationChain (cdr relationChain))
        ;; If we've got a nested list, this (final) entry is a conjunction of subchains
        (when (listp (setq relationName (car relation)))
          (process-simpleRelation-chains crinfo targetProject new? currentDepth fromObjectInfo relationName)
          (return))
        ;; We've got a plural relation; process-plural-simpleRelation recurses to finish processing it
        (when (plural-relation? relation)
          (process-plural-simpleRelation crinfo targetProject new? currentDepth fromObjectInfo relationName relationChain)
          (return))
        ;; We've got a singular relation; process it and continue!
        (setq fromObjectInfo (process-singular-simpleRelation crinfo fromObjectInfo relationName))
        (unless relationChain (return))
        (incf currentDepth)))))


(defun process-creationRelation-chains (crinfo targetProject new? currentDepth fromObjectInfo creationRelation-chains)
  (dolist (creationRelation-chain creationRelation-chains)
    (process-creationRelation-chain crinfo targetProject new? currentDepth fromObjectInfo creationRelation-chain)))

(defun process-simpleRelation-chains (crinfo targetProject new? currentDepth fromObjectInfo simpleRelation-chains)
  (dolist (simpleRelation-chain simpleRelation-chains)
    (process-simpleRelation-chain crinfo targetProject new? currentDepth fromObjectInfo simpleRelation-chain)))


;;; Should only be called when (%flags-has-ports-or-creationRelations? objectFlags), i.e when the object definitely has a creation group
;;; (the reason being that make-creationGroup may increment creationRelationsInfo-maxDepth which, strictly speaking, would be incorrect
;;; if the object didn't have a creation group).
;;;
(defun make-creationGroup (originalObject objectFlags correspondenceInfo crinfo targetProject new? currentDepth)
  (declare (fixnum currentDepth))
  (incf currentDepth)
  (when (> currentDepth (the fixnum (creationRelationsInfo-maxDepth crinfo)))
    (setf (creationRelationsInfo-maxDepth crinfo) currentDepth))
  (when (logbitp %flags-has-ports? objectFlags)
    (let (portCorrInfo)
      ;; InputPort doesn't have wiredTo as a real property, unlike the other ports, so we have a different set of
      ;; creationRelations for InputPorts and for OutputPorts/InputOutputPorts
      (dolist (originalPort (SK8Dev::object-input-ports originalObject))
        (setq portCorrInfo (find-or-make-corresponding-object originalPort crinfo targetProject new? currentDepth))
        (push *io-port-creationRelations-chain* (correspondenceInfo-pendingRelationChains portCorrInfo)))
      (dolist (originalPort (SK8Dev::object-output-ports originalObject))
        (setq portCorrInfo (find-or-make-corresponding-object originalPort crinfo targetProject new? currentDepth))
        (push *output-port-creationRelations-chain* (correspondenceInfo-pendingRelationChains portCorrInfo))
        (push *io-port-creationRelations-chain* (correspondenceInfo-pendingRelationChains portCorrInfo)))))
  (when (%flags-has-creationRelations? objectFlags)
    (process-creationRelation-chains crinfo targetProject new? currentDepth correspondenceInfo
                                     (internal-creationRelations originalObject))))


;;; This gets called recursively (via make-creationGroup --> process-creationRelation-chains --> ... --> find-or-make-corresponding-object)
;;; to do all the creating.
;;;
(defun make-object-and-creationGroup (originalObject crinfo targetProject new? currentDepth)
  (let* ((objectFlags (maybe-object-flags originalObject))
         (newObject (cond ((null objectFlags)
                           (copy-non-SK8-object originalObject))
                          (new?
                           (new-object-and-low-level-init originalObject targetProject))
                          (t
                           (copy-object-and-low-level-init originalObject targetProject nil))))
         (correspondenceInfo (make-correspondenceInfo originalObject newObject currentDepth)))
    (insert-into-crinfo-objectsList correspondenceInfo crinfo)
    (when (and objectFlags (%flags-has-ports-or-creationRelations? objectFlags))
      (make-creationGroup originalObject objectFlags correspondenceInfo crinfo targetProject new? currentDepth))
    correspondenceInfo))


(defun finish-relations-and-initialize-object (crinfo originalObject newObject initArgs targetProject new?)
  (let ((depth (creationRelationsInfo-maxDepth crinfo))
        pendingRelationChains)
    (declare (fixnum depth))
    ;; Enforce all pending relations (i.e. the simple ones -- those that don't force creation)...
    (dolist (correspondenceInfo (cdr (creationRelationsInfo-objects crinfo)))
      (when (setq pendingRelationChains (correspondenceInfo-pendingRelationChains correspondenceInfo))
        (process-simpleRelation-chains crinfo targetProject T (correspondenceInfo-creationDepth correspondenceInfo)
                                       correspondenceInfo pendingRelationChains)))
    (let ((objects (creationRelationsInfo-objects-youngest-first crinfo))
          (SS::!*targetProject* targetProject)
          prev currentHead correspondenceInfo)
      (declare (special SS::!*targetProject*) (cons objects prev currentHead))
      ;; Now initialize all the objects in the creationGroup...
      (loop ; iterate from the max depth down to the start depth, in order to initialize the deepest ones first
        (setq currentHead objects)
        (loop ; iterate over the remaining objects, initializing those at the current depth
          (setq prev currentHead
                currentHead (cdr currentHead))
          (when (null currentHead) (return))
          (setq correspondenceInfo (car currentHead))
          (when (eql (correspondenceInfo-creationDepth correspondenceInfo) depth)
            (setf (cdr prev) (cdr currentHead))
            (SK8::initialize (correspondenceInfo-newObject correspondenceInfo)
                             (correspondenceInfo-originalObject correspondenceInfo) new? NIL)))
        (when (eql (decf depth) *start-depth*) (return)))
      ;; Finally, initialize the primary object
      (SK8::initialize newObject originalObject new? initArgs)
      newObject)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; end creation relation stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 7/ 7/96	sidney  	changes for native PPC build
	3  	 7/25/96	sidney  	give classes of named sk8 objects meaningful names
	4  	10/18/96	sidney  	use correct package when referring to SK8 errors
	5  	 2/27/97	sidney  	light editing of comments to prepare for publication
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
