;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :skil)

(sk8::define-sk8-var sk8::floatlist)

(sk8::define-sk8-function sk8::updateTypeTablesFromForm nil (form)
  (skil::update-type-info-from-skil-form form)
  )
(sk8::define-project-macro sk8::sk8 sk8::addAbstractPropertyType nil (&rest args)
  (declare (ignore args))
  t
  )

;;;____________________________________
;;;Globals


(defparameter object-type-table (make-hash-table :test #'eq))
(defparameter global-type-table nil)
(defparameter function-type-table nil)
(defparameter handler-type-table (make-hash-table :test #'eq))
(defparameter type-error-marker (gensym))


;;;____________________________________
;;;Helper functions

(defun library-symbol-p (sym)
  (and sym (symbolp sym) (find-symbol (symbol-name sym) :sk8)))


(defun clear-type-information ()
  (setf object-type-table (make-hash-table :test #'eq))
  (setf global-type-table nil)
  (setf function-type-table nil)
  (setup-handler-table)
  )



(defun requires-casting (locationType ReturnType)
  (unless locationType (setf LocationType 'sk8::Object))
  (unless ReturnType (setf ReturnType 'sk8::Object))
  (if (eq locationtype 'sk8::floatlist) (setf locationType 'sk8::list))
  (if (eq ReturnType 'sk8::floatlist) (setf ReturnType 'sk8::list))
  (unless (and (symbolp locationType) (boundp locationType) (symbolp ReturnType) (boundp ReturnType))
    (error "Unbound ~a or ~a" locationType returnType))
  (if (and (eq locationtype 'sk8::integer)
           (eq returntype 'sk8::number))
    nil
    (and (neq locationType ReturnType)
         (neq locationType 'sk8::object)  ;shouldnt be necessary but (is-a list object) is returning nil *****
         (not (sk8::is-a (symbol-value returntype) (symbol-value locationType)))))
  )

(defun minimum-shared-ancestor (newtype oldtype)
  (unless newtype (setf newtype 'sk8::Object))
  (unless oldtype (setf oldtype 'sk8::Object))
  (if (eq newtype 'sk8::floatlist) (setf newtype 'sk8::list))
  (if (eq oldtype 'sk8::floatlist) (setf oldtype 'sk8::list))
  (cond
   ((and (memq newtype '(sk8::integer sk8::float sk8::number))
         (memq oldtype '(sk8::integer sk8::float sk8::number)))
    (cond
     ((eq oldtype newtype) 
      newtype)
     ((or (and (eq oldtype 'sk8::integer) (eq newtype 'sk8::number))
          (and (eq oldtype 'sk8::number) (eq newtype 'sk8::integer)))
      'sk8::integer)
     (t
      'sk8::number))
    )
   ((not (and (symbolp oldtype) (boundp oldtype) (symbolp newtype) (boundp newtype)))
    (error "Project must be loaded to do proper type induction"))
   (t
    (let ((newanc (sk8::ancestors (symbol-value newtype))))
      (dolist (i (sk8::ancestors (symbol-value oldtype)))
        (if (memq i newanc)
          (return-from minimum-shared-ancestor (sk8::sk8_id i))))
      'sk8::object)))
  )

(defun confirm-for-new-type (newType newTypeDefinite oldTypestructure)
  (if (lib-form-p newType) (setf newType (fourth newType)))
  (let* ((oldTypeDefinite (and oldTypestructure (listp oldTypestructure)))
         (oldType (if oldTypeDefinite (first oldTypestructure) oldTypestructure))
         (oktoisatest (and oldtype newtype 
                           (symbolp oldtype) (symbolp newtype)
                           (boundp oldtype) (boundp newtype)))
         (oldIsANew (if oktoisatest (sk8::is-a (symbol-value oldType) (symbol-value newType))))
         (newIsAOld (if oktoisatest (sk8::is-a (symbol-value newType) (symbol-value oldType))))
         (newTypeAsAStructure (if newTypeDefinite (list newType) newType))
         )
    (cond 
     ((not newType)
      oldTypestructure)
     ((not oldType)
      newTypeAsAStructure)
     ((and (equalp newtypedefinite oldTypeDefinite)
           (equalp newType oldType))
      newtype
      )
     ((and oldTypeDefinite 
           newTypeDefinite)
      ;;(if (eq newType oldType)
      (list newType)
      ;;  (list type-error-marker "Cant have two definite types which are different"))
      )
     (oldTypeDefinite
      (if newIsAold
        oldTypestructure
        (list type-error-marker "type which is definite prevents a superclass from being specified as a type")
        ))
     (newTypeDefinite
      (if oldIsAnew
        newTypeAsAStructure
        (list type-error-marker "type which is definite which is not a superclasss of other inferred type cannot be used")
        ))
     (t
      (cond
       (oldIsAnew
        newTypeAsAStructure)
       (newIsAOld
        oldTypestructure)
       (t
        (minimum-shared-ancestor newtype oldtype))))
     ))
  )

(defun confirm-args-for-new-type (newType newTypeDefinite oldTypestructure)
  (let (res returnval)
    (dotimes (n (length newtype))
      (setf res (confirm-for-new-type (nth n newType) (nth n newTypeDefinite) (nth n oldTypestructure)))
      (if (confirmed-type-p res)
        (setf returnval (nconc returnval (list res)))
        (return-from confirm-args-for-new-type res)))
    returnval)
  )


(defun confirmed-type-p (returnedVal)
  (not (and (listp returnedVal) (eq (first returnedVal) type-error-marker)))
  )


;;;____________________________________
;;;____________________________________
;;;Properties



(defun add-property-type (obj prop type definite)
  (if (lib-form-p obj) (setf obj (fourth obj)))
  (let* ((objentry (gethash obj object-type-table))
         (propentry (assoc prop objentry))
         returnval)
    (setf propentry (second propentry))
    (unless propentry 
      (push (list prop nil) (gethash obj object-type-table)))
    (setf returnVal (confirm-for-new-type type definite propentry))
    (if (confirmed-type-p returnval)
      (progn
        (setf (second (assoc prop (gethash obj object-type-table))) returnVal)
        (add-handler-type obj prop (list type) (list definite))
        (add-handler-type obj (list 'setf prop) (list type) (list definite))
        t)
      returnVal)))

(defun get-property-type (obj prop)
  (if (lib-form-p obj) (setf obj (fourth obj)))
  (let ((type (if (library-symbol-p obj)
                (first (get-function-type prop)) ;;want the return type, hence the property type
                (second (assoc prop (gethash obj object-type-table))))))
    (if (listp type) (first type) type)))

;;;____________________________________
;;;____________________________________
;;;Globals

(defun add-global-type (glob type definite)
  (let* ((globentry (assoc glob global-type-table))
         returnval)
    (setf globentry (second globentry))
    (unless globentry 
      (push (list glob nil) global-type-table))
    (setf returnVal (confirm-for-new-type type definite globentry))
    (if (confirmed-type-p returnval)
      (progn
        (setf (second (assoc glob global-type-table)) returnVal)
        t)
      returnVal)))


(defparameter sk8-global-table (list (list 'sk8::pi sk8::float) (list 'sk8::true 'sk8::boolean)))

(defun get-global-type (glob)
  (if (library-symbol-p glob)
    (second (assoc glob sk8-global-table))
    (let ((type (second (assoc glob global-type-table))))
      (if (listp type) (first type) type))))

;;;____________________________________
;;;____________________________________
;;;Functions

(defun add-function-type (func type definite)
  (let* ((funcentry (assoc func function-type-table))
         returnval)
    (setf funcentry (second funcentry))
    (unless funcentry 
      (push (list func nil) function-type-table))
    (setf returnVal (confirm-args-for-new-type type definite funcentry))
    (if (confirmed-type-p returnval)
      (progn
        (setf (second (assoc func function-type-table)) returnVal)
        t)
      returnVal)))


(defparameter sk8-core-handlers '((& (sk8::string (sk8::string sk8::string))
                                        (sk8::list (sk8::list sk8::list)))
                                     (sk8::container (sk8::actor sk8::actor))
                                     (sk8::contents (sk8::list sk8::actor))
                                     (sk8::boundsrect (sk8::floatlist sk8::actor))
                                     (sk8::framesize (sk8::floatlist sk8::actor))
                                     (sk8::size (sk8::floatlist sk8::actor))
                                     (sk8::location (sk8::floatlist sk8::actor))
                                     (sk8::boundsrectlist (sk8::floatlist sk8::actor))
                                     (sk8::framesizelist (sk8::floatlist sk8::actor))
                                     (sk8::sizelist (sk8::floatlist sk8::actor))
                                     (sk8::locationlist (sk8::floatlist sk8::actor))
                                     (sk8::fillcolor (sk8::renderer sk8::actor))
                                     (sk8::framecolor (sk8::renderer sk8::actor))
                                     (sk8::length (sk8::integer sk8::collection))
                                     (sk8::h (sk8::number))
                                     (sk8::v (sk8::number))
                                     (sk8::height (sk8::number sk8::actor))
                                     (sk8::width (sk8::number sk8::actor))
                                     (sk8::left (sk8::number sk8::actor))
                                     (sk8::right (sk8::number sk8::actor))
                                     (sk8::top (sk8::number sk8::actor))
                                     (sk8::bottom (sk8::number sk8::actor))
                                     (sk8::text (sk8::string sk8::actor))
                                     (sk8::textoffset (sk8::floatlist sk8::actor))
                                     (sk8::textlocation (sk8::symbol sk8::actor))
                                     (sk8::textcolor (sk8::renderer sk8::actor))))

;;note & is handled specially cause it can be of two types
(defparameter sk8-function-table (append sk8-core-handlers
                                            '((+ (sk8::number sk8::number sk8::number))
                                              (- (sk8::number sk8::number sk8::number))
                                              (* (sk8::number sk8::number sk8::number))
                                              (/ (sk8::number sk8::number sk8::number))
                                              (^ (sk8::number sk8::number sk8::number))
                                              (div (sk8::integer sk8::number sk8::number))
                                              (mod (sk8::integer sk8::number sk8::number))
                                              (and (sk8::boolean sk8::boolean sk8::boolean))
                                              (or (sk8::boolean sk8::boolean sk8::boolean))
                                              (xor (sk8::boolean sk8::boolean sk8::boolean))
                                              (not (sk8::boolean sk8::boolean))
                                              (= (sk8::boolean sk8::object sk8::object))
                                              (eq (sk8::boolean sk8::object sk8::object))
                                              (> (sk8::boolean sk8::number sk8::number))
                                              (< (sk8::boolean sk8::number sk8::number))
                                              (>= (sk8::boolean sk8::number sk8::number))
                                              (<= (sk8::boolean sk8::number sk8::number))
                                              (sk8::random (sk8::number sk8::number))
                                              (startsWith (sk8::boolean sk8::collection sk8::object))
                                              (endsWith (sk8::boolean sk8::collection sk8::object))
                                              (contains (sk8::boolean sk8::collection sk8::object))
                                              (is-a (sk8::boolean sk8::object sk8::object))
                                              (sk8::actorathvcoordinates (sk8::actor sk8::number sk8::number))
                                              (sk8::optionkeydown (sk8::boolean))
                                              (sk8::shiftkeydown (sk8::boolean))
                                              (sk8::commandkeydown (sk8::boolean))
                                              (insertatend (sk8::collection sk8::object sk8::collection))
                                              (insertinfront (sk8::collection sk8::object sk8::collection))
                                              (sk8::list (sk8::list))
                                              ;;;sk8 functions...some of these should be handlers...
                                              (sk8::quote (sk8::symbol))
                                              (quote (sk8::symbol))
                                              )))


(defparameter skil-operators '( + - * / SKIL::^ SKIL::div mod > < >= <= 
                                   SKIL::startswith SKIL::endswith SKIL::contains SKIL::is-a))


(defun get-function-return-type (func &optional obj)
  (let (type (functype (assoc func sk8-function-table)))
    (cond
     (functype
      (setf type (second functype)))
     (t
      (setf type (first (get-handler-type obj func)))
      ))
    (if (listp type) (first type) type)))

(defun get-function-type (func)
  (if (lib-form-p func) (setf func (fourth func)))
  (let ((sk8funlookup (second (assoc func sk8-function-table)))
        (projfunlookup (second (assoc func function-type-table))))
    
    (cond
     ((and (library-symbol-p func) sk8funlookup)
      sk8funlookup)
     (projfunlookup
      (mapcar #'(lambda (x) (if (listp x) (first x) x)) projfunlookup))
     (t
      (get-handler-type nil func)))))

(defun project-function-p (sym)
  (assoc sym function-type-table))

;;(get-function-return-type 'testproj::hud)
;;;____________________________________
;;;____________________________________
;;;Handlers

(defun setup-handler-table ()
  (let (currentType)
    (setf handler-type-table (make-hash-table :test #'eq))
    (dolist (i sk8-core-handlers)
      (setf currentType (second i))
      (setf (gethash (first i) handler-type-table) 
            (list (list (second currentType) 
                        (cons (first currentType)
                              (rest (rest currentType))))
                  )))
    ))

;;Only add a handler to the type table if it's nearest ancestor DOES NOT have the same signature.
;;there could be load order problems with this but they are unlikely ***
(defun handler-type-exists (obj hand type definite)
  (declare (ignore definite))
  (when (boundp obj)
    (let ((handentry (gethash hand handler-type-table))
          (ancs (sk8::ancestors (symbol-value obj)))
          closestAncestor)
      (dolist (i ancs)
        (setf closestAncestor (assoc (sk8::sk8_id i) handentry))
        (when closestAncestor (return)))
      (and closestAncestor
           (equalp type (mapcar #'(lambda (x) (if (listp x) (first x) x)) (second closestAncestor))))))
  )


(defun add-handler-type (obj hand type definite)
  (if (lib-form-p obj) (setf obj (fourth obj)))
  (let* ((handentry (gethash hand handler-type-table))
         (objentry (assoc obj handentry))
         returnval)
    (when (and (not objentry) (handler-type-exists obj hand type definite))
      (return-from add-handler-type t))
    (setf objentry (second objentry))
    (unless objentry 
      (push (list obj nil) (gethash hand handler-type-table)))
    (setf returnVal (confirm-args-for-new-type type definite objentry))
    (if (confirmed-type-p returnval)
      (progn
        (setf (second (assoc obj (gethash hand handler-type-table))) returnVal)
        t)
      returnVal)))

(defun find-most-general-handler-type (argslists)
  (let (currentGuy bestGuy)
    (setf bestGuy (first (first argslists)))
    (dolist (i (rest argsLists))
      (setf currentGuy (first i))
      (when (sk8::is-a (symbol-value bestGuy) (symbol-value currentGuy))
        (setf bestGuy currentGuy)))
    (if bestGuy
      (assoc bestGuy argslists)
      nil))
  )

(defun find-best-handler-type (obj argsLists)
  (unless obj (return-from find-best-handler-type (find-most-general-handler-type argslists)))
  (let (closest 
        (closestCount 9999)
        currentGuy
        distance
        (ancs (sk8::ancestors (symbol-value obj))))
    (dolist (i argsLists)
      (setf currentGuy (first i))
      (when (eq currentGuy obj) 
        (return-from find-best-handler-type i))
      (when (sk8::is-a (symbol-value obj) (symbol-value currentGuy))
        (setf distance (position (symbol-value currentGuy) ancs))
        (when (and distance (< distance closestCount))
          (setf closestCount distance)
          (setf closest currentGuy))))
    (if closest
      (assoc closest argslists)
      nil)))


(defun get-handler-type (obj hand)
  (if (lib-form-p hand) (setf hand (fourth hand)))
  (if (lib-form-p obj) (setf obj (fourth obj)))
  (if (and (library-symbol-p obj) (not (gethash hand handler-type-table)))
    (get-function-type hand)
    (let* ((type (reverse (gethash hand handler-type-table)))
           (best-hand-type (if (> (length type) 1)
                             (find-best-handler-type obj type)))
           res)
      (if best-hand-type
        (setf type best-hand-type)
        (setf type (first type)))
      (setf res (mapcar #'(lambda (x) (if (listp x) (first x) x)) (second type)))
      (when res (setf (cdr res) (cons (first type) (cdr res))))
      res)))


;;(get-handler-type 'ORGANIZERS:TeamView 'organizers::domousedown)
;;(get-handler-type 'ORGANIZERS:collageview 'organizers::domousedown)
;;(get-handler-type 'ORGANIZERS:LabelView 'organizers::domousedown)
;;(get-handler-type 'organizers::cbu 'organizers::fillcolor)
;;(get-function-type 'testproj::testfun)
;;(gethash 'organizers::fillcolor handler-type-table)
;;(gethash 'organizers::domousedown handler-type-table)
;;;____________________________________
;;;____________________________________
;;;____________________________________
;;;____________________________________
;;;  Type table building functions
;;;____________________________________
;;;____________________________________

(defun update-type-from-add-property (form)
  (let* ((proptypeLoc (position "TYPE" form :key #'(lambda (x) (if (symbolp x) (symbol-name x) "")) :test #'string=))
         (proptype (if proptypeLoc (nth (1+ proptypeLoc) form) 'sk8::Object))
         (curObjSym (second form))
         (propname (third form))
         )
    (if (lib-form-p proptype) (setf proptype (fourth proptype)))
    (if (lib-form-p curobjsym) (setf curobjsym (fourth curobjsym)))
    (if (and (listp propname) (eq (first propname) 'quote)) (setf propname (second propname)))
    (when proptype
      (add-property-type curObjSym
                         propname
                         proptype
                         t))))

(defun construct-type-and-definite-lists (varList)
  (let (typelist definitelist newtype newdefinite)
    (dolist (i varList)
      (if (and (listp i) (third i))
        (progn
          (if (lib-form-p (third i)) 
            (setf newtype (fourth (third i)))
            (setf newtype (third i)))
          (setf newtype newtype
                newdefinite t))
        (setf newtype nil
              newdefinite nil))
      (setf typelist (nconc typelist (list newtype)))
      (setf definitelist (nconc definitelist (list newdefinite))))
    (sk8::sk8-multivals typelist definitelist)))
    
(defun update-type-from-tagpart (form)
  (add-property-type (second form)
                     (second (fourth form))
                     'sk8::actor ;;;;;;(third form)
                     nil)
  )


(defun update-type-from-declareglobal (form)
  (let* ((glob (second form))
         (type (and (listp glob) (second glob))))
    (if (listp glob) (setf glob (first glob)))
    (when type
      (if (lib-form-p type) (setf type (fourth type)))
      (add-global-type glob type t)
      ))
  )

(defun update-type-info-from-skil-form (form)
  (cond 
   ((and (listp form)
         (lib-form-p (first form))
         (or (eq (fourth (first form)) 'sk8::addproperty)
             (eq (fourth (first form)) 'sk8::addAbstractPropertyType))
         )
    (update-type-from-add-property form))
   ((and (listp form)
         (lib-form-p (first form))
         (eq (fourth (first form)) 'sk8::TAGPART))
    (update-type-from-tagpart form))
   ((and (listp form)
         (eq (first form) 'script)
         (listp (second form))
         (eq (first (second form)) 'declareglobal))
    (update-type-from-declareglobal (second form)))
   ((and (listp form) 
         (eq (first form) 'script))
    (let* ((name (skil-get-key form 'name t))
           (inputs (skil-get-key form 'input t))
           (return-type (skil-get-key form 'return-type t))
           (firstarg (if (listp (first inputs)) (first (first inputs)) nil))
           handlerObject)
      (if (listp name) (setf name (first name)))
      (when name
        (if (eq firstarg 'sk8::me)
          (sk8::sk8-multival-bind (types defs) (construct-type-and-definite-lists (cons `(nil nil ,(first return-type)) (cdr inputs)))
            (setf handlerObject (second (first inputs)))
            (if (lib-form-p handlerObject) (setf handlerObject (fourth handlerObject)))
            (add-handler-type handlerObject
                              name
                              types
                              defs))
          (sk8::sk8-multival-bind (types defs) (construct-type-and-definite-lists (cons `(nil nil ,(first return-type)) inputs))
            (add-function-type name
                               types
                               defs))
          ))))))


#|

(defun print-all-type-info ()
  (dolist (k (sk8::keys object-type-table))
    (dolist (j (gethash k object-type-table))
      (print (cons k j))))
  (dolist (k (sk8::keys handler-type-table))
    (dolist (j (gethash k handler-type-table))
      (print (cons k j))))
  (dolist (k global-type-table)
    (print k))
  (dolist (k function-type-table)
    (print k))

  )

(print-all-type-info)

(java-it "on foo
     set {x,y} to {uiselection, uiselection}
     set z to x + y
end foo")
tagpart obj, obj2, 'pipo'

(trace construct-type-and-definite-lists)
(untrace)

(clear-type-information)
(gc)

(setf sk8::MaintainTypeTables t)
(get-function-type 'organizers::view)
(get-handler-type nil 'organizers::view)
(java-it "addproperty of rectangle, 'pipo'")
(java-it "addAbstractPropertyType of rectangle, 'pipo' with type oval")
(get-handler-type 'sk8::rectangle 'ORGANIZERS::pipo)


(trace add-handler-type)

(get-property-type 'testproj::pipo 'testproj::hud)
(get-handler-type 'sk8::oval 'sk8::fillcolor)
(get-global-type  'tp::xxx)
(sk8::loadscriptfile 

(sk8::view

|#





;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________

;;;Now the type inference for SKIL Forms

;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________
;;;_________________________________________________________________________________________________________

;;;_________________________________________________________________________________________________________
;;infrastructure
        
(defparameter current-local-variable-list nil)
(defparameter current-variable-changed nil)

(defun set-variable-type (curVar curType)
  (setf current-variable-changed t)
  (if (lib-form-p curType) (setf curType (fourth curType)))
  (if (assoc curVar current-local-variable-list)
    (setf (second (assoc curVar current-local-variable-list)) curType)
    (push (list curVar curType) current-local-variable-list)))


(defun get-variable-type (sym)
  (second (assoc sym current-local-variable-list)))

(defun get-return-type (form)
  (when form
    (let (funname dispatchArgType (res nil))
      (when (listp form)
        (setf funname (first form))
        (if (lib-form-p funname)
          (setf funname (fourth funname)))
        (when (second form)
          (setf dispatchArgType (get-skil-form-type (second form))))
        (cond 
         ((eq funname 'sk8::new)
          (setf res dispatchArgType))
         ((eq funname 'as-a)
          (setf res (if (third form) (get-skil-form-type (third form)) nil)))
         (t
          (setf res (get-function-return-type funname dispatchArgType)))))
      res)))

(defun is-sk8-object-sym (sym)
  (if (and (symbolp sym)
           (boundp sym)
           (or (and (sk8::is-a (symbol-value sym) sk8::object)
                    (string-equal (symbol-name sym) (sk8::objectname (symbol-value sym))))
               (MACFRAMES::clos-class-shadow-p (symbol-value sym))))
    t
    nil)
  )

(defun get-skil-form-type (form)
  (cond
   ((integerp form)
    'sk8::integer)
   ((floatp form)
    'sk8::float)
   ((stringp form)
    'sk8::string)
   ((current-local-variable-p form)
    (get-variable-type form))
   ((and (listp form)
         (eq (first form) 'script)
         (listp (second form))
         (eq (first (second form)) 'local)
         (deep-member 'SK8::positioninternal (second form)))
    'sk8::integer
    )
   ((lib-form-p form)
    (if (is-sk8-object-sym (fourth form))
      (fourth form)
      (get-global-type (fourth form))))
   (t
    (get-return-type form)))
  )

;;;_________________________________________________________________________________________________________
;;Functions to induct the info where needed.

(defun recursive-search-for-locals (form)
  (when (listp form)
    (when (eq (first form) 'local)
      (let (curVar curType)
        (dolist (loc (rest form))
          (setf curVar (if (listp loc) (first loc) loc))
          (setf curType (if (listp loc) (third loc) nil))
          (if (and (second loc) (not curtype))
            (setf curtype (get-skil-form-type (second loc)))) ;; induct it from the initialization value
          (set-variable-type curVar curType))))
    (dolist (i form)
      (recursive-search-for-locals i))))

(defparameter current-input-variable-list nil)

(defun setup-types-from-input (form)
  (when (and (listp form) (eq (first form) 'script))
    (let (curVar curType (inputs (skil-get-key form 'input nil)))
      (dolist (loc inputs)
        (setf curVar (if (listp loc) (first loc) loc))
        (if (eq curvar 'sk8::me) 
          (setf curtype (if (listp loc) (second loc)))
          (setf curType (if (listp loc) (third loc) nil)))
        (if (and (listp loc) (second loc) (not curtype) (neq curvar 'sk8::me))
          (setf curtype (get-skil-form-type (second loc)))) ;; induct it from the initialization value
        (push curvar current-input-variable-list)
        (set-variable-type curVar curType)))))

(defun setup-for-type-induction (form &optional dontclear)
  (unless dontclear
    (setf current-local-variable-list nil)
    (setf current-input-variable-list nil))
  (setup-types-from-input form)
  (recursive-search-for-locals form)
  t)

(defun current-local-variable-p (sym &optional excludingInputs)
  (and (assoc sym current-local-variable-list)
       (or (not excludingInputs)
           (not (memq sym current-input-variable-list)))))

;;;broddy@carroll1.cc.edu

(defun do-update-variable (curvar newtype &optional isArgumentType)
  (let* ((oldtype (get-variable-type curvar))
         (newTypeValue (confirm-for-new-type newtype
                                             nil
                                             oldtype))
         (propernewtype (if (eq newtype 'sk8::floatlist) 'sk8::list newtype))
         (properoldtype (if (eq oldtype 'sk8::floatlist) 'sk8::list oldtype))
         )
    (when (and (neq oldtype newtypevalue)
               (or (not isArgumentType)
                   (and (memq properoldtype '(sk8::number sk8::integer sk8::float))
                        (memq propernewtype '(sk8::number sk8::integer sk8::float)))
                   (not (sk8::is-a (symbol-value properoldtype) (symbol-value propernewtype)))))
      (set-variable-type curvar newTypeValue))
    ))

(defun induct-variable-types (form)
  (let (curvar newtype valform)
    (when (listp form)
      (cond
       ((eq (first form) 'set)
        (setf curvar (second form))
        (setf valform (fourth form))
        ;;;induct var's type in "set var to form"
        (when (current-local-variable-p curvar t)
          (if (and (listp valform)
                   (lib-form-p (first valform))
                   (eq (fourth (first valform)) 'skil::collectionnth)
                   (eq (get-skil-form-type (second valform)) 'sk8::floatlist))
            (setf newtype 'sk8::integer)
            (setf newtype (get-skil-form-type valform)))
          (when newtype
            (do-update-variable curvar newtype))
          )
        ;;;induct var's type in "set prop of obj to var"
        (when (and (listp curvar)
                   (lib-form-p (first curvar))
                   (current-local-variable-p valform t))
          (setf newtype (get-skil-form-type curvar))
          (when newtype
            (do-update-variable valform newtype))
          )
        )
       ((or (lib-form-p (first form))  ;;; function call
            (and (symbolp (first form)) (memq (first form) skil-operators)))
        (let (curfun curtypeinfo curargtype)
          (setf curfun (first form))
          (if (lib-form-p curfun)
            (setf curfun (fourth curfun)))
          (setf curtypeinfo (rest (get-function-type curfun)))  ;;ignore first arg the return type
          (when curtypeinfo
            (dolist (curarg (rest form))
              (setf curargtype (pop curtypeinfo))
              (when (and (current-local-variable-p curarg t) 
                         curargtype
                         )
                (do-update-variable curarg curargtype t)
                ))))
        )
       )
      (dolist (i form)
        (induct-variable-types i))
      )))

(defun induct-types-on-form (form &optional dontclear)
  (setup-for-type-induction form dontclear)
  (loop
    (setf current-variable-changed nil)
    (induct-variable-types form)
    (unless current-variable-changed
      (return)))
  )


#|
(trace minimum-shared-ancestor)
(trace add-handler-type)
(trace do-update-variable)
(untrace)
(skil::java-it "on unlinkFrom of me (a CBU), linkTarget (a CBU)
	
	if my links contains linkTarget then
		remove linkTarget from my links
		remove me from linkTarget's backwardLinks
	end if
	
end unlinkFrom

")
(get-skil-form-type 'x)
(get-variable-type 'x)
(get-skil-form-type '(lib "COMMON-LISP" object String))

(java-it "on foo
   set xx to 1
   if xx < 10 then beep()
end foo")

(get-skil-form-type '(SKIL::lib "ORGANIZERS" SKIL::object ORGANIZERS:CBU))
(confirm-for-new-type 'sk8::number nil 'sk8::number)

(get-function-type 'organizers::mode)
(get-property-type 'organizers::modemanager 'organizers::mode)


|#


(clear-type-information)




#|
	Change History (most recent last):
	2  	 9/19/96	Brian   	
	6  	 9/27/96	Brian   	
	7  	10/ 7/96	Brian   	
	8  	10/ 8/96	Brian   	
	9  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
