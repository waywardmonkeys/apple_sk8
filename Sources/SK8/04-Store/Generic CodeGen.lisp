;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

; FILE                "Generic-CodeGen.lisp"
; IMPLEMENTS     Generic code generator. 
; AUTHOR            Ken Dickey
; DATE                1994 October 10
; COPYRIGHT (c)  1994 Apple Computer, Inc. ; All rights reserved
; NOTES               See "CodeGenerator-class.lisp" "Project-Walker.lisp",
;                         "SK8:Function Library:source-dumper.lisp".
; SAMPLE USAGE  See "SK8Script-Codegen.lisp", "ScriptX-Codegen.lisp".
; STATUS            Alpha              

(in-package :SK8Dev)


(defconstant *%%CodeGen-Debug%%* NIL)   ;; @@ for debug @@

(defconstant delay-marker "'setting-delayed'")

;; (require 'Simple-Queue 'CodeGenerator-class)

;; WRINKLES
;
;; It is assumed that code is being generated for a single project 
;; and its subprojects.
;
;
;; References to other "outside" objects must be through global
;; names.  Externally required libraries are assumed to be loaded
;; before the project we are generating code for.
;
;; Following the current SK8 Project save/restore practice, 
;; "outside" values are never set.  The "main" routine should 
;; do this explicitly if it is required.
;
;; Values of project globals are assumed to obey the creation
;; order protocol (of the project walker).
;
;; In order to minimize assignments, if a slot value is the same between
;; parent and child then the value is assumed to be inherited via
;; _new_ and is not set.  If the parent is not a project object, then
;; we don't know what the initial value was and so take the conservative
;; strategy of setting the slot.  We may wish to revisit this someday
;; by adding this info.  So the conditions for eliding the setting of
;; a slot value are:
;; - slot exists in parent
;; - parent is in the project or its subprojects (i.e. is in order-info-table)
;; - slot values are the same for (new) child as parent
;; - slot value is not 'delayed'
;
;; NB: special care must be taken with SK8 "inheritable" property slots.

;;=======================================================

;; hacks for generic dispatching stuff so we can use multimethods 
;; rather than SK8 handlers {st specialize for various code
;; generators.

(ccl::set-find-class '_Object_class (ccl::class-of SK8::Object))
(ccl::set-find-class '_Actor_class (ccl::class-of sk8::Actor))
(ccl::set-find-class '_File_class (ccl::class-of SK8::File))
(ccl::set-find-class '_LineSegment_class (ccl::class-of SK8::LineSegment))
(ccl::set-find-class '_Media_class (ccl::class-of SK8::Media))
(ccl::set-find-class '_MenuBar_class (ccl::class-of SK8::MenuBar))
(ccl::set-find-class '_Menu_class (ccl::class-of SK8::Menu))
(ccl::set-find-class '_MenuItem_class (ccl::class-of SK8::MenuItem))
(ccl::set-find-class '_QuickTimeMovie_class (ccl::class-of SK8::QuickTimeMovie))
(ccl::set-find-class '_DynamicRenderer_class (ccl::class-of SK8::DynamicRenderer))
(ccl::set-find-class '_QuickTimeRenderer_class (ccl::class-of SK8::QuickTimeRenderer))
(ccl::set-find-class '_PixelMap_class (ccl::class-of SK8::PixelMap))
(ccl::set-find-class '_EditText_class (ccl::class-of SK8::EditText))

;;=======================================================


;; ------------------------  PUBLIC INTERFACE  ------------------------

;;(defclass <Whatever>-CodeGenerator (CodeGenerator)
;;
;;(defmethod Get-object-info-maker ( (cg CodeGenerator) )
;;(defmethod DoOpen  (
;;                  (cg CodeGenerator)  
;;                  (sk8-proj sk8::Project)
;;                  order-info-table
;;                  library-info-set
;;                  output-protocol
;;                  warning-stream
;;                  optional-splash-screen
;;                  optional-sound-loop
;;                  non-interactive? )
;;(defmethod DoGenCode  ( (cg CodeGenerator)  
;;                                       (SK8-info-rec CodeGenerator-SK8-object-info-class) )
;;(defmethod DoClose      ( (cg CodeGenerator)   main-fn-or-nil )
;;(defmethod Open?         ( (cg CodeGenerator) )




;---------------------
(defmethod Get-object-info-maker ( (cg CodeGenerator) )
  ;---------------------
  (error "You must specialize this function for your code generator!!!")
  )


;---------------------
(defmethod Open? ( (cg CodeGenerator) )
  ;---------------------
  (not (null (project cg)))
  )



;; open() is not generic 8^(
;---------------------
(defmethod DoOpen ( (cg CodeGenerator)  
                     (sk8-proj sk8::Project)
                     order-info-table
                     library-info-set
                     output-protocol
                     warning-stream
                     optional-splash-screen
                     optional-sound-loop
                     non-interactive?)
  ;---------------------
  
  ; remember what is going on here
  (setf (project cg) sk8-proj)
  (setf (order-table cg) order-info-table)
  (setf (libs-info cg) library-info-set)
  (setf (outp cg) output-protocol)
  (setf (warnp cg) warning-stream)
  (setf (splash cg) optional-splash-screen)
  (setf (sound cg)  optional-sound-loop)
  (setf (no-interact? cg) non-interactive?)
  
  ; generate header info for project
  (gen-header-info cg)
  
  ;; Gen code for optional splash screen and sound loop
  (gen-splash-screen        cg)
  (gen-sound-loop             cg)
  
  
  ;;;MAKE tables for anonomous objects which are eq (lists, arrays, strings, etc.)
  
  (Generate-Anonomous-Eqs-Tables cg)
  
  ;; Gen code to create the anonymous-objects-vector
  ;;@@@ Note: We don't know how many slots to allocate at this point.
  ;;@@@ Obvious options are to [1] generate code at end in a separate file
  ;;@@@ which gets loaded first, [2] make 2 passes over the objects, or
  ;;@@@ [3] have the project walker find out and tell us (preferred).
  ;;@@@
  (gen-anonymous-vector cg) 
  
  (when *%%CodeGen-Debug%%*
    (format (warnp cg) "~%-- Open complete for project ~s" (project cg)))
  
  T
  )


;---------------------
(defmethod DoGenCode ( (cg CodeGenerator)
                         (SK8-info-rec CodeGenerator-SK8-object-info-class))
  (gen-code-to-make (sk8-object SK8-info-rec) SK8-info-rec cg)
  )

;---------------------
(defmethod DoClose ( (cg CodeGenerator) main-fn-or-nil)
  ;---------------------
  (declare (ignore cg main-fn-or-nil))
  (error "You must specialize this function for your code generator!!!")
  )


;; ================== "Internal", specializable funs  ==================


(defmethod GEN-CODE-TO-MAKE ( (obj sk8::object) 
                                 (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                 (cg CodeGenerator) )
  
  ;; Generate code for indicated object
  
  ;; Find out what slots we need to set; Forget virtual slots.
  ;; NB: don't trash non-local, inheritable slots; known by:
  ;;     (mf::dynamically-inheritable-p object property)
  ;; if values are immediate or seen, set them when we create the
  ;; object, else delay the set! until the object needed is 
  ;; created.
  
  ;; find out (object-property -> parent X value-in-parent)
  (let* ( 
         (obj-name 
          (if (objectName (sk8-object SK8-info-rec))
            (objectName (sk8-object SK8-info-rec))
            (progn
              (unless (anonymous-object-vector-index SK8-info-rec)
                (setf (anonymous-object-vector-index SK8-info-rec)
                      (allocate-anonymous-object-index cg)))
              (gen-anon-name-string sk8-info-rec cg)
              )) )
         )
    (labels (
             (PROCESS-DELAYED-SETTERS-FOR-SELF (SK8-info-rec)
               (let ( (delayed-slot-queue (object-setter-queue SK8-info-rec)) )
                 (labels ( 
                          (loop (setter-info)
                            (when setter-info
                              (let* ( (prop-name 
                                       (delayed-setter-info-property-name-of-slot-to-be-set setter-info))
                                      (useset?
                                       (delayed-setter-info-use-set-instead-of-set!? setter-info))
                                      (obj-rec 
                                       (gethash 
                                        (delayed-setter-info-object-to-be-updated setter-info) 
                                        (order-table cg) 
                                        nil))
                                      (obj-name   (gen-name-string obj-rec cg))
                                      (prop-value (delayed-setter-info-value-of-slot-to-be-set setter-info))
                                      )
                                (gen-one-real-prop-init (sk8-object obj-rec) obj-name SK8-info-rec prop-name prop-value useset? cg)
                                (loop (pop-first delayed-slot-queue))))
                            )
                          )
                   (when delayed-slot-queue
                     (loop (pop-first delayed-slot-queue)))
                   ))))
      
      ; gen code to make the object
      (gen-object-maker obj
                        obj-name
                        SK8-info-rec
                        (filtered-real-properties obj) 
                        (filtered-local-properties  obj) 
                        cg)
      
      ; set slots of others depending on the value of this object
      (process-delayed-setters-for-self SK8-info-rec)
      
      T    ;; return value: Nil => failure, else success
      )))


(defmethod GEN-OBJECT-MAKER 
           ( (obj SK8::object)
             obj-name 
             (SK8-info-rec CodeGenerator-SK8-object-info-class) 
             real-propnames 
             local-propnames 
             (cg CodeGenerator) )
  
  (let* ( (parent (baseparent obj))
          (otherparents (cdr (reverse (parents obj))))
          )
    ; new ...
    (gen-new obj obj-name parent cg) 
    
    ; otherParents
    (when otherparents
      (gen-other-parents obj otherparents cg))
    
    ; init Local properties
    (gen-local-prop-inits obj obj-name
                          local-propnames 
                          cg)
    
    (gen-real-prop-inits obj obj-name SK8-info-rec 
                         real-propnames 
                         cg)

    ; save the localCreationRelations.
    (when (and (localCreationRelations obj) (not (is-a obj Port)))
      (delay-setting-self obj 'localCreationRelations obj (localCreationRelations obj) t cg))
    
    ; allow customization
    (add-custom-codegen-action obj obj-name SK8-info-rec cg)  ;; specialize as needed
    ) ) ; end gen-object-maker


(defmethod DELAY-SETTING-SELF (obj slot slot-value-object prop-value useset?
                                      (cg CodeGenerator))
  (unless (dontsavescript slot-value-object)
    (when *%%CodeGen-Debug%%*
      (format (warnp cg) "~%-- DELAY-SETTING-SELF: ~a's ~a gets ~a"
              obj slot slot-value-object))
    (let ( (obj-rec (gethash slot-value-object (order-table cg) nil)) )
      ; add to queue in object
      (unless (object-setter-queue obj-rec)
        (setf (object-setter-queue obj-rec)
              (make-instance 'queue)))
      (push-last (make-delayed-setter-info
                  :object-to-be-updated obj
                  :use-set-instead-of-set!? useset?
                  :property-name-of-slot-to-be-set slot
                  :value-of-slot-to-be-set prop-value)
                 (object-setter-queue obj-rec))
      )))

;; return a component which is an object in the project that has not yet been created,
;; or nil if all components are immediately available
;; T if it is not settable and we don't know if it will ever be
(defmethod NOT-IMMEDIATELY-SETTABLE? (prop-value 
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  (declare (ignore prop-value SK8-info-rec cg))
  nil ;; default - most objects are immediate
  )

(defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value sk8::object)
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  ; in project and already exist?
  (let ( (probe (gethash prop-value (order-table cg))) )
    (if probe ; in project
      (when  (> (creation-order-number probe)
                (creation-order-number SK8-info-rec))
        prop-value)
      ; outside project: if it is named, that's easy. If it isn't, helpfindmychild should take care of it.
      ;; who knows what will happen if it doesn't... we'll just return nil to say that it should be ok.
      nil)))

(defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value list)
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  ;; this loop is careful not to break on a cdr-coded list
  (loop
    (if (consp prop-value)
      (let ((ret (not-immediately-settable? (pop prop-value) SK8-info-rec cg)))
        (when ret (return ret)))
      (return (when prop-value (not-immediately-settable? prop-value SK8-info-rec cg))))))

(defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value array)
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  (dotimes (i  (array-total-size prop-value))
    (let ((ret (not-immediately-settable? (row-major-aref prop-value i) SK8-info-rec cg)))
      (when ret
        (return ret)))))

(defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value vector)
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  (dotimes (i (length prop-value))
    (let ((ret (not-immediately-settable? (aref prop-value i) SK8-info-rec cg)))
      (when ret
        (return ret)))))

(defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value string)
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  (declare (ignore prop-value SK8-info-rec cg))
  nil)

(defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value gs:*sk8-menu-item*)
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  (let* ((mnitem (mf::my-frame prop-value))
         (ownermenu (menu mnitem))
         (checkedp (checkmark mnitem))
         (cmdkey (command-key prop-value))
         (txt (text mnitem))
         (enbld (enabled mnitem))
         (style (menu-item-style prop-value))
         (color-list (slot-value prop-value 'ccl::color-list)))
    (or (not-immediately-settable? mnitem SK8-info-rec cg)
        (not-immediately-settable? ownermenu SK8-info-rec cg)
        (when ownermenu (not-immediately-settable? (macMenu ownermenu) SK8-info-rec cg))
        (not-immediately-settable? checkedp SK8-info-rec cg)
        (not-immediately-settable? cmdkey SK8-info-rec cg)
        (not-immediately-settable? txt SK8-info-rec cg)
        (not-immediately-settable? enbld SK8-info-rec cg)
        (not-immediately-settable? style SK8-info-rec cg)
        (not-immediately-settable? color-list SK8-info-rec cg)
        )))

(defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value gs:*sk8-menu*)
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg CodeGenerator))
  (let* ((mn (mf::my-frame prop-value))
         (ownermenu (menu mn))
         (ownermacmenu (when ownermenu (macMenu ownermenu)))
         (txt (menu-title prop-value)))
    (or (not-immediately-settable? mn SK8-info-rec cg)
        (not-immediately-settable? ownermenu SK8-info-rec cg)
        (not-immediately-settable? ownermacmenu SK8-info-rec cg)
        (not-immediately-settable? txt SK8-info-rec cg))))

;; --------------------- some properties must be defaulted ---------------------


(defmethod filtered-real-properties ( (obj SK8::object))
  (realProperties  obj))

(defmethod filtered-local-properties ( (obj SK8::object))
  (localRealProperties  obj))


(defmethod reinitialize-codegenerator ( (cg CodeGenerator) )
  (setf (order-table cg)  nil)
  (setf (libs-info cg)    nil)
  (setf (splash cg)       nil)
  (setf (sound cg)        nil)
  (setf (no-interact? cg) nil)
  (setf (anonymous-object-high-water-index cg) 1)
  (setf (project cg)      nil)
  (setf (warnp cg)        nil)
  (setf (outp cg)         nil)
  (setf (out-of-project-objects cg) nil)
  (setf (localCreationRelations-objects cg) nil)
  )

;;    ----------------------------- stuff for dealing with eq anon objects -----------------------------

(defun maprealproperties (obj func)
  (mapc #'(lambda (x) 
            (unless (memq (car x) mf::*mf-internal-format-slot-names*)
              (funcall func (car x))))
        (class-instance-slots (class-of obj)))
  nil
  )

;;;****NEED TO REMOVE PROPERTIES TO SAVE AS FALSE AND PROPERTIES TO SAVE
;;;SPECIALLY!!!!!!!
(defun Potential-Eqs-For-Save (proj)
  (let* ((allobjs (make-hash-table  :test #'eq))
         val
         (ourobjfun #'(lambda (i)
                        (maprealproperties i 
                                           #'(lambda (j)
                                               (setf val (and (not (memq j '(physicalboundsrect logicalboundsrect flags contents knownchildren)))
                                                              (getvalue j i)))
                                               (when (and val (or (arrayp val) (consp val))) ;;;(is-a (getvalue j i) collection))
                                                 (push (list i j) (gethash val allobjs) ))))))
         )
    (mapprojectobjects proj  ourobjfun)
    (funcall ourobjfun proj)
    (dolist (j (constants proj))
      (setf val (symbol-value j))
      (when (and val (or (arrayp val) (consp val))) ;;;(is-a (getvalue j i) collection))
        (push (list 'VARIABLE j) (gethash val allobjs) )))
    (dolist (j (globals proj))
      (setf val (symbol-value j))
      (when (and val (or (arrayp val) (consp val))) ;;;(is-a (getvalue j i) collection))
        (push (list 'VARIABLE j) (gethash val allobjs) )))
    allobjs))

;;;(time (Potential-Eqs-For-Save foo))


(defun create-store-cons-table (SharedList &optional ConsTable (startCount 1))
  (unless constable (setf constable (make-hash-table  :test #'eq)))
  (let* ((curlist (remove-duplicates sharedlist :test #'eq))
         (curCount startCount)
         (currentCons (nthcdr  (- (length (car curlist)) (1- startCount)) (car curlist)))
         (scratchTable (make-hash-table  :test #'eq))
         curentry)
    
    (loop
      (unless curlist (return))
      (if (optionkeydown) (return))  ;;;***debugging.  remove when working...
      (setf scratchTable (clrhash scratchTable))
      
      ;;;we make a hash table keyed off of all our lists based at the current count (from the rear)
      (mapc #'(lambda (i) (push i (gethash (nthcdr (- (length i) curcount) i) scratchtable)))
            curlist)
      
      ;;now we see if we split
      (if (> (length (keys scratchtable)) 1)
        (progn  ;;; if we split our cons inverse tree
          ;;;make an entry for curcount - 1 with the value of our 
          (setf curentry (nthcdr (- (length (car curlist)) (- curcount 1)) (car curlist)))
          (unless (gethash curentry constable)
            (setf (gethash curentry constable)
                  currentCons))
          ;;;and recurse for each of the branches...
          (maphash #'(lambda (kkk val) 
                       (declare (ignore kkk))
                       (dolist (i val)
                         (setf curlist (nremove i curlist)))
                       (create-store-cons-table val ConsTable curcount)
                       ) scratchtable)
          )
        ;;;otherwise check and see if our current cons is one of our values  (we've reached a property value)
        (maphash #'(lambda (kkk val) 
                     (dolist (i val)
                       (when (eq i kkk)   ;;;its a leaf node
                         (setf curlist (nremove i curlist))
                         (setf (gethash i constable) currentCons)
                         (setf currentCons i)
                         ))
                     ) scratchtable))
      ;;then we see if any of the entries are leaf nodes.
      ;;if so, we add an entry for that cell
      (incf curcount)
      )
    )
  consTable)



#|

(setf a8 (list (foo::anonprop2 (nth 2 (contents foo::cacho))) (foo::anonprop (nth 2 (contents foo::cacho)))))
(setf a7 (cons (foo::anonprop3 (nth 2 (contents foo::cacho))) a8))
(setf a6 (cons 9 a7))
(setf a5 (nconc (list 5 4) a7))
(setf a4 (cons 6 a5))

(setf allas (list a4 a5 a6 a7 a8))

(setf foo::xxxxx allas)
(maphash #'(lambda (kkk val) (print (list kkk val))) (create-store-cons-table allas))

(setf (foo::superprop (nth 2 (contents foo::cacho))) *)


|#










(defun Gen-Table-Of-Odd-Eqs (proj)
  (let* ((allobjs (Potential-Eqs-For-Save proj))
         (interEqDependencies (make-hash-table  :test #'eq))
         (alleqs (make-hash-table  :test #'eq))
         (RawCdrTable (make-hash-table  :test #'eq))
         (ConsTables nil)
         (SpecialPropTable (make-hash-table  :test #'eq))
         remainToProcess
         curitem
         keys allConsItems
         allProcessed
         (ProcessItemFun #'(lambda (j) 
                             (when (or (arrayp j) (listp j))
                               (unless (memq j allProcessed)
                                 (push j allProcessed)
                                 (push j remainToProcess))
                               (dolist (it (gethash curitem allobjs))
                                 (push it (gethash j allobjs))))
                             ))
         )
    ;;;Start with all the different arrays and lists that properties contain...
    (setf remainToProcess (keys allobjs))
    (setf allProcessed (keys allobjs))
    
    ;;;-------------------------------------------------------------------
    ;;;and go through each one to check their items
    (loop
      (unless remainToProcess (return))
      (setf curitem (pop remainToProcess))
      ;;;Now for arrays and lists we go through their items and check for multiple uses
      (unless (stringp curitem)
        (if (and (listp curitem) (not (listp (cdr curitem))))   ;;check for cons pairs separately
          (progn
            (funcall ProcessItemFun (car curItem))
            (funcall ProcessItemFun (cdr curItem)))
          (map nil ProcessItemFun (if (arrayp curitem) (CCL::array-data-and-offset curitem) curitem ))))
      )
    
    ;;;-------------------------------------------------------------------
    ;;;Now check for duplicated cons cells
    ;;;first we cull out all regular lists that share the same last cons cell...
    (maphash #'(lambda (kkk val) 
                 (declare (ignore val))
                 (when (and kkk (listp kkk) (listp (cdr kkk))) ;;all straight lists
                   (push kkk (gethash (nthcdr (1- (length kkk)) kkk) RawCdrTable))
                   )
                 )
             allobjs)
    ;;;we then figure out the inverse cons tree that is rooted at this last item for each set of lists
    ;;;This info is stored in a "cons-table"
    (maphash #'(lambda (kkk val) 
                 (declare (ignore kkk))
                 (when (> (length val) 1)
                   (dolist (i val)
                     (dolist (j (gethash i allobjs))
                       (unless (memq (cadr j) (gethash (car j) SpecialPropTable))
                         (push (cadr j) (gethash (car j) SpecialPropTable)))))
                   (push (create-store-cons-table val) ConsTables)
                   )) RawCdrTable)
    
    ;;;-------------------------------------------------------------------
    ;;;Now we use the info to generate our final tables:
    
    ;;;From raw table, cull out all the properties that use each object and
    ;;;make a table keyed on objects with list of special properties
    (maphash #'(lambda (kkk val) 
                 (when (> (length val) 1) 
                   (setf (gethash kkk alleqs) val)
                   (dolist (i val)
                     (unless (memq (cadr i) (gethash (car i) SpecialPropTable))
                       (push (cadr i) (gethash (car i) SpecialPropTable))))
                   )) allobjs)
    ;;;Now generate dependencies to determine what order to make the eqs...
    (setf keys (keys alleqs))
   
    (dolist (i constables)
      (setf keys (nconc keys (keys i)))
      (maphash #'(lambda (kkk val) 
                   (when val
                     (push val (gethash kkk interEqDependencies))
                     ))
               i))
    
    (setf keys (delete-duplicates keys))
    (mapc #'(lambda (obj) 
              (unless (stringp obj)
                (map nil #'(lambda (j) 
                             (when (memq j keys)
                               (push j (gethash obj interEqDependencies))))
                     obj)))
          keys)
    

  
    (setf allConsItems (delete-duplicates (append (keys alleqs) (apply #'append (mapcar #'keys consTables)))))
    (clrhash alleqs)
    (clrhash allobjs)
    (values allConsItems interEqDependencies SpecialPropTable constables)
    )
  )

#|
(print "intereqdepends...")
(maphash #'(lambda (kkk val) (if val (print (list kkk val)))) interEqDependencies)
(print "alleqs")
(maphash #'(lambda (kkk val) (if val (print (list kkk val)))) alleqs)
(print "SpecialPropTable")
(maphash #'(lambda (kkk val) (if val (print (list kkk val)))) SpecialPropTable)
(print "SpecialPropTable")
  (maphash #'(lambda (kkk val) (if val (print (list kkk val)))) SpecialPropTable)
  |#

(defmethod Generate-Anonomous-Eqs-Tables ((cg CodeGenerator) )
  (multiple-value-bind (allConsItems interEqDependencies SpecialPropTable consTables) (Gen-Table-Of-Odd-Eqs (project cg))
    (setf (allConsItems cg) allConsItems)
    (setf (interEqDependencies cg) interEqDependencies)
    (setf (specialPropTable cg) SpecialPropTable)
    (setf (consTables cg) consTables)
    ))


(defun Gen-Order-From-Dependencies (cg)
  (let* (curlist 
        (interEqDependencies (interEqDependencies cg))
        ok
        (remaining (allConsItems cg)))
    (loop
      (setf ok nil)
      (dolist (i remaining)
        (when ok (return))
        (when (every #'(lambda (x) (memq x curlist)) (gethash i interEqDependencies))
          (setf ok t)
          (setf remaining (nremove i remaining))
          (setf curlist (nconc curlist (list i)))
          ))
      (unless remaining (return))
      (unless ok (error "NO CHANGE")))
    curlist
    )
  
  )

(defmethod gen-eq-set-form (val anonID (cg CodeGenerator))
  (declare (ignore val anonID))
  nil)

(defmethod gen-special-prop-forms ((cg CodeGenerator))
  (let (makeOrder
        (vecIDTable (make-hash-table  :test #'eq))
        ;; (interEqDependencies (interEqDependencies cg))
        (SpecialPropTable (specialPropTable cg))
        curID
        )
    (setf (specialPropTable cg) 'eq)  ;;ALERT gen-one-real-prop-init to make these property settings no matter what
    (setf (EqVectorIDTable cg) vecIDTable)
    (setf makeOrder (Gen-Order-From-Dependencies cg))
    ;(PRINT MAKEORDER)
    (dolist (i makeOrder)
      (setf curID (allocate-anonymous-object-index cg))
      (gen-eq-set-form i curID cg)
      (setf (gethash i vecIDTable) curID)
      )
    (maphash #'(lambda (itm val)  
                 (if (eq itm 'variable)
                   (dolist (i val) 
                     (gen-var i (not (constantp i)) cg)
                     )
                   (dolist (i val) 
                     (gen-one-real-prop-init itm (objectname itm) 
                                             nil i
                                             (getvalue i itm)
                                             nil cg)
                     )))
             SpecialPropTable)
    
    (setf (specialPropTable cg) (make-hash-table  :test #'eq))  ;;Put a table back in, just in case.
    ))


;;    ----------------------------- Mother's little helpers -----------------------------


(defun print-separator-line (a-stream)
  (format a-stream 
          "~%~%-- ******************************************************")
  )

(defmethod allocate-anonymous-object-index ( (cg CodeGenerator))
  (let ( (new-index (anonymous-object-high-water-index cg)) )
    (incf (anonymous-object-high-water-index cg))
    new-index
    ) )

;;                                  --- E O F ---

#|
	Change History (most recent last):
	1  	10/12/94	It      	Generic codegen logic common to SK8 and SX
	2  	10/13/94	kend    	Add forgotten parameter to calls to filter-inherited-props-for via next-method.
	3  	10/20/94	kend    	Added more property filtration to Actor
	4  	10/21/94	dy      	remove ownsRegion
	5  	10/26/94	sidney  	declare some handlers as private
	6  	10/26/94	kend    	Added 'layer to properties filtered from Actor
	7  	10/26/94	chip    	took out now unnecessary MAKE-PRIVATE... calls
	8  	10/28/94	kend    	White out physicalscale prop from Actor
	9  	11/ 4/94	kend    	filtered-*-properties white out localCreationRelations (must be treated specially); remembers objects with localCreationRelations in codegenerator object.
	10 	11/ 4/94	dy      	fix filter-inherited-props-for QuickTimeMovie
	11 	11/ 8/94	kend    	Simplify localCreationRelations test (as per Adam)
	12 	11/ 8/94	kend    	Only note creationRelations which are local and not inheritable.
	13 	11/14/94	dy      	fix filtering for QT objects
	14 	11/15/94	kend    	Don't set properties if they contain values which are mac handles.
	15 	11/15/94	kend    	Don't set properties which don't have setters
	16 	11/16/94	kend    	Filter colorDepth property from lineSegment
	17 	11/16/94	kend    	Added _MenuItem_class recognizer
	18 	11/17/94	kend    	white-out 'layer' in menuItem and 'macMenu' in menu
	19 	11/18/94	kend    	PixelMap recognizer added.
	20 	11/21/94	kend    	Don't set properties which are tagged as creationRelations.  Don't white-out localCreationRelations.  *Don't* remember localCreationRelations for later processing.
	21 	11/23/94	dy      	add DynamicRenderer filter, remove QTTrackCollection, QTTrack, and QTMedia
	22 	11/23/94	dy      	add onStage to the filtered properties of QuickTimeRenderer
	23 	 1/12/95	sidney  	changes to save as text to be more like binary store
	24 	 1/24/95	sidney  	more tweaking to suport save as text of strange objects
	25 	 1/26/95	sidney  	tweaked again: deal with sk8-fred, sk8-menu, etc. clos objects
	26 	 2/ 2/95	sidney  	1202036: do right thing with delayed composite values
	27 	 3/ 8/95	sidney  	don't increment count in anonymous object vector for objects that will not be written out
	28 	 3/14/95	sidney  	add media class definition so we can define a method on media class
	29 	 3/16/95	rod     	fileSpecGCHandle -> appleFileSpecGCHandle
	30 	 3/18/95	sidney  	1227949: changed way composit prims are dealt with as part of getting menus to save/load properly
	31 	 3/19/95	sidney  	1227949: get the fix to work right (deal with multilayer menus)
	32 	 3/19/95	sidney  	don't ASS-U-ME that menu-items always have an owner menu (it can be just a prototype)
	33 	 3/21/95	sidney  	no more top-level-actors field in codegenerator
	34 	 3/23/95	sidney  	now that we use helpfindmychild, we can treat unnamed objects in other projects as immediately settable
	2  	 7/ 5/95	sidney  	remove some gratuitous output that is supposed to be comments but makes the output language specific
	3  	 8/ 4/95	Brian    	new eq stuff added
	4  	 8/ 4/95	Brian   	various fixes.  print statements removed.
	5  	 8/ 8/95	Hernan  	CONS CELL WORK!!!
	6  	 8/ 9/95	Brian   	Making cons stuff work with variables and
						constants
	7  	 8/11/95	Brian   	included project object in eq stuff
	8  	 8/28/95	Brian   	
	9  	11/27/95	Brian   	
	2  	 8/ 9/96	Hernan  	Saving the creation relations.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
