;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :graphics-system)

;;; _______________________________ 
;;; General pool mechanism.
;;; _______________________________ 

(defstruct (pool (:conc-name pq-))
  available
  creatorFunction)

(defun new-pool (size creatorFunction)
  (let ((thePool (make-pool))
        items)
    (dotimes (i size)
      (push (funcall creatorFunction) items))
    (setf (pq-available thePool) items)
    (setf (pq-creatorFunction thePool) creatorFunction)
    thePool))

;;; This has to be an atomic operation!

(defun get-pool-item (thePool)
  (without-interrupts
   (let ((theItem (pop (pq-available thePool))))
     (or theItem 
         (let ((availableItems (pq-available thePool))
               (creatorFunction (pq-creatorFunction thePool)))
           (dotimes (i 5)
             (push (funcall creatorFunction) availableItems))
           (pop availableItems))))))

(defun return-pool-item (thePool theItem)
  (setf (pq-available thePool)
        (cons theItem (pq-available thePool))))

;;; _______________________________ 
;;; Globals
;;; _______________________________ 

;;; Globals used by GET-TEMP-RGN-MAYBE-EXTEND and LET+:

(defparameter !*rgn-rect-initializer*! (newRecordGCPtr :rect))

(defparameter !*temp-region-pool*! (new-pool 50 #'T_NewRGNGC))

(defun new-region-pool ()
  (new-pool 50 #'T_newRgnGC))
(defun get-region-from-pool ()
  (if !*temp-region-pool*!
    (get-pool-item !*temp-region-pool*!)
    (T_NewRGNGC)))
(defun return-region-to-pool (theRegion)
  (when !*temp-region-pool*!
    (return-pool-item !*temp-region-pool*! theRegion)))

;;; Global used by gen-temp-port-maybe-extend in let+.

(defun make-port ()
  (let ((port (newRecordGCPtr :cGrafPort))) 
    (#_OpenCPort port) 
    port))

(defparameter !*temp-port-pool*! (new-pool 5 #'make-port))

(defun new-port-pool ()
  (new-pool 5 #'make-port))
(defun get-port-from-pool ()
  (if !*temp-port-pool*!
    (get-pool-item !*temp-port-pool*!)
    (make-port)))
(defun return-port-to-pool (thePort)
  (when !*temp-port-pool*!
    (return-pool-item !*temp-port-pool*! thePort)))

;;; _______________________________ 
;;; Helpers
;;; _______________________________ 

(defun SK8Rect-to-rect (SK8Rect rect)
  (setf (rref rect :rect.left) (f.round (rect-left SK8Rect))
        (rref rect :rect.top) (f.round (rect-top SK8Rect))
        (rref rect :rect.right) (f.round (rect-right SK8Rect))
        (rref rect :rect.bottom) (f.round (rect-bottom SK8Rect))))

;;; _______________________________ 
;;; LET+ System Def. Starts Here!!!
;;; _______________________________ 

(defun mac-record-size (mac-record-type)
  (ccl::record-field-length mac-record-type))

(defun mac-record-inits (var mac-record-type init-args)
  (unless (memq mac-record-type '(:point :longint :pointer))
    (ccl::record-field-forms var mac-record-type init-args)))

(defun struct-creator-form (structure-type)
  (let ((entry (gethash structure-type ccl::%defstructs%)))
    (if entry
      `(,(elt entry 4))
      (error "~A is not a known structure type" structure-type))))

(defun struct-slot-index (structure-type slot)
  (let ((entry (gethash structure-type ccl::%defstructs%))
        position)
    (if entry
      (progn
        (unless (eq (symbol-package slot) (symbol-package structure-type))
          (setq slot (intern (symbol-name slot) (symbol-package structure-type))))
        (if (setq position (position (symbol-name slot) (cdr (elt entry 1))
                                     :key #'(lambda (x) (symbol-name (car x))) :test #'string=))
          (1+ position)
          (error "~A has no ~A slot" structure-type slot)))
      (error "~A is not a known structure type" structure-type))))

(defun structure-inits (var structure-type init-args)
  (unless (gethash structure-type ccl::%defstructs%)
    (error "~A is not a known structure type" structure-type))
  (let ((init-forms nil)
        (init-pairs (reverse init-args))
        slot val)
    (loop
      (unless init-pairs (return))
      (unless (cdr init-pairs)
        (error "Uneven number of init arguments in ~A" init-args))
      (setq val (first init-pairs)
            slot (struct-slot-index structure-type (second init-pairs)))
      (push `(setf (ccl::struct-ref ,var ,slot) ,val) init-forms)
      (setq init-pairs (cddr init-pairs)))
    init-forms))

;;; This data structure tells let+ how to deal with special cases. The format is the following:
;;; (<caseKeyword>
;;;      <allocationForm>            how the thing will be allocated.
;;;      <declarationForm>           any declarations pertaining to this type of thing
;;;      <cleanUpFunction>           if allocation is done from a pool, this can be used to return it to it.
;;;      <initializationOptions>     can provide a seed data item from which the record will be generated
;;;                                      using the transformation function specified here.
;;; )


(defparameter *let+types*
  '((:region
     (get-region-from-pool) ((TYPE MACPTR) CCL::UNSETTABLE) return-region-to-pool
     (:init-empty
      (0            . `((require-trap #_SetEmptyRgn ,var))))
     (:init-rect
      (1            . `((require-trap #_RectRgn ,var ,(first args))))
      ((> 1)        . `((,@(mac-record-inits '!*rgn-rect-initializer*! :rect args)
                         (require-trap #_RectRgn var !*rgn-rect-initializer*!)))))
     (:init-SK8Rect
      (1            . `((SK8Rect-to-rect ,(first args) !*rgn-rect-initializer*!)
                        (require-trap #_RectRgn ,var !*rgn-rect-initializer*!)))))
    (:cgrafport
     (get-port-from-pool) ((TYPE MACPTR) CCL::UNSETTABLE) return-port-to-pool)
    (:rect
     (ccl::%new-ptr 8 nil) (DYNAMIC-EXTENT (TYPE MACPTR) CCL::UNSETTABLE) nil
     (:SK8Rect
      (1            . `((SK8Rect-to-rect ,(first args) ,var)))))
    
    ('pt
     (make-pt) (DYNAMIC-EXTENT (TYPE PT) CCL::UNSETTABLE) nil
     (:point
      (1            . `((point-to-SK8Point ,(first args) ,var)))))
    
    ('rect
     (make-rect) (DYNAMIC-EXTENT (TYPE RECT) CCL::UNSETTABLE) nil
     (:rect
      (1            . `((rect-to-SK8Rect ,(first args) ,var)))))))

(defconstant *let+mult-val-ignore-sym* nil)

(defmacro let+ ((&rest binding-list) &body body)
  (let ((temp-var (gensym))
        (bindings nil)
        (declare-forms nil)
        (multi-value-var-list nil)
        (clean-up-forms nil)
        first-var other-vars
        let+structure-p let+record-p
        var value-form init-args
        initializer-keyword args
        num-args let+type init-forms
        let+info creator-form
        declarations cleanUpFunction possible-inits)
    (declare (special var args))
    (flet ((add-var-declaration (var decl)
             (let ((type-decl-p (and (consp decl) (eq (first decl) 'type)))
                   (decl-list (cdr (find decl declare-forms :test #'(lambda (a b)
                                                                      (or (eq a (car b))
                                                                          (and (consp a)
                                                                               (eq (first b) 'type)
                                                                               (eq (second a) (second b)))))))))
               (if decl-list
                 (unless (memq var (if type-decl-p (cdr decl-list) decl-list))
                   (nconc decl-list (list var)))
                 (push (if type-decl-p (list 'type (second decl) var) (list decl var))
                       declare-forms)))))
      (if (and (consp (first body))
               (eq (caar body) 'declare))
        (setq declare-forms (cdar body)
              body (cdr body)))
      ;; NREVERSE is not used here because surgery on the forms passed to a macro
      ;; causes many problems when the macro is to be expanded within another macro.
      (dolist (binding (reverse binding-list))
        (when (consp binding)
          (setq var (first binding)
                value-form (second binding)
                let+structure-p nil
                let+record-p nil)
          (cond
           ((consp var) ;; This is a multiple-valued binding
            (setq multi-value-var-list (substitute temp-var *let+mult-val-ignore-sym* var)
                  first-var (car multi-value-var-list)
                  other-vars (remove temp-var multi-value-var-list))
            (unless (eq first-var temp-var)
              (setf other-vars (cdr other-vars)
                    (car multi-value-var-list) temp-var))
            (push `(,first-var (multiple-value-setq ,multi-value-var-list ,value-form)) bindings)
            (setq bindings (nconc (cdr other-vars) bindings))
            (setq binding (or (car other-vars) temp-var)))
           
           ((and (consp value-form)
                 (or (setq let+structure-p (and (consp (car value-form)) (eq (caar value-form) 'quote)))
                     (setq let+record-p (keywordp (car value-form)))))  ;; This variable is a struct or record
            (setq init-args (cdr value-form)
                  initializer-keyword (car init-args)
                  args (cdr init-args)
                  num-args (length args)
                  let+type (car value-form)
                  creator-form nil
                  init-forms nil
                  let+info (cdr (find let+type *let+types* :key #'car :test #'equal)))
            (when let+info  ;; LET+ has special behavior for this type
              (setq creator-form (first let+info)
                    declarations (second let+info)
                    cleanUpFunction (third let+info)
                    possible-inits
                    (find initializer-keyword (cdddr let+info) :key #'car :test #'(lambda (a b) (or (eq a b) (eq b t)))))
              (when possible-inits
                (setq init-forms
                      (find num-args (cdr possible-inits) :key #'car
                            :test #'(lambda (n test)
                                      (or (eql n test)
                                          (and (consp test)
                                               (funcall (first test) n (second test)))
                                          (eq test t)))))
                (unless init-forms
                  (let ((num-args-list (mapcar #'car (cdr possible-inits))))
                    (error "The ~A initializer for the ~A type requires ~A~{ or ~A~} arg(s)."
                           initializer-keyword let+type (car num-args-list) (cdr num-args-list))))
                (setq init-forms (eval (cdr init-forms))))
              (when cleanUpFunction
                (push `(,cleanUpFunction ,var) clean-up-forms)
                ))
            (unless creator-form
              (cond (let+structure-p
                     (setq creator-form (struct-creator-form (second let+type))
                           declarations `(DYNAMIC-EXTENT (TYPE ,(second let+type)) CCL::UNSETTABLE)))
                    (let+record-p
                     (setq creator-form `(ccl::%new-ptr ,(mac-record-size let+type) nil)
                           declarations `(DYNAMIC-EXTENT (TYPE MACPTR) CCL::UNSETTABLE)))))
            (unless init-forms
              (setq init-forms (cond (let+structure-p (structure-inits var (second let+type) init-args))
                                     (let+record-p (mac-record-inits var let+type init-args)))))
            (if init-forms
              (push `(,temp-var (progn ,@init-forms nil)) bindings))
            (setq binding `(,var ,creator-form))
            (dolist (decl declarations)
              (add-var-declaration var decl)))))
        (push binding bindings))
      (add-var-declaration temp-var 'IGNORE-IF-UNUSED)
      (if multi-value-var-list
        (push temp-var bindings))
      `(let* ,bindings
         ,@(if declare-forms `((declare ,@declare-forms)))
         (prog1
           (progn ,@body)
           ,@(if clean-up-forms `((progn ,@clean-up-forms)))
           ))
      )))


#|
	Change History (most recent last):
	3  	 8/ 8/96	Hernan  	Changed the region pool to use T_newRgnGC.
	4  	 9/ 3/96	Hernan  	Using T_newRgnGC instead of %new-rgn.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
