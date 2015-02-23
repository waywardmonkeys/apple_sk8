;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; A collection of useful lisp tools.
;;; 
;;;  -- Till

;;; list-callers (obj)
;;;   list the functions that call of a function or reference an object.
;;; print-callers (obj)
;;;   prints ...
;;; map-callers (mapping-func obj)
;;;   maps a function over ...
;;; list-function-references (func)
;;;   list the objects that a function references
;;; list-references (obj)
;;;   list the objects that reference this object
;;; print-references (obj)
;;;   prints ... 
;;; map-references (mapping-func referencee)
;;;   maps the mapping function over all objects that reference this one.
;;; map-lisp-space (func &key dynamicp staticp)
;;;   maps the function over all of lisp space.

;;; Since the intent is for SK8 developers to use these tools.
(in-package :SK8DEV)

(require :lapmacros)


(defmacro do-function-refs-internal ((var lfun-vector) &body body)
  (let ((i (gensym)))
    `(dotimes (,i (ccl::%count-immrefs ,lfun-vector))
       (let ((,var (ccl::%nth-immediate ,lfun-vector ,i)))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lists the callers to a function or other object.
;;; 
;;; Takes 20 seconds or so while it bops through dynamic space.
(defun list-callers (obj)
  (let (callers)
    (map-callers #'(lambda (a) (push a callers)) obj)
    callers))

(defun print-callers (obj)
  (map-callers #'print obj)
  (values))

;;; Map the mapping function over all the functions that call obj.
(defun map-callers (mapping-func obj)
  (let ((name (if (functionp obj) (function-name obj) obj)))
    (ccl::map-dynamic-space
     #'(lambda (a) 
         (when (typep a 'ccl::lfun-vector)
           (do-function-refs-internal (this a)
             (when (or (eq this obj)
                       (and (symbolp this) (eq this name)))
               (let ((this-func (ccl::%lfun-vector-lfun a)))
                 (funcall mapping-func 
                          (or (function-name this-func) this-func))))))))))

;;; List the objects this function references.
(defun list-function-references (func)
  (check-type func function)
  (let ((refs nil))
    (do-function-refs-internal (a (ccl::%lfun-vector func))
      (pushnew a refs))
    refs))

;;; Lists all references to an object.
(defun list-references (obj)
  (let (refs)
    (map-references #'(lambda (a) (push a refs)) obj)
    refs))

(defun print-references (obj)
  (map-references #'print obj))

;;; Maps func over all references to it.
(defun map-references (func referencee)
  (labels ((mapper (ob)
             (when (ccl::%lfun-vector-p ob)
               (setq ob (ccl::%lfun-vector-lfun ob)))
             (when
               (cond ((consp ob)
                      (or (eq referencee (car ob))
                          (eq referencee (cdr ob))))
                     ((symbolp ob)
                      (or (and (boundp ob) (eq referencee (symbol-value ob)))
                          (and (fboundp ob) (eq referencee (symbol-function ob)))))
                     ((uvectorp ob)
                      (dotimes (i (uvsize ob))
                        (when (eq referencee (uvref ob i)) (return t))))
                     ((ccl::%lfun-vector-p ob)
                      (and (not (eq ob #'mapper))
                           (do-function-refs-internal (each ob)
                             (when (eq each referencee)
                               (return t))))))
               (funcall func ob))))
    (declare (dynamic-extent #'mapper))
    (ccl::map-dynamic-space #'mapper)))



;;; Maps the function over ever memory space.
;;; dynamic & static are boolean keyword args.
;;;
;;; Hint: Dynamic space is for objects that can reference other 
;;; objects like cons cells, structures, vectors, code, symbols, 
;;; etc.
;;; Static space is for objects that can't reference other 
;;; objects, like strings, keyword symbols, floats, bignums, etc.
(defun map-lisp-space (function &key (dynamicp t) (staticp nil))
  (when dynamicp (ccl::map-dynamic-space function))
  (when staticp (ccl::map-static-space function))
  nil)


(in-package :ccl)
(defun map-dynamic-space (function)
  (old-lap-inline ()
    (move.l (a5 $Pdynamic_cons_area) atemp1)
    (move.l (atemp1 $cons-area.gspace-start) atemp0)
    (prog#
     (move.l @atemp0 da)
     (if# (ne (cmp.b ($ $object-header) da))
       (add ($ $t_cons) atemp0)
       elseif# (eq (cmp.w ($ $symbol-header) da))
       (add ($ $t_symbol) atemp0)
       else#
       (add ($ $t_vector) atemp0))
     (vpush atemp0)
     (move.l atemp0 arg_z)
     (set_nargs 1)
     (lfjsr (varg function 4))
     (vpop dx)
     (move.l dx atemp0)
     (ttag dx da)
     (sub.w da atemp0)
     (if# (eq (sub.b ($ $t_vector) da))
       (move.l ($ #xf) da)
       (add.l (atemp0 (+ $t_vector $v_log)) da)
       (and.l ($ #x00ff #xfff8) da)
       (add.l da atemp0)
       elseif# (eq (sub.b ($ (- $t_symbol $t_vector)) da))
       (lea (atemp0 $sym_size) atemp0)
       else# (add ($ 8) atemp0))
     (until# (geu (a5 $gfree) atemp0)))))

(defun map-static-space (function)
  (old-lap-inline ()
    (move.l (a5 $Pstatic_cons_area) atemp1)
    (move.l (atemp1 $cons-area.gspace-start) atemp0)
    (prog#
     (move.l @atemp0 da)
     (if# (ne (cmp.b ($ $object-header) da))
       (add ($ $t_cons) atemp0)
       elseif# (eq (cmp.w ($ $symbol-header) da))
       (add ($ $t_symbol) atemp0)
       else#
       (add ($ $t_vector) atemp0))
     (vpush atemp0)
     (move.l atemp0 arg_z)
     (set_nargs 1)
     (lfjsr (varg function 4))
     (vpop dx)
     (move.l dx atemp0)
     (ttag dx da)
     (sub.w da atemp0)
     (if# (eq (sub.b ($ $t_vector) da))
       (move.l ($ #xf) da)
       (add.l (atemp0 (+ $t_vector $v_log)) da)
       (and.l ($ #x00ff #xfff8) da)
       (add.l da atemp0)
       elseif# (eq (sub.b ($ (- $t_symbol $t_vector)) da))
       (lea (atemp0 $sym_size) atemp0)
       else# (add ($ 8) atemp0))
     (move.l (a5 $Pstatic_cons_area) atemp1)
     (move.l (atemp1 $cons-area.ispace-end) atemp1)
     (until# (geu atemp1 atemp0)))))



#|
	Change History (most recent last):
	1	4/29/94	till	It's here for all to enjoy.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
