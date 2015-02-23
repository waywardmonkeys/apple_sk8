;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

(define-handler sk8:handlers (sk8:Object &key name (inherited t) (local t))
                (unless (ccl::standard-instance-p me)
                  (setq me (class-of me)
                        local nil))
                (if name
                  (mf::find-handler name me :inherited-p inherited :local-p local)
                  (mf::get-handlers me :inherited-p inherited :local-p local)))

(define-handler sk8:localHandlers (sk8:Object)
                (when (ccl::standard-instance-p me)
                  (mf::get-handlers me :local-p t :inherited-p nil)))

(define-handler sk8:inheritedHandlers (sk8:Object)
                (let ((local nil))
                  (unless (ccl::standard-instance-p me)
                    (setq me (class-of me)
                          local t))
                  (mf::get-handlers me :local-p local :inherited-p t)))

(define-handler sk8:name (SK8:Symbol)
                (ss::pretty-symbol-name me))

(define-handler sk8:value (SK8:symbol)
                (symbol-value me))

(define-handler sk8:implementors (SK8:Symbol)
                (let ((gf (fboundp me))
                      (result nil)
                      obj name)
                  (when (and gf (typep gf 'standard-generic-function))
                    (dolist (m (generic-function-methods gf))
                      (when (setq obj (get-handler-object m))
                        (push obj result)))
                    result)))

(define-handler sk8:handlers (SK8:Symbol &key name (inherited t) (local t))
                (declare (ignore name inherited local))
                (sk8:implementors me))

;; was (define-handler sk8:name (SK8:Function)
(defmethod sk8:name ((me Function) &key)
  (let ((name (function-name me)))
    (if (consp name)
      (ccl::setf-function-name (second name))
      name)))

;; was (define-handler sk8:version (SK8:Function)
(defmethod sk8:version ((me Function) &key)
  (ps::lfunVersion me))

;; was (define-handler sk8:project (SK8:Function)
(defmethod sk8:project ((me Function) &key)
  (or (get-function-project me)
      (ccl::package-project (symbol-package (function-name me)))))


;; was (define-handler sk8:name (SK8:Handler)
(defmethod sk8:name ((me standard-method) &key)
  (let ((name (method-name me)))
    (if (consp name)
      (ccl::setf-function-name (second name))
      name)))

;; was (define-handler sk8:version (SK8:Handler)
(defmethod sk8:version ((me standard-method) &key)
  (ps::lfunVersion (method-function me)))

;; was (define-handler sk8:object (SK8:Handler)
(defmethod sk8:object ((me standard-method) &key)
  (get-handler-object me))

;; was (define-handler sk8:project (SK8:Handler)
(defmethod sk8:project ((me standard-method) &key)
  (let ((lfun (method-function me)))
    (or (get-function-project lfun)
        (let ((obj (get-handler-object-from-lfun lfun)))
          (if (is-a obj Project)
            obj
            (project obj))))))


;; was (define-handler sk8:implementors (SK8:Handler)
(defmethod sk8:implementors ((me standard-method) &key)
  (sk8:implementors (sk8:name me)))

;; was (define-handler sk8:removeHandler (SK8:Handler)
(defmethod sk8:removeHandler ((me standard-method) &key)
  (delete-handler me :register t))

;; was (define-handler sk8:removeFunction (SK8:Function)
(defmethod sk8:removeFunction ((me Function) &key)
  (delete-function (sk8:name me) :project (sk8:project me)  :register t))

(define-handler sk8:copy (Number &rest props)
                (declare (ignore props))
                (+ me 0))
(define-handler sk8:copy (List &rest props)
                (declare (ignore props))
                (copy-list me))
(define-handler sk8:copy (Array &rest props)
                (declare (ignore props))
                (let ((newArray (CCL::copy-uvector me)))
                  #+ppc-target
                  (setf (uvref newarray PPC::arrayh.data-vector-cell) (CCL::copy-uvector (CCL::array-data-and-offset me)))
                  #-ppc-target
                  (setf (CCL::ARH.VECT newArray) (CCL::copy-uvector (CCL::ARH.VECT me)))
                  newArray))
(define-handler sk8:copy (Vector &rest props)
                (declare (ignore props))
                (copy-seq me))


(defmethod SK8:new ((me (eql SK8:List)) &key length item ((:project InProject)))
  (declare (ignore inProject))
  (unless (or (null length) (eql 0 length))
    (make-list length :initial-element item)))

(defmethod SK8:new ((me (eql SK8:Array)) &key length dimensions item ((:project InProject)))
  (declare (ignore inProject))
  (when (and length dimensions) (error "Can't specify both length and dimensions when making a new array"))
  (when (and (null length) (null dimensions)) (error "Must specify either length or dimensions when making a new array"))
  (make-array (or length dimensions)
              :initial-element item
              ;:initial-contents
              :adjustable t
              :fill-pointer (> 2 (length dimensions)) ;; has fill-pointer iff it is legal to have one
              ))

(define-handler SK8:dimensions (SK8:Array)
                (array-dimensions me))


(defmethod SK8:new ((me (eql SK8:Vector)) &key length item ((:project InProject)))
  (declare (ignore inProject))
  (make-array (or length 0)
              :initial-element item
              :adjustable t
              :fill-pointer t))

(defmethod SK8:new ((me (eql SK8:String)) &key length ((:character ch)) ((:project InProject)))
  (declare (ignore inProject))
  (make-array (or length 0)
              :element-type 'character
              :initial-element (or ch #\Space)
              ;:adjustable t :fill-pointer t
              ))

(defmethod SK8:new ((me (eql SK8:Symbol)) &key name publish ((:project InProject)))
  (unless (stringp name)
    (SK8:SK8-error SK8:ArgumentTypeMismatchError :handlerName 'new :argumentName 'name :object name :expectedType String))
  (let ((sym (intern-symbol name (package InProject))))
    (when publish (sk8:publishSymbol InProject sym))
    sym))

(defun sk8::originalAncestor (me original ancestor)
  (if (sk8::inheritsFrom original ancestor)
    original
    (progn
      (setf original (dolist (c (sk8::parents me)) (when (sk8::inheritsFrom c ancestor) (return c))))
      (or original 
          ;; This should not happen!
          (error "Expected ~a to be a ~a but not!" me ancestor)))))

#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/10/96	sidney  	changed build order of some things
	4  	 7/ 7/96	sidney  	changes for native PPC build
	5  	10/18/96	sidney  	use correct package when referring to SK8 errors
	6  	11/22/96	Hernan  	Defining originalAncestor here.
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
