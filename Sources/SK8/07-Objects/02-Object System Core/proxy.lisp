;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; proxy object  -- should this be part of the core object system?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The Proxy Object.
(sk8:new sk8:object
         :project sk8:sk8
         :objectName "Proxy"
         :properties '(originalObject))

(defmethod SK8:makeProxyFor ((me SK8:Object) &key objectName)
  (let ((peroxide (new SK8:Proxy
                       :project (SK8:project me)
                       :objectName objectName)))
    (setf (SK8:originalObject peroxide) me)
    peroxide))

(defmethod SK8:MakeObjectAProxy ((me SK8:Object) ObjectProxied &key)
  (progn
    (SK8:addparent me SK8:proxy)
    (setf (SK8:originalObject me) ObjectProxied)
    me))


;;; Note that SETFs present a special case because their arguements are in a funny order.
;;; We do our best to guess (ie., there's no sure fire way to tell if this is a setf method
;;; or not.)
(defmethod ccl::no-applicable-method :around ((func t) &rest args)
  (let ((proxyClass (class-of Proxy))
        first-obj second-obj orig)
    (cond 
     ;; proxy case
     ((and (typep (setq first-obj (first args)) proxyClass)
           (setq orig (originalObject first-obj)))
      (apply func orig (rest args)))
     ;; possibly a setf
     ((and (setq second-obj (cadr args))
           (null (cddr args))
           (typep second-obj proxyClass)
           (setq orig (originalObject second-obj)))
      (apply func first-obj (originalObject second-obj) nil))
     ;; give up
     (t 
      (call-next-method)))))


#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
