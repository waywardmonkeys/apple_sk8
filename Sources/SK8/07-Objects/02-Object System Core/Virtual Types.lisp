;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated 12-06-94   1:46 pm
                  SK8::BOUNDSRECTLIST SK8::POINTLIST SK8::POLYGONPOINTSLIST)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  VirtualType
;;;

(new Object :project SK8 :objectName "VirtualType")

#|
  (define-handler initialize (VirtualType original isNew initargs)
  (declare (ignore original isNew initargs))
  (call-next-method)
  (MF::virtual-type!-inline me t))
|#

(define-handler typeSatisfied (VirtualType obj)
  (declare (ignore obj))
  nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  EnumeratedType
;;;

(new VirtualType :project SK8 :objectName "EnumeratedType" :properties '(options))

;;; So that the prototype "EnumeratedType" itself isn't treated as a virtual type!
;;(mf::virtual-type!-inline EnumeratedType nil)

(define-handler typeSatisfied (EnumeratedType obj)
  (when (memq obj (options me))
    t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  A couple basic virtual types...
;;;

(new VirtualType :project SK8 :objectName "PositiveInteger")
(define-handler typeSatisfied (PositiveInteger obj)
  (and (integerp obj) (plusp obj)))

(new VirtualType :project SK8 :objectName "NonNegativeInteger")
(define-handler typeSatisfied (NonNegativeInteger obj)
  (and (integerp obj) (not (minusp obj))))

;;; And more for easy checking in 2d...

(new VirtualType :project SK8 :objectName "PointList")
(define-handler typeSatisfied (PointList obj)
  (and (listp obj) (= (length obj) 2)))

(new VirtualType :project SK8 :objectName "BoundsRectList")
(define-handler typeSatisfied (BoundsRectList obj)
  (and (listp obj) (= (length obj) 4)))

(new VirtualType :project SK8 :objectName "PolygonPointsList")
(define-handler typeSatisfied (PolygonPointsList obj)
  (and (listp obj) 
       (>= (length obj) 6)
       (evenp (length obj))))


#|
	Change History (most recent last):
	1  	 9/19/94	chip    	new file
	2  	12/ 7/94	Hernan  	Added virtual types to facilitate type checking.
	2  	 5/ 7/96	sidney  	Changes for new object system
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
