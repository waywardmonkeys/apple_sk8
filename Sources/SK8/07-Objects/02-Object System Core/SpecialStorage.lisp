;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

(new Object
     :project SK8
     :objectName "SpecialStorageMixin")

(define-handler SK8::restoreOrInitializeFromStore :private (SK8:SpecialStorageMixin whichFunction)
  (declare (ignore whichFunction))
  t)

(define-handler SK8::initializeFromStore :private (SK8:SpecialStorageMixin)
  (and (call-next-method)
       (SK8::restoreOrInitializeFromStore me #'SK8::initializeFromStore)))

(define-handler SK8::restore :private (SK8:SpecialStorageMixin)
  (and (call-next-method)
       (SK8::restoreOrInitializeFromStore me #'SK8::restore)))

(define-handler SK8::preserve :private (SK8:SpecialStorageMixin)
  (call-next-method)
  ;;(preserveLikeSave me)
  (sk8-return-nothing))

#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 7/ 7/96	sidney  	changes for native PPC build
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
