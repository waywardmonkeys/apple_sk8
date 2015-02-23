;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)



(define-sk8-function bound nil (sym)
  (boundp sym))

(define-sk8-function functional nil (sym)
  (when (fboundp sym) t))

(define-sk8-function constant nil (sym)
  (constantp sym))

(define-sk8-function even nil (int)
  (evenp int))

(define-sk8-function untrue nil (thing)
  (null thing))

(define-sk8-function odd nil (int)
  (oddp int))

(define-sk8-function positive nil (num)
  (plusp num))

(define-sk8-function negative nil (num)
  (minusp num))



#|
	Change History (most recent last):
	7  	 9/14/94	chip    	added 'functional' (radar #1186546)
	8  	11/16/94	chip    	removed null.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
