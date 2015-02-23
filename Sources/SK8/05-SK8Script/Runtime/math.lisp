;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)


;;; Do it the long way so that this file can be intelligently parsed

(mf::publish-function-symbol 'ABS  mf::*SK8-package*)
(mf::publish-function-symbol 'ACOS  mf::*SK8-package*)
(mf::publish-function-symbol 'ACOSH  mf::*SK8-package*)
(mf::publish-function-symbol 'ASH  mf::*SK8-package*)
(mf::publish-function-symbol 'ASIN  mf::*SK8-package*)
(mf::publish-function-symbol 'ASINH  mf::*SK8-package*)
(mf::publish-function-symbol 'ATAN  mf::*SK8-package*)
(mf::publish-function-symbol 'ATANH  mf::*SK8-package*)
(mf::publish-function-symbol 'CEILING  mf::*SK8-package*)
(mf::publish-function-symbol 'COS  mf::*SK8-package*)
(mf::publish-function-symbol 'COSH  mf::*SK8-package*)
(mf::publish-function-symbol 'EXP  mf::*SK8-package*)
(mf::publish-function-symbol 'EXPT  mf::*SK8-package*)
(mf::publish-function-symbol 'FLOOR  mf::*SK8-package*)
(mf::publish-function-symbol 'GCD  mf::*SK8-package*)
(mf::publish-function-symbol 'imagPart  mf::*SK8-package*)
(mf::publish-function-symbol 'LCM  mf::*SK8-package*)
(mf::publish-function-symbol 'LOG  mf::*SK8-package*)
(mf::publish-function-symbol 'MAX  mf::*SK8-package*)
(mf::publish-function-symbol 'MIN  mf::*SK8-package*)
(mf::publish-function-symbol 'PHASE  mf::*SK8-package*)
(mf::publish-function-symbol 'RANDOM  mf::*SK8-package*)
(mf::publish-function-symbol 'realPart  mf::*SK8-package*)
(mf::publish-function-symbol 'ROUND  mf::*SK8-package*)
(mf::publish-function-symbol 'SIN  mf::*SK8-package*)
(mf::publish-function-symbol 'SINH  mf::*SK8-package*)
(mf::publish-function-symbol 'SQRT  mf::*SK8-package*)
(mf::publish-function-symbol 'TAN  mf::*SK8-package*)
(mf::publish-function-symbol 'TANH  mf::*SK8-package*)
(mf::publish-function-symbol 'TRUNCATE  mf::*SK8-package*)

;; SQUAREROOT, SQRT
(mf::publish-function-symbol 'sqrt  mf::*SK8-package*)
(mf::publish-function-symbol 'squareRoot  mf::*SK8-package*)
(setf (symbol-function 'squareRoot) #'sqrt)

;;nicities
(define-sk8-function sign nil (ANumber)
  (signum ANumber))
(define-sk8-function dollarString nil (num)
  (unless (numberp num) (SK8-error TypeMismatchError :object num :expectedType Number))
  (format nil "$~0,2f" num))



(define-sk8-constant Pi pi :project SK8 :register t)


;;_____________________________________________________________________
#|
(define-handler minimum (Number &rest moreNumbers)
  (declare (dynamic-extent moreNumbers))
  (apply 'min me moreNumbers))

(define-handler maximum (Number &rest moreNumbers)
  (declare (dynamic-extent moreNumbers))
  (apply 'max me moreNumbers))
|#


#|
	Change History (most recent last):
	8		1/11/94	hernan	self -> me
	9		2/12/94	kleiman	lcd
	10		4/4/94	kleiman	added <,>,=,<=.>=
	11 	 9/ 1/94	chip    	publish-project-symbol --> publish-global/function-symbol where necessary (radar #1183935)
	12 	 9/ 2/94	chip    	took out extraneous publishes
	13 	11/15/94	chip    	minimum and maximum now work on numbers in addition to lists
	14 	11/20/94	chip    	added realPart & imagPart
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	3  	 4/16/96	Brian   	
	4  	 4/22/96	sidney  	get rid of minimum and maximum handlers, defined as functions elsewhere
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
