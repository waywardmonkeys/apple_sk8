;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

;;; 94-12-13 Dave Yost

(defmacro %boolean-to-lisp (value) `(not (zerop ,value)))
(defmacro %lisp-to-boolean (value) `(if ,value #xff 0))
(defmacro %get-boolean (ptr)       `(%boolean-to-lisp (%get-byte ,ptr)))
(defmacro %put-boolean (ptr value) `(%put-byte ,ptr (%lisp-to-boolean ,value)))

#| Proof:
;;; an independent source of Booleans:
(rlet ((pn1 :ProcessSerialNumber)
       (pn2 :ProcessSerialNumber)
       (result :boolean))
  (rset pn1 ProcessSerialNumber.highLongOfPSN 0)
  (rset pn1 ProcessSerialNumber.lowLongOfPSN  0)
  (rset pn2 ProcessSerialNumber.highLongOfPSN 0)
  (rset pn2 ProcessSerialNumber.lowLongOfPSN  1)
  (format t "Output should be t nil")
  (#_SameProcess pn1 pn1 result)
  (print (%get-boolean result))
  (#_SameProcess pn1 pn2 result)
  (print (%get-boolean result))
  (values))

;;; a local storage place for a Boolean
(rlet ((foo :boolean))
  (format t "Output should be t nil")
  (%put-boolean foo t)
  (print (%get-boolean foo))
  (%put-boolean foo nil)
  (print (%get-boolean foo))
  (values))
|#

(export '(%boolean-to-lisp
          %lisp-to-boolean
          %get-boolean
          %put-boolean)
        :ccl)

#|
	Change History (most recent last):
	1  	12/14/94	dy      	
	2  	 1/25/95	dy      	Change %lisp-to-boolean to return #xFF to be more defensive
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
