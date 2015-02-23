(in-package :graphics-system)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

07-26-92 ruben moves exports into exports.lisp
05-12-92 ruben d29 conversion

|#

(deftype fixed-point () 'short-float)

(defmacro fixed-point-p (thing)
  `(typep ,thing 'short-float))

(defmacro float-to-fixed (a)
  `(coerce ,a 'short-float))

(defmacro ratio-to-fixed (a b)
  `(float-to-fixed (/ ,a ,b)))

(set-dispatch-macro-character #\# #\%
                              #'(lambda (theStream ch something)
                                  (declare (ignore ch something))
                                  (let ((num (read thestream t nil t)))
                                    (unless (and (numberp num) (< -32768 num 32767))
                                      (error "#% requires a number between -32768 and 32767 (~A was supplied)" num))
                                    (float-to-fixed num))))

(defmacro %fixed.+ (a b)
  `(+ ,a ,b))

(defmacro %fixed.- (a b)
  `(- ,a ,b))

(defmacro %fixed.* (a b)
  `(* ,a ,b))

(defmacro %fixed./ (a b)
  `(// ,a ,b))

(defmacro %fixed.int (a)
  `(values (truncate ,a)))

(defmacro %fixed.round (a)
  `(values (round ,a)))

;; The public interfaces:

(defmacro f+ (a &rest rest)
  `(+ ,a ,@rest))

(defmacro fincf (dest &optional (delta 1))
  `(setf ,dest (f+ ,dest ,delta)))

(defmacro f- (a &rest rest)
  `(- ,a ,@rest))

(defmacro fdecf (dest &optional (delta 1))
  `(setf ,dest (f- ,dest ,delta)))

(defmacro fhalf (a)
  `(f/ ,a 2))

(defmacro fdouble (a)
  `(f* ,a 2))

(defmacro faverage (a b)
  `(fhalf (f+ ,a ,b)))

(defmacro f* (a &rest rest)
  (cond (rest
         `(%fixed.* ,a (f* ,(car rest) ,@(cdr rest))))
        (t a)))

(defmacro f/ (a &rest rest)
  (cond (rest
         `(%fixed./ ,a (f* ,(car rest) ,@(cdr rest))))
        (t `(%fixed./ 1 ,a))))

(defmacro fmod (a b)
  `(mod ,a ,b))

(defmacro f.ceiling (a b)
  `(values (ceiling ,a ,b)))

(defmacro f.trunc (a)
  `(%fixed.int ,a))

(defmacro f.round (a)
  `(floor (+ ,a 0.5)))

(defmacro f.float (a)
  a)

(defmacro int-to-fixed (a)
  `(coerce ,a 'short-float))

#|
	Change History (most recent last):
	2	9/1/93	hernan	The great integrating build for d4!!!
	3  	 3/30/95	Hernan  	Changing f.round to predictably round when the number
							is half way from an integer.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
