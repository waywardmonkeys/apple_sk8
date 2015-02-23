(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :private
                  sk8::T_KeyTranslate
)

(let (previousTransData)
  (defun_X T_KeyTranslate :private (keycode &optional
                                             (transData previousTransData))
     "See IM-Macintosh Toolbox Essentials, p. 2-110"
     (prog1
       (rlet ((state :long (if (eq previousTransData transData) 1 0)))
         (#_keytrans transData keyCode state))
       (setf previousTransData transData))))


#|
	Change History (most recent last):
	1  	10/26/94	rod     	
	2  	10/26/94	rod     	T_KeyTrans -> T_KeyTranslate
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
