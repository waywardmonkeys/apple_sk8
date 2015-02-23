#+SK8
(in-package :SK8DEV)

(provide "SCRAPTRAPS")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#+SK8
(SK8-declare-syms :SK8 :private
                  SK8::T_ZeroScrap
                  SK8::T_PutScrap
                   )

(defun_X T_ZeroScrap :private ()
  "IM-More Macintosh Toolbox, p. 2-35"
  (checking-toolbox-error (:OSErr)
                          (#_ZeroScrap)))

(defun_X T_PutScrap :private (type ptr &optional size)
  "N.B. The argument order is different so that size can be optional.
IM-More Macintosh Toolbox, p. 2-36"
  (checking-toolbox-error (:OSErr)
                          (#_PutScrap
                           (or size (T_GetPtrSize ptr))
                           type
                           ptr)))


#|
	Change History (most recent last):
	1  	11/28/95	dy      	new file
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
