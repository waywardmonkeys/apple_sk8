(in-package :sk8dev)


;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(defun_X T_NewRgn nil ()
  "Returns a :Region handle.
See IM-Imaging p. ???"
  (checking-toolbox-error (:pointer) (#_NewRgn)))

(defun_X T_NewRgnGC nil ()
  "Returns a :Region GCHandle.
See IM-Imaging p. ???"
  (makeGCOSPtr (checking-toolbox-error (:pointer) (#_NewRgn))
                 #_DisposeRgn))

(defun_X T_NewPixPatGC nil ()
  "Returns a :Region GCHandle.
See IM-Imaging p. ???"
  (makeGCOSPtr (checking-toolbox-error (:pointer) (#_NewPixPat))
                 #_DisposePixpat))
    
(defun_X T_GetCIconGC nil (resId)
  (makeGCOSPtr (checking-toolbox-error (:pointer
                                          (#_ResError)
                                          :error-result #$resNotFound)
                                         (#_GetCicon resid))
                 #_DisposCIcon))

(defun_X T_GetPixPatGC nil (resId)
  (makeGCOSPtr (checking-toolbox-error (:pointer
                                          (#_ResError)
                                          :error-result #$resNotFound)
                                         (#_GetPixPat resid))
                 #_DisposePixPat))

(defun_X T_GetCCursorGC nil (resId)
  (makeGCOSPtr (checking-toolbox-error (:pointer
                                          (#_ResError)
                                          :error-result #$resNotFound)
                                         (#_GetCCursor resid))
                 #_DisposCCursor))
    
;;; Some helper functions

(defun_X T_rectRecordToRectArray :private (rectRecordPtr)
  "takes a macptr to a Rect record and returns
a lisp array of numbers in left top right bottom order.
Not a trap."
  (let ((rectArray (make-array 4)))
    (setf (aref rectArray 0) (rref rectRecordPtr rect.left))
    (setf (aref rectArray 1) (rref rectRecordPtr rect.top))
    (setf (aref rectArray 2) (rref rectRecordPtr rect.right))
    (setf (aref rectArray 3) (rref rectRecordPtr rect.bottom))
    rectArray))

(defun_X T_rectRecordValues :private (rectRecordPtr)
  "takes a macptr to a Rect record and returns
four values: left top right bottom.
Not a trap."
  (values_X
    (rref rectRecordPtr rect.left)
    (rref rectRecordPtr rect.top)
    (rref rectRecordPtr rect.right)
    (rref rectRecordPtr rect.bottom)))

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	3/1/94	hernan	makeGCOSPtr -> T_makeGCOSPtr.
	3	3/18/94	Hernan	Adding T_newPixPatGC.
	4	3/23/94	Hernan	Adding functions to get cicns, ppats and crsrs
				checking for errors and making them gcable.
	5	3/23/94	dy	move resource calls to new Resources.lisp file
	6	6/9/94	till	Move the symbol exports to sk8PublicSyms
	7  	 9/ 6/94	dy      	checking-toolbox-error :error-result rework
	8  	11/28/94	dy      	gc macro mods
	9  	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	10 	 4/ 3/95	dy      	Move T_rectRecordToRectArray here; add SK8::T_rectRecordValues
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
