(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(defconstant_X TC_dsMemFullErr #$dsMemFullErr :private? t :project SK8) ; 25 out of memory!

(defun_X T_NewHandle :private (&optional (size 0))
  "See IM-Memory p. 2-29"
  (declare (type :integer size))
  (checking-Toolbox-Error (:Pointer)
                          (#_NewHandle size)))

(defun_X T_NewHandleGC :private (&optional (size 0))
  "Returns a new GCHandle.
A GCHandle is a GCOSPtr whose termination function is DisposeHandle."
  (makeGCOSPtr
   (checking-toolbox-error (:Pointer) (#_NewHandle size))
   #_DisposeHandle))

(defun_X T_NewPtr :private (&optional (size 0))
  "See IM-Memory p. 2-36"
  (declare (type :integer size))
  (checking-Toolbox-Error (:Pointer)
                          (#_NewPtr size)))

(defun_X T_NewPtrGC :private (size)
  "Returns a new GCPtr.
A GCPtr is a GCOSPtr whose termination function is DisposePtr."
  (makeGCOSPtr
   (checking-toolbox-error (:Pointer) (#_NewPtr size))
   #_DisposePtr))

(defun_X T_DisposeHandle :private (handle)
  "Returns t.
See IM-Memory p. 2-34"
  (declare (type :Handle handle))
  (checking-Toolbox-Error (:void (#_MemError))
                          (#_DisposeHandle handle)))

(defun_X T_GetPtrSize :private (ptr)
  "Returns the ptr size."
  (let ((result (#_GetPtrSize ptr)))
    (when (zerop result)
      (let ((theError (#_MemError)))
        (unless (zerop theError)
          (raiseSystemError 'T_GetPtrSize theError))))
    result))
#|TESTS
(T_GetPtrSize (T_NewPtrGC 5))
(T_GetPtrSize (T_NewPtrGC 0))
|#

(defun_X T_GetHandleSize :private (handle)
  "Returns the handle size."
  (let ((result (#_GetHandleSize handle)))
    (when (zerop result)
      (let ((theError (#_MemError)))
        (unless (zerop theError)
          (raiseSystemError 'T_GetHandleSize theError))))
    result))
#|TESTS
(T_GetHandleSize (T_NewHandleGC 5))
(T_GetHandleSize (T_NewHandleGC 0))
|#

(defun_X T_hLock :private (h)
  (checking-toolbox-error (:void (#_MemError)) (#_hLock h)))

(defun_X T_hUnlock :private (h)
  (checking-toolbox-error (:void (#_MemError)) (#_hunLock h)))

(defmacro_X T_with-hlock (h &body body)
  (let ((h-var (gensym)))
    `(let ((,h-var ,h))
       (unwind-protect 
         (progn (checking-toolbox-error (:void (#_MemError)) (#_hLock ,h-var))
                ,@body)
         (checking-toolbox-error (:void (#_MemError)) (#_hunLock ,h-var))))))

(defun_X T_hNoPurge :private (h)
  (checking-toolbox-error (:void (#_MemError)) (#_hNoPurge h)))


#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	3/1/94	hernan	makeGCOSPtr -> T_makeGCOSPtr.
	3	6/14/94	dy	add TC_dsMemFullErr
	4	7/20/94	dy	privatize symbols
	5  	 8/24/94	dy      	make TC_dsMemFullErr private
	6  	11/28/94	dy      	gc macro mods
	7  	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	8  	 3/16/95	till    	add T_with-hlock
	9  	 3/17/95	till    	add T_getHandleSize
	10 	 3/17/95	till    	add T_hNoPurge
	11 	 3/17/95	till    	maybe not hLock et al.
	2  	11/21/95	Hernan  	Fixed T_hNoPurge to call memError to check for errors.
						We also uncommented the previous three wrappers also
						making them call memerror.
	3  	11/29/95	dy      	new T_GetPtrSize, fixed T_GetHandleSize error checking
	4  	 1/12/96	dy      	some defuns made into defun_X
	3  	 2/27/97	Hernan  	Removing case preserving reader macro.
						2   9/ 4/96Hernan  Putting the traps in sk8dev.
|# ;(do not edit past this line!!)
