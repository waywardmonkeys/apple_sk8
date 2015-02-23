;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;;========================================================

(export '(with-temporary-ptr
           with-temporary-handle))

(defmacro with-temporary-ptr ((thePtr
                                  &optional
                                  ptr-generator
                                  &body
                                  disposal)
                                 &body body)
  "Allocate, use, then dispose a Ptr.
ptr-generator defaults to (T_NewPtr 4).
disposal defaults to (T_DisposePtr thePtr)."
  `(let ((,thePtr ,(if ptr-generator
                     ptr-generator
                     `(T_NewPtr 4))))
     (unwind-protect
       (progn ,@body)
       ,(if disposal
          `(progn ,@disposal)
          `(T_DisposePtr ,thePtr)))))

(defmacro with-temporary-handle ((theHandle
                                      &optional
                                      handle-generator
                                      &body
                                      disposal)
                                     &body body)
  "Allocate, use, then dispose a handle.
ptr-generator defaults to (T_NewHandle 4).
disposal defaults to (T_DisposePtr theHandle)."
  `(let ((,theHandle ,(if handle-generator
                        handle-generator
                        `(T_NewHandle 4))))
     (unwind-protect
       (progn ,@body)
       ,(if disposal
          `(progn ,@disposal)
          `(T_DisposeHandle ,theHandle)))))
#| TESTS
(macroexpand '(with-temporary-handle (thing (newthingy)
                                 (whatever)
                                 (dispose-thingy thing))
  (foo)))
  
(macroexpand '(with-temporary-handle (thing (newthingy))
                (foo)))
  
(macroexpand '(with-temporary-handle (thing)
                (foo)))
  
|#


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
