(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; Make sure that every Handle or Pointer has onone SK8 object representing it.

(defvar *osptrs-represented-by-sk8-objects* (ccl::%cons-population nil))
(defvar *osptr-to-sk8-object-table* (make-hash-table :weak :value :test 'eql))

;;; Use this instead of "new QuickTimeTimeBase"!
(defun sk8ObjectRepresentingOSPtr (osPtr)
  (and (member osPtr
               (ccl::population-data *osptrs-represented-by-sk8-objects*))
       (gethash osPtr *osptr-to-sk8-object-table*)))

(defun registerSk8ObjectRepresentingOSPtr (sk8Object osPtr
                                                           &key
                                                           masterGCOSPtr
                                                           slaveGCOSPtr)
  (cond
   (masterGCOSPtr
    (when slaveGCOSPtr
      (sk8-error IncorrectArgumentsError
                 :handlername 'registerSk8ObjectRepresentingOSPtr
                 :arguments (list :masterGCOSPtr masterGCOSPtr :slaveGCOSPtr slaveGCOSPtr)))
    (when (terminable-macptr-p masterGCOSPtr)
      (register-slave-macptr osPtr masterGCOSPtr)))
   (slaveGCOSPtr
    (when (terminable-macptr-p osPtr)
      (register-slave-macptr slaveGCOSPtr osPtr))))
  (push osPtr (ccl::population-data *osptrs-represented-by-sk8-objects*))
  (setf (gethash osPtr *osptr-to-sk8-object-table*)
        sk8Object)
  ;; (clear-hashtable-cache *osptr-to-sk8-object-table*)
  sk8Object)


#|
	Change History (most recent last):
	1  	 3/ 6/95	dy      	new
	2  	 4/28/95	dy      	add slaveOSPtr keyword to registerSk8ObjectRepresentingOSPtr for QTTimeBase
	3  	 2/27/97	Hernan  	Removing case preserving reader macro.
						2   9/23/96Hernan  Commenting out call to clear-hashtable-cache.
|# ;(do not edit past this line!!)
