(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




(defmacro checking-error-make-record (recordType &body forms)
  `(checking-toolbox-error (:Pointer) (make-record ,recordType ,@forms)))

(defmacro newRecordHandle (recordType &rest initForms)
  `(checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Handle) ,@initForms)))

(defmacro newRecordGCHandle (recordType &body initForms)
  `(handleToGCHandle (checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Handle) ,@initForms))))

(defmacro newRecordHandleClear (recordType &body initForms)
  `(checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Handle :clear t) ,@initForms)))

(defmacro newRecordGCHandleClear (recordType &body initForms)
  `(handleToGCHandle (checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Handle :clear t) ,@initForms))))

(defmacro newRecordPtr (recordType &body initForms)
  `(checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Pointer) ,@initForms)))

(defmacro newRecordPtrClear (recordType &body initForms)
  `(checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Pointer :clear t) ,@initForms)))

(defmacro newRecordGCPtr (recordType &body initForms)
  `(ptrToGCPtr (checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Pointer) ,@initForms))))

(defmacro newRecordGCPtrClear (recordType &body initForms)
  `(ptrToGCPtr (checking-toolbox-error (:Pointer) (make-record (,recordType :storage :Pointer :clear t) ,@initForms))))

(defmacro checking-error-dispose-record (&body forms)
  `(checking-toolbox-error (:None (require-trap #_MemError)) (dispose-record ,@forms)))

(defmacro T_DisposeRecordHandle (recordType)
  `(checking-error-dispose-record (,recordType :storage :Handle)))

(defmacro T_DisposeRecordPtr (recordType)
  `(checking-error-dispose-record (,recordType :storage :Pointer)))

(defun_X getRecordField nil (obj recordType fieldName)
  "Given a handle or pointer to a low-level record of the given recordType (a symbol),
get the field named fieldName (a symbol)"
  (get-record-field
   obj
   (intern (symbol-name recordType) :keyword)
   (intern (symbol-name fieldName) :keyword)))

(defun_X setRecordField nil (obj recordType fieldName newValue)
  "Given a handle or pointer to a low-level record of the given recordType (a symbol),
set the field named fieldName (a symbol) to newValue"
  (set-record-field
   obj
   (intern (symbol-name recordType) :keyword)
   (intern (symbol-name fieldName) :keyword)
   newValue))

(defmacro T_getRecordPtrContents (ptr recordType)
  "Returns a hash table of the record contents.
Not a trap."
  `(let* ((fieldList (record-fields ,recordType))
          (dict (make-hash-table :size (length fieldList))))
     (dolist (field fieldList)
       (setf (gethash field dict) (GetRecordField ,ptr ,recordType field)))
     dict))

(defmacro T_getRecordHandleContents (handle recordType)
  "Returns a hash table of the record contents.
Not a trap."
  `(with-dereferenced-handles ((ptr ,handle))
     (T_GetRecordPtrContents ptr ,recordType)))

(defmacro T_setRecordPtrContents (ptr recordType dict)
  "Sets the fields of a record to the contents of a corresponding hash table.
Not a trap."
  `(progn
     (loop
       for key being each hash-key in ,dict using (hash-value value)
       do (SetRecordField ,ptr ,recordType key value))
     ,ptr))

(defmacro T_setRecordHandleContents (handle recordType dict)
  "Gets the fields of a record into a hash table.
Not a trap."
  `(with-dereferenced-handles ((ptr ,handle))
     (T_SetRecordPtrContents ptr ,recordType ,dict)))

;;; These 4 have to be macros because the record-length macro will only accept a literal
(defmacro copyRecordHandleToRecordHandle (fromHandle toHandle recordType)
  `(progn
     (with-dereferenced-handles ((fromPtr ,fromHandle)
                                 (toPtr   ,toHandle))
       (require-trap #_BlockMove fromPtr toPtr (record-length ,recordType)))
     ,toHandle))

(defmacro copyRecordPtrToRecordHandle (fromPtr toHandle recordType)
  `(progn
     (with-dereferenced-handles ((toPtr ,toHandle))
       (require-trap #_BlockMove ,fromPtr toPtr (record-length ,recordType)))
     ,toHandle))

(defmacro copyRecordHandleToRecordPtr (fromHandle toPtr recordType)
  `(progn
     (with-dereferenced-handles ((fromPtr ,fromHandle))
       (require-trap #_BlockMove fromPtr ,toPtr (record-length ,recordType)))
     ,toPtr))

(defmacro copyRecordPtrToRecordPtr (fromPtr toPtr recordType)
  `(progn
     (require-trap #_BlockMove ,fromPtr ,toPtr (record-length ,recordType))
     ,toPtr))

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	3/1/94	hernan	handleToGCHandle -> T_handleToGCHandle and
				ptrToGCPtr -> T_ptrToGcPtr.
	3	4/6/94	dy	add 4 macros to copy records
	4	4/6/94	dy	fix copyRecordXXX macros
	5  	11/28/94	dy      	gc macro mods
	6  	 1/25/95	dy      	uncapitalize some function names
	7  	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	8  	 3/ 9/95	dy      	remove T_ from  T_copyRecordxxx macro names
	9  	 3/ 9/95	dy      	Use require-trap!
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
