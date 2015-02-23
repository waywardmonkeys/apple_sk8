;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)


;;; **************************************************************************
;;; GLOBALS
;;; **************************************************************************

;;; filename of wood file with SK8 documentation
(defvar *wood-doc-db-filename* "ccl;SK8 Documentation")



;;; **************************************************************************
;;; CLASSES
;;; **************************************************************************

(defclass wood_doc_root_object ()
  ((object-btree :accessor object-btree :initarg :object-btree)
   (object-names :accessor object-names :initform nil)
   (function-btree :accessor function-btree :initarg :function-btree)
   (function-names :accessor function-names :initform nil)
   (constant-btree :accessor constant-btree :initarg :constant-btree)
   (constant-names :accessor constant-names :initform nil)
   (variable-btree :accessor variable-btree :initarg :variable-btree)
   (variable-names :accessor variable-names :initform nil))
  )

(defclass object_ ()
  ((name :accessor object_name :initarg :name :initform nil)
   (info :accessor object_info :initarg :info :initform nil)
   (owner :accessor object_owner :initarg :owner :initform nil)
   (description :accessor object_description :initarg :description :initform nil)
   (example :accessor object_example :initarg :example :initform nil)
   (seealso :accessor object_seealso :initarg :seealso :initform nil)
   (properties :accessor object_properties :initarg :properties :initform nil)
   (handlers :accessor object_handlers :initarg :handlers :initform nil))
  )

(defmethod save ((me object_))
  (declare (special *doc-pheap* *doc-root*))
  (let ((key (string-upcase (object_name me))))
    (setf (wood::p-btree-lookup (object-btree *doc-root*) key)
          (wood::p-store *doc-pheap* me))
    (push key (object-names *doc-root*))
    (wood::p-store  *doc-pheap* *doc-root* t)))

(defclass argument_ ()
  ((name :accessor argument_name :initarg :name :initform nil)
   (type :accessor argument_type :initarg :type :initform nil)
   (required :accessor argument_required :initarg :required :initform nil)
   (description :accessor argument_description :initarg :description :initform nil))
  )

(defclass handler_ ()
  ((name :accessor handler_name :initarg :name :initform nil)
   (info :accessor handler_info :initarg :info :initform nil)
   (description :accessor handler_description :initarg :description :initform nil)
   (arguments :accessor handler_arguments :initarg :arguments :initform nil)
   (example :accessor handler_example :initarg :example :initform nil)
   (seealso :accessor handler_seealso :initarg :seealso :initform nil))
  )

(defclass property_ ()
  ((name :accessor property_name :initarg :name :initform nil)
   (info :accessor property_info :initarg :info :initform nil)
   (description :accessor property_description :initarg :description :initform nil)
   (getter_description :accessor property_getter_description :initarg :getter_description :initform nil)
   (getter_arguments :accessor property_getter_arguments :initarg :getter_arguments :initform nil)
   (setter_description :accessor property_setter_description :initarg :setter_description :initform nil)
   (setter_arguments :accessor property_setter_arguments :initarg :setter_arguments :initform nil)
   (example :accessor property_example :initarg :example :initform nil)
   (seealso :accessor property_seealso :initarg :seealso :initform nil))
  )

(defclass function_ ()
  ((name :accessor function_name :initarg :name :initform nil)
   (info :accessor function_info :initarg :info :initform nil)
   (syntax :accessor function_syntax :initarg :syntax :initform nil)
   (description :accessor function_description :initarg :description :initform nil)
   (arguments :accessor function_arguments :initarg :arguments :initform nil)
   (example :accessor function_example :initarg :example :initform nil)
   (seealso :accessor function_seealso :initarg :seealso :initform nil))
  )

(defmethod save ((me function_))
  (declare (special *doc-pheap* *doc-root*))
  (let ((key (string-upcase (function_name me))))
    (setf (wood::p-btree-lookup (function-btree *doc-root*) key)
          (wood::p-store *doc-pheap* me))
    (push key (function-names *doc-root*))))

(defclass constant_ ()
  ((name :accessor constant_name :initarg :name :initform nil)
   (info :accessor constant_info :initarg :info :initform nil)
   (description :accessor constant_description :initarg :description :initform nil)
   (example :accessor constant_example :initarg :example :initform nil)
   (seealso :accessor constant_seealso :initarg :seealso :initform nil))
  )

(defmethod save ((me constant_))
  (declare (special *doc-pheap* *doc-root*))
  (let ((key (string-upcase (constant_name me))))
    (setf (wood::p-btree-lookup (constant-btree *doc-root*) key)
          (wood::p-store *doc-pheap* me))
    (push key (constant-names *doc-root*))))

(defclass variable_ ()
  ((name :accessor variable_name :initarg :name :initform nil)
   (info :accessor variable_info :initarg :info :initform nil)
   (description :accessor variable_description :initarg :description :initform nil)
   (example :accessor variable_example :initarg :example :initform nil)
   (seealso :accessor variable_seealso :initarg :seealso :initform nil))
  )

(defmethod save ((me variable_))
  (declare (special *doc-pheap* *doc-root*))
  (let ((key (string-upcase (variable_name me))))
    (setf (wood::p-btree-lookup (variable-btree *doc-root*) key)
          (wood::p-store *doc-pheap* me))
    (push key (variable-names *doc-root*))))


;;; **************************************************************************
;;; DOC DB ACCESS FUNCTIONS
;;; **************************************************************************

;;; Saves all wood operations to the specified filename
(defmacro with-wood-doc-file ((filename) &body body)
  `(let ((*doc-pheap* (wood::open-pheap ,filename :if-does-not-exist :create :write-p t)))
     (declare (special *doc-pheap*))
     (unwind-protect
       (progn
         (unless (wood::root-object *doc-pheap*)
           (setf (wood::root-object *doc-pheap*)
                 (wood::p-store
                  *doc-pheap*
                  (make-instance 'wood_doc_root_object
                    :object-btree (wood::p-make-btree *doc-pheap*)
                    :function-btree (wood::p-make-btree *doc-pheap*)
                    :constant-btree (wood::p-make-btree *doc-pheap*)
                    :variable-btree (wood::p-make-btree *doc-pheap*)
                    ))))
         (let ((*doc-root* (wood::p-load (wood::root-object *doc-pheap*) :single)))
           (declare (special *doc-root*))
           ,@body))
       (wood::close-pheap *doc-pheap*)))
  )

(defmacro with-help-file (&body body)
  `(with-wood-doc-file (*wood-doc-db-filename*)
     ,@body))

;;; fetch an object_ given its name (a string)
(defun fetch-object (name)
  (declare (special *doc-root*))
  (setq name (string-upcase name))
  (let ((object_ (wood::p-load (wood::p-btree-lookup (object-btree *doc-root*) name) :single)))
    (when object_
      (setf (object_name object_) (wood::p-load (object_name object_)))
      (setf (object_info object_) (wood::p-load (object_info object_)))
      (setf (object_owner object_) (wood::p-load (object_owner object_)))
      (setf (object_description object_) (wood::p-load (object_description object_)))
      (setf (object_example object_) (wood::p-load (object_example object_)))
      (setf (object_seealso object_) (wood::p-load (object_seealso object_)))
      object_)))

;;; returns a list with the names of all of the documented objects in alphabetical order
(defun fetch-all-object-keys ()
  (declare (special *doc-root*))
  (sort (wood::p-load (object-names *doc-root*))
        #'(lambda (a b) (string< a b))))

;;; fetch a handler given an object_ and the handler name
(defun fetch-handler (object_ name)
  (declare (special *doc-root*))
  (dolist (h (wood::p-load (object_handlers object_) :single))
    (when (string-equal (handler_name (setq h (wood::p-load h)))
                      name)
      (return h))))

(defun fetch-property (object_ name)
  (declare (special *doc-root*))
  (dolist (p (wood::p-load (object_properties object_) :single))
    (if (string-equal (property_name (setq p (wood::p-load p)))
                      name)
      (return p))))

(defun fetch-function (name)
  (declare (special *doc-root*))
  (setq name (string-upcase name))
  (let ((function_ (wood::p-load (wood::p-btree-lookup (function-btree *doc-root*) name) :single)))
    (when function_
      (setf (function_name function_) (wood::p-load (function_name function_)))
      (setf (function_info function_) (wood::p-load (function_info function_)))
      (setf (function_syntax function_) (wood::p-load (function_syntax function_)))
      (setf (function_description function_) (wood::p-load (function_description function_)))
      (setf (function_arguments function_) (wood::p-load (function_arguments function_)))
      (setf (function_example function_) (wood::p-load (function_example function_)))
      (setf (function_seealso function_) (wood::p-load (function_seealso function_)))
      function_
      )))

(defun fetch-constant (name)
  (declare (special *doc-root*))
  (setq name (string-upcase name))
  (let ((constant_ (wood::p-load (wood::p-btree-lookup (constant-btree *doc-root*) name) :single)))
    (when constant_
      (setf (constant_name constant_) (wood::p-load (constant_name constant_)))
      (setf (constant_info constant_) (wood::p-load (constant_info constant_)))
      (setf (constant_description constant_) (wood::p-load (constant_description constant_)))
      (setf (constant_example constant_) (wood::p-load (constant_example constant_)))
      (setf (constant_seealso constant_) (wood::p-load (constant_seealso constant_)))
      constant_
      )))

(defun fetch-variable (name)
  (declare (special *doc-root*))
  (setq name (string-upcase name))
  (let ((variable_ (wood::p-load (wood::p-btree-lookup (variable-btree *doc-root*) name))))
    (when variable_
      (setf (variable_name variable_) (wood::p-load (variable_name variable_)))
      (setf (variable_info variable_) (wood::p-load (variable_info variable_)))
      (setf (variable_description variable_) (wood::p-load (variable_description variable_)))
      (setf (variable_example variable_) (wood::p-load (variable_example variable_)))
      (setf (variable_seealso variable_) (wood::p-load (variable_seealso variable_)))
      variable_
      )))

;;; maps function over each object entry in the doc db
(defun map-object-doc-entries (fun)
  (declare (special *doc-root*))
  (wood::p-map-btree (object-btree *doc-root*)
                     #'(lambda (key object_)
                         (declare (ignore key))
                         (setq object_ (wood::p-load object_ :single))
                         (setf (object_name object_) (wood::p-load (object_name object_)))
                         (setf (object_info object_) (wood::p-load (object_info object_)))
                         (setf (object_owner object_) (wood::p-load (object_owner object_)))
                         (setf (object_description object_) (wood::p-load (object_description object_)))
                         (setf (object_example object_) (wood::p-load (object_example object_)))
                         (setf (object_seealso object_) (wood::p-load (object_seealso object_)))
                         (funcall fun object_))))

;;; maps function over each property entry in the doc db object entry

(defun map-property-doc-entries (fun object_ &optional (sorted? t))
  (let ((props (wood::p-load (object_properties object_) :single)))
    (when sorted?
      (setf props (sort props #'string-lessp :key #'(lambda (x) (property_name (wood::p-load x))))))
    (dolist (p props)
      (funcall fun (wood::p-load p)))))

;;; maps function over each handler entry in the doc db object entry
(defun map-handler-doc-entries (fun object_ &optional (sorted? t))
  (let ((props (wood::p-load (object_handlers object_) :single)))
    (when sorted?
      (setf props (sort props #'string-lessp :key #'(lambda (x) (handler_name (wood::p-load x))))))
    (dolist (p props)
      (funcall fun (wood::p-load p)))))

;;; maps function over each function entry in the doc db

(defun map-function-doc-entries (fun &optional (sorted? t))
  (declare (special *doc-root*))
  (let ((funs (wood::p-load (cl-user::function-names cl-user::*doc-root*))))
    (when sorted?
      (setf funs (sort funs #'string-lessp)))
    (dolist (oneF funs)
      (funcall fun (fetch-function oneF)))))

(defun map-constant-doc-entries (fun &optional (sorted? t))
  (declare (special *doc-root*))
  (let ((funs (wood::p-load (cl-user::constant-names cl-user::*doc-root*))))
    (when sorted?
      (setf funs (sort funs #'string-lessp)))
    (dolist (oneF funs)
      (funcall fun (fetch-constant oneF)))))

(defun map-variable-doc-entries (fun &optional (sorted? t))
  (declare (special *doc-root*))
  (let ((funs (wood::p-load (cl-user::variable-names cl-user::*doc-root*))))
    (when sorted?
      (setf funs (sort funs #'string-lessp)))
    (dolist (oneF funs)
      (funcall fun (fetch-variable oneF)))))

#| test area
(with-help-file (fetch-object "ABORT"))
(ccl::arglist (fboundp (find-symbol (string-upcase "getregularpolygonfromuser") :sk8)))
(with-help-file
  (map-function-doc-entries
   #'(lambda (i) (print (list (function_name i) (function_syntax i))))))
(with-help-file
  (let ((p (find-package :cl-user)))
    (dolist (n (fetch-all-object-keys))
      (when (eq (symbol-package n) p)
        (print (list n (fetch-object (symbol-name n))))))))

(with-help-file (inspect (fetch-object "Actor")))
  (let ((names (object_handlers (fetch-object "Actor"))))
    (print names)))

(inspect
 (with-help-file
  (fetch-handler (fetch-object "Actor") "setboundsRect")))
|#

#|
	Change History (most recent last):
	1		8/1/94	Hernan	New file. The API for talking to WOOD and the classes
							that we save.
	2		8/2/94	Hernan	Should not get a wood error when trying to reopen
							a wood doc file.
	3		8/2/94	Hernan	with-wood-doc-file no longer removes the old
							file. If you are building the documentation file
							you have to delete it yourself!
	4		8/3/94	kleiman	added *wood-doc-db-filename*
	5		8/3/94	kleiman	save for constant_
	6		8/4/94	kleiman	enhancements
	7		8/4/94	sidney	Fix glitch in source files introduced by SourceServer interface
	8		8/4/94	kleiman	enhancements
	9		8/4/94	kleiman	with-help-file speedup
	10		8/4/94	rod	little buggy... very tiny... almost nothing--really
	11 	 8/24/94	Hernan  	1181020: Making the doc file be inside the SK8 folder.
	12 	11/ 9/94	Hernan  	Adding the write-p argument to the call to open-pheap.
	13 	11/16/94	sidney  	move SK8 Documentation to the new SK8 Resources folder
	14 	 2/22/95	sidney  	1221621: no more SK8 Resources folder
	15 	 4/25/95	Hernan  	Sorting properties, handlers and functions.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
