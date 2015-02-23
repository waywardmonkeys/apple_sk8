;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;; This file will contain the utilities that allows projects to be saved
;;; requiring the right stuff...

;;; _______________________________ 
;;; Record everything in SK8 at the time of core SK8.
;;; _______________________________ 

;;; These variables are used to record, at build time, every SK8 thing that
;;; is available automatically to the user (as part of the Core build). This
;;; is required to avoid requiring files that are already available. The vars
;;; should be cleared when we build a standalone (ie. they should not be present
;;; in standalones).

(defvar *sk8-core-objects* nil)
(defvar *sk8-core-functions* nil)
(defvar *sk8-core-constants* nil)
(defvar *sk8-core-globals* nil)

(defun register-sk8-core-things ()
  (setf *sk8-core-functions* (functions SK8))
  (setf *sk8-core-constants* (constants SK8))
  (setf *sk8-core-globals* (globals SK8))
  (let ((objs nil))
    (mapprojectobjects SK8 #'(lambda (thing)
                               (when (objectName thing)
                                 (push thing objs))))
    (setf *sk8-core-objects* objs)))

;;; Actually register the state of the world. This should be called after the graphics system comes up.

(register-sk8-core-things)

(defun clear-sk8-core-things ()
  (makunbound *sk8-core-objects*)
  (makunbound *sk8-core-functions*)
  (makunbound *sk8-core-constants*)
  (makunbound *sk8-core-globals*))

(defun inCoreSK8 (thing)
  (cond ((and (symbolp thing) (fboundp thing))
         (when (memq (symbol-function thing) *sk8-core-functions*)
           t))
        ((functionp thing) (when (memq thing *sk8-core-functions*)
                             t))
        ((inheritsFrom thing object) 
         (when (memq thing *sk8-core-objects*)
           t))))
       
;;; _______________________________ 
;;; (1) Find every SK8 thing used by a project.
;;; _______________________________ 

(defparameter *all-used-sk8-objects* nil)

(defun valid-used-sk8-obj (x)
  (and (eq (project x) sk8)
       (objectname x)
       (or (not (is-a x actor)) (not (container x)))
       (or (not (is-a x sk8::menuitem)) (not (sk8::menu x)))
       (or (not (is-a x sk8::menu)) (not (sk8::menubar x)))))

;;; This is the union of all the objects in the project + things refered to 
;;; in handlers or which are values of properties. These could be other objects
;;; or function calls (globals and constants too but all are defined in core SK8).

(defun functions-and-objects-in-code (lfun)
  (let ((lvect (ccl::%lfun-vector (ccl::closure-function lfun)))
        thing result)
    (dotimes (i (ccl::%count-immrefs lvect))
      (setf thing (ccl::%nth-immediate lvect i))
      (when (symbolp thing)
        (pushnew thing result)))
    result))
    
(defun objects-and-functions-in-compiled-thing (proj thing)
  (declare (special *sk8-functions*))
  (let* ((lfun (if (mf::handlerp thing) ;;  (inheritsFrom thing handler) 
                 (!newGetLfun proj (name thing) (object thing))
                 (if (functionp thing)
                   thing
                   (!newGetLfun proj thing))))
         (sk8-functions *sk8-functions*)
         (thingsInCode (when lfun (functions-and-objects-in-code lfun)))
         objsAndFuns)
    (dolist (c thingsInCode)
      (cond ((fboundp c)
             (when (and (memq c sk8-functions)
                        (not (memq c *sk8-core-functions*)))
               (pushnew c objsAndFuns)))
            ((boundp c)
             (when (and (valid-used-sk8-obj (symbol-value c))
                        (not (memq (symbol-value c) *sk8-core-objects*)))
               (pushnew (symbol-value c) objsAndFuns)))))
    objsAndFuns))

(defun get-used-ancestors (obj)
  (mapancestors obj :function
                #'(lambda (x) 
                    (when (and (valid-used-sk8-obj x)
                               (not (inCoreSk8 x)))
                      (push x *all-used-sk8-objects*)))))

;;;what other collection types????

(defun add-all-sk8-vals-in (obj)
  (cond
   ((and (listp obj) (not (listp (cdr obj)))) ;; (is-a obj pair)
    (add-all-sk8-vals-in (car obj))
    (add-all-sk8-vals-in (cdr obj)))
   ((or 
     (listp obj)
     (vectorp obj))
    (map nil #'add-all-sk8-vals-in obj))
   ((and (valid-used-sk8-obj obj) (not (inCoreSK8 obj)))
    (push obj *all-used-sk8-objects*)
    (get-used-ancestors obj)
    )))

;;; This is duplicated in the Store.

(defun maprealproperties (obj func)
  (mapc #'(lambda (x) 
            (unless (memq (car x) mf::*mf-internal-format-slot-names*)
              (funcall func (car x))))
        (class-instance-slots (class-of obj)))
  nil
  )

(format t "~%mapRealProperties should be removed from this file (defined in store).")

(defun get-sk8-objs-in-props (obj)
  (let (res)
    (declare (special res))
    (maprealproperties obj #'(lambda (x) (add-all-sk8-vals-in (getvalue x obj))))
    res))

(defun get-sk8-objs-in-handlers (obj)
  (let ((globalThings nil))
    (dolist (c (localHandlers obj))
      (setf globalThings (nconc (objects-and-functions-in-compiled-thing (project obj) c) 
                                globalThings)))
    (when globalThings
      (setf *all-used-sk8-objects* (nconc globalThings *all-used-sk8-objects*)))))
    
(defun process-object-for-used-sk8-objects (x)
  (get-sk8-objs-in-handlers x)
  (get-sk8-objs-in-props x)
  (get-used-ancestors x))

(defun sk8-things-required (proj)
  (let ((*sk8-functions* (functions sk8)))
    (declare (special *sk8-functions*))
    (unless (and (is-a proj project) (neq proj sk8))
      (error "sk8-things-required must be given a valid subproject of SK8."))
    (setf *all-used-sk8-objects* nil)
    (mapprojectobjects proj #'process-object-for-used-sk8-objects)
    (dolist (i (functions proj)) 
      (setf *all-used-sk8-objects* (nconc (objects-and-functions-in-compiled-thing proj i) 
                                          *all-used-sk8-objects*)))
    (setf *all-used-sk8-objects* (delete-duplicates *all-used-sk8-objects*))))

;;; _______________________________ 
;;; (2) Map this to a list of modules required by the project. 
;;; _______________________________ 

(defun thing->pathname (thing)
  (cond ((mf::handlerp thing)) ;;  (inheritsFrom thing handler))
        ((symbolp thing) 
         (cond ((memq thing (constants sk8))
                (cdr (assoc 'ccl::constants (edit-definition-p thing))))
               ((memq thing (globals sk8))
                (cdr (assoc 'variable (edit-definition-p thing))))
               ((fboundp thing) 
                (cdr (assoc 'function (edit-definition-p thing))))))
        (t ;; must be an object...
         (let ((files (edit-definition-p (sk8::sk8_id thing))))
           (unless files
             (format t "Object ~a was not found." thing)
             (return-from thing->pathname nil))
           ;; Find the file that defines the object.
           (cdr (assoc 'ccl::variable files))))))

;;; This is the main function. Given a project it returns a progn of requires
;;; (files to be loaded to reconstitute the SK8 environment the project
;;; expects). This only deals with things in SK8. The store should load things
;;; in other projects that this project might need. 

(defun require-preface (aProject &optional thingsRequired)
  ;; Get the objects and functions needed.
  (let ((sk8ThingsRequired (or thingsRequired (sk8-things-required aProject)))
        (resultPathnames nil)
        (resultForm nil)
        thePath requireString)
    ;; Find where they come from and remove duplicates. 
    (dolist (c sk8ThingsRequired)
      (unless (InCoreSK8 c)
        (setf thePath (thing->pathname c))
        (when (and thePath (not (member thePath resultPathnames :test #'string-equal)))
          (push thePath resultPathnames))))
    ;; Build the string and return it. 
    (dolist (c resultPathnames)
      (setf requireString (ccl::pathString->requireString c))
      (when requireString
        (setf resultForm 
              (cons (list 'require requireString (ccl::remove-extension (sk8::relativize-pathname c)))
                    resultForm))))
    ;; Finish up the form by inserting the progn.
    (cons 'progn resultForm)))

#|
	Change History (most recent last):
	2  	 4/25/96	Hernan  	Changed require-form to output a progn.
	3  	 5/23/96	sidney  	
	4  	 7/ 7/96	sidney  	changes for native PPC build
	5  	 9/ 3/96	Hernan  	Added an optional argument to require-preface to 
						actually take from the caller the things required.
	6  	10/18/96	Hernan  	functions-and-objects-in-code needs to look into the 
						closure function to get the lvect. Also, functions of SK8
						no longer returns a list of functions but a list of symbols.
						Changing the code to account for this.
	7  	 2/27/97	Hernan  	
	8  	 3/ 7/97	Hernan  	require-preface has to output a lisp form, not a string, since
						the release version of SK8 is unable to read-from-string lisp forms.
|# ;(do not edit past this line!!)
