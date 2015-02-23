;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :macframes)
(provide "COMPLETIONFUNCTIONS")

;;;;;;;;;;;;;;;
;;; Completion
;;;;;;;;;;;;;;;

;;; withObjectNames -- sets VAR to a string representing an objectname
;;;        whose name matches the one provided in NAME.
;;;  var - the variable into which a successful match is put
;;;  obj - the object that corresponds to the match
;;;  name - the string to match
;;;  completion - How well the string is matched. Options are:
;;;                ':any' : any partial match is okay (the default)
;;;                ':starts' : any match that starts with NAME is okay
;;;                ':exact' : must exactly match NAME
;;;  case - Case sensitive or not (true means case-sensitive)
;;;  projects - a list containing the projects to look in
;;;             The default is to look at all open projects
;;;

(defmacro withObjectNames ((var obj name &key (completion :any) (case nil) (project nil)) &body body)
  (let ((thing (gensym))
        (str (gensym))
        (len (gensym))
        (function (gensym))
        (package (gensym))
        (sym (gensym)))
    `(progn
       (let ((,var nil)
             (,obj nil)
             (,len (length ,name))
             (,function (if ,case #'string= #'string-equal))
             (,package (sk8::package ,project))
             ,str ,thing)
         (declare (ignore-if-unused ,var ,len ,function ,str ,thing))
         (do-symbols (,sym ,package)
           (when (and (boundp ,sym)
                      ;(constantp ,sym)
                      (CCL::standard-instance-p (setq ,thing (symbol-value ,sym)))
                      (setq ,str (SK8::objectName ,thing))
                      (eq (intern-symbol ,str ,package) ,sym)
                      (case ,completion
                        (:any (search ,name ,str :test ,function))
                        (:starts
                         (and (>= (length ,str) ,len)
                              (search ,name ,str :test ,function :start2 0 :end2 ,len)))
                        (:exact (and (= (length ,str) ,len)
                                     (funcall ,function ,name ,str)))))
             (setq ,var ,str
                   ,obj ,thing)
             ,@body))))))

(defmacro withFunctionNames ((name match function &key (completion :any) (case nil) (project nil)) &body body)
  (let ((str (gensym))
        (len (gensym))
        (fun (gensym))
        (package (gensym))
        (sym (gensym)))
    `(progn
       (let ((,len (length ,name))
             (,function nil)
             (,match nil)
             (,fun (if ,case #'string= #'string-equal))
             ,str ,package)
         (declare (ignore-if-unused ,len ,fun ,str))
         (setq ,package (sk8::package ,project))
         (do-symbols (,sym ,package)
           (when (and (function-sym-p ,sym)
                      (setq ,function (fboundp ,sym))
                      (setq ,str (or (pretty-symbol-name ,sym)
                                     (symbol-name ,sym)))
                      (case ,completion
                        (:any (search ,name ,str :test ,fun))
                        (:starts
                         (and (>= (length ,str) ,len)
                              (search ,name ,str :test ,fun :start2 0 :end2 ,len)))
                        (:exact (and (= (length ,str) ,len)
                                     (funcall ,fun ,name ,str)))))
             (setq ,function ,function ;(SS::!get_maybeWrapped_actualFcn ,function)
                   ,match ,str)
             ,@body))))))

(defmacro withGlobalNames ((name match global &key (completion :any) (case nil) (project nil)) &body body)
  (let ((str (gensym))
        (len (gensym))
        (fun (gensym))
        (package (gensym))
        (sym (gensym)))
    `(progn
       (let ((,len (length ,name))
             (,global nil)
             (,match nil)
             (,fun (if ,case #'string= #'string-equal))
             ,str ,package)
         (declare (ignore-if-unused ,len ,fun ,str))
         (setq ,package (sk8::package ,project))
         (do-symbols (,sym ,package)
           (when (and (global-sym-p ,sym)
                      (boundp ,sym)
                      (not (constantp ,sym))
                      (setq ,str (or (pretty-symbol-name ,sym)
                                     (symbol-name ,sym)))
                      (case ,completion
                        (:any (search ,name ,str :test ,fun))
                        (:starts
                         (and (>= (length ,str) ,len)
                              (search ,name ,str :test ,fun :start2 0 :end2 ,len)))
                        (:exact (and (= (length ,str) ,len)
                                     (funcall ,fun ,name ,str)))))
             (setq ,global ,sym
                   ,match ,str)
             ,@body))))))

(defmacro withConstantNames ((name match constant &key (completion :any) (case nil) (project nil)) &body body)
  (let ((str (gensym))
        (len (gensym))
        (fun (gensym))
        (package (gensym))
        (sym (gensym)))
    `(progn
       (let ((,len (length ,name))
             (,constant nil)
             (,match nil)
             (,fun (if ,case #'string= #'string-equal))
             ,str ,package)
         (declare (ignore-if-unused ,len ,fun ,str))
         (setq ,package (sk8::package ,project))
         (do-symbols (,sym ,package)
           (when (and (global-sym-p ,sym)
                      (boundp ,sym)
                      (constantp ,sym)
                      (setq ,str (or (pretty-symbol-name ,sym)
                                     (symbol-name ,sym)))
                      (case ,completion
                        (:any (search ,name ,str :test ,fun))
                        (:starts
                         (and (>= (length ,str) ,len)
                              (search ,name ,str :test ,fun :start2 0 :end2 ,len)))
                        (:exact (and (= (length ,str) ,len)
                                     (funcall ,fun ,name ,str)))))
             (setq ,constant ,sym
                   ,match ,str)
             ,@body))))))


;;; withHandlerNames -- matches a handler name to handlers given criteria
;;;  handler - a variable name that gets bound to each handler that matches criteria
;;;  match - a variable name that gets bound to the full handler name (a string)
;;;  name - a string representing a partial or exact handler name
;;;  completion - whether the string match should be exact (:exact), partial (:any)
;;;            or whether the string starts the name (:starts). The default is :any.
;;;  strict - if true (the default), then only SK8 handlers for SK8 objects will be returned. (Note that there are
;;;        handlers (i.e., CLOS methods) defined in the rest of the system that are not necessarily associated with SK8 objects.)
;;;
(defmacro withHandlerNames ((handler match name &key (case nil) (completion :any) (strict nil) (project nil)) &body body)
  (let ((gf (gensym))
        (len (gensym))
        (n (gensym))
        (d (gensym))
        (ss (gensym))
        (matcher (gensym))
        (projects (gensym))
        (packages (gensym))
        (s (gensym)))
    `(let* ((,n nil)
            (,d nil)
            (,s nil)
            (,match nil)
            (,len (length ,name))
            (,ss nil)
            (,matcher (if ,case #'string= #'string-equal))
            (,projects (if ,project
                         (cons ,project (sk8::requiredprojects ,project))))
            (,packages (mapcar #'(lambda (x) (sk8::package x))
                               ,projects)))
       (dolist (,gf (ccl::population-data ccl::%all-gfs%))
         (setq ,n (function-name ,gf))
         (setq ,match (pretty-symbol-name (setq ,s (if (listp ,n) (cadr ,n) ,n))))
         (if (listp ,n) (setq ,match (concatenate 'string "set " ,match)))
         (when (and (memq (symbol-package ,s) ,packages)
                    (null (find #\- ,match)))
           (if (case ,completion
                 (:any (search ,name ,match :test ,matcher))
                 (:starts (and (>= (length ,match) ,len)
                               (search ,name ,match :test ,matcher :start2 0 :end2 ,len)))
                 (:exact (and (= (length ,match) ,len)
                              (funcall ,matcher ,name ,match))))
             (dolist (,handler (generic-function-methods ,gf))
               (when (if ,strict
                       (or (and (null ,project)
                                (not (memq (method-name ,handler) ,packages)))
                           (and (setq ,ss (method-specializers ,handler))
                                (setq ,d (if (listp ,n) (cadr ,ss) (car ,ss)))
                                (not (listp ,d))  ; ensure this isn't an EQL specializer - adam
                                ;;***?!?!?!?!?!(std-inst-p (setq ,d (mf::class-owner ,d)))
                                (memq (sk8::project ,d) ,projects)
                                ))
                       t)
                 ,@body))))))))

(defmacro withPropertyNames ((match object name &key (case nil) (completion :any) (project nil)
                                       (objects t)) &body body)
  (let ((p (make-symbol "P"))
        (str (make-symbol "S"))
        (syms (make-symbol "SS"))
        (len (make-symbol "L"))
        (function (make-symbol "F"))
        (sym (make-symbol "SYM")))
    `(progn
       (let ((,match nil)
             (,object nil)
             (,len (length ,name))
             (,syms nil)
             (,function (if ,case #'string= #'string-equal))
             ,str)
         (dolist (,p (cons ,project (sk8::requiredprojects ,project)))
           (sk8::mapprojectobjects
            ,p
            #'(lambda (obj)
                (dolist (,sym (class-direct-instance-slots (class-of obj)))
                  (setq ,sym (car ,sym))
                  (setq ,str (or (pretty-symbol-name ,sym)
                                 (symbol-name ,sym)))
                  (when (and (if ,objects
                               t
                               (not (memq ,sym ,syms)))
                             (case ,completion
                               (:any (search ,name ,str :test ,function))
                               (:starts
                                (and (>= (length ,str) ,len)
                                     (search ,name ,str :test ,function :start2 0 :end2 ,len)))
                               (:exact (and (= (length ,str) ,len)
                                            (funcall ,function ,name ,str)))))
                    (setq ,match ,sym)
                    (setq ,object obj)
                    (if (not ,objects) (setq ,syms (cons ,sym ,syms)))
                    ,@body)))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *** define-sk8-functions should be moved into a macframes-post-boot file
;;;

;;; findObject -- finds a named object
;;;
(sk8::define-sk8-function sk8::findObject nil (name &key fromproject ((:project InProject)))
  (let ((package (if inproject (package inproject))))
    (do-symbols (v (if fromproject (package fromproject)))
      (if (and (boundp v)
               (typep (symbol-value v) 'sk8::object)
               (string-equal (symbol-name v)
                             name))
        (if package
          (if (eq package (symbol-package v))
            (return (symbol-value v)))
          (return (symbol-value v)))))))

(sk8::define-sk8-function sk8::getPropertyNameCompletions nil (name &key (completion :any) (matches t) (objects t)
                                                                    (case nil) project)
  (setq name (require-type name 'string))
  (if (and project (not (sk8::is-a project sk8::Project))) (error "~a must be a project" project))
  (let (a b)
    (withpropertynames (match object name :case case :completion completion :objects objects
                              :project (or project sk8::sk8))
      (if matches (push match a))
      (if objects (push object b)))
    (list a b)))

(sk8::define-sk8-function sk8::getGlobalNameCompletions nil (name &key (completion :any) project (case nil) (matches t))
  (setq name (require-type name 'string))
  (if (and project (not (sk8::is-a project sk8::Project))) (error "~a must be a project" project))
  (let (a b)
    (withglobalnames (name match global :case case :completion completion
                           :project (or project sk8::sk8))
      (if matches (push match a))
      (if global (push global b)))
    (list a b)))

(sk8::define-sk8-function sk8::getConstantNameCompletions nil (name &key (completion :any) project (case nil) (matches t))
  (setq name (require-type name 'string))
  (if (and project (not (sk8::is-a project sk8::Project))) (error "~a must be a project" project))
  (let (a b)
    (withconstantnames (name match constant :case case :completion completion
                             :project (or project sk8::sk8))
      (if matches (push match a))
      (if constant (push constant b)))
    (list a b)))

(sk8::define-sk8-function sk8::getFunctionNameCompletions nil (name &key (completion :any) project (case nil) (functions t)
                                                                    (matches t))
  (setq name (require-type name 'string))
  (if (and project (not (sk8::is-a project sk8::Project))) (error "~a must be a project" project))
  (let (a b)
    (withfunctionnames (name match function :case case :completion completion
                             :project (or project sk8::sk8))
      (if matches (push match a))
      (if functions (push function b)))
    (list a b)))

;;; getObjectNameCompletions -- returns either (or both of) a list of strings which are valid completions or a list of objects whose name validly completes the string
;;;      Either list is returned ONLY IF REQUESTED (see objects and strings keywords)!!!
;;;      The first value returned is a list of strings and the second value a list of objects.
;;;  name - a partial string standing for a possible object name
;;;  completion - type of completion:
;;;             :exact - string must exactly match objectname
;;;             :any - string may partially match the objectname (the default)
;;;             :starts - string must partially match at the starting position of the objectname
;;;  project - project to which the search should be restricted. (default: search ALL projects)
;;;  all - if true and if a project has been provided, whether projects used by this project should also be searched (this is the default)
;;;  case - whether match should be case-sensitive or not. True means case-sensitive. The default is case-insensitive.
;;;  objects - whether a list of objects should be returned (default: objects are not returned)
;;;  matches - whether a list of strings should be returned (default: matching strings are returned)
;;;
(sk8::define-sk8-function sk8::getObjectNameCompletions nil (name &key (completion :any) project (all t)
                                                                  (case nil) (objects nil) (matches t))
  (declare (ignore all))
  (setq name (require-type name 'string))
  (if (and project (not (sk8::is-a project sk8::Project))) (error "~a must be a project" project))
  (let (a b)
    (withobjectnames (v o name :case case :completion completion
                        :project (or project sk8::sk8))
      (if matches (push v a))
      (if objects (push o b)))
    (list a b)))


;;; getHandlerNameCompletions -- returns either (or both of) a list of strings which are valid matches or a list of handler objects that match given the criteria
;;;      Either list is returned ONLY IF REQUESTED (see objects and strings keywords)!!!
;;;      The first value returned is a list of strings and the second value a list of objects.
;;;  name - a partial string standing for a possible handler name
;;;  completion - the type of completion (same as for getObjectNameCompletions)
;;;  strict - if true and project is not nil, only handlers defined on SK8 objects visible in the project are returned.
;;;  case - whether match should be case-sensitive or not (default: case-insensitive).
;;;  handlers - if true, a list of handler objects is returned as the first returned value (default: handlers are NOT returned)
;;;  matches - if true, a list of handler strings that match the criteria is returned as the second value (default: strings are returned)
;;;
(sk8::define-sk8-function sk8::getHandlerNameCompletions nil (name &key (completion :any) (strict nil)
                                                                   (handlers nil) (matches t) (project nil))
  (setq name (require-type name 'string))
  (let (a b)
    (withhandlernames (handler match name :completion completion
                               :strict strict :project (or project sk8::sk8))
      (when (and (sk8::object handler) (not (mirror-method-p handler)) (not (hidden-handler-p handler)))
        (if matches (push match a))
        (if handlers (push handler b))))
    (list a b)))


#|
	Change History (most recent last):
	2  	11/26/96	Brian   	Adding Completion Functions.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
