;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :ps)

(provide "HANDLERSTORE")

;;; Everything in the old store seeming to have anything to do with
;;; storing handlers in memory... This should be an optional part of SK8Script.
;;; It is required by the EditorScriptEditText...

;;; _______________________________ 
;;; Globals
;;; _______________________________ 

;;; *handlers-to-register* -- list of handlers which should be registered in the store
;;;   Each entry of this list is of the form: (object1 handler-name object2), where
;;;     object1 - the object on which handler-name's information is already registered
;;;     handler-name - a handler name (symbol or (setf name) cons)
;;;     object2 - the object under which the handler information
;;;             should be registered in the project file

(defvar *handlers-to-register* nil)

;;; *okay-to-save-any-project* -- when true, will allow any project's data to be saved

(defvar *okay-to-save-any-project* nil)

;;; _______________________________ 
;;; Helpers
;;; _______________________________ 

;; A common idiom - The project object is for some things in the parent's project, but for others in its own
;; (project proj) is the parent, Use this function when that isn't what you want
(defun owning-project (obj)
  (if (sk8:inheritsfrom obj SK8:Project)
    obj
    (sk8:project obj)))

;;; ok-to-save-p -- returns true when it is okay to save this project's data into its project file
;;;
(defun ok-to-save-p (proj)
  (or *okay-to-save-any-project*
      (neq proj SK8:Project)))

;;; _______________________________ 
;;; Structures
;;; _______________________________ 

;;; hand - handler version record
;;;  version - handler version id is the universal time (date/time) in which version was saved
;;;  flags - handler characteristics encoded in a fixnum
;;;           Bit 0 = true if handler is private
;;;           Bit 1 = true if handler is locked (internal)
;;;  script - the handlers SK8Script source (a string)
;;;  globals - list of globals defined by handler (list of symbols)
;;;  locals - list of locals defined by handler (list of symbols0
;;;  handlers - list of functions and handlers called by this handler (list of symbols)
;;;  breakpoints - list of breakpoints (nested lists/vectors of immediates)
;;;  wstate - watch state (integer or vector of immediates)
;;;  wexps - watch expressions (arbitrary lisp forms)
;;;  indentation - indentation information (how scripts are indented)
;;;  editor - editor information (used by the SK8Script editor)
;;;  returns - type of object returned by handler (a sk8 object)
;;;  doc - handler documentation (a string)
;;;  active - if true, this is an active handler

(defstruct (hand (:copier nil))
  (version nil)
  (flags nil)
  (script nil)
  (globals nil)
  (locals nil)
  (handlers nil)
  (breakpoints nil)
  (wstate nil)
  (wexps nil)
  (indentation nil)
  (editor nil)
  (returns nil)
  (doc nil)
  (active nil)
  )

;;; fun -- function version record (field meanings are as in hand)
;;;

(defstruct (fun (:copier nil))
  (version nil)
  (flags nil)
  (script nil)
  (globals nil)
  (locals nil)
  (handlers nil)
  (breakpoints nil)
  (wstate nil)
  (wexps nil)
  (indentation nil)
  (editor nil)
  (returns nil)
  (doc nil)
  (active nil)
  )

;;; active-fun -- active function record (describes the function that is now active)
;;;  version-record - the function's fun record (contains all other information)
;;;  lfun - the function's lfun (binary code ready for loading back)
;;;  wrapper - the function's wrapper lfun (optional for debugging)
;;;  qualifier - handler qualifier if this is a function for a handler (saves an extra disk seek)
;;;  translation - the lisp translation of the script (a cons)
;;;

(defstruct (active (:copier nil))
  (version nil)
  (records nil)
  )

;;; global - global variable record
;;;  name - the variable's name (a symbol)
;;;  value - the variable's value (an arbitrary object)
;;;  flags - bit 0 is private, bit 1 is lock
;;;  type - the type of value
;;;  handlerName - the name (a symbol) of a function or handler in which this global was declared
;;;  handlerObject - the object on which the handler was defined (if functionName is a handler)
;;;  handlerQualifier - the qualifier of the handler (if functionName is a handler)
;;;  doc - variable documentation

(defstruct (global (:copier nil))
  (created (get-universal-time))
  (name nil)
  (value nil)
  (flags nil)
  (type nil)
  (functionName nil)
  (handlerObject nil)
  (handlerQualifier nil)
  (doc nil)
  )

;; support for saving these structures in fasl form

(defun fasl-store-struct-instance (strobj)
  (let* ((len (1- (uvsize strobj)))
         (vec (make-array len)))
    (dotimes (i len)
      (setf (svref vec i) (ccl::%svref strobj (1+ i))))
    vec))

(defun fasl-load-struct-instance (strobj vec)
  (dotimes (i (1- (uvsize strobj)))
    (setf (ccl::%svref strobj (1+ i)) (svref vec i)))
  strobj)

(defmethod make-load-form ((me hand))
  (values
   `(make-hand)
   `(fasl-load-struct-instance ,me ,(fasl-store-struct-instance me)))
  )

(defmethod make-load-form ((me fun))
  (values
   `(make-fun)
   `(fasl-load-struct-instance ,me ,(fasl-store-struct-instance me)))
  )

(defmethod make-load-form ((me active))
  (values
   `(make-active)
   `(fasl-load-struct-instance ,me ,(fasl-store-struct-instance me)))
  )

;;; _______________________________ 
;;; Handlers and Functions
;;; _______________________________ 

;; encapsulate access to the handler and function script information
;; Earlier implementations required too many different functions to explicitly deal with the way
;; we were storing this info. We really have to encapsulate it somehow.

;; the value entry of these hash tables contain an Active record object,  which contains a version
;; number of the active version (if any) and a list of versioned script/debug info records

(defun find-handler-records (name obj qualifier)
  (let* ((proj (owning-project obj))
         (table (SK8::handler-record-table proj)))
    (when table
      (gethash (list name obj qualifier) table))))

(defun put-handler-records (name obj qualifier activerec)
  (let* ((proj (owning-project obj))
         (table (or (SK8::handler-record-table proj)
                    (setf (SK8::handler-record-table proj)
                          (make-hash-table :test #'equal)))))
    (setf (gethash (list name obj qualifier) table) activerec)))

(defun delete-handler-records (name obj qualifier)
  (let* ((proj (owning-project obj))
         (table (SK8::handler-record-table proj)))
    (when table
      (remhash (list name obj qualifier) table))))

(defun find-function-records (proj name)
  (let ((table (sk8::function-record-table proj)))
    (when table
      (gethash name table))))

(defun put-function-records (proj name activerec)
  (let ((table (or (sk8::function-record-table proj)
                   (setf (sk8::function-record-table proj)
                         (make-hash-table :test #'equal)))))  ;; equal because name may be '(setf foo)
    (setf (gethash name table) activerec)))

(defun delete-function-records (proj name)
  (let ((table (sk8::function-record-table proj)))
    (when table
      (remhash name table))))

;;; find-handler-record -- retrieves a hand record that meets the criteria
;;; name - the handler name (symbol)
;;; object - the handler's object (object)
;;; qualifier - the handler's qualifier (symbol)
;;; version - the handler's version (a universal time). The most recent version
;;;         is returned if this is nil
;;;
;;;  Note: Assumes that versions are in most-recent first order in the list
;;;
(defun find-handler-record (name object qualifier version)
  (let ((activerec (find-handler-records name object qualifier)))
    (when activerec
      (let ((records (active-records activerec)))
        (if version
          (find version records :key #'hand-version)
          (car records))))))

;;; find-function-record -- retrieves a function record that meets the criteria
;;; proj - the project the function is defined in
;;; name - the function name (a symbol)
;;; version - the handler's version (a universal time). The most recent version
;;;         is returned if this is not provided
;;;
;;;  Note: Assumes that versions are in most-recent first order in the list
;;;
;;; returns the record, if it is found

(defun find-function-record (proj name version)
  (let ((activerec (find-function-records proj name)))
    (when activerec
      (let ((records (active-records activerec)))
        (if version
          (find version records :key #'fun-version)
          (car version))))))

;;; find-active-record -- retrieves the active-fun record that meets criteria
;;;  object - if nil, then we are dealing with a project function and not a handler
(defun find-active-record (proj name obj qualifier)
  (if obj
    (find-active-handler-record name obj qualifier)
    (find-active-function-record proj name)))

;; finds the method given the method name, object and qualifier. Will only find a local method, no inheritance.
;; There's another function around here somewhere that does the same thing using some more complicated
;; lookup starting form the generic function. I sould compare timings and use one or the other
;; Returns the method object.
(defun find-active-handler-method (name object qualifier)
  (setq name (mf::make-internal-function-name name))
  (find-if #'(lambda(meth) (and (equal name (method-name meth))
                                (eq qualifier (car (method-qualifiers meth)))))
           (ccl::specializer-direct-methods (class-of object))))

;; Find the object we use to hold all of the versioned script objects for a method.
;; The objects should not require the method or lfun to exist yet
(defun find-active-handler-record (name object qualifier)
  (let ((activerec (find-handler-records name object qualifier)))
    (when activerec
      (let ((version (active-version activerec))
            (records (active-records activerec)))
        (when version
          (find version records :key #'hand-version))))))

(defun find-active-function-record (proj name)
  (let ((activerec (find-function-records proj name)))
    (when activerec
      (let ((version (active-version activerec))
            (records (active-records activerec)))
        (when version
          (find version records :key #'fun-version))))))

(defun replace-or-insert-handler-record
       (name obj qualifier
             version flags script globals locals handlers breakpoints
             watchingState watchExpressions indentationinfo editorinfo
             returnType documentation active)
  (let ((activerec (find-handler-records name obj qualifier)))
    (unless activerec
      (setf activerec (make-active :version (when (and active (neq active :preserve)) version)
                                   :records nil))
      (put-handler-records name obj qualifier activerec))
    (let ((record (find version (active-records activerec) :key #'hand-version)))
      (if record
        (progn
          (unless (eq flags :preserve) (setf (hand-flags record) flags))
          (unless (eq script :preserve) (setf (hand-script record) script))
          (unless (eq globals :preserve) (setf (hand-globals record) globals))
          (unless (eq locals :preserve) (setf (hand-locals record) locals))
          (unless (eq handlers :preserve) (setf (hand-handlers record) handlers))
          (unless (eq breakpoints :preserve) (setf (hand-breakpoints record) breakpoints))
          (unless (eq watchingState :preserve) (setf (hand-wstate record) watchingState))
          (unless (eq watchExpressions :preserve) (setf (hand-wexps record) watchExpressions))
          (unless (eq indentationInfo :preserve) (setf (hand-indentation record) indentationInfo))
          (unless (eq editorInfo :preserve) (setf (hand-editor record) editorInfo))
          (unless (eq returnType :preserve) (setf (hand-returns record) returnType))
          (unless (eq documentation :preserve) (setf (hand-doc record) documentation))
          (unless (eq active :preserve) (setf (hand-active record) active)))
        (progn
          (setf record (make-hand :version version
                                  :flags flags
                                  :script (if (eq :preserve script) "" script)
                                  :globals (unless (eq :preserve globals) globals)
                                  :locals (unless (eq :preserve locals) locals)
                                  :handlers (unless (eq :preserve handlers) handlers)
                                  :wstate (if (eq :preserve watchingState) 0 watchingState)
                                  :wexps (unless (eq :preserve watchExpressions) watchExpressions)
                                  :indentation (unless (eq :preserve indentationInfo) indentationInfo)
                                  :editor (unless (eq :preserve editorInfo) editorInfo)
                                  :returns (unless (eq :preserve returnType) returnType)
                                  :doc (unless (eq :preserve documentation) documentation)
                                  :active (unless (eq :preserve active) active)))
          (push record (active-records activerec))))
      (when (hand-active record)
        (dolist (rec (active-records activerec))  ;; make sure only one record is marked active
          (unless (eq rec record) (setf (hand-active rec) nil)))
        (setf (active-version activerec) version))
      record)))

(defun replace-or-insert-function-record
       (name proj
             version flags script globals locals handlers breakpoints
             watchingState watchExpressions indentationinfo editorinfo
             returnType documentation active)
  (let ((activerec (find-function-records proj name)))
    (unless activerec
      (setf activerec (make-active :version (when (and active (neq active :preserve)) version)
                                   :records nil))
      (put-function-records proj name activerec))
    (let ((record (find version (active-records activerec) :key #'fun-version)))
      (if record
        (progn
          (unless (eq flags :preserve) (setf (fun-flags record) flags))
          (unless (eq script :preserve) (setf (fun-script record) script))
          (unless (eq globals :preserve) (setf (fun-globals record) globals))
          (unless (eq locals :preserve) (setf (fun-locals record) locals))
          (unless (eq handlers :preserve) (setf (fun-handlers record) handlers))
          (unless (eq breakpoints :preserve) (setf (fun-breakpoints record) breakpoints))
          (unless (eq watchingState :preserve) (setf (fun-wstate record) watchingState))
          (unless (eq watchExpressions :preserve) (setf (fun-wexps record) watchExpressions))
          (unless (eq indentationInfo :preserve) (setf (fun-indentation record) indentationInfo))
          (unless (eq editorInfo :preserve) (setf (fun-editor record) editorInfo))
          (unless (eq returnType :preserve) (setf (fun-returns record) returnType))
          (unless (eq documentation :preserve) (setf (fun-doc record) documentation))
          (unless (eq active :preserve) (setf (fun-active record) active)))
        (progn
          (setf record (make-fun :version version
                                 :flags flags
                                 :script (if (eq :preserve script) "" script)
                                 :globals (unless (eq :preserve globals) globals)
                                 :locals (unless (eq :preserve locals) locals)
                                 :handlers (unless (eq :preserve handlers) handlers)
                                 :wstate (if (eq :preserve watchingState) 0 watchingState)
                                 :wexps (unless (eq :preserve watchExpressions) watchExpressions)
                                 :indentation (unless (eq :preserve indentationInfo) indentationInfo)
                                 :editor (unless (eq :preserve editorInfo) editorInfo)
                                 :returns (unless (eq :preserve returnType) returnType)
                                 :doc (unless (eq :preserve documentation) documentation)
                                 :active (unless (eq :preserve active) active)))
          (push record (active-records activerec))))
      (when (fun-active record)
        (dolist (rec (active-records activerec))  ;; make sure only one record is marked active
          (unless (eq rec record) (setf (fun-active rec) nil)))
        (setf (active-version activerec) version))
      record)))

;;; purge-function -- purges a function's version and, if it is active, makes it inactive.
;;;  name - the function's name
;;;  version - the functions version number.
;;;
(defun purge-function (proj name version)
  (let ((activerec (find-function-records proj name)))
    (when activerec   ;; nothing to purge if there are no records
      (when (eql (active-version activerec) version)
        (setf (active-version activerec) nil))
      (setf (active-records activerec)
            (delete version (active-records activerec) :key #'fun-version)))))

;;; purge-all-functions -- purges all versions of the function
;;;  proj - the function's project (an object)
;;;  name - the function's name (a symbol or cons)
;;;
(defun purge-all-functions (proj name)
  (delete-function-records proj name))

(defun purge-handler (name obj qualifier version)
  (let ((activerec (find-handler-records name obj qualifier)))
    (when activerec   ;; nothing to purge if there are no records
      (when (eql (active-version activerec) version)
        (setf (active-version activerec) nil))
      (setf (active-records activerec)
            (delete version (active-records activerec) :key #'hand-version)))))

(defun purge-all-handlers (name object qualifier)
  (delete-handler-records name object qualifier))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Active records

;;; makeActive -- makes the handler or function the active one, saving the associated info
;;;  name - the handler or function's name (a symbol)
;;;  object - the object on which the handler is defined. If nil, then this is a project function--not a handler (an object)
;;;  qualifier - the handler's qualifier (a symbol)
;;;  record - the handler or function's version record (a hand or fun record)
;;;
(defun makeActive (proj name object qualifier record lfun version)
  ;; Get lfun and record:
  (unless lfun
    (unless (setq lfun
                  (if object
                    (method-function
                     (mf::find-local-handler name object :qualifier qualifier))
                    (symbol-function name)))
      (error "Can't find handler ~a~@[ ~a~] for ~a" name qualifier object)))
  (unless record
    (unless (setq record (if object
                           (find-handler-record name object qualifier version)
                           (find-function-record proj name version)))
      (error "No record found for ~a~@[ version ~a~]" name version)))
  
  ;; Activate the record's version:
  (let ((activerec (if object
                     (find-handler-records name object qualifier)
                     (find-function-records proj name)))
        active)
    
    ;; Mark lfun as having sources:
    (when (has-script-p record) (set-lfunSourcesVersion lfun version))
    
    ;; Update or create Active record:
    (when (setq active (find-active-record proj name object qualifier))
      
      ;; Update old record:
      (if object
        (setf (hand-active active) nil)
        (setf (fun-active active) nil)))
    
    ;; Update active info:
    (if object
      (setf (hand-active record) t)
      (setf (fun-active record) t))
    (setf (active-version activerec) version)
    ))

(defun has-script-p (record)
  (if (hand-p record)
    (hand-script record)
    (fun-script record)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;*** Redefined later in SK8Script!
(unless (fboundp 'transform-script-for-new-object)
  (let ((*record-source-file* nil))
    (defun transform-script-for-new-object (str proj obj)
      (declare (ignore proj obj))
      str)))

(defun copy-handler-record (name to qualifier from)
  (let* ((record (find-handler-record name from qualifier nil))      ; returns most recent version
         (proj (owning-project to)))
    (when record
      (unwind-protect
        (progn
          (savehandlerversion proj
                              name to qualifier
                              nil nil (transform-script-for-new-object (hand-script record) proj to)
                              nil nil
                              (hand-globals record) (hand-locals record) (hand-handlers record) nil nil
                              nil (hand-returns record) (hand-indentation record) (hand-editor record)
                              (hand-doc record) t (get-universal-time)))
        )))
  t)

;;; copy-all-pending-handlers -- copies all handler information which is pending
;;;
(defun copy-all-pending-handlers ()
  (mapc #'(lambda (record)
            (let* ((to (first record))
                   (from (second record))
                   (name (third record))
                   (qualifier (fourth record)))
              (copy-handler-record name to qualifier from)
              (sk8:tickEventClock)))
        *handlers-to-register*)
  (setq *handlers-to-register* nil)
  t)

;;; _______________________________ 
;;; External API
;;; _______________________________ 

;;; lfunHasSources -- returns the version number of the sources if there are sources saved for this lfun
;;;               Otherwise returns false.
;;;
(defun lfunHasSources (closure)
  (getf (CCL::%LFUN-INFO (ccl::closure-function closure)) :version))

(defun set-lfunSourcesVersion (closure version)
  (let ((plist (CCL::%LFUN-INFO (ccl::closure-function closure))))
    (if (do ((i plist (cddr i)))
            ((null i) t)
          (when (eq (car i) :version)
            (rplaca (cdr i) version)
            (return nil)))
      (nconc plist (list :version version)))))

(defun lfunVersion (lfun)
  (lfunHasSources lfun))

;;; mapactivefunctions -- maps a function over all active functions in a project file
;;;    proj - the project (an object)
;;;    function - the function will be called on each active function record in the store.
;;;            The function should accept three arguments: (1) the function name (a symbol),
;;;            (2)  a boolean which, if true, means that the function is a set function,
;;;            and (3) the active function record, which may be used to get more information.
;;;
(defun mapactivefunctions (proj function)
  (let ((table (sk8::function-record-table proj)))
    (when table
      (maphash #'(lambda (key activerec)
                   (when activerec
                     (if (listp key)
                       (funcall function (second key) t activerec)
                       (funcall function key nil activerec))))
               table))))

;;; purgeFunction -- purges a function version. If the version is active, it becomes inactive.
;;;  proj - the function's project (an object)
;;;  name - the function's name (a symbol)
;;;  version - the version id to make active (a universal time)
;;;
;;; Returns two values:
;;;   done - true if any function was actually purged
;;;   inactive - true if there is no active function as a side-effect of calling purgeFunction
;;;
;;; Note: if version is not provided, then all versions of the function are purged
;;;
(defun purgeFunction (proj name &key version)
  (declare (ignore-if-unused proj name version))
  (setq name (mf::make-internal-function-name name))
  (if version
    (purge-function proj name version)
    (purge-all-functions project name)))

;;; saveFunctionVersion -- saves a version of a function
;;;  name - the function name (symbol)
;;;  private - true if the function should be private (boolean)
;;;  locked - true if the function should be locked (boolean)
;;;  script - the function's script (string)
;;;  globals - globals for the function (list of symbols)
;;;  locals - locals for the function (list of symbols)
;;;  breakpoints - breakpoint information
;;;  watchingState - watching state info (???)
;;;  watchExpressions - watch expressions (???)
;;;  returnType - type of object the function returns (object)
;;;  documentation - function's documentation (string)
;;;  active - if true, then this version will become the active one
;;;  version - optionally, the time this version was created (if not passed in,
;;;       then the universal time is used) (bignum)
;;;
(defun saveFunctionVersion (proj
                                name
                                private
                                locked
                                script
                                translation
                                lfun
                                globals
                                locals
                                handlers
                                breakpoints
                                watchingState
                                watchExpressions
                                returnType
                                indentationInfo
                                editorInfo
                                documentation
                                active
                                version)
  (declare (ignore locked private translation)) ; *** not implemented or obsolete
  (when (ok-to-save-p proj)
    (setq name (mf::make-internal-function-name name))
    (if (not version) (setq version (get-universal-time)))
    (let ((record (replace-or-insert-function-record
                   name proj version
                   0 ;; *** flag not done (e.g., private, locked)
                   script globals locals handlers breakpoints
                   watchingState watchExpressions indentationinfo editorinfo
                   returnType documentation active)))
      
      (when (and (fun-active record) ; can't look at 'active' arg, since it might be :PRESERVE
                 (neq lfun :PRESERVE))
        (makeActive proj name nil nil record lfun version))))
  version)

;;; getFunctionInfo -- returns information about a function's version
;;;  proj - the function's project (an object)
;;;  name - the function's name (a symbol)
;;;  version - the version id to make active (a universal time)
;;;
;;; Returns: the function's version info
;;;
;;; Note: if the version is not provided, then the information for the active version is returned.
;;;

(defun getFunctionInfo (proj name &key version)
  (setq name (mf::make-internal-function-name name))
  (let ((record (find-function-record proj name version)))
    (when record
      (values (fun-version record)
              nil ; *** private from (fun-flags record)
              nil ; *** locked from (fun-flags record)
              (fun-script record)
              (fun-globals record)
              (fun-locals record)
              (fun-handlers record)
              (fun-breakpoints record)
              (fun-wstate record)
              (fun-wexps record)
              (fun-returns record)
              (fun-indentation record)
              (fun-editor record)
              (fun-doc record)))))

;;; functionVersions -- returns existing versions for a function
;;;  proj- the function's project (an object)
;;;  name - the function's name (a symbol)
;;;
;;; Returns: a list either with all of the function versions with most recent version first
;;;

(defun functionVersions (proj name)
  (setq name (mf::make-internal-function-name name))
  (let ((activerec (find-function-records proj name)))
    (when activerec
      (mapcar #'fun-version (active-records activerec)))))

;;; activeFunction -- returns the active version of the function, if any
;;;  proj- the function's project (an object)
;;;  name - the function's name (a symbol)
;;;
;;; Returns: the function version for the currently active function.
;;;

(defun activeFunction (proj name)
  ;; This function wants a symbol, but some bad pieces of code call it with
  ;; a string. Make it work.
  (when (stringp name)
    (setf name (or (find-symbol (string-upcase name) (package proj))
                   (intern name (package proj)))))
  (setq name (mf::make-internal-function-name name))
  (let ((activerec (find-function-records proj name)))
    (when activerec
      (active-version activerec))))

;;; _______________________________ 
;;; HANDLERS
;;; _______________________________ 

;;; mapactivehandlers -- returns information for all active handlers of objects in project
;;;   proj- the project (an object)
;;;   function - optionally, a function that will be called on each active handler record
;;;           If provided, then handler information will not be returned. function should
;;;           accept the following arguments: (1) the name of the handler (a symbol)
;;;           (2) the name of the object on which the handler was defined (a symbol),
;;;           (3) the handler qualifier (which will be one of nil, :before or :after), (4) a boolean which, if true, means
;;;           that the handler is a setter; and (5) the active record itself which may be used to get more
;;;           information.
;;;


(defun mapactivehandlers (proj function)
  (let ((table (SK8::handler-record-table proj)))
    (when table
      (maphash #'(lambda (key activerec)
                   (when (and activerec (active-version activerec))
                     (if (listp (first key))
                       (funcall function (second (first key)) (second key) (third key) t activerec)
                       (funcall function (first key) (second key) (third key) nil activerec))))
               table))))

;;; mapactiveglobals -- maps a function over all globals records in the project file
;;;  proj- the project object
;;;  function - a function that takes three arguments:
;;;    (1) the global name (a symbol)
;;;    (2) the global record (single p-loaded)
;;;    (3) a boolean: when true, the global is a variable, else it's a constant
;;;
(defun mapactiveglobals (proj function)
  (let (v)
    (dolist (table (list (sk8::constant-record-table project) (sk8::variable-record-table proj)))
      (when table
        (maphash #'(lambda (name global)
                     (funcall function name global v))
                 table))
      (setq v t))))

(defun purgeHandler (name object qualifier &key version)
  (setq name (mf::make-internal-function-name name))
  (if version
    (purge-handler name object qualifier version)
    (purge-all-handlers name object qualifier)))

;;; saveHandlerVersion -- to save a new version of a handler (and optionally to make this the active one)
;;;  project - the handler's project (an object)
;;;  name - the handler name (either a cons = (setf name) or a symbol)
;;;  object - the object on which the handler is defined (an object)
;;;  qualifier - the handler's qualifier (one of NIL, :before or :after)
;;;  private - boolean (specifies whether the handler is private or not)
;;;  locked - boolean (specified whether the handler is locked for modification or not)
;;;  script - the handler's script (a string)
;;;  translation - a cons which is the SK8Script -> Lisp translation
;;;  lfun - [optional] the handler's lfun (binaries)
;;;  globals
;;;  locals
;;;  handlers
;;;  breakpoints
;;;  watchingState
;;;  watchExpressions
;;;  returnType
;;;  indentationInfo
;;;  editorInfo
;;;  documentation - a documentation string as long as you can stand it
;;;  active - boolean (if true, then make this handler the active one)
;;;  version - the handler's version (any immediate will do, but we have agreed that this would be
;;;          the universal time (but... should we use Greenwhich?). If not provided, will
;;;          be the current universal time.
;;;
(defun saveHandlerVersion (proj name object qualifier
                                   private locked script translation lfun
                                   globals locals handlers breakpoints watchingState
                                   watchExpressions returnType indentationInfo editorInfo 
                                   documentation active version)
  (declare (ignore private locked translation))
  (when (ok-to-save-p proj)
    (setq name (mf::make-internal-function-name name))
    (unless version (error "Missing version"))
    (let ((thisproj (if (sk8:inheritsFrom object SK8:Project)
                      object
                      proj)))
      (let ((record (replace-or-insert-handler-record
                     name object qualifier
                     version 0 script globals locals handlers breakpoints
                     watchingState watchExpressions indentationinfo editorinfo
                     returnType documentation active)))
        
        (when (and (hand-active record) ; can't look at 'active' arg, since it might be :PRESERVE
                   (neq lfun :PRESERVE))
          (makeActive thisproj name object qualifier record lfun version))))
    version))

;;; A simple version of the function above. It is used to enter functions AND handlers into 
;;; the store when they are loaded from a script file or from a Fred window. 

(defun saveHandlerVersionFromText (targetProj handlerObj scriptText locals globals)
  (when (string-equal "on " (subseq scriptText 0 (min 3 (length scriptText))))
    (let* ((version (get-universal-time)))
      (if (symbolp handlerObj)
        (when (fboundp handlerObj)
          (PS::saveFunctionVersion targetProj handlerObj
                                   :preserve :preserve scriptText t 
                                   (sk8dev::!newgetLfun targetProj handlerObj)
                                   globals locals nil nil nil nil :preserve nil :preserve :preserve t 
                                   version))
        (PS::saveHandlerVersion targetProj (sk8::name handlerObj) 
                                (mf::get-handler-object handlerObj)
                                nil
                                :preserve :preserve scriptText t
                                (method-function handlerObj)
                                globals locals nil nil nil nil :preserve nil :preserve :preserve t 
                                version)))))

(defun getHandlerInfo (name object qualifier &key version)
  (setq name (mf::make-internal-function-name name object))
  (copy-all-pending-handlers)
  (let ((record (find-handler-record name object qualifier version)))
    (when record
      (values (hand-version record)
              nil ; *** from (hand-flags record)
              nil ; *** from (hand-flags record)
              (if object
                (ps::transform-script-for-new-object (hand-script record) (sk8:project object) object)
                (hand-script record))
              (hand-globals record)
              (hand-locals record)
              (hand-handlers record)
              (hand-breakpoints record)
              (hand-wstate record)
              (hand-wexps record)
              (hand-returns record)
              (hand-indentation record)
              (hand-editor record)
              (hand-doc record)))))

;;; handlerVersions -- returns a list of all the handler versions in the store, starting
;;;                with the most recent version
;;;
(defun handlerVersions (name obj qualifier)
  (setq name (mf::make-internal-function-name name))
  (let ((activerec (find-handler-records name obj qualifier)))
    (when activerec
      (mapcar #'hand-version (active-records activerec)))))

;;; activeHandler -- returns the version of the active handler that meets the criteria
;;;
(defun activeHandler (name object qualifier)
  (setq name (mf::make-internal-function-name name))
  (let ((activerec (find-handler-records name object qualifier)))
    (when activerec
      (active-version activerec))))

;;; A function that removes all the old entries of a handler or function.

(defun deleteOldHandlerVersions (proj)
  (let ((table (SK8::handler-record-table proj))
        name obj qualifier versionsavailable activeversion)
    (when table
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (setf name (car key) obj (cadr key) qualifier (caddr key))
                   (setf versionsavailable (handlerversions name obj qualifier))
                   (when (> (length versionsavailable) 1)
                     (setf activeversion (activehandler name obj qualifier))
                     (dolist (c versionsavailable)
                       (unless (= c activeversion)
                         (purgeHandler name obj qualifier :version c)))))
               table))))

(defun deleteOldFunctionVersions (proj)
  (let ((table (SK8::function-record-table proj))
        versionsavailable activeversion)
    (when table
      (maphash #'(lambda (name value)
                   (declare (ignore value))
                   (setf versionsavailable (functionversions proj name))
                   (when (> (length versionsavailable) 1)
                     (setf activeversion (activefunction proj name))
                     (dolist (c versionsavailable)
                       (unless (= c activeversion)
                         (purgeFunction proj name :version c)))))
               table))))

(defun compactProjectCode (proj)
  (deleteOldFunctionVersions proj)
  (deleteOldHandlerVersions proj))
                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBALS
;;;


;;; saveGlobal -- saves a global into the current project version
;;;  proj - the global's project (an object)
;;;  name - the global's name (a symbol)
;;;  value - the global's value (a thing)
;;;  private - if true, then global is private
;;;  locked - if true, then global is locked from modifications
;;;  type - the type of value
;;;  functionName - the name of any function or handler in which the constant was declared
;;;  handlerObject - the handler's object (if functionName is a handler)
;;;  handlerQualifier - the handler's qualifier (if functionName is a handler)
;;;  doc - global documentation
;;;  constant - if nil (the default), then this is a variable; else it is a constant
;;;
;;; Returns: the project version id in which it was saved
;;;
(defun saveGlobal (proj name value private locked type functionName
                             handlerObject handlerQualifier doc &key constant)
  (declare (ignore private locked))
  (let ((record (make-global :name name
                             :value value
                             :flags 0 ; *** not done (private and locked)
                             :type type
                             :functionName functionName
                             :handlerObject handlerObject
                             :handlerQualifier handlerQualifier
                             :doc doc)))
    (let (table)
      (if constant
        (progn
          (setf table (sk8::constant-record-table proj))
          (unless table
            (setf table (make-hash-table :test #'eq :weak t))
            (setf (sk8::constant-record-table proj) table)))
        (progn
          (setf table (sk8::variable-record-table proj))
          (unless table
            (setf table (make-hash-table :test #'eq :weak t))
            (setf (sk8::variable-record-table proj) table))))
      (setf (gethash name table) record))))

;;; purgeGlobal -- purges the global's version from the store
;;;  proj - the global's project (an object)
;;;  name - the global's name (a symbol)
;;;  constant - if true, then the global is a constant; else it is a variable
;;;
(defun purgeGlobal (proj name &key constant)
  (let ((table (if constant (sk8::constant-record-table project) (sk8::variable-record-table proj))))
    (when table (remhash name table))))

(defun getGlobalInfo (proj name &key constant)
  (let ((table (if constant
                 (sk8::constant-record-table proj)
                 (sk8::variable-record-table proj))))
    (when table
      (let ((record (gethash name table)))
        (when record
          (values (global-created record)
                  nil  ; *** not done (proj-modified)
                  (global-value record)
                  nil  ; *** not done (private)
                  (global-type record)
                  (global-functionName record)
                  (global-handlerObject record)
                  (global-handlerQualifier record)
                  (global-doc record)))))))

;;; globals -- returns a list of all the global names in the project file
;;;  proj - the project object
;;;  constant - if true, then look for constants; else look for variables
;;;
(defun globals (proj &key constant)
  (let ((table (if constant
                 (sk8::constant-record-table proj)
                 (sk8::variable-record-table proj)))
        result)
    (when table
      (maphash #'(lambda (key record) (declare (ignore key))
                  (push (global-name record) result))
               table)
      result)))

;;; PROJECT BUILDER LIBRARY UTILITIES
;;;

;;; searchScripts -- given a text string, will search and match scripts in a project according to criteria
;;;  project - the project in which to find the string
;;;  text - text string to match
;;;  case - whether search should be cases sensitive or not (if true, it is)
;;;  count - number of desired matches (default: return all possible matches)
;;;  functions - whether to search the project's functions (default: yes)
;;;  handlers - whether to search the project's handlers (default: yes)
;;;  active - if true, then only the text of active handlers and functions are searched (default: false)
;;;
;;;  Returns two lists: the first list contains the matched handlers and the
;;;  second list the matched functions. The handlers list consists of entries
;;;  which are lists of the form (handler-name object-name handler-qualifier handler-version)
;;;  and the functions list consists of entries of the form (function-name function-version)
;;;
;;; ** should use PS::HANDLERS and PS::FUNCTIONS UTILITIES

(defun searchScripts (proj text &key (case nil) (count nil)
                                 (functions t) (handlers t)
                                 (active nil))
  (setq case (if case #'string= #'string-equal))
  (let (h
        f)
    (catch :finished
      (when handlers ; search handlers
        (mapactivehandlers
         proj
         (if active
           #'(lambda (name obj qualifier setter activerec)
               (let ((record (find (active-version activerec) (active-records activerec) :key #'hand-version)))
                 (when record
                   (when (search text (hand-script record) :test case)
                     (push (list name obj qualifier setter (hand-version record)) h)
                     (when (and count (zerop (decf count)))
                       (throw :finished nil))))))
           #'(lambda (name obj qualifier setter activerec)
               (dolist (handler (active-records activerec))
                 (when (search text (hand-script handler) :test case)
                   (push (list name obj qualifier setter (hand-version handler)) h)
                   (when (and count (zerop (decf count)))
                     (throw :finished nil))))))))
      (when functions ; search functions
        (mapactivefunctions
         proj
         (if active
           #'(lambda (name setter activerec)
               (declare (ignore setter))
               (let ((record (find (active-version activerec) (active-records activerec) :key #'fun-version)))
                 (when record
                   (when (search text (fun-script record) :test case)
                     (push (list name proj (fun-version record)) f)
                     (when (and count (zerop (decf count)))
                       (throw :finished nil))))))
           #'(lambda (name setter activerec)
               (declare (ignore setter))
               (dolist (function (active-records activerec))
                 (when (search text (fun-script function) :test case)
                   (push (list name proj (fun-version function)) f)
                   (when (and count (zerop (decf count)))
                     (throw :finished nil)))))))))
    (values h f)))

(defun bringUpEditorOnScriptsUsingVarOrConstant (sym proj &key (globals t) (locals t) (watch t))
  (declare (ignore watch))
  (let (result)
    (mapactivehandlers proj
                       #'(lambda (name obj qualifier setter activerec)
                           (let ((record (find (active-version activerec) (active-records activerec) :key #'hand-version)))
                             (when (and record
                                        (or (and locals
                                                 (memq sym (hand-locals record)))
                                            (and globals
                                                 (memq sym (hand-globals record)))))
                               (push (list name obj qualifier setter (hand-version record))
                                     result)))))
    (mapactivefunctions proj
                        #'(lambda (name setter activerec)
                            (let ((record (find (active-version activerec) (active-records activerec) :key #'fun-version)))
                              (when (and record
                                         (or (and locals
                                                  (memq sym (fun-locals record)))
                                             (and globals
                                                  (memq sym (fun-globals record)))))
                                (push (list name setter (fun-version record))
                                      result)))))
    (cond (result
           (if (cdr result)
             (bringupeditor proj (car (select-item-from-list
                                          result
                                          :table-print-function #'(lambda (result s)
                                                                    (if (> (length result) 3)
                                                                      (format s "Handler ~:[set~;~] ~a of ~a in ~a"
                                                                              (fourth result) (first result) (sk8:objectstring (second result))
                                                                              (sk8:objectstring proj))
                                                                      (format s "Function ~:[set~;~] ~a in ~a"
                                                                              (second result) (first result) (sk8:objectstring proj))))
                                          :window-title (format nil "Handler/Function With ~a To Edit" sym))))
             (bringupeditor proj (car result))))
          (t (sk8:messagetouser "Nothing found!")))))

(defun bringUpEditorOnScriptsUsingHandlerOrFunctionSymbol (sym proj &key (watch t))
  (declare (ignore watch))
  (let (record result)
    (mapactivehandlers proj
                       #'(lambda (name obj qualifier setter activerec)
                           (let ((record (find (active-version activerec) (active-records activerec) :key #'hand-version)))
                             (when (and record
                                        (assq sym (hand-handlers record)))
                               (push (list name obj qualifier setter (hand-version record))
                                     result)))))
    (mapactivefunctions proj
                        #'(lambda (name setter activerec)
                            (let ((record (find (active-version activerec) (active-records activerec) :key #'fun-version)))
                              (when (and record
                                         (assq sym (fun-handlers record)))
                                (push (list name setter (fun-version record))
                                      result)))))
    (cond (result
           (if (cdr result)
             (bringupeditor proj (car (select-item-from-list
                                          result
                                          :table-print-function #'(lambda (result s)
                                                                    (if (> (length result) 3)
                                                                      (format s "Handler ~:[set~;~] ~a of ~a in ~a"
                                                                              (fourth result) (first result) (sk8:objectstring (second result))
                                                                              (sk8:objectstring proj))
                                                                      (format s "Function ~:[set~;~] ~a in ~a"
                                                                              (second result) (first result) (sk8:objectstring proj))))
                                          :window-title "Select A Handler/Function to Edit")))
             (bringupeditor proj (car result))))
          (t (sk8:messagetouser "Nothing found!")))))

(defun bringupeditor (proj result)
  (let* ((handler (> (length result) 3))
         (object (when handler (second result)))
         (qualifier (if handler (third result)))
         (setter (if handler (fourth result) (second result)))
         (name (if setter (list 'cl::setf (first result)) (first result)))
         (version (if handler (fifth result) (third result))))
    (if handler
      (sk8:edithandler proj name object qualifier version)
      (sk8:edithandler proj name))))


#|
	Change History (most recent last):
	2  	 5/13/96	Hernan  	handler-record-table moved from SK8 to MF.
	3  	 5/20/96	Brian   	function-record-table is in MF::
	4  	 5/20/96	sidney  	define make-load-form for the structs so they can be saved as fasls
						function-record-table is back in SK8 package
						everything that is explicitly in SK8 package is now external
	5  	 7/ 7/96	sidney  	changes for native PPC build
	6  	 7/29/96	Hernan  	If activeFunction is called with a string, change it into a symbol.
	7  	 7/30/96	Hernan  	saveHandlerVersionFromText cannot use newGetLfun because
						it fails for set handlers. Not a problem since from the generic function
						we can easily obtain the lfun by calling method-function.
	8  	10/10/96	sidney  	searchscripts was ignoring whether handler was setter or not
	9  	10/10/96	Hernan  	Adding a function to remove all old versions of handlers,
						functions and globals in a project.
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
