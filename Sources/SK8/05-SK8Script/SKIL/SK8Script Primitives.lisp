;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)
(SK8-declare-syms :SK8 :public ; Updated 10-21-96  12:24 pm
                  SK8::CALL-DO-INHERITED SK8::CALL-WITH-STATEMENT SK8::INSERTAFTER SK8::INSERTBEFORE
                  SK8::SK8SCRIPTRESULT SK8::TRUE) 

;;______________________________________________________________________________

(define-sk8-var SK8ScriptResult :initial-value nil)

;;_________________________________________________________________________________________________________

(defmacro call-do-inherited (setter? handName &rest args)
  (let ((meonly (and args (listp args) (not (cdr args)) (eq (car args) 'sk8::me))))
    (if setter?
      `(if (next-method-p)
         ,(if (and args (not meonly))
            `(call-next-method ,@args)
            `(call-next-method))
         (SK8::setValue ',handName ,(cadr args) ,(car args)))
      `(if (next-method-p)
         ,(if (and args (not meonly))
            `(call-next-method ,@args)
            `(call-next-method))
         (SK8::getValue ',handName ,(car args))))))


;;_________________________________________________________________________________________________________

(in-package :sk8dev)

(defun !setHandlerName? (sym)
  (eq (symbol-package sym) CCL::*SETF-PACKAGE*))

(defun !withHandlerName? (sym)
  (let ((nameString (symbol-name sym)))
    (and (> (length nameString) 5) (string= "WITH " nameString :end2 5))))

(defun !nicefy-handlerName (handlerName)
  (if (consp handlerName)
    (CCL::SETF-FUNCTION-NAME (second handlerName))
    handlerName))

(defun !printHandlerArgs (strm name obj lambdalist &key declaration call (primaryArgument t) (setterArgument t))
  ;; Can only omit setter arg and primary arg in calling form
  (unless call (setq setterArgument t primaryArgument t))
  (when call (setq declaration t))
  (flet ((!nicefy-lambdalist (handlerName handlerObject arglist forDecl? includeSetterArg?)
           (declare (list arglist))
           (when arglist
             (let ((extraKeys nil)
                   keys? keys)
               (when (eq (first arglist) 'CCL::&METHOD) (setq arglist (cddr arglist)))
               (cond
                ((or (consp handlerName) (!setHandlerName? handlerName))
                 (setq keys? (memq '&KEY arglist)
                       keys (cdr keys?))
                 (unless (or (member-if #'listp keys)
                             (member "TO" keys :test #'equal :key #'symbol-name))
                   (setq arglist (cdr arglist))
                   (when includeSetterArg?
                     (setq extraKeys (if keys?
                                       (if forDecl? '((:to "newValue")) '(:to))
                                       (if forDecl? '(&KEY (:to "newValue")) '(&KEY :to)))))))
                ((!withHandlerName? handlerName)))       
               (when (and forDecl?
                          (setq keys (cdr (memq '&KEY arglist)))
                          (not (member-if #'listp keys)))
                 (let ((currentArgs nil)
                       keyword
                       arg)
                   (doheads (remainingKeys keys)
                     (setq keyword (car remainingKeys))
                     (when (memq keyword '(&REST &OPTIONAL)) (return))
                     (setq arg (char-downcase (aref (symbol-name keyword) 0)))
                     (loop
                       (if (member arg currentArgs :test #'(lambda (a b)
                                                             (if (characterp a)
                                                               (eq a b)
                                                               (when (stringp b) (string= a b)))))
                         (setq arg (if (characterp arg)
                                     (make-string 2 :initial-element arg)
                                     (make-string (1+ (length arg)) :initial-element (char arg 0))))
                         (return)))
                     (push arg currentArgs)
                     (setf (car (the list remainingKeys)) (list keyword arg)))))   
               ;; Make sure there's no &allow-other-keys
               (setq arglist (delete '&ALLOW-OTHER-KEYS arglist :test #'eq))      
               ;; Since we may have the arglist from a system-generated accessor method, ensure 1st arg is ME
               (when (and arglist handlerObject)
                 (setf (first arglist) 'SK8::ME))
               (if extraKeys
                 (nconc arglist extraKeys)
                 arglist)))))
    (let* ((args (!nicefy-lambdalist name obj lambdalist declaration setterArgument))
           (first? t)
           optpos opts keypos keys restpos rest)
      
      (when (setq restpos (position '&REST args))
        (setq rest (nth (1+ restpos) args)))
      (when (and (setq keypos (position '&KEY args))
                 (< keypos (length args))
                 (or (not restpos) (< keypos restpos)))
        (setq keys (subseq args (1+ keypos) restpos)))
      (when (and (setq optpos (position '&OPTIONAL args))
                 (< optpos (length args)))
        (setq opts (subseq args (1+ optpos) (or keypos restpos))))
      (setq args (if (or optpos keypos restpos)
                   (subseq args 0 (or optpos keypos restpos))
                   (copy-list args)))
      (unless primaryArgument (setq args (cdr args)))
      ;; (nsubstitute 'me "SELF" args :test #'equal :key #'symbol-name)
      
      (cond
       ;; Just want to see the arglist
       ((not declaration)
        (when args
          (format strm "~{~a~^, ~}" args))
        (when opts
          (write-string (if args " [, " "[") strm)
          (format strm "~{~a~^, ~}]" opts))
        (when keys
          (setq first? (not (or args opts)))
          (dolist (k keys)
            (format strm "~:[~; ~]~:[~;~a ~]~a:"
                    (not first?)
                    t
                    (if (or (and (listp k) (eq (first k) :to))
                            (eq k :to))
                      "" "with")
                    k)
            (setq first? nil)))
        (when rest
          (if (or args opts keys)
            (format strm " [, ~a]*" rest)
            (format strm "[~a]*" rest))))
       
       ;; Want the arglist in the form of a declaration (i.e. for a handler definition)
       (t
        (when (and opts keys) (setq keys nil)) ; since we don't support &optional args together with &key args
        (when obj (setq obj (ignore-errors (SK8::objectName obj))))
        (flet ((print-dispatch-arg (objname s call)
                 (if call
                   (princ (if (stringp call) call "me") s)
                   (format s "me (a~:[~;n~] ~a)"
                           (if (eql 1 (length objname))
                             (position (aref objname 0) "aefhilmnorsx" :test #'char-equal)
                             (position (aref objname 0) "AEIOUaeiou" :test #'eq))
                           objname))))
          (when args
            (princ "of " strm)
            (if (or obj (stringp call))
              (print-dispatch-arg obj strm call)
              (princ (car args) strm))
            (when (cdr args) (format strm "~{, ~a~}" (cdr args))))
          (when opts
            (write-string (if args " [, " "[") strm)
            (format strm "~{~a~^, ~}]" opts))
          (when keys
            (setq first? (not (or args opts)))
            (dolist (k keys)
              (format strm "~:[~; ~]~:[~;~a ~]~a ~a"
                      (not first?)
                      t
                      (if (or (and (listp k) (eq (first k) :to))
                              (eq k :to))
                        "" "with")
                      (first k)
                      (second k))
              (setq first? nil)))))))))

(defun !minimalExactLfunInfo (lfunOrClosure)
  (when lfunOrClosure    ;; protection aagainst some situations in which an error-frame has a null lfun in it
    (let* ((lfun (ccl::closure-function lfunOrClosure))
           (name (CCL::LFUN-VECTOR-NAME (CCL::%LFUN-VECTOR lfun)))
           (hProj nil)
           (hName nil)
           (hObject nil)
           (hQualifier nil)
           (bodyOwner nil))
      (cond
       ((symbolp name)
        ;; *** Should we do something special for |<name> FOR MODS| ?
        (setq hName name))
       
       ((listp name)
        (setq hName (gethash (first (cdr (the list name))) CCL::%SETF-FUNCTION-NAMES%)))
       
       ((functionp name)
        (setq bodyOwner name)
        (multiple-value-setq (hProj hName hObject hQualifier) (!minimalExactLfunInfo bodyOwner)))
       
       ((CCL::STANDARD-METHOD-P name)
        (when (setq hObject (mf::get-handler-object-from-lfun lfun))
          (setq hQualifier (first (method-qualifiers name)))
          (setq hName (method-name name))
          (when (and hName (listp hName))
            (setq hName (gethash (first (cdr (the list hName))) CCL::%SETF-FUNCTION-NAMES%)))))
       
       (t
        (setq hName name)))
      
      (unless hProj
        (setq hProj (if hObject
                      (ps::owning-project hObject)
                      (mf::get-function-project lfun))))
      
      (values hProj hName hObject hQualifier bodyOwner))))

(defun !exactLfunInfo (lfunOrClosure)
  (let* ((lfun (ccl::closure-function lfunOrClosure))
         (name (CCL::LFUN-VECTOR-NAME (CCL::%LFUN-VECTOR lfun)))
         (hProj nil)
         (hName nil)
         (hObject nil)
         (hQualifier nil))
    
    (cond
     ((symbolp name)
      ;; *** Should we do something special for |<name> FOR MODS| ?
      (setq hName name))
     
     ((listp name)
      (setq hName (gethash (first (cdr (the list name))) CCL::%SETF-FUNCTION-NAMES%)))
     
     ((functionp name)
      (multiple-value-setq (hProj hName hObject hQualifier) (!exactLfunInfo name)))
     
     ((CCL::STANDARD-METHOD-P name)
      (unless (setq hObject (mf::get-handler-object name))
        ;; The object this handler was defined on can't be determined, so just return the first specializer class as the "object".
        (setq hObject (first (member #.(find-class t) (method-specializers name) :test-not 'eq))))
      (setq hQualifier (first (method-qualifiers name)))
      (setq hName (method-name name))
      (when (and hName (listp hName))
        (setq hName (gethash (first (cdr (the list hName))) CCL::%SETF-FUNCTION-NAMES%))))
     
     (t
      (setq hName name)))
    
    (unless hProj
      (setq hProj (if hObject
                    (ps::owning-project hObject)
                    (mf::get-function-project lfun)))
      (unless hProj
        ; *** (when (and (symbolp hName) (!SK8externalSym hName))
        (when hName (setq hProj MF::*SK8-PROJECT*))))
    
    (values hProj hName hObject hQualifier)))

(defun best-lambdalist-for-handler (setterName handlerName &optional handlerObject handlerQualifier)
  (let (theargs)
    (setf theargs (cond
                   (handlerObject
                    (let* ((setterLfun (and setterName (!newgetLfun (sk8::project handlerObject) setterName handlerObject handlerQualifier)))
                           (getterLfun (unless setterLfun (!newgetLfun (sk8::project handlerObject) handlerName handlerObject handlerQualifier)))
                           (addSetterArg? (and setterName getterLfun))
                           (lfun (or setterLfun getterLfun)))
                      (unless lfun
                        (cond
                         (setterName
                          (unless (setq lfun (mf::find-applicable-handler setterName handlerObject))
                            (when (setq lfun (mf::find-applicable-handler handlerName handlerObject))
                              (setq addSetterArg? t))))
                         (t
                          (setq lfun (mf::find-applicable-handler handlerName handlerObject))))
                        (cond
                         (lfun
                          (setq lfun (method-function lfun)))
                         (setterName
                          (unless (setq lfun (fboundp setterName))
                            (when (setq lfun (fboundp handlerName))
                              (setq addSetterArg? t))))
                         (t
                          (setq lfun (fboundp handlerName)))))
                      (if lfun
                        (let ((arglist (arglist (ccl::closure-function lfun))))
                          (if addSetterArg?
                            (cons 'newValue arglist)
                            arglist))
                        (if setterName (list 'newValue 'me) '(me)))))
                   
                   (setterName
                    (or (arglist setterName)
                        (cons 'newValue (arglist handlerName))))
                   (t
                    (arglist handlerName))))
    ;;; obsolete (if (eq (car theargs) *sx-with-context-body-function-name*)   ;;;This is a bogus argument which means the body...remove it for arglist
    ;;; obsolete   (setf theargs (cdr theargs)))
    theargs))

(defun handlerIDstring (handlerName &optional handlerObject handlerQualifier &key declaration ((:project proj)))
  (let ((real-handlerName handlerName)
        (setterName nil)
        IDstring)
    
    (setq IDstring (with-output-to-string (strm)
                     
                     (when (or declaration handlerQualifier)
                       (princ (or handlerQualifier "on") strm)
                       (write-char #\Space strm))
                     
                     (cond
                      ((and (consp handlerName) (eq (first handlerName) 'SETF))
                       (setq handlerName (second handlerName)
                             setterName (CCL::setf-function-name handlerName)))
                      ((eq (symbol-package handlerName) CCL::*setf-package*)
                       (let ((*readtable* CCL::%initial-readtable%))
                         (setq setterName handlerName
                               handlerName (read-from-string (symbol-name handlerName))))))
                     (when setterName (write-string "set " strm))
                     (princ handlerName strm)
                     (when proj (SK8::maybeWriteProjectQualification handlerName strm :project proj))
                     
                     (cond
                      (declaration
                       (write-char #\Space strm)
                       (!printHandlerArgs
                        strm real-handlerName handlerObject
                        (best-lambdalist-for-handler setterName handlerName handlerObject handlerQualifier)
                        :declaration t))
                      (handlerObject
                       (write-string " of " strm)
                       (SK8::writeObject handlerObject strm t)))))
    (if declaration
      (values IDstring
              (with-output-to-string (strm)
                (format strm "end ~@[~*set ~]~a" setterName handlerName)
                (when proj (SK8::maybeWriteProjectQualification handlerName strm :project proj))))
      IDstring)))

#|
	Change History (most recent last):
	2  	 4/ 3/96	Brian   	Fixing collections.
	3  	 4/ 3/96	Hernan  	 moving add-skil-foreign-function
	4  	 4/10/96	Hernan  	Removing case preserving reader macro.
	5  	 4/15/96	Hernan  	
	6  	 4/19/96	Brian   	
	7  	 4/22/96	Brian   	
	8  	 4/26/96	Brian   	
	9  	 4/30/96	Brian   	
	10 	 5/ 6/96	Brian   	Removing dashes from SKIL locals.
	11 	 7/ 8/96	Brian   	
	12 	 9/ 3/96	Hernan  	Removing calls to internal functions...
	13 	10/10/96	Brian   	
	14 	10/10/96	Brian   	
	15 	10/10/96	Brian   	Removing a print statement..
	16 	10/21/96	Hernan  	Adding SK8ScriptResult to the SK8Script runtime.
	17 	10/21/96	Hernan  	Making sk8scriptresult a real sk8 variable.
	18 	11/22/96	Brian   	More error checking in printhandlerargs
	19 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
