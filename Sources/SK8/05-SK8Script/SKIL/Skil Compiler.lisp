;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :skil)

#|

A Rough Draft Language Definition

A valid language statement is a list, ala lisp.
The first item in this list is either a reserved word, (if script set etc.), or a function call.


Programs:
(SCRIPT
 [(key [<args>]*)]*    key arg pairs for specifying additional information
 [<statement>]*)       the body of the program

Script return the value of their last statement

valid keys arg pairs are:
INPUT:    followed by and arg list (possibly empty).  args are of the form argName | (argName type)
          this program is a lambda expression and takes the arguments specified in the args
          (n.b. in sk8, the types associated with the args imply that the object is a handler)

NAME:     followed by a symbol to use as a name or by (SET name) or by (WITH name)
          this program is a function to be labelled by the name
          set and with refer to special setter and with handlers.
          (n.b. in sk8, handlers require a name)
LOCALS:   this is followed by a list of variable declarations of 
          the form  VARNAME | (VARNAME <valueStatement>)
          the values can be an arbitrary statement.  the values are evaluated in order and can use previous
          locals.
          (n.b. in lisp, this corresponds to let*)
ON-ERROR: the first arg is an error type, the rest are statements
          if an error of the appropriate type occurs at any point in the execution of the
          programs code, the statements get executed and the program returns
CLEANUP-CODE:
          takes a list of statements
          Only valid in with handlers, this code is the code that is gauranteed to be
          executed whether or not an error occurs


(n.b. globals is no longer needed in skil because of LIB, and because all non locals are global)


Conditionals:  (fairly standard)
(IF <test> THEN <statement>* [ELSE-IF <test> THEN <statement>*]* [ELSE <statement>*])


Setting:
(SET <variablename> TO <statement>)  variable setting
(SET <libStatement> TO <statement>)  external environment variable setting
(SET <functioncall> TO <statement>)  property setting (set (fillcolor curActor) to newColor)

Accessing Globals in the Environment:
(LIB <place/module> OBJECT <name>)         compiles to a direct reference to that name in that place
(LIB <place/module> FUNCTION <name>)       compiles to a direct reference to that function in that place

Function Calls:
(funName [<arg>]* [WITH [<keyArgName> <valueStatement>]*)

declaring globals:
(DECLAREGLOBAL <variableName>)  makes a new global in the current environment with the specified name

Core Functions:

Loop: executes a series of statements over and over
Catch and Throw: 
   takes a symbol and a set of statements.  standard catch and throw.
   the only three symbols that catch and throw will get are:  NEXT-REPEAT, EXIT-REPEAT, and CURRENT-FUNCTION

Funcall:: first arg is a function name or a script statment which is applied to the rest of the arguments


|#

;;___________________________________________________________________________________________________________________
;;___________________________________________________________________________________________________________________

;;alpha renaming functionality:  this takes a skil form and renames all locals according to some function.
;;this is used to avoid conflicts with reserved words in the target language (e.g. a local named "t" in lisp)
;;and can be used for dealing with lexical scoping (e.g. 
;;(script (local x) (set x to 2) (+ (script (local x) (set x to 1) x) x)))


;;two renaming functions:
;; this handles both name conflicts and lexical scoping but generated code is ugly.
(defun skil-rename-full-gensym (sym)
  (let ((*package* (symbol-package sym)))
    (read-from-string (symbol-name (gensym (symbol-name sym))) nil nil)
    ))
;;this makes nice code and handles name conflicts buts doesn't handle lexical scoping
(defun skil-rename-simple-append (sym)
  (let ((*package* (symbol-package sym)))
    (read-from-string (format nil "~aVar" (symbol-name sym)) nil nil)
    ))

;;the renaming code:
(defun deep-search-for-renaming (form renaming-function)
  (dolist (i form)
    (when (listp i)
      (deep-search-for-renaming i renaming-function))
    (check-for-renaming i renaming-function)))

(defun do-alpha-renaming (form oldsym newsym)
  (let (curform 
        (encounteredWith nil))
    (ccl::while form
      (setf curForm (first form))
      (cond 
       ((and
         (listp curForm)
         (not (memq (first curForm) '(lib
                                      global
                                      name
                                      quote
                                      ))))
        (do-alpha-renaming curForm oldsym newsym))
       ((and (symbolp curForm)
             (eq curForm oldsym))
        (setf (first form) newsym)))
      (if (eq curForm 'with)
        (setf encounteredWith t))
      (setf form (rest form))
      (if encounteredWith (setf form (rest form))))
    ))

(defun check-for-renaming (form renaming-function)
  (when (and (listp form) (eq (first form) 'script))
    (let ((inputs (skil-get-key form 'input t))
          (locals (skil-get-key form 'local nil)))
      (if (memq 'with inputs)
        (setf inputs (subseq inputs 0 (position 'with inputs))))
      (setf locals (append inputs locals))
      (when locals 
        (dolist (i locals)
          (if (listp i) (setf i (first i)))
          (when (neq i 'sk8::me)
            (do-alpha-renaming form i (funcall renaming-function i))))
        ))))

(defun skil-alpha-rename (form renaming-function)
  (when (listp form)
    (setf form (deep-copy-list form))
    (deep-search-for-renaming (list form) renaming-function))
  form)

;;(skil-alpha-rename '(script (local t) (set t to 2) (+ t 1)) 'skil-rename-create-name)
;;(skil-alpha-rename '(script (local t) (set t to 2) (+ t 1)) 'skil-rename-simple-append)


(defparameter lisp-alpha-rename-function 'skil-rename-simple-append)
;;___________________________________________________________________________________________________________________
;;___________________________________________________________________________________________________________________
(defparameter skil-primitive-functions (make-hash-table :test #'eq))

(defun add-skil-primitive-function (funsym lispFun)
  (setf (gethash funsym skil-primitive-functions) lispfun)
  t)

(defun get-primitive-function (funsym)
  (gethash funsym skil-primitive-functions))

;;___________________________________________________________________________________________________________________
(defparameter skil-foreign-functions (make-hash-table :test #'eq))

(defun add-skil-foreign-function (funsym lispFun)
  (unless (fboundp lispFun) (error (format nil "can't add function ~s cause it's not bound." lispfun)))
  (setf (gethash funsym skil-foreign-functions) lispfun)
  t)

(defun get-foreign-function (funsym)
  (gethash funsym skil-foreign-functions))


;;___________________________________________________________________________________________________________________

;;need to clean these up and rename them (use either dashes or not)
;;basic functionality we keep the same
(add-skil-foreign-function 'loop 'loop)
(add-skil-foreign-function 'funcall 'funcall)
(add-skil-foreign-function 'apply 'apply)

;;  the only three cases that catch and throw will get:  NEXT-REPEAT, EXIT-REPEAT, and CURRENT-FUNCTION
(add-skil-foreign-function 'catch 'catch)
(add-skil-foreign-function 'throw 'throw)

;;we rely on symbols, so we need a way to make them...
(add-skil-foreign-function 'quote 'quote)

;;basic operators
(add-skil-foreign-function '^ 'sk8::^)
(add-skil-foreign-function '- 'sk8::-)
(add-skil-foreign-function '+ 'sk8::+)
(add-skil-foreign-function '* 'sk8::*)
(add-skil-foreign-function '/ 'sk8::/)
(add-skil-foreign-function 'div 'sk8::div)
(add-skil-foreign-function 'mod 'sk8::mod)
(add-skil-foreign-function '< 'sk8::<)
(add-skil-foreign-function '> 'sk8::>)
(add-skil-foreign-function '<= 'sk8::<=)
(add-skil-foreign-function '>= 'sk8::>=)
(add-skil-foreign-function '= 'sk8::=)
(add-skil-foreign-function '& 'SK8::&)
(add-skil-foreign-function 'not 'not)
(add-skil-foreign-function 'and 'and)  
(add-skil-foreign-function 'or 'or)   
(add-skil-foreign-function 'xor 'xor)
(add-skil-foreign-function 'eq 'eq)

;;;special operators that are part of the language
(add-skil-foreign-function 'startsWith 'SK8::startsWith)
(add-skil-foreign-function 'endswith 'SK8::endswith)
(add-skil-foreign-function 'contains 'SK8::contains)
(add-skil-foreign-function 'is-a 'SK8::is-a)
(add-skil-foreign-function 'as-a 'sk8::as)

;;special calling macros for weird constructs
(add-skil-foreign-function 'call-do-inherited 'SK8::call-do-inherited)


;;;For collections we need these, and the collection protocol
(add-skil-foreign-function 'singleItemMapper 'sk8::singleItemMapper)
(add-skil-foreign-function 'fullRangeMapper 'SK8::fullRangeMapper)
(add-skil-foreign-function 'insertInFront 'SK8::insertInFront)
(add-skil-foreign-function 'insertAtEnd 'SK8::insertAtEnd)
(add-skil-foreign-function 'anyindex 'SK8::anyindex)
(add-skil-foreign-function 'middleIndex 'SK8::middleIndex)

;;;__________________________
;;for our development purposes only.  don't need to be compiled to any other runtime
(add-skil-foreign-function 'error 'error)
(add-skil-foreign-function 'print 'print)

;;for the sk8 debugger.  also doesn't need to be cross compiled.
(defmacro debugging-setup (lineNo)
  `(progn 
     (setq %%location ,lineNo)
     (when %%break-fn (funcall %%break-fn %%location))))
(add-skil-foreign-function 'debugging-setup 'debugging-setup)

;;___________________________________________________________________________________________________________________
;;___________________________________________________________________________________________________________________
;;___________________________________________________________________________________________________________________
(defun skil-compile-form (form)
  (if (and form (listp form))
    (cond
     ;;; lib forms are function calls.
     ((and (listp (car form)) (eq (caar form) 'lib))
      (if (memq (fourth (car form)) '(SK8::new))  ;;;SK8::copy doesn't seem to require it anymore?!?!
        (skil-compile-new (cons (skil-compile-form (car form)) (cdr form)))
        (skil-compile-function (cons (skil-compile-form (car form)) (cdr form)))))
     ;;; check if first item is a primitive compiler form (e.g. script)
     ((get-primitive-function (car form))
      (funcall (get-primitive-function (car form)) 
               form))
     ;;; check if first item is a foreign function (e.g. +)
     ((get-foreign-function (car form))
      (skil-compile-function (cons (get-foreign-function (car form)) 
                                   (if (eq (car form) 'as-a) (append (cdr form) `(with project ,*current-project*)) (cdr form))
                                   )))
     ;;; otherwise pass it on through.  this is really an error that needs to be fixed.
     (t
      (error (format nil "Unknown SKIL type: ~s~%" (car form)))
      ;;(skil-compile-function form)
      )
     )
    form))

(defun skil-run (form)
  (let ((*save-definitions* nil)
        (*save-local-symbols* t)   ;;*** kludge alert! this must be t so there will be a lfun-info for us to save stuff in
        (*fasl-save-local-symbols* t)
        (*save-doc-strings* nil)
        (*record-source-file* nil)  
        (CCL::*SUPPRESS-COMPILER-WARNINGS* T)
        )
    (eval (skil-compile-to-lisp form))))


(defun skil-compile-to-lisp (form)
  (let ((CCL::*SUPPRESS-COMPILER-WARNINGS* T))
    (skil-compile-form (skil-alpha-rename form lisp-alpha-rename-function))))

(defun skil-eval (form)
  (skil-run form))
;;___________________________________________________________________________________________________________________

(defun skil-compile-function (form)
  (let ((res (list (pop form)))
        curkey
        curitem)
    (ccl::while (and form (neq (setf curitem (pop form)) 'with))
      (setf res (nconc res (list (skil-compile-form curitem)))))
    (ccl::while form
      (setf curkey (pop form))
      (unless (symbolp curkey) (error "keywords must be symbols"))
      (setf curkey (ccl::keywordify curkey))
      (setf res (nconc res (list curkey (skil-compile-form (pop form))))))
    res))

;;___________________________________________________________________________________________________________________

(defun skil-compile-new (form)
  (let ((baseCompile (skil-compile-function form)))
    (unless (memq :project basecompile)
      (setf basecompile (append basecompile (list :project *current-project*))))
    basecompile))

;;___________________________________________________________________________________________________________________
(defun skil-compile-set-to (form)
  (let ((varval (second form))
        (toval (third form))
        (valval (fourth form)))
    (unless (or (= (length form) 4) (eq toval 'to))
      (error (format nil "Set forms must be of the form set var to val ~%~s" form)))
    (unless (or (not (listp varval))
                (eq (car varval) 'lib)
                (eq (caar varval) 'lib))
      (error (format nil "Invalid place to set to in ~s" form)))
    `(setf ,(skil-compile-form varval) ,(skil-compile-form valval)))
  )

(add-skil-primitive-function 'set #'skil-compile-set-to)

;;___________________________________________________________________________________________________________________
(defun skil-compile-declareglobal (form)
  (let (varname vartype varval)
    (declare (ignore-if-unused vartype))
    (if (listp (second form))
      (progn
        (setf varname (first (second form)))
        (setf vartype (second (second form)))
        (setf varval (third (second form)))
        )
      (setf varname (second form)))
    `(SK8::define-sk8-var* ',varname :project ,*current-project* :register t :initial-value ,(skil-compile-form varval))
    ))

(add-skil-primitive-function 'declareglobal #'skil-compile-declareglobal)

;;___________________________________________________________________________________________________________________
(defun skil-compile-lib (form)
  (let ((valval (fourth form)))
    (unless (= (length form) 4)
      (error (format nil "Lib forms must have three arguments ~%~s" form)))
    (when (listp valval)
      (error (format nil "Lib must have an atom as it's fourth element ~%~s" form)))
    (if (symbolp valval)
      (setf valval (maybe-save-original-sym-case (sk8::name valval) (second form))))
    valval)
  )

(add-skil-primitive-function 'lib #'skil-compile-lib)
;;___________________________________________________________________________________________________________________

(defun skil-compile-if (form)
  (let ((res (list 'cond))
        curTest curActions
        )
    (ccl::while form
      (setf curTest (pop form))
      (cond
       ((memq curtest '(if else-if)) 
        (setf curtest (skil-compile-form (pop form))))
       ((eq curtest 'else)
        (setf curtest t))
       (t 
        (error (format nil "invalid conditional ~s, expected else-if or if" curtest))))
      (unless (or (eq curtest t) (eq (pop form) 'then))
        (error "if lacks a corresponding then"))
      (setf curactions nil)
      (ccl::while (and form (not (memq (car form) '(else else-if))))
        (setf curactions (nconc curactions (list (skil-compile-form (pop form)))))
        )
      (setf res (nconc res (list (cons curtest curActions))))
      )
    res))
      
(add-skil-primitive-function 'if  #'skil-compile-if)
;;___________________________________________________________________________________________________________________

(defun skil-compile-call-with-statement (form)
  (let ((baseWithCall (skil-compile-form (second form))))
    `(,(car baseWithCall) (,@(cdr baseWithCall)) 
      ,(skil-compile-form (third form)))
    ))

(add-skil-primitive-function 'call-with-statement  #'skil-compile-call-with-statement)

;;___________________________________________________________________________________________________________________


(defun generate-let-pairs (inputs)
  (let ((res (list 'list))
        (curlist inputs))
    (loop
      (when (or (null curlist) (eq (car curlist) '&key)) (return))
      (setf res (append res (list (list 'list (list 'quote (car curlist)) (car curlist)))))
      (setf curlist (cdr curlist))
      )
    (dolist (i (cdr curlist))
      (setf res (append res (list (list 'list (list 'quote (car i)) (car i))))))
    res)
  )

(defmacro SK8::define-project-macro (proj name private? (&rest lambda-list) &rest body)
  (unless name (error "Name arg missing!"))
  `(progn
     ,(if private?
        `(mf::mark-sym-function ',name)
        `(mf::publish-function-symbol ',name (SK8::package ,proj)))
     (defmacro ,name (,@lambda-list) ,@body)
     ',name))

(defun make-with-command-symbol (commandNameSym)
   (intern (concatenate 'string "WITH " (symbol-name commandNameSym)) (package *current-project*)))

(defun generate-with-macro (name inputs newcode cleanup-code)
  (if (eq (car newcode) 'block) (setf newcode (third newcode)))
  (let* ((cleanup-compiled (mapcar #'skil-compile-form cleanup-code))
         (loc (or (position '|@body| newcode) (- (length newcode) 2)))
         (newcode1 (subseq newcode 0 loc))
         (newcode2 (push 'progn (subseq newcode (1+ loc) (length newcode))))
         (letpairs (generate-let-pairs inputs))
         )
    `(SK8::define-project-macro ,*current-project*
       ,(make-with-command-symbol name)
       nil
       ,(append (list inputs) (list '&body 'body))
       (list 'let* ,letpairs
             (list 'unwind-protect
                   (append '(progn)
                           ,(and newcode1 `(quote ,(list newcode1)))
                           body
                           ,(and newcode2 `(quote ,(list newcode2))))
                   (append '(progn)
                           ,(and cleanup-compiled `(quote ,cleanup-compiled)))
                   )))))

;(setf *compile-with-debugging* t)

(defun sneaky-getf-setter (value place indicator)
  (unless place
    (error "Sneaky-getf-setter won't work on an empty plist."))
  (do ((sublist place (cddr sublist)))
      ((null sublist)
       (push indicator (cdr (last place)))
       (push value (cdr (last place)))
       place)
    (when (eq (car sublist) indicator) 
      (setf (cadr sublist) value)
      (return place))))

(defun handler-debugger-wrap (newcode)
  `(let* ((%%break-fn nil)
          (%%location 'before-entry)
          (fun-obj nil)
          (clos-fun nil))
     (setf fun-obj 
           ,newcode)
     (flet ((breakpoint-setter (proc-or-nil)
              (if (eq proc-or-nil :get-break-fn)
                %%break-fn
                (setq %%break-fn proc-or-nil)))
            (location-getter ()
              %%location)
            )
       (setf clos-fun (ccl::closure-function (slot-value fun-obj 'function)))
       (sneaky-getf-setter
        #'breakpoint-setter 
        (ccl::%lfun-info clos-fun)
        :breakpoint-setter)
       (sneaky-getf-setter
        #'location-getter 
        (ccl::%lfun-info clos-fun)
        :location-getter)
       fun-obj)))

(defun function-debugger-wrap (newcode)
  `(let* ((%%break-fn nil)
          (%%location 'before-entry)
          clos-obj
          fun-obj
          )
     (setf fun-obj 
           ,newcode)
     (flet ((breakpoint-setter (proc-or-nil)
              (if (eq proc-or-nil :get-break-fn)
                %%break-fn
                (setq %%break-fn proc-or-nil)))
            (location-getter ()
              %%location))
       (setf clos-obj (ccl::closure-function (symbol-function fun-obj)) )
       (sneaky-getf-setter
        #'breakpoint-setter 
        (ccl::%lfun-info clos-obj)
        :breakpoint-setter)
       (sneaky-getf-setter
        #'location-getter 
        (ccl::%lfun-info clos-obj)
        :location-getter)
       fun-obj)))

(defun skil-process-keyword-args (inputs with-macro?)
  (setf inputs (cons 'bogus inputs))
  (let ((withPosition (position 'with inputs))
        curitem)
    (dotimes (i (length inputs))  ;;;we ignore all type info in required args.
      (unless (and withPosition (>= i withPosition))
        (setf curitem (nth i inputs))
        (if (and (listp curitem)) (setf (nth i inputs) (car curItem)))))
    (when (setf withPosition (position 'with inputs))
      (setf (nthcdr withPosition inputs) 
            (cons '&key 
                  (mapcar #'(lambda (x) (if (listp x)
                                          (if (second x)
                                            (list (list (ccl::keywordify (first x)) (second x)) 
                                                  (if with-macro?
                                                    (list 'quote (skil-compile-form (third x)))
                                                    (skil-compile-form (third x))))
                                            (list (first x) 
                                                  (if with-macro?
                                                    (list 'quote (skil-compile-form (third x)))
                                                    (skil-compile-form (third x)))))
                                          x))
                          (nthcdr (1+ withPosition) inputs))))
      )
    (rest inputs)))

(defparameter *valid-skil-keys* '(input
                                     local
                                     global
                                     name
                                     Comments
                                     on-error
                                     cleanup-code
                                     return-type))

(defun skil-get-key (form key onlyAllowOne)
  (unless (memq key *valid-skil-keys*)
    (error "Invalid SKIL key: ~a" key))
  (let (keyForms res)
    (dolist (curitem form)
      (when (and (listp curitem) (eq (car curitem) key))
        (push curitem keyForms)))
    (if (and onlyAllowOne (> (length keyForms) 1))
      (error (format nil "In script only allow one key of type ~s" key)))
    (dolist (i keyForms)
      (setf res (nconc (cdr i) res)))
    res))

;;(SK8::withlockedcursor
(defun skil-compile-script (form)
  (let* ((inputs (skil-get-key form 'input nil))
         (locals (skil-get-key form 'local nil))
         (globals (skil-get-key form 'global nil))
         (name (car (skil-get-key form 'name t)))
         (comments (skil-get-key form 'Comments nil))
         (errors (skil-get-key form 'on-error nil))
         (cleanup-code (car (skil-get-key form 'cleanup-code t)))
         (body (mapcar #'skil-compile-form
                       (remove-if #'(lambda (x) 
                                      (and (listp x) (memq (car x) *valid-skil-keys*)))
                                  (cdr form))))
         localVarList
         handlerArg handlerObject
         specialVars
         currentError errorBody
         newCode )
    (declare (ignore globals comments))
    (setf locals
          (mapcar #'(lambda (x) (if (listp x) 
                                  (progn
                                    (push (car x) localVarList)
                                    (list (car x) (skil-compile-form (cadr x))))
                                  (progn
                                    (push x localVarList)
                                    x)))
                  locals))
    (when (and inputs 
               (listp (first inputs))
               (eq (first (first inputs)) 'sk8::me)
               (second (first inputs)))
      (setf handlerArg name)
      (setf handlerObject (second (first inputs)))
      (when (listp handlerObject) 
        (setf handlerObject (skil-compile-form handlerObject)))
     )
    (if locals
      (setf newcode `(let*  (,@locals) 
                       ,@(if *compile-with-debugging*  ;; and name 
                           `((declare (special ,@LocalVarList)))
                           '())
                       ,@body))
      (setf newcode `(progn ,@body)))
    (when errors 
      (setf newcode (list 'sk8::with-sk8-error-handlers newcode))
      (ccl::while errors
        (setf currentError (pop errors))
        (setf errorBody (pop errors))
        ;;This is a bit of a hack to ensure that error code with a "return" in it has will catch things that are returned.
        (if (or name inputs)
          (setf errorBody (list 'catch (list 'quote 'current-function) errorBody)))
        (setf errorBody (skil-compile-form errorBody))
        (setf newcode (append newcode (list (list currentError errorBody)))))
      )
    (setf inputs (skil-process-keyword-args inputs (and (listp name) (eq (car name) 'with))))
    (when (and handlerArg (not name))
      (error "Handler definitions need a name"))
    (when (or name inputs)
      (cond
       ((not name)
        (setf newcode `#'(lambda (,@inputs) ,newcode)))
       (handlerArg
        (setf (first inputs) handlerObject)
        (setf specialVars (mapcar #'(lambda (x) (if (listp x) (car x) x)) 
                                  (remove-if #'(lambda (x) (eq x 'with))
                                             (cdr inputs))))
        (when (listp handlerArg)
          (unless (eq (car handlerArg) 'set)
            (error (format nil "Only handle set handlers not: ~s" (car handlerArg))))
          (setf (car handlerArg) 'SETF)
          (setf inputs (cons (car (last inputs)) (subseq inputs 0 (1- (length inputs)))))
          )
        (setf newcode 
              `(SK8::define-handler ,handlerArg (,@inputs)
                 ,@(if *compile-with-debugging* 
                     `((declare (special sk8::me 
                                         ,@specialVars)))
                     '())
                 ,newcode))
        (when *compile-with-debugging*
          (setf newcode (handler-debugger-wrap newcode))
          ))
       ((and (listp name)
             (eq (car name) 'with))
        (setf newcode (generate-with-macro (second name) inputs newcode cleanup-code)))
       (t
        (setf newcode 
              `(SK8::define-project-function ,*current-project* ,name nil (,@inputs) 
                 ,@(if *compile-with-debugging* 
                     `((declare (special ,@(mapcar #'(lambda (x) (if (listp x) (car x) x)) 
                                                   (remove-if #'(lambda (x) (eq x 'with))
                                                              inputs)))))
                     '())
                 ,newcode 
                 ))
        (when *compile-with-debugging*
          (setf newcode (function-debugger-wrap newcode))
          )
        )
       ))
    newCode)
  )

(add-skil-primitive-function 'script    #'skil-compile-script)
;;___________________________________________________________________________________________________________________

#|
	Change History (most recent last):
	1  	 3/14/96	Brian   	New SKIL compiler.
	2  	 3/14/96	Brian   	Making handlers take an arbitrary expression for the
						handler object.
	3  	 3/15/96	Brian   	Adding eq and neq
	4  	 3/18/96	Brian   	Added with function macro definitions.
	6  	 3/18/96	Brian   	auto adding project argument to new calls.
	8  	 3/19/96	Brian   	As-a and adding project argument to copy.
	9  	 3/19/96	Brian   	Adding the type coercion.
	10 	 3/21/96	Brian   	handler debugging
	2  	 4/ 3/96	Hernan  	moved some skil foreign functions here
	4  	 4/15/96	Brian   	Making everything happen in the SKIL package.
	6  	 4/17/96	Brian   	making compile be non-destructive
	7  	 4/19/96	Brian   	added language definition.
						slimmed down foreign functions.
	9  	 4/29/96	Brian   	added declareGlobal and some comments.
	10 	 4/30/96	Brian   	more cleanup
	17 	 5/13/96	Brian   	fixing up define-project-macro for new object system
	18 	 5/13/96	Brian   	debugging stuff.
	20 	 5/20/96	Brian   	Whoops, missed a parenthesis
	23 	 7/ 7/96	sidney  	changes for native PPC build
	24 	 7/15/96	Brian   	
	25 	 7/17/96	Brian   	fixing renaming to ensure only locals get renamed.
	26 	 7/18/96	Brian   	Oops, another fix to alpha renaming.
	27 	 8/ 1/96	Hernan  	skil-compile-declareglobal has to call define-sk8-var* (the
						function), in order to register the variable in the Store.
	28 	 8/ 5/96	Brian   	
	29 	 8/ 5/96	Brian   	adding returning.
	30 	 8/ 8/96	Brian   	changing global declare.
	31 	 8/ 8/96	Brian   	
	32 	 8/13/96	Brian   	Fixing alpha-rename to handle input variables properly.
	33 	 8/28/96	Brian   	adding comments
	34 	10/10/96	Brian   	Adding error handling.
	35 	10/17/96	sidney  	remove debugging stub of with-sk8-error-handlers macro in order to check in real version elsewhere
	36 	10/17/96	Hernan  	making it actually compile the error code.
	37 	10/18/96	Brian   	fixing spelling mistake
	38 	10/18/96	Brian   	suppressing compiler warnings for skil-compile-to-lisp.
	39 	11/ 7/96	Brian Roddy	Fixing compilation of handlers with expressions
						for argument types.
	40 	11/12/96	Brian   	Added location-getter function to handler and function debugger wrap.
	41 	12/13/96	Brian   	Fixing declaration of special vars in set handlers
						to do the right thing.  that is calculate specials
						before flipping them for handler definition.
	42 	12/13/96	Brian   	fixing declaring special of functions to include
						first argument as there is no reason not to.
	43 	12/17/96	Brian Roddy	
	44 	12/17/96	Brian Roddy	Wrapping eval in ignore-errors to be sure.
	45 	12/17/96	Brian Roddy	
	46 	12/17/96	Brian Roddy	
	47 	12/19/96	Brian Roddy	don't want to alpha-rename symbols (which
						could happen if they had the same name as
						a local variable.)
	48 	 1/13/97	Brian Roddy	In skil-run, making *save-definitions* be true so
						it will save the argument lists.  This way when
						getting "help" the proper argument names appear.
	49 	 1/14/97	Brian Roddy	Putting back save-definitions.  It breaks other things.
	50 	 1/27/97	Brian Roddy	
	51 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
