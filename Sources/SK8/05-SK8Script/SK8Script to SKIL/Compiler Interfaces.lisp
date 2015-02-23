;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated 10-22-96   2:45 pm
                  SK8::CONVERTTOJAVA SK8::INDENTJAVADIRECTORY SK8::SK8SCRIPTRESERVEDWORD)


;;; _______________________________ 
;;; Compiler
;;; _______________________________ 

;;; The error struct. One of these is returned for each error/warning that
;;; the compiler catches. 

(defstruct scriptError
  lineNumber      ; the SK8Script line number where the problem occured
  posNumber       ; the char position number in the line.
  errorMessage    ; string explaining the problem.
  )

;;; SK8 interfaces to the error struct.

(define-sk8-function lineOfError nil (errorObj)
  (scriptError-lineNumber errorObj))
(define-sk8-function characterPositionOfError nil (errorObj)
  (scriptError-posNumber errorObj))
(define-sk8-function descriptionOfError nil (errorObj)
  (scriptError-errorMessage errorObj))
;;__________________________________________________________________________________

(define-sk8-function sk8ScriptReservedWord nil (str)
  (let ((firstWord (SINGLEITEMMAPPER str 1 WORD nil nil nil nil nil))
        (*package* (find-package :skil)))
    (setf firstWord (read-from-string firstWord nil nil))
    (if (memq firstWord skil::*SK8Script-Tokens*)
      t
      nil)))


;;__________________________________________________________________________________
(defun setupParsingProjectVariables (proj &optional curHandlerObject)
  (unless (is-a proj sk8::project) (setf proj sk8))
  (setf skil::*current-project* proj)
  (setf skil::cur-handler-object curHandlerObject)
  )
(defun get-handler-locals ()
  (skil::get-handler-locals))
(defun get-handler-globals ()
  (skil::get-handler-globals))



;;; Evaluates the handler. The handlerObject is only required when defining a handler
;;; on an unnamed object. 

;;; Returns multiple values of the following form:
;;; 
;;; failure?           nil if compilation succeeded. t if syntax errors
;;; errors             a list of structs (nil if no errors/warnings). 
;;;                    Each struct has the following info:
;;;                    lineNumber: the SK8Script line number where the problem occured
;;;                    posNumber: the char position number in the line.
;;;                    errorMessage: string explaining the problem.
;;; locals             List of the locals in the handler.
;;; globals            List of the globals in the handler. 

(define-sk8-function compileScriptHandler nil (targetProject script &optional handlerObject)
  (unless (is-a targetProject sk8::project) (setf targetProject sk8))
  (let (res fail? errs)
    (setupParsingProjectVariables targetProject handlerObject)
    (setf res (skil::SK8Script-Compile-and-Eval script nil))
    (setf errs  skil::current-warning-list)
    (when (and (listp res) (eq (car res) 'error))
      (setf fail? t)
      (setf errs (list (make-scripterror :linenumber (second res)
                                         :posnumber (third res)
                                         :errorMessage (fourth res)))))
    (sk8-multivals fail? errs (get-handler-locals) (get-handler-globals))))


;;; Translates the expression or command. In addition to the values returned by the function
;;; above, the resulting translated form is returned.
(define-sk8-var MaintainTypeTables :initial-value nil)
(define-sk8-function clearTypeTables nil ()
  (skil::clear-type-information))

(define-sk8-function translateScriptCommandOrExpr nil (targetProject script)
  (unless (is-a targetProject sk8::project) (setf targetProject sk8))
  (let (res fail? errs)
    (setupParsingProjectVariables targetProject)
    (setf res (skil::SK8Script-Compile script nil))
    (setf errs  skil::current-warning-list)
    (if (and (listp res) (eq (car res) 'error))
      (progn
        (setf fail? t)
        (setf errs (list (make-scripterror :linenumber (second res)
                                           :posnumber (third res)
                                           :errorMessage (fourth res)))))
      (progn
        (when MaintainTypeTables (sk8::updateTypeTablesFromForm res))
        (setf res (list targetproject res))))
    (sk8-multivals fail? errs (get-handler-locals) (get-handler-globals) res)))

;;; Returns the result of the evaluation. The project should be implied in the form.

(define-sk8-function evaluateScriptTranslation nil (form)
  (let (res)
    (if (eq (car form) 'ERROR) 
      (error "Can't evaluate a translation with an error."))
    (setupParsingProjectVariables (car form))
    (setf res (skil::skil-eval (cadr form)))
    res))

(define-sk8-function evaluateScriptCommand nil (targetProject script)
  (unless (is-a targetProject sk8::project) (setf targetProject sk8))
  (evaluatescripttranslation 
   (fifth (sk8::translateScriptCommandOrExpr targetproject script))))
(define-sk8-function translateScriptCommand nil (targetProject script)
  (unless (is-a targetProject sk8::project) (setf targetProject sk8))
  (sk8::translateScriptCommandOrExpr targetproject script))
(define-sk8-function translateScriptExpression nil (targetProject script)
  (unless (is-a targetProject sk8::project) (setf targetProject sk8))
  (sk8::translateScriptCommandOrExpr targetproject script))

;;;__________________________________________
;;; for loading from files:::

(define-sk8-function evaluateScriptForms nil (targetProject script)
  (unless (is-a targetProject sk8::project) (setf targetProject sk8))
  (let (res fail? errs)
    (setupParsingProjectVariables targetProject)
    (setf res (skil::SK8Script-Compile-and-Eval script t))
    (setf errs  skil::current-warning-list)
    (when (and (listp res) (eq (car res) 'error))
      (setf fail? t)
      (setf errs (list (make-scripterror :linenumber (second res)
                                         :posnumber (third res)
                                         :errorMessage (fourth res)))))
    (sk8-multivals fail? errs nil nil (unless fail? res))))


(defun process-potential-file (myfile)
  (cond ((stringp myfile) (sk8-filename-to-mcl-pathname myfile))
        ((pathnamep myfile) myfile)
        ((is-a myfile file) (ospathname myfile))
        (t (error "Invalid scriptFile argument"))))

(defun loadAndEvalScriptFile (myfile proj)
  (unless (is-a proj sk8::project) (setf proj sk8))
  (setf myfile (process-potential-file myfile))
  ;;(withlockedcursor watchcursor
    (let ((str ""))
      (with-open-file (instream myfile)
        (loop 
          (multiple-value-bind (newstr end-p) (read-line instream nil nil)
            (setf str (concatenate 'string str (string #\newline) newstr))
            (when (or end-p (not newstr)) (return))
            ))
        )
      (sk8::evaluateScriptForms proj str)
      )
    ;;)
  )

(define-sk8-function indentJavaDirectory nil (outDirectory)
  (indent-java-directory outDirectory))

(define-sk8-function convertToJava nil (inFiles outDirectory)
  (skil::convert-to-java infiles outDirectory))


(define-sk8-function translateScriptFile nil (inFile outfile proj &optional toType)
  (unless (is-a proj sk8::project) (setf proj sk8))
  (setf inFile (process-potential-file inFile))
  (setf outfile (process-potential-file outfile))
    (let ((str ""))
      (with-open-file (instream inFile)
        (loop 
          (multiple-value-bind (newstr end-p) (read-line instream nil nil)
            (setf str (concatenate 'string str (string #\newline) newstr))
            (when (or end-p (not newstr)) (return))
            ))
        )
      (with-open-file (outStream outfile :direction :output :if-does-not-exist :create :if-exists :supersede) 
        (let (res (*package* (find-package :skil)))
          (setupParsingProjectVariables proj)
          (setf res (skil::SK8Script-Compile str t))
          (cond
           ((or (null toType) (eq toType 'sk8::skil))
            (dolist (i (cdr res))
              (when i (pprint i outStream)))
            )
           (t
            (dolist (i (cdr res))
              (when i (pprint (skil::skil-compile-to-lisp i) outStream)))))))
      )
    )
;;(translateScriptFile  "sk8;Song Book Builder.sk8" "sk8;Song Book.lisp" sk8::foo t)
;;(translateScriptFile  "sk8;test.sk8" "sk8;test.sx" sk8::sk8 'sk8::scriptx)
;;(translateScriptFile  "sk8;simkit:simkit.sk8" "sk8;simkit:simkit.skil" sk8::simkit)
;;(translateScriptFile  "sk8;simkit:gravitas.sk8" "sk8;simkit:gravitas.skil" sk8::simkit)

;;(fred "ccl;simkit:simkit.skil")
;;(fred "ccl;simkit:gravitas.skil")


#|
	Change History (most recent last):
	1  	 3/12/96	Brian   	
	2  	 3/12/96	Brian   	removing old imports into the bogus modules
	6  	 3/19/96	Brian   	Added translate to skil file function
	7  	 3/21/96	Brian   	Debugging stuff.
	8  	 3/22/96	Brian   	translate to lisp
	3  	 4/15/96	Brian   	Making everything happen in the SKIL package.
	4  	 4/26/96	Brian   	fixing evaluateScriptForms with packages and 
						returning errors
	12 	 5/ 7/96	sidney  	changes to accomodate some package changes
	14 	 5/ 7/96	Hernan  	Adding handlerObjectToHandlerId.
	15 	 5/ 9/96	Brian   	fixing scriptx header file info
	17 	 5/13/96	Brian   	fixed HandlerIDfromScript
	18 	 5/13/96	Brian   	debugging stuff.
	19 	 5/14/96	Brian   	Cleaning up debugger stuff.  Take 2.
	21 	 5/14/96	Brian   	More debugger work.
	22 	 5/20/96	Brian   	
	23 	 7/15/96	Brian   	added stuff for alpha renaming.
	24 	 8/ 5/96	Brian   	
	25 	 8/13/96	Brian   	
	26 	 8/13/96	Brian   	
	27 	 9/16/96	Brian   	
	28 	 9/23/96	Brian   	
	29 	10/10/96	Brian   	
	30 	10/11/96	Brian   	Adding warnings.
	31 	10/18/96	Brian   	
	32 	10/22/96	Hernan  	
	33 	10/22/96	Hernan  	Adding indentJavaDirectory.
	34 	11/25/96	Brian   	Making null projects become sk8 in all functions.
	35 	12/19/96	Brian Roddy	Passing on through warnings...
	36 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
