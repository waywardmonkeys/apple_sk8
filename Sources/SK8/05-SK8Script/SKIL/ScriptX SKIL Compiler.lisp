;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :skil)

;;___________________________________________________________________________________________________________________

(defparameter sk8return (make-symbol (format nil "~%")))

;; object and handler mangling
(defun sk8ObjectMangle (var)
  (let ((printname (symbol-name var)))
    (make-symbol (concatenate 'string "sk8" printname))))
(defun sk8HandlerMangle (var)
  (let ((printname (symbol-name var)))
    (make-symbol (concatenate 'string printname "Handler"))))
(defun sk8setHandlerMangle (var)
  (let ((printname (symbol-name var)))
    (make-symbol (concatenate 'string "set_" printname))))
(defun sk8GetterMangle (var)
  (let ((printname (symbol-name var)))
    (make-symbol (concatenate 'string printname "Getter"))))
(defun sk8SetterMangle (var)
  (let ((printname (symbol-name var)))
    (make-symbol (concatenate 'string printname "Setter"))))

;;___________________________________________________________________________________________________________________
(defparameter sx-skil-primitive-functions (make-hash-table :test #'eq))

(defun sx-add-skil-primitive-function (funsym scriptXFun)
  (setf (gethash funsym sx-skil-primitive-functions) scriptXFun)
  t)

(defun sx-get-primitive-function (funsym)
  (gethash funsym sx-skil-primitive-functions))

;;___________________________________________________________________________________________________________________
(defparameter sx-skil-foreign-functions (make-hash-table :test #'eq))

(defun sx-add-skil-foreign-function (funsym scriptXFun)
  ;(unless (fboundp scriptXFun) (error (format nil "can't add function ~s cause it's not bound." scriptXFun)))
  (setf (gethash funsym sx-skil-foreign-functions) scriptXFun)
  t)

(defun sx-get-foreign-function (funsym)
  (gethash funsym sx-skil-foreign-functions))


;;___________________________________________________________________________________________________________________

;(add-skil-foreign-function 'debugging-setup nil)




;;basic functionality we keep the same
(sx-add-skil-foreign-function 'return 'return)

;;basic operators
(sx-add-skil-foreign-function '^ 'sk8expt)
(sx-add-skil-foreign-function '* 'mul)
(sx-add-skil-foreign-function '/ 'sk8primdiv)
(sx-add-skil-foreign-function 'div 'sk8primintdiv)
(sx-add-skil-foreign-function 'mod 'sk8primmodulo)
(sx-add-skil-foreign-function 'not 'not)
(sx-add-skil-foreign-function 'xor 'xor) ;;check me
(sx-add-skil-foreign-function '< 'sk8primlt)
(sx-add-skil-foreign-function '> 'sk8primgt)
(sx-add-skil-foreign-function '<= 'sk8primle)
(sx-add-skil-foreign-function '>= 'sk8primge)
(sx-add-skil-foreign-function '= 'sk8primeq)
(sx-add-skil-foreign-function 'eq 'eq)
(sx-add-skil-foreign-function 'call-do-inherited 'next-method)  ;;;wrong!!!!!***
(sx-add-skil-foreign-function 'apply 'apply)
(sx-add-skil-foreign-function 'is-a 'sk8primisa)
(sx-add-skil-foreign-function 'contains 'member)
(sx-add-skil-foreign-function 'as-a 'sk8primcoerce)
(sx-add-skil-foreign-function '& 'sk8primconcat)

(sx-add-skil-foreign-function 'startsWith 'sk8startswith)
(sx-add-skil-foreign-function 'endswith 'sk8endswith)

;;collection stuff....
(sx-add-skil-foreign-function 'singleItemMapper 'singleItemMapperHandler)
(sx-add-skil-foreign-function 'fullRangeMapper 'fullRangeMapperHandler)
(sx-add-skil-foreign-function 'insertInFront 'insertInFrontHandler)
(sx-add-skil-foreign-function 'insertAtEnd 'insertAtEndHandler)
(sx-add-skil-foreign-function 'anyindex 'anyindexHandler)
(sx-add-skil-foreign-function 'middleIndex 'middleIndexHandler)


;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-form (form)
  (if (and form (listp form))
    (cond
     ;;; lib forms are function calls.
     ((and (listp (car form)) (eq (caar form) 'lib))
      (if (memq (fourth (car form)) '(sk8::new))
        (sx-skil-compile-new (cons (sx-skil-compile-form (car form)) (cdr form)))
        (sx-skil-compile-function (cons (sx-skil-compile-form (car form)) (cdr form)))))
     ;;; check if first item is a primitive compiler form (e.g. script)
     ((sx-get-primitive-function (car form))
      (funcall (sx-get-primitive-function (car form)) form))
     ;;; check if first item is a foreign function (e.g. +)
     ((sx-get-foreign-function (car form))
      (sx-skil-compile-function (cons (sx-get-foreign-function (car form)) (cdr form))))
     ;;; otherwise pass it on through.  this is really an error that needs to be fixed.
     (t
      (format t "Unknown SKIL type: ~s~%" (car form))
      (inspect (car form))
      (sx-skil-compile-function form))
     )
    (sx-compile-atom form)))

;;____________________________________________________________________________________________________________

(defun sx-compile-atom (x)
  (cond
   ((eq x 'sk8::me) 'self)
   ((eq x t) 'true)
   (t
    x)))
;;___________________________________________________________________________________________________________________

(defun sx-compile-function-name (res)
  (if (and (fboundp res)
           (CCL::standard-generic-function-p (symbol-function res)))
    (setf res (sk8handlermangle res))
    (setf res (sk8handlermangle res)))
  res)


(defparameter scriptx-reserved-words '(before after))
;;note that we compile it to (function args)
;;the outside parens are for functions nested in the arguments of other functions
(defun sx-skil-compile-function (form)
  (let ((funName (pop form))
        (res nil)
        curkey
        curitem)
    (ccl::while (and form (neq (setf curitem (pop form)) 'with))
      (setf res (append res (list (sx-skil-compile-form curitem)))))
    (ccl::while form
      (setf curkey (pop form))
      (unless (symbolp curkey) (error "keywords must be symbols"))
      (if (memq curkey scriptx-reserved-words) (setf curkey (sk8objectmangle curkey)))
      (setf res (append res (list curkey '|:| (sx-skil-compile-form (pop form))))))
    `(|(| ,funname ,@(if res res '(|(| |)|)) |)|)))
;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-new (form)
  (setf form (deep-copy-list form))
  (let* ((objNameLoc (position 'sk8::objectName form))
         (objNameString (and objNameLoc (nth (1+ (position 'sk8::objectName form)) form)))
         (curObjSym (and objNameString (let ((*package* (package *current-project*))) (read-from-string objNameString nil nil))))
         (curObj (if curObjSym (if (boundp curObjSym) (symbol-value curObjSym) (error "object must be loaded")) nil))
         (curObjTrans (and curObjSym (sk8ObjectMangle curObjSym)))
         (objProps (and curObj (sk8::localProperties curObj)))
         (propLoc (position 'sk8::properties form))
         formProps compiledForm newproplist)
    (if propLoc
      (progn
        (setf formProps (nth (1+ propLoc) form))
        (print "UHOH!")
        (print formprops)
        (setf (nth (1+ propLoc) form) nil)
        )
      (when objProps
        (setf form (nconc form (list 'sk8::properties nil)))
        (setf propLoc (position 'sk8::properties form))))
    (setf compiledForm (sx-skil-compile-function form))
    (setf propLoc (position 'sk8::properties compiledForm))
    (when (and propLoc objProps)
      (setf newproplist (list (skil::sx-skil-compile-quote (list nil (car objProps)))))
      (dolist (x (cdr objProps))
        (setf newproplist (nconc newproplist (list '|,| (skil::sx-skil-compile-quote (list nil x))))))
      (setf (nth (+ propLoc 2) compiledForm) `(|#||(| ,@newproplist |)|)))
    (append
     compiledForm
     (list sk8return)
     (mapcar #'(lambda (x) (list
                            `(method ,(sk8HandlerMangle x) self { class |(| getClassSpecial ,curObjTrans |)| } -> |(| ,(sk8GetterMangle x) self |)| ,sk8return)
                            `(method ,(sk8HandlerMangle (sk8SetHandlerMangle x)) self { class |(| getClassSpecial ,curObjTrans |)| } newval -> |(| ,(sk8SetterMangle x) self newval |)| ,sk8return)))
             objProps)
     )))

;;___________________________________________________________________________________________________________________

;;(sx-skil-compile-items 'val)
(defun sx-skil-compile-items (forms)
  (unless (listp forms) (setf forms (list forms)))
  (mapcar #'(lambda (x) 
              (let (curres)
                (setf curres (sx-skil-compile-form x))
                (if (and (listp x) (eq (car x) 'script))
                  curres
                  (append (if (listp curres) curres (list curres)) (list sk8return) )
                  ))) forms))

(defun sx-skil-compile-loop (form)
  `(repeat while true do |(| ,sk8return
           ,(sx-skil-compile-items (cdr form)) |)|))

(sx-add-skil-primitive-function 'loop #'sx-skil-compile-loop)
;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-error (form)
  `(report exception ,(sx-skil-compile-form (second form))))

(sx-add-skil-primitive-function 'error #'sx-skil-compile-error)
;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-quote (form)
  (if (consp (second form))
    `(\#\( ,@(second form) \))
    (make-symbol 
     (concatenate 'string
                  "@"
                  (string-upcase (symbol-name (second form)))))))

(sx-add-skil-primitive-function 'quote #'sx-skil-compile-quote)
;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-funcall (form)
  (if (eq (second form) 'sscurrentpathexpressioncollectionset)
    (sx-skil-compile-form (third form))
    (progn
      (setf (first form) 'apply)
      (let* ((res (copy-list (sx-skil-compile-function form)))
             (len (- (length res) 2)))
        (setf (nth len res) `(|#||(| ,(nth len res) |)|))
        res)))
  )

(sx-add-skil-primitive-function 'funcall #'sx-skil-compile-funcall)
;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-andor (form)
  `(|(| ,(sx-skil-compile-form (second form)) ,(first form) ,(sx-skil-compile-form (third form)) |)|))

(sx-add-skil-primitive-function 'and #'sx-skil-compile-andor)
(sx-add-skil-primitive-function 'or #'sx-skil-compile-andor)

;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-Minus (form)
  (if (third form)
    (sx-skil-compile-function (cons 'sub (cdr form)))
    (sx-skil-compile-function (cons 'sub (cons 0 (cdr form))))))
(sx-add-skil-primitive-function '- #'sx-skil-compile-Minus)

(defun sx-skil-compile-Plus (form)
  (if (third form)
    (sx-skil-compile-function (cons 'sum (cdr form)))
    (skil-compile-form (second form))))
(sx-add-skil-primitive-function '+ #'sx-skil-compile-Plus)

;;___________________________________________________________________________________________________________________
(defun sx-skil-compile-set-to (form)
  (let ((varval (second form))
        (toval (third form))
        (valval (fourth form)))
    (unless (or (= (length form) 4) (eq toval 'to))
      (error (format nil "Set forms must be of the form set var to val ~%~s" form)))
    (if (and (listp varVal) (neq (car varval) 'lib))
      `(,(sk8setHandlerMangle (sx-skil-compile-form (car varval)))
        ,(sx-skil-compile-form (second varval))   ;;;*** Ignoring with args...
        ,(sx-skil-compile-form valval))
      `(,(sx-skil-compile-form varval) |:=| ,(sx-skil-compile-form valval) ))
    ))


(sx-add-skil-primitive-function 'set #'sx-skil-compile-set-to)
;;___________________________________________________________________________________________________________________
(defun deep-member (item loc)
  (if (listp loc)
    (some #'(lambda (x) (deep-member item x)) loc)
    (eq item loc)))


(defun sx-skil-compile-catch (form)
  (let (catchObject
        (otherForms (sx-skil-compile-items (cddr form))))
    (case (second (second form)) 
      (NEXT-REPEAT (setf catchObject (and (deep-member 'nextRepeatObject otherForms) 'nextRepeatObject)))
      (EXIT-REPEAT (setf catchObject nil))
      (CURRENT-FUNCTION (setf catchObject nil)))
    (if CatchObject
      `(guard |(| ,sk8return ,@otherForms |)| ,sk8return catching ,sk8return ,catchObject returnArg |:| returnArg ,sk8return end)
      otherForms))
  )

(sx-add-skil-primitive-function 'catch #'sx-skil-compile-catch)

;;___________________________________________________________________________________________________________________
(defun sx-skil-compile-throw (form)
  (case (second (second form)) 
    (NEXT-REPEAT `(report nextRepeatObject true))
    (EXIT-REPEAT `(exit with true))
    (CURRENT-FUNCTION `(return ,(sx-skil-compile-form (third form)))))
  )

(sx-add-skil-primitive-function 'throw #'sx-skil-compile-throw)

;;___________________________________________________________________________________________________________________
(defun sx-skil-compile-declareglobal (form)
  `(global ,(second form))
  )

(sx-add-skil-primitive-function 'declareglobal #'sx-skil-compile-declareglobal)

;;___________________________________________________________________________________________________________________
(defun sx-skil-compile-lib (form)
  (let ((valval (fourth form)))
    (unless (= (length form) 4)
      (error (format nil "Lib forms must have three arguments ~%~s" form)))
    (when (listp valval)
      (error (format nil "Lib must have an atom as it's fourth element ~%~s" form)))
    (if (symbolp valval)
      (setf valval (intern-symbol (symbol-name valval) (second form))))
    (if (eq (third form) 'object)
      (sk8ObjectMangle valval)
      (sx-compile-function-name valval))
    )
  )

(sx-add-skil-primitive-function 'lib #'sx-skil-compile-lib)
;;___________________________________________________________________________________________________________________
;;**************FIX ME
(defun sx-skil-compile-call-with-statement (form)
  (sx-skil-compile-form (third form)) 
  )

(sx-add-skil-primitive-function 'call-with-statement        #'sx-skil-compile-call-with-statement)
;;___________________________________________________________________________________________________________________
;;**************FIX ME
(defun sx-skil-compile-call-do-inherited (form)
  (declare (ignore form))
  '(nextMethod self)
  )

(sx-add-skil-primitive-function 'call-do-inherited        #'sx-skil-compile-call-do-inherited)

;;___________________________________________________________________________________________________________________

(defun sx-skil-compile-if (form)
  (let (curTest curActions)
    (setf curTest (pop form))
    (cond
     ((memq curtest '(if else-if)) 
      (setf curtest (sx-skil-compile-form (pop form))))
     ((eq curtest 'else)
      (setf curtest t))
     (t 
      (error (format nil "invalid conditional ~s, expected else-if or if" curtest))))
    (unless (or (eq curtest t) (eq (pop form) 'then))
      (error "if lacks a corresponding then"))
    (setf curactions nil)
    (ccl::while (and form (not (memq (car form) '(else else-if))))
      (setf curactions (append curactions (list (pop form))))
      )
    (setf curActions (sx-skil-compile-items curActions))
    (if form
      `(if ,curTest then |(| ,sk8return ,curactions |)| ,sk8return else |(| ,sk8return ,(sx-skil-compile-if form) |)|)
      `(if ,curTest do |(| ,sk8return ,curactions |)|)
      )))

(sx-add-skil-primitive-function 'if        #'sx-skil-compile-if)
;;___________________________________________________________________________________________________________________


(defun sx-skil-process-keyword-args (inputs with-macro?)
  (setf inputs (deep-copy-list inputs))
  (let ((withPosition (position 'with inputs))
        curitem)
    (dotimes (i (length inputs))  ;;;we ignore all type info in required args.
      (unless (and withPosition (>= i withPosition))
        (setf curitem (nth i inputs))
        (if (listp curitem) (setf (nth i inputs) (car curItem)))))
    (when (setf withPosition (position 'with inputs))
      (setf (nthcdr withPosition inputs) 
            (cons '&key 
                  (mapcar #'(lambda (x) (if (listp x)
                                          (if (second x)
                                            (list (list (ccl::keywordify (first x)) (second x)) 
                                                  (if with-macro?
                                                    (list 'quote (sx-skil-compile-form (third x)))
                                                    (sx-skil-compile-form (third x))))
                                            (list (first x) 
                                                  (if with-macro?
                                                    (list 'quote (sx-skil-compile-form (third x)))
                                                    (sx-skil-compile-form (third x)))))
                                          x))
                          (nthcdr (1+ withPosition) inputs))))
      )
    inputs))

(defun sx-skil-compile-script (form)
  (let* ((inputs (skil-get-key form 'input nil))
         (locals (skil-get-key form 'local nil))
         (globals (skil-get-key form 'global nil))
         (name (car (skil-get-key form 'name t)))
         (errors (skil-get-key form 'on-error nil))
         (cleanup-code (car (skil-get-key form 'cleanup-code t)))
         (body (mapcar #'(lambda (x) 
                           (let (curform)
                             (setf curform (sx-skil-compile-form x))
                             (if (listp curform)
                               (append curform (list sk8return))
                               (cons curform (list sk8return)))))
                       (append (delete-if #'null (mapcar #'(lambda (x) (if (listp x) 
                                                                         (if (and (second x) (neq (car x) 'sscurrentpathexpressioncollectionset)) 
                                                                           `(set ,(car x) to ,(second x))
                                                                           `(set ,(car x) to nil))
                                                                         `(set ,x to nil)))
                                                         locals))
                               (remove-if #'(lambda (x) 
                                              (and (listp x) (memq (car x) *valid-skil-keys*)))
                                          (cdr form)))))
         handlerArg handlerObject
         newCode )
    (declare (ignore globals errors cleanup-code))
    (setf locals
          (mapcar #'(lambda (x) (if (listp x) `(local ,(car x) ,sk8return) `(local ,x ,sk8return)))
                  locals))
    (if locals
      (setf newcode `(|(| ,@locals ,@body |)|))
      (setf newcode body))
    (when (and inputs (listp (car inputs)) (second (first inputs)))
      (setf handlerArg name)
      (setf handlerObject (cadar inputs))
      (when (listp handlerObject) (setf handlerObject (sx-skil-compile-form handlerObject)))
      (setf inputs (cons handlerObject (cdr inputs))))
    (setf inputs (sx-skil-process-keyword-args inputs (and (listp name) (eq (car name) 'with))))
    (when (and handlerArg (not name))
      (error "Handler definitions need a name"))
    (when (or name inputs)
      (cond
       ((not name)
        (setf newcode `(|(| ,@inputs -> |(| ,sk8return ,newcode |)| |)|)))
       (handlerArg
        (if (listp handlerArg)
          (progn
            (unless (eq (car handlerArg) 'set)
              (error (format nil "Only handle set handlers not: ~s" (car handlerArg))))
            (setf handlerArg (sk8setHandlerMangle (second handlerArg)))
            )
          (setf handlerArg (sk8HandlerMangle handlerArg)))
        (setf newcode 
              `(method ,handlerArg self { class |(| getClassSpecial ,(car inputs) |)| } 
                       ,@(cdr inputs)  -> |(| ,sk8return  ,newcode |)| ,sk8return)
              )
        )
       ((and (listp name)
             (eq (car name) 'with))
        (error "CAN'T CONVERT WITH FUNCTIONS YET!!!!"))
       (t
        (setf newcode 
              `(function ,(sk8HandlerMangle name) ,@inputs -> |(| ,sk8return ,newcode |)| ,sk8return)))
       ))
    newCode)
  )

(sx-add-skil-primitive-function 'script    #'sx-skil-compile-script)



;;___________________________________________________________________________________________________________________


(defun stringify-form (form)
  (cond 
   ((not form)
    "false")
   ((listp form)
    (mapcar #'stringify-form form)
    )
   ((eq form ':=)
    ":=")
   ((eq form t)
    "true")
   ((stringp form)
    (format nil "~s" form))
   (t
    (format nil "~a" form)
    )))

(defun do-print-form (form)
  (unless (listp form) (setf form (list form)))
  (let ((str "") 
        curRes
        i
        (curitem form)
        )
    (dotimes (curNum (length form))
      (setf i (car curitem))
      (setf curRes (if (listp i) (do-print-form i) i))
      (setf str (concatenate 'string str curRes))
      (setf curitem (cdr curitem))
      (unless (or (and (stringp i) (string= i "(")) 
                  (and (stringp (car curitem)) (string= (car curitem) "
"))
                  (not curitem)
                  (and (stringp (car curitem)) (string= (car curitem) ")")))
        (setf str (concatenate 'string str " ")))
      )
    str
    )
  )

(defun print-form (form)
  (let ((str (do-print-form (stringify-form form))))
    (unless (string= (subseq str (1- (length str)) (length str)) (format nil "~%")) 
      (setf str (format nil "~a~%" str)))
    str))

(defun sx-it (str)
  (print-form
   (sx-skil-compile-form 
    (second (fifth (sk8::translatescriptcommand sk8::sk8  str ))))))


#|(untrace)

(sk8::translatescriptcommand sk8::sk8 "global glozz = 1")
(skil-compile-form (second (fifth (sk8::translatescriptcommand sk8::sk8 "global glozz = 1"))))
(sx-it "global glozz = \"haha\"")

(sx-it
"on jiz of x, y
   if x = y then
      return 1
   else if x > y then
      return 2
   else return 3
end jiz
" )
(sx-it
"set fillcolor of rectangle to blue" )
(sx-it
"on collectionLength of me (a collection),  typeFilter, whereFilter
     local cnt = 0
     return cnt
end collectionLength
" )




|#

#|
	Change History (most recent last):
	1  	 4/29/96	Brian   New file.	
	2  	 4/29/96	Brian   	pass two, everything but path expressions,
						inheritance and with handlers.
	3  	 4/29/96	Brian   	
	4  	 4/30/96	Brian   	more cleanup
	5  	 4/30/96	Brian   	
	6  	 5/ 3/96	Brian   	
	7  	 5/ 6/96	Brian   	
	8  	 5/ 9/96	Brian   	
	9  	 5/ 9/96	Brian   	
	10 	 5/13/96	Brian   	
	11 	 5/21/96	Brian   	
	12 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
