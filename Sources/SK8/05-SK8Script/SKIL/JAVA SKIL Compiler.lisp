;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :skil)


;;___________________________________________________________________________________________________________________

;; object and handler mangling
(defun sk8ObjectInJava (var)
  (if (symbolp var) (setf var (symbol-name var)))
  (setf var (string-downcase var))
  (cond 
   ((string= var "object") (setf var "Object"))
   ((string= var "undefined") (setf var "null"))
   ((and (string= var "false")
         current-java-form-type-for-set
         (neq current-java-form-type-for-set 'sk8::boolean))
    (setf var "null"))
   )
  var)
(defun sk8HandlerInJava (var)
  (if (symbolp var) (setf var (symbol-name var)))
  (string-downcase var))
(defun sk8setHandlerInJava (var)
  (if (symbolp var) (setf var (symbol-name var)))
  (string-downcase (concatenate 'string "set" var)))
(defun currentProjectInJava ()
  (sk8objectinjava (sk8::sk8_id (currentProjectSymbolForJava))))
(defun currentProjectSymbolForJava ()
  *current-project*)

;;(get-function-return-type '<)

(defun lib-form-p (form)
  (and (listp form) (= (length form) 4) (eq (first form) 'lib)))

;;___________________________________________________________________________________________________________________

(defparameter java-in-set-parameter nil)
(defparameter current-java-form-type-for-set nil)

;;___________________________________________________________________________________________________________________
;;___________________________________________________________________________________________________________________
;;___________________________________________________________________________________________________________________


(defparameter java-skil-object-table nil)
(defparameter java-skil-property-table nil)
(defparameter java-skil-tag-table nil)
(defparameter java-skil-handler-table nil)
(defparameter java-skil-function-list nil)
(defparameter java-skil-global-list nil)
(defparameter java-skil-setup-list nil)
(defparameter java-skil-new-objects nil)
(defparameter java-skil-current-function nil)
(defparameter java-skil-current-arguments nil)
(defparameter java-skil-current-function-locals nil)
(defparameter java-skil-setup-table nil)
(defparameter java-skil-contents-table nil)

(defun setup-java-skil-globals ()
  (setf generate-list-of-globals-p t)
  (setf generate-readable-code t)
  (setf current-special-global-list nil)
  (setf java-skil-object-table (make-hash-table :test #'eq))
  (setf java-skil-property-table (make-hash-table :test #'eq))
  (setf java-skil-tag-table (make-hash-table :test #'eq))
  (setf java-skil-handler-table (make-hash-table :test #'eq))
  (setf java-skil-function-list nil)
  (setf java-skil-global-list nil)
  (setf java-skil-new-objects nil)
  (setf java-skil-setup-list nil)
  (setf java-skil-current-function nil)
  (setf java-skil-current-arguments nil)
  )

(defun valid-object-to-make-a-java-class-for (sym)
  (or (member (sk8objectinjava sym) java-skil-new-objects :test #'string=)
      (gethash sym java-skil-property-table)
      (gethash sym java-skil-tag-table)
      (gethash sym java-skil-handler-table)))

(defun generate-java-property-var (prop)
  (concatenate 'string "f" (string-capitalize (sk8objectinjava prop)) ))


(defun transform-sets-for-initialize (form)
  (let (curform)
    (setf curform (first form))
    (setf (second (second curform)) 'sk8::me)
    (java-skil-compile-form curform)))
          

(defun find-and-remove-appropriate-setup (obj)
  (let (curform curObj formlist curprop curval curvalval)
    (dolist (i (copy-list java-skil-setup-list))
      (setf curform (first i))
      (when (and (listp curform) 
                 (eq (first curform) 'set)
                 (listp (second curform))
                 (second (second curform)))
        (setf curObj (second (second curform)))
        (if (lib-form-p curObj) (setf curObj (fourth curObj)))
        (setf curprop (first (second curform)))
        (if (lib-form-p curprop) (setf curprop (fourth curprop)))
        (setf curval (fourth curform))
        (setf curvalval (if (lib-form-p curval) 
                          (fourth curval)
                          curval))
        (cond
         ((and (eq curvalval obj)
               (eq curprop 'sk8::container))
          (push curobj (gethash curvalval
                                java-skil-contents-table))
          (setf java-skil-setup-list (remove i java-skil-setup-list)))
         ((and (eq curObj obj)
               (assoc curprop (gethash obj java-skil-property-table)))
          (setf (second (assoc curprop (gethash obj java-skil-property-table))) 
                (java-skil-compile-form curval))
          (setf java-skil-setup-list (remove i java-skil-setup-list)))
         ((and (eq curObj obj)
               (neq curprop 'sk8::container))
          (setf formlist (nconc formlist (list i)))
          (setf java-skil-setup-list (remove i java-skil-setup-list))))
        ))
    (setf (gethash obj java-skil-setup-table) (mapcar #'transform-sets-for-initialize formlist))
    ))


(defun preprocess-setup ()
  (setf java-skil-contents-table (make-hash-table :test #'eq))
  (setf java-skil-setup-table (make-hash-table :test #'eq))
  (dolist (obj (sk8::keys java-skil-object-table))
    (when (valid-object-to-make-a-java-class-for obj) 
      (find-and-remove-appropriate-setup obj)
      )))


;;(convert-to-java "sk8;temp.sk8" "sk8;tempdir:")
;;(trace skil::convert-to-java output-current-java-translation find-and-remove-appropriate-setup)
;;(trace skil::valid-object-to-make-a-java-class-for)
(defun output-current-java-translation (&optional (outputPath ""))
  (let (currentFile 
        objname objtype 
        propvar proptype propvalue propgetter propsetter 
        cursetup 
        curcontents
        curtags curtag
        curhandlers
        curhand)
    (sk8::sendtolog "Outputting Translation:~%")
    (sk8::sendtolog "Outputting Objects~%")
    (preprocess-setup)
    (dolist (obj (sk8::keys java-skil-object-table))
      (when (valid-object-to-make-a-java-class-for obj) 
        (setf objname (sk8ObjectInJava obj))
        (sk8::sendtolog (format nil "--Outputting ~a.java~%" objname))
        (if (> (length objname) 25) (setf objname (subseq objname 0 25)))
        (setf currentFile (sk8dev::process-potential-file (format nil "~a~a.java" outputPath objname)))
        (with-open-file (outStream currentFile :direction :output :if-does-not-exist :create :if-exists :supersede)
          (format outstream "//  The following code was automatically converted to Java from SK8Script~%")
          (format outstream "//  Copyright 1996 Apple Computer Inc.~%~%")
          (format outstream "//  Generated at ~a on ~a ~a, ~a with SK8 ~a~%~%" 
                  (sk8::timestring sk8::now)
                  (sk8::monthname sk8::now) (sk8::day sk8::now) (sk8::year sk8::now)
                  COMMON-LISP-USER::*versiontext*)
          (format outstream "import java.awt.*;~%~%~%")
          (format outstream "public class ~a extends ~a {~%~%" objname (string-downcase (sk8objectinjava (gethash obj java-skil-object-table))) )
                    
          (setf curhandlers (gethash obj java-skil-handler-table))

            ;;Output Properties
          (when (gethash obj java-skil-property-table)
            (format outstream "	//----------------------------------------~%")
            (format outstream "	//Property Definitions:~%")
            (format outstream "	//----------------------------------------~%")
            (dolist (prop (nreverse (gethash obj java-skil-property-table)))
              (setf propvar (generate-java-property-var (first prop)))
              (setf proptype (get-property-type obj (first prop)))
              (setf propvalue (java-create-single-arg (second prop) proptype))
              (setf proptype (generate-java-type proptype))
              (setf propgetter (sk8HandlerInJava (first prop)))
              (setf propsetter (sk8setHandlerInJava (first prop)))
              (format outstream "~%	//The ~a Property:~%" propgetter)
              (format outstream "	protected ~a ~a~a;~%" proptype propvar 
                      (if propvalue (format nil " = ~a" propvalue) ""))
              
              ;;OUTPUT GETTER
              (setf curhand (assoc (first prop) curhandlers))
              (if curhand
                (progn
                  (format outstream (second curhand))
                  (setf curhandlers (nremove curhand curhandlers)))
                (format outstream 
                        "	public ~a ~a () {
		return ~a;
	}~%" 
                        proptype propgetter propvar))
              
              
              ;;OUTPUT SETTER
              (setf curhand (assoc (list 'set (first prop)) curhandlers :test #'equal))
              (if curhand
                (progn
                  (format outstream (second curhand))
                  (setf curhandlers (nremove curhand curhandlers)))
                (format outstream 
                        "	public void ~a (~a value) {
		~a = value;
	}~%"  
                        propsetter proptype propvar))
              )
            )

          (setf curtags (gethash obj java-skil-tag-table))
          (when curtags
            (format outstream "	//Tag Properties~%")
            (dolist (i curtags)
              (setf propvar (generate-java-property-var (second i)))
              (setf propgetter (sk8HandlerInJava (second i)))
              (setf proptype (sk8objectinjava (first i)))
              
              (format outstream "	protected ~a ~a;~%" 
                      proptype
                      propvar)
              (format outstream 
                      "	public ~a ~a () {
		return ~a;
	}~%" 
                      proptype propgetter propvar)
              ))
          
          (setf cursetup (gethash obj java-skil-setup-table))
          (setf curcontents (gethash obj java-skil-contents-table))
          ;;Output Handlers
          (when (or curtags cursetup curhandlers)
            (format outstream "~%~%	//----------------------------------------~%")
            (format outstream "	//Handler Definitions:~%" objname)
            (format outstream "	//----------------------------------------~%~%")
            
            ;;Initialization function to set up contents and tags...
            (when (or curtags cursetup)
              (format outstream "	//Initialization function~%")
              (format outstream "	public ~a() {~%" objname)
              (format outstream "		super();~%")
              (when curcontents
                (format outstream "		actor currentContent;~%")
                (dolist (i curcontents)
                  (format outstream "		currentContent = new ~a();~%" (sk8objectinjava i))
                  (format outstream "		currentContent.setcontainer(this);~%")
                  (setf curtag (assoc i curtags))
                  (when curtag
                    (format outstream "		this.~a = (~a)currentContent;~%" 
                            (generate-java-property-var (second curtag))
                            (sk8objectinjava i))
                    (setf curtags (remove (assoc i curtags) curtags))
                    )
                  ))
              (dolist (i curtags)
                (format outstream "		this.~a = new ~a();~%"
                        (generate-java-property-var (second i))
                        (sk8objectinjava (first i))
                        ))
              (dolist (i cursetup)
                (format outstream "		~a;~%" i))
              (format outstream "	}~%"))
            
            ;;and the user defined handlers...
            (dolist (hand (nreverse curhandlers))
              (write-string (second hand) outstream)  ;;;(format outstream hand)
              (format outstream "~%~%")
              )
            )
          
          (format outstream "}~%")
          )
        ))
    
    ;; now the project file definition
    (setf objname (currentProjectInJava))
    (if (> (length objname) 25) (setf objname (subseq objname 0 25)))
    (setf currentFile (sk8dev::process-potential-file (format nil "~a~a.java" outputPath objname)))
    (with-open-file (outStream currentFile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format outstream "//  The following code was automatically converted to Java from SK8Script~%")
      (format outstream "//  Copyright 1996 Apple Computer Inc.~%~%")
      (format outstream "public final class ~a {~%~%" (currentProjectInJava) )
      
      
      
      ;;we make the instances, one per class.
      (sk8::sendtolog "Outputting Globals~%")
      (when java-skil-global-list
        (format outstream "~%~%	//----------------------------------------~%")
        (format outstream "	//Global Variables~%")
        (format outstream "	//----------------------------------------~%~%")
        (dolist (obj (nreverse java-skil-global-list))
          (format outstream obj)))
      
      ;;we make the instances, one per class.
      (sk8::sendtolog "Outputting Instances~%")
      (when (sk8::keys java-skil-object-table)
        (format outstream "~%~%	//----------------------------------------~%")
        (format outstream "	//Object Instances~%")
        (format outstream "	//----------------------------------------~%~%")
        (dolist (obj (sk8::keys java-skil-object-table))
          (if (listp obj) (setf obj (first obj)))
          (setf objname (sk8ObjectInJava obj))
          (setf objtype objname)
          (when (and (not (valid-object-to-make-a-java-class-for obj))
                     (boundp obj)
                     (symbol-value obj)
                     (sk8::baseparent (symbol-value obj)))
            (setf objtype (string-downcase (sk8::objectname (sk8::baseparent (symbol-value obj))))))
          (format outstream "	public static ~a ~a = new ~a();~%" objtype objname objtype)))
      
      ;;we output the project functions
      (sk8::sendtolog "Outputting Functions~%")
      (when java-skil-function-list
        (format outstream "~%~%	//----------------------------------------~%")
        (format outstream "	//Project Functions~%")
        (format outstream "	//----------------------------------------~%~%")
        (dolist (fun (nreverse java-skil-function-list))
          (write-string fun outstream)
          (format outstream "~%~%")))
      
      ;;setup function
      (sk8::sendtolog "Outputting Setup Function~%")
      (format outstream "~%~%	//----------------------------------------~%")
      (format outstream "	//The Setup Function to initialize the Program~%")
      (format outstream "	//----------------------------------------~%~%")
      (format outstream "	public static void setup() {~%")
      (dolist (com java-skil-setup-list)
        (format outstream "		~a;~%" (second com)))
      (format outstream "	}~%")
      
      (format outstream "}~%"))
    (sk8::sendtolog "Done~%~%")
    t))





;;___________________________________________________________________________________________________________________
;;___________________________________________________________________________________________________________________
(defparameter java-skil-primitive-functions (make-hash-table :test #'eq))

(defun java-add-skil-primitive-function (funsym javaFun)
  (setf (gethash funsym java-skil-primitive-functions) javaFun)
  t)

(defun java-get-primitive-function (funsym)
  (gethash funsym java-skil-primitive-functions))

;;___________________________________________________________________________________________________________________
(defparameter java-skil-foreign-functions (make-hash-table :test #'eq))

(defun java-add-skil-foreign-function (funsym javaFun)
  (setf (gethash funsym java-skil-foreign-functions) javaFun)
  t)

(defun java-get-foreign-function (funsym)
  (gethash funsym java-skil-foreign-functions))


;;___________________________________________________________________________________________________________________

;(add-skil-foreign-function 'debugging-setup nil)

;;basic operators

(java-add-skil-foreign-function 'startsWith 'startswith)
(java-add-skil-foreign-function 'endswith 'startswith)
(java-add-skil-foreign-function 'as-a 'asa)
(java-add-skil-foreign-function 'contains 'contains)   ;;;note that if replace the single quote with a double quote we see a bogus object

;;collection stuff....
(java-add-skil-foreign-function 'singleItemMapper 'singleitemmapper)
(java-add-skil-foreign-function 'fullRangeMapper 'fullrangemapper)
(java-add-skil-foreign-function 'insertInFront 'insertinfront)
(java-add-skil-foreign-function 'insertAtEnd 'insertatend)
(java-add-skil-foreign-function 'anyindex 'anyindex)
(java-add-skil-foreign-function 'middleIndex 'middleindex)


;;___________________________________________________________________________________________________________________

(defun java-skil-compile-form (form)
  (if (and form (listp form))
    (cond
     ;;; lib forms are function calls.
     ((and (listp (first form)) (lib-form-p (first form)))
      (cond
       ((eq (fourth (first form)) 'sk8::new)
        (java-skil-compile-new (cons (java-skil-compile-form (first form)) (rest form))))
       ((eq (fourth (first form)) 'sk8::addproperty)
        (java-skil-compile-addproperty (cons (java-skil-compile-form (first form)) (rest form))))
       ((eq (fourth (first form)) 'sk8::addAbstractPropertyType)
        "")
       ((eq (fourth (first form)) 'sk8::tagpart)
        (java-skil-compile-tagpart (cons (java-skil-compile-form (first form)) (rest form))))
       (t 
        (java-skil-compile-function form))))
     ;;; check if first item is a primitive compiler form (e.g. script)
     ((java-get-primitive-function (first form))
      (funcall (java-get-primitive-function (first form)) form))
     ;;; check if first item is a foreign function (e.g. +)
     ((java-get-foreign-function (first form))
      (java-skil-compile-function (cons (java-get-foreign-function (first form)) (rest form))))
     ;;; otherwise pass it on through.  this is really an error that needs to be fixed.
     (t
      (sk8::sendtolog (format nil "Warning (could be a compiler error): Unknown SKIL type: ~s~%" form))
      (java-skil-compile-function form))
     )
    (java-compile-atom form)))

;;____________________________________________________________________________________________________________

(defun java-compile-atom (x)
  (cond
   ((eq x 'sk8::me) "this")
   ((eq x t) "true")
   ((eq x nil)
    (if (and current-java-form-type-for-set
             (neq current-java-form-type-for-set 'sk8::boolean))
      "null" 
      "false"))
   ((eq x sk8::undefined) "null")
   ((symbolp x)
    (sk8ObjectInJava x))
   (t
    (sk8::objectstring x))
   ))
;;___________________________________________________________________________________________________________________


(defun java-create-arg-string (form types)
  (let ((res "")
        (atleastone nil)
        key val
        curitem
        curtype
        )
    (ccl::while (and form (neq (setf curitem (pop form)) 'with))
      (setf curtype (pop types))
      (setf res (concatenate 'string res (java-create-single-arg curitem curtype) ", "))
      (setf atleastone t))
    (when atleastone
      (setf res (subseq res 0 (- (length res) 2))))
    (ccl::while form
      (setf key (pop form))
      (setf val (pop form))
      (if (or (string= (symbol-name key) "PHYSICAL")
              (string= (symbol-name key) "AFTER"))
        (setf res (concatenate 'string res (if atleastone ", " "") (java-skil-compile-form val)))
        (setf res  (concatenate 'string res  
                                (format nil "/* IGNORED KEY: ~a ~a */"
                                        (symbol-name key)
                                        (java-skil-compile-form val))
                                ))))
    res))


;;(defparameter gather-sk8-function-list t)
;;(defparameter sk8-function-list nil)
;;(pprint sk8-function-list)

(defun java-skil-compile-function (form)
  (let* ((funName (pop form))
         funNameString
         (res nil)
         types
         firstitem
         (libfunction? (lib-form-p funname))
         (sk8function? (and libfunction?
                            (member (second funname) '("COMMON-LISP" "CCL" "SK8") :test #'string=)))
         function?
         compiledFirstItem
         )
    (if libfunction? (setf funname (fourth funname)))
    (if (eq funname 'sk8::boundsrect) (setf funname 'sk8::boundsrectlist))
    (if (eq funname 'sk8::location) (setf funname 'sk8::locationlist))
    (if (eq funname 'sk8::size) (setf funname 'sk8::sizelist))
    (when (eq funname 'skil::collectionnth)
      (case current-java-form-type-for-set
        (sk8::integer (setf funname 'nthint))
        (sk8::number (setf funname 'nthint))
        (sk8::float (setf funname 'nthfloat))
        (t (setf funname 'nth)))
      (setf libfunction? t) 
      (setf sk8function? nil))
    (cond
     ;;;not a lib form, so it must be either a primitive or a bogus call which we default as a function...
     ((not libfunction?)
      (setf function? t)
      (setf firstitem 'sk8::sk8))
     ;;;this is a function which is function in sk8's library
     ((and sk8function?
           (fboundp funname)
           (or (eq funname 'sk8::collectionsubsequence)
               (not (CCL::standard-generic-function-p (symbol-function funname)))))
      (setf function? t)
      ;;(if gather-sk8-function-list (pushnew funname sk8-function-list));;could gather up a list of functions here...
      (setf firstitem 'sk8::sk8)
      )
     ;;;this is a project function...
     ((and libfunction?
           (project-function-p funname))
      (setf function? t)
      (setf firstitem (sk8::sk8_id (currentProjectSymbolForJava)))
      )
     ;;;otherwise it's a handler
     (t
      (setf function? nil)
      (setf firstitem (pop form))))
    (if function?
      (setf types (rest (get-function-type funname)))
      (setf types (rest (get-handler-type (get-skil-form-type firstitem) funname))))
    (if function?
      (setf compiledFirstItem (java-skil-compile-form firstitem))
      (progn
        (setf compiledFirstItem (java-create-single-arg firstitem (first types)))
        (setf types (rest types))))
    (setf funNameString (java-skil-compile-form funName))
    (setf res (concatenate 'string  compiledFirstItem "." (sk8HandlerInJava funNameString) "("))
    (setf res (concatenate 'string res (java-create-arg-string form types)))
    (setf res (concatenate 'string res ")"))
    res))

;;(java-it "set {xx,yy} to the contents of the stage")
;;(java-it "sendtolog(10)")
;;(java-it "max(1,2)")
;;(java-it "beep()")

;;___________________________________________________________________________________________________________________

(defun java-skil-compile-new (form)
  (setf form (deep-copy-list form))
  (let* (curObjSym
         key val setvalform
         (keywordArgs (rest (rest (rest form))))
         (parentObj (second form))
         )
    (ccl::while keywordArgs
      (setf key (pop keywordArgs))
      (setf val (pop keywordArgs))
      (case key
        (sk8::objectName
         (if (stringp val)
           (setf curObjSym 
                 (let ((*package* (package (currentProjectSymbolForJava))))  ;;; this seems lame..
                   (read-from-string val nil nil)))))
        (otherwise
         (when curObjSym
           (setf setvalform `(set ((lib ,(ccl::keywordify (sk8::sk8_id (currentProjectSymbolForJava))) function ,key) 
                                   (lib ,(ccl::keywordify (sk8::sk8_id (currentProjectSymbolForJava))) object ,curObjSym))
                                  to ,val))
           (setf val (java-skil-compile-form setvalform))
           (setf java-skil-setup-list 
                 (nconc java-skil-setup-list (list (list setvalform val))))
           )
         )))
    (if (lib-form-p parentObj)
      (setf parentObj (sk8objectinjava (fourth parentobj))))
    (if java-skil-current-function
      (progn
        (if curObjSym
          (print "can only define an object with an objectname outside of a handler"))
        (if (stringp parentObj) 
          (pushnew parentObj java-skil-new-objects))
        (if (stringp parentObj)
          (format nil "new ~a()" (sk8objectinjava parentObj)) 
          (format nil "sk8.sk8new(~a)" (java-skil-compile-form parentObj)))
        )
      (progn
        (pushnew parentObj java-skil-new-objects)
        (when curObjSym
          (unless (boundp curobjsym) (pushnew (sk8objectinjava curObjSym) java-skil-new-objects))
          (setf (gethash curObjSym java-skil-object-table) parentObj))))))

;;___________________________________________________________________________________________________________________

(defun java-skil-compile-addproperty (form)
  (setf form (deep-copy-list form))
  (let* ((initValLoc (position "INITIALVALUE" form :key #'(lambda (x) (if (symbolp x) (symbol-name x) "")) :test #'string=))
         (initVal (if initValLoc (nth (1+ initValLoc) form) sk8::undefined))
         (curObjSym (second form))
         (propname (third form))
         )
    (if (lib-form-p curobjsym) (setf curobjsym (fourth curobjsym)))
    (if (and (listp propname) (eq (first propname) 'quote)) (setf propname (second propname)))
    (if java-skil-current-function
      (error "can only perform addproperty outside of a handler")
      (push (list propname (if initValLoc initval nil))
            (gethash curObjSym java-skil-property-table))
      )))

;;___________________________________________________________________________________________________________________

(defun java-skil-compile-tagpart (form)
  (setf form (deep-copy-list form))
  (let* ((tagger (second form))
         (taggee (third form))
         (propname (fourth form))
         )
    (if (lib-form-p tagger) (setf tagger (fourth tagger)))
    (if (lib-form-p taggee) (setf taggee (fourth taggee)))
    (pushnew (sk8objectinjava taggee) java-skil-new-objects)
    (if (and (listp propname) (eq (first propname) 'quote)) (setf propname (second propname)))
    ;;;ALSO SHOULD ADD TAGEMIT HANDLER TO TAGGEE SO PEOPLE CAN FIND IT!
    (if java-skil-current-function
      (error "can only perform tagpart outside of a handler")
      (push (list taggee propname) (gethash tagger java-skil-tag-table))
      )))
;;___________________________________________________________________________________________________________________

;;(java-skil-compile-items 'val)
(defun java-skil-compile-items (forms)
  (if (stringp forms)
    forms
    (let ((res "") curres)
      (unless (listp forms) (setf forms (list forms)))
      (dolist (x forms)
        (when x
          (setf curres (java-skil-compile-form x))
          (unless (or (string= curres "")
                      (and (> (length curres) 34) (string= (subseq curres 0 34) "		{  //---------Special Collection"))
                      (and (listp x) 
                           (memq (first x) '(script catch loop if call-with-statement))))
            (setf curres (format nil "		~a;~%" curres)))
          (when (and (listp x) (memq (first x) '(loop if)))
            (setf curres (format nil "		~a" curres)))
          (setf res (concatenate 'string res curres))))
      res)))

;;___________________________________________________________________________________________________________________

;;  ignoring form and special setter and getter forms only work when project is loaded...***

(defun java-skil-compile-call-do-inherited (form)
  (declare (ignore form))  ;;;****************************
  (let* ((setterp? (and java-skil-current-function (listp java-skil-current-function)))
         (curfunction (if setterp? (second java-skil-current-function) java-skil-current-function))
         currentobject
         currentsettovar)
    (when (and java-skil-current-arguments
               (listp (first java-skil-current-arguments))
               (eq (first (first java-skil-current-arguments)) 'SK8::ME))
      (setf currentobject (second (first java-skil-current-arguments)))
      (if (lib-form-p currentobject) (setf currentobject (fourth currentobject))))
    (when (and currentobject
               setterp?)
      (setf currentSetToVar (first (last java-skil-current-arguments)))
      (if (listp currentSetToVar) (setf currentSetToVar (first currentSetToVar))))
    (if (and curfunction 
             currentobject
             (boundp currentobject)
             (symbol-value currentobject)
             (memq curfunction (sk8::localproperties (symbol-value currentobject))))
      (if setterp?
        (format nil "~a = ~a" (generate-java-property-var curfunction) (java-skil-compile-form currentSetToVar))
        (format nil "return ~a" (generate-java-property-var curfunction)))
      (let (args) 
        (setf args (java-create-arg-string (mapcar #'(lambda (x) (if (listp x) (first x) x))
                                                   (rest java-skil-current-arguments))
                                           nil
                                           ))
        (format nil "super.~a(~a)" 
                (javaHandlerName java-skil-current-function) args)))))

(java-add-skil-primitive-function 'call-do-inherited #'java-skil-compile-call-do-inherited)


;;___________________________________________________________________________________________________________________
#| simple version
(defun java-skil-compile-loop (form)
  (format nil "while (true) {~%~a }~%" (java-skil-compile-items (rest form))))
|#
;;more readable generated code version.
(defun java-skil-compile-loop (form)
  (let* ((body (rest form))
         (firstform (and (listp body) (first body))))
    ;;; if the first form is the thing that would exit our loop for us, we 
    ;;; make it into a nice while loop
    (if (and (listp firstform)
             (eq (first firstform) 'if)
             (equalp (fourth firstform) '(throw 'exit-repeat nil)))
      (format nil "while (~a) {~%~a		}~%" 
              (java-skil-compile-form (list 'not (second firstform)))
              (java-skil-compile-items (rest body)))
      ;; otherwise we do "while true" for the equivalent of loop.
      (format nil "while (true) {~%~a		}~%" (java-skil-compile-items body)))))

(java-add-skil-primitive-function 'loop #'java-skil-compile-loop)
;;___________________________________________________________________________________________________________________

(defun java-skil-compile-error (form)
  (declare (ignore form))
  (format nil "throw new Exception() ;~%")) ;;,(java-skil-compile-form (second form))

(java-add-skil-primitive-function 'error #'java-skil-compile-error)
;;___________________________________________________________________________________________________________________

(defun java-skil-compile-quote (form)
  (format nil "sk8.quote(\"~a\")" (symbol-name (second form))))

(java-add-skil-primitive-function 'quote #'java-skil-compile-quote)
;;___________________________________________________________________________________________________________________

(defun java-skil-compile-funcall (form)
  (if (and (listp (second form)) (equal (second (second form)) '(input val)))
    (java-skil-compile-form (third form)) ;;ignore setter hack
    (progn
      (unless (symbolp (second form)) (setf (second form) "UNKNOWNANON"))   ;;; THIS NEEDS TO BE THE GENSYM OF THE ANON FUNCTION
      (setf (first form) '(lib "SK8" function sk8::invoke))
      (java-skil-compile-function form)))
  )

(java-add-skil-primitive-function 'funcall #'java-skil-compile-funcall)
;;___________________________________________________________________________________________________________________

(defun java-skil-compile-middlefun (form)
  (format nil "(~a ~a ~a)" 
          (java-skil-compile-form (second form))
          (java-skil-converttostring (first form))
          (java-skil-compile-form (third form))))

(defun java-skil-converttostring (sym)
  (case sym
    (and "&&")
    (or "||")
    (+ "+")
    (- "-")
    (* "*")
    (/ "/")
    (^ "^")
    (< "<")
    (> ">")
    (<= "<=")
    (>= ">=")
    (div "/")
    (mod "%")
    (eq "==")
    ))

(java-add-skil-primitive-function '* #'java-skil-compile-middlefun)
(java-add-skil-primitive-function '/ #'java-skil-compile-middlefun)
(java-add-skil-primitive-function '< #'java-skil-compile-middlefun)
(java-add-skil-primitive-function '> #'java-skil-compile-middlefun)
(java-add-skil-primitive-function '<= #'java-skil-compile-middlefun)
(java-add-skil-primitive-function '>= #'java-skil-compile-middlefun)
(java-add-skil-primitive-function 'div #'java-skil-compile-middlefun)
(java-add-skil-primitive-function 'mod #'java-skil-compile-middlefun)
(java-add-skil-primitive-function 'eq #'java-skil-compile-middlefun)
(java-add-skil-primitive-function '& #'java-skil-compile-middlefun)

;;___________________________________________________________________________________________________________________
(defun java-skil-compile-= (form)
  (if (or (stringp (second form))
          (stringp (third form)))
    (let ((firstone (if (stringp (second form)) (third form) (second form) ))
          (secondone (if (stringp (second form)) (second form) (third form))))
      
      (format nil "~a.equals(~a)"
              (java-skil-compile-form firstone)
              (java-skil-compile-form secondone)))
    (format nil "(~a == ~a)"
            (java-skil-compile-form (second form))
            (java-skil-compile-form (third form)))))

(java-add-skil-primitive-function '= #'java-skil-compile-=)

;;(java-it "if  1 = 1 then sendtolog(1)")

;;___________________________________________________________________________________________________________________
(defun java-skil-compile-is-a (form)
  (format nil "(~a instanceof ~a)" 
          (java-skil-compile-form (second form))
          (generate-java-type (third form)))
  )

(java-add-skil-primitive-function 'is-a #'java-skil-compile-is-a)

;;(java-it "if x is a rectangle then sendtolog(1)")

;;___________________________________________________________________________________________________________________
(defun java-skil-compile-& (form)
  (format nil "sk8.concatenate(~a, ~a)" 
          (java-skil-compile-form (second form))
          (java-skil-compile-form (third form)))
  )

(java-add-skil-primitive-function '& #'java-skil-compile-&)

;;(java-it "set x to x & {1}")


;;___________________________________________________________________________________________________________________



(defun java-skil-compile-andor (form)
  (format nil "(~a ~a ~a)" 
          (java-skil-compile-test (second form))
          (java-skil-converttostring (first form))
          (java-skil-compile-test (third form))))
(java-add-skil-primitive-function 'and #'java-skil-compile-andor)
(java-add-skil-primitive-function 'or #'java-skil-compile-andor)


;;___________________________________________________________________________________________________________________

(defun java-skil-compile-MinusPlus (form)
  (if (third form)
    (java-skil-compile-middlefun form)
    (java-skil-compile-middlefun (list (first form) 0 (second form)))))
(java-add-skil-primitive-function '+ #'java-skil-compile-MinusPlus)
(java-add-skil-primitive-function '- #'java-skil-compile-MinusPlus)

;;___________________________________________________________________________________________________________________

(defun java-skil-compile-not (form)
  (cond
   ((and (listp (second form))
         (eq (first (second form)) 'not))
    (java-skil-compile-form (second (second form))))
   ((and (listp (second form))
         (eq (first (second form)) '=))
    (format nil "(~a != ~a)" 
            (java-skil-compile-form (second (second form)))
            (java-skil-compile-form (third (second form))))
    )
   ((eq (get-skil-form-type (second form)) 'sk8::boolean)
    (format nil "(! ~a)" (java-skil-compile-form (second form))))
   (t
    (format nil "(~a == null)" (java-skil-compile-form (second form))))))

(java-add-skil-primitive-function 'not #'java-skil-compile-not)

;;___________________________________________________________________________________________________________________

(defun java-skil-compile-set-to (form)
  (let ((varval (second form))
        (toval (third form))
        (valval (fourth form)))
    (unless (or (= (length form) 4) (eq toval 'to))
      (error (format nil "Set forms must be of the form set var to val ~%~s" form)))
    (if (and (listp varVal) (neq (first varval) 'lib))
      (let ((additionalArgs (rest (rest varval))))  ;; to handle with physical stuff
        (setf valval (cons valval additionalArgs))  ;; add all aditional args of the property access after the value argument
        (format nil "~a.~a(~a)" 
                (java-skil-compile-form (second varval))
                (sk8setHandlerInJava (java-skil-compile-form (first varval)))
                (java-create-arg-string valval (list (get-return-type varval)))))
      (if (and (lib-form-p varval) (eq (fourth varval) 'SK8::SK8SCRIPTRESULT))
        (java-skil-compile-form valval)
        (let* ((vartype (get-skil-form-type varval))
               (java-in-set-parameter varval)
               (valueString (let* ((java-in-set-parameter varval) 
                                   (current-java-form-type-for-set vartype))
                              (java-skil-compile-form valval))))
          (if (and (> (length valuestring) 2) (string= (subseq valuestring 0 2) "		"))   ;;; a hack to see if we are setting to a special collection expression...
            valueString
            (format nil "~a = ~a"
                    (java-skil-compile-form varval)
                    (java-create-single-arg valval vartype)
                    ))
          ))
      )))

(java-add-skil-primitive-function 'set #'java-skil-compile-set-to)


;;___________________________________________________________________________________________________________________
(defun deep-member (item loc)
  (if (listp loc)
    (some #'(lambda (x) (deep-member item x)) loc)
    (eq item loc)))


(defun all-but-last (ll)
  (let ((len (length ll)))
    (subseq ll 0 (1- len))))

(defun java-skil-compile-catch (form)
  (when (and (eq (second (second form)) 'CURRENT-FUNCTION)
             (deep-member 'CURRENT-FUNCTION (all-but-last (rest (rest form))))
             )
    (setf (nthcdr (1- (length form)) form) nil)
    )
  (let (catchObject
        (otherForms (java-skil-compile-items (cddr form))))
    (case (second (second form)) 
      (NEXT-REPEAT  (setf catchObject (and (deep-member 'nextRepeatObject otherForms) 'nextRepeatObject)))
      (EXIT-REPEAT (setf catchObject nil))
      (CURRENT-FUNCTION (setf catchObject nil)))
    (if CatchObject
      (setf otherforms (format nil "try {~% ~a ~%} guard (e ~a) { }~%" otherforms (symbol-name catchobject))))
    otherforms)
  )

(java-add-skil-primitive-function 'catch #'java-skil-compile-catch)


#|

(java-it "on testfun
  repeat with i in {1,2,3}
     sendtolog of i
     repeat with j in {4,5,6}
        sendtolog of j
     end repeat
  end repeat
end testfun")

(java-it "on testfun
   set {xx, yy} to the location of the stage
end testfun")

ssCurrentCollection


|#

;;___________________________________________________________________________________________________________________



(defun java-skil-compile-throw (form)
  (case (second (second form)) 
    (NEXT-REPEAT "throw new nextrepeatobject()")
    (EXIT-REPEAT "break")
    (CURRENT-FUNCTION 
     (let* ((setterp? (and java-skil-current-function (listp java-skil-current-function)))
            (curfunction (if setterp? (second java-skil-current-function) java-skil-current-function))
            (return-type (first (get-function-type curfunction)))
            (returnform (third form))
            )
       (format nil "return ~a" (java-create-single-arg returnform return-type )))
    )))

(java-add-skil-primitive-function 'throw #'java-skil-compile-throw)

 ;;___________________________________________________________________________________________________________________
(defun java-skil-compile-declareglobal (form)
  (let (varname vartype varval)
    (declare (ignore-if-unused vartype))
    (if (listp (second form))
      (progn
        (setf varname (first (second form)))
        (setf vartype (second (second form)))
        (setf varval (third (second form)))
        )
      (setf varname (second form)))
    (setf  varval (let* ((current-java-form-type-for-set (if (lib-form-p vartype) (fourth vartype) vartype))) 
                    (java-skil-compile-form varval)))
    (push (format nil "	public static ~a = ~a;~%" (java-generate-type-arg (list varname nil vartype)) varval)
          java-skil-global-list)
    ""
    ))

(java-add-skil-primitive-function 'declareglobal #'java-skil-compile-declareglobal)

;;___________________________________________________________________________________________________________________
(defun java-skil-compile-lib (form)
  (let ((valval (fourth form))
        (packagename (second form)))
    (unless (= (length form) 4)
      (error (format nil "Lib forms must have three arguments ~%~s" form)))
    (when (listp valval)
      (error (format nil "Lib must have an atom as it's fourth element ~%~s" form)))
    (if (member packagename '("SKIL" "GRAPHICS-SYSTEM" "SK8DEVELOPMENT" "PS" "MACFRAMES"
                              "COMMON-LISP-USER"  "CCL" "COMMON-LISP" "KEYWORD")
                :test #'string=)
      (setf packagename "SK8"))
    (when (symbolp valval)
      (when (and (boundp valval) 
                 (sk8::project (symbol-value valval))
                 (not (string= packagename (package-name (package (sk8::project (symbol-value valval)))))))
        (setf packagename (package-name (package (sk8::project (symbol-value valval))))))
      (setf valval (maybe-save-original-sym-case (symbol-name valval) packagename)))
    (case valval
      (sk8::true (java-compile-atom t))
      (sk8::false (java-compile-atom nil))
      (sk8::undefined (java-compile-atom sk8::undefined))
      (t
       (if (eq (third form) 'object)
         (format nil "~a.~a" (string-downcase packagename) (sk8ObjectInJava valval))
         (sk8handlerInJava valval))))
    )
  )

(java-add-skil-primitive-function 'lib #'java-skil-compile-lib)

;;___________________________________________________________________________________________________________________
;;**************FIX ME
(defun java-skil-compile-call-with-statement (form)
  (java-skil-compile-form (third form)) 
  )

(java-add-skil-primitive-function 'call-with-statement        #'java-skil-compile-call-with-statement)
;;___________________________________________________________________________________________________________________

(defun java-skil-compile-test (curtest)
  (if (eq (get-skil-form-type curtest) 'sk8::boolean)
    (progn
      (setf curtest (java-skil-compile-form curtest))
      (when (or (not (listp curtest)) (listp (first curtest)))
        (setf curtest (format nil "(~a)" curtest))))
    (setf curtest (format nil "(~a != null)" (java-skil-compile-form curtest))))
  curtest
  )

(defun java-skil-compile-if (form)
  (let (curtest curactions)
    (pop form) ;; remove if
    (setf curtest (java-skil-compile-test (pop form)))
    (unless (eq (pop form) 'then)
      (error "if lacks a corresponding then"))
    (setf curactions nil)
    (ccl::while (and form (not (memq (first form) '(else else-if))))
      (setf curactions (append curactions (list (pop form))))
      )
    (setf curActions (java-skil-compile-items curActions))
    (cond
     ((not (first form))
      (format nil "if ~a { ~% ~a		}~%" curtest curactions))
     ((eq (first form) 'else-if)
      (concatenate 'string 
                   (format nil "if ~a { ~% ~a		} else " curtest curactions)
                   (java-skil-compile-if form)))
     ((eq (first form) 'else)
      (format nil "if ~a { ~% ~a		} else { ~% ~a		}~%" curtest curactions (java-skil-compile-items (rest form))))
     (t
      (error "Expected else or else if or end of statement")))
    )
  )

(java-add-skil-primitive-function 'if  #'java-skil-compile-if)
;;___________________________________________________________________________________________________________________

(defun java-compile-as-collection-for-loop (form)
  (let* ((locals (skil-get-key form 'local nil))
         (simplecollectionvarval (or (symbolp (second (first locals))) (lib-form-p (second (first locals)))))
         (collectionvarval (java-skil-compile-form (second (first locals))))
         (collectionvar (if simplecollectionvarval collectionvarval (java-skil-compile-form (first (first locals)))))
         (vsvar (java-skil-compile-form (first (second locals))))
         (iteratorvar (if (listp (third locals)) (first (third locals)) (third locals)))
         (iteratorvartype (generate-java-type (get-variable-type iteratorvar)))
         (internalbody (rest (third (third (fourth (fourth form))))))
         )
    (setf iteratorvar (java-skil-compile-form iteratorvar))
    (concatenate 
     'string 
     (format nil "		~a ~a;~%" iteratorvartype iteratorvar)
     (if simplecollectionvarval "" (format nil "		collection ~a = ~a;~%" collectionvar collectionvarval))
     (format nil "		for (visitstate ~a = ~a.initialvisitstate(); (~a != null); ~a = ~a.succeedingvisitstate(~a)) {~%"
             vsvar collectionvar vsvar vsvar collectionvar vsvar)
     (format nil "		 ~a = ~a~a.elementatvisitstate(~a);~%" 
             iteratorvar 
             (if (string= iteratorvartype "Object") "" (format nil "(~a)" iteratorvartype))
             collectionvar
             vsvar)
     (java-skil-compile-items internalbody)
     (format nil "		}~%")
     )
    ))

(defun java-compile-as-counting-for-loop (form)
  (let* ((locals (skil-get-key form 'local nil))
         (rawbyvarval (second (second locals)))
         (negativebyvarp (and (listp rawbyvarval)
                              (= (length rawbyvarval) 2)
                              (eq (first rawbyvarval) '-)
                              ))
         (simplebyvar (or (not (listp rawbyvarval)) 
                          (lib-form-p rawbyvarval)
                          negativebyvarp))
         (internalbody (rest (third (third (fourth form)))))
         (countvar (java-skil-compile-form (first (first locals)))) 
         (startval (java-skil-compile-form (second (first locals)))) 
         (endval (java-skil-compile-form (third (third (second ( second (second (fourth form))))))))
         (byvar (java-skil-compile-form (first (second locals)))) 
         (byvarval (java-skil-compile-form rawbyvarval)) 
         testform)
    (when simplebyvar
      (setf simplebyvar (java-skil-compile-form rawbyvarval))
      (setf byvar simplebyvar))
    (cond
     ((and (numberp rawbyvarval)
           (>= rawbyvarval 0))
      (setf testform (format nil "(~a <= ~a)" countvar endval)))
     (negativebyvarp
      (setf byvarval (java-skil-compile-form (second rawbyvarval)))
      (setf testform (format nil "(~a >= ~a)" countvar endval)))
     (t
      (setf testform (format nil "(((~a >= 0) && (~a <= ~a)) || ((~a < 0) && (~a >= ~a)))" 
                             byvar countvar endval byvar countvar endval))))
    (concatenate 
     'string 
     (if simplebyvar "" (format nil "		int ~a = ~a;" byvar byvarval))
     (format nil "		for (int ~a = ~a;  ~a;  ~a ~a= ~a) {~%"
             countvar startval testform countvar (if negativebyvarp "-" "+") (if simplebyvar byvarval byvar))
     (java-skil-compile-items internalbody)
     (format nil "		}~%")
     )
    ))

;;___________________________________________________________________________________________________________________

(defun java-create-single-arg (returnform return-type)
  (let* ((returnformstring (let ((current-java-form-type-for-set return-type)) (java-skil-compile-form returnform))))
    (if (or (null returnform)
            (eq returnform 'sk8::false)
            (eq returnform 'sk8::undefined)
            (and (lib-form-p returnform)
                 (eq (fourth returnform) 'sk8::false)))
      (if (eq return-type 'sk8::boolean)
        (setf returnformstring "false")
        (setf returnformstring "null"))
      (when (and (neq returnform 'sk8::me) ;;should never bother trying to type me...
                 return-type
                 (requires-casting return-type (get-skil-form-type returnform))
                 (not (string= returnformstring "null")))
        (unless (or (lib-form-p returnform) (not (listp returnform)))
          (setf returnformstring (format nil "(~a)"  returnformstring)))
        (setf returnformstring (format nil "((~a)~a)" (generate-java-type return-type) returnformstring))))
    returnformstring)
  )

;;___________________________________________________________________________________________________________________


(defun java-skil-process-keyword-args (inputs with-macro?)
  (declare (ignore with-macro?))
  inputs)


(defun generate-java-type (type)
  (if (lib-form-p type)
    (setf type (fourth type)))
  (if (listp type) (setf type (first type)))
  (cond
   ((symbolp type)
    (setf type (sk8objectinjava type)))
   ((stringp type)
    nil)
   ((not type)
    (setf type "Object"))
   (t (inspect type) (error "weird type ~a" type)))
  (cond
   ((or (string= type "false") (string= type "nil"))
    (setf type "Object"))
   ((string= type "string")
    (setf type "String"))
   ((string= type "character")
    (setf type "Character"))
   ((string= type "float")
    (setf type "float"))
   ((string= type "integer")
    (setf type "int"))
   ;;these two are weird
   ((string= type "floatlist")
    (setf type "list"))
   ((string= type "number")
    (setf type "int"))
   )
  type)



(defun java-generate-type-arg (i)
  (let ((res "") curvar curtype)
    (if (listp i)
      (setf curvar (first i)
            curtype (third i))
      (setf curvar i))
    (setf curtype (generate-java-type curtype))
    (setf res (concatenate 'string  curtype " " (sk8objectinjava curvar)))
    res))

(defun java-process-input-args (arglist) 
  (let ((res ""))
    (dolist (i arglist) 
      (setf res (concatenate 'string res (java-generate-type-arg i) ", ")))
    (when arglist
      (setf res (subseq res 0 (- (length res) 2))))
    res))

(defun javaHandlerName (handlerArg)
  (if (listp handlerArg)
    (progn
      (unless (eq (first handlerArg) 'set)
        (error (format nil "Only handle set handlers not: ~s" (first handlerArg))))
      (sk8setHandlerInJava (second handlerArg))
      )
    (sk8HandlerInJava handlerArg)))


(defun generate-java-local (x)
  (let ((curtype (java-generate-type-arg x)))
    (concatenate 'string
                 "		"
                 curtype 
                 (if (and (listp x) (second x) (neq (first x) 'sscurrentpathexpressioncollectionset)) 
                   (concatenate 'string
                                " = "
                                (java-skil-compile-form (second x)))
                   "")    
                 ";" (string #\newline)))
  )


(defun generate-java-locals-for-script (locals)
  (let ((res "")
        (condensedLines "")
        (curline "")
        (typetable (make-hash-table :test #'equal))
        curtype curitem)
    (setf locals (mapcar #'(lambda (x) (if (listp x) 
                                         (list (first x) (second x) (get-variable-type (first x)))
                                         (list x nil (get-variable-type x))))
                         locals))
    (dolist (x locals)
      (if (and (or (eq (second x) nil) (eq (second x) 'sk8::undefined))
               (neq (third x) 'sk8::boolean))
        (progn
          (setf curtype (generate-java-type (third x)))
          (push (first x) (gethash curtype typetable)))
        (setf res (concatenate 'string res (generate-java-local x)))))
    (dolist (k (sk8::keys typetable))
      (setf curline (concatenate 'string
                                 "		"
                                 k " "
                                 ))
      (setf curitem (gethash k typetable))
      (dolist (i (nreverse curitem))
        (setf curline (concatenate 'string curline
                                   (sk8ObjectInJava i) ", ")))
      (setf curline (subseq curline 0 (- (length curline) 2)))
      
      (setf curline (concatenate 'string curline
                                 ";" (string #\newline)))
      (setf condensedLines (concatenate 'string condensedLines curline)))
    (concatenate 'string condensedLines res)
    ))


(defun do-java-skil-compile-script (form)
  (let* ((inputs (skil-get-key form 'input nil))
         (locals (skil-get-key form 'local nil))
         (globals (skil-get-key form 'global nil))
         (comments (skil-get-key form 'Comments nil))
         (return-type (first (skil-get-key form 'return-type t)))
         (name (first (skil-get-key form 'name t)))
         (errors (skil-get-key form 'on-error nil))
         (cleanup-code (first (skil-get-key form 'cleanup-code t)))
         (body (java-skil-compile-items 
                (remove-if #'(lambda (x) 
                               (and (listp x) (memq (first x) *valid-skil-keys*)))
                           (rest form))))
         handlerArg handlerObject
         newCode )
    (declare (ignore globals errors cleanup-code))

    ;;; we handle the loop cases specially so they become very readable
    (when locals
      (let ((firstLocalName (first locals)))
        (when (and firstLocalName (listp firstLocalName))
          (setf firstLocalName (symbol-name (first firstLocalName)))
          (if (and (> (length firstLocalName) 19) (string= (subseq firstLocalName 0 19) "SSCURRENTCOLLECTION"))
            (return-from do-java-skil-compile-script (java-compile-as-collection-for-loop form)))
          ))
      (let ((firstLocalName (second locals)))
        (when (and firstLocalName (listp firstLocalName))
          (setf firstLocalName (symbol-name (first firstLocalName)))
          (if (and (> (length firstLocalName) 12) (string= (subseq firstLocalName 0 12) "SSBYVARIABLE"))
            (return-from  do-java-skil-compile-script (java-compile-as-counting-for-loop form))))))

    (setf locals (generate-java-locals-for-script locals))
    (if locals
      (setf newcode (concatenate 'string locals body))
      (setf newcode body))
    (when (and inputs (listp (first inputs)) (second (first inputs)))
      (setf handlerArg name)
      (setf handlerObject (cadar inputs))
      (when (listp handlerObject) (setf handlerObject (java-skil-compile-form handlerObject)))
      (setf inputs (cons handlerObject (rest inputs))))
    (setf inputs (java-skil-process-keyword-args inputs (and (listp name) (eq (first name) 'with))))
    (when (and handlerArg (not name))
      (error "Handler definitions need a name"))
    (when (or name inputs)
      (cond
       ((not name)
        (error "no anoymous functions"))
       (handlerArg
        (setf handlerArg (javaHandlerName handlerArg))
        (setf newcode 
              (format nil "	public ~a ~a ( ~a ) {~% ~a	}~%"
                      (generate-java-type return-type)
                      handlerArg (java-process-input-args (rest inputs)) newcode
                      )
              ))
       ((and (listp name)
             (eq (first name) 'with))
        (error "CAN'T CONVERT WITH FUNCTIONS YET!!!!"))
       (t
        (setf name (sk8HandlerInJava name))
        (setf newcode 
              (format nil "	public static ~a ~a ( ~a ) {~% ~a	}~%"
                       (generate-java-type return-type)
                     name (java-process-input-args inputs) newcode
                      )
              ))
       ))
    (when comments
      (setf newcode (concatenate 'string " */" (string #\newline)  newcode))
      (dolist (i (reverse comments))
        (when (stringp i)
          (setf newcode (concatenate 'string i (string #\newline) newcode))))
      (setf newcode (concatenate 'string "/*  "  newcode)))
    newCode)
  )

;;if it's an anonymous function, then make sure we are in named handler
;;generate name and input list from inputs and locals.  
;;generate function call and push it into a global for this named handler
;;replace funcall and anonymous function with function name and added locals list.


(defun java-skil-compile-anonymous-function (form)
  (declare (ignore form))
  "ANONYMOUS")



(defun java-collection-macro (coll multiple startIndex endindex typeObj wherefilter action)
  (let (ourTest
        (needOurCount (sk8dev::deepmemq 'ssOurCount action))
        (needCurrentCount (not (if multiple 
                                 (and (numberp startIndex) (= startIndex 1) (not endIndex)) 
                                 (and (numberp startIndex) (= startIndex 1)))))
        )
    (when (sk8dev::deepmemq 'ssargtobefilledin action)
      (if (eq (first action) 'script) (setf action (third action)))
      (do-alpha-renaming action 'ssargtobefilledin 'it)
      )
    
    (cond
     ((and typeObj whereFilter)
      (setf ourTest `(and (is-a it ,typeObj) ,wherefilter)))
     (whereFilter
      (setf ourTest wherefilter))
     (TypeObj
      (setf ourTest `(is-a it ,typeObj))))
    (setf action 
          `(script
            ,action
            ,(if multiple
               (if endindex `(if (>= currentCount ,endIndex) then (THROW 'exit-repeat NIL)))
               '(THROW 'exit-repeat NIL))))
    (unless (and (numberp startIndex) (= startIndex 1))
      (setf action `(if (,(if multiple '>= '=) currentCount ,startIndex) then ,action)))
    (if needCurrentCount
      (setf action `(script
                     (SET currentCount TO (+ currentCount 1))
                     ,action
                     )))
    `(script
      (LOCAL ,@(if needOurCount '((ssOurCount 0 (lib "SK8" object sk8::INTEGER))) nil)
             ,@(if needCurrentCount '((currentCount 0 (lib "SK8" object sk8::INTEGER))) nil)
             (ssCurrentCollection ,coll (lib "SK8" object sk8::COLLECTION))
             (ssCurrentState ((lib "SK8" FUNCTION INITIALVISITSTATE) ssCurrentCollection)
                             (lib "SK8" object sk8::VISITSTATE))
             (it NIL (lib "SK8" object sk8::OBJECT)))
      (CATCH 'exit-repeat
        (LOOP 
          (IF (= ssCurrentState (lib "SK8" object UNDEFINED)) then (THROW 'exit-repeat NIL))
          (CATCH 'next-repeat
            (script
             ,@(if needOurCount '((SET ssOurCount TO (+ ssOurCount 1))) nil)
             (SET it TO
                  ((lib "SK8" FUNCTION ELEMENTATVISITSTATE) ssCurrentCollection ssCurrentState))
             ,(if ourTest `(IF ,ourTest then ,action) action)
             (SET ssCurrentState TO ((lib "SK8" FUNCTION SUCCEEDINGVISITSTATE) ssCurrentCollection ssCurrentState)))))))))

#|
;;(java-it "set the fillcolor of every item in the contents of the stage to red")
;;(java-it "set the fillcolor of every item in the contents of the stage to red")
;;(java-it "set the fillcolor of item 2 thru 3 whose fillcolor is black in the contents of the stage to red")
;;(java-it "set the fillcolor of item 2 thru 3 whose fillcolor is black in the contents of the stage to red")
;;(java-it "fillcolor of  item 2 thru 3 whose fillcolor is black in the contents of the stage ")
|#

(defun java-skil-compile-simple-anon-function (form)
  (let ((action (third (second (second (second form)))))
        (actiontype (second (third (second form))))
        resArg)
    (setf form (third form))
    (if (and (eq (first (third form)) 'funcall)
             (eq (second (third form)) 'sscurrentpathexpressioncollectionset))
      (setf (third form) (third (third form))))
    (let* ((mappercall (third (third form)))
           (singlep (eq (first mappercall) 'SKIL::singleitemmapper))
           (startindex (third mappercall))
           (endindex (and (not singlep) (fourth mappercall)))
           (typeObj (if singlep (fourth mappercall) (fifth mappercall)))
           (locs (append (skil-get-key form 'LOCAL nil)
                         (skil-get-key (third form) 'LOCAL nil)))
           (coll (second (assoc 'SKIL::sscurrentpathexpressioncollection locs)))
           (collofone (or (not (listp coll)) (lib-form-p coll)))
           ;;;(setP (second (assoc 'SKIL::sscurrentpathexpressioncollectionset locs)))  this can be ignored!
           (whereFilter (second (assoc 'SKIL::sscurrentwherefilter locs)))
           (dir (second (assoc 'SKIL::sscurrentiterationdirection locs)))
           (loc (second (assoc 'SKIL::ssstartingiterationlocation locs)))
           newform
           )
      ;;For now we can't handle this.
      (unless (equal dir '(quote sk8::after)) 
        (sk8::sendtolog (format nil "Can't translate befores in handler ~a...doing the best i can" cur-handler-name)))
      (unless (and (numberp loc) (= loc 0)) 
        (sk8::sendtolog (format nil "Can't translate path expressions with locations in handler ~a...doing the best i can" cur-handler-name)))
      (cond
       ((and (not whereFilter) (not typeobj) (not actiontype))
        (if singlep
          (java-skil-compile-form `((lib "sk8" function collectionnth) ,coll ,startindex))
          (progn
            (setf coll (java-skil-compile-form coll))
            (unless collofone (setf coll (format nil "(~a)" coll)))
            (format nil "sk8.collectionsubsequence(~a, ~a, ~a)" coll 
                    (java-skil-compile-form startindex) 
                    (java-skil-compile-form endindex)))))
       ((and (equal actiontype '(quote SK8::positioninternal))
             (= startindex 1)
             (eq (first (third wherefilter)) '=))
        (setf coll (java-skil-compile-form coll))
        (unless collofone (setf coll (format nil "(~a)" coll)))
        (format nil "~a.position(~a)" coll 
                (java-skil-compile-form (third (third wherefilter)))))
       ((equal actiontype '(quote set))
        (setf coll (java-skil-compile-form coll))
        (unless collofone (setf coll (format nil "(~a)" coll)))
        (format nil "		~a.setnth(~a, ssvaltosetto);~%" coll 
                (java-skil-compile-form startindex)
                ))
       (t 
        (if (and action (listp action) (eq (first action) 'script))
          (setf resArg "")
          (progn
            (unless action (setf action 'it))
            (if singlep
              (setf resArg (if java-in-set-parameter "" (format nil "		~a res;~%" (if (eq action 'ssourcount) "int" "list")))
                    action `(set ,(or java-in-set-parameter 'res) to ,action))
              (setf action `(insertAtEnd ,action ,(or java-in-set-parameter 'res))
                    resArg (if java-in-set-parameter "" (format nil "		list res = new list();~%"))))))
        (setf newform (java-collection-macro coll
                                             (not singlep)
                                             startIndex
                                             endindex
                                             typeobj
                                             (third whereFilter) 
                                             action))
        (induct-types-on-form newform t)      
        (format nil "~a		{  //---------Special Collection Expression Begins:~%~a~%		}~%"
                resArg
                (java-skil-compile-form newform))
        )
       ))))

;;(java-it "item 1 thru 3 in stage")

;;(untrace java-collection-macro)
;;(java-it "item (the first item in symlist) in multi_token_dictionary_table")
;;(java-it "the first item in symlist")
;;(java-it "insert x into uiselection")
;;(java-it "fillcolor of item 2 thru 3 whose name = bing in uiselection")
;;(java-it "position of x in y")
;;(java-it "insert x after item 1 in uiselection")
;;(java-it "insert x after item 3 in uiselection")
;;(java-it "item 2 thru 3 where it > 3 in {1,2,3}")
;;(java-it "position of item 1 where it > 3 in {1,2,3}")
;;(java-it "set the fillcolor of every item in stage to red")
;;(java-it "item 3 before 2 in uiselection")
;;(java-it "oval 1 in uiselection")
;;(java-it "x = undefined")


;;if it's a named function then we have to setup a global
;;do a deep search for local defs.  
;;if there are any duplicates, do a renaming (where necessary)
;;pull all locals to the top and store in a global


(defun java-skil-compile-named-function (form)
  (let* ((inputs (skil-get-key form 'input nil))
         res handlerObject 
         )
    (if (and inputs (listp (first inputs)) (eq (first (First inputs)) 'sk8::me) (second (first inputs)))
      (setf handlerObject (second (first inputs)))
      (setf handlerObject nil)
      )
    
    (when java-skil-current-function
      (error "can't have nested named functions"))
    (setf java-skil-current-function (first (skil-get-key form 'name t)))
    (setf java-skil-current-arguments inputs)
    (setf java-skil-current-function-locals nil)

    ;;do it
    (setf res (do-java-skil-compile-script form))
    
    (if handlerObject
      (push (list java-skil-current-function res) 
            (gethash (if (listp handlerObject) (fourth handlerObject) handlerObject)
                     java-skil-handler-table))
      (push res java-skil-function-list))

    (setf java-skil-current-function nil)
    (setf java-skil-current-arguments nil)
    (setf java-skil-current-function-locals nil)
    res
    ))

(defun java-skil-compile-simple-remove (coll item)
  (format nil "~a.removeElement(~a);~%" 
          (java-skil-compile-form coll)
          (java-skil-compile-form item))
  )

(defun java-skil-compile-script (form)
  (cond 
   ((skil-get-key form 'name t)
    (java-skil-compile-named-function form))
   ((skil-get-key form 'input nil)
    (java-skil-compile-anonymous-function form))
   
   ;;***** A HACK FOR NOW
   ((and (eq (first (first (skil-get-key form 'LOCAL nil))) 'sscurrentpathexpressionaction)
         (eq (first (third form)) 'script))
    (let* ((wherefilter (and (deep-member 'SK8::removeinternal (skil-get-key form 'LOCAL nil))
                             (third (second (second (second (third (third (third form)))))))))
           (collection (if wherefilter
                         (second (second (second (third form)))))))
      (if (and wherefilter
               (eq (first wherefilter) '=)
               (eq (second wherefilter) 'it))
        (java-skil-compile-simple-remove collection (third wherefilter))
        (java-skil-compile-simple-anon-function form))
    ))
   
   ((and (eq (first (first (skil-get-key form 'LOCAL nil))) 'sscurrentpathexpressioncollection)
         (eq (first (third form)) 'script))
    (java-skil-compile-script `(script (LOCAL (sscurrentpathexpressionaction NIL) (sscurrentpathexpressionactiontype NIL)) 
                                       ,form))
    )
   (t
    (do-java-skil-compile-script form))))

(java-add-skil-primitive-function 'script    #'java-skil-compile-script)

;;___________________________________________________________________________________________________________________

(defparameter java-reserved-words '("INT" "CHAR" "PACKAGE"))
(defparameter java-reserved-syms '(sscurrentcollection ssCurrentState ssisPositive ssCurrentCount ssbyvariable))

(defun skil-java-rename (sym)
  (cond
   ((member (symbol-name sym) java-reserved-words :test #'string=)
    (let ((*package* (symbol-package sym)))
      (read-from-string (format nil "~aVar" (symbol-name sym)) nil nil)
      ))
   ((memq sym java-reserved-syms)
    (let ((*package* (symbol-package sym)))
      (read-from-string (symbol-name (gensym (symbol-name sym))) nil nil)
      ))
   (t
    sym)
   ))


(defun do-java-compilation (form)
  (let* ((*compile-with-debugging* nil)
         (generate-readable-code t)
         (newform (skil-alpha-rename form 'skil-java-rename)))
    (induct-types-on-form newform)
    (java-skil-compile-form newform)
    ))

(defun java-it (str)
  (let* ((*compile-with-debugging* nil)
         (generate-readable-code t))
    (setup-java-skil-globals)
    (do-java-compilation 
     (second (fifth (sk8::translatescriptcommand (sk8::targetproject sk8::ui)  str ))))))

(defun convert-to-java (myfiles &optional (outputPath "sk8;"))
  (progn
    (setf outputPath (sk8dev::process-potential-file outputPath))
    (unless (directory-pathname-p outputPath) (error "Output Path must be a directory path"))
    (setup-java-skil-globals)
    (unless (listp myfiles) (setf myfiles (list myfiles)))
    (dolist (myfile myfiles)
      (sk8::sendtolog (format nil "Converting ~a.~%" myfile))
      (setf myfile (sk8dev::process-potential-file myfile))
      (let ((*compile-with-debugging* nil)
            (sk8::maintaintypetables nil)  ;;;don't want to mess up the type info we already have from loading it
            (str "")
            curentry
            res)
        (with-open-file (instream myfile)
          (loop 
            (multiple-value-bind (newstr end-p) (read-line instream nil nil)
              (setf str (concatenate 'string str (string #\newline) newstr))
              (when (or end-p (not newstr)) (return))
              ))
          )
        (sk8dev::setupParsingProjectVariables (sk8::targetproject sk8::ui))
        (setf res (skil::SK8Script-Compile str t))
        (dolist (i res)
          (setf curentry (do-java-compilation i))
          (when (and (listp i)
                     (neq (first i) 'script)
                     (not (and (lib-form-p (first i))
                               (memq (fourth (first i)) '(sk8::new 
                                                          sk8::addproperty
                                                          sk8::addAbstractPropertyType
                                                          sk8::tagpart
                                                          )))))
            (setf java-skil-setup-list (nconc java-skil-setup-list (list (list i curentry))))))
        ))
    (output-current-java-translation outputPath)
    (setf generate-list-of-globals-p nil)
    (setf generate-readable-code nil)
    t
    ))


#|

(get-property-type 'testproj::pipo 'testproj::hud)
(inspect object-type-table)
(symbol-package 'max)

(java-it "max(1,2)")

(untrace)

(convert-to-java "sk8;ssss:parser basics.sk8" "sk8;tempdir:")
(convert-to-java "sk8;jim.sk8")
(convert-to-java "sk8;test.sk8" "sk8;tempdir:")
(convert-to-java "sk8;temp.sk8" "sk8;tempdir:")
(convert-to-java "sk8;organizers:source java port:findericonview.sk8" "sk8;tempdir:")
(gc)

(convert-to-java (list "sk8;ssss:parser basics.sk8" "sk8;ssss:splice.sk8"  "sk8;ssss:lex.sk8")
                 "sk8;tempdir:")


(dolist (i (list "sk8;ssss:splice.sk8" "sk8;ssss:parser basics.sk8" "sk8;ssss:lex.sk8"))
  (sk8dev::loadAndEvalScriptFile i sk8::testproj))


(defun skil-it (str)
  (let ((*compile-with-debugging* nil))
    (pprint (second (fifth (sk8::translatescriptcommandorexpr sk8::sk8 
                                                              str
                                                              ))))))

(java-it "on foo
        local xxx (an integer)
        set xxx to 10 + 10
end foo")

handler-locals

(sk8::translatescriptcommand sk8::testproj  "new rectangle with objectname \"hud\"")
(sk8::translatescriptcommand sk8::sk8 "global glozz = 1")
(skil-compile-form (second (fifth (sk8::translatescriptcommand sk8::sk8 "global glozz = 1"))))
(java-it "global glozz = \"haha\"")
(java-it "fillcolor of my framecolor")
(java-it "return 3")
(java-it "if xx <> y then beep()")
(java-it
 "on foo of mylist (a list)
   repeat with i in mylist
sendtolog of i
end repeat
end  foo" )

(get-handler-type 'organizers::cbu 'sk8::fillcolor)
(get-handler-type 'organizers::oval 'sk8::fillcolor)
(skil-it "set item 1 in foo to bar")
(java-it "set item 1 in foo to bar")

|#




#|
	Change History (most recent last):
	3  	 7/12/96	Brian   	wahoo, simple translations work.
	5  	 8/ 5/96	Brian   	More fixups.  Still have to do anonymous functions.
	8  	 8/ 8/96	Brian   	First pass at basic path expressions.
	12 	 8/13/96	Brian   	Fixing quote.
	16 	 8/20/96	Brian   	Adding type casting.
	19 	 8/28/96	Brian   	adding comments
	30 	 9/19/96	Brian   	
	31 	 9/23/96	Brian   	
	32 	 9/23/96	Brian   	
	33 	 9/24/96	Brian   	
	34 	 9/27/96	Brian   	
	35 	 9/30/96	Brian   	
	36 	 9/30/96	Brian   	
	37 	10/ 7/96	Brian   	
	38 	10/ 7/96	Brian   	
	39 	10/ 8/96	Brian   	Fixing property value output.
	40 	10/ 8/96	Brian   	
	41 	10/10/96	Brian   	
	42 	10/10/96	Brian   	Fixed Tags Output.
	43 	10/11/96	Brian   	Fixing tag output.
	44 	10/11/96	Brian   	
	45 	10/14/96	Brian   	
	46 	10/22/96	Hernan  	Error checking for convert to java.
	47 	10/22/96	Hernan  	Fixing typo
	48 	10/22/96	Brian   	sending things to the log rather than the listener
	49 	 2/27/97	Hernan  	
	50 	 3/ 6/97	sidney  	remove reference to package-store package which is no longer used
|# ;(do not edit past this line!!)
