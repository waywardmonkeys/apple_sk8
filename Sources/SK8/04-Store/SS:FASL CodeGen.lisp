;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Dev)

;;;;----------------------------------------------------------------------------------
;;;------Helper Functions----------
;;;;----------------------------------------------------------------------------------

;;;A macro which simply pushes an element on the END of a list rather than the beginning.
(defmacro AddToEnd (l el)
  `(setf ,l (nconc ,l (list ,el))))


;;This function is called to add a new form to the stream of forms to be compiled.
;;if memory gets too low it calls flush forms which will compile the forms we've done
;;so far and free up the memory so we can continue.
;;;***Note the 10000 byte check is arbitrary.  We should be smarter about that...
(defun AddFASLForm (cg form)
  (AddToEnd (outp cg) form)
  (when (and (not (asLisp cg)) (< (CCL::%freebytes) 10000))  ;;;for testing (> (length (outp cg)) 50)
    (FlushForms cg :done nil))
  )

;;;This function will flush the fasl-codegenerators forms out to disk by compiling them.
;;;if the "done" keyword is not true then we will compile all the forms to a temporary file
;;;and then do a gc to free up the memory.
;;;if the "done" keyword is true then we will compile all the forms left, and if there have
;;;been any temporary files, we concatenate all of them together to make our final save.

(defun FlushForms (cg &key (done nil))
  (cond
   ((and done (not (tempfiles cg)))
    (with-compilation-unit ()
      (ccl::%compile-forms
       (outp cg)
       nil
       (outputFile cg)
       nil
       :overwrite nil))
    )
   (t
    (let (newfile)
      ;;generate a temporary file that doesn't exist...
      (loop
        (setf newfile (name (gensym "TempStoreFile")))
        (unless (probe-file newfile) (return)))
      ;;create the file to dump into.
      (with-open-file (*fasdump-stream* newfile :direction :output
                                        :element-type 'base-character ; huh
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :external-format #.(ccl::make-keyword (pathname-type *.fasl-pathname*
                                                                                             :case :common)))
        (declare (ignore *fasdump-stream*))
        )
      (push newfile (tempfiles cg))
      ;;(print (list "making fasl: " newfile))
      ;;compile the forms...
      (with-compilation-unit ()
        (ccl::%compile-forms
         (outp cg)
         nil
         newfile
         nil
         :overwrite t))
      ;;now let go of the forms and gc to free the memory.
      (setf (outp cg) nil)
      (gc)
      )))
  (when (and done (tempfiles cg))
    ;;This is the case where we are done, but there are temp files.
    ;;we concatenate the temp files to the outputfile and then kill the temp files.
    ;;(print (list "concatenateing:" (reverse (tempfiles cg))))
    (fasl-concatenate (outputFile cg) (nreverse (tempfiles cg)) :if-exists :append)
    (mapc #'delete-file (tempfiles cg))
    )
  )

;;This generates a sk8script version of a name that is sure to be
;;rereadable regardless of the package.
(defmethod legal-sk8script-name-string ((me symbol) proj)
  (let* ((name (pretty-symbol-name me))
         (projpkg (package proj))
         (pkgname (when (neq me (find-symbol (symbol-name me) projpkg))
                    (package-name (symbol-package me)))))
    (when pkgname
      (dolist (nick (package-nicknames (symbol-package me)))
        (when (< (length nick) (length pkgname))
          (setf pkgname nick))))
    (cond (pkgname
           (concatenate 'string "|(" pkgname ")" name "|"))
          ((not (legal-ident-name? name))
           (concatenate 'string "|" name "|"))
          (t name))))

;;This gets a symbol for an object that has an objectname.
;;if this ever is the source of errors, see the note in GEN-NAME-STRING of SK8Script-object-info-class
(defun sym-from-obj (obj)
  (let ((*package* (package (project obj))))
    (read-from-string (objectname obj))))


;;;_____________________________________________________________________________________________
;;;_____________________________________________________________________________________________
;;;_____________________                      SK8 Store CodeGen Object                  ________________________________
;;;_____________________________________________________________________________________________
;;;_____________________________________________________________________________________________
;;;This is the prototype that both fasl and sk8script code generators are children of which encapsulates all of their shared
;;;;functionality.
;;;_____________________________________________________________________________________________


(defvar *load-file-anonymous-index*)

;;----------------------------------------------------------------------------------
;;These are our prototype class definitions.
(defclass SK8Store-CodeGenerator (CodeGenerator)
  ())

(defclass SK8Script-object-info-class (CodeGenerator-SK8-object-info-class)
  ())

(defmethod Get-object-info-maker ( (cg SK8Store-CodeGenerator) )
  #'(lambda (obj num)
      (make-instance 'SK8Script-object-info-class
        :sk8-object obj
        :creation-order-number num)
      )   )


;;----------------------------------------------------------------------------------
;;These next three methods are pretty bogus, so we make sure they do nothing.
(defmethod gen-splash-screen ( (cg SK8Store-CodeGenerator) )
  T ;; no code
  )
(defmethod gen-sound-loop ( (cg SK8Store-CodeGenerator) )
  T ;; no code
  )
(defmethod add-custom-codegen-action ((obj SK8::Object)
                                           obj-name 
                                           (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                           (cg SK8Store-CodeGenerator))
  (declare (ignore obj obj-name SK8-info-rec cg))
  T
  )


;;----------------------------------------------------------------------------------
;;This is where the bulk of the work is done.   This goes through and generates
;;code for the project object and handlers and functions and variables.  both
;;sk8script and fasl generators use this method and specialize it's hooks
(defmethod DoClose ( (cg SK8Store-CodeGenerator) main-fn-or-nil)
  (declare (ignore main-fn-or-nil))
  (let* ((proj (project cg))
         (projname (gen-value-string proj cg))) ;;maybe make this more general...
    
    ;; Generate code for out-of-project objects
    (dolist (obj (reverse (out-of-project-objects cg)))
      (DoGenCode cg (gethash obj (order-table cg) nil))
      )
    
    ;; Generate code for project properties
    (gen-local-prop-inits proj projname
                          (filtered-local-properties proj) 
                          cg)
    (gen-real-prop-inits proj projname nil 
                         (filtered-local-properties proj) 
                         ;;nil nil 
                         cg)
    
    ;; Generate code for project functions
    (gen-functions cg)
    
    ;; Generate code for project constants and variables
    (dolist (i (constants proj))
      (gen-var i nil cg))
    (dolist (i (globals proj))
      (gen-var i t cg))

    ;;;Here we make code for all of the eq junk...
    (gen-special-prop-forms cg)

    ;; Generate code for project handlers
    (gen-object-handlers proj projname cg)

    ;; Generate code for object handlers
    (mapProjectObjects 
     proj
     #'(lambda (obj)
         (let ((objrec (gethash obj (order-table cg) NIL)))
           (when objrec
             (gen-object-handlers obj (gen-handler-obj-name objrec cg) cg)))))
    

    ;; Generate cleanup code
    (gen-cleanup-code cg)

    ;; Clean up the code generator object so it doesn't hold on to things...
    (reinitialize-codeGenerator cg)
    
    T
    ) )


;;----------------------------------------------------------------------------------
;;These next four methods are the hooks into doClose to do various output.
;;the sk8script and fasl codegen specialize these.
(defmethod gen-cleanup-code ( (cg SK8Store-CodeGenerator))
  (error "THIS VERSION SHOULDN'T BE CALLED"))

(defmethod gen-functions ( (cg SK8Store-CodeGenerator))
  (error "THIS VERSION SHOULDN'T BE CALLED"))

(defmethod gen-var (name var? (cg SK8Store-CodeGenerator))
  (declare (ignore name var?))
  (error "THIS VERSION SHOULDN'T BE CALLED"))

(defmethod gen-object-handlers (obj name-string (cg SK8Store-CodeGenerator) )
  (declare (ignore obj name-string targetproj fastOutput))
  (error "THIS VERSION SHOULDN'T BE CALLED"))


;;----------------------------------------------------------------------------------
;;These next two methods make sure we generate setvalues for all of the appropriate
;;properties.
(defmethod gen-real-prop-inits ((obj SK8::Object)
                                    obj-name 
                                    SK8-info-rec 
                                    real-propnames 
                                    (cg SK8Store-CodeGenerator))
  (let* ((false-props   (sk8::propertiesToSaveAsFalse   obj))
         (special-props (sk8::propertiesToSaveSpecially obj))
         (normal-props  (set-difference real-propnames (append false-props special-props '(knownchildren))))
         props)
    
    (dolist (prop-name special-props)
      (push (cons prop-name (multiple-value-list (saveToScript obj prop-name)))
            props))
    (dolist (prop-name normal-props)
      (push (list prop-name (getValue prop-name obj))
            props))
    (dolist (prop props)
      (let ((prop-name  (first  prop))
            (prop-value (second prop))
            (useset-p   (third  prop)))
        (gen-one-real-prop-init obj obj-name SK8-info-rec prop-name prop-value useset-p cg)))
    ))

;;; Returns whether a property value needs saving. This is true if:
;;; (1) the property value is not the parent's default value OR
;;; (2) the creation order of the value is less than the creation order of the parent. 
;;; This means that the object the property refers to has not been installed in the parent when the object
;;; is made, meaning that the default value will not be copied! What we have to do, is set this value anyway
;;; and delay the setting till the object exists. 

(defun prop-value-needs-saving? (obj prop-name prop-value default-value cg)
  (cond ((neq default-value prop-value) t)
        (t 
         ;; It is the default value but it has not been set in the parent yet...
         (let ((parent (mf::find-parent-slot-value obj prop-name nil t))
               sk8-info-rec)
           (when (eq (project obj) (project parent))
             (setf sk8-info-rec (gethash parent (order-table cg)))
             (when sk8-info-rec
               (not-immediately-settable? prop-value sk8-info-rec cg)))))))

(defmethod gen-one-real-prop-init ((obj SK8::Object)
                                       obj-name 
                                       SK8-info-rec
                                       prop-name
                                       prop-value
                                       useset-p
                                       (cg SK8Store-CodeGenerator))
  (declare (ignore obj-name))
  (let ((default-value (mf::find-parent-slot-value obj prop-name nil))
        delaying-obj)
    (if (or (eq (specialproptable cg) 'eq)
            (and (prop-value-needs-saving? obj prop-name prop-value default-value cg)
                 (not (memq prop-name (gethash obj (specialproptable cg))))
                 (not (ccl::macptrp prop-value))))  ;; supress references to a macptr
      (if (or (null SK8-info-rec) (null (setf delaying-obj (not-immediately-settable? prop-value SK8-info-rec cg))))
        (gen-setter prop-name useset-p (gen-value-string obj cg) (gen-value-string prop-value cg) cg)
        (delay-setting-self obj prop-name delaying-obj prop-value useset-p cg)))
    ))

;;;__________________________________________________________________________________________________________________
;;;__________________________________________________________________________________________________________________
;;;__________________________________  Save as Fasl and Save as Sk8script CodeGen Classes     _____________________________________
;;;__________________________________________________________________________________________________________________
;;;__________________________________________________________________________________________________________________

;; public defs

(defclass SK8Script-CodeGenerator (SK8Store-CodeGenerator)
  ())

(defclass FASL-CodeGenerator (SK8Store-CodeGenerator)
  ((asLisp :accessor asLisp :initarg :asLisp :initform 'nil)
   (outputFile :initarg :outputFile)
   (tempFiles :accessor tempFiles :initarg :tempFiles :initform 'nil)
   ))

(defmethod outputFile (:method-class mf::sk8-reader-method) ((me SK8Store-CodeGenerator) &key)
           (slot-value me 'outputFile))

(defmethod (setf outputFile) (:method-class mf::sk8-writer-method) (newValue (me SK8Store-CodeGenerator) &key)
           (setf (slot-value me 'outputFile) newValue))

(defconstant SK8Script-Codegen-version "0.03")

(defconstant FASL-Codegen-version "0.01")

(defconstant SK8-anonymous-objects-vector-name 
  "___anonymous_objects___")  ;; pick an unlikely name...

;;;We use these variables while loading to have a reference to anonymous objects.
(define-sk8-var ____anonymous_objects_vector____ :initial-value nil)
(defparameter FASLVector '____anonymous_objects_vector____)


;;----------------------------------------------------------------------------------
;;;***********
;;;Here is the code for headers.  These are different because the fasl version makes a project
;;;while the sk8script one assumes that project is already made.  The reason it assumes this is
;;;that this text file cannot serve as a "project file".  This should probably be fixed and changed.
(defmethod gen-header-info ((cg SK8Script-CodeGenerator))
  (let* ((outstrm (outp cg))
         (proj (project cg))
         (projname (objectname proj))
         (parproj (objectname (requiredProject proj))))
    
    ;; "header" info for project == create the project object
    (format outstrm 
            "~%~%-- ******** V V V ******** START ******** V V V ********~%")
    (format outstrm "~%-- ***********************")
    (format outstrm "~%-- *** SK8 Project: ~a" projname)
    (format outstrm "~%-- ***********************")
    
    (format outstrm "~%~%--  Script Created on ~a at ~a~%" 
            (SK8::datestring SK8::today) (timestring now))
    (format outstrm "~%--  SK8Script code generator version ~a" 
            SK8Script-Codegen-version)
    ;; It is really difficult to change the name of a project and get all the references right, especially if
    ;; a project has a parent project other than SK8.
    (format outstrm
            "~%-- Load this back in by creating a project (menu: File>New)")
    (format outstrm
            "~%-- and then using the menu: File>Load Script File... ~%" )
    (format outstrm
            "~%-- The following statement warns if the project name is not the same as the one that was saved ~%" )
    ;; This causes the script to check for proper project names
    (format outstrm
            "~%if (((name(TargetProject(UI)) is not equal to ~s) or Â
     (name(requiredProject (TargetProject(UI))) is not equal to ~s)) and Â
    (yesOrNoDialog of \"This script expects the current project to be named ~a and for it to be a subproject of ~a. Do you want to continue loading anyway?\" Â
       with width 400 with notext \"Cancel\") = False) then Â
   AbortBreak()"
            projname parproj projname parproj)
    
    (format (outp cg) "~%clearSystemStateForProjectLoad()~%")
    (print-separator-line outstrm)
    
    ))

(defmethod gen-header-info ((cg FASL-CodeGenerator))
  ;;;BEFORE WE BEGIN, CHECK MEMORY  Note that 30K is somewhat arbitrary.  see flushforms
  (if (and (not (aslisp cg)) (< (CCL::%freebytes) 30000))
    (error "Not enough memory to save!!!  We need at least 30K"))

  (let* ((swapfile nil)  ;;; swapfile is always nil so it will get created automatically...
         (proj (project cg))
         (projname (objectname proj))
         ;;(parproj (objectname (requiredProject proj)))
         )
    
    (AddFASLForm cg '(sk8dev::in-package :sk8dev))
    ;; This causes the script to check for proper project names
    (AddFASLForm cg
              `(CreateNewProjectForLoad ,projname
                                        *load-file-namestring*
                                        ,swapfile
                                        ,(gen-value-string (requiringprojects proj) cg)
                                        ',(gen-value-string (requiredproject proj) cg)))
    
    (AddFASLForm cg `(setf (file ,(gen-value-string proj cg)) (new file :project sk8 :ospathname *load-file-namestring*)))
    (AddFASLForm cg '(sk8dev::clearSystemStateForProjectLoad))
    ))



;;----------------------------------------------------------------------------------
;;;The following two functions do the same thing.  The fasl code is simply the translation of the sk8script.
;;;The following comment applys to both.
;;@@@ Note: We don't know how many slots to allocate at this point.
;;@@@ Obvious options are to [1] generate code at end in a separate file
;;@@@ which gets loaded first, [2] make 2 passes over the objects, or
;;@@@ [3] have the project walker find out and tell us (preferred).
;;@@@
;;@@@ NB: This is for the initial value.  We need to extend the vector to
;;@@@ handle reference for prim composits: Lists, Vectors, and Arrays.

(defmethod gen-anonymous-vector ( (cg SK8Script-CodeGenerator) )
  ;; Gen code to create the anonymous-objects-vector
  (format (outp cg) "~%~%set ~a to new Vector" sk8-anonymous-objects-vector-name)
  ;; reserve the first slot for a file object that points to the file being loaded
  (format (outp cg) "~%insert False at the end of ~a" sk8-anonymous-objects-vector-name)
  (format (outp cg) "~%set (item ~d in ~a) to new File with physicalName loadFilePhysicalName()"
          (setf *load-file-anonymous-index* (allocate-anonymous-object-index cg))
          sk8-anonymous-objects-vector-name)
  )

(defmethod gen-anonymous-vector ( (cg FASL-CodeGenerator) )
  (let ()
    (setf (anonymous-object-high-water-index cg) 0)
    ;; Gen code to create the anonymous-objects-vector
    (AddFASLForm cg `(sk8dev::setf ,FASLVector (make-array 0 :fill-pointer t :adjustable t)) )
    ;; reserve the first slot for a file object that points to the file being loaded  (this just pushes nil at the end of the vector)
    (AddFASLForm cg `(vector-push-extend sk8dev::nil ,FASLVector)
              )
    (AddFASLForm cg
              `(sk8dev::setf (sk8dev::aref ,FASLVector 
                                           ,(setf *load-file-anonymous-index* 
                                                  (allocate-anonymous-object-index cg)))
                             (new File :project ,(project cg) :physicalName (loadFilePhysicalName)))            
              )
    ))



;;----------------------------------------------------------------------------------
;;This outputs the functions of the project in the apropriate format.
(defmethod gen-functions ( (cg SK8Script-CodeGenerator))
  (sk8::TextDumpProjectFunctions (project cg) (outp cg)))

(defmethod gen-functions ( (cg FASL-CodeGenerator))
  (sk8::DumpFASLProjectFunctions (project cg) cg))

(define-handler SK8::DumpFASLProjectFunctions (project &optional (cg t) (fastOutput t))
  (let* ((projpkg (sk8::package me))
         curFunDef
         outstring funcstr)
    (dolist (name (functions me)) ;; (mapcar #'name (functions me))) Why turn it into a string and loose information??? Ehh?!
      (setf curFunDef nil)
      (setf outstring (make-string-output-stream))
      (sk8::TextDumpProjectFunction me name outstring)
      (setf funcstr (get-output-stream-string outstring))
      (if fastOutput
        (let ((public-p
               (multiple-value-bind (sym-p state) (find-symbol (symbol-name name) projpkg)
                 (declare (ignore sym-p))
                 (eq state :external))))
          (addToEnd curFunDef `(ps::put-function-records ,me ',name 
                                                         ',(gethash name (sk8::function-record-table me))))
          (addToEnd curFunDef `(mf::mark-sym-function ',name))
          (when public-p (addToEnd curFunDef `(mf::publish-project-symbol ',name (find ,(objectName me) (knownChildren project) :key #'objectName :test #'string-equal))))
          (addToEnd curFunDef `(setf (symbol-function ',name) ,(symbol-function name)))
          )
        (progn
          (addToEnd curFunDef 
                    (translatescriptcommandorexpr me funcstr))      
          ))
      (AddFASLForm cg `(withStoreErrorWrap 
                         (progn
                           (sendtolog (format nil "Encountered surprise problem while trying to create the function ~a. ~
                                                   This function was not created.  Ignoring problem and continuing."
                                              ,(ps::pretty-symbol-name name)))
                           (sendtolog (format nil "You may recompile the function text to redefine the function.~
                                                   Note you may have to recompile scripts that referenced this function. ~
                                                   The function text was:"))
                           (sendtolog ,funcstr)
                           (ignore-errors
                            (when (memq ,name (mapcar #'name (functions ,me)))
                              (removefunction  ,me :name ,name))
                            (when (fboundp ,name)
                              (fmakunbound ,name))))
                         ,(cons 'progn curFunDef)))
      )))

;;----------------------------------------------------------------------------------
(defmethod gen-var (name var? (cg SK8Script-CodeGenerator))
  (unless (or (string-equal SK8-anonymous-objects-vector-name name)
              (and (hash-table-p (specialproptable cg))
                   (memq name (gethash 'Variable (specialproptable cg)))))
    (format (outp cg)
            "~%global ~a~a~:[~; = ~a~]"
            (if var? "" "constant ")
            name
            (boundp name)
            (when (boundp name)
              (gen-value-string (symbol-value name) cg)))))

(defmethod gen-var (name var? (cg FASL-CodeGenerator))
  (unless (or (string-equal FASLVector name)
              (and (hash-table-p (specialproptable cg))
                   (memq name (gethash 'Variable (specialproptable cg)))))
    (AddFASLForm cg
              `(CreateNewVariableFromStore ',name 
                                           ,(if (boundp name)
                                              (gen-value-string (symbol-value name) cg)
                                              nil) 
                                           ,(not var?)
                                           ,(project cg)))
    ))



;;----------------------------------------------------------------------------------

(defmethod gen-eq-set-form (val anonID (cg FASL-CodeGenerator))
  (AddFASLForm cg 
               ;; `(sk8dev::insertBeforeCurrentItem ,FASLVector sk8dev::nil sk8dev::nil)
               `(vector-push-extend sk8dev::nil ,FASLVector)
               )
  (AddFASLForm cg 
               `(sk8dev::setf ,(gen-anon-name-string anonID cg)
                              ,(gen-value-string val cg)
                              ))
  )

(defmethod gen-eq-set-form (val anonID (cg SK8Script-CodeGenerator))
  (format (outp cg) 
          "~%~%insert False at the end of ~a ~%set ~a to ~a~%"
          sk8-anonymous-objects-vector-name  ;; grow vector
          (gen-anon-name-string anonID cg) 
          (gen-value-string val cg)
          ))

;;----------------------------------------------------------------------------------

(defmethod gen-cons-value-string (l thecdr (cg FASL-CodeGenerator))
  (let ((firstpart (copy-list l)))
    (setf (nthcdr (- (length l) (length thecdr)) firstpart) nil)
    `(ccl::nconc ,(gen-value-string firstpart cg)
                 ,(gen-value-string thecdr cg)
                 )))

(defmethod gen-cons-value-string (l thecdr (cg SK8Script-CodeGenerator))
  (let ((firstpart (copy-list l)))
    (setf (nthcdr (- (length l) (length thecdr)) firstpart) nil)
    (format nil 
            "(|(ccl)nconc|(~a, ~a))"
            (gen-value-string firstpart cg) 
            (gen-value-string thecdr cg)
            )))



;;----------------------------------------------------------------------------------
(defmethod gen-handler-obj-name ( (obj-info-rec SK8Script-object-info-class)
                                      (cg FASL-CodeGenerator) )
  (if (anonymous-object-vector-index obj-info-rec)
    (format NIL 
            "(item ~d in ~a (in project sk8))"
            (1+ (anonymous-object-vector-index obj-info-rec))
            (symbol-name FASLVector))
    (let ((obj (sk8-object obj-info-rec))
          (targetproj (project cg)))
      (if (eq obj targetproj)
        "(TargetProject(UI))"
        (objectstring obj :project targetproj)))))


(defmethod gen-handler-obj-name ( (obj-info-rec SK8Script-object-info-class)
                                      (cg SK8Script-CodeGenerator) )
  (gen-name-string obj-info-rec cg))



;;----------------------------------------------------------------------------------

(defmethod gen-object-handlers (obj name-string (cg SK8Script-CodeGenerator))
  (SK8::GenerateSK8ScriptForHandlers obj name-string (project cg) (outp cg))
  )

(defmethod gen-object-handlers (obj name-string (cg FASL-CodeGenerator))
  (SK8::GenerateFASLForHandlers obj 
                                name-string
                                (project cg)
                                cg)
  )

(define-handler SK8::GenerateFASLForHandlers (object name-string targetproj cg &optional (fastOutput t))
  (when (mf::object-class-p me)
    (let ((anonymous (null (objectname me)))
          handstr
          handler-name
          handler-name-string
          handler-obj
          handler-obj-string
          curHandDef
          outstring)
      (if (eq me targetproj) (setf name-string (objectname me)))
      (dolist (h (localhandlers me))
        (setf curHandDef nil)
        (setf outstring (make-string-output-stream))
        (sk8::GenerateSK8ScriptForHandler me h name-string targetproj anonymous outstring)
        (setf handstr (get-output-stream-string outstring))
        (setf handler-name (ccl::%method-name h) )
        (setf handler-name-string (gen-value-string handler-name cg))
        (setf handler-obj me)
        (setf handler-obj-string (gen-value-string me cg))
        (if fastOutput
          (let* ((handler-qual (method-qualifiers h) )
                 (handler-qual-string (gen-value-string handler-qual cg) ))
            (addToEnd curHandDef `(ps::put-handler-records ,handler-name-string 
                                                           ,handler-obj-string
                                                           ,handler-qual-string
                                                           ',(ps::find-handler-records handler-name 
                                                                                       handler-obj
                                                                                       (method-qualifiers h))))
            (addToEnd curHandDef `(unless (eq (type-of ,h) 'standard-method) 
                                    (print (list "WARNING: LOADING A NON SK8 HANDLER:" ,(objectstring h)))))
            (when (symbolp handler-name)
              (addToEnd curHandDef
                        `(,(if (mf::private-property-p me handler-name)
                             'mf::make-private-property
                             'mf::make-public-property)
                          ,handler-obj-string
                          ,handler-name-string))))                      
          (progn
            (addToEnd curHandDef 
                      (translatescriptcommandorexpr targetproj handstr))
            ))
        (AddFASLForm cg 
                     `(withStoreErrorWrap 
                        (progn
                          (sendtolog (format nil "Encountered surprise problem while trying to create the handler ~a.  This handler was not created.  Ignoring problem and continuing."
                                             ,(objectstring h)))
                          (sendtolog  (sendtolog "You may recompile the handler text to redefine the handler.  Note you may have to recompile scripts that referenced this handler.  The handler text was:"))
                          (sendtolog ,handstr)
                          (ignore-errors 
                           (when (MACFRAMES::find-local-handler ,handler-name-string ,handler-obj-string)
                             (removehandler  ,handler-obj-string :name ,handler-name-string)
                             
                             ))
                          )
                        ,(cons 'progn curHandDef)))
        )
      )))


;;----------------------------------------------------------------------------------

(defmethod gen-cleanup-code ( (cg SK8Script-CodeGenerator))
  (let ((outport (outp cg))
        (proj (project cg)))
    
    ;; Gen code to eliminate the storage for anonymous-objects-vector
    (format outport "~%set ~a to false" sk8-anonymous-objects-vector-name)
    
    (format outport "~%finalizeEnvironment(targetProject(UI))~%")
    
    (when *%%CodeGen-Debug%%*
      (format (warnp cg) "~%-- Close complete for project ~s" proj))
    
    (format outport 
            "~%~%-- ******** E O F ******** E O F ******** E O F ********~%")  
    
    ))

(defmethod gen-cleanup-code ( (cg FASL-CodeGenerator))
  (let ((proj (project cg)))
    
    ;; Gen code to eliminate the storage for anonymous-objects-vector
    (AddFASLForm cg `(sk8dev::setf ,FASLVector sk8dev::nil))
    
    (AddFASLForm cg `(sk8dev::finalizeEnvironment ,proj))
    
    (funcall (if SS::!*lisp-dev-mode* #'print #'sendToLog)  ;; this is really just for debugging
             (format nil "Writing ~a ..." (objectname proj)))
    
    ;; Now we have the forms, do the actual output (should turn this into a streaming model someday)
    (if (asLisp cg)
      (let ((*package* (find-package :sk8dev)))
        (with-open-file (strm (outputFile cg) :direction :output :if-does-not-exist :create :if-exists :append)
          (dolist (i (outp cg))
            (pprint i strm)  ;;pprint is not exactly right to ensure readability, just here as a first cut
            ))
        )
      (FlushForms cg :done t))
    ))




;; --------------------- INTERNAL HELPER FUNCTIONS ---------------------

;;----------------------------------------------------------------------------------

;; plain vanilla generate the "new" ststement, with or without initialization

(defun basic-Gen-New-Function (obj obj-name parent cg skipinit)
  (let* ((pname (objectName parent))
         (myname (objectname obj))
         (parent-name
          (if pname
            pname
            (if (eq (sk8::project parent) (sk8::project cg))
              (gen-name-string (gethash parent (order-table cg) NIL) cg)
              (gen-value-string parent cg))))
         (skipinitstring (if skipinit " with skipInitialization " " ")))
    (if myname
      (format (outp cg) "~%~%new ~a with objectname \"~a\"~a" parent-name myname skipinitstring)
      (format (outp cg) 
              "~%~%insert False at the end of ~a ~%set ~a to (new ~a~a)"
              sk8-anonymous-objects-vector-name  ;; grow vector
              obj-name 
              parent-name
              skipinitstring
              ))))

(defun basic-FASLGen-New-Function (obj obj-name parent cg skipinit)
  (let* ((pname (objectName parent))
         (myname (objectname obj))
         (otherparents (cdr (reverse (parents obj))))
         (parent-name
          (if pname
            (sym-from-obj parent)
            (if (eq (sk8::project parent) (sk8::project cg))
              (gen-name-string (gethash parent (order-table cg) NIL) cg)
              (gen-value-string parent cg)))))
    (setf otherparents (mapcar #'(lambda (x) (gen-value-string x cg)) otherparents))
    (when otherparents (push 'list otherparents))
    (if myname
      (AddFASLForm cg 
                `(CreateNewObjectFromStore ,parent-name :project ,(sk8::project cg) :objectname ,myname :otherparents ,otherparents
                                           :skipInitialization ,skipinit))
      (progn
        (AddFASLForm cg 
                     ;; `(sk8dev::insertBeforeCurrentItem ,FASLVector sk8dev::nil sk8dev::nil)
                     `(vector-push-extend sk8dev::nil ,FASLVector)
                     )
        (AddFASLForm cg 
                  `(sk8dev::setf ,obj-name
                                 (CreateNewObjectFromStore ,parent-name :project ,(sk8::project cg)  :otherparents ,otherparents
                                                           :skipInitialization ,skipInit)))
        ))))

; for GEN-OBJECT-MAKER 
(defmethod gen-new ((obj SK8::Object) obj-name parent (cg SK8Script-CodeGenerator))
  (basic-Gen-New-Function obj obj-name parent cg t))
(defmethod gen-new ((obj SK8::Object) obj-name parent (cg FASL-CodeGenerator))
  (basic-FASLGen-New-Function obj obj-name parent cg t))



;;----------------------------------------------------------------------------------

(defmethod gen-new ((obj _Media_class) obj-name parent (cg SK8Script-CodeGenerator))
  (let (trans)
    (if (or
         (null (resourceid obj))
         (neq :project (getvalue 'file obj))
         (null (setf trans (find obj (knowndescendants translator)
                                 :test #'(lambda(a b)
                                           (is-a a (finalobject b)))))))
      (call-next-method)
      (progn
        (basic-Gen-New-Function obj obj-name parent cg nil)
        (format (outp cg)
                "~%import ~a with source (new ~a with file (item ~d in ~a) with resourceId ~a) with destination ~a"
                (gen-value-string trans cg) (gen-value-string (finalobject trans) cg)
                *load-file-anonymous-index*
                sk8-anonymous-objects-vector-name
                (copy-resource-to-a-file (ccl::stream-filename (outp cg))
                                         (macGetResourceHandleFromId (file obj) 
                                                                     (externalType trans) 
                                                                     (resourceId obj)) (externalType trans))
                (gen-value-string obj cg))))))

(defmethod gen-new ((obj _Media_class) obj-name parent (cg FASL-CodeGenerator))
  (let (trans)
    (if (or
         (null (resourceid obj))
         (neq :project (getvalue 'file obj))
         (null (setf trans (find obj (knowndescendants translator)
                                 :test #'(lambda(a b)
                                           (is-a a (finalobject b)))))))
      (call-next-method)
      (progn
        ;; require the translator!
        (AddToEnd (outp cg) (require-preface nil (list trans)))
        ;; now make the object and call import. 
        (basic-FASLGen-New-Function obj obj-name parent cg nil)
        (AddToEnd (outp cg)
                  `(sk8::import ,trans 
                                :source (CreateNewObjectFromStore ,(finalobject trans) 
                                                                  :file (aref ,FASLVector ,*load-file-anonymous-index*) 
                                                                  :resourceId ,(copy-resource-to-a-file 
                                                                                (outputFile cg)
                                                                                (macGetResourceHandleFromId (file obj) 
                                                                                                            (externalType trans) 
                                                                                                            (resourceId obj)) (externalType trans)) 
                                                                  :project ,(project cg)) 
                                :destination ,(gen-value-string obj cg))
                  )))))

;;----------------------------------------------------------------------------------
;;the fasl version doesn't use this as it assumes a simple stream of output.  we move the
;;other parents code for fasls into the gen-new method.

; for GEN-OBJECT-MAKER 
(defmethod gen-other-parents ( (obj SK8::Object) otherparents (cg SK8Script-CodeGenerator) )
  (format (outp cg) " with otherparents ~a " 
          (gen-value-string otherparents cg))
  )
(defmethod gen-other-parents ( (obj SK8::Object) otherparents (cg FASL-CodeGenerator) )
  nil  )


;;----------------------------------------------------------------------------------

;;*****DOES THIS EVER GET CALLED?!?!!?
; for GEN-OBJECT-MAKER 
(defmethod gen-prop-definits  ( prop-name prop-value (cg SK8Script-CodeGenerator) )
  (format (outp cg)
          "Â~%        with ~a: ~a " 
          prop-name
          (gen-value-string prop-value cg))
  )
(defmethod gen-prop-definits  ( prop-name prop-value (cg FASL-CodeGenerator) )
  ;;;;***NEEDS FIXING:  PROBABLY SHOULD BE NCONCED TO THE END OF LAST ITEM OF OUTP CG...
  (error "WHEN DOES THIS GET CALLED?!?! -Brian")
  (AddFASLForm cg
            (list (ccl::make-keyword prop-name) prop-value)))


;;----------------------------------------------------------------------------------

; for GEN-OBJECT-MAKER 
(defmethod gen-local-prop-inits ((obj SK8::Object)
                                     obj-name 
                                     local-propnames 
                                     (cg SK8Script-CodeGenerator))
  (dolist (prop-name local-propnames)
    (format (outp cg) "~%addProperty ~a, \'~a\' " obj-name prop-name)
    (when (sk8:propagatableValue obj prop-name)
      (format (outp cg) " with propagatedValue "))
    ))

(defmethod gen-local-prop-inits ((obj SK8::Object)
                                     obj-name 
                                     local-propnames 
                                     (cg FASL-CodeGenerator))
  (declare (ignore obj-name))  ;;we don't care about strings...
  (dolist (prop-name local-propnames)
    (AddFASLForm cg 
              `(sk8dev::addProperty ,(gen-value-string obj cg) ',prop-name 
                                    :propagatedValue ,(if (sk8:propagatableValue obj prop-name) t nil)))))




;;----------------------------------------------------------------------------------

(defmethod GEN-SETTER (property-name useset? obj value-name (cg SK8Script-CodeGenerator))
  (format (outp cg) 
          (if (typep property-name Integer) ; index vs propname
            "~%set (item ~a in ~a) to ~a"
            (if useset?
              "~%set the ~a of ~a to ~a"
              "~%setValue('~a', ~a, ~a)"))   ;;;***Need to fix.
          property-name
          obj
          value-name)
  )

(defmethod GEN-SETTER (property-name useset? obj val (cg FASL-CodeGenerator))
  (AddFASLForm cg 
              (if (typep property-name Integer) ; index vs propname
                `(sk8dev::setf (sk8dev::nth ,property-name ,obj) ,val)
                (if useset?
                  `(sk8dev::setf (,property-name ,obj) ,val)
                  `(sk8dev::SetValueFromStore ,obj ',property-name ,val nil)))  ;;;NEED TO PUT VERSION INFO HERE RATHER THAN NIL!!! ***
              ))



;;----------------------------------------------------------------------------------

(defmethod GEN-NAME-STRING ( (obj-info-rec SK8Script-object-info-class)
                                (cg SK8Script-CodeGenerator) )
  (if (anonymous-object-vector-index obj-info-rec)
    (gen-anon-name-string obj-info-rec cg)
    (let ((obj (sk8-object obj-info-rec))
          (targetproj (project cg)))
      (if (eq obj targetproj)
        "(TargetProject(UI))"
        (objectstring obj :project targetproj)))))

(defmethod GEN-NAME-STRING ( (obj-info-rec SK8Script-object-info-class)
                                (cg FASL-CodeGenerator) )
  ;;note that we assume that all anonymous objects will have an anon-obj-vector-index.
  ;;if this function or sym-from-obj gives an error while saving, it is likely 
  ;;that somehow an anonymous object snuck through without a vector.  check for that...
  (if (anonymous-object-vector-index obj-info-rec)
    (gen-anon-name-string obj-info-rec cg)
    (let ((obj (sk8-object obj-info-rec))
          (targetproj (project cg)))
      (if (eq obj targetproj)
        '(TargetProject UI)
        (sym-from-obj obj)))))


;;----------------------------------------------------------------------------------

(defmethod gen-anon-name-string ( ind 
                                      (cg SK8Script-CodeGenerator) )
  (unless (numberp ind)
    (setf ind (anonymous-object-vector-index ind)))
  (format NIL 
          "(item ~d in ~a)"
          ind
          sk8-anonymous-objects-vector-name)
  )

(defmethod gen-anon-name-string ( ind 
                                      (cg FASL-CodeGenerator) )
  (unless (numberp ind)
    (setf ind (anonymous-object-vector-index ind)))
  `(sk8dev::aref ,FASLVector ,ind)
  )


;;----------------------------------------------------------------------------------

(defmethod GEN-VALUE-STRING ( random-value (cg SK8Store-CodeGenerator) )
  (cond 
   ((and (dontsave random-value) (not (is-a random-value project)))
    (Create-value-string nil cg))
   ((and (or (arrayp random-value) (listp random-value))
         (EqVectorIDTable cg) (gethash random-value (EqVectorIDTable cg)))
    (gen-anon-name-string (gethash random-value (EqVectorIDTable cg)) cg))
   ((and (listp random-value)     ;;;whoa... will this slow us down????*********
         (constables cg)
         (some #'(lambda (x) (gethash random-value x)) (constables cg)) )
    (let (xxx)
      (dolist (i (constables cg))
        (when (gethash random-value i)
          (setf xxx (gethash random-value i))
          ))
      (gen-cons-value-string random-value xxx cg))
    )
   (t
    (Create-value-string random-value cg))
   )
  )

;;;-------------------------------------------------------------------------------------------------


(defmethod CREATE-VALUE-STRING ((random-value sk8::object) (cg SK8Script-CodeGenerator))
  (let ((probe (gethash random-value (order-table cg) nil)))
    (if probe
      ;; Obj in the project? 
      (gen-name-string probe cg)
      ;; Outside the project: is it named? 
      (let ((oName (objectName random-value)))
        (if oName
          (objectstring random-value :project (project cg))
          ;; Not named... Can we get it through a creation relation?
          (let ((relationName (with-output-to-string (theName)
                                (maybe-write-relation random-value theName))))
            (if (and relationName (not (string= relationName "")))
              (format nil "(~a)" relationName)
              ;; Nope: out of project sk8-object without a name. we have to resort to helpfindmychild to get it
              (let* ((pleaseFindMyChildMethod (ccl::method-exists-p #'pleaseFindMyChild random-value))
                     (namedparent (when pleaseFindMyChildMethod
                                    (mf::safe-class-owner (car (ccl::method-specializers pleaseFindMyChildMethod))))))
                (if pleaseFindMyChildMethod
                  (if (objectname namedparent)
                    (format nil "(PleaseFindMyChild (~a, ~a))"
                            (gen-value-string namedparent cg)
                            (gen-value-string (helpFindMyChild random-value) cg))
                    (error "Handler pleaseFindMyChild should not be defined on an unnamed object ~s"
                           random-value))
                  (error "Cannot save ~a, an unnamed object in another project that ~
                          does not define helpFindMyChild." random-value)))
              )))))))

(defmethod CREATE-VALUE-STRING ((random-value sk8::object) (cg FASL-CodeGenerator))
  (let ((probe (gethash random-value (order-table cg) nil)))
    (if probe
      ;; Obj in the project? 
      (gen-name-string probe cg)
      ;; Outside the project: is it named? 
      (let ((oName (objectName random-value)))
        (if oName
          (sym-from-obj random-value)
          ;; Not named... Can we get it through a creation relation?
          (multiple-value-bind (relatedObject relatedProperty) 
                               (find-relation-or-pseudo-relation random-value)
            (if relatedObject
              `(,relatedProperty ,(gen-value-string relatedObject cg))
              ;; Nope: out of project sk8-object without a name. we have to resort to helpfindmychild to get it
              (let* ((pleaseFindMyChildMethod (ccl::method-exists-p #'pleaseFindMyChild random-value))
                     (namedparent (when pleaseFindMyChildMethod
                                    (mf::safe-class-owner (car (ccl::method-specializers pleaseFindMyChildMethod))))))
                (if pleaseFindMyChildMethod
                  (if (objectname namedparent)
                    `(PleaseFindMyChild ,(gen-value-string namedparent cg) 
                                        ,(gen-value-string (helpFindMyChild random-value) cg))
                    (error "Handler pleaseFindMyChild should not be defined on an unnamed object ~s"
                           random-value))
                  (error "Cannot save ~a, an unnamed object in another project that ~
                          does not define helpFindMyChild." random-value)))
              )))))))

(defmethod CREATE-VALUE-STRING ((guy standard-object) (cg SK8Script-CodeGenerator))
  (let ((str (format nil "(MakeInstanceFromStore(~a)" (gen-value-string (class-name (class-of guy)) cg))))
    (dolist (i (ccl::class-instance-slots (class-of guy)))
      (when (and (slot-boundp guy (car i)) (third i))
        (setf str (concatenate 'string str (format nil " with |~a| " (car (third i)))))
        (setf str (concatenate 'string str (gen-value-string (slot-value guy (car i)) cg)))
        ))
    (setf str (concatenate 'string str ")"))
    str
    ))

(defmethod CREATE-VALUE-STRING ((guy standard-object) (cg FASL-CodeGenerator))
  (let ((frm (list 'MakeInstanceFromStore (gen-value-string (class-name (class-of guy)) cg))))
    (dolist (i (ccl::class-instance-slots (class-of guy)))
      (when (and (slot-boundp guy (car i)) (third i))
        (addtoend frm (car (third i)))
        (addtoend frm (gen-value-string (slot-value guy (car i)) cg))
        ))
    frm
    ))

;;----------------------------------------------------------------------------------

(defmethod CREATE-VALUE-STRING ((random-value pathname) (cg SK8Script-CodeGenerator))
  (format nil "(pathname (in project CL) (\"~a\"))" (namestring random-value)))

(defmethod CREATE-VALUE-STRING ((random-value pathname) (cg FASL-CodeGenerator))
  `(ccl::pathname ,(namestring random-value)))


;;----------------------------------------------------------------------------------

(defmethod CREATE-VALUE-STRING ( random-value (cg SK8Script-CodeGenerator) )
  (cond
   ; white out handles, nil -> #f
   ((or (null random-value) (ccl::macptrp random-value)) "False")
   ((structurep random-value)
    (let ((entry (gethash (type-of random-value) ccl::%defstructs%)))
      (if entry
        (let ((maker (elt entry 4))
              (inits (cdr (elt entry 1)))
              (indx 0)
              (namevals nil))
          (dolist (els inits)
            (let ((name (car els))
                  (val (ccl::struct-ref random-value (incf indx))))
              (push (list name (gen-value-string val cg)) namevals)))
          (format nil "(~a() ~:{ with ~a ~a~})"
                  ;; (nsubstitute #\| #\' (objectstring maker :project (project cg)))
                  (legal-sk8script-name-string maker (project cg))
                  (nreverse namevals))
          )
        ;; this is really an error, but here's the best we can do under the circumstances
        (objectString random-value :project (project cg))))) 
   (T ; simple prim
    (objectString random-value :project (project cg)))
   )
  )

(defmethod CREATE-VALUE-STRING ( random-value (cg FASL-CodeGenerator) )
  (cond
   ; white out handles, nil -> #f
   ((or (null random-value) (ccl::macptrp random-value)) nil)
   ((structurep random-value)
    (let ((entry (gethash (type-of random-value) ccl::%defstructs%)))
      (if entry
        (let* ((maker (elt entry 4))
               (inits (cdr (elt entry 1)))
               (indx 0)
               (namevals nil)
               )
          (dolist (els inits)
            (let ((name (car els))
                  (val (ccl::struct-ref random-value (incf indx))))
              (push (ccl::make-keyword name) namevals)
              (push (gen-value-string val cg) namevals)
              ))
          (cons maker (nreverse namevals))
          )
        ;; this is really an error, but here's the best we can do under the circumstances
        random-value))) 
   (T ; simple prim
    random-value
    )))





;;----------------------------------------------------------------------------------
;;; Also generate the text and the styles. 
;;; Returns every line of text as a list of strings.

(defun compile-text-list (buf)
  (let ((lineStart 0)
        (size (buffer-size buf))
        lineEnd result)
    (loop
      (when (<= size lineStart) (return))
      (setf lineEnd (buffer-line-end buf lineStart))
      (when (= (1+ lineEnd) size)
        (when (> lineEnd lineStart)
          (setf result (nconc result (list (buffer-substring buf lineStart lineEnd)))))
        (return))
      (setf result (nconc result (list (buffer-substring buf lineStart lineEnd))))
      (setf lineStart (1+ lineEnd)))
    result))

;;; A list of style runs of the form: {{runStartPos, fontSpec},...}

(defun compile-styles-list (buf)
  (let ((start 0)
        result)
    (loop
      (setf result (nconc result (list (list start (buffer-char-font-spec buf start)))))
      (setf start (buffer-next-font-change buf start))
      (unless start
        (return)))
    result))

;;----------------------------------------------------------------------------------

(defmethod CREATE-VALUE-STRING ((me gs:*sk8-menu-item*) (cg SK8Script-CodeGenerator))
  (let* ((myitem (mf::my-frame me))
         (myowner (menu myitem)))
    (let ((mnitem (gen-value-string myitem cg))
          (ownermenu (gen-value-string myowner cg))
          (itemnumber (gen-value-string (when myowner
                                          (position me (menu-items (slot-value me 'ccl::owner)))) cg))
          (checkedp (gen-value-string (checkmark myitem) cg))
          (cmdkey (gen-value-string (command-key me) cg))
          (txt (gen-value-string (text myitem) cg))
          (enbld (gen-value-string (enabled myitem) cg))
          (style (gen-value-string (menu-item-style me) cg))
          (color-list (gen-value-string (slot-value me 'ccl::color-list) cg)))
      (format nil "(|(sk8)makeSK8MenuItem| (~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a))"
              mnitem ownermenu itemnumber checkedp cmdkey txt enbld style color-list))))

(defmethod CREATE-VALUE-STRING ((me gs:*sk8-menu-item*) (cg FASL-CodeGenerator))
  (let* ((myitem (mf::my-frame me))
         (myowner (menu myitem)))
    (let ((mnitem (gen-value-string myitem cg))
          (ownermenu (gen-value-string myowner cg))
          (itemnumber (gen-value-string (when myowner
                                          (position me (menu-items (slot-value me 'ccl::owner)))) cg))
          (checkedp (gen-value-string (checkmark myitem) cg))
          (cmdkey (gen-value-string (command-key me) cg))
          (txt (gen-value-string (text myitem) cg))
          (enbld (gen-value-string (enabled myitem) cg))
          (style (gen-value-string (menu-item-style me) cg))
          (color-list (gen-value-string (slot-value me 'ccl::color-list) cg)))
      `(sk8::makeSK8MenuItem 
        ,mnitem ,ownermenu ,itemnumber ,checkedp ,cmdkey ,txt ,enbld ,style ,color-list))))


;;----------------------------------------------------------------------------------

(defmethod CREATE-VALUE-STRING ((me gs:*sk8-menu*) (cg SK8Script-CodeGenerator))
  (let* ((myitem (mf::my-frame me))
         (myowner (menu myitem)))
    (let ((mn (gen-value-string myitem cg))
          (ownermenu (gen-value-string myowner cg))
          (itemnumber (gen-value-string (when myowner
                                          (position me (menu-items (slot-value me 'ccl::owner)))) cg))
          (txt (gen-value-string (menu-title me) cg))
          (numitems (gen-value-string (length (menu-items me)) cg)))
      (format nil "(|(sk8)makeSK8Menu| (~a, ~a, ~a, ~a, ~a))" mn ownermenu itemnumber txt numitems))))

(defmethod CREATE-VALUE-STRING ((me gs:*sk8-menu*) (cg FASL-CodeGenerator))
  (let* ((myitem (mf::my-frame me))
         (myowner (menu myitem)))
    (let ((mn (gen-value-string myitem cg))
          (ownermenu (gen-value-string myowner cg))
          (itemnumber (gen-value-string (when myowner
                                          (position me (menu-items (slot-value me 'ccl::owner)))) cg))
          (txt (gen-value-string (menu-title me) cg))
          (numitems (gen-value-string (length (menu-items me)) cg)))
      `(sk8::makeSK8Menu ,mn ,ownermenu ,itemnumber ,txt ,numitems))))


;;----------------------------------------------------------------------------------
(defmethod CREATE-VALUE-STRING ( (L List) (cg SK8Script-CodeGenerator) )
  (cond
   ((null L)
    "False")
   ((and (cdr l) (not (consp (cdr l))))
    (format nil "(|(ccl)cons|(~a, ~a))" (gen-value-string (car l) cg) (gen-value-string (cdr l) cg)))
   (t
    (let ( (outp (make-string-output-stream)) )
      (SK8DEV::write-list
       L 
       outp 
       T 
       #'(lambda (elt strm ignored-reReadably)
           (declare (ignore ignored-reReadably))
           (format strm "~a" (gen-value-string elt cg))
           )
       )
      (get-output-stream-string outp)
      ))))

(defmethod CREATE-VALUE-STRING ( (L List) (cg FASL-CodeGenerator) )
  (cond
   ((null L)
    l)
   ((and (cdr l) (not (consp (cdr l))))
    `(cons ,(gen-value-string (car l) cg) ,(gen-value-string (cdr l) cg)))
   (t
    (cons 'list (mapcar #'(lambda (x) (gen-value-string x cg)) l))
    )))


;;----------------------------------------------------------------------------------

(defmethod CREATE-VALUE-STRING ((S String) (cg SK8Script-CodeGenerator))
  (objectString S :project (project cg)))

(defmethod CREATE-VALUE-STRING ( (s string) (cg FASL-CodeGenerator) )
  s)


;;----------------------------------------------------------------------------------


(defmethod CREATE-VALUE-STRING ((A Array) (cg SK8Script-CodeGenerator))
  ; code hacked from writeObject
  (let ((outp (make-string-output-stream))
        (writeObjectRecursionLimit writeObjectRecursionLimit)
        (*collectionsBeingWritten* (cons A *collectionsBeingWritten*))
        (fill-p (array-has-fill-pointer-p A))
        (adj-p (adjustable-array-p A))
        (dimensions (array-dimensions A))
        (datadims (array-dimensions A)))
    (declare (dynamic-extent *collectionsBeingWritten*))
    (when fill-p
      (setf datadims (list (fill-pointer A)))
      (when adj-p
        (setf dimensions datadims)))
    (format outp "(|(cl)make-array| (~a) "
            (gen-value-string dimensions cg))
    (when fill-p
      (format outp "with |fill-pointer| ~a " (gen-value-string (fill-pointer A) cg)))
    (when adj-p
      (format outp "with adjustable "))
    (when (or (cdr datadims) (> (car datadims) 0))
      (format outp "with |initial-contents| ")
      (if (and (memq A (cdr *collectionsBeingWritten*)) 
               (eql 0 (decf writeObjectRecursionLimit)))
        (write-string "[...]}" outp)
        (multiple-value-bind (arrayData offset) (CCL::array-data-and-offset A)
          (write-SK8-array-elements outp A arrayData offset 
                                    datadims 
                                    #'(lambda (elt strm ignored-reReadably)
                                        (declare (ignore ignored-reReadably))
                                        (format strm "~a" (gen-value-string elt cg)))
                                    ))))
    (write-string ")" outp)
    (get-output-stream-string outp)))

(defun write-FASL-array-elements (overallArray arrayData offset dimensions func)
  (declare (fixnum offset) (list dimensions)
           (type vector arrayData) (type itemWriter function))
  (let* ((tail (cdr dimensions))
         (limit (car dimensions))
         (step 1)
         (o offset)
         (i 0)
         res)
    (declare (fixnum limit step o i))
    (dolist (e tail) (declare (fixnum e)) (setq step (* e step)))
    (unless (eql i limit) (setf res (list 'list)))
    (loop
      (when (eql i limit) (return))
      (if tail
        (AddToEnd res (write-FASL-array-elements overallArray arrayData o tail func))
        (AddToEnd res (funcall func (aref arrayData o))))
      (incf i)
      (incf o step))
    res))

(defmethod CREATE-VALUE-STRING ((A Array) (cg FASL-CodeGenerator))
  ; code hacked from writeObject
  (let ((res nil)
        (writeObjectRecursionLimit writeObjectRecursionLimit)
        (*collectionsBeingWritten* (cons A *collectionsBeingWritten*))
        (fill-p (array-has-fill-pointer-p A))
        (adj-p (adjustable-array-p A))
        (dimensions (array-dimensions A))
        (datadims (array-dimensions A)))
    (declare (dynamic-extent *collectionsBeingWritten*))
    (when fill-p
      (setf datadims (list (fill-pointer A)))
      (when adj-p
        (setf dimensions datadims)))
    (setf res `(ccl::make-array ,(gen-value-string dimensions cg)))
    (when fill-p
      (setf res (nconc res `(:fill-pointer ,(gen-value-string (fill-pointer A) cg)))))
    (when adj-p
      (setf res (nconc res '(:adjustable t))))
    ;; (when (or (cdr datadims) (> (car datadims) 0))  for some weird reason, when we do this filter, every non "initial-contents" form gets the last initial-contents form appended to it.?!?!
    (setf res (append res (list :initial-contents)))  ;;DON't MAKE THIS ADD TO END OR THE SAME PROBLEM APPEARS!
    (if (and (memq A (cdr *collectionsBeingWritten*)) 
             (eql 0 (decf writeObjectRecursionLimit)))
      (print "HIT LIMIT WHILE WRITING AN ARRAY!!!")
      (multiple-value-bind (arrayData offset) (CCL::array-data-and-offset A)
        (addtoend res (write-FASL-array-elements A arrayData offset 
                                                 datadims 
                                                 #'(lambda (x) (gen-value-string x cg))
                                                 ))
        
        ))
    res))




;;----------------------------------------------------------------------------------
;;;I bet we could rewrite this a lot simpler...

(defmethod CREATE-VALUE-STRING ( (V Simple-Vector) (cg SK8Script-CodeGenerator) )
  (let ( (outp (make-string-output-stream)) )
    ; code hacked from writeObject
    (let ((writeObjectRecursionLimit writeObjectRecursionLimit)
          (*collectionsBeingWritten* (cons V *collectionsBeingWritten*)))
      (declare (dynamic-extent *collectionsBeingWritten*))
      ;; (write-string "the Vector {" outp)
      ;; REMOVE THIS LINE (and replace with the one above). Hernan.
      (write-string "({" outp)
      (if (and (memq V (cdr *collectionsBeingWritten*)) 
               (eql 0 (decf writeObjectRecursionLimit)))
        (write-string "[...]}" outp)
        (let ((first t))
          (dovector (item V)
            (if first
              (setq first nil)
              (write-string ", " outp))
            (format outp "~a" (gen-value-string item cg)))))
      (stream-tyo outp #\})
      ;; REMOVE THIS LINE TOO! Hernan.
      (write-string " as a vector)" outp)
      (get-output-stream-string outp)
      ) ) )

;this is easy.  we simply map gen-value-string on the elements and make sure the result is a vector.
(defmethod CREATE-VALUE-STRING ( (V Simple-Vector) (cg FASL-CodeGenerator) )
  (call-next-method)
  ;(map 'vector #'(lambda (x) (gen-value-string x cg)) v)
  )


;;----------------------------------------------------------------------------------
;;;There is no equivalent here for sk8script-codegen because the default, objectstring of
;;;symbol, is always rereadable...

(defmethod CREATE-VALUE-STRING ( (s symbol) (cg SK8Script-CodeGenerator) )
  (objectString s :project (project cg)))

(defmethod CREATE-VALUE-STRING ( (s symbol) (cg FASL-CodeGenerator) )
  (if (or (eq s nil) (eq s t) (keywordp s))
    s
    `(quote ,s)))

;;----------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------

;;;The following two functions need a home...
(define-handler sk8::genFASLForms (project asLisp &key forStandAlone)
  (declare (special *load-file-namestring*))
  (let ((*FASL-CodeGenerator* (make-instance 'FASL-CodeGenerator :asLisp aslisp :outputFile *load-file-namestring*))
        (output-protocol nil)
        finishedOK?)
    (declare (special *FASL-CodeGenerator*))
    (unwind-protect
      (progn
        (ensureHandlersSaved me)
        (ps::copy-all-pending-handlers) ;; make sure everything being edited is flushed to disk before we do anything else
        (mapc #'sendUpdateEvent (contents stage))   ;; is this the right place to make things look clean?
        (when forStandAlone
          ;; Start by outputting the list of requires. 
          ;; (addFaslForm *FASL-CodeGenerator* (read-from-string (require-preface me) nil nil))
          (setf output-protocol (list (require-preface me)))
          )
        (withLockedCursor AnimatedClock (walk-project me *FASL-CodeGenerator* output-protocol t '() t nil nil nil))
        (setf finishedOK? t))
      (unless finishedOK? 
        (when (and *FASL-CodeGenerator* (tempfiles *FASL-CodeGenerator*))
          (mapc #'(lambda (x) (when (probe-file x) (delete-file x))) 
                (tempfiles *FASL-CodeGenerator*))))
      )))

(define-sk8-function generateFASLforProject nil (proj &key 
                                                          (console SS::!*lisp-dev-mode*)
                                                          outputfile 
                                                          forStandAlone
                                                          asLisp)
  
  (let (finishedOK?)
    (unwind-protect 
      (progn
        ;;;THIS MAKES A FILE PATHNAME OF TYPE FASL BEFORE ANYTHING ELSE IS DONE.
        ;;;We need to do this before we start generating code so that when resources
        ;;;are copied, everything gets done properly...
        (cond ((stringp outputfile) (setq outputfile (sk8-filename-to-mcl-pathname outputfile)))
              ((pathnamep outputfile) nil)
              ((inheritsfrom outputfile file) (setq outputfile (ospathname outputfile)))
              (t (error "Invalid outputfile argument")))
        (setf outputfile  (make-pathname :defaults outputfile)) ;;; :type  (if aslisp "lisp" "fasl")))
        (with-open-file (*fasdump-stream* outputfile :direction :output
                                          :element-type 'base-character ; huh
                                          :if-exists :supersede
                                          :if-does-not-exist :create
                                          :external-format (if aslisp :text #.(ccl::make-keyword (pathname-type *.fasl-pathname*
                                                                                                                :case :common))))
          (declare (ignore *fasdump-stream*))
          )
        
        (let ((*save-definitions* nil)
              (*save-local-symbols* t)   ;;*** kludge alert! this must be t so there will be a lfun-info for us to save stuff in
              (*fasl-save-local-symbols* t)
              (*save-doc-strings* nil)
              (*record-source-file* nil)  
              (CCL::*SUPPRESS-COMPILER-WARNINGS*    T)
              (*load-file-namestring* outputfile))
          (declare (special *load-file-namestring*))
          (funcall (if console #'print #'sendToLog)
                   (format nil "Saving ~a to ~a ..." (objectname proj) outputfile))
          (sk8::genFASLForms proj asLisp :forStandAlone forStandAlone)
          (setf finishedOK? t)
          (funcall (if console #'print #'sendToLog)
                   (format nil "Finished saving ~a to ~a ..." (objectname proj) outputfile))
          ))
      (unless finishedOK? 
        (when (and outputfile (probe-file outputfile))
          (delete-file outputfile))))))


;;(generateFASLforProject foo :aslisp t :outputfile (new file :project sk8 :ospathname (namestring "haha")))




#|
	Change History (most recent last):
	1  	 9/26/94	kend    	Code generator to output SK8Script.
	2  	 9/26/94	kend    	Set *%%CodeGen-Debug%%* to false
	3  	 9/30/94	kend    	Added optional package name to objectName in gen-name-string and gen-value-string {fixes Bug 1187844}
	4  	10/12/94	kend     	SK8Script specific part of otherwise generic codegen.
	5  	10/12/94	kend     	made gen-prin-array-or-vector specific to SK8Script.
	6  	10/12/94	kend     	Replaced (in-package :SK8Dev) which I had inadvertandly deleted.
	7  	10/13/94	kend    	Added Get-object-info-maker {inadvertantly deleted}.
	8  	10/26/94	sidney  	declare methods as local handlers so they don't show up in object editor
	9  	10/26/94	chip    	(^ he meant "private", not "local") took out now unnecessary MAKE-PRIVATE... calls
	10 	10/27/94	kend    	Update to set layers info for Actor after container is set.
	11 	10/27/94	kend    	Need to set the layers of top-level Actors as well.
	12 	10/31/94	kend    	Now sorts actors by layer and sets layers in order.
	13 	11/ 4/94	kend    	Now outputs  localCreationRelations
	14 	11/ 4/94	kend    	Coerce rationals to floats (SX don't have no rationals; SK8Script does not have raionals)
	15 	11/ 7/94	kend    	Remove rationals printing hack from gen-value-string (->writeObject)
	16 	11/ 8/94	kend    	Fix bug in gen-value-string: refs to SK8 objects within list, vector, or array
	17 	11/13/94	sidney  	get global value from global, not store. Don't print initialization if it is not initialized.
	18 	11/15/94	kend    	Don't set properties if they contain values which are mac handles.
	19 	11/16/94	kend    	Remember layers for menuItems and Actors--but only if they are contained in something.
	20 	11/17/94	kend    	gen-new now uses objectName if available
	21 	11/18/94	kend    	PixMap restoration code added.
	22 	11/21/94	kend    	Fix parent ref in GEN-NEW
	23 	12/13/94	sidney  	use objectstring for unnamed refs in other project in GEN-NEW
	24 	12/26/94	sidney  	add :skipInitialization keyword to new to support save as text
	25 	 1/12/95	sidney  	changes to save as text to be more like binary store
	26 	 1/21/95	sidney  	deal with special cases
	27 	 1/24/95	sidney  	more tweaking to suport save as text of strange objects
	28 	 1/26/95	sidney  	tweaked again: deal with sk8-fred, sk8-menu, etc. clos objects
	29 	 2/14/95	sidney  	arrays with fill-pointer
	30 	 2/16/95	sidney  	eliminate unnecessary explicit references to package in printed form of an object name
	31 	 2/20/95	sidney  	1201562, 1208925: implement saving of local handlers and properties of the project being saved
	32 	 3/ 7/95	sidney  	things work better when I don't type in the wrong variable name when checking in a fix
	33 	 3/ 8/95	dy      	make gen-real-prop-inits use propertiesToSaveAsFalse and propertiesToSaveSpecially
	34 	 3/ 8/95	sidney  	dont write out references to objects that shouldn't be written out
	35 	 3/ 8/95	sidney  	write out pathnames in 'legal' SK8Script
	36 	 3/ 9/95	sidney  	Needed another pair of parentheses in the pathname syntax used in the previous change
	37 	 3/10/95	sidney  	Do the right thing generating script for vectors with fillpointers
	38 	 3/13/95	sidney  	we were suppressing references to objects that have dontsavescript set. They should not be generated, but can be referenced.
	39 	 3/14/95	sidney  	changing the way we deal with media objects in the project: copy the resources to the generated script file and import them from there
	40 	 3/15/95	sidney  	when copying resources, it works better if what you copy are the resources
	41 	 3/18/95	sidney  	1227949: changed way composit prims are dealt with and menu and menu-items to get menus to save/load properly
	42 	 3/19/95	sidney  	don't ASS-U-ME that menu-items always have an owner menu (it can be just a prototype)
	43 	 3/21/95	sidney  	pixmaps are now like other media, so we can remove special case code for them
							use helpfindmychild the way we do in the binary store
							use gen-value-string instead of objectstring so more general things work
	44 	 3/23/95	sidney  	fix error generating output from large array values in gen-value-string
	45 	 3/29/95	sidney  	1233452: correct output syntax for globals whose names need to be escaped
	46 	 3/30/95	sidney  	make output indepent of name of project
	47 	 4/ 3/95	sidney  	1235400: make output indepent of name of project
	48 	 4/17/95	sidney  	can't reliably make output independent of project name, so have the script check that the name is correct
	49 	 4/18/95	sidney  	fix error in script output of illegal name in current project
	50 	 4/18/95	sidney  	1240528: used wrong default value when deciding not to write out initialization of a property
	51 	 4/19/95	sidney  	screwed up a little in the last checkin
	52 	 4/25/95	rod     	Fixing YesOrNoDialog and MessageToUser calls to 
							not set the heights so it can be computed 
							manually.
	2  	 6/19/95	sidney  	don't write out contents of arrays after their fill-pointer
	3  	 6/26/95	Hernan  	Fixing gen-value-string of array.  Badly formed let statement.
	4  	 6/30/95	sidney  	(no change) force recompile after dynamic-extent bug has been fixed in MCL
	5  	 6/30/95	sidney  	save text styles info with fred buffer
	1  	 7/ 6/95	Brian Roddy	Here is the file which consolidates the
						sk8script and fasl generation.
	2  	 7/ 6/95	Brian   	Let the bug fixing begin.  Fixing minor problems
						with argument names and with abstraction.
	3  	 7/12/95	Brian Roddy	Adding robustness to functions and handlers.
	4  	 7/14/95	Brian Roddy	Adding streaming.
	5  	 7/14/95	Brian  	Fixing bug in generating fasl versions of Cons
						pairs.
	6  	 7/17/95	Brian  	Fixed problems with arrays.
	7  	 7/19/95	Brian  	Fixing bug in save as text.  It didn't handle
						cons pairs.
	8  	 7/24/95	Brian  	
	9  	 8/ 4/95	Hernan  	
	10 	 8/ 4/95	Hernan  	various fixes. print statements removed.
	11 	 8/ 7/95	Hernan  	
	12 	 8/ 9/95	Brian   	making cons code work with variables.
	13 	 8/11/95	Brian   	fixing fasl bug for setter handlers.
	14 	 8/11/95	Brian   	fixed setter handler stuff
	15 	 8/28/95	Brian   	
	16 	11/27/95	Brian   	fixing problem with gen-real-prop-init to always make
						property for shared eq items
	17 	 2/ 7/96	sidney  	remove wood
	18 	 2/ 8/96	sidney  	translateScriptCommandOrExpr subsumes older separate functions
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	5  	 4/25/96	Hernan  	Added a forStandAlone argument to the save method. Then outputting
						the list of requires at the start of the fasl file.
	6  	 5/20/96	sidney  	use symbol-value instead of find-object to get a project from its name
	7  	 5/23/96	sidney  	use the compile time MCL's file type for fasl, not hardcoded :fasl
	8  	 7/ 7/96	sidney  	changes for native PPC build
	9  	 7/29/96	Hernan  	Various fixes to get save as text to happen.
	10 	 8/ 1/96	Hernan  	LOTS of fixes to port the store to the PPC.
	11 	 8/ 1/96	Hernan  	Fixed gen-var to work when (specialproptable gc) is not a
						hash table.
	12 	 8/ 9/96	Hernan  	Giving a meaningful error when trying to save unnamed
						objects in other projects and helpFindMyChild is not defined. And many more fixes.
	13 	 8/12/96	Hernan  	prop-value-needs-saving now implemented correctly.
	14 	 9/ 3/96	Hernan  	Lots of changes, to make the store work in MCL 4.0
	15 	 2/27/97	Hernan  	
	16 	 3/ 7/97	Hernan  	require-preface now returns a lisp form, not a string. Get
						rid of those read-from-string that could not work on a SK8
						release version.
|# ;(do not edit past this line!!)
