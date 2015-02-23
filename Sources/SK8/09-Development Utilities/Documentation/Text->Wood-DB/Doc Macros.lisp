;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; SK8 Documentation Macros!!!!! 
;;; A Definition Parser and a String Expander


;;;;______________________________________________________________________________________________________
;;;;______________________________________________________________________________________________________
;;;;
;;;;  THE API....
;;;;   
;;;; The user makes a file(s) of "Defines" of the form:

;;;;  =============================================================
;;;;  DEFINE: specialization handlerName, object
;;;;  =============================================================
;;;;  
;;;;  [Note: This handler is just a specialization of [[<handlerName>]] of 
;;;;  [[<object>]]. To see what the point of it is, see the description of
;;;;  that handler.]

;;;;
;;;;  The following two functions can be used to convert such files into files of 
;;;;  lisp code which can then be compiled and evaluated.
;;;;
;;;; 0 ParseDefineFile takes four optional arguments.  The first is a file object specifying
;;;;    the input file.  The second is a file object specifying the output file.  If either optional is left blank
;;;;    standard file dialogs will be used to get the files.  The third argument specifies whether the file
;;;;    should be loaded after being created.  The fourth argument specifies whether the file should be 
;;;;    compiled after creation.
;;;;
;;;; 0 ParseDefineFolder takes two optional arguments.  The first is a file object specifying
;;;;    the directory containing only a set of define files.  The second is a file object specifying the output 
;;;;    file.  If either optional is left blank standard file dialogs will be used to get the directory and/or the file.
;;;;
;;;;  ----------------------------------------------------------------------------------------------------
;;;;
;;;; The next function can be used after the outputted lisp files have been loaded.
;;;;
;;;; 0 ExpandDefines takes a string of documentation, and the three record id args (recordName recordType recordObject);
;;;;    It returns a new string with all the defines exploded.  
;;;;
;;;;______________________________________________________________________________________________________
;;;;______________________________________________________________________________________________________


(in-package :sk8dev)

(defparameter *current-file* nil)

(defparameter docmacro-CurrentDefinition nil)
(defparameter docmacro-CurrentArgs nil)
(defparameter docmacro-CurrentString nil)
(defparameter docmacro-DefineList nil)
(defparameter docmacro-xrefsMacro? nil)


(defun GetNextLine (inFile)
  (read-line inFile nil nil))

(defun BreakIntoWords (str)
  (let (theWords)
    (mapWordChunks str nil nil #'(lambda (s e) (push (subseq str s e) theWords)))
    (nreverse theWords)))

(defun ParseDefinitionLine (str)
  (let* ((theWords (breakintowords str))
         (FuncName (car thewords)))
    
    (unless (char= (aref str 0) #\()
      ;; Ignore the lines of the form (Engineer: Brian, modified ...)
      (unless (string-equal FuncName "define")
        (error "First word of a definition must be DEFINE"))
      
      (cond
       ((string-equal (second theWords) "XREFS")
        (pop theWords)
        (setf docmacro-xrefsMacro? t))
       (t
        (setf docmacro-xrefsMacro? nil)))
      
      (setf docmacro-CurrentDefinition (second theWords))
      (setf docmacro-CurrentArgs (mapcar #'(lambda (x) (list x (concatenate 'string x (string (gensym))))) 
                                         (append '("recordName" "recordType" "recordObject") (cddr thewords))))
      (setf docmacro-CurrentString nil)
      )))


(defun getkeyfromstring (str)
  (let (theval)
    (dolist (i docmacro-CurrentArgs)
      (if (string-equal str (car i))
        (setf theval i)
        ))
    (if theval
      theval
      (error (concatenate 'string "Can't find corresponding argument in definition to " str))
      )))

(defun RemoveFrontStrings ()
  (loop
    (if (not docmacro-CurrentString) (return))
    (if (string= (subseq docmacro-CurrentString 0 1) (string #\newline))
      (setf docmacro-CurrentString (subseq docmacro-CurrentString 1 ))
      (return))))


(defun MakeDefinitionFromGlobals ()
  (RemoveFrontStrings)  ;;; Remove trailing junk
  (setf docmacro-CurrentString (nreverse docmacro-CurrentString))
  (RemoveFrontStrings) ;;; Remove front junk
  (setf docmacro-CurrentString (nreverse docmacro-CurrentString))
  (let (curchar
        stringchunks
        (len (length docmacro-CurrentString))
        (curCount 0)
        (lastchunkbegin 0)
        (lastkeybegan nil)
        stringToEval
        )
    (loop
      (if (>= curcount len) (return))
      (setf curchar (subseq docmacro-CurrentString curCount (+ CurCount 1)))
      (cond
       ((string= curchar "<") 
        (push (subseq docmacro-CurrentString lastchunkbegin curcount) stringchunks)
        (setf lastkeybegan (1+ curcount))
        )
       ((string= curchar ">")
        (if lastkeybegan
          (push (getkeyfromstring (subseq docmacro-CurrentString lastkeybegan curcount)) stringchunks)
          (error "Encountered a > without a corresponding <"))
        (setf lastkeybegan nil)
        (setf lastchunkbegin (1+ curcount))))
      (incf curcount)
      )
    (push (subseq docmacro-CurrentString lastchunkbegin) stringchunks)  ;;;need to check end of line here.
    (setf stringchunks (nreverse stringchunks))
    (setf stringToEval (concatenate 'string "(defun " docmacro-CurrentDefinition " (&optional "))
    (dolist (i docmacro-CurrentArgs)
      (setf stringToEval (concatenate 'string stringToEval (cadr i) " "))
      )
    (setf stringToEval (concatenate 'string stringToEval ") "))

    (flet ((bodyCreatorString ()
             (let ((str "(concatenate 'string"))
               (dolist (i stringchunks)
                 (setf str (concatenate 'string str " " (if (listp i)
                                                          (cadr i)
                                                          (prin1-to-string i)))))
               (concatenate 'string str ")"))))
      
      (if docmacro-xrefsMacro?
        (setf stringToEval (concatenate 'string stringToEval
                                        "(let ((xrefsString " (bodyCreatorString)
                                        ")) (SK8Dev::buildXrefsString (SK8Dev::allXrefsButCurrent (SK8Dev::buildXrefsList xrefsString)"
                                        " " (second (first docmacro-CurrentArgs))
                                        " " (second (second docmacro-CurrentArgs))
                                        " " (second (third docmacro-CurrentArgs))
                                        "))))"))
        (setf stringToEval (concatenate 'string stringToEval (bodyCreatorString) ")")))
      
      (push stringToEval docmacro-DefineList)
      )))


;;; theFolder and the outfile are strings representing the pathname of the input folder
;;; and the output file.

(defun ParseDefineFolder (&optional theFolder outFile loadIt compileIt)
  (unless theFolder
    (setf theFolder (osPathName (MacChooseDirectoryDialog))))
  (unless outFile
    (setf outFile (osPathName (MacChooseNewFileDialog))))
  (setf theFolder (namestring theFolder))
  (when (eq #\: (char theFolder (1- (length theFolder))))
    (setq theFolder (concatenate 'string theFolder "*.*")))
  (format t "~%Define Translation started at ~a.~2%" (timeString today))
  (with-open-file (outPutFile outFile :direction :output :if-exists :supersede)
    (dolist (aFile (directory theFolder))
      (setf *current-file* aFile)
      (with-open-file (inputFile aFile :direction :input)
        (ParseDefineFile inputFile outputFile))))
  ;; Load the output file.
   (when compileIt
      (compile-file outFile))
   (when loadIt
    (load outFile))
  (format t "~2%Define Translation ended at ~a.~2%" (timeString today))
  ;; NOTE: get rid of the temp file!
  )

(defun ParseDefineFile (&optional inFile outFile)
  (let* (curline
         LookForDef
         defining
         (numberOfDefs 0))
    (format t "Parsing Define File ~a...~%" *current-file*)
    ;; (initializeForDefines)
    (loop
      (setf curline (getnextline inFile))
      (when (string= (subseq curline 0 (min 2 (length curline))) "#|") (setf curline nil))
      (if defining
        (cond
         ((or (not curline)
              (and (> (length curline) 3) (string= "====" (subseq curline 0 4))))
          (MakeDefinitionFromGlobals)
          (setf defining nil)
          (setf LookForDef t)
          (if (not curline) (return)))
         (t
          (setf docmacro-CurrentString (concatenate 'string
                                           (concatenate 'string docmacro-CurrentString curline)
                                           (string #\newline)))))
        (cond
         ((not curline)
          (if (= numberOfDefs 0)
            (print (concatenate 'string "Warning:  File " (objectstring *current-file*) 
                                " ended with no definitions!"))
            (error "Unexpected End of File!?!?!"))
          (return))
         ((not (breakintowords curline)) nil)
         ((and (> (length curline) 3) (string= "====" (subseq curline 0 4)))
          (when LookForDef 
            (setf defining t)
            (incf numberOfDefs))
          (setf LookForDef (not LookForDef)))
         ((and (= numberOfDefs 0) (not LookForDef))
          (error "Files must begin with a definition!"))
         (LookForDef
          (when (and (not (string-equal "(modified" (subseq curline 0 9)))
                     (not (string-equal "(engineer" (subseq curline 0 9))))
            (ParseDefinitionLine curline)))
         (t
          (error "Definitions should be a line of =s, a definition, followed by a line of =s"))
         )))
    (write-line "(unless (find-package \"DocDefines\") (defpackage \"DocDefines\"))" outFile)
    (write-line "(in-package \"DocDefines\")" outFile)
    (dolist (i (nreverse docmacro-DefineList))
      (write-line i outFile))
    (format t "Saving ~a Definitions to File ~a~%" numberOfDefs outFile)
    ;; (CleanUpForDefines)
    ))

#|  Tests:

(parsedefinefile jism jasm t nil)


(print docmacro-CurrentString)
(defparameter jism (MacChooseFileDialog :macFileType "TEXT"))
(defparameter jasm (MacChooseNewFileDialog))
(parsedefinefile jism jasm)
(ExpandDefines "Thank you for your support. $$specialization mousedown foobar$$  Bite me.  $$Floz TablePicker$$")
(ExpandDefines "Thank you for your support. $$disclaimer$$")

(defpackage "Spazz" (:use ()))
(defpackage "Spazz" )

(or (find-package "Spazz")
                (defpackage "Spazz" ))
(in-package "Spazz")

(defun disclaimer () )

|#


;;;;;;;;__________________________________________________________________________________________________
;;;;;;;;__________________________________________________________________________________________________
;;;;;;;; These functions support the "XREFS" macros when they expand
;;;;;;;;__________________________________________________________________________________________________
;;;;;;;;__________________________________________________________________________________________________


(defun trimXref (str)
  (string-trim '(#\Space #\Newline #\Tab #\.) str))


(defun parseXref (xref)
  (flet ((substring= (mainString targetString mainStart mainEnd)
           (let ((len (length mainString)))
             (when (and mainStart (> mainStart len))
               (setq mainStart len))
             (when (and mainEnd (> mainEnd len))
               (setq mainEnd len))
             (string-equal mainString targetString :start1 mainStart :end1 mainEnd))))
    (setq xref (trimXref xref))
    (let ((originalString xref)
          (useOriginal? nil))
      (when (substring= xref "the " nil 4) (setq xref (subseq xref 4)))
      (let* ((types '(("property"	. :property)
                      ("virtual property" . :property)
                      ("handler"		. :handler)
                      ("object"		. :object)
                      ("function"	. :function)
                      ("global"		. :global)
                      ("constant"	. :constant)))
             (type (cdr (assoc-if #'(lambda (type)
                                      (let ((len (length type)))
                                        (when (and (substring= xref type nil len)
                                                   (eq #\Space (char xref len)))
                                          (setq xref (subseq xref (1+ len))))))
                                  types)))
             (name nil)
             (obj nil)
             (xrefLength (length xref))
             pos)
        
        (cond
         ;; THE TYPE WAS AT THE BEGINNING
         (type
          (cond
           ((or (eq type :handler) (eq type :property))
            (if (setq pos (search " of " xref :test #'char-equal))
              (setq name (subseq xref 0 pos)
                    obj (parseXref (subseq xref (+ pos 4))))
              (setq name xref)))
           (t
            (setq name xref))))
         
         ;; THE TYPE FOLLOWS THE NAME
         ((setq type (cdr (assoc-if #'(lambda (type)
                                        (let* ((len (length type))
                                               (pos (search type xref :test #'char-equal)))
                                          (when (and pos
                                                     (or (eql pos 0)
                                                         (eq #\Space (char xref (1- pos))))
                                                     (or (eql xrefLength (+ pos len))
                                                         (eq #\Space (char xref (+ pos len)))))
                                            (if (eql pos 0)
                                              (setq name (if (string= type "object") "Object" ""))
                                              (setq name (subseq xref 0 (1- pos))))
                                            (cond
                                             ((eql xrefLength (+ pos len))
                                              (setq xref nil))
                                             (t
                                              (setq xref (trimXref (subseq xref (+ pos len))))
                                              (when (eql 0 (length xref)) (setq xref nil))))
                                            t)))
                                    types)))
          (cond
           ;; There's something after the type...
           (xref
            (if (and (or (eq type :handler) (eq type :property))
                     (substring= xref "of " nil 3))
              ;; It seems to be the object
              (setq obj (parseXref (subseq xref 3)))
              ;; It seems to be excess junk, so punt
              (setq useOriginal? t)))
           ;; There's nothing after the type, but there's a possessive clause...
           ((setq pos (or (search "'s " name :test #'char-equal)
                          (search "Õs " name :test #'char-equal)))
            (setq obj (subseq name 0 pos)
                  name (subseq name (+ pos 3))))))
         
         ;; NO TYPE WAS GIVEN
         ((setq pos (search " of " xref :test #'char-equal))
          (setq name (subseq xref 0 pos)
                obj (parseXref (subseq xref (+ pos 4)))))
         
         ((setq pos (or (search "'s " xref :test #'char-equal)
                        (search "Õs " xref :test #'char-equal)))
          (setq obj (subseq xref 0 pos)
                name (subseq xref (+ pos 3))))
         (t
          (setq useOriginal? t)))
        
        (unless useOriginal?
          (when name
            (setq name (trimXref name))
            (unless (intern-legal-handlerName name :SK8 nil)
              ;; What we thought was an global/handler/property name is bogus, so punt
              (setq useOriginal? t)))
          (cond
           (obj
            (setq obj (trimXref obj))
            (unless (intern-legal-varName obj :SK8 nil)
              ;; What we thought was an objectName is bogus, so punt
              (setq useOriginal? t)))
           (t
            (when (eq type :object) (setq obj name)))))
        
        (if useOriginal?
          (values originalString nil nil)
          (values name type obj))))))


#|

(parseXref "Actor's fillColor")
(parseXref "fillColor of Actor")
(parseXref "the fillColor property of Actor")
(parseXref "the fillColor property of the object Actor")

(parseXref "the object Actor")
(parseXref "the Actor object")

(parseXref "beep")
(parseXref "the beep function")
(parseXref "the function beep")

|#


(defun buildXrefsList (str)
  (let ((xrefs nil)
        (i 0)
        commaPos)

    (labels ((addXref (x)
               (push (multiple-value-list (parseXref x)) xrefs))
             
             (maybeBreakAnds (xref)
               (let ((j 0)
                     andPos)
                 (loop
                   (cond
                    ((null (setq andPos (search " and " xref :start2 j :test #'char-equal)))
                     (addXref (subseq xref j))
                     (return))
                    (t
                     (unless (<= andPos 1)
                       (addXref (subseq xref j andPos)))
                     (setq j (+ andPos 5))))))))
      
      (loop
        (cond
         ((null (setq commaPos (position #\, str :start i :test #'eq)))
          (maybeBreakAnds (subseq str i))
          (return))
         (t
          (maybeBreakAnds (subseq str i commaPos))
          (setq i (1+ commaPos)))))
      (nreverse xrefs))))



(defun xrefMatchesCurrentRecord? (xrefName xrefType xrefObject recordName recordType recordObject)
  (and
   ;; Names must match
   (string-equal xrefName recordName)
   ;; Type must either be empty or match semantically
   (or (null xrefType)
       (eq xrefType recordType)
       (and (memq xrefType '(:handler :property))
            (memq recordType '(:handler :property))))
   ;; Object must either be empty or match exactly
   (or (null xrefObject)
       (and recordObject (string-equal xrefObject recordObject)))))



(defun allXrefsButCurrent (xrefsList recordName recordType recordObject)
  (let ((match nil))
    (dolist (xref xrefsList)
      (when (xrefMatchesCurrentRecord? (first xref) (second xref) (third xref)
                                       recordName recordType recordObject)
        (setq match xref)
        (return)))
    (if match
      (remove match xrefsList :test #'eq)
      xrefsList)))



(defun buildXrefsString (xrefsList)
  (let ((first? t)
        (str "")
        name type obj)
    (dolist (xref xrefsList str)
      (setq name (pop xref)
            type (pop xref)
            obj (pop xref))
      (case type
        
        (:PROPERTY
         (setq xref (concatenate 'string "the " name " property of " obj)))
        
        (:HANDLER
         (setq xref (concatenate 'string "the " name " handler of " obj)))
        
        (:OBJECT
         (setq xref name))
        
        (:FUNCTION
         (setq xref (concatenate 'string "the function " name)))
        
        (:GLOBAL
         (setq xref (concatenate 'string "the global " name)))
        
        (:CONSTANT
         (setq xref (concatenate 'string "the constant " name)))
        
        (t
         (if obj
           (setq xref (concatenate 'string name " of " obj))
           (setq xref name))))
      
      (if first?
        (setq first? nil
              str xref)
        (setq str (concatenate 'string str ", " xref))))))



;;;;;;;;__________________________________________________________________________________________________
;;;;;;;;__________________________________________________________________________________________________
;;;;;;;; These two functions are all you need after lisp definitions are loaded.
;;;;;;;;__________________________________________________________________________________________________
;;;;;;;;__________________________________________________________________________________________________

(defun EvalDefine (str recordName recordType recordObject)
  (let* ((*package* (find-package "DocDefines"))
         (theWords (breakintowords str))
         (FuncName (car thewords)))
    (apply (symbol-function (read-from-string FuncName)) recordName recordType recordObject (cdr thewords))
    ))

(defun ExpandDefines (str &optional recordName recordType recordObject)
  (let (curchar
        stringchunks
        (len (length str))
        (curCount 0)
        (lastchunkbegin 0)
        (lastkeybegan nil)
        (inCall? nil)
        NewString
        )
    (decf len)
    (loop
      (if (>= curcount len) (return))
      (setf curchar (subseq str curCount (+ CurCount 2)))
      (when (string= curchar "$$")
        (cond
         ((not inCall?)
          (push (subseq str lastchunkbegin curcount) stringchunks)
          (setf lastkeybegan (+ curcount 2))
          (setf inCall? t))
         (t
          (push (list (subseq str lastkeybegan curcount)) stringchunks)
          (setf lastkeybegan nil)
          (setf lastchunkbegin (+ curcount 2))
          (setf inCall? nil))))
      (incf curcount)
      )
    (push (subseq str lastchunkbegin) stringchunks)  ;;;need to check end of line here.
    (setf stringchunks (nreverse stringchunks))
    (dolist (i stringchunks)
      (if (listp i)
        (setf NewString (concatenate 'string NewString (EvalDefine (car i) recordName recordType recordObject)))
        (setf NewString (concatenate 'string NewString i))))
    NewString
    ))




#|
	Change History (most recent last):
	1	8/1/94	Hernan	New file. Implements reader for documentation
				macros.
	2	8/2/94	Hernan	Doing stuff using the lisp file system.
	3	8/2/94	Hernan	Changing the macro character from "{{" to $$.
	4	8/8/94	chip	Fixed ExpandDefines glitch (due to changing {{ to $$); added implicit "record id" args; added XREFS macro facility
	5	8/15/94	rod	
	6	8/22/94	chip	MakeDefinitionFromGlobals now makes args optional
	7  	 8/23/94	Brian   	
	8  	 9/ 2/94	Hernan  	Addam fixed bug in parseXref.
	9  	11/ 3/94	Rick Evans	
	10 	11/ 3/94	Rick Evans	
	11 	11/17/94	Hernan  	The macro reader should not die when the definition
							line is followed by one of those (modified...) lines.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
