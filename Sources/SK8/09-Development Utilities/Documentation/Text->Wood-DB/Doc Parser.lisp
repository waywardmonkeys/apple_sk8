;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;; This file reads in a set of files from a specified folder and writes them out into
;;; a wood file. From that wood file we get random access for both online documentation
;;; and frame maker MIF generation.

(defparameter *source-folder* "ccl;Documentation:Text:*")
(defparameter *macros-folder* "ccl;Documentation:Macros:*")
(defparameter *macros-outfile* "ccl;Temporary:Doc Macros")

;;; This is the stream that the sources->RTF functions write to.  

(defparameter *RTF-stream* t)

;;; This macro returns whether we are compiling to RTF for QuickView. Used
;;; by the register-XXX functions to redirect control to the RTF methods. 

(defmacro compiling-to-RTF? ()
  (neq t *RTF-stream*))

;;; If true, all doc files are compiled.

(defparameter *compile-all-docs* t)

(defparameter *wood-file* "ccl;Sk8 Documentation")

;;; The current object being documented.

(defparameter *current-object* nil)


;;; The names identifying the "record" currently being processed.

(defparameter *current-object-str* nil)
(defparameter *current-handler-str* nil)
(defparameter *current-property-str* nil)
(defparameter *current-function-str* nil)
(defparameter *current-global-str* nil)
(defparameter *current-constant-str* nil)


;;; The list of handlers and properties for the current object. These
;;; are stored because they will be saved in a slot of the object.

(defparameter *current-handlers* nil)
(defparameter *current-properties* nil)
(defparameter *current-args* nil)

;;; Delimiters commonly used.

(defparameter *header-delimiter* "======")
(defparameter *property-delimiter* "__")
(defparameter *arg-delimiter* #\¥)

;;; _______________________________ 
;;; Lazy Compilation Functions
;;; _______________________________ 

;;; This function checks all files to determine whether they should be processed.
;;; Ideally, it would return false for files that have not changed.

;;; Uses the same scheme sk8-build-files uses. When compiled, a file is time stamped with its write
;;; date at the time of the compile. If the file is modified subsequently, it needs a recompile.

(defun compile-required (theFile)
  (let ((pn thefile))
    (or *compile-all-docs*
        (multiple-value-bind (sourcetime sourcename) (cl-user::get-source-info-stamp pn)
          (declare (ignore sourcename))
          (or (null sourcetime)
              (/= sourcetime (file-write-date pn))
              )))))

;;; _______________________________ 
;;; Registration Functions
;;; _______________________________ 

;;; NOTE: for all description fields, the text is expanded using the macro functions.

;;; These make the objects and store them in the right list for saving at the
;;; end of the file.

;;; obj = the name
;;; as soon as the object name was detected.

(defun register-object (obj info owner description example seeAlso handlers properties)
  (let ((*current-object-str* obj))
    (if (compiling-to-rtf?)
      (object->RTF obj info owner
                   (process-general-text description)
                   (process-general-text example)
                   (process-xref-text seeAlso)
                   handlers
                   properties)
      (let ((theObject (make-instance 'cl-user::object_)))
        (setf (cl-user::object_name theObject) obj)
        (setf (cl-user::object_info theObject) info)
        (setf (cl-user::object_owner theObject) owner)
        (setf (cl-user::object_description theObject) (process-general-text description))
        (setf (cl-user::object_example theObject) (process-general-text example))
        (setf (cl-user::object_seeAlso theObject) (process-xref-text seeAlso))
        (setf (cl-user::object_handlers theObject) handlers)
        (setf (cl-user::object_properties theObject) properties)
        ;; And save it!
        (cl-user::save theObject)))))

(defun register-argument (name type required description)
  (let ((theArg (make-instance 'cl-user::argument_)))
    (setf (cl-user::argument_name theArg) name)
    (setf (cl-user::argument_type theArg) type)
    ;; Store t or nil for the required.
    (setf (cl-user::argument_required theArg) (string-equal required "REQUIRED"))
    (setf (cl-user::argument_description theArg) (process-general-text description))
    ;; and save it.
    (push theArg *current-args*)))

(defun register-handler (name info description arguments example seeAlso)
  (let ((*current-handler-str* name)
        (theHandler (make-instance 'cl-user::handler_)))
    (setf (cl-user::handler_name theHandler) name)
    (setf (cl-user::handler_info theHandler) info)
    (setf (cl-user::handler_description theHandler) (process-general-text description))
    (setf (cl-user::handler_arguments theHandler) arguments)
    (setf (cl-user::handler_example theHandler) (process-general-text example))
    (setf (cl-user::handler_seeAlso theHandler) (process-xref-text seeAlso))
    ;; and save it!
    (push theHandler *current-handlers*)))

(defun register-property (name info description 
                                  getter-arguments getter-description
                                  setter-arguments setter-description
                                  example seeAlso)
  (let ((*current-property-str* name)
        (theProp (make-instance 'cl-user::property_)))
    (setf (cl-user::property_name theProp) name)
    (setf (cl-user::property_info theProp) info)
    (setf (cl-user::property_description theProp) (process-general-text description))
    ;; Getter.
    (setf (cl-user::property_getter_arguments theProp) getter-arguments)
    (setf (cl-user::property_getter_description theProp) (process-general-text getter-description))
    ;; Setter.
    (setf (cl-user::property_setter_arguments theProp) setter-arguments)
    (setf (cl-user::property_setter_description theProp) (process-general-text setter-description))
    ;; Example and see also.
    (setf (cl-user::property_Example theProp) (process-general-text example))
    (setf (cl-user::property_SeeAlso theProp) (process-xref-text seeAlso))
    ;; and save it!
    (push theProp *current-properties*)))

(defun register-function (name info arguments description example seeAlso)
  (let ((*current-function-str* name))
    (if (compiling-to-rtf?)
      (function->RTF name info arguments
                     (process-general-text description)
                     (process-general-text example)
                     (process-xref-text seeAlso))
      (let ((theFun (make-instance 'cl-user::function_)))
        (setf (cl-user::function_name theFun) name)
        (setf (cl-user::function_info theFun) info)
        (setf (cl-user::function_arguments theFun) arguments)
        (setf (cl-user::function_description theFun) (process-general-text description))
        (setf (cl-user::function_example theFun) (process-general-text example))
        (setf (cl-user::function_seeAlso theFun) (process-xref-text seeAlso))
        ;; And save it.
        (cl-user::save theFun)))))

(defun register-constant (name info description example seeAlso)
  (let ((*current-constant-str* name))
    (if (compiling-to-rtf?)
      (constant->RTF name info
                     (process-general-text description)
                     (process-general-text example)
                     (process-xref-text seeAlso))
      (let ((theConstant (make-instance 'cl-user::constant_)))
        (setf (cl-user::constant_name theConstant) name)
        (setf (cl-user::constant_info theConstant) info)
        (setf (cl-user::constant_description theConstant) (process-general-text description))
        (setf (cl-user::constant_example theConstant) (process-general-text example))
        (setf (cl-user::constant_seeAlso theConstant) (process-xref-text seeAlso))
        ;; And save it.
        (cl-user::save theConstant)))))

(defun register-variable (name info description example seeAlso)
  (let ((*current-global-str* name))
    (if (compiling-to-rtf?)
      (variable->RTF name info
                     (process-general-text description)
                     (process-general-text example)
                     (process-xref-text seeAlso))
      (let ((theVar (make-instance 'cl-user::variable_)))
        (setf (cl-user::variable_name theVar) name)
        (setf (cl-user::variable_info theVar) info)
        (setf (cl-user::variable_description theVar) (process-general-text description))
        (setf (cl-user::variable_example theVar) (process-general-text example))
        (setf (cl-user::variable_seeAlso theVar) (process-xref-text seeAlso))
        ;; And save it.
        (cl-user::save theVar)))))

;;; _______________________________ 
;;; Reading/Parsing Helpers
;;; _______________________________ 

(defmacro tis-a-property? (theObj theSymbol)
  `(and (some #'(lambda (x) (inheritsFrom ,theObj x))  (implementors ,theSymbol))
        (some #'(lambda (x) (inheritsFrom ,theObj x)) (implementors (setter ,theSymbol)))))

(defmacro trim (theString)
  `(string-trim '(#\space #\Newline) ,theString))

(defmacro get-line (inFile)
  `(read-line ,inFile nil nil))

(defun string-prefix? (theString thePrefix)
  (string-equal theString thePrefix :start1 0 
                :end1 (min (length thePrefix) (length theString))))


(defun check-keys (theLine theKeys)
  (if (listp theKeys)
    ;; Return true if any key matches.
    (dolist (c theKeys)
      (when (string-prefix? theLine c)
        (return-from check-keys t)))
    (string-prefix? theLine theKeys)))

(defun check-end-comment (theLine)
  (and (>= (length theLine) 2)
       (string= theLine "#|" :end1 2)))



;;; Keeps reading from the file until it hits a line starting with the key specified.
;;; Returns the first line that contains the key.

(defun skip-until (inFile theKeys &optional missedItWhenAnyOfTheseShowsUp)
  (let ((theLine ""))
    (loop
      (setf theLine (get-line inFile))
      (cond ((or (null theLine) (check-end-comment theLine))
             (return-from skip-until (values nil t)))
            ((check-keys theLine theKeys)
             (return-from skip-until (values theLine nil)))
            ((and missedItWhenAnyOfTheseShowsUp (check-keys theLine missedItWhenAnyOfTheseShowsUp))
             (return-from skip-until (values theLine t)))))))

;;; Returns the string from the current point to the stop point specified.

(defun accumulate-until (inFile theKeys)
  (let ((theLine "")
        (result ""))
    (loop
      (setf theLine (get-line inFile))
      (cond ((or (null theLine) (check-end-comment theLine))
             (return-from accumulate-until (values nil result)))
            ((check-keys theLine theKeys)
             (return-from accumulate-until (values theLine result))))
      (unless (string= theLine "")
        (setf result (concatenate 'string result theLine (string #\newLine)))))))

(defun check-chars (theChar theKeys)
  (if (listp theKeys)
    ;; Return true if any key matches.
    (dolist (c theKeys)
      (when (char= theChar c)
        (return-from check-chars t)))
    (char= thechar theKeys)))

;;; Returns the position at which we stopped searching whether we missed it.

(defun string-skip-until (theString startPos theKeys &optional missedItWhenAnyOfTheseShowsUp)
  (let ((len (length theString))
        theChar)
    (loop
      (when (>= startPos len) (return-from string-skip-until (values nil t)))
      (setf theChar (aref theString startPos))
      (cond ((check-chars theChar theKeys) (return-from string-skip-until (values startPos nil)))
            ((and missedItWhenAnyOfTheseShowsUp (check-chars theChar missedItWhenAnyOfTheseShowsUp))
             (return-from string-skip-until (values startPos t))))
      (incf startPos))))

;;; _______________________________ 
;;; Processing of general text blocks
;;;   - Expands macros
;;;   - Deals with "executable" code fragments
;;; _______________________________ 

(defmacro macroExpansionRequired? (str)
  `(position #\$ ,str))

(defmacro codeCheckingRequired? (str)
  `(position #\© ,str))

(defun get-current-record-id ()
  (cond
   (*current-handler-str*
    (values *current-handler-str* :handler *current-object-str*))
   
   (*current-property-str*
    (values *current-property-str* :property *current-object-str*))
   
   (*current-object-str*
    (values *current-object-str* :object *current-object-str*))
   
   (*current-function-str*
    (values *current-function-str* :function nil))
   
   (*current-global-str*
    (values *current-global-str* :global nil))
   
   (*current-constant-str*
    (values *current-constant-str* :constant nil))))


(defun process-general-text (str)
  ;; Expand macros
  (when (macroExpansionRequired? str)
    (multiple-value-bind (recordName recordType recordObject) (get-current-record-id)
      (setq str (expandDefines str recordName recordType recordObject))))
  ;; Process code fragments
  (when (codeCheckingRequired? str)
    (setq str (process-executable-code str)))
  ;; Maybe more stuff in the future...
  str)


(defun process-xref-text (str)
  (if (macroExpansionRequired? str)
    ;; Just expand macros
    (multiple-value-bind (recordName recordType recordObject) (get-current-record-id)
      (expandDefines str recordName recordType recordObject))
    str))


(defun find-matching-bracket (str openBracket closeBracket startPos)
  (let ((len (length str))
        (i startPos)
        (openCount 0)
        (openLen (length openBracket))
        (closeLen (length closeBracket))
        (openChar1 (char openBracket 0))
        (closeChar1 (char closeBracket 0))
        c)
    (loop
      (when (eql i len) (return len))
      (setq c (char str i))
      (cond
       
       ((and (eq c closeChar1)
             (string= str closeBracket :start1 (1+ i) :end1 (min (+ i closeLen) len) :start2 1))
        (when (eql openCount 0) (return i))
        (decf openCount)
        (incf i closeLen))
       
       ((and (eq c openChar1)
             (string= str openBracket :start1 (1+ i) :end1 (min (+ i openLen) len) :start2 1))
        (incf openCount)
        (incf i openLen))
       
       (t
        (incf i))))))

(defun process-executable-code (str)
  (let ((i 0)
        start end)
    (loop
      (unless (setq start (search "[[©" str :start2 i :test #'eq))
        (return))
      (incf start 2)
      (setq str (concatenate 'string (subseq str 0 start) (subseq str (1+ start)))
            end (find-matching-bracket str "[[" "]]" start)
            i end)
      ;;*** HERE'S WHERE WE'D PROCESS THE CODE IN (subseq str start end) !!!
      )
    str))

;;; _______________________________ 
;;; Parsing functions
;;; _______________________________ 

(defun parse-macros ()
  (parseDefineFolder *macros-folder* *macros-outfile* t))
   
;;; This is the main function. It runs through the files in alphabetical order calling parse-file.

(defun parse-folder ()
  (let ((theFiles (directory *source-folder*)))
    ;; If the file already exists, get rid of it!
    (when (probe-file *wood-file*)
      (delete-file *wood-file*))
    ;; Now make the new file.
    (cl-user::with-wood-doc-file 
     (*wood-file*)
     ;; Loop through the files. If the compile is required, do it!
     (format t "~%Documentation Compilation started at ~a.~2%" (timeString today))
     (dolist (aFile theFiles)
       (when (compile-required aFile)
         (parse-file aFile)))
     (format t "~%Documentation Compilation ended at ~a.~2%" (timeString today)))))

;;; Opens the file and calls parse-object to do the work. Handles special cases of functions, 
;;; variables and constants files.

(defun parse-file (theFile)
  (when (probe-file theFile)
    ;; Notify the user and stamp the file.
    (format t "~%Parsing   ~a..." theFile)
    (with-open-file (inFile thefile :direction :input)
      (let ((theName (pathname-name thefile)))
        (cond ((string-equal theName "Globals") (parse-variables inFile))
              ((string-equal theName "Constants") (parse-constants inFile))
              ((string-equal theName "Functions") (parse-functions inFile))
              (t (parse-object inFile)))))))

;;; _______________________________ 
;;; Parsing Function files.
;;; _______________________________ 

;;; called when theLine is a "FUNCTION:..." line.

(defun parse-one-function (inFile theLine)
  (let (missedIt? 
        headerLine 
        blockString
        ;; The Fields.
        name info description example seeAlso)
    ;; [1] Parse header.
    (multiple-value-setq (name info) (parse-handler-header theLine))
    ;; Jump to the start of fields.
    (multiple-value-setq (theLine missedIt?) (skip-until inFile *header-delimiter*))
    (setf theLine (get-line inFile))
    ;; [2] Parse fields.
    (loop 
      ;; Look for property start unless already there.
      (cond ((null theLine) (return))
            ((string-prefix? theLine *property-delimiter*))
            ((string-prefix? theLine *header-delimiter*) (setf missedIt? t))
            (t 
             ;; We were not there. Jump to it.
             (multiple-value-setq (theLine missedIt?) 
               (skip-until inFile *property-delimiter* *header-delimiter*))))
      (when missedIt?
        ;; This object has no fields. Return.
        (return))
      (setf headerLine (copy-seq theLine))
      ;; Get the text.
      (multiple-value-setq (theLine blockString)
        (accumulate-until infile (list *property-delimiter* *header-delimiter*)))
      ;; Bind text to variable.
      (cond ((string-prefix? headerLine "__ARGUMENTS:") (parse-arguments blockString))
            ((string-prefix? headerLine "__DESCRIPTION:") (setf description blockString))
            ((string-prefix? headerLine "__EXAMPLE:") (setf example blockString))
            ((string-prefix? headerLine "__SEE ALSO:") (setf seeAlso blockString))))
    ;; [3] Register and return the last line.
    (register-function name info (reverse *current-args*) 
                      description example seeAlso)
    theLine
    ))

(defun parse-functions (inFile)
  (let ((theLine ""))
    ;; Do each function.
    (loop
      ;; Done! Next line will be a function heading.
      (setf theLine (get-line inFile))
      (unless theLine (return))
      (when (string-prefix? theLine "FUNCTION:")
        (parse-one-function inFile theLine)
        (event-dispatch)
        ;; Clear the arg variables.
        (setf *current-args* nil))
      )))

;;; _______________________________ 
;;; Parsing Constants and Variables Files
;;; _______________________________ 

(defun parse-one-variable (inFile theLine register-function)
  (let (missedIt? 
        headerLine 
        blockString
        ;; The Fields.
        name info description example seeAlso)
    ;; [1] Parse header.
    (multiple-value-setq (name info) (parse-handler-header theLine))
    ;; Jump to the start of fields.
    (multiple-value-setq (theLine missedIt?) (skip-until inFile *header-delimiter*))
    (setf theLine (get-line inFile))
    ;; [2] Parse fields.
    (loop 
      ;; Look for property start unless already there.
      (cond ((null theLine) (return))
            ((string-prefix? theLine *property-delimiter*))
            ((string-prefix? theLine *header-delimiter*) (setf missedIt? t))
            (t 
             ;; We were not there. Jump to it.
             (multiple-value-setq (theLine missedIt?) 
               (skip-until inFile *property-delimiter* *header-delimiter*))))
      (when missedIt?
        ;; This object has no fields. Return.
        (return))
      (setf headerLine (copy-seq theLine))
      ;; Get the text.
      (multiple-value-setq (theLine blockString)
        (accumulate-until infile (list *property-delimiter* *header-delimiter*)))
      ;; Bind text to variable.
      (cond ((string-prefix? headerLine "__DESCRIPTION:") (setf description blockString))
            ((string-prefix? headerLine "__EXAMPLE:") (setf example blockString))
            ((string-prefix? headerLine "__SEE ALSO:") (setf seeAlso blockString))))
    ;; [3] Register and return the last line.
    (funcall register-function name info description example seeAlso)
    theLine
    ))

(defun parse-variables (inFile)
  (let ((theLine ""))
    ;; Do each variable.
    (loop
      ;; Done! Next line will be a variable heading.
      (setf theLine (get-line inFile))
      (unless theLine (return))
      (when (string-prefix? theLine "GLOBAL:")
        (parse-one-variable inFile theLine 'register-variable)
        (event-dispatch))
      )))

(defun parse-constants (inFile)
  (let ((theLine ""))
    ;; Do each constant.
    (loop
      ;; Done! Next line will be a constant heading.
      (setf theLine (get-line inFile))
      (unless theLine (return))
      (when (string-prefix? theLine "CONSTANT:")
        (parse-one-variable inFile theLine 'register-constant)
        (event-dispatch))
      )))

;;; _______________________________ 
;;; Parsing handler/property headers.
;;; _______________________________ 

;;; Given something like: "HANDLER (object system event, protocol): initialize"
;;; it returns 2 values: "initialize" and "object system event, protocol"

(defun parse-handler-header (theLine)
  (let ((startPos 0) endPos
        missedIt?
        ;; the fields.
        handlername info)
    ;; [1] find info.
    (multiple-value-setq (startPos missedIt?) 
      (string-skip-until theLine startPos #\( #\:))
    (unless missedIt?
      ;; Look for the end paren.
      (multiple-value-setq (endPos missedIt?) 
        (string-skip-until theLine startPos #\) #\:))
      (if missedIt? 
        (error "Found open paren but no matching close paren in handler description.")
        (setf info (subseq theLine (1+ startPos) endPos))))
    ;; [2] Find name.
    (multiple-value-setq (startPos missedIt?) 
      (string-skip-until theLine startPos #\:))
    (if missedIt? 
      (error "This handler has no name!")
      (setf handlername (trim (subseq theLine (1+ startPos) (length theLine)))))
    ;; Return both things.
    (values handlername info)))

;;; _______________________________ 
;;; Parsing Arguments
;;; _______________________________ 

;;; StartPos = 1+ the place where the ¥ was.
;;; The name and the required will always be there. The type might not be.
;;; Returns the position of the next bullet or false.
;;; Makes the argument and pushes it into the list.

(defun parse-an-argument (argumentsBlock startPos)
  (let (endPos 
        missedIt?
        ;; The fields.
        name type required description)
    ;; Look for the paren or the comma.
    (multiple-value-setq (endPos missedIt?) 
      (string-skip-until argumentsBlock startPos '(#\( #\,)))
    (setf name (trim (subseq argumentsBlock startPos endPos)))
    ;; Look for the paren.
    (setf startPos (1- endPos))
    (multiple-value-setq (endPos missedIt?) 
      (string-skip-until argumentsBlock startPos #\( #\,))
    (unless missedIt?
      (setf startPos (1+ endPos))
      ;; look for the end of the paren.
      (multiple-value-setq (endPos missedIt?) 
        (string-skip-until argumentsBlock startPos #\) #\,))
      ;; get the type info.
      (setf type (trim (subseq argumentsBlock startPos endPos))))
    ;; Skip the comma and get the required.
    (setf startPos (1- endPos))
    (multiple-value-setq (startPos missedIt?) 
      (string-skip-until argumentsBlock startPos #\,))
    ;; find the newLine and get stuff in between.
    (multiple-value-setq (endpos missedIt?) 
      (string-skip-until argumentsBlock startPos #\Newline))
    (setf required (trim (subseq argumentsBlock (1+ startPos) (1- endPos))))
    ;; Everything between here and the next bullet is the description.
    (setf startPos endPos)
    (multiple-value-setq (endpos missedIt?) 
      (string-skip-until argumentsBlock startPos *arg-delimiter*))
    ;; Now, either we hit the next arg, or endPos is false in which
    ;; case everything to the end is the description.
    (if endPos
      (setf description (trim (subseq argumentsBlock startPos (1- endPos))))
      (setf description (trim (subseq argumentsBlock startPos (length argumentsBlock)))))
    ;; Register the arg and return the next startpos.
      (register-argument name type required description)
    endPos))
    
;;; Takes a string containing all the arg, parses it and creates all the arg objects required.

(defun parse-arguments (argumentsBlock)
  (unless (zerop (length argumentsBlock))
    (let ((curpos 1))
      (loop
        (setf curpos (parse-an-argument argumentsBlock curPos))
        (unless curpos (return))
        (incf curpos)))))

;;; _______________________________ 
;;; Parsing object files.
;;; _______________________________ 

(defmacro read-from-string-into-package (str pckg)
  `(let ((*package* (find-package ,pckg)))
     (read-from-string ,str)))

(defmacro tis-a-sk8-object? (name)
  `(and (boundp (read-from-string-into-package ,name :sk8))
        (or (typep (symbol-value (read-from-string-into-package ,name :sk8)) 'sk8::Object)
            (typep (symbol-value (read-from-string-into-package ,name :sk8)) 'built-in-class))))

;;; Parses the object. Then looks for property/handler blocks and calls the 
;;; appropriate parse functions.

(defun parse-object (inFile)
  (let ((theLine (get-line infile))
        name owner description example seeAlso)
    ;; Do the object itself.
    (unless theLine (return-from parse-object))
    ;; We have a valid line. Parse object description.
    (multiple-value-setq (name owner description example seeAlso)
      (parse-object-description infile))
    ;; Error checking. Return if there is any problem.
    (let ((everythingOk? t))
      (cond ((not (tis-a-sk8-object? name))
             (format t "~%   WARNING: object ~a does not exist." name)
             (setf everythingOk? nil))
            ((and (boundp 'COMMON-LISP-USER::*doc-root*)
                  (cl-user::fetch-object (string-upcase name)))
             (format t "~%   WARNING: object ~a already documented from other file." name)
             (setf everythingOk? nil)))
      (unless everythingOk? 
        (format t "~%   *** FILE SKIPPED ***")
        (return-from parse-object nil)))
    ;; Now do the properties and handlers.
    (let ((*current-object-str* name))
      (loop
        ;; Done! Next line will be a property (or handler) heading.
        (setf theLine (get-line inFile))
        (unless theLine (return))
        (cond ((string-prefix? theLine "PROPERTY") (parse-Property inFile theLine name))
              ((string-prefix? theLine "HANDLER") (parse-Handler inFile theLine name)))
        (event-dispatch)
        ;; Clear the arg variables.
        (setf *current-args* nil)
        ))
    ;; At this point all the handlers and properties are in their lists. Save the object.
    (register-object name nil owner description example seeAlso
                     (reverse *current-handlers*)
                     (reverse *current-properties*))
    ;; Clear the variables.
    (setf *current-handlers* nil)
    (setf *current-properties* nil)))

;;; Gets called with file pointer right before the line that has the object header.
;;; Returns when the eof or the next "=============..." is hit.

(defun parse-object-description (inFile)
  ;; Read until we hit the "Object:"
  (let (headerLine 
        theLine blockString missedIt? 
        ;; Fields.
        obj owner description example seeAlso)
    ;; Set the object.
    (multiple-value-setq (theLine missedIt?) (skip-until infile "OBJECT:"))
    (setf obj (trim (subseq theLine 8 (length theLine))))
    ;; Get the owner (must be on the next line!)
    (setf theLine (get-line inFile))
    (unless (string-prefix? theLine *header-delimiter*)
      (setf owner (trim theLine))
      ;; Now jump the bottom of the header.
      (multiple-value-setq (theLine missedIt?) (skip-until infile *header-delimiter*)))
    (setf theLine (get-line inFile))
    ;; Look for the field.
    (loop 
      ;; Look for property start unless already there.
      (cond ((null theLine) (return))
            ((string-prefix? theLine *property-delimiter*))
            ((string-prefix? theLine *header-delimiter*) (setf missedIt? t))
            (t 
             ;; We were not there. Jump to it.
             (multiple-value-setq (theLine missedIt?) 
               (skip-until inFile *property-delimiter* *header-delimiter*))))
      (when missedIt?
        ;; This object has no fields. Return.
        (return))
      (setf headerLine (copy-seq theLine))
      ;; Get the text.
      (multiple-value-setq (theLine blockString)
        (accumulate-until infile (list *property-delimiter* *header-delimiter*)))
      ;; Bind text to variable.
      (cond ((string-prefix? headerLine "__DESCRIPTION:") (setf description blockString))
            ((string-prefix? headerLine "__EXAMPLE:") (setf example blockString))
            ((string-prefix? headerLine "__SEE ALSO:") (setf seeAlso blockString))))
    ;; Write out the object data.
    (values obj owner description example seeAlso)))
      
;;; theLine is the HANDLER:... line.

(defun parse-handler (inFile theLine &optional name)
  (let (missedIt? 
        headerLine 
        blockString
        ;; The Fields.
        handlerName info description example seeAlso)
    ;; [1] Parse header.
    (multiple-value-setq (handlerName info) (parse-handler-header theLine))
    ;; Make sure this should not be documented as a property.
    (when name
      (let ((theObj (read-from-string-into-package name :sk8)))
        (when (boundp theObj)
          (when (tis-a-property? (symbol-value theObj) 
                                 (read-from-string-into-package handlerName :sk8))
            (format t "~%   WARNING: handler ~a should be documented as a property." handlerName)))))
    ;; Jump to the start of fields.
    (multiple-value-setq (theLine missedIt?) (skip-until inFile *header-delimiter*))
    (setf theLine (get-line inFile))
    ;; [2] Parse fields.
    (loop 
      ;; Look for property start unless already there.
      (cond ((null theLine) (return))
            ((string-prefix? theLine *property-delimiter*))
            ((string-prefix? theLine *header-delimiter*) (setf missedIt? t))
            (t 
             ;; We were not there. Jump to it.
             (multiple-value-setq (theLine missedIt?) 
               (skip-until inFile *property-delimiter* *header-delimiter*))))
      (when missedIt?
        ;; This object has no fields. Return.
        (return))
      (setf headerLine (copy-seq theLine))
      ;; Get the text.
      (multiple-value-setq (theLine blockString)
        (accumulate-until infile (list *property-delimiter* *header-delimiter*)))
      ;; Bind text to variable.
      (cond ((string-prefix? headerLine "__ARGUMENTS:") (parse-arguments blockString))
            ((string-prefix? headerLine "__DESCRIPTION:") (setf description blockString))
            ((string-prefix? headerLine "__EXAMPLE:") (setf example blockString))
            ((string-prefix? headerLine "__SEE ALSO:") (setf seeAlso blockString))))
    ;; [3] Register and return the last line.
    (register-handler handlerName info description 
                      (reverse *current-args*) example seeAlso)
    theLine
    ))

;;; TheLine is the "PROPERTY:..." line.

(defun parse-property (inFile theLine &optional name)
  (let (missedIt? 
        headerLine 
        blockString
        ;; The fields:
        propertyName info description
        getterArguments getterDescription
        setterArguments setterDescription
        example seeAlso)
    ;; [1] Parse header.
    (multiple-value-setq (propertyname info) (parse-handler-header theLine))
    ;; If this property has no setter, issue a warning.
    (when name
      (let ((theObj (read-from-string-into-package name :sk8)))
        (when (boundp theObj)
          (unless (tis-a-property? (symbol-value theObj) 
                                   (read-from-string-into-package propertyName :sk8))
            (format t "~%   WARNING: property ~a does not have a setter and a getter." propertyname)))))
    ;; Jump to individual fields.
    (multiple-value-setq (theLine missedIt?) (skip-until inFile *header-delimiter*))
    (setf theLine (get-line inFile))
    ;; [2] Parse fields.
    (loop 
      ;; Look for property start unless already there.
      (cond ((null theLine) (return))
            ((string-prefix? theLine *property-delimiter*))
            ((string-prefix? theLine *header-delimiter*) (setf missedIt? t))
            (t 
             ;; We were not there. Jump to it.
             (multiple-value-setq (theLine missedIt?) 
               (skip-until inFile *property-delimiter* *header-delimiter*))))
      (when missedIt?
        ;; This object has no fields. Return.
        (return))
      (setf headerLine (copy-seq theLine))
      ;; Get the text.
      (multiple-value-setq (theLine blockString)
        (accumulate-until infile (list *property-delimiter* *header-delimiter*)))
      ;; Bind text to variable.
      (cond ((string-prefix? headerLine "__DESCRIPTION:") (setf description blockString))
            ((string-prefix? headerLine "__GETTER ARGUMENTS:")
             ;; Compile the args, save in variable and clear global.
             (parse-arguments blockString)
             (setf getterArguments (reverse *current-args*))
             (setf *current-args* nil))
            ((string-prefix? headerLine "__GETTER DESCRIPTION:") (setf getterDescription blockString))
            ((string-prefix? headerLine "__SETTER ARGUMENTS:")
             ;; Compile the args, save in variable and clear global.
             (parse-arguments blockString)
             (setf setterArguments (reverse *current-args*))
             (setf *current-args* nil))
            ((string-prefix? headerLine "__SETTER DESCRIPTION:") (setf setterDescription blockString))
            ((string-prefix? headerLine "__EXAMPLE:") (setf example blockString))
            ((string-prefix? headerLine "__SEE ALSO:") (setf seeAlso blockString))))
    ;; [3] Register and return the last line.
    (register-property propertyName info description
                       getterArguments getterDescription
                       setterArguments setterDescription
                       example seeAlso)
    theLine
    ))


#|
	Change History (most recent last):
	1		8/1/94	Hernan	New file. The parser that outputs documentation
							into our wood db.
	2		8/2/94	Hernan	Changing the folder that contains the documentation.
	3		8/2/94	chip	added stubs for dealing with syntax-checking of code fragments; made scanners recognize comment at end of file
	4		8/2/94	Hernan	Calling register-handler with the right args.
	5		8/2/94	Hernan	Fixing the pathname to the macros file.
	6		8/5/94	Hernan	The required argument is now stored as t or nil.
	7		8/8/94	chip	tweaked things to support the new macro args and the new XREFS macro capability
	8		8/10/94	Hernan	Adding calls to event-dispatch to avoid freezing
							your Mac while this runs. Also added two macros
							to avoid calling the macro expansion and code
							checking code when it is not required to do so.
	9  	 8/24/94	Hernan  	1181020: Making the doc file be inside the SK8 folder.
	10 	 9/16/94	Hernan  	Really stupid bug that ate all see also notes away.
	11 	11/16/94	sidney  	move SK8 Documentation to the new SK8 Resources folder
	12 	11/21/94	Hernan  	Using the right function to tell whether a file needs
							to be recompiled.
	13 	11/21/94	Hernan  	Using the right function to tell whether a file needs
							to be recompiled.
	14 	12/14/94	Hernan  	Ooops! Was missing all virtual properties.
	15 	12/14/94	Hernan  	Now fixing it right!
	16 	 2/22/95	sidney  	1221621: no more SK8 Resources folder
	17 	 3/22/95	Hernan  	Adding some error checking to deal with non
							existant objects and duplicate object files.
	18 	 4/ 5/95	Hernan  	Adding error checking. The case we look for is a 
							handler that should be documented as a property
							and viceversa.
	2  	 2/ 5/96	Hernan  	Added functionality to produce a QuickView version of the
						docs. Tried to change this code as little as possible.
	3  	 2/ 6/96	Hernan  	tis-a-sk8-object? tests for SK8 objectness using typep.
	4  	 2/ 8/96	Hernan  	Moved tis-a-property back.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
