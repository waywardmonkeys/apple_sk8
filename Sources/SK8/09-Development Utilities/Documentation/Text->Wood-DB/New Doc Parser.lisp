;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  2-01-96  11:21 pm
                  SK8::EMPTYALLDBS SK8::GENERATESCRIPT)


;;; Uses a FileMaker Pro Database to store the documentation database.
;;; Lots of AppleScript will be required. 

;;; _______________________________ 
;;; Apple Script Helpers:
;;; _______________________________ 

(defparameter *script-header* (format nil "tell application \"FileMaker Pro\"~%"))

;;; Opens filemaker and all the DBs. A generally useful thing, also for the online help.

(new AppleScript :project sk8 :objectname "OpenFileMakerAndDBs")

#|

(setf (scriptText OpenFileMakerAndDBs)
"tell application \"Finder\"
      open application \"FileMaker Pro\"
      open document \"Jose Arcadio Segundo:SK8 on MCL3.0:SK8 Documentation DBs:Objects\"
end tell
"
)

|#

;;; Empties the records of all DBs, to start parsing anew. 

(new AppleScript :project sk8 :objectname "EmptyAllDBs")

(setf (scriptText EmptyAllDBs)
"tell application \"FileMaker Pro\"
      delete every record of database \"Objects\"
      delete every record of database \"Properties\"
      delete every record of database \"Handlers\"
      delete every record of database \"Arguments\"
      delete every record of database \"Functions\"
      delete every record of database \"Globals\"
end tell
"
)

;;; General script to add new record to the database. 

(new AppleScript :project sk8 :objectname "AddRecordScript")

(defun False->emptyStr (args)
  (mapcar #'(lambda (x) (or x "")) args))

(defmacro remove-quoted-strings (aString)
  `(if (stringp ,aString)
     (substitute #\' #\" ,aString)
     ,aString))
  
(defun build-string-list (args)
  (let ((result "{")
        (firstDone? nil))
    (dolist (oneArg args)
      (setf result
            (concatenate 'string result
                         (if firstDone?
                           ","
                           (progn (setf firstDone? t)
                                  ""))
                         (if (and (stringp oneArg) (position #\Newline oneArg))
                           (format nil "\"~a\"" (simpleObjectString (remove-quoted-strings oneArg)))
                           (objectString (remove-quoted-strings oneArg))))))
    (concatenate 'string result "}")))

(defun add-a-record (database &rest args)
  (let ((resultString nil))
    (setf resultString 
          (concatenate 'string *script-header*
                       (format nil "   Create new record at Database \"~a\" with data ~a~
                                    ~%end tell~
                                    ~%"
                               database (build-string-list (false->emptyStr args)))))
    (setf (scriptText AddRecordScript) resultString)
    (format t "~%..Adding a record to the ~a database." database)
    ;; (print resultString)
    (execute AddRecordScript)
    ))

;;; _______________________________ 
;;; The main functions:
;;; _______________________________ 

(defun register-object (obj info owner description example seeAlso handlers properties)
  (declare (ignore handlers properties))
  (let ((*current-object-str* obj))
    (add-a-record "Objects"
                  "SK8"
                  obj
                  info
                  owner
                  (process-general-text description)
                  (process-general-text example)
                  (process-xref-text seeAlso))))

(defun register-argument (name type required description)
  (let ((theArg (list name type required (process-general-text description))))
    ;; and save it.
    (push theArg *current-args*)))

(defun save-argument (obj functionName order name type required description)
  (add-a-record "Arguments"
                "SK8"
                obj
                functionName
                order
                name
                type
                required
                (process-general-text description)))

;;; Saves all args assigning a number to each one. This is how the order is implemented. 

(defun save-arguments (obj name arguments)
  (dotimes (i (length arguments))
    (destructuring-bind (argname type required description) (car arguments)
      (add-a-record "Arguments"
                    "SK8"
                    obj
                    name
                    i
                    argname
                    type
                    required
                    (process-general-text description)))
    (setf arguments (cdr arguments))
    ))

(defun register-handler (name info description arguments example seeAlso)
  (let ((obj *current-object-str*)
        (*current-handler-str* name))
    ;; Save the handler.
    (add-a-record "Handlers"
                  "SK8"
                  obj
                  name
                  info
                  (process-general-text description)
                  (process-general-text example)
                  (process-xref-text seeAlso))
    ;; Save the arguments. 
    (save-arguments obj name arguments)))

(defun register-property (name info description 
                                 getter-arguments getter-description
                                 setter-arguments setter-description
                                 example seeAlso)
  (let ((*current-property-str* name)
        (obj *current-object-str*))
    ;; Save the property.
    (add-a-record "Properties"
                  "SK8"
                  obj
                  name
                  info
                  (process-general-text description)
                  (process-general-text getter-description)
                  (process-general-text setter-description)
                  (process-general-text example)
                  (process-xref-text seeAlso))
    ;; Save the args of the getter and setter. 
    (save-arguments obj name getter-arguments)
    (save-arguments obj (format nil "set ~a" name) setter-arguments)))

(defun register-function (name info arguments description example seeAlso)
  (let ((*current-function-str* name))
      ;; Save the function.
    (add-a-record "Functions"
                  "SK8"
                  name
                  info
                  (process-general-text description)
                  (process-general-text example)
                  (process-xref-text seeAlso))
    ;; Save the arguments. 
    (save-arguments "" name arguments)))

(defun register-constant (name info description example seeAlso)
  (let ((*current-constant-str* name))
    (add-a-record "Globals"
                  "SK8"
                  "Constant"
                  name
                  info
                  (process-general-text description)
                  (process-general-text example)
                  (process-xref-text seeAlso))))

(defun register-variable (name info description example seeAlso)
  (let ((*current-global-str* name))
    (add-a-record "Globals"
                  "SK8"
                  "Variable"
                  name
                  info
                  (process-general-text description)
                  (process-general-text example)
                  (process-xref-text seeAlso))))

;;; Other patches to the source. 

(defun parse-folder ()
  (let ((theFiles (list "ccl;Documentation:Text:Function.doc"))) ;; (directory *source-folder*)))
    ;; Open file maker and the DBs.
    nil
    ;; Empty all DBs.
    (execute EmptyAllDBs) 
    ;; Do the compilation.
    (format t "~%Documentation Compilation started at ~a.~2%" (timeString today :seconds t))
    (dolist (aFile theFiles)
      (when (compile-required aFile)
        (parse-file aFile)))
    (format t "~%Documentation Compilation ended at ~a.~2%" (timeString today :seconds t))
    ;; Quit FileMaker.
    nil
    ))

(defun cl-user::fetch-Object (name)
  (declare (ignore name))
  nil)


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
