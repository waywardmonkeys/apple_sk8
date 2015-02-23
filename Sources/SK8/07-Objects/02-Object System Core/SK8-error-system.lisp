;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


(in-package :SK8DEV)

#|
  Implementation of the SK8 error (condition) system:
 The CommonLisp spec says that error conditions are supposed to be signaled via a call
 to error, and not simply by calling signal. MCL gives us a hook for calls to error
 that are not handled, by letting us define an application-error method on our application
 class. If we assume that error conditions we are interested in are always signaled via error
 and that the MCL code that is running will not set up any handlers, then we can implement
 a SK8 condition system as follows:

 In application-error, look up the condition being passed in to find the corresponding
 SK8 condition object. If one is found, SK8-Signal that SK8 condition, passing it the
 Lisp condition instance.

 The lookup can be done by setting up a hash table that maps Lisp conditions to their
 corresponding SK8 conditions. The table is fixed and is built at SK8 build time. Only
 those SK8 condition objects will take a Lisp condition instance as an argument to
 SK8-Signal and each will have a definition of a method that copies property values from
 the Lisp condition instance to the SK8 condition object.

 SK8-Signal looks a lot like the MCL implementation of Signal. It goes through a list in
 a global (like %handlers%) popping items and looking for one that refers to a condition
 that is an ancestor of the one being signalled. The elements of the list are unique conses
 that are catch tags, set where the condition handler is defined. The first one that is
 found is thrown to.

 The on error form sets up a catch and pushes the catch tag on the global that is used
 for the purpose. To make threads work, the global must be separately bound in each thread.
 A SK8Script on error form compiles into code similar to handler-case. The generated code
 produces a cons containing the symbol name of the condition, binds the global, pushes it
 on the global and sets up a catch with the cons as the tag. It isn't necessary for the
 condition object to exist at SK8Script compile time if the name will be used as the
 object name.

|#

#| Here is the error hierarchy as was defined in SK8 0.9, for reference
    (SK8::Error                     SK8::Condition            errorID)
    (SK8::GeneralError              SK8::Error                strings objects)
    (SK8::ScriptSyntaxError         SK8::Error                input errorPosition description)
    (SK8::ArithmeticError           SK8::Error                operation operands)
    (SK8::ProgrammaticError         SK8::Error                )
    (SK8::GeneralProgrammaticError  SK8::ProgrammaticError    )
    (SK8::DisposedObjectError       SK8::ProgrammaticError     object)
    (SK8::EventModeError            SK8::ProgrammaticError     eventMode)
    (SK8::TypeMismatchError         SK8::ProgrammaticError     object expectedType)
    (SK8::PropertyTypeMismatchError SK8::TypeMismatchError     ownerObject propertyName)
    (SK8::ArgumentTypeMismatchError SK8::TypeMismatchError     handlerName argumentName)
    (SK8::FileSystemError           SK8::Error                )
    (SK8::FileError                 SK8::FileSystemError       file)
    (SK8::ProjectImproperlyClosedError SK8::FileError                 )
    (SK8::SystemError               SK8::Error                )
    (SK8::ConditionSystemError      SK8::SystemError           originalCondition response #~errorCause)
    (SK8::MemoryFullError           SK8::SystemError               )
    (SK8::UnknownError              SK8::SystemError           arguments)
    (SK8::CompilationError          SK8::Error                )))
    (SK8::UnboundVariableError      SK8::ProgrammaticError      variableName)
    (SK8::UndefinedHandlerError     SK8::ProgrammaticError      handlerName arguments)    
    (SK8::IncorrectArgumentsError   SK8::ProgrammaticError      arguments handlerName)    
    (SK8::IncorrectSubscriptsError  SK8::ProgrammaticError      array rank)
    (SK8::IndexOutOfBoundsError     SK8::ProgrammaticError      index array
    (SK8::CoercionError             SK8::ProgrammaticError      object type)
    (SK8::NotAHandlerError          SK8::ProgrammaticError      object)
    (SK8::CantChangeConstantError   SK8::ProgrammaticError      constantName)
    (SK8::DivisionByZeroError       SK8::ArithmeticError          )
    (SK8::ArithmeticOverflowError   SK8::ArithmeticError          )
    (SK8::OSHeapFullError           SK8::MemoryFullError          )
    (SK8::FileMemoryFullError       SK8::MemoryFullError          )
    (SK8::SK8HeapFullError          SK8::MemoryFullError          )
    (SK8::StackOverflowError        SK8::MemoryFullError          )
    (SK8::NumberMemoryFullError     SK8::MemoryFullError          )
    (SK8::FileNotFoundError         SK8::FileError               file)
    (SK8::EndOfFileError            SK8::FileError               file)
    (SK8::IllegalFilenameError      SK8::FileError               file)
    (SK8::IODriverError             SK8::SystemError              )
    (SK8::DiskError                 SK8::FileSystemError         disk)
    (SK8::ClockError                SK8::SystemError              )
    (SK8::SerialPortError           SK8::SystemError              )
    (SK8::AppleTalkError            SK8::SystemError              )
    (SK8::ScrapManagerError         SK8::SystemError              )
    (SK8::MemoryManagerError        SK8::SystemError              )
    (SK8::DirectoryError            SK8::FileSystemError         SK8::directory)
    (SK8::MenuManagerError          SK8::SystemError              )
    (SK8::ResourceError             SK8::SystemError              )
    (SK8::SoundManagerError         SK8::SystemError              )
    (SK8::PPCToolboxError           SK8::SystemError              )
    (SK8::AppleEventError           SK8::SystemError             oserr)
    (SK8::MovieError                SK8::SystemError              )
    (SK8::MovieDataHandlerError     SK8::MovieError               )
    (SK8::ComponentManagerError     SK8::SystemError              )
    (SK8::ImageCompressionError     SK8::SystemError              )
    (SK8::MovieSequenceGrabberError SK8::MovieError               )
    (SK8::MovieControllerError      SK8::MovieError               )

|#

;; define the condition system

(new object :objectname "Condition" :project sk8 :properties '(sk8::MCLCondition))
(new SK8:Condition :objectname "Error" :project SK8)
(new SK8:Error :objectname "GeneralError" :project SK8 :properties '(sk8::strings sk8::objects))
(new SK8:Error :objectname "ArithmeticError" :project SK8 :properties '(sk8::operation sk8::operands))
(new SK8:GeneralError :objectname "ProgrammaticError" :project SK8)
(new SK8:ProgrammaticError :objectname "GeneralProgrammaticError" :project SK8)
(new SK8:ProgrammaticError :objectname "DisposedObjectError" :project SK8 :properties '(sk8::object))
(new SK8:ProgrammaticError :objectname "EventModeError" :project SK8 :properties '(sk8::eventMode))
(new SK8:ProgrammaticError :objectname "TypeMismatchError" :project SK8 :properties '(sk8::object sk8::expectedType))
(new SK8:TypeMismatchError :objectname "PropertyTypeMismatchError" :project SK8 :properties '(sk8::ownerObject sk8::propertyName))
(new SK8:TypeMismatchError :objectname "ArgumentTypeMismatchError" :project SK8 :properties '(sk8::handlerName sk8::argumentName))
(new SK8:Error :objectname "FileSystemError" :project SK8)
(new SK8:FileSystemError :objectname "FileError" :project SK8 :properties '(sk8::file))
(new SK8:FileError :objectname "ProjectImproperlyClosedError" :project SK8)
(new SK8:Error :objectname "SystemError" :project SK8)
(new SK8:SystemError :objectname "MemoryFullError" :project SK8)
(new SK8:SystemError :objectname "UnknownError" :project SK8 :properties '(sk8::arguments))
(new SK8:Error :objectname "CompilationError" :project SK8)
(new SK8:ProgrammaticError :objectname "UnboundVariableError" :project SK8 :properties '(sk8::variableName))
(new SK8:ProgrammaticError :objectname "UndefinedHandlerError" :project SK8 :properties '(sk8::handlerName sk8::arguments))
(new SK8:ProgrammaticError :objectname "IncorrectArgumentsError" :project SK8 :properties '(sk8::arguments sk8::handlerName))
(new SK8:ProgrammaticError :objectname "IncorrectSubscriptsError" :project SK8 :properties '(sk8::array sk8::rank))
(new SK8:ProgrammaticError :objectname "IndexOutOfBoundsError" :project SK8 :properties '(sk8::index sk8::array))
(new SK8:ProgrammaticError :objectname "CoercionError" :project SK8 :properties '(sk8::object sk8::type))
(new SK8:ProgrammaticError :objectname "NotAHandlerError" :project SK8)
(new SK8:ProgrammaticError :objectname "CantChangeConstantError" :project SK8 :properties '(sk8::constantName))
(new SK8:ArithmeticError :objectname "DivisionByZeroError" :project SK8)
(new SK8:ArithmeticError :objectname "ArithmeticOverflowError" :project SK8)
(new SK8:MemoryFullError :objectname "OSHeapFullError" :project SK8)
(new SK8:MemoryFullError :objectname "FileMemoryFullError" :project SK8)
(new SK8:MemoryFullError :objectname "SK8HeapFullError" :project SK8)
(new SK8:MemoryFullError :objectname "StackOverflowError" :project SK8)
(new SK8:MemoryFullError :objectname "NumberMemoryFullError" :project SK8)
(new SK8:FileError :objectname "FileNotFoundError" :project SK8)
(new SK8:FileError :objectname "EndOfFileError" :project SK8)
(new SK8:FileError :objectname "IllegalFilenameError" :project SK8)
(new SK8:SystemError :objectname "IODriverError" :project SK8)
(new SK8:FileSystemError :objectname "DiskError" :project SK8 :properties '(sk8::disk))
(new SK8:SystemError :objectname "ClockError" :project SK8)
(new SK8:SystemError :objectname "SerialPortError" :project SK8)
(new SK8:SystemError :objectname "AppleTalkError" :project SK8)
(new SK8:SystemError :objectname "ScrapManagerError" :project SK8)
(new SK8:SystemError :objectname "MemoryManagerError" :project SK8)
(new SK8:FileSystemError :objectname "DirectoryError" :project SK8 :properties '(SK8::directory))
(new SK8:SystemError :objectname "MenuManagerError" :project SK8)
(new SK8:SystemError :objectname "ResourceError" :project SK8)
(new SK8:SystemError :objectname "SoundManagerError" :project SK8)
(new SK8:SystemError :objectname "PPCToolboxError" :project SK8)
(new SK8:SystemError :objectname "AppleEventError" :project SK8 :properties '(sk8::oserr))
(new SK8:SystemError :objectname "MovieError" :project SK8)
(new SK8:MovieError :objectname "MovieDataHandlerError" :project SK8)
(new SK8:SystemError :objectname "ComponentManagerError" :project SK8)
(new SK8:SystemError :objectname "ImageCompressionError" :project SK8)
(new SK8:MovieError :objectname "MovieSequenceGrabberError" :project SK8)
(new SK8:MovieError :objectname "MovieControllerError" :project SK8)


;; map MCL conditions to corresponding SK8 conditions.
;; The simplest way is to just set the MCLCondition property. That's enough to cause the SK8 error handling
;; system to catch the error properly and to use the MCL report-condition to generate the error message.
;; If you want the SK8Script error handler to have access to the condition's properties, you also have
;; to set them from the MCL condition's corresponding instance variables.
;; If that is not done in any of the following it is simply because I did not take the time to do it all.
;; Feel free to contribute.

(defmethod condition-to-sk8-condition ((c error))
  (new sk8:error :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c type-error))
  (new sk8:TypeMismatchError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c program-error))
  (new sk8:ProgrammaticError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c simple-error))
  (new sk8:GeneralError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c simple-type-error))
  (new sk8:TypeMismatchError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c ccl::simple-program-error))
  (new sk8:ProgrammaticError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c ccl::compile-time-error))
  (new sk8:CompilationError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c end-of-file))
  (new sk8:EndOfFileError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c file-error))
  (new sk8:FileSystemError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c unbound-variable))
  (new sk8:UnboundVariableError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c undefined-function))
  (new sk8:UndefinedHandlerError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c ccl::undefined-function-call))
  (new sk8:UndefinedHandlerError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c arithmetic-error))
  (new sk8:ArithmeticError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c division-by-zero))
  (new sk8:DivisionByZeroError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c floating-point-overflow))
  (new sk8:ArithmeticOverflowError :project sk8 :MCLCondition c))

(defmethod condition-to-sk8-condition ((c appleevent-error))
  (new sk8:AppleEventError :project sk8 :MCLCondition c))


#|

type-error TypeMismatchError
program-error ProgrammaticError

simple-error GeneralError
 simple-type-error TypeMismatchError
 ccl::simple-program-error ProgrammaticError
   simple-destructuring-error
   compile-time-program-error
   eval-program-error
 ccl::compile-time-error CompilationError

control-error
package-error
  no-such-package
  unintern-conflict-error
  import-conflict-error
  use-package-conflict-error
  export-conflict-error
  package-name-conflict-error    simple-error

stream-error
  end-of-file EndOfFileError
modify-read-only-buffer
file-error FileSystemError
cell-error
  unbound-variable  UnboundVariableError
  undefined-function  UndefinedHandlerError
  ccl::undefined-function-call    UndefinedHandlerError

arithmetic-error ArithmeticError
  division-by-zero  DivisionByZeroError
  floating-point-underflow
  floating-point-overflow ArithmeticOverflowError
  inexact-result
  invalid-operation

appleevent-error AppleEventError

eval-cant-cope

simple-reader-error

|#

#|
;;; Build the heirarchy of condition objects.
(loop for (name from-obj mcl-error)
      in '(("Error"                        SK8:Condition         error)
           ("GeneralError"                 SK8:Error             simple-error)
           ("ScriptSyntaxError"            SK8:Error             simple-error)
           ("ArithmeticError"              SK8:Error             arithmetic-error)
           ("ProgrammaticError"            SK8:GeneralError      simple-error)
           ("GeneralProgrammaticError"     SK8:ProgrammaticError simple-error)
           ("DisposedObjectError"          SK8:ProgrammaticError simple-error)
           ("EventModeError"               SK8:ProgrammaticError simple-error)
           ("TypeMismatchError"            SK8:ProgrammaticError type-error)
           ("PropertyTypeMismatchError"    SK8:TypeMismatchError type-error)
           ("ArgumentTypeMismatchError"    SK8:TypeMismatchError type-error)
           ("FileSystemError"              SK8:Error             file-error)
           ("FileError"                    SK8:FileSystemError   file-error)
           ("ProjectImproperlyClosedError" SK8:FileError         simple-error)
           ("SystemError"                  SK8:Error             simple-error)
           ("ConditionSystemError"         SK8:SystemError       simple-error)
           ("MemoryFullError"              SK8:SystemError       simple-error)
           ("UnknownError"                 SK8:SystemError       error)
           ("CompilationError"             SK8:Error             CCL::compile-time-error)
           ("UnboundVariableError"         SK8:ProgrammaticError unbound-variable)
           ("UndefinedHandlerError"        SK8:ProgrammaticError ccl::undefined-function-call)
           ("IncorrectArgumentsError"      SK8:ProgrammaticError simple-error)
           ("IncorrectSubscriptsError"     SK8:ProgrammaticError simple-error)
           ("IndexOutOfBoundsError"        SK8:ProgrammaticError simple-error)
           ("CoercionError"                SK8:ProgrammaticError simple-error)
           ("NotAHandlerError"             SK8:ProgrammaticError CCL::undefined-function-call)
           ("CantChangeConstantError"      SK8:ProgrammaticError simple-error)
           ("DivisionByZeroError"          SK8:ArithmeticError   division-by-zero)
           ("ArithmeticOverflowError"      SK8:ArithmeticError   floating-point-error)
           ("OSHeapFullError"              SK8:MemoryFullError   simple-error)
           ("FileMemoryFullError"          SK8:MemoryFullError   simple-error)
           ("SK8HeapFullError"             SK8:MemoryFullError   simple-error)
           ("StackOverflowError"           SK8:MemoryFullError   simple-error)
           ("NumberMemoryFullError"        SK8:MemoryFullError   simple-error)
           ("FileNotFoundError"            SK8:FileError         simple-error)
           ("EndOfFileError"               SK8:FileError         end-of-file)
           ("IllegalFilenameError"         SK8:FileError         simple-error)
           ("IODriverError"                SK8:SystemError       simple-error)
           ("DiskError"                    SK8:FileSystemError   simple-error)
           ("ClockError"                   SK8:SystemError       simple-error)
           ("SerialPortError"              SK8:SystemError       simple-error)
           ("AppleTalkError"               SK8:SystemError       simple-error)
           ("ScrapManagerError"            SK8:SystemError       simple-error)
           ("MemoryManagerError"           SK8:SystemError       simple-error)
           ("DirectoryError"               SK8:FileSystemError   simple-error)
           ("MenuManagerError"             SK8:SystemError       simple-error)
           ("ResourceError"                SK8:SystemError       simple-error)
           ("SoundManagerError"            SK8:SystemError       simple-error)
           ("PPCToolboxError"              SK8:SystemError       simple-error)
           ("AppleEventError"              SK8:SystemError       appleevent-error)
           ("MovieError"                   SK8:SystemError       simple-error)
           ("MovieDataHandlerError"        SK8:MovieError        simple-error)
           ("ComponentManagerError"        SK8:SystemError       simple-error)
           ("ImageCompressionError"        SK8:SystemError       simple-error)
           ("MovieSequenceGrabberError"    SK8:MovieError        simple-error)
           ("MovieControllerError"         SK8:MovieError        simple-error))
      doing (new (symbol-value from-obj) :project sk8 :objectname name :mcl-error-class mcl-error))
|#

(defvar %error-handlers% nil)

;;; The SK8 user function to signal a condition.
;;; This mates with the on-condition compiler in compile-let-forms in skil-compiler.lisp.
;;;
(define-handler sk8-signal (sk8:condition)
  (dolist (tag %error-handlers%)
    (loop for (cond fn) on tag by #'cddr
          doing 
          (when (and (symbolp cond) (boundp cond) (sk8:inheritsfrom me (symbol-value cond)))
            (cond ((null fn) (throw tag me)) ;; with-sk8-error-handler with one error clause
                  ((fixnump fn) (throw tag (cons fn me))) ;; with-sk8-error-handler with multiple error clause: generates one catch tag which dipatches on the index
                  (t (return (funcall fn me))))  ;; copied from CLOS condition system. Not used in SK8 yet so should never execute
            )))
  ;; Okay, no SK8 signal handlers.  Just return. If this is an an error, the default error handling will take it
  )

(define-sk8-function sk8::sk8-error nil (type &rest args)
  (when (inheritsfrom type sk8:condition)
    (let ((cnd (apply #'new type :project sk8 args)))
      (sk8-signal cnd)
      ;; it is an error if it returns
      (error (with-output-to-string (s) (writeobject cnd s nil))))))

;; here is the form generated by the skil compiler for on error code. It is similar to handler-case
(defmacro sk8::with-sk8-error-handlers (form &rest clauses)
  (cond ((null clauses) form)
        ((null (cdr clauses))
         (let ((block   (gensym))
               (cluster (gensym)))
           (let ((type (caar clauses))
                 (body (cdar clauses)))
             `(block ,block
                ((lambda (currentCondition) (declare (special currentCondition)) ,@body)
                 (let* ((,cluster (list ',type))
                        (%error-handlers% (cons ,cluster %error-handlers%)))
                   (declare (dynamic-extent ,cluster %error-handlers%))
                   (catch ,cluster (return-from ,block ,form))))))))
        (t (let ((block (gensym)) (cluster (gensym)) (val (gensym))
                 (index -1) handlers cases)
             (loop (unless clauses (return nil))
                   (setq index (1+ index))
                   (let* ((clause (pop clauses))
                          (type (car clause))
                          (body (cdr clause)))
                     (push `',type handlers)
                     (push index handlers)
                     (when (null clauses) (setq index t))
                     (push `(,index ((lambda (currentCondition) (declare (special currentCondition)) ,@body) ,val)) cases)))
             `(block ,block
                (let ((,val (let* ((,cluster (list ,@(nreverse handlers)))
                                   (%error-handlers% (cons ,cluster %error-handlers%)))
                              (declare (dynamic-extent ,cluster %error-handlers%))
                              (catch ,cluster (return-from ,block ,form)))))
                  (case (pop ,val)
                    ,@(nreverse cases))))))))

#|
	Change History (most recent last):
	2  	10/18/96	sidney  	Implement a SK8 error system
	3  	10/18/96	sidney  	create the anonymous error objects in the SK8 project. Make sk8-error an error if it returns
	4  	 2/27/97	Hernan  	
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
