;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)


#|

FUNCTIONS
   objectString
   simpleObjectString

HANDLER
   writeObject

GLOBAL
   writeObjectRecursionLimit

|#

;;; _______________________________ 
;;; Globals
;;; _______________________________ 

(define-SK8-var writeObjectRecursionLimit :initial-value 2)
;;; This variable is bound to the target project when the macro below is called. 
(define-SK8-var targetProjectForWriteObject :initial-value nil)

;;; this macro does the usual with-output-to-string thing but first it binds the targetProject
;;; to a special variable that the writeObject methods can use to determine the targetProject for
;;; the write operation. A lot cleaner than redefining the class output-stream. 

(defmacro with-output-to-string-in-targetProject ((streamVar targetProject) &body body)
  `(let ((targetProjectForWriteObject ,targetProject))
     (declare (special targetProjectForWriteObject))
     (with-output-to-string (,streamVar)
       ,@body)))

(defvar *collectionsBeingWritten* nil)

(defconstant unreadableStartCharacter #\[)
(defconstant unreadableEndCharacter #\])
(defconstant *specialStringChars* (coerce (vector #\Newline #\Tab #\" #\\) 'string))
(defconstant *specialStringCharEscapes*
  '((#\Newline . #\n)
    (#\Tab . #\t)))

;;; _______________________________ 
;;; Helpers
;;; _______________________________ 

(defun writeEscapedChar (theStream ch &optional (streamWriter 'stream-tyo))
  (funcall streamWriter theStream #\\)
  (funcall streamWriter theStream (or (cdr (assq ch *specialStringCharEscapes*)) ch)))

(defmacro !writeMaybeEscapedChar (theStream ch &optional (streamWriter ''stream-tyo))
  (unless (and (symbolp theStream) (symbolp ch) (or (symbolp streamWriter) (constantp streamWriter)))
    (error "stream, ch, and streamWriter must be variables"))
  `(if (position ,ch *specialStringChars*)
     (writeEscapedChar ,theStream ,ch ,streamWriter)
     (funcall ,streamWriter ,theStream ,ch)))

(defun writeEscapedString (theStream str)
  (declare (string str) (stream theStream))
  (multiple-value-bind (writer writer-arg) (stream-writer thestream)
    (funcall writer writer-arg #\")
    (let (ch)
      (do ((limit (length str))
           (i 0 (+ i 1)))
          ((eql i limit))
        (declare (fixnum last limit i))
        (setq ch (char str i))
        (!writeMaybeEscapedChar writer-arg ch writer)))
    (funcall writer writer-arg #\")))


(defun writeArgsNicely (arglist thestream)
  (let ((start? t)
        (positionals? t))
    (dolist (arg arglist)
      (cond
       ((keywordp arg)
        (write-char #\Space thestream)
        (princ arg thestream)
        ;;(unless (memq (find-symbol (symbol-name arg) :SK8) !*SK8Script-public-reservedArgKeywords*)
        ;;  (write-char #\: thestream))
        (setq positionals? nil))
       (t
        (when (and positionals? (not start?)) (write-char #\, thestream))
        (write-char #\Space thestream)
        (writeObject arg thestream t)))
      (when start? (setq start? nil)))))

(defmethod find-relation-or-pseudo-relation ((me t))
  (declare (ignore me))
  nil)

;; later we will special case actors on stage by defining a handler on Actor that adds a test to this
(defmethod find-relation-or-pseudo-relation ((me Object))
  (multiple-value-bind (relatedObject relationProperty) (mf::find-relation me t)
    (cond
     (relatedObject
      (values relatedObject relationProperty))     
     (t
      (let* ((parent (baseParent me))
             (kc (when (realProperty parent 'knownChildren)
                   (getValue 'knownchildren parent))))
        (when (memq me kc)
          (values parent 'knownChildren)))))))

(defun maybe-write-relation (obj strm)
  (multiple-value-bind (relatedObject relationProperty) (find-relation-or-pseudo-relation obj)
    (when relatedObject
      (let ((relationValue (funcall relationProperty relatedObject)))
        (cond
         ((eq relationValue obj)
          (write-string "the " strm)
          (writeObject relationProperty strm nil)
          (write-string " of " strm))
         (t
          (let (;; (parent (baseParent obj))
                itemNumber)
            ;(cond
            ; ((and (neq relationProperty 'knownChildren) (maybeWriteObjectName parent strm))
            ;  (setq itemNumber 0)
            ;  (SS::do-collection-items ((relationValue parent nil nil item nil nil nil nil nil))
            ;    (incf itemNumber)
            ;    (when (eq item obj) (return))))
            ; (t
            (write-string "item" strm)
            (setq itemNumber (positionOfItem relationValue obj))
            (write-string (format nil " ~a in " itemNumber) strm)
            (write-string "the " strm)
            (writeObject relationProperty strm nil)
            (write-string " of " strm))))
        (writeObject relatedObject strm t)
        t))))

;;; _______________________________ 
;;; SK8 Functions. 
;;; _______________________________ 

(define-SK8-function maybeWriteProjectQualification nil (sym theStream &key ((:object obj)) ((:project proj)))
  (let ((symPackage (symbol-package sym))
        (targetPackage (package (or proj targetProjectForWriteObject sk8::sk8))))
    (cond
     ((null symPackage)
      (write-string " (in no project)" theStream)
      t)
     ((and (neq symPackage CCL::*KEYWORD-PACKAGE*)
           (neq symPackage targetPackage)
           (neq sym (find-symbol (symbol-name sym) targetPackage)))
      (write-string " (in project " theStream)
      (let ((proj (if obj
                    (Project obj)
                    (CCL::PACKAGE-PROJECT symPackage))))
        (if proj
          (write-pretty-symbol-name (sk8::sk8_id proj) theStream)
          (write-string (concatenate 'string "|" (package-name symPackage) "|") theStream)))
      (stream-tyo theStream #\))
      t))))
  
(define-SK8-function maybeWriteObjectName nil (obj thestream)
  (multiple-value-bind (name err?) (IGNORE-ERRORS (objectName obj))
    (cond
     ;; Write the object's name (qualified if necessary) & return T
     ((and name (symbolp name))
      (write-pretty-symbol-name name theStream)
      (maybeWriteProjectQualification name theStream :object obj)
      t)
     ;; Write the object's name (not qualified, since this is one of SK8's special "built-in-class" objects) & return T
     ((and (not err?) (setq name (objectName obj)))
      (write-string name thestream)
      t)
     ;; The object is unnamed, so return NIL
     (t
      nil))))

(define-sk8-function writeUninterestingObject nil (me theStream)
  (unless (or (maybeWriteObjectName me theStream)
              (maybe-write-relation me theStream))
    (if (mf::slot-value-no-error me 'project)  ;; nil means this is a deleted object or not yet fully created
      (let ((namedAncestor (baseParent me))
            (parent? t))
        (loop
          (when (objectName namedAncestor) (return))
          (setq namedAncestor (baseParent namedAncestor)
                parent? nil))
        (format theStream "~aa ~:[descendant~;child~] of " unreadableStartCharacter parent?)
        (maybeWriteObjectName namedAncestor theStream))
      (format theStream "~aa deleted Object" unreadableStartCharacter))
    (stream-tyo theStream unreadableEndCharacter)))

(define-sk8-function objectString nil (obj &key ((:project proj) sk8))
  (with-output-to-string-in-targetProject (theStream proj)
    (writeObject obj theStream t)))

(define-sk8-function simpleObjectString nil (obj &key ((:project proj) sk8))
  (with-output-to-string-in-targetProject (theStream proj)
    (writeObject obj theStream nil)))

;;; _______________________________ 
;;; Individual writeObject methods. 
;;; _______________________________ 

(define-handler writeObject (Text theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (if (not (collectionLike me))
      (call-next-method)
      (multiple-value-bind (writer writer-arg) (stream-writer theStream)
        (if rereadably
          (flet ((displayer (ch) (!writeMaybeEscapedChar writer-arg ch writer)))
            (declare (dynamic-extent #'displayer))
            (funcall writer writer-arg #\")
            (mapCharacters me nil nil #'displayer)
            (funcall writer writer-arg #\"))
          (flet ((displayer (ch) (funcall writer writer-arg ch)))
            (declare (dynamic-extent #'displayer))
            (mapCharacters me nil nil #'displayer)))))))

(define-handler writeObject (String theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (if rereadably
      (writeEscapedString theStream me)
      (write-string me theStream))))

(define-handler writeObject (Symbol theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (if (or (eq me NIL) (eq me T))
      (write-pretty-symbol-name me theStream)
      (let ((package (symbol-package me)))
        (when rereadably (stream-tyo theStream #\'))
        (cond
         ;; Special-case for keyword symbols
         ((eq package CCL::*KEYWORD-PACKAGE*)
          (stream-tyo theStream #\:))
         ;; Special-case for setter symbols
         ((eq package CCL::*SETF-PACKAGE*)
          (let ((*readtable* CCL::%INITIAL-READTABLE%))
            (setq me (read-from-string (symbol-name me))))
          (write-string "set " theStream)))
        (write-pretty-symbol-name me theStream)
        (when rereadably
          (stream-tyo theStream #\')
          ;; Qualify the symbol if necessary
          (maybeWriteProjectQualification me theStream))))))

(defmethod writeObject ((me built-in-class) theStream rereadably &key)
  (declare (ignore rereadably))
  (unless (maybeWriteObjectName me theStream)
    (call-next-method)))

;;; A couple of "nice" writeObject handlers for some "ugly" internal things that show up in some user-visible places...

(defmethod writeObject ((me CCL::population) theStream rereadably &key)
  (declare (ignore rereadably))
  (stream-tyo theStream unreadableStartCharacter)
  (write-string "a weak list" theStream)
  (stream-tyo theStream unreadableEndCharacter))

(defmethod writeObject ((me hash-table) theStream rereadably &key)
  (declare (ignore rereadably))
  (stream-tyo theStream unreadableStartCharacter)
  (write-string "an internal table" theStream)
  (stream-tyo theStream unreadableEndCharacter))

;;; SK8Script does not support ratios, so make sure any that happen to appear write out as floats

(defmethod writeObject ((me ratio) theStream rereadably &key)
  (declare (ignore rereadably))
  (print-object (float me) theStream))

;;; The default object writer (also specially handles *undefined*)

(define-handler writeObject (Object theStream rereadably)
  (cond
   ((eq me *undefined*)
    (write-string "#Undefined#" theStream))
   ((typep me 'object)
    (writeUninterestingObject me theStream))
   (t (let ((*print-escape* rereadably))
        (print-object me theStream)))))

(define-handler writeObject (SmallFloat theStream rereadably)
  (declare (ignore rereadably))
  (unless (maybeWriteObjectName me theStream)
    (let ((*read-default-float-format* 'short-float))
      (princ me theStream))))

;; *** Just for "dev-mode" (i.e. so collections write their descriptive representation in the listener)
(eval
 `(defmethod print-object ((me ,(class-of Collection)) theStream)
    (let ((targetProjectForWriteObject (project me))) ;; since this is not going through objectString...
      (declare (special targetProjectForWriteObject))
      (stream-tyo theStream #\[)
      (writeObject me theStream *print-escape*)
      (stream-tyo theStream #\]))))

(define-handler writeObject (Vector theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (let ((writeObjectRecursionLimit writeObjectRecursionLimit)
          (*collectionsBeingWritten* (cons me *collectionsBeingWritten*)))
      (declare (dynamic-extent *collectionsBeingWritten*))
      (if rereadably
        (write-string "the Vector {" theStream)
        (stream-tyo theStream #\{))
      (if (and (memq me (cdr *collectionsBeingWritten*)) (eql 0 (decf writeObjectRecursionLimit)))
        (write-string "[...]}" theStream)
        (let ((first t))
          (dovector (item me)
            (if first
              (setq first nil)
              (write-string ", " theStream))
            (writeObject item theStream rereadably))))
      (stream-tyo theStream #\}))))

(defun write-SK8-array-elements (strm overallArray arrayData offset dimensions itemWriter)
  (declare (fixnum offset) (list dimensions)
           (type stream strm) (type vector arrayData) (type itemWriter function))
  (let* ((rereadably T)
         (tail (cdr dimensions))
         (limit (car dimensions))
         (step 1)
         (o offset)
         (i 0))
    (declare (fixnum limit step o i))
    (dolist (e tail) (declare (fixnum e)) (setq step (* e step)))
    (stream-tyo strm #\{)
    (loop
      (when (eql i limit) (return))
      (unless (eql i 0)
        (stream-tyo strm #\,) (stream-tyo strm #\Space))
      (if tail
        (write-SK8-array-elements strm overallArray arrayData o tail itemWriter)
        (funcall itemWriter (aref arrayData o) strm rereadably))
      (incf i)
      (incf o step))
    (stream-tyo strm #\})))

(define-handler writeObject (Array theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (cond
     ((> (array-total-size me) 100)
      (format theStream "~aa child of " unreadableStartCharacter)
      (maybeWriteObjectName Array theStream)
      (let ((dims (array-dimensions me)))
        (format theStream "with dimensions {")
        (dotimes (i (1- (length dims)))
          (format theStream "~A, " (elt dims i)))
        (format theStream "~A" (elt dims (1- (length dims)))))
      (format theStream "}")
      (stream-tyo theStream unreadableEndCharacter))
     (t
      (let ((writeObjectRecursionLimit writeObjectRecursionLimit)
            (*collectionsBeingWritten* (cons me *collectionsBeingWritten*)))
        (declare (dynamic-extent *collectionsBeingWritten*))
        (when rereadably (write-string "the Array " theStream))
        (if (and (memq me (cdr *collectionsBeingWritten*)) (eql 0 (decf writeObjectRecursionLimit)))
          (write-string "[...]}" theStream)
          (multiple-value-bind (arrayData offset) (CCL::array-data-and-offset me)
            (write-SK8-array-elements theStream me arrayData offset (array-dimensions me) #'writeObject))))))))

(defun write-list (me theStream rereadably itemWriter)
  (let ((head me)
        (headRevisitLimit writeObjectRecursionLimit)
        (writeObjectRecursionLimit writeObjectRecursionLimit)
        (*collectionsBeingWritten* (cons me *collectionsBeingWritten*)))
    (declare (dynamic-extent *collectionsBeingWritten*))
    (cond
     ((and (memq me (cdr *collectionsBeingWritten*)) (eql 0 (decf writeObjectRecursionLimit)))
      (write-string "{[...]}" theStream))
     (t
      (stream-tyo theStream #\{)
      (funcall itemWriter (pop me) theStream rereadably)
      (loop
        (unless me (return))
        (when (and (eq me head) (eql 0 (decf headRevisitLimit)))
          (write-string " [,...]" theStream)
          (return))
        (unless (listp me)
          (write-string " . " theStream)
          (funcall itemWriter me theStream rereadably)
          (return))
        (write-string ", " theStream)
        (funcall itemWriter (pop me) theStream rereadably))
      (stream-tyo theStream #\})))))

(define-handler writeObject (List theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (write-list me theStream rereadably #'writeObject)))

(define-handler writeObject (Character theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (if rereadably
      (let ((charName (cdr (assq me *character-names*))))
        (write-string "the Character " theStream)
        (cond
         (charName
          (writeObject charName theStream nil))
         ((> (char-code me) #.(char-code #\Space))
          (stream-tyo theStream #\")
          (!writeMaybeEscapedChar theStream me)
          (stream-tyo theStream #\"))
         (t
          (princ (char-code me) theStream))))
      (stream-tyo theStream me))))

(define-handler writeObject (Handler theStream rereadably)
  ;; *** Could be written rereadably as some path expression, or maybe some kind of weird literal
  (multiple-value-bind (hproj hname hobj hqual)
                       (!exactLfunInfo (method-function me))
    (declare (ignore hproj))
    (when rereadably (stream-tyo theStream unreadableStartCharacter))
    (write-string (handlerIDstring hname hobj hqual :project targetProjectForWriteObject) theStream)
    (when rereadably (stream-tyo theStream unreadableEndCharacter))))

(define-handler writeObject (SK8::Function theStream rereadably)
  ;; *** Could be written rereadably as some path expression, or maybe some kind of weird literal
  (multiple-value-bind (hproj hname) (!exactLfunInfo me)
    (when rereadably (stream-tyo theStream unreadableStartCharacter))
    (cond
     (hname
      (writeObject hname theStream nil)
      (ccl::stream-write-entire-string theStream " (function in project ")
      (writeObject hproj theStream nil)
      (stream-tyo theStream #\)))
     (t
      (ccl::stream-write-entire-string theStream "an anonymous function")))
    (when rereadably (stream-tyo theStream unreadableEndCharacter))))

;; SK8 Condition objects use the writeObject handler the way MCL conditions use report-condition
;; To cause the SK8 condition to use the MCL report-condition to generate the error string, simply leave the MCLcondition property set to an MCL condition instance
;; To use a different string, clear the property before writeObject gets called, and then writeObject has to figure out what to output

;;  A utility function. This checks for the cases that all sk8 conditions have in common. Returns T if it handled the output, nil if it didn't
(defun default-write-sk8-condition (me theStream rereadably)
  (or (maybeWriteObjectName me theStream)
      (when rereadably
        (writeUninterestingObject me theStream)
        t)
      (let ((cnd (SK8::MCLcondition me)))
        (when cnd
          (ccl::report-condition cnd theStream)
          t))))

(define-handler writeObject (SK8::Error theStream rereadably)
  (default-write-sk8-condition me theStream rereadably)
  )

(define-handler writeObject (SystemError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (let ((errortype (objectname (baseparent me))))  ;; this assumes that an error object is always a child of its named prototype
      (write-string "System error of type " theStream)
      (write-string errortype theStream))))

(define-handler writeObject (GeneralError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (let ((strings (strings me))
          (objects (objects me))
          str)
      (loop
        (setq str (pop strings))
        (when str (write-string str theStream))
        (when (null objects) (return))
        (writeObject (pop objects) theStream t)))))

(define-handler writeObject (UndefinedHandlerError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (let ((name (handlerName me))
          (args (arguments me)))
      (write-string "Handler " theStream)
      (writeObject name theStream t)
      (write-string " is undefined" theStream)
      (when (fboundp name)
        (cond
         (args
          (write-string " for given argument" theStream)
          (when (cdr args) (write-char #\s theStream))
          (write-char #\: theStream)
          (writeArgsNicely args theStream))
         (t
          (write-string " for no arguments" theStream)))))))

(define-handler writeObject (IncorrectArgumentsError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (let ((name (handlerName me))
          (args (arguments me)))
      (cond
       ((and (not name) (not args))
        (write-string "Wrong number of arguments" theStream))
       (t
        (write-string (if args "Incorrect arguments" "No arguments given") theStream)
        (when name
          (write-string " to " theStream)
          (writeObject name theStream t))
        (when args
          (write-char #\: theStream)
          (writeArgsNicely args theStream)))))))

(define-handler writeObject (IncorrectSubscriptsError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (let ((numSubscripts (rank me)))
      (writeObject (array me) theStream nil)
      (write-string " canÕt be accessed with " theStream)
      (princ numSubscripts theStream)
      (write-string (if (eq 1 numSubscripts) " subscript" " subscripts") theStream))))

(define-handler writeObject (IndexOutOfBoundsError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (format theStream "Index ~a is out of bounds for " (index me))
    (writeObject (array me) theStream t)))

(define-handler writeObject (UnboundVariableError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "Unbound global variable " theStream)
    (writeObject (variableName me) theStream t)))

(define-handler writeObject (TypeMismatchError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (let ((type (expectedType me)))
      (writeObject (object me) theStream t)
      (cond
       ((consp type)
        (write-string (if (CCL::standard-instance-p (first (the list type)))
                        " is not of one of the expected types "
                        " is not one of the expected values ") theStream)
        (loop
          (writeObject (pop type) theStream t)
          (unless type (return))
          (write-string (if (null (cdr type)) " or " ", ") theStream)))
       (t
        (write-string " is not of the expected type " theStream)
        (writeObject type theStream t))))))

(define-handler writeObject (PropertyTypeMismatchError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "In the " theStream)
    (writeObject (propertyName me) theStream nil)
    (write-string " of " theStream)
    (writeObject (ownerObject me) theStream t)
    (write-string ", " theStream)
    (call-next-method)))

(define-handler writeObject (ArgumentTypeMismatchError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "In " theStream)
    (writeObject (handlerName me) theStream nil)
    (write-string "Õs '" theStream)
    (writeObject (argumentName me) theStream nil)
    (write-string "' argument, " theStream)
    (call-next-method)))

(define-handler writeObject (CoercionError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "CanÕt coerce " theStream)
    (writeObject (object me) theStream t)
    (write-string " to " theStream)
    (writeObject (type me) theStream t)))

(define-handler writeObject (CantChangeConstantError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "CanÕt set global constant " theStream)
    (writeObject (constantName me) theStream t)
    (write-string " to a new value" theStream)))

(define-handler writeObject (DivisionByZeroError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "CanÕt divide by zero" theStream)))

(define-handler writeObject (ArithmeticOverflowError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "Arithmetic overflow" theStream)))

(define-handler writeObject (EventModeError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "Error in EventMode: " theStream)
    (writeObject (eventMode me) theStream t)))

(define-handler writeObject (FileNotFoundError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "File " theStream)
    (writeObject (file me) theStream t)
    (write-string " not found" theStream)))

(define-handler writeObject (DisposedObjectError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (write-string "CanÕt call a handler or reference a property of " theStream)
    (writeObject (object me) theStream t)))

(define-handler writeObject (AppleEventError theStream rereadably)
  (unless (default-write-sk8-condition me theStream rereadably)
    (let ((errorID (slot-value me 'CCL::oserr)))
      (if errorID
        (format theStream "AppleEvent error id ~a" errorID)
        (write-string "AppleEvent error" theStream)))))

#|
	Change History (most recent last):
	2	6/7/93	chip	
	3	6/28/93	chip	added a printObject for array to print a description of its dimensions if it has more than 200 elements
	4	6/28/93	chip	added printObject for new error IncorrectSubscriptsError
	10	8/30/93	chip	added printObject handler for IndexOutOfBoundsError
	11	8/31/93	chip	cleaned up printObject for UndefinedHandlerError a bit
	12	9/1/93	hernan	The great integrating build for d4!!!
	13	9/28/93	chip	symbols now print qualified in their project if necessary (updated maybePrintObjectName & Symbol's printObject)
	14	9/29/93	chip	fixed maybePrintObjectName
	15	10/11/93	chip	fixed printing of IndexOutOfBoundsError when the array is a vector
	16	10/20/93	kleiman	printUninterestingObject fix for store loadtime
	17	10/27/93	chip	fixed printObject for Symbol NOT to qualify keywords
	18	10/29/93	chip	factored out new sk8 function: maybePrintProjectQualification; added printObject of function
	19	11/15/93	chip	maybePrintProjectQualification now deals with a symbol without a package
	20	11/22/93	chip	added printer for ConditionSystemError
	21	12/13/93	sidney	Ignore some possible errors when printing a partially initialized SK8 object
	22	1/7/94	sidney	big mf/ss merge
	23	1/11/94	hernan	self -> me
	24	2/14/94	sidney	use all-children instead of children for printing unnamed objects
	25	2/18/94	chip	now assumes objectName won't ever cause an error
	26	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	27	2/23/94	kleiman	TaggedObject -> TagMixin
	28	2/25/94	chip	now assumes targetProject for printing is given by the stream!
	29	2/28/94	chip	added new object literal printing for certain objects; fixed printUninterestingObject for a childrenless world
	30	2/28/94	chip	made printUninterestingObject print object unreadably if rereadably is requested but the object can't be re-read
	31	3/1/94	kleiman	mf::object-id -> sk8_id
	32	3/1/94	chip	renamed TextCollection --> IndirectText
	33	3/2/94	sidney	Who knows? I'm just checking this in for Ruben.
	34	3/3/94	sidney	Move definition of print-object for collection to where it can be used; moved special-case for character *after* Collection & Array's printObject handlers
	35	3/6/94	chip	print... to write... & new writeObject api; recursion limiting
	1	3/6/94	chip	moved to function library folder
	2	3/6/94	chip	fixed typo in writeObject for TagMixin
	3	3/7/94	chip	fixed a few argument omissions in calls to writeObject
	4	3/7/94	chip	new arg to mapItems calls
	5	3/8/94	chip	replaced writeObject of IndirectText with a more general one for Text
	6	3/9/94	chip	fixed how GeneralError writes
	7	3/16/94	chip	added some inadvertantly omitted stream args
	8	3/24/94	chip	obsoleted UndefinedErrorError
	9	3/24/94	chip	made ScriptSyntaxError's writeObject a bit cleaner
	10	4/12/94	Hernan	Avoiding use of contents when not necessary.
	11	4/18/94	chip	ScriptSyntaxError's writeObject now works with new 'errorPosition' representation
	12	4/22/94	kleiman	returned writeUninterestingObject to its original version -- it conses, but it's simple & who cares
	13	4/28/94	chip	made "print-object" representation of collections a bit more readable (added delimiters!)
	14	6/2/94	chip	UndefinedHandlerError's writeObject now points out handlers that are part of the Collection Protocol
	15	6/15/94	chip	now ArgumentTypeMismatchError & PropertyTypeMismatchError write themselves correctly (radar 1168523)
	16	6/17/94	chip	ErrorSystemError --> ConditionSystemError
	17	6/29/94	chip	no longer uses the obsolete :original-case symbol property
	18	7/6/94	chip	restored to assume old case-saving mechanism
	19	7/14/94	chip	True & False now write themselves capitalized (since they're objects -- radar #1174468)
	20	7/21/94	chip	Array's writeObject now writes the structure of the array (radar #1175270)
	21	7/21/94	chip	TypeMismatchError's writeObject now works better for "disjoint" types (for radar #1152186)
	22	7/26/94	chip	prettied IndexOutOfBoundsError's writeObject a bit (since vectors and arrays now write themselves nicely)
	23 	 9/ 1/94	chip    	writeObject of TagMixin no longer dies if 'container' is undefined (radar #1183704)
	24 	 9/ 2/94	chip    	added special-case appearance for "record-like" ObjectTables (related to radar #1180425)
	25 	 9/22/94	chip    	tweaked Object's writeObject to handle *undefined*
	26 	 9/22/94	chip    	touched to force recompile
	27 	10/12/94	chip    	obsoleted TagMixin; now writeUninterestingObject deals with "tags" (creationRelations)
	28 	10/17/94	sidney  	prevent yet another error printing an object while in the middle of loading it
	29 	10/19/94	sidney  	touched to force recompile
	30 	11/ 7/94	kend    	Added coercion of ratios to floats (SK8Script does not support rationals)
	31 	11/ 8/94	kend    	Parameterize writeObject for arrays (for use by SK8Script code generator).
	32 	11/ 9/94	chip    	moved Ken's "coercion of ratios" functionality to the ratio class, which is the type of number that fix was intended to address (radar #1198622)
	33 	11/20/94	chip    	added "nice" writeObject handlers for population and hash-table, since they show up in user-visible places (radar #1184838)
	34 	12/20/94	Hernan  	Fixing find-relation to avoid printing objects refering
							to virtual properties.
	35 	 2/27/95	sidney  	getting rid of use of a useless macro
	36 	 3/17/95	me      	Fixed bug in writeObject for Array
	37 	 4/ 5/95	sidney  	don't crash trying to output a scriptsyntaxerror object when it has no value for the error position
	3  	 6/12/95	sidney  	bulletproof maybeWriteObjectName to not crash if object seems named but isn't
	4  	 6/22/95	sidney  	print deleted object reasonably
	5  	 6/22/95	sidney  	bulletproof writeuninterestingobject to not crash on object that is not yet fully loaded
	6  	 6/30/95	sidney  	(no change) force recompile after dynamic-extent bug has been fixed in MCL
	7  	 7/ 5/95	sidney  	even better bulletproofing of maybeWriteObjectName to catch some other error cases
	8  	12/15/95	sidney  	sk8 error objects are different. comment out write-object definitions for them. Perhaps we'll put them back when we have time to rewrite them
	9  	 1/19/96	sidney  	removing/rewriting refrences to old sk8script compiler
	2  	 5/13/96	Hernan  	The writeObject protocol is back in SK8.
	3  	 7/26/96	sidney  	find-relation-or-pseudo-relation is now a handler to make it easier to specialize cases
	4  	 7/29/96	Hernan  	Fixing writeObject of Symbol to print the symbol in a rereadable form.
	5  	 8/ 8/96	Hernan  	Fixed maybe-write-relation of object.
	6  	 9/ 3/96	Hernan  	Using position instead of some obscure ccl thing that was
						actually many times slower.
	7  	10/18/96	sidney  	Add writeObject handlers for SK8 error objects, used like MCL's report-condition
	8  	11/ 7/96	Brian Roddy	Fixing problem writing out the object LIST
	9  	11/22/96	Brian   	Added maybeWriteObjectName to all built-in
						classes writeobject.
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
