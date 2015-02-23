;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;; This file implements translation of the Documentation files into
;;; RTF. This is done for the benefit of porting the online documentation
;;; to QuickView. We choose to use the original parsing machinery. All
;;; that we need change is the methods that register the objects. And that
;;; is all we do. 

;;; A file is used for each object, one file for the functions and one
;;; for the globals and locals. This will make easy the reparsing of single
;;; files. 

;;; _______________________________ 
;;; Globals
;;; _______________________________ 

(defparameter *basic-concepts-folder* "ccl;Documentation:Basic Concepts:*.doc")
(defparameter *faqs-folder* "ccl;Documentation:FAQs:*.doc")

;;; This is where the results of the parsing go. 

(defparameter *destination-folder* "ccl;Documentation:QuickView:")

;;; The current object being parsed. 

(defparameter *object* nil)

(defparameter *RTF-header* 
"{\\rtf1\\mac\\deff2 
{\\fonttbl
{\\f0\\fswiss Chicago;}
{\\f2\\froman New York;}
{\\f3\\fswiss Geneva;}
{\\f4\\fmodern Monaco;}
{\\f13\\fnil Zapf Dingbats;}
{\\f16\\fnil Palatino;}
{\\f20\\froman Times;}
{\\f21\\fswiss Helvetica;}
{\\f22\\fmodern Courier;}
{\\f23\\ftech Symbol;}}
{\\colortbl\\red0\\green0\\blue0;\\red0\\green0\\blue255;\\red0\\green255\\blue255;\\red0\\green255\\blue0;
\\red255\\green0\\blue255;\\red255\\green0\\blue0;\\red255\\green255\\blue0;\\red255\\green255\\blue255;}
{\\stylesheet{\\sbasedon222\\snext0 Normal;}}}
\\widowctrl\\ftnbj \\sectd \\sbknone\\linemod0\\linex0\\cols1\\endnhere\\margl100 \\pard\\li80\\plain")

(defparameter *RTF-footer* 
"")

(defmacro with-rtf-for-output (theFileName &body body)
  (let ((rtfStream (gensym)))
    `(with-open-file (,rtfStream (format nil "~a~a.rtf" *destination-folder* ,theFileName)
                                 :direction :output :if-exists :supersede)
       (setf *RTF-stream* ,rtfStream)
       ;; write the header.
       (format *RTF-stream* *RTF-header*)
       ,@body
       ;; write the footer.
       (format *RTF-stream* *RTF-footer*))))

(defparameter *page-title* "\\pard\\qr\\ri300\\f3\\fs20\\b")
(defparameter *plain-style* "\\pard\\plain\\f3\\fs18\\li80")
(defparameter *bold-style* "\\pard\\b\\f3\\fs18\\li80")
(defparameter *level1-title* "\\pard\\f3\\fs18\\b\\li80")
(defparameter *code-style* "\\pard\\plain\\f22\\fs20\\li80")

;;; _______________________________ 
;;; Macros
;;; _______________________________ 

(defmacro vowelp (char)
  `(position ,char "AEIOUaeiou" :test #'eq))

;;; report progress
(defun report (format &rest msg)
  (apply #'format t format msg)
  (stream-force-output *terminal-io*))

;;; Returns true if the symbol has setters and getters.

(defmacro tis-some-property? (theSymbol)
  `(and (implementors ,theSymbol) (implementors (setter ,theSymbol))))
  
(defmacro tis-some-handler? (theSymbol)
  `(implementors ,theSymbol))

;;; _______________________________ 
;;; HyperLinks
;;; _______________________________ 

;;; This hash table stores every contextString we create when compiling the source files. 
;;; This is useful to record whether created context strings actually get filled up
;;; with a real description. It might be the case that at the end of the process
;;; we will have to create a dummy page for hyperlinks that point to nowhere. 

;;; The hash table is also useful to compile the list of handler and property pages. 

(defparameter *contextString-hash-table* (make-hash-table :test #'equal))

(defun register-contextString (name)
  (setf (gethash name *contextString-hash-table*) :notYet))
(defun contextString-status (name)
  (gethash name *contextString-hash-table*))
(defun set-contextString-status (contextString newStatus)
  (setf (gethash contextString *contextString-hash-table*) newStatus))

;;; These are used to collect things together to allow us to do clever things at the end.

(defparameter *all-handlers-defined* nil)    ;; These two are alists, handler name -> all
(defparameter *all-properties-defined* nil)  ;; context strings defined for that handler.
(defparameter *all-objects-defined* nil)      ;; The rest are lists of context strings.
(defparameter *all-globals-defined* nil)
(defparameter *all-constants-defined* nil)
(defparameter *all-functions-defined* nil)

(defun init-contextString-hashTable ()
  (setf *contextString-hash-table* (make-hash-table :test #'equal))
  ;; and the variables. 
  (setf *all-handlers-defined* nil)
  (setf *all-properties-defined* nil) 
  (setf *all-objects-defined* nil)    
  (setf *all-globals-defined* nil)
  (setf *all-constants-defined* nil)
  (setf *all-functions-defined* nil))

(defun add-item-to-assoc-list (theListSymbol key fullString)
  (let* ((theList (symbol-value theListSymbol))
         (alreadyThere? (assoc key theList :test #'string-equal)))
    (if alreadyThere?
      (pushnew fullString (cadr alreadyThere?) :test #'string-equal)
      (setf (symbol-value theListSymbol)
            (pushnew (list key (list fullString)) theList)))))
  
(defun generate-object-contextString (objname)
  (let ((theString (string-downcase (format nil "object_~a" objName))))
    (unless (contextString-status theString)
      (register-contextString theString)
      (pushnew theString *all-objects-defined* :test #'string-equal))
    theString))
(defun generate-handler-contextString (objname handname)
  (let ((theString (string-downcase (format nil "handler_~a_of_~a" handName objName))))
    (unless (contextString-status theString)
      (register-contextString theString)
      (add-item-to-assoc-list '*all-handlers-defined* handName theString))
    theString))
(defun generate-property-contextString (objName propName)
  (let ((theString (string-downcase (format nil "property_~a_of_~a" propname objname))))
    (unless (contextString-status theString)
      (register-contextString theString)
      (add-item-to-assoc-list '*all-properties-defined* propName theString))
    theString))
(defun generate-function-contextString (funName)
  (let ((theString (string-downcase (format nil "function_~a" funName))))
    (unless (contextString-status theString)
      (register-contextString theString)
      (pushnew theString *all-functions-defined* :test #'string-equal))
    theString))
(defun generate-global-contextString (globalName)
  (let ((theString (string-downcase (format nil "variable_~a" globalName))))
    (unless (contextString-status theString)
      (register-contextString theString)
      (pushnew theString *all-globals-defined* :test #'string-equal))
    theString))
(defun generate-constant-contextString (constantName)
  (let ((theString (string-downcase (format nil "constant_~a" constantName))))
    (unless (contextString-status theString)
      (register-contextString theString)
      (pushnew theString *all-constants-defined* :test #'string-equal))
    theString))
(defun generate-general-property-contextString (propname)
  (let ((theString (string-downcase (format nil "property_~a_of_ALL" propname))))
    (unless (contextString-status theString)
      (register-contextString theString))
    theString))
(defun generate-general-handler-contextString (handName)
  (let ((theString (string-downcase (format nil "handler_~a_of_ALL" handName))))
    (unless (contextString-status theString)
      (register-contextString theString))
    theString))

(defun contextString->objectName (contextString)
  (subseq contextString (1+ (position #\_ contextString :from-end t)) (length contextString)))

(defun contextString->object (contextString)
  (let* ((theObjName (contextString->objectName contextString))
         (thesymbol (let ((*package* (package sk8::sk8)))
                      (read-from-string theObjName nil nil))))
    (and (boundp theSymbol)
         (typep (symbol-value theSymbol) 'sk8::object)
         (symbol-value theSymbol))))

;;; If it is only one word, allow it to be a hyperlink when it is bould to something in
;;; SK8.

(defun really-a-link? (theWord)
  (let (theSymbol)
    (unless (or (char= (aref theWord 0) #\')
                (string-equal theWord "False")
                (string-equal theWord "True"))
      ;; should not represent a symbol.
      ;; Now we return t if it is bound or fbound.
      (setf thesymbol (let ((*package* (package sk8::sk8))) (read-from-string theWord nil nil)))  ;;what a kludge, uh, system
      (and theSymbol
           (symbolp theSymbol)
           (or (boundp theSymbol)
               (fboundp theSymbol))))))

(defun try-removing-quotes (str)
  (if (string= (subseq str 0 1) "'") (setf str (subseq str 1)))
  (if (string= (subseq str (1- (length str)) (length str)) "'") 
    (setf str (subseq str 0 (1- (length str)))))
  str)

;;; Ok. We have a piece of text in code form. Is it a hyperlink? 
;;; We try to parse it using a few stock posibilities. If it is ok, we
;;; construct a contextString for the hyperlink topic and 

(defun text->contextString (thetext &optional theobj)
  (let ((*package* (find-package "SK8"))
        theWords temp result theSymbol)
    (mapWordChunks thetext nil nil #'(lambda (s e) (push (subseq thetext s e) thewords)))
    (setf thewords (nreverse theWords))
    ;; Remove common strings...
    (when (string-equal (car theWords) "set") (setf theWords (cdr theWords)))
    (when (string-equal (car theWords) "the") (setf theWords (cdr theWords)))
    ;; Deal with the easy case: 1 word.
    (when (and (= 1 (length theWords)) (really-a-link? (car theWords)))
      (setf temp (car theWords))
      (setf theSymbol (let ((*package* (package sk8::sk8)))
                        (read-from-string temp nil nil)))
      (if theSymbol
        (cond ((boundp theSymbol)
               (cond ((memq theSymbol (constants SK8)) ;; a constant?
                      (setf result (generate-constant-contextString temp)))
                     ((memq theSymbol (globals SK8))   ;; a variable?
                      (setf result (generate-global-contextString temp)))
                     ((inheritsFrom (symbol-value theSymbol) Object) ;; an object? 
                      (setf result (generate-object-contextString temp)))))
              ((fboundp theSymbol)
               (cond ((and theObj (tis-a-property? theObj theSymbol)) ;; a property of the current object?
                      (setf result (generate-property-contextString (objectName theObj) temp)))
                     ((and theObj (memq theObj (implementors theSymbol))) ;; a handler of the current object?
                      (setf result (generate-handler-contextString (objectName theObj) temp)))
                     ((tis-some-property? theSymbol) ;; a property of something?
                      (setf result (generate-general-property-contextString temp)))
                     ((tis-some-handler? theSymbol) ;; a handler of something?
                      (setf result (generate-general-handler-contextString temp)))
                     ;; Must be a function.
                     (t (setf result (generate-function-contextString temp))))))))
    result))
      
;;; _______________________________ 
;;; Text handling functions.
;;; _______________________________ 

;;; converts an objectname (string) into an object, if possible.

(defun object-name->object (str &optional (error-string " in this version of SK8."))
  (let ((s (find-symbol (string-upcase str) :sk8)))
    (unless (boundp s)
      (error "Could not find object ~a" s error-string))
    (symbol-value s)))

(defun parse-parents (objs)
  (if objs
    (progn
      (setf objs (reverse objs))
      (let (result)
        ;; Determine whether the list should start with an or a.
        (if (vowelp (aref (sk8::objectString (car objs)) 0))
          (setf result "(an ")
          (setf result "(a "))
        (dolist (aParent objs)
          (setf result (concatenate 'string result (sk8::objectstring aParent) ", ")))
        ;; Remove last comma and add paren.
        (concatenate 'string (subseq result 0 (- (length result) 2)) ")")))
    ;; Dealing with object. Just return an empty string.
    ""))

;;; Outputs in RTF safe form. Every string printed should be piped through this. 

(defun output-to-string (str)
  (let ((s *RTF-stream*))
    ;; Write all chars.
    (let (theChar)
      (dotimes (i (length str))
        (case (setq theChar (aref str i))
          (#\> (princ "\\>" s))
          ;; No smart quotes. Let's be consistent!
          (#\Õ (princ "'" s))
          (#\Ô (princ "'" s))
          (#\' (princ "'" s))
          (#\` (princ "'" s))
          (#\{ (princ "\\{" s))
          (#\} (princ "\\}" s))
          (#\\ (princ "\\\\" s))
          (#\Tab (princ "\\tab " s))
          (otherwise (princ theChar s)))))))

;;; _______________________________ 
;;; Formating macros
;;; _______________________________ 

(defmacro with-plain-text (&body body)
  `(progn
     (format *RTF-stream* "~a " *plain-style*)
     ,@body
     (format *RTF-stream* "~%")))

;;; PRIMITIVES.

(defparameter *allow-paralines* t)

(defun emit-paraline ()
  (when *allow-paralines*
    (format *RTF-stream* "\\par ~%")))

(defun emit-topic-end ()
  (emit-paraline)
  (emit-paraline)
  (format *RTF-stream* "\\\page~%"))

(defun emit-keyword (str)
  (format *RTF-stream* 
          "K{\\\footnote ~a}~%"
          str))

(defun emit-context-string (str)
  (format *RTF-stream* 
          "#{\\\footnote ~a}~%"
          str))

(defun emit-topic-title (str)
  (format *RTF-stream* 
          "${\\\footnote ~a}~%"
          str))

(defun emit-page-title (str)
  (format *RTF-stream* "~a ~a \\par \\par ~a" *page-title* str *plain-style*))

(defun emit-paragraph (str)
  (with-plain-text
    (output-to-string str)))

(defun emit-in-computer-voice-with-hyperlink (str hyperlink)
  (format *RTF-stream* "{~a \\uldb " *code-style*) ;; courier. 
  (output-to-string str)
  (format *RTF-stream* "}{\\v ~a} " hyperlink))

(defun emit-in-computer-voice (str)
  (let ((hyperlinkDestination (text->contextString str *current-object*)))
    (if hyperlinkDestination
      (emit-in-computer-voice-with-hyperlink str hyperlinkDestination)
      (progn
        (format *RTF-stream* "~a " *code-style*)
        (output-to-string str)
        (format *RTF-stream* "~a " *plain-style*)))))

(defun emit-in-bold (str)
  (format *RTF-stream* "~a " *bold-style*)
  (output-to-string str)
  (format *RTF-stream* "~a " *plain-style*))

(defun emit-hard-return ()
  (format *RTF-stream* "\\line ~%"))

;;; In bold, with 2 newlines after it.

(defun emit-simple-header (titleStr)
  (format *RTF-stream* "~a ~a ~a" *level1-title* titleStr *plain-style*))

;;; OTHERS...

(defun emit-hardreturn-paraline-text (str &optional return?)
  (emit-in-computer-voice str)
  (when return? (emit-hard-return)))

;;; Calls emit-paragraph-text and adds the newlines by hand.

(defun emit-paragraph-text-with-returns (str)
  (let ((startpos 0)
        (curpos 0)
        (length (length str)))
    (loop
      (when (= curpos length)
        (emit-hardreturn-paraline-text (subseq str startPos length))
        (return))
      (when (char= (aref str curpos) #\Newline)
        (emit-hardreturn-paraline-text (subseq str startpos curpos) t)
        (setf startpos (1+ curpos)))
      (incf curpos))))

;;; Separates the computerVoice from the rest of the text.

(defun emit-paragraph-text (str)
  ;; Output all the text.
  (let ((computerVoiceStart -2)
        (computerVoiceEnd 0)
        (curpos 0))
    (loop
      (setf computerVoiceStart (search "[[" str :start2 (+ computerVoiceStart 2)))
      (if computerVoiceStart
        (progn
          (output-to-string (subseq str curpos computerVoiceStart))
          (setf computerVoiceEnd (search "]]" str :start2 (+ computerVoiceStart 2)))
          (if computerVoiceEnd
            (progn (emit-in-computer-voice (subseq str (+ computerVoiceStart 2) computerVoiceEnd))
                   (setf curpos (+ 2 computerVoiceEnd)))
            (error "Found start but not end of computer voice!")))
        (progn
          (output-to-string (subseq str curpos (length str)))
          (return))))))

(defun emit-note-paragraph (str)
  ;; Strip out the brackets.
  (setf str (subseq str 6 (1- (length str))))
  ;; Add the note in bold.
  (emit-in-bold "Note: ")
  ;; And now the note's text.
  (emit-paragraph-text str)
  ;; Add the note's terminator character.
  ;(with-fontchange "OurDingbats"
  ;  (start-paraline s)
  ;  (format s "~%  <String `u'>")
  ;  (end-paraline s))
  )

;;; Makes sure that either we find the end of the string after whitespace or
;;; the next thing after the whitespace is a newline.

(defun really-a-code-block? (str i length)
  (let (theChar)
    (loop
      (when (>= i length) (return-from really-a-code-block? t))
      (setf theChar (aref str i))
      (cond ((char= theChar #\Newline) (return-from really-a-code-block? t))
            ((not (char= theChar #\Space)) (return-from really-a-code-block? nil)))
      (incf i))))

;;; Gets code with newlines in it. Must deal with these as hard returns.

(defun emit-code-paragraph (str)
  ;; Strip out the brackets.
  (setf str (subseq str 2 (- (length str) 2)))
  (format *RTF-stream* "~a " *code-style*)
  (emit-paragraph-text-with-returns str)
  (format *RTF-stream* "~a " *plain-style*))

;;; Three cases:
;;; (1) Paragraph starts with "[Note:": look for the end of the note by counting square brackets.
;;; (2) Paragraph starts with "[[": ends as soon as we find the "]]".
;;; (3) Normal text: ends with first newline.

(defun emit-text (str)
  (with-plain-text 
    (let ((length (length str))
          (whiteSpace '(#\Newline #\Space))
          (status :lookingForParagraph)
          thechar startParagraph endParagraph bracketCount)
      (dotimes (i length)
        (setf theChar (aref str i))
        (case status
          (:lookingForParagraph ;; Check if paragraph starts.
           (unless (memq theChar whiteSpace)
             ;; What sort of paragraph are we looking at?
             (cond ((not (char= theChar #\[))
                    (setf status :lookingForParagraphEnd))
                   ((and (< (+ i 6) length) (string-equal (subseq str i (+ i 6)) "[Note:"))
                    (setf bracketCount 1)
                    (setf status :lookingForEndOfNote))
                   ((and (< (+ i 2) length) (string-equal (subseq str i (+ i 2)) "[["))
                    (setf bracketCount 1)
                    (setf status :lookingForEndOfCode)))
             (setf startParagraph i)))
          ;; Normal paragraph end.
          (:lookingForParagraphEnd
           (when (or (char= theChar #\Newline)
                     (= i length)) ;; (1- length)))
             (setf status :lookingForParagraph)
             (setf endParagraph i)
             ;; Found end! Send paragraph in for processing.
             (emit-paragraph-text (subseq str startParagraph endParagraph))
             (emit-paraline)
             (emit-paraline)))
          ;; Note paragraph end.
          (:lookingForEndOfNote
           (cond ((char= theChar #\[) (incf bracketCount))
                 ((char= theChar #\]) (decf bracketCount)))
           (when (zerop bracketCount)
             (setf status :lookingForParagraph)
             (setf endparagraph i)
             (emit-note-paragraph (subseq str startParagraph (1+ endParagraph)))
             (emit-paraline)
             (emit-paraline)))
          ;; Code paragraph end.
          (:lookingForEndOfCode
           (cond ((char= theChar #\[) (incf bracketCount))
                 ((char= theChar #\]) (decf bracketCount)))
           (when (zerop bracketCount)
             (if (really-a-code-block? str (1+ i) length)
               (progn
                 (setf status :lookingForParagraph)
                 (setf endparagraph i)
                 (emit-code-paragraph (subseq str startParagraph (1+ endParagraph)))
                 (emit-paraline)
                 (emit-paraline))
               ;; Twas a false alarm: a text block started with a computer voice word.
               (setf status :lookingForParagraphEnd)))))))))

;;; _______________________________ 
;;; Object output functions.
;;; _______________________________ 

(defun argument->rtf (argument)
  (let ((name (cl-user::argument_name argument))
        (type (cl-user::argument_type argument))
        (required (cl-user::argument_required argument))
        (description (cl-user::argument_description argument)))
    (emit-text (format *RTF-stream* "¥ ~a,~@[ ~a,~] ~a: " name type 
                       (if required "required" "optional")))
    (emit-text description)))

(defun arguments->RTF (arguments)
  (let ((*allow-paralines* nil))
    (declare (special *allow-paralines*))
    (dolist (oneArg arguments)
      (argument->RTF oneArg)
      (emit-hard-return)))
  (emit-paraline))
   
(defun emit-page-title-for-property-or-handler (name obj type)
  (emit-page-title 
   (format nil "{\\uldb ~a}{\\v ~a} (a ~a of {\\uldb ~a}{\\v ~a})"
           name (if (string-equal type "property")
                  (generate-general-property-contextString name)
                  (generate-general-handler-contextString name))
           type
           (objectName obj) (generate-object-contextString (objectName obj)))))

(defun property->RTF (obj name info description getterDescription getterArguments 
                             setterDescription setterArguments example seeAlso)
  (declare (ignore info))
  (let ((contextString (generate-property-contextString (objectName obj) name)))
    (report "~%.. Property ~a of ~a." name (objectName obj))
    (set-contextString-status contextString :ok)
    ;; Emit context string.
    (emit-context-string contextString)
    ;; Emit topic title.
    (emit-topic-title (format nil "~a (a property of ~a)" name (objectName obj)))
    (emit-keyword (format nil "~a of ~a" name (objectName obj)))
    ;; Emit title
    (emit-page-title-for-property-or-handler name obj "property")
    ;; Emit description.
    (unless (string-equal description "")
      (emit-in-bold "Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text description))
    ;; Emit GETTER.
    (unless (string-equal getterdescription "")
      (emit-in-bold "Getter Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text getterdescription))
    (when getterarguments
      (emit-in-bold "Getter Arguments:")
      (emit-paraline)
      (emit-paraline)
      (arguments->RTF getterarguments))
    ;; Emit SETTER.
    (unless (string-equal setterdescription "")
      (emit-in-bold "Setter Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text setterdescription))
    (when setterarguments
      (emit-in-bold "Setter Arguments:")
      (emit-paraline)
      (emit-paraline)
      (arguments->RTF setterarguments))
    ;; Emit example.
    (unless (string-equal example "")
      (emit-simple-header "Example:")
      (emit-paraline)
      (emit-paraline)
      (emit-text example))
    ;; Emit seeAlso.
    (unless (string-equal seeAlso "")
      (emit-in-bold "See Also: ")
      (emit-paraline)
      (emit-paraline)
      (emit-text seeAlso))
    ;; Done.
    (emit-topic-end)))

(defun properties->RTF (obj properties)
  (dolist (oneProperty properties)
    (property->RTF obj
                   (cl-user::property_name oneProperty)
                   (cl-user::property_info oneProperty)
                   (cl-user::property_description oneProperty)
                   (cl-user::property_getter_description oneProperty)
                   (cl-user::property_getter_arguments oneProperty)
                   (cl-user::property_setter_description oneProperty)
                   (cl-user::property_setter_arguments oneProperty)
                   (cl-user::property_example oneProperty)
                   (cl-user::property_seeAlso oneProperty))))

(defun handler->RTF (obj name info description arguments example seeAlso)
  (declare (ignore owner info))
  (let ((contextString (generate-handler-contextString (objectName obj) name)))
    (report "~%.. Handler ~a of ~a." name (objectName obj))
    (set-contextString-status contextString :ok)
    ;; Emit context string.
    (emit-context-string contextString)
    ;; Emit topic title.
    (emit-topic-title (format nil "~a (a handler of ~a)" name (objectName obj)))
    (emit-keyword (format nil "~a of ~a" name (objectName obj)))
    ;; Emit title
    (emit-page-title-for-property-or-handler name obj "handler")
    ;; Emit description.
    (unless (string-equal description "")
      (emit-in-bold "Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text description))
    ;; Emit arguments. 
    (when arguments
      (emit-in-bold "Arguments:")
      (emit-paraline)
      (emit-paraline)
      (arguments->RTF arguments))
    ;; Emit example.
    (unless (string-equal example "")
      (emit-simple-header "Example:")
      (emit-paraline)
      (emit-paraline)
      (emit-text example))
    ;; Emit seeAlso.
    (unless (string-equal seeAlso "")
      (emit-in-bold "See Also: ")
      (emit-paraline)
      (emit-paraline)
      (emit-text seeAlso))
    ;; Done.
    (emit-topic-end)))

(defun handlers->RTF (obj handlers)
  (dolist (oneHandler handlers)
    (handler->RTF obj 
                  (cl-user::handler_name oneHandler)
                  (cl-user::handler_info oneHandler)
                  (cl-user::handler_description oneHandler)
                  (cl-user::handler_arguments oneHandler)
                  (cl-user::handler_example oneHandler)
                  (cl-user::handler_seeAlso oneHandler))))

(defun emit-all-handlers-and-properties (objName handlers properties)
  (let ((firstOne t))
    (dolist (aHandler handlers)
      (if firstOne
        (setf firstOne nil)
        (format *RTF-stream* ", "))
      (emit-in-computer-voice-with-hyperlink 
       (cl-user::handler_name aHandler)
       (generate-handler-contextString objName (cl-user::handler_name aHandler))))
    (dolist (aProp properties)
      (if firstOne
        (setf firstOne nil)
        (format *RTF-stream* ", "))
      (emit-in-computer-voice-with-hyperlink 
       (cl-user::property_name aProp)
       (generate-property-contextString objName (cl-user::property_name aProp))))
    (format *RTF-stream* ".")
    (emit-paraline)))
    
(defun object->RTF (obj info owner description example seeAlso handlers properties)
  (declare (ignore owner info))
  (let ((contextString (generate-object-contextString obj)))
    (setf *current-object* (object-name->object obj))
    (report "~%~%Object ~a" obj)
    (set-contextString-status contextString :ok)
    ;; Emit context string.
    (emit-context-string contextString)
    ;; Emit topic title.
    (emit-topic-title (format nil "~a (the object)" obj))
    (emit-keyword obj)
    ;; Emit title and inheritance.
    (emit-page-title (format nil "~a " obj (parse-parents (sk8::parents *current-object*))))
    ;; Emit description.
    (unless (string-equal description "")
      (emit-in-bold "Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text description))
    ;; Emit example.
    (unless (string-equal example "")
      (emit-simple-header "Example:")
      (emit-paraline)
      (emit-paraline)
      (emit-text example))
    ;; Emit seeAlso.
    (unless (string-equal seeAlso "")
      (emit-in-bold "See Also: ")
      (emit-paraline)
      (emit-paraline)
      (emit-text seeAlso))
    ;; Emit all handlers and properties. 
    (when (or handlers properties)
      (emit-in-bold (format nil "All handlers and properties of ~a: " obj))
      (emit-paraline)
      (emit-paraline)
      (emit-all-handlers-and-properties obj handlers properties))
    ;; Done.
    (emit-topic-end)
    ;; Emit list of properties. 
    (properties->RTF *current-object* properties)
    ;; Emit list of handlers.
    (handlers->RTF *current-object* handlers)
    (setf *current-object* nil)))

(defun function->RTF (name info arguments description example seeAlso)
  (declare (ignore info))
  (let ((contextString (generate-function-contextString name)))
    (report "~%.. Function ~a." name)
    (set-contextString-status contextString :ok)
    ;; Emit context string.
    (emit-context-string contextString)
    ;; Emit topic title.
    (emit-topic-title (format nil "~a (a function)" name))
    (emit-keyword name)
    ;; Emit title
    (emit-page-title (format nil "~a (a function)" name))
    ;; Emit description.
    (unless (string-equal description "")
      (emit-in-bold "Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text description))
    ;; Emit arguments. 
    (when arguments
      (emit-in-bold "Arguments:")
      (emit-paraline)
      (emit-paraline)
      (arguments->RTF arguments))
    ;; Emit example.
    (unless (string-equal example "")
      (emit-simple-header "Example:")
      (emit-paraline)
      (emit-paraline)
      (emit-text example))
    ;; Emit seeAlso.
    (unless (string-equal seeAlso "")
      (emit-in-bold "See Also: ")
      (emit-paraline)
      (emit-paraline)
      (emit-text seeAlso))
    ;; Done.
    (emit-topic-end)))

(defun variable->RTF (name info description example seeAlso)
  (declare (ignore info))
  (let ((contextString (generate-global-contextString name)))
    (report "~%.. Global ~a." name)
    (set-contextString-status contextString :ok)
    ;; Emit context string.
    (emit-context-string contextString)
    ;; Emit topic title.
    (emit-topic-title (format nil "~a (a global)" name))
    (emit-keyword name)
    ;; Emit title
    (emit-page-title (format nil "~a (a global)" name))
    ;; Emit description.
    (unless (string-equal description "")
      (emit-in-bold "Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text description))
    ;; Emit example.
    (unless (string-equal example "")
      (emit-simple-header "Example:")
      (emit-paraline)
      (emit-paraline)
      (emit-text example))
    ;; Emit seeAlso.
    (unless (string-equal seeAlso "")
      (emit-in-bold "See Also: ")
      (emit-paraline)
      (emit-paraline)
      (emit-text seeAlso))
    ;; Done.
    (emit-topic-end)))

(defun constant->RTF (name info description example seeAlso)
  (declare (ignore info))
  (let ((contextString (generate-constant-contextString name)))
    (report "~%.. Constant ~a." name)
    (set-contextString-status contextString :ok)
    ;; Emit context string.
    (emit-context-string contextString)
    ;; Emit topic title.
    (emit-topic-title (format nil "~a (a constant)" name))
    (emit-keyword name)
    ;; Emit title
    (emit-page-title (format nil "~a (a constant)" name))
    ;; Emit description.
    (unless (string-equal description "")
      (emit-in-bold "Description:")
      (emit-paraline)
      (emit-paraline)
      (emit-text description))
    ;; Emit example.
    (unless (string-equal example "")
      (emit-simple-header "Example:")
      (emit-paraline)
      (emit-paraline)
      (emit-text example))
    ;; Emit seeAlso.
    (unless (string-equal seeAlso "")
      (emit-in-bold "See Also: ")
      (emit-paraline)
      (emit-paraline)
      (emit-text seeAlso))
    ;; Done.
    (emit-topic-end)))

;;; _______________________________ 
;;; Messing with the hash table.
;;; _______________________________ 

;;; At the end of the process, we go through the hash table and do the following things:
;;; [1] We point things that have not been documented at a special page. 

;;; Can send you back to the contents so far...

;;; [2] We make a page for each handler and property, listing all the implementors of it.

(defun allHandlers->RTF ()
  (let (handlerName pageContextString)
    (dolist (oneHandler *all-handlers-defined*)
      (setf handlerName (car oneHandler))
      (setf pageContextString (generate-general-handler-contextString handlerName))
      (emit-context-string pageContextString)
      (set-contextString-status pageContextString :ok)
      (emit-topic-title (format nil "All definitions of ~a" handlerName))
      (emit-page-title (format nil "All definitions of ~a" handlerName))
      ;; Now write the whole list. 
      (dolist (aContextString (sort (cadr oneHandler) #'string<))
        (when (eq (contextString-status aContextString) :ok)
          (emit-in-computer-voice-with-hyperlink 
           (contextString->objectName aContextString)
           aContextString)
          (emit-paraline)))
      ;; Write the end of the topic.
      (emit-topic-end))))

(defun allProperties->RTF ()
  (let (propName pageContextString)
    (dolist (oneprop *all-properties-defined*)
      (setf propName (car oneprop))
      (setf pageContextString (generate-general-property-contextString (string-downcase propName)))
      (emit-context-string pageContextString)
      (set-contextString-status pageContextString :ok)
      (emit-topic-title (format nil "All definitions of ~a" propName))
      (emit-page-title (format nil "All definitions of ~a" propName))
      ;; Now write the whole list. 
      (dolist (aContextString (sort (cadr oneprop) #'string<))
        (when (eq (contextString-status aContextString) :ok)
          (emit-in-computer-voice-with-hyperlink 
           (contextString->objectName aContextString)
           aContextString)
          (emit-paraline)))
      ;; Write the end of the topic.
      (emit-topic-end))))

;;; [3] We make a page for each of those categories, to allow easy access to the items. 

(defun allObjects->RTF ()
  (let ((pageContextString (format nil "objects_ALL")))
    (emit-context-string pageContextString)
    (set-contextString-status pageContextString :ok)
    (emit-topic-title (format nil "All Objects"))
    (emit-page-title (format nil "All Objects"))
    (let (anObjName)
      (dolist (aContextString (sort *all-objects-defined* #'string<))
        (setf anObjName (contextString->objectName aContextString))
        ;; Now write the whole list. 
        (when (eq (contextString-status aContextString) :ok)
          (emit-in-computer-voice-with-hyperlink anObjName aContextString)
          (emit-paraline))))
    ;; Write the end of the topic.
    (emit-topic-end)))

(defun allFunctions->RTF ()
  (let ((pageContextString (format nil "functions_ALL")))
    (emit-context-string pageContextString)
    (set-contextString-status pageContextString :ok)
    (emit-topic-title (format nil "All Functions"))
    (emit-page-title (format nil "All Functions"))
    (let (anObjName)
      (dolist (aContextString (sort *all-functions-defined* #'string<))
        (setf anObjName (contextString->objectName aContextString))
        ;; Now write the whole list. 
        (when (eq (contextString-status aContextString) :ok)
          (emit-in-computer-voice-with-hyperlink anObjName aContextString)
          (emit-paraline))))
    ;; Write the end of the topic.
    (emit-topic-end)))

(defun allGlobals->RTF ()
  (let ((pageContextString (format nil "globals_ALL")))
    (emit-context-string pageContextString)
    (set-contextString-status pageContextString :ok)
    (emit-topic-title (format nil "All Global Variables"))
    (emit-page-title (format nil "All Global Variables"))
    (let (anObjName)
      (dolist (aContextString (sort *all-globals-defined* #'string<))
        (setf anObjName (contextString->objectName aContextString))
        ;; Now write the whole list. 
        (when (eq (contextString-status aContextString) :ok)
          (emit-in-computer-voice-with-hyperlink anObjName aContextString)
          (emit-paraline))))
    ;; Write the end of the topic.
    (emit-topic-end)))

(defun allConstants->RTF ()
  (let ((pageContextString (format nil "constants_ALL")))
    (emit-context-string pageContextString)
    (set-contextString-status pageContextString :ok)
    (emit-topic-title (format nil "All Global Constants"))
    (emit-page-title (format nil "All Global Constants"))
    (let (anObjName)
      (dolist (aContextString (sort *all-constants-defined* #'string<))
        (setf anObjName (contextString->objectName aContextString))
        ;; Now write the whole list. 
        (when (eq (contextString-status aContextString) :ok)
          (emit-in-computer-voice-with-hyperlink anObjName aContextString)
          (emit-paraline))))
    ;; Write the end of the topic.
    (emit-topic-end)))

;;; [4] We make the default page, with a little blurb about SK8 and a link to
;;;     objects, functions, globals and constants. 

(defparameter *contents-page-blurb-header*
  (format nil "Welcome to the SK8 online help reference manual. Here you will find ~
               everything that is documented in the SK8 Reference manual. Click on ~
               a topic, any topic..."))

(defparameter *contents-page-blurb-tail*
  (format nil "This online reference, as well as the text of the whole reference ~
               manual was produced by the SK8 engineering team (not by an army of ~
               people dedicated to documenting the system). So, be thankful for ~
               anything that you get out of these pages. Good luck!"))

;;; _______________________________ 
;;; Adding new sections: the FAQs and Basic Concepts.
;;; _______________________________ 

;;; *RTF-stream* has to be set already. 

(defun parse-special-folder (folder prefix sectionTitle)
  (let ((contextStrings nil)
        (theText "")
        title longTitle inString)
    (dolist (aFile (directory folder))
      (when (probe-file aFile)
        ;; Notify the user and stamp the file.
        (format t "~%Parsing   ~a..." aFile)
        (with-open-file (inFile afile :direction :input)
          (let ((theName (pathname-name afile)))
            ;; Parse titles.
            (setf longTitle (read-line inFile))
            (setf longTitle (subseq longTitle (length "TITLE: ")))
            (setf title (read-line inFile))
            (setf title (subseq title (length "SHORT TITLE: ")))
            (emit-context-string (format nil "~a_~a" prefix theName))
            (setf contextStrings (push (list (format nil "~a_~a" prefix theName) longTitle)
                                       contextStrings))
            (emit-topic-title title)
            (emit-page-title longTitle)
            ;; Build up the whole text.
            (loop
              (setf inString (read-line inFile nil nil nil))
              (when (or (null instring)
                        (check-end-comment inString))
                (return))
              (setf theText (concatenate 'string theText inString (string #\Newline))))
            ;; Parse text.
            (emit-text theText)
            ;; Emit topic end.
            (emit-topic-end)
            )))
      (setf theText nil))
    ;; Generate main page.
    (emit-context-string (format nil "~a_Index" prefix))
    (emit-topic-title sectionTitle)
    (emit-page-title sectionTitle)
    (dolist (aTopic contextStrings)
      (emit-in-computer-voice-with-hyperlink (cadr aTopic) (car aTopic))
      (emit-paraline))
    (emit-topic-end)
    ))

(defparameter *basic-concepts-output-file* "BasicConcepts")
(defparameter *faqs-output-file* "FAQs")

(defparameter *otherSections-blurb*
  (format nil "As an added bonus, this online documentation presents two extra sections. ~
               The first one comes to us directly from some version of the SK8 User Guide. ~
               This is the Basic Concepts section, a collection of one pagers describing the ~
               basic concepts of SK8. These are the concepts that any SK8 author should master. ~
               The second section (courtesy of the SK8 DTS team) is a collection of frequently ~
               asked questions about SK8 and how to do things in SK8. The answers usually include ~
               some sample code. "))

(defun produce-other-sections ()
  (with-rtf-for-output *basic-concepts-output-file*
    (parse-special-folder *basic-concepts-folder* "basicconcepts" "Basic Concepts"))
  (with-rtf-for-output *faqs-output-file*
    (parse-special-folder *faqs-folder* "faqs" "SK8 Frequently Asked Questions")))

;;; _______________________________ 
;;; Dealing with undefined entries. 
;;; _______________________________ 

(defparameter *undocumented-objects-blurb* 
  (format nil "The objects in this page are referred to in the documentation but (strangely) are ~
               not documented. This page is generated automatically to inform you of this sad fact ~
               and also to show you the parentage of the undocumented object. "))

(defun output-undocumented-object (contextString theObj)
  (let* ((baseParent (baseParent theObj))
         (parentContextString (generate-object-contextString (objectname baseParent))))
    (emit-context-string contextString)
    (emit-in-computer-voice (objectName theObj))
    (when baseParent 
      (format *RTF-stream* ": a child of ")
      (if (eq (contextString-status (objectName baseParent)) :ok)
        (emit-in-computer-voice-with-hyperlink (objectName baseParent) parentContextString)
        (emit-in-computer-voice (objectName baseParent))))
    (emit-paraline)
    ;; Set the status of this link to ok. 
    (set-contextString-status contextString :ok)))

(defun resolve-undefined-objects ()
  (let ((fileName (format nil "~a~a.rtf" *destination-folder* "RESOLVED-objects"))
        (pageContextString "undocumented_objects_page"))
    (with-open-file (rtfStream filename :direction :output :if-exists :supersede)
      (setf *RTF-stream* rtfStream)
      ;; write the header.
      (format *RTF-stream* *RTF-header*)
      ;; parse the file.
      (emit-context-string pageContextString)
      (emit-topic-title "Undocumented Objects")
      (emit-page-title "Undocumented Objects")
      (emit-paragraph *undocumented-objects-blurb*)
      (emit-paraline)
      (emit-paraline)
      ;; Run through the undocumented objects.
      (dolist (anObjContextString *all-objects-defined*)
        (when (and (eq (contextString-status anObjContextString) :notYet)
                   (contextString->object anObjContextString))
          (output-undocumented-object anObjContextString (contextString->object anObjContextString))))
      (emit-topic-end)
      ;; write the footer.
      (format *RTF-stream* *RTF-footer*))))

(defparameter *undocumented-rest-blurb* 
  (format nil "The item you clicked on is not really documented (even though when creating the documentation ~
               file we hoped it would be at some point). This in no way invalidates the quality of your SK8 ~
               online documentation experience. "))

(defun resolve-the-rest ()
  (let ((fileName (format nil "~a~a.rtf" *destination-folder* "UNDOCUMENTED-things"))
        (pageContextString "things_left_undocumented_page"))
    (with-open-file (rtfStream filename :direction :output :if-exists :supersede)
      (setf *RTF-stream* rtfStream)
      ;; write the header.
      (format *RTF-stream* *RTF-header*)
      ;; parse the file.
      (emit-context-string pageContextString)
      (emit-topic-title "Undocumented Things")
      ;; Emit all offending context strings.
      (maphash #'(lambda (key value) (when (eq value :notYet)
                                       (emit-context-string key)
                                       (format t "~%....*WARNING* could not find: ~a." key)))
               *contextString-hash-table*)
      ;; End the page.
      (emit-page-title "Everything that was left undocumented")
      (emit-paragraph *undocumented-rest-blurb*)
      (emit-topic-end)
      ;; write the footer.
      (format *RTF-stream* *RTF-footer*))))

;;; Runs through the hash-table doing stuff to contextStrings whose status is :notYet.

(defun resolve-undefined-entries ()
  (resolve-undefined-objects)
  (resolve-the-rest))

;;; _______________________________ 
;;; Main code.
;;; _______________________________ 

(defun emit-contents-page ()
  (let ((pageContextString (format nil "global_contents_page")))
    (emit-context-string pageContextString)
    (emit-topic-title (format nil "SK8 Online Reference"))
    (emit-page-title (format nil "SK8 Online Reference"))
    (emit-paragraph *contents-page-blurb-header*)
    (emit-paraline)
    (emit-paraline)
    (emit-in-computer-voice-with-hyperlink "All objects." "objects_ALL")
    (emit-paraline)
    (emit-in-computer-voice-with-hyperlink "All Functions." "functions_ALL")
    (emit-paraline)
    (emit-in-computer-voice-with-hyperlink "All Global Variables." "globals_ALL")
    (emit-paraline)
    (emit-in-computer-voice-with-hyperlink "All Global Constants." "constants_ALL")
    (emit-paraline)
    (emit-paraline)
    (emit-paragraph *otherSections-blurb*)
    (emit-paraline)
    (emit-paraline)
    (emit-in-computer-voice-with-hyperlink "Basic Concepts" "basicConcepts_Index")
    (emit-paraline)
    (emit-in-computer-voice-with-hyperlink "Frequently Asked Questions" "faqs_Index")
    (emit-paraline)
    (emit-paraline)
    (emit-paragraph *contents-page-blurb-tail*)
    (emit-topic-end)))

(defun show-QuickView-compiling-instructions (manualName)
  (ed-beep)
  (message-dialog 
   (format nil "You are done parsing the doc files required to create ~
                the manual. What's left is to compile them using the QuickView ~
                compiler. To do that, open the file ~a.mvp with the ~
                QuickViewª Compiler and select \"Compile\""
           manualName)
   :ok-text "Good luck!"
   :size #@(400 150)))

;;; Creates the .mvp file. This is just a list of RTF files to use in generating the
;;; manual. Here is the text that this function produces:

#|

;the project file for Example #1

[CONFIG] ; this section lists macros that occur at the opening of the Title
Std20Menus() ; add the standard menus
Std20Buttons() ; add the standard buttons

[FILES] ; list the RTF files to compile
example 1.rtf

|#

(defun create-mvp-file (manualName files globalsFile)
  (let ((fileName (format nil "~a~a.mvp" *destination-folder* manualName)))
    (with-open-file (mvpStream filename :direction :output
                               :if-exists :supersede)
      (format mvpStream "[CONFIG]~
                         ~%Std20Menus() ; add the standard menus~
                         ~%Std20Buttons() ; add the standard buttons~
                         ~2%[FILES]~%")
      ;; Do the files.
      (dolist (oneFile files)
        (format mvpStream (format nil "~a.rtf~%" (pathname-name oneFile))))
      (format mvpStream (format nil "~a.rtf~%" *basic-concepts-output-file*))
      (format mvpStream (format nil "~a.rtf~%" *faqs-output-file*))
      (format mvpStream (format nil "~a.rtf~%" globalsFile))
      (format mvpStream (format nil "RESOLVED-objects.rtf~%"))
      (format mvpStream (format nil "UNDOCUMENTED-things.rtf~%"))
      (terpri mvpStream)
      ;; Pointing the main page at the right thing.
      (format mvpStream "[OPTIONS]~
                         ~%contents=global_contents_page
                         ~2%")
      ;; Settings for the main window.
      (format mvpStream "[WINDOWS]~
                         ~%main=\"SK8 Reference\",(20,20,500,600,1),,,,,~
                         ~%")
      (terpri mvpStream))))

(defun docFile->RTF (theFile)
  (let ((fileName (format nil "~a~a.rtf" *destination-folder* (pathname-name theFile))))
    (with-open-file (rtfStream filename :direction :output
                               :if-exists :supersede)
      (setf *RTF-stream* rtfStream)
      ;; write the header.
      (format *RTF-stream* *RTF-header*)
      ;; parse the file. 
      (parse-file theFile)
      ;; write the footer.
      (format *RTF-stream* *RTF-footer*))))

(defparameter *globals-file-name* "globals-file")

(defun produce-globals-file ()
  (let ((fileName (format nil "~a~a.rtf" *destination-folder* *globals-file-name*)))
    (with-open-file (rtfStream filename :direction :output
                               :if-exists :supersede)
      (setf *RTF-stream* rtfStream)
      ;; write the header.
      (format *RTF-stream* *RTF-header*)
      ;; parse the file. 
      (format t "~%....For each handler, writing list of implementor.")
      (allhandlers->RTF)
      (format t "~%....For each property, writing list of implementor.")
      (allProperties->RTF)
      (format t "~%....Writing object list.")
      (allObjects->RTF)
      (format t "~%....Writing function list.")
      (allFunctions->RTF)
      (format t "~%....Writing constants list.")
      (allConstants->RTF)
      (format t "~%....Writing globals list.")
      (allGlobals->RTF)
      (format t "~%....Writing contents page.")
      (emit-contents-page)
      ;; write the footer.
      (format *RTF-stream* *RTF-footer*))))

(defun docs->QuickView (manualName files)
  (init-contextString-hashtable)
  (format t "~%Documentation Compilation started at ~a.~2%" (timeString today :seconds t))
  ;; Run through the files, compiling each one. 
  (dolist (oneFile files)
    (docFile->RTF oneFile))
  ;; Write the special sections.
  (produce-other-sections)
  ;; Write the globals file.
  (produce-globals-file)
  ;; Deal with unresolved references. 
  (resolve-undefined-entries)
  ;; Write the mvp file.
  (create-mvp-file manualName files *globals-file-name*)
  (format t "~%Documentation Compilation ended at ~a.~2%" (timeString today :seconds t))
  ;; Print instructions. 
  (show-QuickView-compiling-instructions manualName))

#|

(docs->QuickView "Clock" (list "ccl;Documentation:Text:Clock.doc"))
(docs->QuickView "Functions" (list "ccl;Documentation:Text:Functions.doc"))
(docs->QuickView "Actor" (list "ccl;Documentation:Text:Actor.doc"))
(docs->QuickView "CheckBox" (list "ccl;Documentation:Text:CheckBox.doc"))

|#
#|
	Change History (most recent last):
	1  	 2/ 5/96	Hernan  	New file. Implements output of parsed doc files into RTF and
						the extra stuff required to generate QuickView databases.
	2  	 2/ 5/96	Hernan  	Fixed strangeness in emit-text.
	3  	 2/ 5/96	Hernan  	Trying to correct the formatting problems. And fixed many
						of them...
	4  	 2/ 5/96	Hernan  	Stupid typo...
	5  	 2/ 5/96	Hernan  	Removing leftover emit-paraline. Also added keyword
						searches to the manual.
	6  	 2/ 6/96	Hernan  	Fixing the duplicate contextString problem by downcasing
						everything that goes into a context string.
	7  	 2/ 6/96	Hernan  	Also kludged a way to resolve unresolved references. The
						warnings are now printed on the SK8 side.
	8  	 2/ 8/96	Hernan  	Adding the basic concepts section and the FAQs section.
	9  	 2/ 8/96	Hernan  	Allowing for comments at the end of faq files.
	10 	 2/ 8/96	Hernan  	Emit-topic-end will output two paralines before emiting the
						page break.
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
