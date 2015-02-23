;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)

#| to do
consistency check: arguments in database against real arguments
|#

;;; **************************************************************************
;;; UTILITIES
;;; **************************************************************************

(defmacro vowelp (char)
  `(position ,char "AEIOUaeiou" :test #'eq))

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

;;; fetch the local handler
(defun fetchhandler (obj name)
  (or (sk8::handlers obj :name name :inherited nil)
      (let ((objname (sk8::objectname obj))
            (setterp (listp name))
            (fun (fboundp name))
            (ob (eq obj mf::*object*))
            temp)
        ;; If not a generic function, return false.
        (unless (eq (type-of fun) 'standard-generic-function)
          (return-from fetchhandler nil))
        (when fun
          (dolist (m (generic-function-methods fun))
            (when (or (and (setq temp (caadr (ccl::%lfun-info (ccl::closure-function (method-function m)))))
                           (string-equal (symbol-name (aref temp 0))
                                         objname))
                      (if ob
                        (eq ps::*true-class*
                            (if setterp
                              (cadr (ccl::method-specializers m))
                              (car (ccl::method-specializers m))))))
              (return m)))))))

;;; thing should be non-empty text
(defun textp (thing)
  (and thing
       (not (string= thing ""))))

;;; converts an objectname (string) into an object, if possible.
(defun object-name->object (str &optional (error-string " in this version of SK8."))
  (let ((s (find-symbol (string-upcase str) :sk8)))
    (unless (boundp s)
      (warning "Could not find object ~a" s error-string)
      (throw :abort-object-output nil))
    (symbol-value s)))

;;; writes start of string delimiter
(defun start-string (s)
  (format s "~%  <String `"))

;;; writes end of string delimiter
(defun end-string (s)
  (format s "'>"))

;;; writes end of para delimiter
(defun end-of-para (s)
  (format s "~% > # end of Para"))

;;; writes end of paraline delimiter
(defun end-paraline (s)
  (format s "~% >"))

;;; writes start of paraline delimiter
(defun start-paraline (s)
  (format s "~%  <ParaLine "))

;;; writes a hard return independent of the context (assumes inside a para)
(defun hard-return (s)
  (start-paraline s)
  (format s "~%   <Char HardReturn >")
  (end-paraline s))

;;; concatenates the contents of input-files into a new destination file

(defun concatenate-files (destination &rest input-files)
  (report "~%Concatenating files...")
  ;; (copy-file (car input-files) destination :if-exists :supersede)
  (with-open-file (output-stream destination
                                 :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :error)
    (dolist (file-name (cdr input-files))
      (terpri output-stream)
      (format output-stream "<Comment - start of concatenated file.>~%")
      (let (ln)
        (with-open-file (input-stream file-name)
          (loop (setq ln (read-line input-stream nil nil) )
                (unless ln (return nil))
                (unless (string= ln "")
                  (write-line ln output-stream)))))
      ;; Signal the end of the file as a comment.
      (format output-stream "<Comment - end of concatenated file.>~%")
      ))
  )

(defun concatenate-into-open-stream (s newFile)
  (report "~%Adding ~a into output stream..." newFile)
  (let (ln)
    (with-open-file (input-stream newFile)
      (loop (setq ln (read-line input-stream nil nil) )
            (unless ln (return nil))
            (unless (string= ln "")
              (write-line ln s))))))

;;; Adds the tbls paragraph to the document.

(defun emit-table-include-line (s tableFile)
  (format s "<Tbls ")
  (format s "~%include (~a)" tableFile)
  ;; Could not make this work!!!
  ;; (format s "~% <include (`<v\\>Aureliano Segundo<c\\>SK8 1.0<c\\>Mif Output<c\\>~a.~a')>"
  ;;         (pathname-name tableFile) (pathname-type tableFile))
  (format s "~%> # end of Tbls~%"))

;;; returns t if the property or handler name is illegal
(defun illegal-name (name)
  (not (every #'(lambda (i)
                  (let ((code (char-code i)))
                    (or (and (> code 96)
                             (< code 123))
                        (and (> code 64)
                             (< code 91))
                        (and (> code 47)
                             (< code 58))
                        (eq i #\Space))))
              name)))

;;; report progress
(defun report (format &rest msg)
  (apply #'format t format msg)
  (stream-force-output *terminal-io*))

;;; go into bold style
(defun in-bold-style (s)
  (format s "~%  <Font ~
             <FTag `'>
       <FWeight `Bold'>
       <FPostScriptName `Palatino-Bold'>
      > # end of Font"))

;;; return to default font
(defun in-default-font (s)
  (format s "~%  <Font ~
             ~%   <FTag `'>~
             ~%  > # end of Font"))

;;; **************************************************************************
;;; GLOBALS
;;; **************************************************************************

(defvar *warnings-on* t)

;;; print a diagnostic warning

(defun warning (format &rest msg)
  (when *warnings-on*
    (apply #'format t (append (list (concatenate 'string "~%~30T*** WARNING *** " format))
                              msg))
    (stream-force-output *terminal-io*)))

(defparameter *header-preTables-filename* "ccl;SK8:Development Utilities:Documentation:Wood-DB->FrameMaker:01-preTables.text")
(defparameter *header-afterTables-filename* "ccl;SK8:Development Utilities:Documentation:Wood-DB->FrameMaker:02-afterTables.text")
(defparameter *trailer-text-filename* "ccl;SK8:Development Utilities:Documentation:Wood-DB->FrameMaker:03-trailer.text")

(defparameter *tables-folder* "ccl;Mif Output:")
(defvar *table-stream* nil)

(defvar *unique-mif-id* 1)

;;; returns a unique id (may have to keep track of the ones used in header and trailer)

(defun new-unique-mif-id ()
  (incf *unique-mif-id*))

;;; how much to indent when printing a line

(defparameter *indent* 0)

(defvar *unique-table-id* 1)

;;; returns a unique table id

(defun new-unique-table-id ()
  (incf *unique-table-id*))

;;; **************************************************************************
;;; TEXT HANDLING
;;; **************************************************************************

(defun emit-textflow-start ()
  (declare (special *framemaker-body-stream*))
  (format *framemaker-body-stream* "~%<TextFlow ~
                                    ~% <TFTag `A' >~
                                    ~% <TFAutoConnect Yes >"))

(defun emit-textflow-end ()
  (declare (special *framemaker-body-stream*))
  (format *framemaker-body-stream* "~%> # end of TextFlow"))

;;; writes a start of para
(defun emit-para-header (s)
  (format s "~% <Para ")
  )

;;; writes an end of Para
(defun emit-para-trailer (s)
  (format s "~% > # end of Para"))

;;; **************************************************************************
;;; EMIT TEXT
;;; **************************************************************************

;;; Outputs in FrameMakes safe form.

(defun output-to-string (str s)
  ;; Open the string form.
  (format s "~%  <String `")
  ;; Write all chars.
  (let (theChar)
    (dotimes (i (length str))
      (case (setq theChar (aref str i))
        (#\> (princ "\\>" s))
        ;; No smart quotes. Let's be consistent!
        (#\Õ (princ "\\q" s))
        (#\Ô (princ "\\q" s))
        (#\' (princ "\\q" s))
        (#\` (princ "\\Q" s))
        (#\\ (princ "\\\\" s))
        (#\Tab (princ "\\t" s))
        (otherwise (princ theChar s)))))
  ;; Close the string off.
  (format s "'>"))

;;; Gets the string with the [[ ]] included. Assumes we are within a paraline!

(defun emit-with-fontChange (str s Ftag)
  (format s "~%  <Font ~
             ~%   <FTag `~a'>~
             ~%  > # end of Font" Ftag)
  (output-to-string str s)
  (format s "~%  <Font ~
             ~%   <FTag `'>~
             ~%  > # end of Font"))

;;; Changes the font and then runs the body. Assumes within a para.

(defmacro with-fontChange (FTag &body body)
  `(prog2
     (format s "~%  <Font ~
                ~%   <FTag `~a'>~
                ~%  > # end of Font" ,Ftag)
     (progn ,@body)
     (format s "~%  <Font ~
                ~%   <FTag `'>~
                ~%  > # end of Font")))
  
;;; Separates the computerVoice from the rest of the text.

(defun emit-paragraph-text (str s &optional (paraline? t))
  (when paraline?
    ;; Open paraline.
    (start-paraline s))
  ;; Output all the text.
  (let ((computerVoiceStart -2)
        (computerVoiceEnd 0)
        (curpos 0))
    (loop
      (setf computerVoiceStart (search "[[" str :start2 (+ computerVoiceStart 2)))
      (if computerVoiceStart
        (progn
          (output-to-string (subseq str curpos computerVoiceStart) s)
          (setf computerVoiceEnd (search "]]" str :start2 (+ computerVoiceStart 2)))
          (if computerVoiceEnd
            (progn (emit-with-fontChange (subseq str (+ computerVoiceStart 2) computerVoiceEnd)
                                         s "AComputerVoice")
                   (setf curpos (+ 2 computerVoiceEnd)))
            (error "Found start but not end of computer voice!")))
        (progn
          (output-to-string (subseq str curpos (length str)) s)
          (return)))))
  (when paraline?
    ;; Close paraline.
    (end-paraline s)))

(defun emit-hardreturn-paraline-text (str s &optional return?)
  (start-paraline s)
  (emit-paragraph-text str s nil)
  (when return?
    (format s "~%  <Char HardReturn>"))
  (end-paraline s))

;;; Calls emit-paragraph-text and adds the newlines by hand.

(defun emit-paragraph-text-with-returns (str s)
  (let ((startpos 0)
        (curpos 0)
        (length (length str)))
    (loop
      (when (= curpos length)
        (emit-hardreturn-paraline-text (subseq str startPos length) s)
        (return))
      (when (char= (aref str curpos) #\Newline)
        (emit-hardreturn-paraline-text (subseq str startpos curpos) s t)
        (setf startpos (1+ curpos)))
      (incf curpos))))

(defun emit-text-paragraph (str s paragraphStyle)
  (format s "~% <Para ~
             ~%  <Unique ~a>~
             ~%  <PgfTag `~a'>"
          (new-unique-mif-id) paragraphStyle)
  (emit-paragraph-text str s)
  (end-of-para s))

(defun normalStyle->codeStyle (normalStyle)
  (cond ((string-equal normalStyle "AProporHandler.Description") "AProperty.Code")
        ((string-equal normalStyle "AObject.Description") "AObject.Code")
        ((or (string-equal normalStyle "APropertyGetterSetter.Syntax")
             (string-equal normalStyle "AFunction.Syntax"))
         normalStyle)
        (t "AProperty.Code")))

;;; Gets code with newlines in it. Must deal with these as hard returns.

(defun emit-code-paragraph (str s paragraphStyle &key useStyleProvided)
  ;; Strip out the brackets.
  (setf str (subseq str 2 (- (length str) 2)))
  ;; Get on with it.
  (format s "~% <Para ~
             ~%  <Unique ~a>~
             ~%  <PgfTag `~a'>"
          (new-unique-mif-id) (if useStyleProvided
                                paragraphStyle
                                (normalStyle->codeStyle paragraphStyle)))
  (emit-paragraph-text-with-returns str s)
  (end-of-para s))

(defun emit-note-paragraph (str s paragraphStyle)
  ;; Strip out the brackets.
  (setf str (subseq str 6 (1- (length str))))
  ;; Get on with it.
  (format s "~% <Para ~
             ~%  <Unique ~a>~
             ~%  <PgfTag `~a'>"
          (new-unique-mif-id) paragraphStyle)
  ;; Add the note in bold.
  (start-paraline s)
  (emit-with-fontChange "Note: " s "ArgumentName")
  (end-paraline s)
  ;; And now the note's text.
  (emit-paragraph-text-with-returns str s)
  ;; Add the note's terminator character.
  (with-fontchange "OurDingbats"
    (start-paraline s)
    (format s "~%  <String `u'>")
    (end-paraline s))
  (end-of-para s))

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

;;; Three cases:
;;; (1) Paragraph starts with "[Note:": look for the end of the note by counting square brackets.
;;; (2) Paragraph starts with "[[": ends as soon as we find the "]]".
;;; (3) Normal text: ends with first newline.

(defun emit-text (str s paragraphStyle &key useStyleProvided)
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
                   (= i (1- length)))
           (setf status :lookingForParagraph)
           (setf endParagraph i)
           ;; Found end! Send paragraph in for processing.
           (emit-text-paragraph (subseq str startParagraph endParagraph) s paragraphStyle)))
        ;; Note paragraph end.
        (:lookingForEndOfNote
         (cond ((char= theChar #\[) (incf bracketCount))
               ((char= theChar #\]) (decf bracketCount)))
         (when (zerop bracketCount)
           (setf status :lookingForParagraph)
           (setf endparagraph i)
           (emit-note-paragraph (subseq str startParagraph (1+ endParagraph)) s paragraphStyle)))
        ;; Code paragraph end.
        (:lookingForEndOfCode
         (cond ((char= theChar #\[) (incf bracketCount))
               ((char= theChar #\]) (decf bracketCount)))
         (when (zerop bracketCount)
           (if (really-a-code-block? str (1+ i) length)
             (progn
               (setf status :lookingForParagraph)
               (setf endparagraph i)
               (emit-code-paragraph (subseq str startParagraph (1+ endParagraph)) s paragraphStyle
                                    :useStyleProvided useStyleProvided))
             ;; Twas a false alarm: a text block started with a computer voice word.
             (setf status :lookingForParagraphEnd))))))))

;;; **************************************************************************
;;; FUNCTIONS, CONSTANTS AND VARIABLES
;;; **************************************************************************

;;; emits text for an individual function
(defun emit-function-text (function_)
  (declare (special *framemaker-body-stream*))

  (let* ((s *framemaker-body-stream*)
         (name (function_name function_))
         (sym (find-symbol (string-upcase name) :sk8))
         (function (and sym (fboundp sym))))
    
    (when (listp function) ; it's a macro
      (warning "~a is a macro--not a function, so it was skipped (not printed)." name)
      (return-from emit-function-text))
    
    (unless function
      (warning "Function ~a in database does not exist, so it was skipped (not printed)." name)
      (return-from emit-function-text))
    
    (report "~%   Function ~a" name)
    
    ;; Function Title:
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AObjectPropOrHandler.Header'>~
               ~%  <TextRectID 7>~
               ~%  <ParaLine ~
               ~%   <Marker ~
               ~%    <MType 2>~
               ~%    <MText `~a (a Function)'>~
               ~%    <MCurrPage 1>~
               ~%    <Unique ~a>~
               ~%   > # end of Marker~
               ~%   <String `~-2*~a'>~
               ~%  >"
            (new-unique-mif-id)
            name
            (new-unique-mif-id))
    (end-of-para s)
    
    ;; Function Syntax:
    (emit-text (make-formal-syntax-description function :function) s "APropertyGetterSetter.Syntax")
  
    ;; Description:
    (when (textp (function_description function_))
      (emit-text (function_description function_) s "AProporHandler.Description"))
    
    ;; Function Arguments:
    (when (function_arguments function_)
      (emit-arguments s (function_arguments function_)))
    
    ;; Example:
    (when (textp (function_example function_))
      (format s "~% <Para ~%  <Unique ~a>~
                 ~%  <PgfTag `AExample.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%    <FPlatformName `M.Palatino.B'>~
                 ~%    <FFamily `Palatino'>~
                 ~%    <FWeight `Bold'>~
                 ~%    <FPostScriptName `Palatino-Bold'>~
                 ~%    <FSize  12.0 pt>~
                 ~%   > # end of Font~
                 ~%   <String `Example'>~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%   > # end of Font~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (function_example function_) s "AExample.Text"))
    
    ;; See Also:
    (when (textp (function_seealso function_))
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `ASeeAlso.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <String `See Also '>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (function_seealso function_) s "ASeeAlso.Text" :useStyleProvided t))
    ))

;;; emits text for an individual constant
(defun emit-constant-text (constant_)
  (declare (special *framemaker-body-stream*))
  (let* ((s *framemaker-body-stream*)
         (name (constant_name constant_))
         (sym (find-symbol (string-upcase name) :sk8))
         (constant (and sym
                        (boundp sym))))
    
    (unless constant
      (warning "Constant ~a in database does not exist." name)
      (return-from emit-constant-text))
    
    (report "~%   Constant ~a" name)
    
    ;; Constant Title:
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AObjectPropOrHandler.Header'>~
               ~%  <TextRectID 7>~
               ~%  <ParaLine ~
               ~%   <String `~a'>~
               ~%  >"
            (new-unique-mif-id) name)
    (end-of-para s)
    
    ;; Description:
    (when (textp (constant_description constant_))
      (emit-text (constant_description constant_) s "AProporHandler.Description"))
    
    ;; Example:
    (when (textp (constant_example constant_))
      (format s "~% <Para ~%  <Unique ~a>~
                 ~%  <PgfTag `AExample.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%    <FPlatformName `M.Palatino.B'>~
                 ~%    <FFamily `Palatino'>~
                 ~%    <FWeight `Bold'>~
                 ~%    <FPostScriptName `Palatino-Bold'>~
                 ~%    <FSize  12.0 pt>~
                 ~%   > # end of Font~
                 ~%   <String `Example'>~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%   > # end of Font~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (constant_example constant_) s "AExample.Text"))
    
    ;; See Also:
    (when (textp (constant_seealso constant_))
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `ASeeAlso.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <String `See Also '>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (constant_seealso constant_) s "ASeeAlso.Text" :useStyleProvided t))
    ))

;;; emits text for an individual variable
(defun emit-variable-text (variable_)
  (declare (special *framemaker-body-stream*))
  (let* ((s *framemaker-body-stream*)
         (name (variable_name variable_))
         (sym (find-symbol (string-upcase name) :sk8))
         (variable (and sym
                        (boundp sym))))
    
    (unless variable
      (warning "Variable ~a in database does not exist." name)
      (return-from emit-variable-text))
    
    (report "~%   Variable ~a" name)
    
    ;; Variable Title:
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AObjectPropOrHandler.Header'>~
               ~%  <TextRectID 7>~
               ~%  <ParaLine ~
               ~%   <String `~a'>~
               ~%  >"
            (new-unique-mif-id) name)
    (end-of-para s)
    
    ;; Description:
    (when (textp (variable_description variable_))
      (emit-text (variable_description variable_) s "AProporHandler.Description"))
    
    ;; Example:
    (when (textp (variable_example variable_))
      (format s "~% <Para ~%  <Unique ~a>~
                 ~%  <PgfTag `AExample.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%    <FPlatformName `M.Palatino.B'>~
                 ~%    <FFamily `Palatino'>~
                 ~%    <FWeight `Bold'>~
                 ~%    <FPostScriptName `Palatino-Bold'>~
                 ~%    <FSize  12.0 pt>~
                 ~%   > # end of Font~
                 ~%   <String `Example'>~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%   > # end of Font~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (variable_example variable_) s "AExample.Text"))
    
    ;; See Also:
    (when (textp (variable_seealso variable_))
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `ASeeAlso.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <String `See Also '>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (variable_seealso variable_) s "ASeeAlso.Text" :useStyleProvided t))
    ))

;;; emits text for all functions
(defun emit-functions-text ()
  (report "~%------------- FUNCTIONS -------------")
  (map-function-doc-entries #'emit-function-text))

;;; emits text for all constants
(defun emit-constants-text ()
  (report "~%------------- CONSTANTS -------------")
  (map-constant-doc-entries #'emit-constant-text))

;;; emits text for all variables
(defun emit-variables-text ()
  (report "~%------------- VARIABLES -------------")
  (map-variable-doc-entries #'emit-variable-text))



;;; **************************************************************************
;;; PROPERTIES AND HANDLERS
;;; **************************************************************************

;;; emits text for a single property
(defun emit-property-text (property_)
  (declare (special *object* *framemaker-body-stream*))
  
  (let* ((s *framemaker-body-stream*)
         (name  (property_name property_))
         (sym (find-symbol (string-upcase name) :sk8))
         (illegal-name nil)
         (private nil)
         (getter (and sym (fetchhandler *object* sym)))
         (setter (and sym (fetchhandler *object* (list 'cl::setf sym)))))
    
    ;; Warn and exit if there's an inconsistency:
    (when (or (not (and getter setter))
              (setq private (sk8::private *object* :property sym))
              (setq illegal-name (illegal-name name)))
      (warning "Property '~a' in database ~a"
               name
               (if illegal-name
                 "had an illegal name, so it was skipped."
                 (if (or setter getter)
                   (format nil "did not have a ~a defined, so I'll print a generic \"undefined\" entry for it." (if getter "setter" "getter"))
                   (if (not (and setter getter))
                     "had neither a getter or setter, so the whole property was skipped (not printed)."
                     (if private
                       "was private, so it was skipped (not printed)."
                       "has some kind of weird problem, so it was skipped (not printed).")))))
      (when (or private
                illegal-name
                (not (or setter getter)))
        (return-from emit-property-text nil)))
    
    ;; Property Title:
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AObjectPropOrHandler.Header'>~
               ~%  <TextRectID 7>~
               ~%  <ParaLine ~
               ~%   <Marker ~
               ~%    <MType 2>~
               ~%    <MText `~a (property of ~a)'>~
               ~%    <MCurrPage 1>~
               ~%    <Unique ~a>~
               ~%   > # end of Marker~
               ~%   <String `~a of ~a'>~
               ~%   <Font ~
               ~%    <FTag `'>~
               ~%   > # end of Font~
               ~%  >"
            (new-unique-mif-id)
            name
            (sk8::objectstring *object*)
            (new-unique-mif-id)
            name
            (sk8::objectstring *object*))
    (end-of-para s)
    
    ;; Description:
    (when (textp (property_description property_))
      (emit-text (property_description property_) s "AProporHandler.Description"))
    
    ;;; EMIT GETTER:
    (when getter
      (report "~%   Property '~a' Getter" name)
      ;; Getter Header:
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `AExample.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <String `Getter'>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      ;; Getter Syntax:
      (emit-text (make-formal-syntax-description getter :getter) s "APropertyGetterSetter.Syntax")
      ;; Getter Description:
      (when (textp (property_getter_description property_))
        (emit-text (property_getter_description property_) s "AGetterSetter.Description"))
      ;; Getter Arguments:
      (when (property_getter_arguments property_)
        (emit-arguments s (property_getter_arguments property_)))
      )
    
    (when setter
      ;;; EMIT SETTER:
      ;; Setter Header:
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `AExample.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <String `Setter'>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      ;; Setter Syntax:
      (emit-text (make-formal-syntax-description setter :setter) s "APropertyGetterSetter.Syntax")
      ;; Setter Description:
      (when (textp (property_setter_description property_))
        (emit-text (property_setter_description property_) s "AGetterSetter.Description"))
      ;; Setter Arguments:
      (when (property_setter_arguments property_)
        (emit-arguments s (property_setter_arguments property_)))
      )
    
    ;; Example:
    (when (textp (property_example property_))
      (format s "~% <Para ~%  <Unique ~a>~
                 ~%  <PgfTag `AExample.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <String `Example'>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (property_example property_) s "AExample.Text"))
    
    ;; See Also:
    (when (textp (property_seealso property_))
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `ASeeAlso.Header'>~
                 ~%  <TextRectID 7>~
                 ~%  <ParaLine ~
                 ~%   <String `See Also '>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text  (property_seealso property_) s "ASeeAlso.Text" :useStyleProvided t))
    ))

;;; emits text for a single handler
(defun emit-handler-text (handler_)
  (declare (special *object* *framemaker-body-stream*))
  (let* ((s *framemaker-body-stream*)
         (name (handler_name handler_))
         (sym (find-symbol (string-upcase name) :sk8))
         (illegal-name nil)
         (handler (and sym
                       (fetchhandler *object* (find-symbol (string-upcase name) :sk8)))))
    
    ;; Warn and exit if there's an inconsistency:
    (when (or (null handler)
              (sk8::private *object* :property sym)
              (setq illegal-name (illegal-name name)))
      (warning "Handler '~a' in database ~a, so it was skipped (not printed)."
               name
               (if illegal-name
                 "has an illegal name"
                 (if (null handler)
                   "was not defined"
                   "was private")))
      (return-from emit-handler-text nil))

    (report "~%   Handler '~a'" name)
    
    ;; Handler Title:
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AObjectPropOrHandler.Header'>~
               ~%  <TextRectID 7>~
               ~%  <ParaLine ~
               ~%   <Marker ~
               ~%    <MType 2>~
               ~%    <MText `~a (handler of ~a)'>~
               ~%    <MCurrPage 1>~
               ~%    <Unique ~a>~
               ~%   > # end of Marker~
               ~%   <String `~a of ~a'>~
               ~%   <Font ~
               ~%    <FTag `'>~
               ~%   > # end of Font~
               ~%  >"
            (new-unique-mif-id)
            name
            (sk8::objectstring *object*)
            (new-unique-mif-id)
            name
            (sk8::objectstring *object*))
    (end-of-para s)
    
    ;; Handler Syntax:
    (emit-text (make-formal-syntax-description handler :handler) s "APropertyGetterSetter.Syntax")
    
    ;; Description:
    (when (textp (handler_description handler_))
      (emit-text (handler_description handler_) s "AProporHandler.Description"))
    
    ;; Handler Arguments:
    (when (handler_arguments handler_)
      (emit-arguments s (handler_arguments handler_)))
    
    ;; Example:
    (when (textp (handler_example handler_))
      (format s "~% <Para ~%  <Unique ~a>~
                 ~%  <PgfTag `AExample.Header'>~
                 ~%  <ParaLine ~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%    <FPlatformName `M.Palatino.B'>~
                 ~%    <FFamily `Palatino'>~
                 ~%    <FWeight `Bold'>~
                 ~%    <FPostScriptName `Palatino-Bold'>~
                 ~%    <FSize  12.0 pt>~
                 ~%   > # end of Font~
                 ~%   <String `Example'>~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%   > # end of Font~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (handler_example handler_) s "AExample.Text"))
    
    ;; See Also:
    (when (textp (handler_seealso handler_))
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `ASeeAlso.Header'>~
                 ~%  <ParaLine ~
                 ~%   <String `See Also '>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (handler_seealso handler_) s "ASeeAlso.Text" :useStyleProvided t))
    ))

;;; emits all properties for the object
(defun emit-properties (s object_)
  (when (object_properties object_)
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AHandlersSection.Header'>~
               ~%  <TextRectID 7>~
               ~%  <ParaLine ~
               ~%   <String `Properties of ~a'>~
               ~%  >"
            (new-unique-mif-id) (object_name object_))
    (end-of-para s)
    (map-property-doc-entries #'emit-property-text object_)))

;;; emits all handlers for the object
(defun emit-handlers (s object_)
  (when (object_handlers object_)
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AHandlersSection.Header'>~
               ~%  <TextRectID 7>~
               ~%  <ParaLine ~
               ~%   <String `Handlers of ~a'>~
               ~%  >"
            (new-unique-mif-id) (object_name object_))
    (end-of-para s)
    (map-handler-doc-entries #'emit-handler-text object_)))

(defun property-name-from-setter-symbol (setterSymbol)
  (let ((setterString (format nil "~a" setterSymbol))
        thePos)
    (if (setf thePos (position #\: setterString :from-end t))
      (subseq setterString (1+ thePos))
      setterString)))

;;; makes a getter syntax statement
;;; returns a string with the right format for emission. Encloses the result in [[ ]] to signify should be treated as a code block.

(defun make-formal-syntax-description (h type)
  (flet ((makeargs (handlerObj type)
           (let* ((name nil)
                  (lfun (cond
                         ((symbolp handlerObj)
                          (setq name handlerObj)
                          (symbol-function handlerObj))
                         
                         ((functionp handlerObj)
                          (setq name (function-name handlerObj))
                          (when (typep name 'method) (setq name (method-name name)))
                          handlerObj)
                         (t ;must be a method
                          (setq name (method-name handlerObj))
                          (method-function handlerObj))))
                  (result (arglist lfun)))
             (case type
               ((:getter :handler) (cdr result))
               (:setter (cddr result))
               (:function result)))))
    
    (let ((name (sk8::name h))
          (key nil) ; are we looking at keyword args?
          (optional nil) ; are we looking at optional args?
          (required t) ; are we looking at required args?
          (first-arg t)
          sym)
      (concatenate 
       'string
       "[["
       (with-output-to-string (s)
         ;; emit preamble:
         (format s
                 (case type
                   (:getter (format nil "get the ~a of me " name))
                   (:setter (format nil "set the ~a of me" (setq name (property-name-from-setter-symbol name))))
                   (:handler (format nil "~a me" name))
                   (:function (format nil "~a " name))))
         (block genargs
           (do ((args (makeargs h type) (cdr args)))
               ((null args))
             (setq sym (car args))
             (case sym
               (&key (setq required nil
                           key t
                           optional nil))
               (&optional (setq required nil
                                key nil
                                optional t))
               (&rest (setq required nil
                            key nil
                            optional t))
               (&aux (return-from genargs nil))
               (OTHERWISE
                (cond ((or required optional)
                       (if (eq type :handler)
                         (princ ", " s)
                         (if (and (eq type :function)
                                  (not first-arg))
                           (princ ", " s)))
                       (setq first-arg nil)
                       (princ (pretty-symbol-name sym) s))
                      (key
                       (format s "~%~awith ~a" #\Tab (pretty-symbol-name sym) )))))))
         (when (eq type :setter) (format s "~%~ato NewValue" #\Tab)))
       "]]"))))

#|
(make-formal-syntax-description 
   (fetchhandler sk8::actor 'sk8::boundsrect)
     :getter)
(make-formal-syntax-description 
   (fetchhandler sk8::actor '(setf sk8::boundsrect))
     :setter)
(make-formal-syntax-description 
   (fetchhandler sk8::actor 'sk8::drag)
   :handler)
(make-formal-syntax-description 
   (fetchhandler sk8::actor 'sk8::draggingmouseleave)
   :handler)
(make-formal-syntax-description 
   (fetchhandler sk8::actor 'sk8::setboundsrect)
   :handler)
(make-formal-syntax-description 
 (fboundp 'foo)
 :function)
|#




;;; **************************************************************************
;;; NOTES
;;; **************************************************************************

;;; emits a string as a note
(defun emit-note (str)
  (declare (special *framemaker-body-stream*))
  (format *framemaker-body-stream* "~% <Para ~%  <Unique ~a>~%  <PgfTag `AImportant.Note'>~%  <ParaLine ~%  <String `~a '>~%  >~% > # end of Para"
          (new-unique-mif-id) str))

;;; **************************************************************************
;;; ARGUMENTS
;;; **************************************************************************

(defun within-arguments-table (textStream tableFormat thefun)
  (declare (special *framemaker-tables-stream*))
  (let ((s *framemaker-tables-stream*)
        (tableNumber (new-unique-table-id)))
    ;; [1] Create a table and add it to table stream.
    (format s "~% <Tbl ~
               ~%  <TblID ~a> ~
               ~%  <TblTag `~a'> ~
               ~%  <TblNumColumns 1> ~
               ~%  <TblBody ~
               ~%   <Row ~
               ~%    <Cell ~
               ~%     <CellContent " 
            tableNumber tableFormat)
    ;; [2] Write the arguments into it.
    (funcall theFun s)
    ;; [3] End the table.
      (format s "~%     > # end of CellContent ~
               ~%    > # end of Cell ~
               ~%   > # end of Row ~
               ~%  > # end of TblBody ~
               ~% > # end of Tbl ")
    ;; [4] Write a reference to the table in the textflow.
      (format textStream "~% <Para ~
                        ~%  <Unique ~a> ~
                        ~%  <ParaLine ~
                        ~%   <ATbl ~a> ~
                        ~%  > # end of Paraline ~
                        ~% > # end of Para "
            (new-unique-mif-id) tableNumber)
    ))

;;; Gets a list of argument objects.

(defun emit-arguments (s arguments &optional (tableFormat "ArgumentTable"))
  (when arguments
    (within-arguments-table
        s
        tableFormat
     #'(lambda (theStream)
         ;; Emit first arg.
         (emit-an-argument theStream (car arguments) t)
         ;; Emit others.
         (dolist (anArg (cdr arguments))
           (emit-an-argument theStream anArg))))))

(defun emit-an-argument (s theArg &optional firstArg?)
  (let ((name (argument_name theArg))
        (type (argument_type theArg))
        (required (argument_required theArg))
        (description (argument_description theArg))
        (paralineSignaled nil))
    ;; name is bold, type within parens. If a description is supplied add ":" at the end.
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `Argument.Heading'>~
               ~%  <PgfWithPrev ~a> ~
               ~%  <ParaLine"
            (new-unique-mif-id) (if firstArg? "No" "Yes"))
    ;; Now write the name in bold.
    (format s "~%  <Font ~
               ~%   <FTag `ArgumentName'> ~
               ~%  > # end of Font ~
               ~%  <String `~a'> ~
               ~%  <Font ~
               ~%   <FTag `'> ~
               ~%  > # end of Font "
            (if required
              name
              (concatenate 'string "[" name "]")))
    ;; Now, it provided, the type.
    (when (stringp type)
      (format s "~%  <String ` (~a)'>" type))
    ;; If optional, show it.
    ;; (unless required
    ;;  (format s "~%  <String `, optional'>"))
    ;; If there is a description, add the ":".
    (when (textp description)
      ;; Get rid of all new lines in the description. Argument descriptions should
      ;; not be more than one paragraph!
      (when (position #\Newline description)
        (setf description (remove #\Newline description)))
      (format s "~%  <String `: '>")
      (progn (end-paraline s)
             (setf paralineSignaled t))
      (emit-paragraph-text description s))
      ;; End the paraline and the paragraph.
      (unless paralineSignaled (end-paraline s))
    (end-of-para s)))

;;; **************************************************************************
;;; OBJECTS
;;; **************************************************************************

;;; emits object text, catching an abort
(defun maybe-emit-object-text (object_)
  (catch :abort-object-output
    (emit-object-text object_)))

;;; emits object text
(defun emit-object-text (object_)
  (declare (special *framemaker-body-stream*))
  (let ((*object* (object-name->object (object_name object_)))
        (s *framemaker-body-stream*))
    (declare (special *object*))
    
    (report "~%~%Object ~a" (object_name object_))
    
    ;; Object Title:
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AObject.Header'>~
               ~%  <TextRectID 7>~
               ~%  <PgfNumString `1'>~
               ~%  <ParaLine ~
               ~%   <Marker ~
               ~%    <MType 2>~
               ~%    <MText `~a'>~
               ~%    <MCurrPage 1>~
               ~%    <Unique ~a>~
               ~%   > # end of Marker~
               ~%   <String `~a'>~
               ~%  >"
            (new-unique-mif-id)
            (object_name object_)
            ;; (mapcar #'sk8::objectstring (sk8::parents *object*))
            (new-unique-mif-id)
            (object_name object_)
            ;;(mapcar #'sk8::objectstring (sk8::parents *object*))
            )
    (end-of-para s)

    ;; Object parentage:
    (format s "~% <Para ~
               ~%  <Unique ~a>~
               ~%  <PgfTag `AObject.Parentage'>~
               ~%  <ParaLine ~
               ~%   <String `~a'>~
               ~%  >~
               ~%  > # end of Para"
            (new-unique-mif-id)
            (parse-parents (sk8::parents *object*)))
    
    ;; Description:
    (emit-text (object_description object_) s "AObject.Description")
    
    ;; Example:
    (when (textp (object_example object_))
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `AObjectExample.Header'>~
                 ~%  <ParaLine ~
                 ~%   <Font ~
                 ~%    <FTag `'>~
                 ~%    <FPlatformName `M.Palatino.B'>~
                 ~%    <FFamily `Palatino'>~
                 ~%    <FWeight `Bold'>~
                 ~%    <FPostScriptName `Palatino-Bold'>~
                 ~%    <FSize  12.0 pt>~
                 ~%   > # end of Font~
                 ~%   <String `Example'>~
                 ~%   >~
                 ~% > # end of Para"
              (new-unique-mif-id))
      (emit-text (object_example object_) s "AObject.Description"))
    
    ;; See Also:
    (when (textp (object_seealso object_))
      (format s "~% <Para ~
                 ~%  <Unique ~a>~
                 ~%  <PgfTag `AObjectSeeAlso.Header'>~
                 ~%  <ParaLine ~
                 ~%   <String `See Also '>~
                 ~%  >"
              (new-unique-mif-id))
      (end-of-para s)
      (emit-text (object_seealso object_) s "AObjectSeeAlso.Text" :useStyleProvided t))
    
    ;; Do properties:
    (emit-properties s object_)
    
    
    ;; Do handlers:
    (emit-handlers s object_)
    
    ))

;;; **************************************************************************
;;; MAIN FUNCTION: emit-framemaker-doc
;;; **************************************************************************

(defun emit-section-heading (sectionTitle)
  (declare (special *framemaker-body-stream*))
  (format *framemaker-body-stream*
          "~% <Para ~
           ~%  <Unique ~a>~
           ~%  <PgfTag `Chapter.Header'>~
           ~%  <ParaLine ~
           ~%   <TextRectID 7>~
           ~%   <String `~a'>~
           ~%  >~
           ~% > # end of Para"
          (new-unique-mif-id) sectionTitle))

;;; main function for creating a new framemaker object reference document from a SK8 on-line database

(defun emit-framemaker-doc (filename &key this-object-only 
                                         (functions t) (variables t) 
                                         (constants t) (objects t))
  (window-select (front-window :class 'listener))
  (report "~%Time translation started: ~a" (get-time-string))
  (let* ((*unique-mif-id* 1)
         (*unique-table-id* 1)
         (tableFilename (concatenate 'string (pathname-name filename) "Tables.mif"))
         (completeTablePath (concatenate 'string *tables-folder* tableFileName))
          object_)
    ;; [1] Copy the header file into the destination.
      (copy-file *header-preTables-filename* filename :if-exists :supersede)
    ;; [2] Append the body to it.
    (with-open-file (*framemaker-body-stream* filename
                                              :direction :output
                                              :if-exists :append
                                              :if-does-not-exist :error)
      (declare (special *framemaker-body-stream*))
      ;; Write the file to include the tables.
      (emit-table-include-line *framemaker-body-stream* tableFilename)
      ;; Write the rest of the preface.
      (concatenate-into-open-stream *framemaker-body-stream* *header-afterTables-filename*)
      ;; Now write the text itself.
      (with-open-file (*framemaker-tables-stream* completeTablePath
                                                  :direction :output
                                                  :if-exists :supersede
                                                  :if-does-not-exist :create)
        (declare (special *framemaker-tables-stream*))
        (emit-textflow-start)
        (with-help-file
          ;; [] Objects.
          (when objects
            (report "~%------------- OBJECTS -------------")
            (emit-section-heading "Object Reference")
            (dolist (name (if (listp objects) objects (fetch-all-object-keys)))
              (when (and (setq object_ (fetch-object name))
                         (if this-object-only
                           (string-equal (object_name object_) this-object-only)
                           t))
                (maybe-emit-object-text object_)
                (if this-object-only
                  (return nil)))
              ))
          ;; [] Functions.
          (when functions
            (emit-section-heading "Functions")
            (emit-functions-text))
          ;; [] Constants:
          (when constants
            (emit-section-heading "Constants")
            (emit-constants-text))
          ;; [] Vartiables.
          (when variables
            (emit-section-heading "Global Variables")
            (emit-variables-text))
          )
        );; close tables file.
      (emit-textflow-end)
      ) ; close body file
    ;; [3] Append the trailer file to the result file.
    (concatenate-files filename *trailer-text-filename*)
    (set-mac-file-creator filename "Fram")
    (report "~%Time translation ended: ~a" (get-time-string))
    ))

(defun compile-children-names (kids)
  (cond ((null kids) nil)
        (t (append (compile-hierarchy-names (car kids))
                   (compile-children-names (cdr kids))))))

(defun compile-hierarchy-names (rootObj)
  (cons (string-upcase (sk8::objectName rootObj))
        (compile-children-names (sk8::knownChildren rootObj))))

(defun emit-framemaker-hierarchy-doc (fileName rootObj)
  (let ((names (compile-hierarchy-names rootObj)))
    (emit-frameMaker-doc fileName :objects names :constants nil :functions nil :variables nil)))

#| Test

(defun getHandler (theObj theHandler)
  (with-help-file
    (let ((object_ (fetch-object theObj)))
      (when object_
        (fetch-handler object_ theHandler)))))

(defun getProperty (theObj theHandler)
  (with-help-file
    (let ((object_ (fetch-object theObj)))
      (when object_
        (fetch-property object_ theHandler)))))

(format t "~%  <PgfNumString `CHAPTER 1\\t'>")

(emit-framemaker-doc "ccl;Mif Output:Actor.mif" :this-object-only "actor" :constants nil :variables nil :functions nil)
(emit-framemaker-doc "ccl;Mif Output:LineSegment.mif" :this-object-only "lineSegment" :constants nil :variables nil :functions nil)
(emit-framemaker-doc "ccl;Mif Output:QuickTimeMovie.mif" :this-object-only "QuickTimeMovie" :constants nil :variables nil :functions nil)
(emit-framemaker-doc "ccl;Mif Output:Polygon.mif" :this-object-only "polygon" :constants nil :variables nil :functions nil)
(emit-framemaker-doc "ccl;Mif Output:Object.mif" :this-object-only "object"  :constants nil :variables nil :functions nil)

(emit-framemaker-doc "ccl;Mif Output:Functions.mif" :objects nil :constants nil :variables nil :functions t)
(emit-framemaker-doc "ccl;Mif Output:Constants.mif" :objects nil :constants t :variables nil :functions nil)
(emit-framemaker-doc "ccl;Mif Output:Globals.mif" :objects nil :constants nil :variables t :functions nil)

;;; THIS ONE GENERATES THE WHOLE BOOK...

(cl-user::emit-framemaker-doc "ccl;Mif Output:SK8REF.mif" :objects t :constants t :variables t :functions t)

;;; This one generates part of the hierarchy...

(emit-framemaker-hierarchy-doc "ccl;Mif Output:Clocks.mif" sk8::Clock)
(emit-framemaker-hierarchy-doc "ccl;Mif Output:Translators.mif" sk8::Translator)

|#


#|
	Change History (most recent last):
	1		 8/1/94	Hernan	New file.
	2		 8/3/94	kleiman	initial development
	3		 8/3/94	kleiman	emit-framemaker-doc
	4		 8/4/94	kleiman	final debugging phase
	5		 8/5/94	kleiman	fixed FrameMaker MIF bugs
	6		 8/5/94	kleiman	
	7		 8/8/94	nil	
	8		 8/8/94	nil	
	9		 8/9/94	kleiman	fixed bugs and enhanced tables
	10		 8/9/94	kleiman	make required argument bold
	11		 8/9/94	kleiman	indexing
	12		 8/9/94	kleiman	headers show section title
	13		 8/10/94	kleiman	more useful warnings
	14		 8/10/94	kleiman	fix up constants, variables and function output
	15		 8/11/94	kleiman	chapter headings
	16 	 8/26/94	Hernan  	Changing the pathname to the prefix files.
	17 	 9/ 9/94	Hernan  	Total rewrite of the whole emit-text part. The
							arguments are now printed in simple tables.
	18 	 9/ 9/94	Hernan  	Fixing the pathnames to the mif header files.
							Also changed the end of note character to the
							standard thing.
	19 	 9/12/94	Hernan  	Now making a table file for each file we generate.
	20 	 9/13/94	Hernan  	Fixed page numbering/header/footer problem. 
							Also added rudimentary page numbering for the 
							whole book.
	21 	 9/27/94	Hernan  	Making functions, globals and constants be
							formatted like handlers.
	22 	 9/30/94	Hernan  	Made the types of objects appear in the line 
							below the object's name.
	23 	11/15/94	Hernan  	Name of handler now returns a symbol (not a list!)
							Also fetchHandler returns false when the function
							is not a generic function.
	24 	 4/ 5/95	Hernan  	Adding the capability to generate the book using
							the hierarchy for order.
	25 	 4/25/95	dy      	Set the .mif file creator to FrameMaker
	2  	 7/28/95	sidney  	MCL3.0 change - (front-window :class 'listener)
	3  	12/18/95	sidney  	work around bug in read-line in a way that will also work when it is fixed
	4  	 1/19/96	sidney  	removing/rewriting refrences to old sk8script compiler
	5  	 2/ 7/96	sidney  	remove wood
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
