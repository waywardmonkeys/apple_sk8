;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8dev)

;;; SK8Script indentation engine. Does NOT call the parser for anything.

(defconstant *whitespace-charbag* '(#\Space #\Tab #\Newline))

;;; _______________________________ 
;;; Macros.
;;; _______________________________ 

(defmacro find-forward (theBuffer theString &optional startPos)
  `(ccl::buffer-forward-search ,theBuffer ,theString ,startPos))

(defmacro jump-whitespace (theBuffer &optional startPos)
  `(ccl::buffer-fwd-skip-wsp ,theBuffer
                             (or ,startPos (buffer-position ,theBuffer))))

(defmacro jump-whitespace-bwd (theBuffer &optional startPos)
  `(ccl::buffer-bwd-skip-wsp ,theBuffer
                             (or ,startPos (buffer-position ,theBuffer))))

(defmacro jump-darkspace (theBuffer &optional startPos)
  `(ccl::buffer-fwd-sexp ,theBuffer (or ,startPos (buffer-position ,theBuffer))))

(defmacro jump-darkspace-bwd (theBuffer &optional startPos)
  `(ccl::buffer-bwd-sexp ,theBuffer (or ,startPos (buffer-position ,theBuffer))))

(defmacro do-buffer-lines-reverse ((line-start-pos theBuffer) &body body)
  `(progn
     (setf ,line-start-pos (1- (buffer-line-start ,theBuffer ,line-start-pos)))
     (loop
       (when (minusp ,line-start-pos) 
         (setf ,line-start-pos 0)
         (return))
       (setf ,line-start-pos (buffer-line-start ,theBuffer ,line-start-pos))
       ,@body
       (setf ,line-start-pos (1- ,line-start-pos)))))

;;; For some reason, you cannot cache the size of the buffer, because it is 
;;; incorrect. Oy!

(defmacro do-buffer-lines ((line-start-pos theBuffer) &body body)
  `(progn
     (setf ,line-start-pos (buffer-line-start ,theBuffer ,line-start-pos))
     (loop
       (when (>= ,line-start-pos (buffer-size ,theBuffer))
         (return))
       ,@body
       (setf ,line-start-pos (1+ (buffer-line-end ,theBuffer ,line-start-pos)))
       )))

;;; _______________________________ 
;;; Utilities
;;; _______________________________ 

;;; Returns whether the given string is within the line that starts at start-line-pos.

(defun line-contains-string? (theBuffer start-line-pos theString)
  (let* ((eol (buffer-line-end theBuffer start-line-pos))
         (caseMarkerPos (find-forward theBuffer theString start-line-pos)))
    (and caseMarkerPos (<= caseMarkerPos eol))))
  
;;; Returns the first word of the current line in the buffer.

(defun first-word (theBuffer &optional position)
  (let* ((line-start (buffer-line-start theBuffer position))
         (wordStartPos (jump-whiteSpace theBuffer line-start))
         wordEndPos)
    (if (or (null wordStartPos)
            (char= #\Newline (buffer-char theBuffer wordStartPos)))
      ""
      (progn
        (setf wordEndPos (jump-darkspace theBuffer wordStartPos))
        (if wordEndPos
          (buffer-substring theBuffer wordStartPos wordEndPos)
          "")))))

(defun last-word (theBuffer &optional position)
  (let* ((line-end (buffer-line-end theBuffer position))
         (wordEndPos (jump-whiteSpace-bwd theBuffer line-end))
         (wordStartPos (when wordEndPos
                         (jump-darkspace-bwd theBuffer wordEndPos)))
         (theWord (when (and wordStartPos wordEndPos)
                    (buffer-substring theBuffer wordStartPos wordEndPos))))
    (when theWord
      (if (and (string-equal theWord "Â") (> (buffer-size theBuffer) line-end))
        (last-word theBuffer (1+ line-end))
        theWord))))

;;; Removes all tabs at the start of the line and inserts the required number.

(defun set-indent (theBuffer numTabs)
  (unless (minusp numTabs)
    (let* ((newTabs (make-string numTabs :initial-element #\Tab))
           (line-start (buffer-line-start theBuffer))
           (startWordPos (jump-whitespace theBuffer line-start)))
      (buffer-delete theBuffer line-start startWordPos)
      (buffer-insert theBuffer newTabs line-start))))

;;; Returns true if the previous line ends with a continuation character.
;;; FIX IT: should also make sure the continuation char is the last thing in the line.

(defun continuation-line? (theBuffer start-line-pos)
  (let ((prevLineEol (1- (buffer-line-start theBuffer start-Line-Pos))))
    (unless (minusp prevLineEol)
      (setf start-Line-Pos (buffer-line-start theBuffer prevLineEol))
      (line-contains-string? theBuffer start-line-pos "Â"))))

(defun countTabs (theBuffer start-line-pos)
  (let ((count 0))
    (loop
      (if (char= (buffer-char theBuffer start-line-pos) #\Tab)
        (incf count)
        (return-from counttabs count))
      (incf start-line-pos))))

(defun first-token-prev-line-is (keyword lineStartPos buf)
  (declare (ignore lineStartPos))
  (string-equal keyword (first-word buf (1- (buffer-line-start buf)))))

(defun first-token-prev-line-is-not (keyword lineStartPos buf)
  (declare (ignore lineStartPos))
  (not (string-equal keyword (first-word buf (1- (buffer-line-start buf))))))

(defun last-token-is (theWord lineStartPos buf)
  (if (listp theWord)
    (member (last-word buf lineStartPos) theWord :test #'string-equal)
    (string-equal theWord (last-word buf lineStartPos))))

(defun count-tokens (buf lineStartPos) 
  (let ((startPos (buffer-line-start buf lineStartPos))
        (endPos (buffer-line-end buf lineStartPos))
        (count 0)
        (dark? nil))
    (dotimes (i (- endPos startPos) count)
      (when (position (buffer-char buf startPos) *whitespace-charbag*)
        (setf dark? nil))
      (when (and (not dark?)
                 (not (position (buffer-char buf startPos) *whitespace-charbag*)))
        (setf dark? t)
        (incf count))
      (incf startPos))))

(defun num-tokens-is (num lineStartPos buf)
  (eql num (count-tokens buf lineStartPos)))

;;; Travels up the code looking for the first open if statement... 
;;; Returns the position where its line starts.

(defun endIfLine? (buffer lineStartPos)
  (or
   ;; line is "end if"
   (and (string-equal (first-word buffer lineStartPos) "END")
        (string-equal (last-word buffer lineStartPos) "IF"))
   ;; line is an else whose last word is not else.
   (and (string-equal (first-word buffer lineStartPos) "ELSE")
        (not (string-equal (last-word buffer lineStartPos) "ELSE")))
   ))

(defun ifLine? (buffer lineStartPos)
  (and (string-equal (first-word buffer lineStartPos) "IF")
       (or (string-equal (last-word buffer lineStartPos) "THEN")
           (string-equal (last-word buffer lineStartPos) "OF"))))
      
(defun find-enclosing-if (buffer lineStartPos)
  (let ((curIfLevel 1))
    (do-buffer-lines-reverse (lineStartPos buffer)
      (cond ((endIfLine? buffer lineStartPos) (incf curIfLevel))
            ((ifLine? buffer lineStartPos) (decf curIfLevel)))
      (when (zerop curIfLevel)
        (return-from find-enclosing-if lineStartPos)))))

(defun enclosing-if-ends-with (endWord lineStartPos buf)
  (let ((ifStartPos (find-enclosing-if buf lineStartPos)))
    (and ifStartPos
         (string-equal (last-word buf ifStartPos) endWord))))

;;; _______________________________ 
;;; Indentors.
;;; _______________________________ 

;;; The basic class.

(defclass sk8-script-indentor ()
  ((trigger :initform nil :initarg :trigger :accessor trigger)
   (action :initform nil :initarg :action :accessor action)
   (condition :initform nil :initarg :condition :accessor condition)
   (nextAction :initform nil :initarg :nextAction :accessor nextAction)
   (nextCondition :initform nil :initarg :nextCondition :accessor nextCondition)
   ))

(defparameter *default-indentor* (make-instance 'sk8-script-indentor))

;;; Methods everyone shares. 

(defmethod action->levelDelta ((indentor sk8-script-indentor) theBuffer curPos actionType)
  (case (funcall actionType indentor)
    (indent (if (conditions-Satisfied? (if (eq actionType 'action)
                                         (condition indentor)
                                         (nextCondition indentor))
                                       theBuffer curPos) 1 0))
    (outdent (if (conditions-Satisfied? (if (eq actionType 'action)
                                         (condition indentor)
                                         (nextCondition indentor))
                                        theBuffer curPos) -1 0))
    (otherwise 0)))

;;; Return the first word and indentation level of the previous line that is not
;;; a continuation line.

(defmethod significant-previous-line ((indentor sk8-script-indentor) theBuffer)
  (if (zerop (buffer-line-start theBuffer))
    (values nil 0 nil)
    (let ((curPos (buffer-position theBuffer)))
      (do-buffer-lines-reverse (curpos theBuffer)
        (unless (continuation-Line? theBuffer curPos)
          (return)))
      ;; CurPos points at the significant line.
      (values (first-word theBuffer curpos)
              (countTabs theBuffer curPos)
              curPos))))

(defparameter *construct-indentation-types* nil)

;;; Indentors whose keywords are strings, are placed at the head of the list.
;;; Indentors whose keywords are symbols are placed at the end. This way we test
;;; for string equality first.

(defun add-indentation-type (indentor)
  ;; First remove an item with the same keyword (if there is one).
  (let* ((newTrigger (trigger indentor))
         (alreadythere? (find-if #'(lambda (key) (if (Stringp key) 
                                                  (String-equal key newTrigger)
                                                  (eq key newTrigger)))
                                *construct-indentation-types*
                                :key #'trigger)))
    (when alreadythere?
      (setf *construct-indentation-types* (remove alreadythere? *construct-indentation-types*)))
    ;; Ok. Now add the new one.
    (if (stringp newTrigger)
      (setf *construct-indentation-types*
            (cons indentor *construct-indentation-types*))
      (setf *construct-indentation-types*
            (nconc *construct-indentation-types* (list indentor))))))

;;; Adds the indentor to the *construct-indentation-types*.

(defmethod initialize-instance ((indentor sk8-script-indentor) &rest initargs)
  (declare (ignore-if-unused initargs))
  (call-next-method)
  (add-indentation-type indentor))

;;; This function looks up the keyword in the *construct-indentation-types* list.
;;; If the line is the current line, no prev-eol is specified.

(defun trigger->indentor (trigger buf &optional prev-eol)
  (find-if #'(lambda (key)
               (if (stringp key)
                 (string-equal trigger key)
                 ;; It is a symbol! Funcall it.
                 (funcall key buf prev-eol)))
           *construct-indentation-types* 
           :key #'trigger))

(make-instance 'sk8-script-indentor
  :trigger "IF"
  :nextAction 'indent
  :nextCondition '(AND (last-token-is ("THEN" "OF"))))

(make-instance 'sk8-script-indentor
  :trigger "REPEAT"
  :nextAction 'indent)

(make-instance 'sk8-script-indentor
  :trigger "WHILE"
  :action 'outdent)

(make-instance 'sk8-script-indentor
  :trigger "WITH"
  :nextaction 'indent)

(make-instance 'sk8-script-indentor
  :trigger "UNLESS"
  :nextAction 'indent
  :nextCondition '(AND (last-token-is ("DO"))))

(make-instance 'sk8-script-indentor
  :trigger "ON"
  :nextAction 'indent)

;;; Else is complicated because it can be inside a if ... one of... statement.
;;; The else has to to back to the level of the enclosing if. Need to redefine
;;; significant-previous-line for that purpose.

(defclass else-statement-indentor (sk8-script-indentor)
  ())

(make-instance 'else-statement-indentor
  :trigger "ELSE"
  :action 'outdent
  :condition '(AND (FIRST-TOKEN-PREV-LINE-IS-NOT "THEN"))
  :nextAction 'indent
  :nextCondition '(AND (LAST-TOKEN-IS "else") (NUM-TOKENS-IS 1)))

(defmethod significant-previous-line ((indentor else-statement-indentor) theBuffer)
  (let ((enclosingIfStartPos (find-enclosing-if theBuffer (buffer-position theBuffer))))
    (if enclosingIfStartPos
      (values (first-word theBuffer enclosingIfStartPos)
              (countTabs theBuffer enclosingIfStartPos)
              enclosingIfStartPos)
      (values nil 0 nil))))

;;; A case in an if ... one of. Looks for the significant previous line and
;;; indents appropriately. The significant previous line is either the if line
;;; or the previos case line. This one needs to redefine significant-Previous-Line
;;; and thus we need to make a new class...

(defclass case-clause-indentor (sk8-script-indentor)
  ())

;;; Returns the previous line that is either another clause or an if statement.

(defmethod significant-previous-line ((indentor case-clause-indentor) theBuffer)
  (if (zerop (buffer-line-start theBuffer))
    (values nil 0 nil)
    (let ((curPos (find-enclosing-if theBuffer (buffer-position theBuffer))))
      ;; CurPos points at the significant line.
      (values (first-word theBuffer curpos)
              (if (string-equal (first-word theBuffer curpos) "IF")
                (countTabs theBuffer curPos)
                (1- (countTabs theBuffer curPos)))
              curPos))))

(defun case-clause-line? (theBuffer curPos)
  (and (line-contains-string? theBuffer 
                              (buffer-line-start theBuffer (or curPos (buffer-position theBuffer)))
                              ":")
       (enclosing-if-ends-with "OF" curPos theBuffer)))

;;; The indentor for if ... one of...
;;; All it needs to do is recognize the right significant line...

(make-instance 'case-clause-indentor
  :trigger 'case-clause-line?
  :nextAction 'indent)

;;; The end if in the middle of a clause in an if one of might require
;;; a double outdent to match the if. Need to make a new indentor for this case. 
;;; Indents end if lines matching the enclosing if.  Does nothing if the end line if not an end if line.

(defclass end-statement-indentor (sk8-script-indentor)
  ())

(make-instance 'end-statement-indentor
  :trigger "END"
  :action 'outdent)

;;; Returns the word that follows the END in the line. 

(defun end-what? (buf lineStart)
  (let* ((endPos (find-Forward buf "END" lineStart)) 
         (nextPos (jump-whitespace buf endPos))
         (endWhatPos (jump-darkspace buf nextPos)))
    (buffer-substring buf nextPos endWhatPos)))
  
;;; Returns whether prefix is the start of theString.

(defun string-prefix? (prefix theString)
  (let ((len1 (length prefix))
        (len2 (length theString)))
    (and (>= len2 len1)
         (string-equal prefix (subseq theString 0 len1)))))

(defmethod significant-previous-line ((indentor end-statement-indentor) theBuffer)
  (if (string-prefix? "IF" (end-what? theBuffer (buffer-line-start theBuffer)))
    (let ((enclosingIfStartPos (find-enclosing-if theBuffer (buffer-position theBuffer))))
      (if enclosingIfStartPos
        (values "IF"
                (countTabs theBuffer enclosingIfStartPos)
                enclosingIfStartPos)
        (values nil 0 nil)))
    (call-next-method)))

;;; _______________________________ 
;;; The engine.
;;; _______________________________ 

(defun conditions-satisfied? (conds buf this-line)
  (cond ((null conds) t)
        (t
         (let* ((logicalOperator (car conds))
                (conditions (cdr conds))
                (resultValue (eq logicalOperator 'and)))
           (dolist (c conditions resultValue)
             (setf resultValue 
                   (if (eq logicalOperator 'and)
                     (and (funcall (car c) (cadr c) this-line buf) resultValue)
                     (or (funcall (car c) (cadr c) this-line buf) resultValue)))
             ;; test for completion.
             (when (or (and (eq logicalOperator 'and) (null resultValue))
                       (and (eq logicalOperator 'or) resultValue))
               (return-from conditions-satisfied? resultValue)))))))

;;; 1. find the text of this line.
;;; 2. find corresponding indentor for it.
;;; 3. find significant line for it.
;;; 4. modify formatting of significant line by this line here actions.
;;; 5. make the indentation happen.

(defun indent-this-line (fredItem &optional justTheTabLevelPlease)
  (let* ((buf (fred-buffer fredItem))
         (firstWord (first-word buf))
         (indentor (trigger->indentor firstWord buf))
         (prevLineFirstWord nil)
         (tabLevel 0)
         (prevLineStartPos nil)
         prevLineIndentor)
    ;; 1. Find prev significant line, and adjust its tab level by the next action.
    (multiple-value-setq (prevLineFirstWord tabLevel prevLineStartPos)
      (significant-previous-line (or indentor *default-indentor*) buf))
    (when prevLineFirstWord
      (setf prevLineIndentor (trigger->indentor prevLineFirstWord buf prevLineStartPos))
      (when prevLineIndentor
        (setf tabLevel 
              (+ tabLevel (action->levelDelta prevLineIndentor buf prevLineStartPos 'nextAction)))))
    ;; 2. Now adjust the tab level by this indentor's here action.
    (when indentor
      (setf tabLevel
            (+ tabLevel (action->levelDelta indentor buf (buffer-position buf) 'action))))
    ;; 3. Adjust for this line being a continuation line.
    (when (continuation-line? buf (buffer-position buf))
      (incf tabLevel))
    ;; 4. Do the indentation.
    (if justTheTabLevelPlease
      tabLevel
      (set-indent buf tabLevel))))

(defun indent-this-block (w)
  (let ((buf (fred-buffer w)))
    (multiple-value-bind (start end) (selection-range w)
      (do ((line-point start (1+ (buffer-line-end buf))))
          ((or (> line-point end)
               (> line-point (buffer-size buf))))
        (set-mark buf line-point)
        (indent-this-line w)))))
      
;;; _______________________________ 
;;; Comtab functions.
;;; _______________________________ 

(defun anything-selected? (window)
  (multiple-value-bind (start end)
                       (selection-range window)
    (not (= start end))))

(defun indent-selection (w)
  (if (anything-selected? w)
    (indent-this-block w)    
    (indent-this-line w)))

(defun indent-next-line (w)
  ;; If too slow, only do this when the first word is an else or an end.
  ;; But doing it always is guaranteed to work for all cases. 
  (indent-this-line w)
  (ed-insert-char w #\Newline)
  (indent-this-line w))

(defun line-continuation (w)
  (ed-insert-char w "Â")
  (indent-this-line w)
  (indent-next-line w))

(defun indent-everything (w)
  (let ((buf (fred-buffer w))
        (line-start-pos 0))
    (do-buffer-lines (line-start-pos buf)
      (set-mark buf line-start-pos)
      (indent-this-line w))))

(defparameter *script-fred-comtab* (copy-comtab *comtab*))
(comtab-set-key *script-fred-comtab* '(:control #\CR)       'line-continuation)
(comtab-set-key *script-fred-comtab* '(:meta #\CR)          'line-continuation)
(comtab-set-key *script-fred-comtab* '(:control :meta #\CR) 'line-continuation)
(comtab-set-key *script-fred-comtab* #\Tab                  'indent-selection)
(comtab-set-key *script-fred-comtab* #\CR                   'indent-next-line)

#|

(let ((theEditor (make-instance *default-editor-class* :comtab *script-fred-comtab*)))
  (setf (fred-tabcount theEditor) 3))

|#



#|
	Change History (most recent last):
	1  	 1/ 9/96	Hernan  	New file. Indentation code for ScriptEditText.
	2  	 1/10/96	Hernan  	Adding a function to indent everything.
	3  	 1/10/96	Hernan  	Fixing indent-everything.
	4  	 1/10/96	Hernan  	indent-this-line can do the indentation or return the number
						of tabs required. Adding an optional argument for this
						purpose.
	1  	 1/22/96	Hernan  	
	2  	 1/25/96	Hernan  	Adding 1 line else statements. Also created an indentor for
						end XXX lines. If XXX = IF then we indent according to the
						immediate enclosing IF. This is required to deal with double
						indentation in the case when end if follows a clause in an
						if one of form.
	3  	 1/25/96	Hernan  	Count-tokens was not computing the end position from the
						start position given.
	4  	 1/29/96	Hernan  	Indend next line only does what its name says: indent the 
						next line.
	2  	 5/ 8/96	Hernan  	Indent-next-line has to indent the current line too (this is so
						that elses and end things get outdented). Also fixed a few
						bugs that came up when Brian wrote a large file of SK8Script code.
	3  	 7/29/96	Hernan  	Avoiding buffer position out of bounds error. Fixing do-buffer-lines to
						check before calling the body.
	4  	 9/ 3/96	Hernan  	Fixed add-indentation-type to correctly add the first
						indentor when its trigger is a symbol.
	5  	 9/20/96	Hernan  	Adding an indentor for with blocks.
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
