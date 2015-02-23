;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :skil)



;;;__________________________________________________________________________________________
;;;__________________________________________________________________________________________
;;;__________________________________________________________________________________________
;;;__________________________________________________________________________________________





(defparameter *SK8Script-Tokens* '(A AN THE 
                                      |'T| |'S|
                                      SET TO GET MOD DIV
                                      REMOVE FROM INSERT AT IN POSITION
                                      WITH WITHOUT GIVEN USING
                                      WAIT FOR WHILE UNTIL EVENTS
                                      IF ONE OF THEN ELSE END 
                                      UNLESS DO ON
                                      REPEAT FOREVER BY EXIT TIMES NEXT
                                      CLEANUP BODY INHERITED ME MY
                                      GLOBAL LOCAL CONSTANT
                                      RETURN IT ITS RETURNING
                                      DEFAULTING ERROR
                                      IS ISNT SAME DOES DOESNT ISN DOESN
                                      AS GREATER LESS THAN EQUAL EQUALS  
                                      BACK FRONT REAR BEGINNING START INTO
                                      BEFORE AFTER BEHIND
                                      STARTS ENDS MEMBER
                                      CONTAINS CONTAINED CONTAIN
                                      FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGTH NINTH TENTH ELEVENTH TWELTH
                                      TH ALL THING AND OR XOR EXCLUSIVE NOT
                                      NAMED WHOSE WHERE THAT
                                      ANY ANYTHING EVERY EVERYTHING MIDDLE LAST 
                                      THRU THROUGH ITEM
                                      PASS APPLY ONTO CALL RESULT
                                      ))

(defparameter *tokens-that-are-reserved-only-in-multi-token-context*
  '(CLEANUP BODY INHERITED
    DEFAULTING
    GREATER LESS THAN  
    REAR BEGINNING START
    STARTS ENDS MEMBER
    EXCLUSIVE ERROR
    ))

;;to check for duplicates in above:  (let ((cnt 1))  (dolist (i skil::*SK8Script-Tokens*) (if (memq i (nthcdr cnt skil::*SK8Script-Tokens*)) (print i))  (incf cnt)))

;;WE LOVE THE BOGUS TOKEN!!!
(defparameter THE-BOGUS-TOKEN (make-sk8-token 'BOGUS -1 -1))

;;;__________________________________________________________________________________________

(defun tokenizer-do-get-char (str loc)
  (if (>= loc (length str))
    'END-OF-STREAM
    (aref str loc)))

(defun end-of-stream-object? (char)
  (eq char 'END-OF-STREAM))

(defun string->project-symbol (str) 
  (maybe-save-original-sym-case str 
                                (or (and (sk8::is-a *current-project* sk8::project)
                                         (package *current-project*))
                                    :sk8)
                                ))

(defun string->token-symbol (str)
  (declare (special token-list))
  (let ((sym (read-from-string str nil nil)))
    (if (and (memq sym *sk8script-tokens*) 
             (neq (first token-list) 'WITH)
             (neq (first token-list) 'WITHOUT))
      sym
      (string->project-symbol str))))

;;(string->token-symbol "BACK")

;;____________________________________________________________________________________________________
(defun do-tokenizer-do-bar-identifier (Str startCount curCount curPackage)
  (let ((inchar (tokenizer-do-get-char str curCount)))
    (if (AND (not (end-of-stream-object? inChar))
             (not (char= inChar #\|)))
      (do-tokenizer-do-bar-identifier Str startCount (1+ curCount) curPackage)
      (make-sk8-token
       (if curPackage
         (maybe-save-original-sym-case (subSeq Str startCount curCount) curPackage) 
         (string->token-symbol (subSeq Str startCount curCount)))
       startCount curCount
       ))))

(defun tokenizer-do-bar-identifier (Str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str curCount)))
    (if (char= inChar #\()
      (let ((endloc (position #\) str :start curcount :test #'char=))
            pack)
        (setf pack (ccl::keywordify (string->project-symbol (subseq str (+ 2 startcount) endloc))))
        (do-tokenizer-do-bar-identifier Str (1+ endloc) (1+ endloc) pack))
      (do-tokenizer-do-bar-identifier Str curcount curCount nil))))

;(tokenizer-do-bar-identifier "|sendtolog| " 0 1)

;;____________________________________________________________________________________________________
(defun tokenizer-do-identifier (Str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str curCount)))
    (if (AND (not (end-of-stream-object? inChar))
             (OR (char<= #\a inChar #\z)
                 (char<= #\A inChar #\Z)
                 (char<= #\0 inChar #\9)
                 (char= inChar #\_)
                 (char= inchar #\!)))
      (tokenizer-do-identifier Str startCount (1+ curCount))
      (make-sk8-token
       (string->token-symbol (subSeq Str startCount curCount))
       startCount (1- curCount)
       ))))
;(tokenizer-do-identifier "sym + 1" 0 1)

;;____________________________________________________________________________________________________
(defun tokenizer-do-symbol-or-special (str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str (1+ curCount)))
        (curChar (tokenizer-do-get-char str curCount))
        (special-chars '(#\s #\S #\t #\T )))
    (if (end-of-stream-object? curChar)
      (parse-error "End of Input seen before end of Symbol" (make-sk8-token 'ERROR startCount startCount)))
    (if (and (or (end-of-stream-object? inChar) (char= inChar #\ ) )
             (memq curChar special-chars))
      (make-sk8-token
       (cond 
        ((or (char= curChar #\s) (char= curChar #\S)) '|'S|)
        ((or (char= curChar #\t) (char= curChar #\T)) '|'T|))
       startCount (1+ curCount))
      (tokenizer-do-symbol str startCount (1+ startCount)))))

;(tokenizer-do-symbol-or-special "'s ym' + 1" 0 1)

;;____________________________________________________________________________________________________
;;Almost the same as do--string, should consolidate the two.

(defun tokenizer-remove-slashquote (str thechar)
  (let ((i 0)
        (limit (1- (length str))))
    (ccl::while (< i limit)
      (when (and (char= (aref str i) #\\)
                 (or (char= (aref str (1+ i)) thechar)
                     (char= (aref str (1+ i)) #\\)))
        (setf str (concatenate 'string (subseq str 0 i) (subseq str (1+ i))))
        (decf limit)
        )
      (incf i))
    str))

(defun tokenizer-do-symbol (str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str curCount))
        (nextchar (tokenizer-do-get-char str (1+ curCount))))
    (cond
     ((end-of-stream-object? inChar)
      (parse-error "End of Input seen before end of Symbol" (make-sk8-token 'ERROR startCount startCount)))
     ((and
       (char= inChar #\\)
       (not (end-of-stream-object? nextchar))
       (or (char= nextchar #\') 
           (char= nextchar #\\)))
      (tokenizer-do-symbol str startCount (+ 2 curCount)))
     ((char= inChar #\')
      (if (and (char= (tokenizer-do-get-char str (1+ startCount)) #\:)
               (not (char= (tokenizer-do-get-char str (+ 2 startCount)) #\')))
        (make-sk8-token
         (intern-symbol (tokenizer-remove-slashquote (subSeq Str (+ 2 startCount) curCount) #\') :keyword)
         startCount curCount
         )
        (make-sk8-token
         `(quote ,(string->project-symbol (tokenizer-remove-slashquote (subSeq Str (1+ startCount) curCount) #\')))
         startCount curCount
         )))
     (t
      (tokenizer-do-symbol str startCount (1+ curCount))))))

;(tokenizer-do-symbol-or-special "'sym' + 1" 0 1)

;;____________________________________________________________________________________________________


(defun tokenizer-do-string (str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str curCount))
        (nextchar (tokenizer-do-get-char str (1+ curCount))))
    (cond
     ((end-of-stream-object? inChar)
      (parse-error "Unmatched quote in string" (make-sk8-token 'ERROR startCount startCount)))
     ((and
       (char= inChar #\\)
       (not (end-of-stream-object? nextchar))
       (or (char= nextchar #\")
           (char= nextchar #\\)) )
      (tokenizer-do-string str startCount (+ curCount 2)))
     ((char= inChar #\")
      (make-sk8-token
       (tokenizer-remove-slashquote (subSeq Str (1+ startCount) curCount) #\")
       
       startCount curCount
       ))
     (t
      (tokenizer-do-string str startCount (1+ curCount))))))

;(tokenizer-do-string "\"sym\\\\"  0 1)

;;____________________________________________________________________________________________________


(defparameter numeric-chars 
  (map 'ccl::list #'(lambda (c) c) "xX0123456789abcdefABCDEF.+-"))
(defun tokenizer-create-number (number-spelling)  ;; SK8 number-string to MCL number or #False
  (let ( (start (if (member (ccl::aref number-spelling 0) '(+ -)) 1 0)) )
    (when (and (>= (length number-spelling) (+ start 2))
               (char= (ccl::aref number-spelling start) #\0)
               (member (ccl::aref number-spelling (+ start 1)) '( #\x #\X )))
      (ccl::aset number-spelling start #\# )
      )
    
    ;; 0x... -> #x...
    (multiple-value-bind (result amt-read)
                         (ccl::read-from-string number-spelling nil nil)
      (if (and result (>= amt-read (length number-spelling)))
        result  ;; number must consume whole string
        nil)
      ) )
  )

(defun tokenizer-do-number (str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str curCount))
        )
    (if (and (not (end-of-stream-object? inChar)) 
             (memq inChar numeric-chars)
             (or (and (not (char= inChar #\+)) (not (char= inChar #\-)))  ;;+ and - can be in a number only in exponentiation
                 (char= (tokenizer-do-get-char str (1- curCount)) #\e)
                 (char= (tokenizer-do-get-char str (1- curCount)) #\E)))
      (tokenizer-do-number str startCount (1+ curCount))
      (make-sk8-token
       (tokenizer-create-number (subSeq Str startCount curCount))
       startCount (1- curCount)
       ))))

;;____________________________________________________________________________________________________

(defparameter tokenizer-eol-chars (list (character 12) (character 10) (character 13)))
(defparameter tokenizer-continuation-char (character 194))

(defun tokenizer-do-comment-or-minus (str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str curCount)))
    (if (char= inChar #\-)
      (let (newtok)
        (ccl::while (not (or  (end-of-stream-object? inChar) 
                              (memq inChar tokenizer-eol-chars)
                              (char= inChar tokenizer-continuation-char) ))
          (incf curCount)
          (setf inchar (tokenizer-do-get-char str curCount)))
        (setf newtok (make-sk8-token 'SK8-COMMENT startCount (1- curCount)))
        (if generate-readable-code (setf (sk8-token-correspondingtoken newtok) (subSeq Str (+ 2 startCount) curCount)))
        newtok)
      (make-sk8-token
       '-
       startCount (1- curCount))
      )))

;;____________________________________________________________________________________________________

(defun tokenizer-do-block-comment (str startCount curCount)
  (let ((inchar (tokenizer-do-get-char str curCount))
        (depth 1)
        newtok)
    (ccl::while (> depth 0)
      (loop 
        (if (or (end-of-stream-object? inChar)
                (end-of-stream-object? (tokenizer-do-get-char str (1+ curCount))))
          (parse-error "Unmatched block comment starting token (*" (make-sk8-token 'ERROR startCount (1+ startCount))))
        (if (and (char= inChar #\*) (char= (tokenizer-do-get-char str (1+ curCount)) #\))) (return))
        (if (and (char= inChar #\() (char= (tokenizer-do-get-char str (1+ curCount)) #\*)) (incf depth))
        (incf curCount)
        (setf inchar (tokenizer-do-get-char str curCount))
        )
      (incf curcount)
      (decf depth))
    (setf newtok (make-sk8-token 'SK8-COMMENT startCount curCount))
    (if generate-readable-code (setf (sk8-token-correspondingtoken newtok) (subSeq Str (+ 2 startCount) (1- curCount))))
    newtok
    ))

;;____________________________________________________________________________________________________

(defun tokenizer-do-operator (str startCount curCount)
  (let ((lastChar (tokenizer-do-get-char str curCount))
        (inchar (tokenizer-do-get-char str (1+ curCount))))
    (if (OR (AND (char= lastChar #\<) (memq inChar '(#\= #\>)))
            (AND (char= lastChar #\=) (memq inChar '(#\< #\>)))
            (AND (char= lastChar #\>) (memq inChar '(#\= #\<))) )
      (incf curcount))
    (make-sk8-token
     (intern-symbol (subSeq Str startCount (1+ curCount)) :skil)
     startCount curCount)))

;;____________________________________________________________________________________________________

(defparameter tokenizer-punctuation-chars '( #\( #\) #\, #\{ #\} #\: #\;))
(defparameter tokenizer-operator-chars (list #\* #\/ #\& #\^ (character 173) (character 178) (character 179) 
                                                 (character 197) (character 214) #\= #\+ #\- #\< #\>))

(defun tokenizer-get-token (str curCount)
  (let ((inchar (tokenizer-do-get-char str curCount)))
    (cond
     ((end-of-stream-object? inChar)
      (make-sk8-token inChar curCount curCount))
     ((char= inChar #\|)
      (tokenizer-do-bar-identifier str curCount (1+ curCount)))
     ((OR (char<= #\a inChar #\z) (char<= #\A inChar #\Z))
      (tokenizer-do-identifier str curCount (1+ curCount)))
     ((and (char= inChar #\() 
           (not (end-of-stream-object? (tokenizer-do-get-char str (1+ curCount))))
           (char= (tokenizer-do-get-char str (1+ curCount)) #\*))
      (tokenizer-do-block-comment str curCount (+ 2 curCount)))
     ((and (char= inChar #\*) 
           (not (end-of-stream-object? (tokenizer-do-get-char str (1+ curCount))))
           (char= (tokenizer-do-get-char str (1+ curCount)) #\)))
      (parse-error "Unmatched block comment ending *)" (make-sk8-token 'ERROR curCount (1+ curCount))))
     ((memq inChar tokenizer-punctuation-chars)
      (make-sk8-token
       (intern-symbol (subSeq Str curCount (1+ curCount)) :skil)
       curCount curCount))
     ((char= inChar #\")
      (tokenizer-do-string str curCount (1+ curCount)))
     ((char= inChar #\')
      (tokenizer-do-symbol-or-special str curCount (1+ curCount)))
     ((char<= #\0 inChar #\9)
      (tokenizer-do-number str curCount (1+ curCount)))
     ((char= inChar #\-)
      (tokenizer-do-comment-or-minus str curCount (1+ curCount)))
     ((memq inChar tokenizer-operator-chars)
      (tokenizer-do-operator str curCount curCount))
     ((memq inChar tokenizer-eol-chars)
      (prog1 
        (make-sk8-token
         'EOL
         curCount curCount)
        (incf tokenizer-current-line)
        (setf tokenizer-current-line-location (1+ curCount))
        ))
     ((char= inChar tokenizer-continuation-char)  ;; Line Continuance?  => skip eol
      (ccl::while (not (memq (aref str curcount) tokenizer-eol-chars))
        (incf curCount))
      (incf curcount)
      (incf tokenizer-current-line)
      (setf tokenizer-current-line-location curCount)
      (let (next-token 
            done)
        (ccl::while (not done)
          (setf next-token (tokenizer-get-token str curcount))
          (if (eq (sk8-token-val next-token) 'SK8-COMMENT)
            (setf curcount (+ 2 (sk8-token-end next-token)))
            (setf done t)))
        next-token)
      )
     (t   ;;whitespace
      (tokenizer-get-token str (1+ curCount))))
    ))


;;____________________________________________________________________________________________________


(defun string->token-list (str)
  (let (token-list commenttoklist curtok)
    (declare (special token-list))
    (setf tokenizer-current-line 0)
    (setf tokenizer-current-line-location 0)
    (setf curTok (tokenizer-get-token str 0))
    (if (eq (sk8-token-val curtok) 'SK8-COMMENT)
      (if generate-readable-code (push curTok commenttoklist))
      (push curTok token-list))
    (ccl::while (neq (sk8-token-val curtok) 'END-OF-STREAM)
      (setf curTok (tokenizer-get-token str (1+ (sk8-token-end curtok))))
      (if (eq (sk8-token-val curtok) 'SK8-COMMENT)
        (if generate-readable-code (push curTok commenttoklist))
        (push curTok token-list))
      )
    (list (nreverse token-list) (nreverse commenttoklist))))


#|
(string->token-list "a , b")

(mapcar #'sk8-token-val (time (string->token-list "on juju of x
        --sendtolog every odd number up to the specfied arg
	repeat with i from 1 to x by 2
		sendtolog of i
	end repeat
        sendtolog of \"Does that make you happy!\"
end juju")))
(tokenizer-get-token "'hahaha' + 1" 0)


|#


;;____________________________________________________________________________________________________

(defparameter multi-token-dictionary-table (make-hash-table :test #'eq))

(defun add-multi-token (symList symsToUse)
  (let (curList)
    (setf curlist (gethash (car symlist) multi-token-dictionary-table))
    (unless (listp symsToUse) (setf symsToUse (list symsToUse)))
    (push (list symList symsToUse) curList)
    (setf curlist (sort curList #'(lambda (x y) (> (length (car x)) (length (car y))))))
    (setf (gethash (car symlist) multi-token-dictionary-table) 
          curlist)
    t))

#|
(string->multi-token-list "{1,2,3,4} as an array")
(add-multi-token '(end repeat eol) 'end-repeat-eol)
(add-multi-token '(end if eol) 'end-if-eol)
(add-multi-token '(an) 'a)

(gethash 'end multi-token-dictionary-table)
|#

(defun tokens->multi-tokens (toklist)
  (let ((cnt 0)
        hashEntry dict-ent len
        tokstart tokend lenTokList
        curproject cursym
        curitem)
    (setf toklist (cons THE-BOGUS-TOKEN toklist))
    (setf lenTokList (length toklist))
    (ccl::while (setf curItem (nth cnt toklist))
      ;;(print (sk8-token-val curItem))
      (setf hashEntry (gethash (sk8-token-val curitem) multi-token-dictionary-table))
      (when hashentry
        (dolist (i hashentry)
          (setf dict-ent (first i))
          (setf len (length dict-ent))
          (when (and (<= (+ cnt len) lenTokList)
                     (equal dict-ent (mapcar #'(lambda (x) (sk8-token-val x)) (subseq toklist cnt (+ cnt len)))))
            (setf tokstart (sk8-token-start curitem))
            (setf tokend (sk8-token-end (nth (+ cnt len -1) toklist)))
            (setf tokenizer-current-line (sk8-token-line-number curitem))
            (setf tokenizer-current-line-location (- (sk8-token-start curitem) (sk8-token-start-line-pos curitem)))
            (setf (nthcdr cnt toklist)
                  (nconc 
                   (mapcar #'(lambda (x) (make-sk8-token x tokstart tokend)) (second i))
                   (nthcdr (+ cnt len) toklist)))
            (incf lenTokList (- (length (second i)) len))
            (when (eq (car (second i)) 'SYMBOL-IN-PROJECT)
              (setf curProject (nth (1+ cnt) toklist))
              (unless (and curproject
                           (setf curproject (sk8-token-val curproject))
                           (or (and (boundp curproject)
                                    (sk8::is-a (symbol-value curproject) sk8::project))
                               (and (find-package (ccl::keywordify curproject))  ;;ensures a valid lisp package..
                                    (setf curproject (intern-symbol (symbol-name curproject) :keyword))
                                    ))
                           (nth (+ cnt 2) toklist)
                           (eq (sk8-token-val (nth (+ cnt 2) toklist)) '|)|))
                (parse-error "Expected a project name followed by a ) after IN PROJECT."
                             (nth (+ cnt 1) toklist)))
              (setf cursym (sk8-token-val (nth (1- cnt) toklist)))
              (cond 
               ((symbolp cursym)
                (set-sk8-token-val (nth (1- cnt) toklist) (maybe-save-original-sym-case (sk8::name cursym)
                                                                  (ccl::keywordify curproject))))
               ((and (listp cursym) (eq (car cursym) 'QUOTE) (symbolp (second cursym)))
                (set-sk8-token-val (nth (1- cnt) toklist) (list 'QUOTE
                                                                (maybe-save-original-sym-case (sk8::name (second cursym))
                                                                        (ccl::keywordify curproject))))             
                )
               (t
                (parse-error "Can only make a symbol be in a project" (nth (1- cnt) toklist))))
              (setf (nthcdr cnt toklist) (nthcdr (+ cnt 3) toklist) )
              (decf cnt))
            )))
      (when (and (eq (sk8-token-val curitem) 'APPLY) 
                 (setf curItem (nth (+ 2 cnt) toklist))
                 (eq (sk8-token-val curitem) 'TO))
        (set-sk8-token-val curItem 'ONTO))
      (when (and (eq (sk8-token-val curitem) 'PASS) 
                 (setf curItem (nth (+ 1 cnt) toklist))
                 (eq (sk8-token-val curitem) 'TO))
        (set-sk8-token-val curItem 'ONTO))
      (incf cnt)
      )
    (cdr toklist)
    ))

(defun string->multi-token-list (str)
  (let* ((*package* (find-package :skil))  ;;;So we intern all the reserved words into SKIL by default
         (tokens (string->token-list str))
         (multi-tokens (tokens->multi-tokens (first tokens))))
    (dolist (i multi-tokens)
      (when (memq (sk8-token-val i) *tokens-that-are-reserved-only-in-multi-token-context*)
        (set-sk8-token-val i (string->project-symbol (symbol-name (sk8-token-val i))))))
    (list multi-tokens (second tokens))
    ))

#|
(time (progn
        (string->multi-token-list " start of me")
      t))
(time (string->token-list "a, b"))


(string->multi-token-list "get 1 is same sk8::object as 1
")

(time (progn
        (string->multi-token-list "position of item 1 that = 2 in the first item in first thru second list where first number in it = 2 and second integer in it = 3 in {{oval, 2,3},{4,5},{2,3,4}}")
      t))

(time (string->multi-token-list "every item named bing in foo"))
(time (string->multi-token-list "on superfun of x
	
	return item 1 thru 3 of x
	
end superfun  "))
(mapcar #'sk8-token-val *)
|#

;;______________________________________________________________

(add-multi-token '(an) 'A)

(add-multi-token '(|(| in sk8::project) 'SYMBOL-IN-PROJECT)


(add-multi-token '(is same sk8::object as) 'eq)
(add-multi-token '(is same as) 'eq)
(add-multi-token '(is the same sk8::object as) 'eq)
(add-multi-token '(is the same as) 'eq)
(add-multi-token '(|'S| same sk8::object as) 'eq)
(add-multi-token '(|'S| same as) 'eq)
(add-multi-token '(|'S| the same sk8::object as) 'eq)
(add-multi-token '(|'S| the same as) 'eq)
(add-multi-token '(is not same sk8::object as) 'neq)
(add-multi-token '(is not same as) 'neq)
(add-multi-token '(is not the same sk8::object as) 'neq)
(add-multi-token '(is not the same as) 'neq)
(add-multi-token '(isnt same sk8::object as) 'neq)
(add-multi-token '(isnt same as) 'neq)
(add-multi-token '(isnt the same sk8::object as) 'neq)
(add-multi-token '(isnt the same as) 'neq)
(add-multi-token '(isn |'T| same sk8::object as) 'neq)
(add-multi-token '(isn |'T| same as) 'neq)
(add-multi-token '(isn |'T| the same sk8::object as) 'neq)
(add-multi-token '(isn |'T| the same as) 'neq)
(add-multi-token '(|'S| not same sk8::object as) 'neq)
(add-multi-token '(|'S| not same as) 'neq)
(add-multi-token '(|'S| not the same sk8::object as) 'neq)
(add-multi-token '(|'S| not the same as) 'neq)

(add-multi-token '(as a) 'as-a)
(add-multi-token '(as an) 'as-a)
(add-multi-token '(as) 'as-a)

(add-multi-token '(equals) '=)
(add-multi-token '(is =) '=)
(add-multi-token '(is equal to) '=)
(add-multi-token '(|'S| =) '=)
(add-multi-token '(|'S| equal to) '=)

(add-multi-token '(is not equal to) '­)
(add-multi-token '(|'S| not equal to) '­)
(add-multi-token '(isnt equal to) '­)
(add-multi-token '(isn |'T| equal to) '­)
(add-multi-token '(does not equal) '­)
(add-multi-token '(does not =) '­)
(add-multi-token '(doesnt equal) '­)
(add-multi-token '(doesnt =) '­)
(add-multi-token '(doesn |'T| equal) '­)
(add-multi-token '(<>) '­)
(add-multi-token '(><) '­)

(add-multi-token '(=<) '<=)
(add-multi-token '(=>) '>=)
(add-multi-token '(²) '<=)
(add-multi-token '(³) '>=)
(add-multi-token '(greater than) '>)

(add-multi-token '(is >) '>)
(add-multi-token '(is <) '<)
(add-multi-token '(is >=) '>=)
(add-multi-token '(is <=) '<=)

(add-multi-token '(is greater than) '>)
(add-multi-token '(|'S| greater than) '>)
(add-multi-token '(greater than or equal to) '>=)
(add-multi-token '(greater than or equal) '>=)
(add-multi-token '(is greater than or equal to) '>=)
(add-multi-token '(is greater than or equal) '>=)
(add-multi-token '(|'S| greater than or equal to) '>=)
(add-multi-token '(|'S| greater than or equal) '>=)
(add-multi-token '(less than) '<)
(add-multi-token '(is less than) '<)
(add-multi-token '(|'S| less than) '<)
(add-multi-token '(less than or equal to) '<=)
(add-multi-token '(less than or equal) '<=)
(add-multi-token '(is less than or equal to) '<=)
(add-multi-token '(is less than or equal) '<=)
(add-multi-token '(|'S| less than or equal to) '<=)
(add-multi-token '(|'S| less than or equal) '<=)

(add-multi-token '(not greater than) '<=)
(add-multi-token '(is not greater than) '<=)
(add-multi-token '(|'S| not greater than) '<=)
(add-multi-token '(isnt greater than) '<=)
(add-multi-token '(isn |'T| greater than) '<=)
(add-multi-token '(not greater than or equal to) '<)
(add-multi-token '(not greater than or equal) '<)
(add-multi-token '(is not greater than or equal to) '<)
(add-multi-token '(|'S| not greater than or equal to) '<)
(add-multi-token '(isnt greater than or equal to) '<)
(add-multi-token '(isn |'T| greater than or equal to) '<)
(add-multi-token '(is not greater than or equal) '<)
(add-multi-token '(|'S| not greater than or equal) '<)
(add-multi-token '(isnt greater than or equal) '<)
(add-multi-token '(isn |'T| greater than or equal) '<)
(add-multi-token '(not less than) '>=)
(add-multi-token '(is not less than) '>=)
(add-multi-token '(|'S| not less than) '>=)
(add-multi-token '(isnt less than) '>=)
(add-multi-token '(isn |'T| less than) '>=)
(add-multi-token '(not less than or equal to) '>)
(add-multi-token '(not less than or equal) '>)
(add-multi-token '(is not less than or equal to) '>)
(add-multi-token '(|'S| not less than or equal to) '>)
(add-multi-token '(isnt less than or equal to) '>)
(add-multi-token '(isn |'T| less than or equal to) '>)
(add-multi-token '(is not less than or equal) '>)
(add-multi-token '(|'S| not less than or equal) '>)
(add-multi-token '(isnt less than or equal) '>)
(add-multi-token '(isn |'T| less than or equal) '>)


(add-multi-token '(one of) 'ONE-OF)
(add-multi-token '(if one of eol) 'IF-ONE-OF-EOL)
(add-multi-token '(then eol) 'THEN-EOL)
(add-multi-token '(else eol) 'ELSE-EOL)
(add-multi-token '(else if) 'ELSE-IF)
(add-multi-token '(end if eol) 'END-IF-EOL)

(add-multi-token '(do eol) 'DO-EOL)
(add-multi-token '(end unless eol) 'END-UNLESS-EOL)

(add-multi-token '(given) 'WITH)
(add-multi-token '(using) 'WITH)
(add-multi-token '(end with eol) 'END-WITH-EOL)

(add-multi-token '(repeat eol) 'REPEAT-EOL)
(add-multi-token '(end repeat eol) 'END-REPEAT-EOL)
(add-multi-token '(end unless eol) 'END-UNLESS-EOL)
(add-multi-token '(next repeat eol) 'NEXT-REPEAT-EOL)
(add-multi-token '(exit repeat eol) 'EXIT-REPEAT-EOL)

(add-multi-token '(defaulting to) 'DEFAULTING-TO)
(add-multi-token '(do inherited) 'DO-INHERITED)

(add-multi-token '(cleanup |:| eol) 'CLEANUP-COLON-EOL)
(add-multi-token '(do body eol) 'DO-BODY-EOL)

(add-multi-token '(on error) 'ON-ERROR)
(add-multi-token '(end error eol) 'END-ERROR-EOL)

(add-multi-token '(at the beginning of) 'FRONT)
(add-multi-token '(at beginning of) 'FRONT)
(add-multi-token '(at the start of) 'FRONT)
(add-multi-token '(at start of) 'FRONT)
(add-multi-token '(at the front of) 'FRONT)
(add-multi-token '(at front of) 'FRONT)
(add-multi-token '(at the beginning in) 'FRONT)
(add-multi-token '(at beginning in) 'FRONT)
(add-multi-token '(at the start in) 'FRONT)
(add-multi-token '(at start in) 'FRONT)
(add-multi-token '(at the front in) 'FRONT)
(add-multi-token '(at front in) 'FRONT)

(add-multi-token '(into) 'BACK)
(add-multi-token '(at the end of) 'BACK)
(add-multi-token '(at end of) 'BACK)
(add-multi-token '(at the back of) 'BACK)
(add-multi-token '(at back of) 'BACK)
(add-multi-token '(at the rear of) 'BACK)
(add-multi-token '(at rear of) 'BACK)
(add-multi-token '(at the end in) 'BACK)
(add-multi-token '(at end in) 'BACK)
(add-multi-token '(at the back in) 'BACK)
(add-multi-token '(at back in) 'BACK)
(add-multi-token '(at the rear in) 'BACK)
(add-multi-token '(at rear in) 'BACK)

(add-multi-token '(in the front of) 'BEFORE)
(add-multi-token '(in front of) 'BEFORE)
(add-multi-token '(in the front in) 'BEFORE)
(add-multi-token '(in front in) 'BEFORE)
(add-multi-token '(in the back of) 'AFTER)
(add-multi-token '(in back of) 'AFTER)
(add-multi-token '(in the back in) 'AFTER)
(add-multi-token '(in back in) 'AFTER)

(add-multi-token '(THRU) 'THROUGH)
(add-multi-token '(THING) 'ITEM)
(add-multi-token '(SOME) 'ANY)


;;;expressions
(add-multi-token '(is a) 'IS-A)
(add-multi-token '(is an) 'IS-A)
(add-multi-token '(|'S| a) 'IS-A)
(add-multi-token '(|'S| an) 'IS-A)

(add-multi-token '(is not a) 'IS-NOT-A)
(add-multi-token '(is not an) 'IS-NOT-A)
(add-multi-token '(|'S| not a) 'IS-NOT-A)
(add-multi-token '(|'S| not an) 'IS-NOT-A)
(add-multi-token '(isn |'T| a) 'IS-NOT-A)
(add-multi-token '(isn |'T| an) 'IS-NOT-A)
(add-multi-token '(isnt a) 'IS-NOT-A)
(add-multi-token '(isnt an) 'IS-NOT-A)

(add-multi-token '(isnt) 'IS-NOT)
(add-multi-token '(isn |'T|) 'IS-NOT)
(add-multi-token '(is not) 'IS-NOT)
(add-multi-token '(|'S| not) 'IS-NOT)

(add-multi-token '(starts with) 'starts-with)
(add-multi-token '(begins with) 'starts-with)
(add-multi-token '(ends with) 'ends-with)

(add-multi-token '(exclusive or) 'xor)


(add-multi-token '(is contained by) 'is-contained)
(add-multi-token '(is in) 'is-contained)
(add-multi-token '(is a member of) 'is-contained)
(add-multi-token '(is a member in) 'is-contained)
(add-multi-token '(is member of) 'is-contained)
(add-multi-token '(is member in) 'is-contained)
(add-multi-token '(|'S| contained by) 'is-contained)
(add-multi-token '(|'S| in) 'is-contained)
(add-multi-token '(|'S| a member of) 'is-contained)
(add-multi-token '(|'S| a member in) 'is-contained)
(add-multi-token '(|'S| member of) 'is-contained)
(add-multi-token '(|'S| member in) 'is-contained)

(add-multi-token '(is not contained by) 'is-not-contained)
(add-multi-token '(is not in) 'is-not-contained)
(add-multi-token '(is not a member of) 'is-not-contained)
(add-multi-token '(is not a member in) 'is-not-contained)
(add-multi-token '(is not member of) 'is-not-contained)
(add-multi-token '(is not member in) 'is-not-contained)
(add-multi-token '(|'S| not contained by) 'is-not-contained)
(add-multi-token '(|'S| not in) 'is-not-contained)
(add-multi-token '(|'S| not a member of) 'is-not-contained)
(add-multi-token '(|'S| not a member in) 'is-not-contained)
(add-multi-token '(|'S| not member of) 'is-not-contained)
(add-multi-token '(|'S| not member in) 'is-not-contained)

(add-multi-token '(does not contain) 'doesnt-contain)
(add-multi-token '(doesnt contain) 'doesnt-contain)
(add-multi-token '(doesn |'T| contain) 'doesnt-contain)



(add-multi-token '(me) '(sk8::me))
(add-multi-token '(my) '(sk8::me |'S|))
(add-multi-token '(that) '(WHERE IT))
(add-multi-token '(whose) '(WHERE IT |'S|))
(add-multi-token '(named) '(WHERE sk8::OBJECTNAME OF IT =))

(add-multi-token '(all of) '(FIRST THROUGH LAST ITEM IN))
(add-multi-token '(all) '(FIRST THROUGH LAST ITEM))
(add-multi-token '(everything) '(FIRST THROUGH LAST ITEM))
(add-multi-token '(every) '(FIRST THROUGH LAST))
(add-multi-token '(anything) '(ANY ITEM))
(add-multi-token '(its) '(IT |'S|))

(add-multi-token '(result) 'sk8::SK8ScriptResult)


;;(add-multi-token '(is) '|'S|)


;----------------------------------------------------------------------------------------------------------

(defparameter *sk8script-commandtokens* 
  '(IF THEN ELSE IS-ONE-OF-EOL IF-ONE-OF-EOL THEN-EOL ELSE-EOL ELSE-IF END-IF-EOL END
    UNLESS DO DO-EOL END-UNLESS-EOL END-ERROR-EOL ON-ERROR
    END-WITH-EOL REPEAT REPEAT-EOL END-REPEAT-EOL NEXT-REPEAT-EOL EXIT-REPEAT-EOL
    DO-INHERITED CLEANUP-COLON-EOL DO-BODY-EOL 
    GET SET TO WAIT GLOBAL LOCAL ON
    BY TIMES WHILE UNTIL
    ))


#|
	Change History (most recent last):
	1  	 3/12/96	Brian   	The New Parser!!!
	2  	 3/12/96	Brian   	making into -> back.
	3  	 3/14/96	Brian   	Lots of new multitokens added.
	4  	 3/14/96	Brian   	Fixing is one of.
	5  	 3/14/96	Brian   	adding block comment "(* comment *)".
	6  	 3/15/96	Brian   	adding 's and same object stuff.
	8  	 3/18/96	Brian   	adding all of and all in.
	10 	 3/19/96	Brian   	Added As A.  Handle (in project foo).  Handle |(mf)sym|.
	11 	 3/21/96	Brian   	Speeding up in project parsing.
	2  	 4/15/96	Brian   	Making everything happen in the SKIL package.
	3  	 4/17/96	Brian   	Fixed up sk8script-tokens list.
	6  	 4/26/96	Brian   	making object be non reserved.
	7  	 4/30/96	Brian   	mod, div, doesnt-contain, is-not-in.
	10 	 6/ 3/96	Brian   	adding forever to word list.
	11 	 7/15/96	Brian   	Removing i am
	12 	 7/16/96	Brian   	adding a few more isnts.
	14 	 7/19/96	Brian   	changed (one of eol) one-of -> (one of) one-of.
	16 	 7/22/96	Brian   	fixing slash doublequotes in strings.
	20 	 7/29/96	Brian   	Fixing in project to take any package name.
	21 	 8/ 5/96	Brian   	adding returning.
	23 	 8/28/96	Brian   	adding comments
	25 	 9/20/96	Brian   	fixing up reserved word stuff.
	26 	10/10/96	Brian   	Fixing on error syntax.
	27 	10/18/96	Brian   	fixing spelling mistake and interning of symbols
	28 	11/ 7/96	Brian Roddy	Adding equal and equals to reserved word list.
	29 	12/13/96	Brian   	
	30 	 1/30/97	Brian Roddy	
	32 	 2/11/97	Brian Roddy	Oh, it's always more complicated.  Need to 
						check for without
	33 	 2/11/97	Brian Roddy	
	34 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
