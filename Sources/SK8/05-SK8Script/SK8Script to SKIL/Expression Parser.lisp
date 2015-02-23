;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :skil)

#|
(SK8-declare-syms :SK8 :public ; Updated  3-12-96   5:08 pm
                  SK8::insertAtEnd SK8::insertInFront SK8::InsertAfter
                  SK8::InsertBefore SK8::TRUE)

|#

(defparameter Arithmetic-Operator-Functions '((^ ^) (- -) (+ +) (* *) (/ /) (mod mod) (div div)))
(defparameter Arithmetic-Operator-List (mapcar #'first Arithmetic-Operator-Functions))
(defparameter Simple-Operator-Functions '((& &) (as-a as-a)
                                               (type-thing-coerce type-thing-coerce)))
(defparameter Simple-Operator-List (mapcar #'first Simple-Operator-Functions))
(defparameter Comparator-Operator-Functions '((eq eq) (neq neq) 
                                                   (is =) (is-not ­) 
                                                   (= =) (­ ­) 
                                                   (< <) (> >)
                                                   (<= <=) (>= >=)
                                                   (is-not-a is-not-a) (is-a is-a)
                                                   (starts-with startsWith)
                                                   (contains contains) (doesnt-contain doesnt-contain)
                                                   (is-contained is-contained) (is-not-contained is-not-contained) 
                                                   (ends-with endswith)))

(defparameter Logical-Inverse-Operators '((neq eq)
                                               (­ =)
                                               (is-not =)
                                               (is-not-a is-a) 
                                               (doesnt-contain contains) 
                                               (is-not-contained is-contained)))
(defparameter Argument-Inverse-Operators '((is-contained contains)
                                                (type-thing-coerce as-a)))

(defparameter Comparator-Operator-List (mapcar #'first Comparator-Operator-Functions))

(defparameter Boolean-Operator-Functions '( (xor xor) (or or) (and and) (not not)))
(defparameter Boolean-Operator-List (mapcar #'first Boolean-Operator-Functions))

(defparameter Operator-Functions (append Boolean-Operator-Functions Comparator-Operator-Functions 
                                            Simple-Operator-Functions Arithmetic-Operator-Functions))
(defparameter Operator-List (mapcar #'first Operator-Functions))

(defparameter path-expression-test-tokens '(WHERE THROUGH IN FROM BEFORE AFTER))

(defparameter numeric-selector-start-tokens '(FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH
                                                   EIGHTH NINTH TENTH ELEVENTH TWELTH))
(defparameter special-path-location-tokens '(MIDDLE LAST ANY))

(defparameter function-ending-tokens (append Comparator-Operator-List
                                                 Boolean-Operator-List
                                                 '(WITH WITHOUT IN)
                                                 '(|}| |)| )))

(defparameter selection-ending-tokens (append function-ending-tokens
                                                  Arithmetic-Operator-List
                                                  '(|,|)))


(defparameter sk8script-expression-reserved-words (append Operator-List
                                                                special-path-location-tokens
                                                                numeric-selector-start-tokens
                                                                path-expression-test-tokens
                                                                '(OF ONTO PASS APPLY CALL
                                                                  ITEM WITH WITHOUT NIL OF |'S| PROCESSED-S |,|)
                                                                '(|(| |)| |{| |}| THE A BOGUS)))

(defparameter non-argument-reserved-words (append Operator-List
                                                       path-expression-test-tokens
                                                       '(OF ONTO PASS APPLY CALL
                                                         ITEM WITH WITHOUT NIL OF |'S| PROCESSED-S |,|)
                                                       '(|(| |)| |{| |}| THE A BOGUS)))


;;;(defparameter sk8script-generated-functions '(LIST SCRIPT NTH))
;;___________________________________________________________________________________________
;;___________________________________________________________________________________________


(defun parse-non-function-identifier (tok)
  (let ((val (if (sk8-token-p tok) (sk8-token-val tok) tok)))
    (if (and (symbolp val) 
             (eq (symbol-package val) (find-package :skil))
             (not (memq val '(me any last middle item it ssOurTestItem ssArgToBeFilledIn))))
      (if (eq val 'with)
        (parse-error "With followed an unidentified possible function.  Please add \"()\" after the function name as function calls with no required arguments must use a syntax of \"functionName() with KeywordArgument value\"" tok)
        (parse-error (format nil "Unexpected reserved word \"~a\" in expression." val) tok) ))
    (if (and (symbolp val)
             (neq (symbol-package val) (find-package :skil))
             (neq val 'last)
             )
      (let ((glob? (member val handler-globals :key #'transform-to-symbol))
            (loc? (member val handler-locals :key #'(lambda (x) (transform-to-symbol (first x))))))
        (if (or glob?
                (memq val '(sk8::true sk8::false sk8::sk8scriptresult))
                (and (not loc?) 
                     (boundp val)
                     (multiple-value-bind (sym type) 
                                          (find-symbol (symbol-name val) (package *current-project*))
                       ;;this makes sure we don't use things like 't and 'nil
                       (and sym (neq type :inherited)))
                     )
                (and generate-list-of-globals-p
                     (not loc?) 
                     (memq val current-special-global-list)
                     ))
          (progn
            (unless glob?
              (setf handler-globals (delete-duplicates (cons val handler-globals) :key #'transform-to-symbol)))
            `(LIB ,(ccl::keywordify (package-name (symbol-package val))) OBJECT ,val))
          (progn
            (unless loc?
              (unless in-set-compilation
                (push (sk8dev::make-scripterror :linenumber (sk8-token-line-number tok)
                                                :posnumber (list
                                                            (sk8-token-start-line-pos  tok)
                                                            (sk8-token-end-line-pos  tok))
                                                :errorMessage (format nil "Variable ~a not initialized before being used." val))
                      current-warning-list))
              (setf handler-locals (delete-duplicates (cons (list val nil) handler-locals) 
                                                      :key #'(lambda (x) (transform-to-symbol (first x))))))
            val)))
      val)
    ))


;;___________________________________________________________________________________________
;;___________________________________________________________________________________________

(defun preprocess-expression (ScanTo)
  (let (res tmp curToken prevTokenVal)
    (ccl::while (not (and (memq (setf curToken (peek-token-symbol)) ScanTo)
                          (not (memq prevTokenVal '(WITH WITHOUT)))))
      (cond 
       ((and (memq curToken *sk8script-commandtokens*)
             (not (memq prevTokenVal '(WITH WITHOUT))))
        (parse-error (format nil "Unexpected reserved word ~s in expression." (pretty-symbol-name curToken)) (peek-token)))
       ((memq (peek-token-symbol) '(EOL END-OF-STREAM)) 
        (parse-error (concatenate 'string "Expected expression to be followed by: " 
                                  (apply 'concatenate (cons 'string (mapcar #'symbol-name scanTo)))))))
      (setf prevTokenVal (peek-token-symbol))
      (setf tmp (consume-token))
      (setf res (nconc res (list tmp))))
    res
    ))

(defun sk8compile-expression (ScanTo consumeLast?) 
  (let (res)
    (setf res (parse-sk8-expression (preprocess-expression ScanTo)))
    (if (not (memq (peek-token-symbol) scanTo))
      (parse-error (concatenate 'string "Expected expression to be followed by: " 
                                (apply 'concatenate (cons 'string (mapcar #'symbol-name scanTo))))))
    (if consumeLast? (consume-token-symbol))
    res
    ))




;;;-------------------------------------------------------------------------------

(defun remove-unneeded-tokens (exprList)
  (setf exprlist (cons THE-BOGUS-TOKEN exprlist))
  (let ((curItem exprlist)
        curval)
    (ccl::while (setf curval (second curitem))
      (setf curval (sk8-token-val curval))
      (cond
       ((or (memq curval '(THE A AN))
            (and (eq curval 'WHERE) 
                 (memq (sk8-token-val (first curitem)) Boolean-Operator-List)))
        (setf (rest curitem) (cddr curitem)))
       ((and (memq curval '(AFTER BEFORE))
             (sk8-token-p (fourth curitem))
             (memq (sk8-token-val (fourth curitem)) path-expression-test-tokens))
        (setf (nthcdr 2 curitem) 
              (nconc (mapcar #'(lambda (x) (make-sk8-token x -1 -1)) '(FIRST ITEM WHERE IT =)) 
                     (nthcdr 2 curitem)))
        )
       ((and (memq curval '(NEW COPY))
             (third curitem)
             (neq (sk8-token-val (third curitem)) 'OF))
        (setf (nthcdr 2 curitem) (cons (make-sk8-token 'OF -1 -1) (nthcdr 2 curitem))))
       ((eq curval 'CALL)
        (set-sk8-token-val (second curitem) 'funcall)
        (unless (or (not (fourth curitem))
                    (eq (sk8-token-val (fourth curitem)) 'OF))
          (parse-error "Expected an OF to follow after function name to call." (fourth curitem)))
        (if (fourth curitem) (set-sk8-token-val (fourth curitem) '|,|))
        (setf (nthcdr 2 curitem) (cons (make-sk8-token 'OF -1 -1) (nthcdr 2 curitem)))
        (setf curitem (rest curitem)))
       ((eq curval 'APPLY)
        (set-sk8-token-val (second curitem) 'apply)
        (unless (and (fourth curitem)
                     (eq (sk8-token-val (fourth curitem)) 'ONTO))
          (parse-error "Expected a TO or an ONTO to follow after function name to apply." (third curitem)))
        (set-sk8-token-val (fourth curitem) '|,|)
        (setf (nthcdr 2 curitem) (cons (make-sk8-token 'OF -1 -1) (nthcdr 2 curitem)))
        (setf curitem (rest curitem)))
       ((eq curval 'PASS)
        (unless cur-handler-name
          (parse-error "Pass can only be called from within a handler definition." (second curitem)))
        (set-sk8-token-val (second curitem) cur-handler-name)
        (unless (and (third curitem)
                     (eq (sk8-token-val (third curitem)) 'ONTO))
          (parse-error "Expected a TO or an ONTO to follow after Pass." (fourth curitem)))
        (set-sk8-token-val (third curitem) 'OF)
        (setf curitem (rest curitem)))
       (t
        (setf curitem (rest curitem)))))
    (rest exprlist)))
;;;-------------------------------------------------------------------------------

(defun preprocess-the-type-coercion (exprList)
  (setf exprlist (cons THE-BOGUS-TOKEN exprlist))
 (let ((curItem exprlist)
        (cnt 0)
        curval
        curtype
        curThing)
    (ccl::while (setf curval (and (first curitem) (sk8-token-val (first curitem))))
      (when (and (eq curval 'THE)
                 (setf curtype (and (second curitem) (sk8-token-val (second curitem))))
                 (item-or-object-p curtype)
                 (neq curtype 'item)
                 (setf curThing (and (third curitem) (sk8-token-val (third curitem))))
                 (or (memq curThing '(|(| |}|))
                     (not (memq curthing sk8script-expression-reserved-words)))
                 )
        (setf (nthcdr cnt exprlist) (cons (second curitem) (cons (make-sk8-token 'type-thing-coerce -1 -1) (nthcdr (+ cnt 2) exprlist))))
        )
      (incf cnt)
      (setf curitem (rest curitem)))
    (rest exprlist)))

;;;-------------------------------------------------------------------------------

(defun preprocess-pair-values (exprList)
  (let (startToksStack 
        (cnt 0)
        correspondingItem)
    (dolist (i exprlist)
      (case (sk8-token-val i)
        (|(|  
         (push (list '|(| cnt) startToksStack))
        (|{|  
         (push (list '|{| cnt) startToksStack))
        (|}|  
         (setf correspondingItem (pop startToksStack))
         (unless (eq (first correspondingItem) '|{|) (parse-error "Unmatched Right Brace" i))
         (setf (sk8-token-CorrespondingToken i) (second correspondingItem))
         (setf (sk8-token-CorrespondingToken (nth (second correspondingItem) exprlist)) cnt)
         )
        (|)|  
         (setf correspondingItem (pop startToksStack))
         (unless (eq (first correspondingItem) '|(|) (parse-error "Unmatched Right Parenthesis" i))
         (setf (sk8-token-CorrespondingToken i) (second correspondingItem))
         (setf (sk8-token-CorrespondingToken (nth (second correspondingItem) exprlist)) cnt)
         ))
      (incf cnt)
      )
    (when startToksStack
      (if (eq (caar startToksStack) '|(|)
        (parse-error "Unmatched Left Parenthesis" (nth (second (pop startToksStack)) exprlist))
        (parse-error "Unmatched Left Brace" (nth (second (pop startToksStack)) exprlist))))
    exprList))



;;;-------------------------------------------------------------------------------


(defun next-token-ignoring-parens (exprlist startNum testFun &optional len)
  (let ((curNum startNum)
        (curitem (nthcdr startNum exprlist)))
    (unless len (setf len (length exprlist)))
    (unless (or (not (first curItem)) (>= curNum len)) 
      (loop
        (if (memq (sk8-token-val (first curItem)) '(|(| |{|))
          (progn
            (setf curNum (1+ (sk8-token-CorrespondingToken (first curItem))))
            (unless curNum (error (format nil "why no corresponding token for this?!?! ~s" (first curitem))))
            (setf curItem (nthcdr curNum exprList))
            )
          (progn
            (setf curItem (rest curItem))
            (incf curNum)))
        (when (or (not (first curItem)) (>= curNum len)) (return))
        (when (funcall testfun (first curItem)) (return))))
    (min curNum len)
    ))
;;(try-it-out "objectstring of (1,2,3)")

;;;------------------------------------------------------------------

#| 
;;(try-it-out "fillcolor of stage")
;;(try-it-out "sendtolog of keytest1 of 1 with key1 3")
;;(try-it-out "item 1 in item 2 in {1,2,3}")
;;(try-it-out "item 1 in item 2 in {{1,2},{3,4}}")
;;(try-it-out "item 1 in item 1 thru 3 where item 1 in it = 1 and item 2 in it = 2 in {{1,2},{3,4}}")


|#

(defun find-end-of-in-statement (exprlist cnt len items)
  (let (curitem selectionPhraseStack)
    (ccl::while (and cnt
                     (< cnt len)
                     (not (and (memq (sk8-token-val (setf curitem (nth cnt exprlist))) items)
                               (not selectionPhraseStack)))
                     (not (and (eq (sk8-token-type curitem) 'SELECTION)
                               (not selectionPhraseStack))))
      (when (memq (sk8-token-type curitem) '(MULTI-SELECTION-PHRASE SELECTION-PHRASE))
        (push curitem selectionPhraseStack))
      (when (eq (sk8-token-type curitem) 'SELECTION)
        (pop selectionPhraseStack))
      (setf cnt (next-token-ignoring-parens exprlist cnt #'(lambda (x) (declare (ignore x)) t) len))
      )
    cnt
    ))

;;(run-it "sendtolog of item 3 whose fillcolor <> white in contents of stage")

(defun preprocess-of-statements (exprList)
  (let* ((curitem exprlist)
         startToken startTokenLoc startTokenVal
         endToken
         (len (length exprList))
         (cnt (1- len))
         )
    (ccl::while (> cnt 0)
      (setf curItem (nthcdr cnt exprlist))
      (when (memq (sk8-token-val (first curitem)) '(IN OF))
        (setf exprlist (preprocess-pair-values exprList))
        (setf startToken (first curitem))
        (setf startTokenVal (sk8-token-val startToken))
        (setf startTokenLoc cnt)
        (setf (sk8-token-type starttoken) (if (eq (sk8-token-val startToken) 'OF)
                                            'FUNCTION-ARGS
                                            'SELECTION))
        (set-sk8-token-val startToken '|(|)
        (incf cnt)
        (setf cnt  (if (eq startTokenVal 'OF)
                     (find-end-of-in-statement exprlist cnt len function-ending-tokens)
                     (find-end-of-in-statement exprlist cnt len selection-ending-tokens)))
        (unless cnt (setf cnt len))
        (setf endToken (make-sk8-token '|)| -1 -1))
        (setf (sk8-token-CorrespondingToken startToken) cnt)
        (setf (sk8-token-CorrespondingToken endToken) startTokenLoc)
        (setf (nthcdr cnt exprlist) (cons endToken (nthcdr cnt exprlist)))
        (incf len)
        (setf cnt startTokenLoc)
        )
      (decf cnt))
    exprlist))

;;----------------------------------------------------------------

(defun appropriate-apostrophe-arg (val)
  (and (symbolp val)
       (not (memq val operator-list))
       (not (memq val '(WITH WITHOUT '|,|)))))


;;Here we parenthesize all apostrophe s blobs and mark them as 
;;already parenthesized by changing their symbol to processed-s

(defun preprocess-apostrophe-statements (exprList)
  (setf exprlist (preprocess-pair-values (cons THE-BOGUS-TOKEN exprList)))
  (let ((cnt 1)
        PreviousStart
        previousStartTok
        NextPotentialS
        (len (length exprlist))
        justAfterFunctionToken
        )
    (if (eq (sk8-token-val (second exprList)) '|'S|)
      (parse-error "A single variable or constant must come before an 's" (second exprList)))
    (ccl::while (nth cnt exprList)
      (when (eq (sk8-token-val (nth cnt exprList)) '|'S|) 
        (set-sk8-token-val (nth cnt exprList) 'PROCESSED-S)
        ;;unless the token after we're done is a special function call token, then we can parenthesize our expression 
        ;;right away.  thus we can do "my height + 1" and not be ambiguous.
        (if (and (setf justAfterFunctionToken (nth (+ 2 cnt) exprlist))
                 (memq (sk8-token-val justAfterFunctionToken) '(OF |(| WITH WITHOUT)))
          (setf NextPotentialS (next-token-ignoring-parens exprlist cnt
                                                           #'(lambda (x) (memq (sk8-token-val x) function-ending-tokens))
                                                           len))
          (setf NextPotentialS (+ 2 cnt)))
        (unless NextPotentialS (setf NextPotentialS len))
        (unless (appropriate-apostrophe-arg (sk8-token-val (nth (1+ cnt) exprlist))) 
          (parse-error "'S must be followed by a function name." (nth cnt exprList)))
        (setf PreviousStart (- cnt 1))
        (setf previousStartTok (nth PreviousStart exprlist))
        (if (sk8-token-correspondingtoken previousStartTok)
          (setf PreviousStart (sk8-token-correspondingtoken previousStartTok)))
        (when (member '|'S| (nthcdr (1+ cnt) exprlist) :key #'sk8-token-val)
          (setf (nthcdr PreviousStart exprlist)
                (cons (make-sk8-token '|(| -1 -1) (nthcdr PreviousStart exprlist)))
          (setf (nthcdr (1+ NextPotentialS) exprlist) 
                (cons (make-sk8-token '|)| -1 -1)
                      (nthcdr (1+ NextPotentialS) exprlist)))
          (setf exprlist (preprocess-pair-values exprList))
          (incf len 2)
          (setf cnt (+ 2 NextPotentialS))
          (decf cnt))
        )
      (incf cnt))
    (preprocess-pair-values (rest exprList))
    ))

;;(member '|'S| (nthcdr (1+ cnt) exprlist) :key #'sk8-token-val)
(defun expression-process-apostrophe-statements (exprList)
  (setf exprList (cons THE-BOGUS-TOKEN exprList))
  ;;then we shift the argument from one side of the 's to the other 
  (let ((cnt 0)
        objname tmp
        (len (length exprList)))
    (ccl::while (nth cnt exprList)
      (when (eq (sk8-token-val (nth cnt exprlist)) 'PROCESSED-S)
        (setf objname (nth (- cnt 1) exprList))
        (setf (nthcdr (- cnt 1) exprlist) (nthcdr (+ cnt 1) exprlist))
        (decf cnt 1)
        (cond
         ((>= (+ cnt 1) (length exprlist))
          (setf tmp (make-sk8-token (list objname) -1 -1))
          (setf (sk8-token-type tmp) 'FUNCTION-ARGS)
          (setf exprlist (nconc exprlist (list tmp)))
          (incf len)
          )
         ((and (listp (sk8-token-val (nth (+ cnt 1) exprList)))
               (neq (sk8-token-type (nth (+ cnt 1) exprList)) 'selection))
          (set-sk8-token-val (nth (+ cnt 1) exprList) (cons (make-sk8-token objname -1 -1) (sk8-token-val (nth (+ cnt 1) exprList))))
          (incf cnt 2))
         (t
          (setf tmp (make-sk8-token (list objname) -1 -1))
          (setf (sk8-token-type tmp) 'FUNCTION-ARGS)
          (setf (nthcdr (+ cnt 1) exprlist) (cons tmp (nthcdr (+ cnt 1) exprlist)))
          (incf len)
          (incf cnt 1)))
        )
      (incf cnt)
      )
    (rest exprlist)))


;;___________________________________________________________________________________________
;;;parentheses 

;;(try-it-out " ({1, 5}) ")
;;(try-it-out " ({1, Oval, 2, Rectangle, 3, Polygon, 4, RoundRect, 5}) ")
;;(try-it-out "foo of x,y")
;;(try-it-out "fillcolor of item 1 thru 3  in contents of stage")


(defun expression-process-lists-elements (exprList &optional DontParse)
  (setf exprlist (preprocess-pair-values exprlist))
  (let ((len (length exprlist))
        curcnt
        allElements
        curItem
        res
        (cnt 0))
    (ccl::while (< cnt len)
      (setf curcnt cnt)
      (setf cnt (next-token-ignoring-parens exprlist cnt 
                                            #'(lambda (x) (eq (sk8-token-val x) '|,|))
                                            len))
      (push (list curcnt cnt) allElements)
      (incf cnt))
    (dolist (i allElements)
      (setf curItem (subseq exprlist (first i) (second i)))
      (unless dontParse
        (setf curitem (do-parse-sk8-expression curItem))
        )
      (push (make-sk8-token curItem -1 -1) res)
      )
    (setf res (make-sk8-token res -1 -1))
    (setf (sk8-token-type res) (if (> (length (sk8-token-val res)) 1) 'FUNCTION-ARGS 'POTENTIAL-FUNCTION-ARGS))
    res
    ))

(defun expression-process-parentheses (exprList)
  (let ((cnt 1)
        curitem tmp allpairs)
    (setf exprList (cons THE-BOGUS-TOKEN exprList))
    (ccl::while (setf curItem (nth cnt exprList))
      (when (eq (sk8-token-val curitem) '|(|)
        (push (list cnt (1+ (sk8-token-CorrespondingToken curitem))) allpairs)
        (setf cnt (1+ (sk8-token-CorrespondingToken curitem)))
        )
      (incf cnt)
      )
    (dolist (i allpairs)
      (setf tmp (expression-process-lists-elements (subseq exprList 
                                                           (1+ (first i))
                                                           (second i))
                                                   (eq (sk8-token-type (nth (first i) exprlist)) 'SELECTION)))
      (when (sk8-token-type (nth (first i) exprlist))
        (setf (sk8-token-type tmp) (sk8-token-type (nth (first i) exprlist))))
      (setf (nthcdr (first i) exprList) 
            (cons tmp
                  (nthcdr (+ 1 (second i)) exprList)))
      )
    (rest exprList)))

;;(try-it-out "fillcolor of stage")
;;;-------------------------------------------------------------------------------

(defun expression-process-lists (exprList)
  (let ((cnt 1)
        curitem
        endPoint
        newList)
    (setf exprlist (cons THE-BOGUS-TOKEN exprlist))
    (setf exprlist (preprocess-pair-values exprlist))
    (ccl::while (setf curitem (nth cnt exprlist))
      (when (eq (sk8-token-val curitem) '|{|)
        (setf endpoint (sk8-token-CorrespondingToken curitem))
        (setf newList (expression-process-lists-elements (subseq exprList (1+ cnt) endpoint)))
        (set-sk8-token-val newList
                           ;;`((LIB :sk8 FUNCTION SK8::Collect) (LIB :sk8 OBJECT SK8::LIST)   
                                         ;;****************************COLLECT IS BROKEN BY CONGRUENCY SHIT!!! 
                           `((LIB :sk8 FUNCTION SK8::List)
                             ,@(sk8-token-val newList)))
        (setf (sk8-token-type newList)
              'LIST)
        (setf (nthcdr cnt exprlist)
              (cons newList (nthcdr (+ 1 endpoint) exprlist)))
        (setf exprlist (preprocess-pair-values exprlist))
        )
      (incf cnt)
      )
    (rest exprlist)
    ))

;;;-------------------------------------------------------------------------------

(defun expression-process-operators (exprList)
  (let ((oprs Operator-Functions)
        memberResult
        opr doInvert doNot res
        )
    (dolist (i oprs)
      (setf memberResult (member (first i) exprList :key #'sk8-token-val))
      (when memberResult
        (setf opr (second i))
        (return)))
    (if memberresult
      (let ((arg1 (subseq exprList 0 (- (length exprList) (length memberResult))))
            (arg2 (rest memberResult)))
        (if (and (memq opr '(not - +)) (not arg1))
          (list opr (make-sk8-token (expression-process-operators arg2) -1 -1))
          (progn
            (when (eq opr 'not)
              (parse-error "Not only takes a single argument." (first memberResult)))
            (unless (and arg1 arg2)
              (parse-error "Operator needs two arguments." (first memberResult)))
            (setf doNot (second (assoc opr Logical-Inverse-Operators :test #'eq)))
            (if doNot (setf opr doNot))
            (setf doInvert (second (assoc opr Argument-Inverse-Operators :test #'eq)))
            (if doInvert 
              (setf res (list doInvert
                              (make-sk8-token (expression-process-operators arg2) -1 -1)
                              (make-sk8-token (expression-process-operators arg1) -1 -1)))
              (setf res (list opr
                              (make-sk8-token (expression-process-operators arg1) -1 -1)
                              (make-sk8-token (expression-process-operators arg2) -1 -1))))
            (if doNot (setf res (list 'not (make-sk8-token res -1 -1))))
            res
            )))
      exprList)))




;;;;---------------------------------------------------------------------------
;;;;insert and remove Commands


(defun preprocess-special-collection-functions (exprlist)
  (let ((cnt 0)
        expressionStart
        curitem)
    (ccl::while (setf curitem (nth cnt exprlist))
      (cond 
       ((memq (sk8-token-val curitem) '(REMOVE POSITION))
        (if (eq (sk8-token-val curitem) 'REMOVE) 
          (set-sk8-token-val curitem 'SK8::RemoveInternal)
          (set-sk8-token-val curitem 'SK8::PositionInternal))
        (unless (eq (sk8-token-val (nth (1+ cnt) exprlist)) 'OF)
          (setf (nthcdr (1+ cnt) exprlist) (cons (make-sk8-token 'OF -1 -1) (nthcdr (1+ cnt) exprlist))))
        (incf cnt 2)
        (setf expressionStart cnt)
        (when (eq (sk8-token-val (nth cnt exprlist)) '|(|)
          (setf exprlist (preprocess-pair-values exprList))
          (setf cnt (sk8-token-CorrespondingToken (nth cnt exprlist)))
          )
        (if (and (sk8-token-p (nth (1+ cnt) exprlist))
                 (memq (sk8-token-val (nth (1+ cnt) exprlist)) '(IN FROM)))
          (progn
            (set-sk8-token-val (nth (1+ cnt) exprlist) 'IN)
            (setf (nthcdr expressionStart exprlist) (nconc (mapcar #'(lambda (x) (make-sk8-token x -1 -1)) '(FIRST ITEM WHERE IT =)) (nthcdr expressionStart exprlist)))
            )
          (when (position 'FROM exprlist :key #'sk8-token-val)
            (set-sk8-token-val (nth (position 'FROM exprlist :key #'sk8-token-val) exprlist) 'IN) )))
       ((eq (sk8-token-val curitem) 'INSERT)
        (unless (eq (sk8-token-val (nth (1+ cnt) exprlist)) 'OF)
          (setf (nthcdr (1+ cnt) exprlist) (cons (make-sk8-token 'OF -1 -1) (nthcdr (1+ cnt) exprlist))))
        (incf cnt 2)
        (when (eq (sk8-token-val (nth cnt exprlist)) '|(|)
          (setf exprlist (preprocess-pair-values exprList))
          (setf cnt (sk8-token-CorrespondingToken (nth cnt exprlist))))
        (case (sk8-token-val (nth (1+ cnt) exprlist))
          (FRONT 
           (set-sk8-token-val curitem 'insertInFront))
          (BACK 
           (set-sk8-token-val curitem 'insertAtEnd))
          (BEFORE
           (set-sk8-token-val curitem 'SK8::InsertBefore))
          (AFTER
           (set-sk8-token-val curitem 'SK8::InsertAfter))
          (t 
           (parse-error "Insert needs to be followed by a single item and an Into, In Front Of, In Back Of, Before, or After"
                        (nth (1+ cnt) exprlist)))
          )
        (set-sk8-token-val (nth (1+ cnt) exprlist) '|,|))
       )
      (incf cnt))
    exprlist
    ))

;;;-------------------------------------------------------------------------------

(defun transform-function-sym (sym)
  (if (memq sym '(call-do-inherited call-with-statement funcall apply insertInFront InsertAtEnd))
    sym
    `(LIB ,(ccl::keywordify (package-name (symbol-package sym))) FUNCTION ,sym)))

(defun parsing-function-sym-p (tok)
  (and (symbolp tok)
       (not (memq tok Operator-List))
       ;(not (memq tok sk8script-generated-functions))
       ))

;(try-it-out "1 + (1 + 1) + 1")

(defun find-last-function-arg-token (exprlist)
  (let ((cnt (1- (length exprlist))))
    (loop
      (when (or (<= cnt 1) 
                (eq (sk8-token-type (nth cnt exprlist)) 'FUNCTION-ARGS)
                (and (eq (sk8-token-type (nth cnt exprlist)) 'POTENTIAL-FUNCTION-ARGS)
                     (>= (1- cnt) 0)
                     (not (memq (sk8-token-val (nth (1- cnt) exprlist)) sk8script-expression-reserved-words))
                     (or (< (- cnt 2) 0)
                         (neq (sk8-token-val (nth (- cnt 2) exprlist)) 'WITH)))
                )
        (return))
      (decf cnt))
    (if (> cnt 1) cnt nil))
  )

;;(try-it-out "fillcolor of (item 1 thru 3  in contents of stage) , y")
;;(try-it-out "foo of x,y")

(defun deepest-token (tok)
  (let ((curval (sk8-token-val tok)))
    (cond 
     ((and (listp curval) (= (length curval) 1) (sk8-token-p (first curval)))
      (deepest-token (first curval)))
     ((sk8-token-p curval)
      (deepest-token curval))
     (t tok))))

(defun process-args-for-potential-path (args)
  (let (pathargs returnval)
    (setf args (mapcar #'deepest-token args))
    (setf pathArgs (remove-if-not #'(lambda (tok)
                                      (and (sk8-token-p tok)
                                           (eq (sk8-token-type tok) 'MULTI-SELECTION-EXPRESSION)))
                                  args))
    (when pathArgs
      (unless (= (length pathArgs) 1)
        (parse-error "Function calls can only have one argument that is a Plural Selection Expression." (second pathArgs)))
      (setf returnval (sk8-token-val (first pathargs)))
      (setf (sk8-token-type (first pathargs)) 'PROCESSED-SELECTION-EXPRESSION)
      (set-sk8-token-val (first pathargs) 'ssArgToBeFilledIn))
    (list args returnval)
    ))

(defun process-args-for-special (args)
  (let (pathargs returnval)
    (setf args (mapcar #'deepest-token args))
    (setf pathArgs (remove-if-not #'(lambda (tok) (and (sk8-token-p tok)
                                                       (memq (sk8-token-type tok) '(SINGLE-SELECTION-EXPRESSION MULTI-SELECTION-EXPRESSION))))
                                  args))
    (when pathArgs
      (unless (= (length pathArgs) 1)
        (parse-error "Special functions can only have one argument that is a selection expression." (second pathArgs)))
      (setf returnval (sk8-token-val (first pathargs)))
      (setf (sk8-token-type (first pathargs)) 'PROCESSED-SELECTION-EXPRESSION)
      (set-sk8-token-val (first pathargs) 'ssArgToBeFilledIn))
    (list args returnval)
    ))


(defun gen-multi-function-call (funName tmp)
  (cond 
   ((memq funName '(SK8::InsertBefore SK8::InsertAfter))
    `(script 
      (input ssCurrentCollection ssCurrentItem ssCurrentState ssOurCount)
      ((lib :sk8 function SK8::insertAtVisitState) ssCurrentCollection ssCurrentState ,(second tmp)
       with after ,(eq funName 'SK8::InsertAfter))
      ))
   ((eq funName 'SK8::RemoveInternal)
    `(script 
      (input ssCurrentCollection ssCurrentItem ssCurrentState ssOurCount)
      ((lib :sk8 function SK8::removeVisitState) ssCurrentCollection ssCurrentState)
      ))
   ((eq funName 'SK8::PositionInternal)
    `(script 
      (input ssCurrentCollection ssCurrentItem ssCurrentState ssOurCount)
      ssOurCount
      ))
   (t
    `(script 
      (input ssCurrentCollection ssArgToBeFilledIn ssCurrentState ssOurCount)
      ,tmp)
    )))

(defun wrap-for-set (code)
  `(funcall ssCurrentPathExpressionCollectionSet ,code))

(defun multi-function-call-wrapper (curFunctionName hasPathArg tmp)
  (if (memq curFunctionName '(sk8::InsertBefore sk8::InsertAfter SK8::RemoveInternal))
    (progn
      (setf haspatharg (deep-copy-list haspatharg))
      (setf (third hasPathArg) (wrap-for-set (third hasPathArg)))
      )
    (progn
      (when (and (listp (second hasPathArg))
                 (listp (third (second hasPathArg)))
                 (eq (first (third (second hasPathArg))) 'sscurrentpathexpressioncollectionset))
        (setf (second (third (second hasPathArg))) nil))  ;;;don't need to do special setting...
      ))
  `(script 
    (local (ssCurrentPathExpressionAction 
            ,(gen-multi-function-call curFunctionName tmp))
           (ssCurrentPathExpressionActionType 
            (quote ,curfunctionname)))
    ,hasPathArg
    ))

(defun expression-process-function-calls (exprList)
  (let (curPartOfList
        args hasPathArg res
        curloc
        curFunctionName
        curFunctionNameTok
        keywordArg keywordArgVal
        tmp)
    (setf exprlist (cons THE-BOGUS-TOKEN exprlist)) 
    (ccl::while (setf curloc (find-last-function-arg-token exprlist))
      (setf curFunctionNameTok (nth (1- curloc) exprlist))   ;;; we store this in case we make a warning.
      (setf curFunctionName (sk8-token-val curFunctionNameTok))
      (unless (parsing-function-sym-p curFunctionName)
        (parse-error "Invalid function name" (nth (1- curloc) exprlist)))
      (setf curPartOfList (nthcdr curloc exprlist))
      (if (memq curFunctionName '(SK8::PositionInternal SK8::InsertBefore SK8::InsertAfter SK8::RemoveInternal))
        (setf res (process-args-for-special (sk8-token-val (first curPartOfList))))
        (setf res (process-args-for-potential-path (sk8-token-val (first curPartOfList)))))
      (setf args (first res))
      (setf hasPathArg (second res))
      (setf tmp (append (list (transform-function-sym curFunctionName))
                        (strip-expression-tokens args)
                        (list 'with)))
      (if (and (lib-form-p (first tmp))
               (symbolp (fourth (first tmp)))
               (not (fboundp (fourth (first tmp))))
               (not (memq (fourth (first tmp)) '(SK8::PositionInternal SK8::InsertBefore SK8::InsertAfter SK8::RemoveInternal))))
       (push (sk8dev::make-scripterror :linenumber (sk8-token-line-number curFunctionNameTok)
                                          :posnumber (list
                                                      (sk8-token-start-line-pos curFunctionNameTok)
                                                      (sk8-token-end-line-pos curFunctionNameTok))
                                          :errorMessage (format nil "Undefined function or handler ~a being called."
                                                                curFunctionName))
                current-warning-list))
      (setf curPartOfList (rest curPartOfList))
      (ccl::while (and (sk8-token-p (first curPartOfList))
                       (memq (sk8-token-val (first curPartOfList)) '(WITH WITHOUT)))
        (cond
         ((eq (sk8-token-val (first curPartOfList)) 'WITHOUT)
          (setf tmp (append tmp (list (sk8-token-val (second curPartOfList)) 'sk8::false)))
          (setf curPartOfList (rest (rest curPartOfList)))
          )
         ((and (eq (sk8-token-val (first curPartOfList)) 'WITH)
               (or (not (third curPartOfList))
                   (memq (sk8-token-val (third curPartOfList)) function-ending-tokens)))
          (setf tmp (append tmp (list (sk8-token-val (second curPartOfList)) 'sk8::true)))
          (setf curPartOfList (cddr curPartOfList))
          )
         (t        
          (setf keywordArg (sk8-token-val (second curPartOfList)))
          (setf keywordArgVal nil)
          (setf curPartOfList (cddr curPartOfList))
          (ccl::while (and (first curPartOfList)
                           (not (memq (sk8-token-val (first curPartOfList)) function-ending-tokens)))
            (push (first curPartOfList) keywordArgVal)
            (setf curPartOfList (rest curPartOfList)))
          (setf keywordArgVal (make-sk8-token 
                               (do-parse-sk8-expression (nreverse keywordArgVal)) 
                               -1 -1))
          (setf tmp (append tmp (list keywordArg keywordArgVal)))
          (when (and generate-list-of-globals-p 
                     (eq curFunctionName 'sk8::new)
                     (eq keywordArg 'sk8::objectname)
                     )
            (setf keywordArgVal (strip-expression-token keywordargval))
            (when (stringp keywordargval)
              (push (let ((*package* (package *current-project*))) (read-from-string keywordargval nil nil)) current-special-global-list))
            ))))
      (when hasPathArg
        (setf tmp (multi-function-call-wrapper curFunctionName hasPathArg tmp)))
      (when (memq curFunctionName '(insertAtEnd InsertInFront))
        (setf tmp (list 'funcall (generate-collection-location-setter (third tmp)) tmp)))
      (setf tmp (make-sk8-token tmp -1 -1))
      (setf (nthcdr (- curloc 1) exprlist)
            (cons tmp
                  curPartOfList))
      )
    (rest exprlist)))

;;;__________________________________________________________________________________________________________

(defmacro not-reserved-word (tok)
  `(not (memq (sk8-token-val ,tok) sk8script-expression-reserved-words)))

(defun item-or-object-p (val)
  (or (eq val 'item)
      (and (symbolp val)
           (or (and (boundp val)
                    (or  ;;** KLUDGE:  Need a test: (sk8objectp symbol)
                     (mf::object-class-p (symbol-value val))
                     (memq val '(list string number integer symbol character)))
                    (not (member val handler-locals :key #'transform-to-symbol)))
               (member val handler-globals :key #'transform-to-symbol)))))

(defun check-for-start-token (tok)
  (cond 
   ((memq (sk8-token-val tok) numeric-selector-start-tokens)
    (set-sk8-token-val tok (1+ (position (sk8-token-val tok) numeric-selector-start-tokens)))
    tok)
   ((memq (sk8-token-val tok) special-path-location-tokens)
    tok)
   nil))

(defun quick-handle-list (exprlist startTok endTok)
  (let (newtok)
    (setf newtok (make-sk8-token (do-parse-sk8-expression (subseq exprlist (+ 1 startTok) (+ 1 endtok))) -1 -1))
    (setf (nthcdr (+ 1 startTok) exprlist) (cons newtok (nthcdr (+ endtok 1) exprlist)))
    ))

(defun preprocess-path-expressions (exprList)
  (setf exprlist (cons THE-BOGUS-TOKEN exprlist))
  (setf exprlist (preprocess-pair-values exprList))
  (let ((cnt 1) previousItem
        endloc type-specifier fromVal
        toval newtok curitem)
    (ccl::while (and (setf curitem (nthcdr cnt exprlist))
                     (rest curitem))  ;;length >= 2
      ;;First we look for items such as "oval 1 thru 3"
      (cond
       ((and (item-or-object-p (sk8-token-val (first curitem)))
             (or (not (memq (sk8-token-val (second curitem)) sk8script-expression-reserved-words))
                 (and (eq (sk8-token-val (second curitem)) '|{|)
                      (preprocess-pair-values exprList)
                      (quick-handle-list exprlist 
                                         cnt 
                                         (sk8-token-correspondingtoken (second curitem))))
                 ;; this allows item to have arguments which are parenthesized expressions.
             (and (eq (sk8-token-val (second curitem)) '|(|)
                  (eq (sk8-token-val (first curitem)) 'item)
                  (preprocess-pair-values exprList)
                  (quick-handle-list exprlist 
                                     cnt 
                                     (sk8-token-correspondingtoken (second curitem))))
                 )
             (and (third curitem)
                  (memq (sk8-token-val (third curitem)) path-expression-test-tokens))) ;;;this will be changed for nested expressions...
        (if (eq (sk8-token-val (third curitem)) 'THROUGH)
          (progn
            (setf exprlist (preprocess-pair-values exprList))
            (setf endloc (+ cnt 3))
            (unless (or (not (memq (sk8-token-val (fourth curitem)) sk8script-expression-reserved-words))
                        (and (eq (sk8-token-val (fourth curitem)) '|{|)
                             (quick-handle-list exprlist 
                                                (+ cnt 2) 
                                                (sk8-token-correspondingtoken (fourth curitem))))
                        ;; this allows item to have arguments which are parenthesized expressions.
                        (and (eq (sk8-token-val (fourth curitem)) '|(|)
                             (eq (sk8-token-val (first curitem)) 'item)
                             (quick-handle-list exprlist 
                                                (+ cnt 2) 
                                                (sk8-token-correspondingtoken (fourth curitem))))
                        )
              (print (second curitem))
              (parse-error "Expected location information." (fourth curitem)))
            (setf toval (fourth curitem)))
          (progn 
            (setf endloc (+ cnt 1))
            (setf toval nil)))
        (setf newTok (translate-selection-operator (list (first curitem) (second curitem) toval)))
        (setf (nthcdr cnt exprlist) (cons newtok (nthcdr (1+ endloc) exprlist))))
       ;;then we look for items such as "first thru third oval"
       ((and (setf fromVal (check-for-start-token (first curitem)))
             (third curitem))
        (if (eq (sk8-token-val (second curitem)) 'THROUGH)
          (progn
            (unless (setf toval (check-for-start-token (third curitem)))
              (parse-error "Expected location information." (third curitem)))
            (setf endloc (+ cnt 3))
            (setf type-specifier (fourth curitem))
            )
          (progn 
            (setf endloc (+ cnt 1))
            (setf type-specifier (second curitem))
            (setf toval nil)))
        (unless (item-or-object-p (sk8-token-val type-specifier))
          (parse-error "Expected a valid type specifier." type-specifier))
        (setf newTok (translate-selection-operator (list type-specifier fromVal toval)))
        (setf (nthcdr cnt exprlist) (cons newtok (nthcdr (1+ endloc) exprlist))))
       ((and (symbolp (sk8-token-val (first curitem)))
             (not-reserved-word (first curitem))
             (not (listp (sk8-token-val (second curitem))))
             (not (sk8-token-p (sk8-token-val (second curitem))))
             (not (memq (sk8-token-val (second curitem)) non-argument-reserved-words))
             (or (not previousItem) (neq (sk8-token-val previousItem) 'WITH))
             )
        (if (and (third curitem)
                 (not (memq (sk8-token-val (third curitem)) sk8script-expression-reserved-words)))
          (parse-error "Can't figure out what to do with this token." (third curitem))
          (setf (rest curitem) (cons (make-sk8-token 'OF -1 -1) (rest curitem))))
        ))
      (setf previousItem (first curitem))
      (incf cnt))
    (when (find 'THROUGH exprlist :key #'sk8-token-val)
      (parse-error "Invalid selection phrase." (find 'THROUGH exprlist :key #'sk8-token-val)))
    (rest exprlist)))

(defun type-filter-value (the-type)
  (if (and (listp the-type)
           (eq (third the-type) 'sk8::object))
    (fourth the-type)
    the-type))


(defun process-special-positions (pos type-filter )
  (cond 
   ((eq pos 'ANY) 
    `(anyIndex ssCurrentPathExpressionCollection ,type-filter ssCurrentWhereFilter))
   ((eq pos 'MIDDLE) 
    `(middleIndex ssCurrentPathExpressionCollection ,type-filter ssCurrentWhereFilter))
   ((eq pos 'LAST) 
    nil)
   (t
    pos))
  )

(defun translate-selection-operator (exprList)
  (let (type-filter startpos endPos res ssCurrentCollection multirange)
    (setf type-filter (strip-expression-token (first exprlist)))
    (setf startpos (strip-expression-token (second exprlist)))
    (setf endPos (strip-expression-token (third exprlist)))
    (if (or (eq (type-filter-value type-filter) 'sk8::object)
            (eq (type-filter-value type-filter) 'item)
            )
      (setf type-filter nil))
    (setf ssCurrentCollection 'ssCurrentPathExpressionCollection)
    (setf startPos (process-special-positions startpos type-filter))
    (if endpos (setf multirange t))
    (setf endPos (process-special-positions endPos type-filter))
    (setf res (make-sk8-token (if multirange
                                `(fullRangeMapper 
                                  ,ssCurrentCollection ,startpos ,endpos 
                                  ,type-filter ssCurrentWhereFilter
                                  ssCurrentPathExpressionAction
                                  ssCurrentPathExpressionActionType
                                  ssCurrentIterationDirection ssStartingIterationLocation)
                                `(singleItemMapper
                                  ,ssCurrentCollection ,startpos 
                                  ,type-filter ssCurrentWhereFilter
                                  ssCurrentPathExpressionAction
                                  ssCurrentPathExpressionActionType
                                  ssCurrentIterationDirection ssStartingIterationLocation))
                              -1 -1))
    ;(setf res (wrap-for-potential-setting res))
    (if multirange 
      (setf (sk8-token-type res) 'MULTI-SELECTION-PHRASE)
      (setf (sk8-token-type res) 'SELECTION-PHRASE))
    res
    ))

(defun do-generate-path-expression-locations (exprlist start)
  (declare (special pathExprs))
  (let ((cnt start)
        currentExpression
        currentWhere
        currentPhrase
        curitem
        exprType
        )
    (setf currentExpression (list (nth start exprlist)))
    (loop
      ;;initialize variables to start
      (setf currentWhere nil)
      (setf currentPhrase nil)
      (decf cnt)
      ;;now we find the corresponding phrase and where...
      (loop
        (if (< cnt 0) 
          (parse-error "Unmatched selection expression." (first (last currentExpression))))
        (setf curitem (nth cnt exprlist))
        (when (eq (sk8-token-type curitem) 'SELECTION)
          (setf cnt (do-generate-path-expression-locations exprlist cnt)))
        (if (memq (sk8-token-val curitem) '(BEFORE AFTER)) 
          (parse-error "Before and After can only occur before a selection phrase." curitem))
        (when (eq (sk8-token-val curitem) 'WHERE)
          (when currentWhere 
            (parse-error "Unexpected extra test in path expression." curitem))
          (setf currentWhere curitem))
        (when (memq (sk8-token-type curitem) '(MULTI-SELECTION-PHRASE SELECTION-PHRASE))
          (setf currentPhrase curitem)
          (return))
        (decf cnt)
        )
      (decf cnt)  
      (if (and (>= cnt 0) 
               (memq (sk8-token-val (setf curitem (nth cnt exprlist))) '(BEFORE AFTER)))
        (progn
          (unless (eq (sk8-token-type currentPhrase) 'SELECTION-PHRASE)
            (parse-error "Only singular selection phrases can occur after a Before or After statement" curitem))
          (setf exprType curitem))
        (setf exprType nil))
      (push (list exprType currentPhrase currentWhere) currentExpression)
      (unless exprType
        (return))
      )
    (push (nreverse currentExpression) pathexprs)
    (1+ cnt)
    )
  )

(defun generate-path-expression-locations (exprlist)
  (declare (special pathExprs))
  (let ((cnt (1- (length exprlist)))
        curitem
        )
    (ccl::while (>= cnt 0)
      (setf curitem (nth cnt exprlist))
      (when (memq (sk8-token-type curitem) '(MULTI-SELECTION-PHRASE SELECTION-PHRASE))
        (parse-error "Unmatched selection phrase." curitem))
      ;; we found a selection so we need to back up finding everything that refers to this...
      (when (eq (sk8-token-type curitem) 'SELECTION)
        (setf cnt (do-generate-path-expression-locations exprlist cnt)))
      (decf cnt))
    (setf pathexprs (nreverse pathexprs)))
  )


(defun generate-collection-location-setter (cur-path)
  (cond
   ((or (symbolp cur-path)
        (and (listp cur-path)
             (eq (first cur-path) 'lib) 
             ))
    `(script
      (input val)
      (local (obj ,cur-path) )
      (if ((lib :sk8 function sk8::is-a) obj (lib :sk8 object sk8::aliasedCollection)) 
        then
        ((lib :sk8 function sk8::SetPropertyValue) obj ((lib :sk8 function sk8::collectionProperty) obj)  val)
        else
        ,(if (and (listp cur-path) (constantp (fourth cur-path)))
           'val
           `(set ,cur-path to val)))))
   ((and 
     (listp cur-path)
     (not (memq (first cur-path) '(script set list))))
    `(script
      (input val)
      (local (obj ,(second cur-path)) (prop (quote ,(first cur-path))))
      (if (and obj prop ((lib :sk8 function sk8::property) obj prop)) then
          ((lib :sk8 function sk8::SetPropertyValue) obj prop  val)
          else val))
    )
   ))

(defun wrap-selection-code (origcode selectionLoc)
  (let* ((cur-path (parse-sk8-expression (sk8-token-val (first (sk8-token-val selectionLoc)))))
         (newcode (make-sk8-token `(script
                                    (local (ssCurrentPathExpressionCollection ,cur-path)
                                           (ssCurrentPathExpressionCollectionSet
                                            ,(generate-collection-location-setter cur-path))
                                           )
                                    ,(sk8-token-val origcode))
                                  -1 -1)))
    (setf (sk8-token-type newcode) (sk8-token-type origcode))
    newcode)
  )

(defun create-selection-code (selectionPhrase wherePhraseList beforeOrAfter Location)
  (let* ((directionSym (if (eq beforeOrAfter 'before) 'sk8::before 'sk8::after)) 
         (newcode (make-sk8-token `(script
                                    (local 
                                     (ssCurrentWhereFilter
                                      ,(if wherephraselist
                                         `(script 
                                           (input it)
                                           ,(parse-sk8-expression wherePhraseList))
                                         nil))
                                     (ssCurrentIterationDirection
                                      (quote ,directionSym))
                                     (ssStartingIterationLocation
                                      ,(or location 0))
                                     )
                                    ,(sk8-token-val selectionPhrase))
                                  -1 -1)))
    (if (eq (sk8-token-type selectionPhrase) 'MULTI-SELECTION-PHRASE)
      (setf (sk8-token-type newcode) 'MULTI-SELECTION-EXPRESSION)
      (setf (sk8-token-type newcode) 'SINGLE-SELECTION-EXPRESSION))
    newcode)
  )


(defun expression-process-path-expressions (exprlist)
  (setf exprlist (cons THE-BOGUS-TOKEN exprlist))
  (let ((pathExprs nil)
        start end curPhrase curWhere
        newCode
        curEnd
        direction
        curExpression)
    (declare (special pathExprs))
    (generate-path-expression-locations exprlist)
    (dolist (i pathExprs)
      (setf newCode 0)
      (setf curExpression (first i))
      (setf end (position curExpression exprlist))
      (setf start (position  (second (first (last i))) exprlist))
      (setf curEnd end)
      (setf direction nil)
      (dolist (curSelection (rest i))
        (setf curPhrase (second curselection))
        (setf curWhere (third curselection))
        (setf newCode (create-selection-code curPhrase
                                             (and curWhere
                                                  (subseq exprlist 
                                                          (1+ (position curWhere exprlist))
                                                          curEnd))
                                             (and direction (sk8-token-val direction))
                                             newcode
                                             ))
        (setf direction (first curselection))
        (when direction
          (setf curEnd (position direction exprlist))
          (setf newcode (multi-function-call-wrapper 'SK8::PositionInternal (sk8-token-val newcode) nil))
          ))
      (setf newcode (wrap-selection-code newcode curExpression))
      (setf (nthcdr start exprlist) (cons newcode (nthcdr (1+ end) exprlist)))
      )
    (rest exprlist)))

;;;-------------------------------------------------------------------------------

(defun do-parse-sk8-expression (exprList)
  (when exprList
    (let (res)
      (setf res (preprocess-pair-values exprList))
      (setf res (preprocess-path-expressions res))
      (setf res (preprocess-pair-values res))
      (setf res (preprocess-apostrophe-statements res))
      (setf res (preprocess-pair-values res))
      (setf res (preprocess-of-statements res))
      (setf res (preprocess-pair-values res))
      (setf res (expression-process-parentheses res))
      (setf res (expression-process-lists res))
      (setf res (expression-process-apostrophe-statements res))
      (setf res (expression-process-function-calls res))
      (setf res (expression-process-path-expressions res))
      (setf res (expression-process-operators res))
      res)))

(defun parse-sk8-expression (exprList)
  (when exprList
    (let (res)
      (setf res (preprocess-the-type-coercion exprList))
      (setf res (remove-unneeded-tokens res))
      (setf res (preprocess-special-collection-functions res))
      (setf res (do-parse-sk8-expression res))
      (setf res (strip-expression-tokens res))
      (if (= (length res) 1)  ;;for single tokens...like "get 1" or "set x to blue"
        (first res)
        res
        ))))

(defun post-process-path-expressions (exprlist)
  (let (curitem curVal)
    (dolist (i exprlist)
      (setf curitem (if (sk8-token-p i) (deepest-token i) nil))
      (when (and (sk8-token-p curitem) (memq (sk8-token-type curitem) '(SINGLE-SELECTION-EXPRESSION MULTI-SELECTION-EXPRESSION)))
        (setf curval (deep-copy-list (sk8-token-val curitem)))
        (unless in-set-compilation (setf (second (third (second curval))) nil))
        (set-sk8-token-val curitem `(script 
                                     (local (ssCurrentPathExpressionAction nil)
                                            (ssCurrentPathExpressionActionType nil))
                                     ,curval))))
    exprlist))

(defun strip-expression-token (x)
  (let ((curval (and (sk8-token-p x) (sk8-token-val x))))
    (cond
     ;;; it's not a structure so we just pass it on through
     ((null curval)
      (if (sk8-token-p x) nil x))
     ;;; it's a structure with a sub expression so we have to be careful...
     ((listp curval)
      (cond
       ;;if it's a single item that's a structure strip it
       ((and (= (length curval) 1) (sk8-token-p (first curval)))
        (strip-expression-token (first curval)))
       ;;if it's first item is stripped then recurse
       ((not (sk8-token-p (first curval)))
        (strip-expression-tokens curval))
       ;;otherwise we have an error...
       (t
        ;(strip-expression-tokens curval) 
        (parse-error "Could not parse starting here." x)
        )))
     ;;;it's a structure pointing at a structure so we just recurse
     ((sk8-token-p curval)
      (strip-expression-token curval))
     ;;;otherwise it was a structure pointing at a basic value.  
     (t
      (parse-non-function-identifier x)))))

(defun strip-expression-tokens (exprList)
  (mapcar #'strip-expression-token (post-process-path-expressions exprlist)))

;;;-------------------------------------------------------------------------------

(defun run-it (str)
  (time 
   (let ((res (fifth (sk8::translatescriptCommandorexpr sk8::sk8 str))))
     (if (eq (first res) 'error)
       res
       (sk8::evaluatescripttranslation res)))))

(defun test-it (str expectedRes &optional (time-it nil))
  (let ((res (if time-it
               (run-it str)
               (let ((res (fifth (sk8::translatescriptCommandorexpr sk8::sk8 str))))
                 (if (eq (first res) 'error)
                   res
                   (sk8::evaluatescripttranslation res))))))
    (unless (equalp res expectedRes)
      (error (error (format nil "ERROR WHILE TESTING ~s ~% GOT ~s ~% EXPECTED ~s." str res expectedRes))))
    t)
  )


(defun try-it-out (str &optional noGet)
  (let (res)
    (unless noget
      (setf str (concatenate 'string  "get "str)))
    (setf res (fifth (time (sk8::translatescriptCommandorexpr sk8::sk8 str))))
    (if (eq (first res) 'error)
      res
      (second res))))


(defun skil-it (str)
  (let ((*compile-with-debugging* nil))
    (pprint (second (fifth (sk8::translatescriptcommandorexpr (sk8::targetproject sk8::ui)
                                                              str
                                                              ))))))
(defun lisp-it (str)
  (let ((*compile-with-debugging* nil))
    (pprint (skil-compile-to-lisp 
             (second (fifth (sk8::translatescriptcommandorexpr (sk8::targetproject sk8::ui) 
                                                               str
                                                               ))))
            )))


#|

(pprint 
 (skil-compile-form (second (fifth (sk8::translatescriptcommandorexpr sk8::sk8 
                                                     "position of 3 in uiselection"
                                                     )))))

(pprint (second (fifth (sk8::translatescriptcommandorexpr sk8::sk8 
                                                     "remove \"isaac\" from uiselection"
                                                     ))))
(time (progn (translatescriptcommandorexpr sk8::foo "position of item 1 that = 2 in the first item in first thru second list where first number in it = 2 and second integer in it = 3 in {{oval, 2,3},{4,5},{2,3,4}}"
                                    ) t))
(time (sk8::evaluatescripttranslation (fifth (translatescriptcommandorexpr sk8::foo "position of item 1 that = 2 in the first item in first thru second list where first number in it = 2 and second integer in it = 3 in {{oval, 2,3},{4,5},{2,3,4}}"
                                    ))))
|#







#|
	Change History (most recent last):
	3  	 4/ 3/96	Brian   	
	4  	 4/ 3/96	Brian   	Replacing collection mapping functions.
	8  	 4/11/96	Brian   	Array Access.
	9  	 4/12/96	Brian   	making text work nicer.
	10 	 4/15/96	Brian   	Making everything happen in the SKIL package.
	11 	 4/17/96	Brian   	fixed collection-resetter.  various skil package fixes.
						now use backquotes.
	12 	 4/19/96	Brian   	
	13 	 4/22/96	Brian   	
	14 	 4/26/96	Brian   	fixing parsing of  "x and not y
	15 	 4/29/96	Brian   	
	16 	 4/30/96	Brian   	handling operators which invert args or which are
						the logical opposite of other operators.
	17 	 4/30/96	Brian   	
	18 	 5/ 2/96	Brian   	
	19 	 5/ 3/96	Brian   	
	20 	 5/ 6/96	Brian   	Removing dashes from SKIL locals.
	21 	 5/ 7/96	Brian   	
	22 	 5/ 9/96	Brian   	
	23 	 5/10/96	Brian   	
	24 	 5/13/96	Brian   	Now we check for reserved words in statements.
	25 	 7/15/96	Brian   	added stuff for alpha renaming.
	26 	 7/18/96	Brian   	fixing item, remove, position and insert to take
						parenthesized expressions as the first argument.
	27 	 7/19/96	Brian   	
	28 	 7/19/96	Brian   	
	29 	 7/22/96	Brian   	
	30 	 7/25/96	Brian   	
	31 	 8/15/96	Brian   	
	32 	 9/ 9/96	Brian   	
	33 	 9/12/96	Brian   	
	34 	 9/20/96	Brian   	
	35 	 9/23/96	Brian   	
	36 	10/11/96	Brian   	Adding warnings.
	37 	10/11/96	Brian   	
	38 	10/14/96	Brian   	Fixing mutli-function-code to only wrap for
						set if it's an insert, remove, etc.
	39 	10/18/96	Brian   	
	40 	11/ 7/96	Brian Roddy	Fixing some parse errors to return proper token.
	41 	12/17/96	Brian Roddy	Adding nicer error message for functions without
						the parentheses but with a with.
	42 	12/17/96	Brian Roddy	
	43 	 1/27/97	Brian Roddy	Fixing warnings not to happen on positioninternal
						and removeInternal.
	44 	 1/27/97	Brian Roddy	
	45 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
