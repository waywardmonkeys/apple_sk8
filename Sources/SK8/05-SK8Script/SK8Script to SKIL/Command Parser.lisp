;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :skil)

;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;The SK8Script Parser!
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------

;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;Here we parse simple Expressions. 
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------


(defun parse-expression-list (delimiter ScanTo)
  (let (curExpr 
        exprList 
        (endingTokens (append (list delimiter) scanTo)))
    (loop
      (if (token-equal (peek-token-symbol) 'END-OF-STREAM) 
        (error "Unexpected end of input"))
      (if (memq (peek-token-symbol) scanTo) (return))
      (setf curExpr (sk8compile-expression endingTokens nil))
      (setf exprList (nconc exprList (list curExpr)))
      (if (token-equal (peek-token-symbol) delimiter) (consume-token))
      )
    exprList
    ))

;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;Now the Command parsing begins:
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------


;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;Command Lists

#|
 Command-list
      <Command-list> -> 
           <Command> 
           <Command>  <Command-list>
           NIL
|#


(defun Command-list (ScanTo consumeLast?)
  (unless (memq 'EOL ScanTo)
    (ccl::while (eq (peek-token-symbol) 'EOL) (consume-token)))
  ;;;Should make replace the items in the member function below.
  (if (memq (peek-token-symbol) ScanTo)
    (progn
      (if consumeLast? (consume-token))
      nil)
    (cons (compile-sk8-command) (Command-list ScanTo consumeLast?))))

;;(trace Command-list)

;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;;Get and Set Commands

#|
< get command > ->
GET <Command> EOL

< set command > ->
SET <set location><Command> EOL   

< set location > ->
<variable>
<function call>

(try-it-out "set item 1 in uiselection to 10" t)

|#

(defun compile-expression-or-do-inherited ()
  (if (eq (peek-token-symbol) 'do-inherited)
    (progn
      (consume-token)
      (do-inherited-command))
    (sk8compile-expression '(EOL) t)))


(defun get-Command ()
  `(set (lib :sk8 object sk8::SK8ScriptResult) to ,(compile-expression-or-do-inherited)))

(defun deep-copy-list (theList)
  (if theList
    (cons (if (listp (first theList)) (deep-copy-list (first theList)) (first theList))
          (deep-copy-list (rest theList)))
    nil))

(defun path-expression-set-command (loc val has-it)
  (let (doCommand)
    (setf loc (deep-copy-list loc))
    (if (second (second (second loc)))
      (setf doCommand `(script 
                        (local (ssArgToBeFilledIn ssCurrentItem))
                        (set ,(third (second (second (second loc)))) to ssValToSetTo)
                        ssCurrentCollection))
      (progn
        (setf doCommand '(script
                          ((lib :sk8 function setElementAtVisitState)
                           ssCurrentCollection ssCurrentState ssValToSetTo)
                          ssCurrentCollection))
        (let ((curloc (third loc)))
          (loop
            (when (or (null curloc) (not (listp curloc))) (print "warning!!!") (print loc))
            (when (eq (first (second (second curloc))) 'ssCurrentPathExpressionCollection)
              (setf (third curloc) (wrap-for-set (third curloc)))
              (return))
            (setf curloc (third curloc))))
        )
      )
    
    (if has-it
      (progn
        (setf (second (second (second loc)))
              `(script 
                (input ssCurrentCollection ssCurrentItem ssCurrentState ssOurCount)
                (local (it ssCurrentItem) (ssValToSetTo ,val))
                ,doCommand
                ssCurrentCollection
                ))
        (setf (second (third (second loc)))
              '(quote set))
        loc)
      (progn
        (setf (second (second (second loc)))
              `(script 
                (input ssCurrentCollection ssCurrentItem ssCurrentState ssOurCount)
                ,doCommand
                ssCurrentCollection
                ))
        (setf (second (third (second loc)))
              '(quote set))
        `(script
          (local (ssValToSetTo ,val))
          ,loc
          )
        ))
    ))



#|  MORE EFFICIENT AND DOESN'T RELY ON NEW PRIMITIVE, BUT UGLIER TO READ.
(defun destructuring-set-command (loc val)
  (let (set-code)
    (dolist (i (rest loc))
      (setf set-code (append set-code 
                             `((set ,i to ((LIB :sk8 FUNCTION elementAtVisitState) ssCurrentCollection ssCurrentState))
                               (set ssCurrentState to ((LIB :sk8 FUNCTION succeedingvisitstate) ssCurrentCollection ssCurrentState)))))
      )
    (setf (nthcdr (1- (length set-code)) set-code) nil)
    `(script
      (local (ssCurrentCollection ,val) ssCurrentState)
      (set ssCurrentState to 
           ((LIB :sk8 FUNCTION initialVisitState) ssCurrentCollection))
      ,@set-code
      t)
    ))
|#
(defparameter current-local-count 0)
(defun generate-symbol-for-local-collection-var (val)
  (if generate-readable-code
    (progn
      (incf current-local-count)
      (if (and (listp val) (listp (first val)) (eq (first (first val)) 'lib)) 
        (intern-symbol (format nil "Collection_From_~a~a" (symbol-name (fourth (first val))) current-local-count) :skil)
        (intern-symbol (symbol-name  (gensym "destructure_collection")) :skil)
        ))
    'ssCurrentCollection))

(defun destructuring-set-command (loc val)
  (let ((set-code (list)) 
        (curcnt 1)
        varName usingvalasvarname)
    (if (or (symbolp val)
            (and (listp val)
                 (eq (first val) 'lib)))
      (progn
        (setf varname val)
        (setf usingvalasvarname t))
      (progn
        (setf varname (generate-symbol-for-local-collection-var val))
        (setf usingvalasvarname nil)))
    (dolist (i (rest loc))
      (setf set-code (append set-code 
                             `((set ,i to ((LIB :sk8 FUNCTION collectionnth) ,varName ,curcnt)))))
      (incf curcnt)
      )
    `(script
        ,@(if usingvalasvarname '() `((local (,varName ,val ,(if (eq (get-skil-form-type val) 'sk8::floatlist) nil '(lib :sk8 OBJECT collection) )))))  ;;
        ,@set-code)
    ))


(defun it-before-eol ()
  (let ((curitem input-tokens)
        res)
    (loop
      (if (or (null curitem) (eq (sk8-token-val (first curitem)) 'EOL)) (return))
      (when (eq (sk8-token-val (first curitem)) 'IT)
        (setf res t)
        (return))
      (setf curitem (rest curitem)))
    res))

(defparameter in-set-compilation nil)

(defun set-Command ()
  (let* ((curloc (peek-token))
         loc
         has-it
         val
         it-stuff)
    (if (and (eq (sk8-token-val (nth 1 input-tokens)) '|(|)
             (eq (sk8-token-val (nth 2 input-tokens)) 'a))
      (progn
        (setf loc (parse-vars-definition))
        (create-vars nil nil (list (first loc)))
        (consume-token)
        (setf loc (first (first loc))))
      (setf loc (let ((in-set-compilation t)) (sk8compile-expression '(TO) t))))
    (setf has-it (it-before-eol))
    (setf val (compile-expression-or-do-inherited))

    (cond 
     ((or (atom loc) (eq (first loc) 'lib))
      `(set ,loc to ,val))  ;;single val...
     ((or (symbolp (first loc)) (and (listp (first loc)) (eq (caar loc) 'lib)))
      (cond
       ;;error checking...cant set an expression with an operator..
       ((memq (first loc) operator-list)
        (parse-error "Can't only set a location which is a variable, property, or path expression." curloc))
       ;;path expression either set fillcolor of all of stage to red, set all of collection to 1
       ((eq (first loc) 'script)
        (unless (second (second loc)) 
          (parse-error "Can't only set a location which is a variable, property, or path expression." curloc))
        (path-expression-set-command loc val has-it)
        )
       ;;;destructuring bind..
       ((eq (fourth (first loc)) 'list)
        (unless (every #'(lambda (x)
                          (or (symbolp x)
                              (and (listp x) (eq (first x) 'lib))
                              (and (listp x) (listp (first x)) (eq (first (first x)) 'lib))
                              )) (rest loc))
          (parse-error "Destructuring sets require a list of variables." curloc))
        (unless (second loc) 
          (parse-error "Destructuring sets need at least one argument!" curloc))
        (destructuring-set-command loc val)
        )
       ;;function call... 
       ((and (listp loc) (neq (first loc) 'lib))
        (if has-it
          (progn
            (setf it-stuff (second loc))
            (setf (second loc) 'IT)
            `(script 
              (local (it ,it-stuff))
              (set ,loc to ,val)))
          `(set ,loc to ,val))
        )
       ;;otherwise a simple variable
       (t
        `(set ,loc to ,val)))) 
     (t 
      (parse-error "Can't only set a location which is a variable, property, or path expression." curloc)))
    ))

;;(pprint (run-it "set {x,y} to size of messagebox"))

;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;; unless Commands

#|
<unless command> ->
                 UNLESS <expr> DO  <single line command> EOL
                 UNLESS <expr> DO-EOL  <commands> END-UNLESS-EOL
|#      

(defun do-tail () 
  (case (consume-token-symbol)
    ((DO) 
     `(,(compile-SK8-Command)))
    ((DO-EOL) 
     `((script ,@(Command-list '(END-UNLESS-EOL) t))))))
(defun unless-Command () 
  (let ((expr (sk8compile-expression '(DO DO-EOL) nil))) 
    `(if (not ,expr) then ,@(do-tail))))



;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;; if then Commands

#|
  If Command
      (<if Command> ->   
           <expr> <then-tail>
           <expr> <IS ONE OF> <one-of-element>
           <ONE OF> <one-of-element> )
      
      (<then-tail> -> 
                   <THEN> <Command>
                   <THEN EOL> <Command-list> <EOL END IF>
                   <THEN EOL> <Command-list> <else-tail>)
                                                 
      (<else-tail> -> 
                   <EOL ELSE EOL> <Command-list> <EOL END IF>
                   <EOL ELSE> <Command>                                   
                   <EOL ELSE IF> <Command> <then-tail>))
 |#      

(defun else-tail ()
  (let ((elseToken (consume-token-symbol)) tmp expr)
    (case elseToken 
      ((ELSE) 
       `(else ,(compile-sk8-command)))
      ((ELSE-EOL) 
       (setf tmp (Command-list '(END-IF-EOL) t))
       `(else ,@tmp))
      ((ELSE-IF) 
       (cond
        ((eq (peek-token-symbol) 'ONE-OF)
         (consume-token-symbol)
         `(else ,(if-one-of-command)))
        ((one-of-on-this-line)
         `(else ,(is-one-of-eol-command)))
        (t
         (setf expr (sk8compile-expression '(THEN THEN-EOL) nil))
         `(else-if ,expr ,@(then-tail)))))
      (t
       (parse-error "Expected an Else or an Else If"))
      )))


(defun then-tail () 
  (let ((tok (consume-token-symbol)))
    (case tok
      ((THEN) 
       `(then ,(compile-SK8-Command)))
      ((THEN-EOL) 
       (let ((cl (Command-list '(END-IF-EOL ELSE ELSE-EOL ELSE-IF) nil)))
         (case (peek-token-symbol)
           ((END-IF-EOL) (consume-token) `(then (script ,@cl)))
           ((ELSE ELSE-EOL ELSE-IF)  
            `(then (script ,@cl) ,@(else-tail)))
           (otherwise
            (parse-error "Expected an else or an end if")))
         ))
      (otherwise
       (parse-error "Expected a Then")
       ))))

(defun if-Command () 
  (if (one-of-on-this-line)
    (is-one-of-eol-command)
    (let ((expr (sk8compile-expression '(THEN THEN-EOL) nil))) 
      `(if ,expr ,@(then-tail)))))



;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;; if is one of Commands

#|   NEEDSBE UPDATED AND FIXEDNEW COMPILE Command PROTOCOL

'(if-is-one-of -> <IF> <Command> <IS ONE OF> < one-of-elements>
  <one-of-elements> ->  <test> COLON <element-body>
  <element-body> -> <element> <element-body-tail>
  <element-body-tail> -> <else-tail>
  <element> <one-of-elements>
  <element> <element-body-tail> EOL END IF)

|#

(defun one-of-on-this-line ()
  (let ((ourTokenList input-tokens) 
        (res nil)
        (cnt 0)
        cursym)
    (loop
      (setf cursym (transform-to-symbol (first ourtokenlist)))
      (cond 
       ((and (symbolp cursym)
             (or (memq cursym '(EOL END-OF-STREAM))
                 (and (> (length (symbol-name cursym)) 4)
                      (string= (subseq (symbol-name cursym) (- (length (symbol-name cursym)) 4)) "-EOL"))))
         (setf res nil)
         (return))
        ((eq cursym 'ONE-OF) 
         (setf res cnt)
         (return)))
      (setf ourtokenlist (rest ourtokenlist))
      (incf cnt)
      (if (not ourTokenList) (return)))
    res))

(defun colon-on-next-line ()
  (let ((ourTokenList input-tokens) (res nil) cursym)
    (loop
      (setf cursym (transform-to-symbol (first ourtokenlist)))
      (when (symbolp cursym)
        (cond 
         ((or (memq cursym '(EOL END-OF-STREAM))
              (and (> (length (symbol-name cursym)) 4)
                   (string= (subseq (symbol-name cursym) (- (length (symbol-name cursym)) 4)) "-EOL")))
          (setf res nil)
          (return))
         ((eq cursym '|:|) 
          (setf res t)
          (return))))
      (setf ourtokenlist (rest ourtokenlist))
      (if (not ourTokenList) (return)))
    res))

(defun is-next-line-a-new-test ()
  (or (memq (peek-token-symbol) '(ELSE ELSE-IF ELSE-EOL END-IF-EOL))
      (colon-on-next-line)))

(defun if-one-of-command-list ()
  (let (commands)
    (loop
      (if (is-next-line-a-new-test) (return))
      (setf commands (nconc commands (list (compile-sk8-command)))))
    commands))

(defun one-of-items (usePlainIf? parseAsISOneOf?)
  (let ((testExpr (list 'or))
        commands)
    (case (peek-token-symbol)
      ((END-IF-EOL)
       (consume-token)
       nil)
      ((ELSE ELSE-EOL ELSE-IF)
       (else-tail))
      (t 
       (ccl::while (eq (peek-token-symbol) 'EOL) (consume-token))  ;;eat extra end of lines...
       (loop 
         (when parseAsISOneOf?
           (setf input-tokens 
                 (tokens->multi-tokens
                  (cons (make-sk8-token 'ssOurTestItem -1 -1)
                        (cons (make-sk8-token (sk8-token-val parseAsISOneOf?) -1 -1) input-tokens)))))
         (setf testexpr (append testexpr (list (sk8compile-expression '(|:| |;|) nil))))
         (if (eq (peek-token-symbol) '|:|) 
           (progn
             (consume-token)
             (return))
           (consume-token))
         )
       (setf commands (if-one-of-command-list))
       (if (= (length testExpr) 2) (setf testExpr (second testExpr))) ;;don't need the OR
       `(,(if usePlainIf? 'if 'else-if) ,testExpr then 
         ,@commands ,@(one-of-items nil parseAsISOneOf?))
       ))))

(defun if-one-of-command ()
  (one-of-items t nil))

(defun is-one-of-EOL-command ()
  (let ((one-of-loc (one-of-on-this-line))
        opr expr)
    (setf opr (nth (1- one-of-loc) input-tokens))
    (setf (nthcdr (1- one-of-loc) input-tokens)
          (nthcdr one-of-loc input-tokens))
    (if (eq (sk8-token-val opr) '|'S|) (set-sk8-token-val opr 'IS))
    (setf expr (sk8compile-expression '(ONE-OF) t))
    `(script (local (ssOurTestItem ,expr)) ,(one-of-items t opr))))


;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;;  Wait commands       

#|

< wait command > ->
                 WAIT FOR <expr> <timeUnit> <wait tail>   ;;;wait for 10 seconds
                 WAIT <expr> <timeUnit> <wait tail>       ;;;wait xx + 1 minutes
                 WAIT WHILE <expr> <wait tail>
                 WAIT UNTIL <expr> <wait tail>
                                         
<wait tail> -> [<WITH> <EVENTS> | <WITHOUT EVENTS>]                                     
                                   
|#


(defun time-tail-expression ()
  (let  ((timeExpr (sk8compile-expression '(SK8::MINUTE SK8::MINUTES
                                            SK8::SECOND SK8::SECONDS
                                            SK8::TICK SK8::TICKS
                                            SK8::MILLISECOND SK8::MILLISECONDS)
                                          nil)))
    `((LIB :SK8 FUNCTION sk8::wait-time-period) ,(consume-token-symbol) ,timeExpr)
    ))


(defun wait-Command ()
  (let (curResult eventsToken (with? t))
    (when (memq (peek-token-symbol) '(WITH WITHOUT))
      (setf with? (eq (consume-token-symbol) 'WITH))
      (setf eventsToken (consume-token))
      (unless (token-equal eventsToken 'EVENTS)
        (parse-error  "Error: events is the only valid keyword" eventsToken))
      )
    (case (peek-token-symbol)
      ((WHILE)
       (consume-token)
       (setf curResult `((LIB :SK8 FUNCTION sk8::wait-for-condition) 
                         ,(sk8compile-expression '(EOL) nil))))
      ((UNTIL)
       (consume-token)
       (setf curResult `((LIB :SK8 FUNCTION sk8::wait-for-condition) 
                         (not ,(sk8compile-expression '(EOL) nil)))))
      ((FOR)
       (consume-token)
       (setf curResult (time-tail-expression)))
      (t
       (setf curResult (time-tail-expression))))
    (setf curResult (append curResult (list 'with 'events with?)))
    curResult
    ))



;;;------------------------------------------------------------------------------------
;;;;  Repeat commands       


#|
'(
  <repeat command> ->
  REPEAT <repeatTest> <commands> END-REPEAT-EOL
  REPEAT-EOL <commands> <repeatTest>
  
  <repeat test> -> 
  FOREVER-EOL
  FOR <expr> TIMES EOL
  UNTIL <expr> EOL
  WHILE <expr> EOL
  WITH <var> IN <expr> EOL
  WITH <var> FROM <expr><expr> [BY <expr>] EOL  ;;;from 1100 by 5
  
  )

|#


(defun generate-repeat (test testAtEnd? exprs)
  (let (var fromExpr toExpr byExpr byVar positivebyvar? listexpr)
    (case (first test)
      (WHILE
        (if testAtEnd?
          `(catch (quote EXIT-REPEAT)
             (loop 
               (catch (quote NEXT-REPEAT) (script 
                                           ,@exprs
                                           ))
               (if (not ,@(rest test)) then (throw (quote EXIT-REPEAT) nil))))
          `(catch (quote EXIT-REPEAT)
             (loop 
               (if (not ,@(rest test)) then (throw (quote EXIT-REPEAT) nil))
               (catch (quote NEXT-REPEAT) (script 
                                           ,@exprs
                                           ))))))
      (TIMES
       (setf var 'ssCurrentCount)
       (setf toExpr (nth 1 test))
       `(catch (quote EXIT-REPEAT)
          (script
           (input )
           (local (,var 1 (lib :sk8 object sk8::number)))
           (loop
             (if (> ,var ,toExpr) then (throw (quote EXIT-REPEAT) nil))
             (catch (quote NEXT-REPEAT) (script ,@exprs))
             (set ,var to (+ ,var 1))
             ))))
      (IN
       (setf var (nth 1 test))
       (setf listexpr (nth 2 test))
       `(catch (quote EXIT-REPEAT)
          (script
           (input )
           (local (ssCurrentCollection ,listexpr (lib :sk8 object sk8::collection))
                  (ssCurrentState ((LIB :sk8 FUNCTION initialVisitState) ssCurrentCollection) (lib :sk8 OBJECT sk8::VisitState))
                  ,var)
           (if ssCurrentState then
               (loop 
                 (set ,(if (listp var) (first var) var) to ((LIB :sk8 FUNCTION elementAtVisitState) ssCurrentCollection ssCurrentState))
                 (catch (quote NEXT-REPEAT) (script 
                                             ,@exprs
                                             ))
                 (if ((LIB :sk8 FUNCTION SK8::isFinalVisitState) ssCurrentCollection ssCurrentState) then (throw (quote EXIT-REPEAT) nil))
                 (set ssCurrentState to ((LIB :sk8 FUNCTION succeedingvisitstate) ssCurrentCollection ssCurrentState))
                 ))))
       )
      (FROM
       (setf var (nth 1 test))
       (setf fromExpr (nth 2 test))
       (setf toExpr (nth 3 test))
       (setf byExpr (nth 4 test))
       (setf byVar 'ssbyVariable)
       (setf positivebyvar? 'ssisPositive)
       (if testAtEnd? 
         ;;This version tests at the end of the loop, so its body will always execute at least once
         `(catch (quote EXIT-REPEAT)
            (script
             (input )
             (local (,var ,fromExpr (lib :sk8 object sk8::integer)) 
                    (,byVar ,byExpr (lib :sk8 object sk8::integer))
                    (,positivebyvar? (> ,byvar 0) (lib :sk8 object sk8::boolean)))
             (loop
               (catch (quote NEXT-REPEAT) (script 
                                           ,@exprs)) 
               (set ,var to (+ ,var ,byVar))
               (if (or (and ,positivebyvar? (> ,var ,toexpr)) (and (not ,positivebyvar?) (< ,var ,toexpr))) 
                 then
                 (throw (quote EXIT-REPEAT) nil))
               )))
         ;;This version tests at the start of the loop, so its body may never execute
         `(catch (quote EXIT-REPEAT)
            (script
             (input )
             (local (,var ,fromExpr (lib :sk8 object sk8::integer)) 
                    (,byVar ,byExpr (lib :sk8 object sk8::integer))
                    (,positivebyvar? (> ,byvar 0) (lib :sk8 object sk8::boolean)))
             (loop
               (if (or (and ,positivebyvar? (> ,var ,toexpr))
                       (and (not ,positivebyvar?) (< ,var ,toexpr)))
                 then
                 (throw (quote EXIT-REPEAT) nil))
               (catch (quote NEXT-REPEAT) (script    
                                           ,@exprs 
                                           ))
               (set ,var to (+ ,var ,byVar))
               )))
         ))
      (t 
       (parse-error "Error while generating repeat loop.  Invalid test."))
      )
    ))


(defun check-repeat-local-variables (repeatTest oldlocals)
  (when (and (listp repeatTest) (memq (first repeatTest) '(IN FROM)))
    (when (member (second repeatTest) oldlocals :key #'first)
      (parse-error (format nil 
                           "Repeat variable ~a is already used elsewhere in the handler as a local variable.  Try a different name"
                           (second repeatTest))))
    (setf handler-locals (remove (if (listp (second repeatTest)) 
                                   (first (second repeattest))
                                   (second repeattest))
                                 handler-locals :key #'first))
    )
  )

(defun repeat-Command ()
  (let  (repeatTest exprs oldlocals)
    (setf oldlocals (copy-list handler-locals))
    (setf repeatTest (compile-repeat-test))
    (confirm-and-consume-token-symbol 'EOL)
    (setf exprs (Command-list '(END-REPEAT-EOL) t))
    (check-repeat-local-variables repeatTest oldlocals)
    (generate-Repeat repeatTest nil exprs)
    ))

(defun repeat-EOL-Command ()
  (let  (repeatTest exprs oldlocals)
    (setf oldlocals (copy-list handler-locals))
    (setf exprs (Command-list '(WHILE UNTIL FOREVER END-REPEAT-EOL) nil))
    (when (eq (peek-token-symbol) 'END-REPEAT-EOL)
      (set-sk8-token-val (peek-token) 'EOL)
      (push (make-sk8-token 'FOREVER -1 -1) input-tokens))
    (setf repeatTest (compile-repeat-test))
    (confirm-and-consume-token-symbol 'EOL)
    (check-repeat-local-variables repeatTest oldlocals)
    (generate-Repeat repeatTest t exprs)
    ))

(defun compile-repeat-test ()
  (let ((tok (consume-token-symbol)) var fromOrIn fromExpr toExpr (byExpr 1) type)
    (case tok
      ((FOREVER) 
       '(WHILE T))
      ((FOR)
       `(TIMES ,(sk8compile-expression '(TIMES) t)))
      ((WHILE)
       `(WHILE ,(sk8compile-expression '(EOL) nil)))
      ((UNTIL)
       `(WHILE (not ,(sk8compile-expression '(EOL) nil))))
      ((WITH)
       (setf var (consume-token-symbol))
       (when (eq (peek-token-symbol) '|(|)
         (setf type (first (parse-type-declaration nil)))
         )
       (setf var (list var nil type))
       (push var handler-locals)
       (setf fromOrIn (consume-token-symbol))
       (case fromOrIn
         ((IN)
          `(IN ,var ,(sk8compile-expression '(EOL) nil))
          )
         ((FROM)
          (setf fromExpr (sk8compile-expression '(TO) t))
          (setf toExpr (sk8compile-expression '(BY EOL) nil))
          (when (token-equal (peek-token-symbol) 'BY)
            (consume-token)
            (setf byExpr (sk8compile-expression '(EOL) nil))
            )
          (setf var (if (listp var) (first var)))
          `(FROM ,var ,fromexpr ,toexpr ,byexpr)
          )
         (t
          (parse-error "Invalid WITH repeat test, expected a From or In")))
       )
      (t
       (parse-error "Expected Forever For While Until or With"))
      )))



;;;------------------------------------------------------------------------------------
;;;next repeat and end repeat


(defun next-repeat-EOL-Command ()
  '(throw (quote NEXT-REPEAT) nil))

(defun exit-repeat-EOL-Command ()
  '(throw (quote EXIT-REPEAT) nil))


;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;;  return commands       


(defun return-Command ()
  `(throw (quote CURRENT-FUNCTION) ,(compile-expression-or-do-inherited)))


;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;;  with commands    

#|

;;;only allowed in handler definitions

(<with command> ->
     WITH <With Handler Name> <functional args> EOL <command list> END-WITH-EOL
     )
|#

(defun with-command ()
  (let ((withname (consume-token-symbol)) 
        res commands
        withfunctionname)
    ;;we make the with name by squishing it together...
    (setf withfunctionname (make-with-command-symbol withname))
    ;;then we parse it as a function
    (unless (eq (peek-token-symbol) 'OF)
      (setf input-tokens (nconc (list (make-sk8-token 'OF -1 -1))
                                input-tokens)))
    
    (setf input-tokens (nconc (list (make-sk8-token withfunctionname -1 -1))
                              input-tokens))
    (set-sk8-token-val (first input-tokens) withfunctionname)
    (setf res (sk8compile-expression  '(EOL) nil))
    (setf commands (cons 'script (command-list '(END-WITH-EOL) t)))
    `(call-with-statement ,res ,commands)))


;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;;  do inherited commands    

#|

;;;only allowed in handler definitions

(<do inherited command> ->
     DO-INHERITED EOL
     DO-INHERITED <function args>
     DO-INHERITED <function args>expression  ;;;in setter handlers only
     )

(call-do-inherited (set foo) newval bing)
|#


(defparameter cur-set-val-var nil)

(defun do-inherited-command ()
  (let (res
        toToken
        (insetter (and (listp cur-handler-name) 
                       (eq (first cur-handler-name) 'set))))
    (declare (special setval))
    (if (not cur-handler-name)
      (parse-error "Do inherited can only occur in a handler or function definition."))
    (if (eq (peek-token-symbol) 'EOL)
      (progn
        (consume-token)       
        (setf res `(call-do-inherited ,insetter 
                                             ,(if inSetter 
                                                (second cur-handler-name)
                                                cur-handler-name)
                                             ,@(if inSetter 
                                                 `(,cur-set-val-var sk8::me)
                                                 '(sk8::me)))))
      (progn
        (unless (memq (peek-token-symbol) '(of |(|))
          (push (make-sk8-token 'OF -1 -1) input-tokens))
        (push (make-sk8-token 'call-do-inherited -1 -1) input-tokens)
        (setf res (sk8compile-expression  '(TO EOL) nil))
        (when (token-equal (setf toToken (consume-token)) 'TO)
          (unless insetter
            (parse-error "A TO argument can only occur in a Set Handler." toToken))
          (setf (nthcdr 1 res) (cons (sk8compile-expression  '(EOL) nil) (nthcdr 1 res))))
        (when (= (length res) 2)
          (setf (nthcdr 1 res) (cons setval (cons 'sk8::me (nthcdr 1 res)))))
        (setf (nthcdr 1 res) (cons insetter 
                                   (cons (if insetter 
                                           (second cur-handler-name) 
                                           cur-handler-name)
                                         (nthcdr 1 res))))))
    res
    )
  )




;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;;  global and local and constant commands    


(defun create-vars (isGlobal isConst theVars)
  (declare (ignore isConst))
  (if isGlobal  
    (setf handler-globals (nconc handler-globals (mapcar #'car theVars)))
    (setf handler-locals (nconc handler-locals (mapcar #'(lambda (x) (list (first x) nil (second x))) theVars)))
    )
  )

(defun generate-global-code (theVars)
  (let (res)
    (dolist (i thevars)
      (if generate-list-of-globals-p (push (if (listp i) (first i) i) current-special-global-list))
      (push `(declareGlobal ,i) res)
      )
    (cons 'script (nreverse res)))
  )

(defun generate-local-code (theVars)
  (let (res)
    (dolist (currentVar thevars)
      (when (and (listp currentVar) (third currentVar)) ;;it has a value to set it to..
        (setf res (nconc res (list (list 'set (first currentVar) 'to (third currentVar)))))
        ))
    ;; Now we clean up the code to make it nice skil..
    (when res 
      (if (rest res)
        (push 'script res)  ;;; more than one line needs a script in front..
        (setf res (first res)))) ;;; a single set so we just use that as our skil
    res)
  )


(defun parse-vars-definition (global?)
  (let (curToken curType curValue theVars)
    (loop
      (setf curType nil)
      (setf curValue nil)
      (if (token-equal (peek-token-symbol) 'END-OF-STREAM)
        (parse-error "Unexpected end of input"))
      (if (or (token-equal (peek-token-symbol) 'EOL)
              (token-equal (peek-token-symbol) 'TO))
        (return))
      (setf curtoken (consume-token-symbol))
      (when (token-equal (peek-token-symbol) '|(|)
        (setf curType (parse-type-declaration t))
        (setf curValue (second curtype))
        (setf curType (first curtype)))
      (when (token-equal (peek-token-symbol) '=) 
        (if (and global? cur-handler-name) 
          (parse-error "Cannot declare a global or specify a new value for a global inside of a handler."))
        (consume-token)
        (setf curValue (sk8compile-expression '(EOL) nil))
        )
      (case (peek-token-symbol)
        ((|,|) (consume-token))
        ((EOL TO) nil)
        (t (parse-error "Expected a comma or an end-of-line")))
      (setf theVars (nconc theVars (list (list curToken curType curValue)))))
    theVars))

(defun global-local-Command ()
  (let (global? constant? vars)
    (setf global? (token-equal (consume-token-symbol) 'GLOBAL))
    (when (token-equal (peek-token-symbol) 'CONSTANT) 
      (setf constant? t)
      (consume-token))
    (setf vars (parse-vars-definition global?))
    (confirm-and-consume-token-symbol 'EOL)
    (create-vars global? constant? vars)
    (if (and global? (not cur-handler-name))
      (generate-global-code vars)
      (generate-local-code vars))))

;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;;  Handler Definitions commands       



#|
;;how about optional types like in the setter value varname
;;how about marking handlers private?

 <type declaration> ->
        ( A <type> )
        ( A <type> DEFAULTING-TO <expr> )

<handler def> ->
                ON <validHandlerNameToken> <handlerArgSpecification> EOL 
                     <commands> END <same validHandlerNameToken> EOL
                ON <validFunctionNameToken> <functionArgSpecification> EOL 
                     <commands> END <same <validFunctionNameToken> EOL
                ON SET <validHandlerNameToken> <handlerArgSpecification>
                     <varname> EOL <commands> END-SET <same validHandlerNameToken> EOL
                ON WITH <validHandlerNameToken> <handlerArgSpecification> EOL 
                     <commands> CLEANUP-:-EOL <commands> END-WITH <same validHandlerNameToken> EOL


<handlerArgSpecification> ->
                OF ME <additionalargsdef> [<with args decl>]* EOL
                OF ME ( [A AN] <expr>) <additionalargsdef> [<with args decl>]* EOL


<functionArgSpecification> ->
                ( <argsdef> ) [<with args decl>]* EOL

|#

(defun find-next-right-paren ()
  (let ((cnt -1)
        (depth 1)
        curitem)
    (ccl::while (> depth 0)
      (incf cnt)
      (loop
        (setf curitem (nth cnt input-tokens))
        (unless curitem (parse-error "Can't find end of type declaration"))
        (if (eq (sk8-token-val curitem) '|(|) (incf depth))
        (when (eq (sk8-token-val curitem) '|)|)
          (return))
        (incf cnt))
      (decf depth))
    cnt))

(defun parse-type-declaration (allow-defaulting-to)
  (let (type default)
    (when (token-equal (peek-token-symbol) '|(|)
      (consume-token-symbol)
      ;;;we can't just search for |)|, cause there could be matched parentheses in 
      ;;;the expression.  we need to find the next unmatched one..
      (set-sk8-token-val (nth (find-next-right-paren) input-tokens) 'END-OF-TYPE-DECL)
      (unless (token-equal (peek-token-symbol) 'DEFAULTING-TO) 
        (setf type (sk8compile-expression '(DEFAULTING-TO END-OF-TYPE-DECL) nil)))  
      (when (and allow-defaulting-to (token-equal (peek-token-symbol) 'DEFAULTING-TO)) 
        (consume-token)
        (setf default (sk8compile-expression '(END-OF-TYPE-DECL) nil)))
      (confirm-and-consume-token-symbol 'END-OF-TYPE-DECL)
      (list type (and allow-defaulting-to default)))))


(defun parse-args-definition ()
  (let (curArg curType allArgs)
    (loop
      (if (memq (peek-token-symbol) '(EOL TO WITH RETURNING)) (return))
      (setf curArg (consume-token-symbol))
      (setf curType (parse-type-declaration nil))
      (setf allArgs (nconc allargs (list (if curtype (list curarg (second curtype) (first curtype)) curarg))))
      (if (not (memq (peek-token-symbol) '(EOL TO WITH RETURNING)))
        (confirm-and-consume-token-symbol '|,|))
      )
    allArgs))

(defun parse-keyword-args-definition ()
  (let (keyargs curKey altKeyName defaultval)
    (loop
      (case (peek-token-symbol)
        ((EOL TO RETURNING)
         (return))
        ((WITH)
         (consume-token-symbol)
         (setf curkey (consume-token-symbol))
         (setf altKeyName (if (memq (peek-token-symbol) '(|(| WITH RETURNING TO EOL))
                            nil 
                            (consume-token-symbol)))
         (setf defaultval (cadr (parse-type-declaration t)))
         (setf keyargs (nconc keyargs (list (list curkey altkeyname defaultval))))
         )
        (t
         (parse-error "Expected an end of line, or a with"))
        ))
    (if keyargs
      `(with ,@keyargs)
      nil)
    ))

(defun remove-reqds (locs reqs)
  (setf reqs (cons 'sk8::me (mapcar #'(lambda (x) (if (listp x) (if (second x) (second x) (first x)) x)) reqs)))
  (cond
   ((null locs) 
    nil)
   ((memq (caar locs) reqs)
    (remove-reqds (rest locs) reqs))
   (t
    (cons (first locs) (remove-reqds (rest locs) reqs)))))



(defun add-function-code (handlername args returntype comms)
  (if *compile-with-debugging*
    `(script
      (name ,handlername)
      (input ,@args) 
      ,@handler-errors
      (return-type ,returntype) 
      (local sk8script_cur_result_var ,@(remove-reqds handler-locals args))
      (set sk8script_cur_result_var to ,@comms)
         (debugging-setup :done)    ;;;This is the additional step we need to do before finishing.  This way stepping can know it's done.
      sk8script_cur_result_var
      )
    (append
     `(script
       (name ,handlername)
       (return-type ,returntype) 
       (input ,@args) 
       (local ,@(remove-reqds handler-locals args))
       ,@handler-errors)
     comms))
  )

(defun add-handler-code (handlername args returntype comms)
  (setf (first args) (list 'sk8::me (first args)))
  (add-function-code handlername args returntype comms)
  )

(defparameter last-function-definition-line-number 0)
(defun on-Command ()
  (let (EndingLine
        (handlerName (consume-token-symbol)) 
        requiredArgs keywordArgs
        function?
        setter? SetVal
        with?
        theCode
        type
        (Comments (list 'Comments))
        returntype)
    (declare (special setval))
    (if cur-handler-name
      (parse-error "Can't define a handler within a handler"))    
    (case handlername
      ((SET)
       (setf setter? t)
       (setf handlername (list 'set (consume-token-symbol))))
      ((WITH)
       (setf with? t)
       (setf handlername (list 'with (consume-token-symbol)))
       ))
    (setf cur-handler-name handlerName)
    (setf handler-errors nil)
    (setf handler-globals nil)
    (setf handler-locals nil)
    (case (peek-token-symbol)
      ((EOL RETURNING) 
       (setf function? t)
       (setf requiredArgs nil)
       (setf keywordArgs nil)
       )
      ((WITH)
       (setf function? t)
       (setf requiredArgs nil)
       (setf keywordArgs (parse-keyword-args-definition))
       )
      (t
       (if (eq (peek-token-symbol) 'OF) (consume-token-symbol))
       (cond
        ((eq (peek-token-symbol) '|(|)
         (consume-token-symbol)
         (unless (eq (peek-token-symbol) '|)|)
           (parse-error "Sorry, you can only use parentheses in function definitions for specifying no required arguments, (e.g. \"noArgumentFuntion()\").  For specifying required arguments you must use the OF syntax, (e.g. \"functionName of xx, yy\")."))
         (consume-token-symbol)
         (setf function? t)
         (setf requiredArgs nil)
         (setf keywordArgs (parse-keyword-args-definition)))
        ((eq (peek-token-symbol) 'sk8::ME)
         (consume-token-symbol) ;;me
         (setf type (or (first (parse-type-declaration nil)) cur-handler-object))   ;;;note the global variable to specify a handler object in the with targetobject
         (unless type (parse-error "Cannot determine handler object for this handler"))
         (if (token-equal (peek-token-symbol) '|,|)
           (consume-token-symbol))
         (setf function? nil)
         (setf requiredArgs (cons type (parse-args-definition)))
         (setf keywordArgs (parse-keyword-args-definition))
         )
        ((or (memq (peek-token-symbol) *SK8Script-Tokens*)
             (eq (peek-token-symbol) '|)|)
             )
         (parse-error "Expected a valid first argument name"))
        (t
         (setf function? t)
         (setf requiredArgs (parse-args-definition))
         (setf keywordArgs (parse-keyword-args-definition))
         ))))

    ;;initialize handler-locals to be the input arguments..
    (if function?
      (setf handler-locals requiredArgs)
      (setf handler-locals (cons 'sk8::me (cdr requiredArgs))))
    (setf handler-locals (append (mapcar #'(lambda (x) (if (listp x) x (list x nil nil))) handler-locals)
                                 (mapcar #'(lambda (x) (if (listp x) (list (if (second x) (second x) (first x)) nil nil) (list x nil nil))) (cdr keywordargs)) ))

    (when setter?
      (if (or function? with?) (parse-error "Functions and With Handlers can not be setters."))
      (confirm-and-consume-token-symbol 'TO)
      (setf SetVal (parse-args-definition))
      (if (> (length setval) 1) (parse-error "Can only except one value after TO"))
      (setf SetVal (first SetVal))
      (setf cur-set-val-var (if (listp setVal) (first setVal) setVal))
      (push (list cur-set-val-var nil nil) handler-locals)
      )
    (if (and with? (not function?))
      (parse-error "With commands cannot be handlers"))
    (when (eq (peek-token-symbol) 'RETURNING)
      (consume-token-symbol)
      (setf returntype (sk8compile-expression '(EOL) nil))
      )
    (confirm-and-consume-token-symbol 'EOL)
    (cond 
     (with?
      (let ((*compile-with-debugging* nil))
        (setf theCode (append (command-list '(DO-BODY-EOL) t)
                              (list '|@body|)
                              (command-list '(CLEANUP-COLON-EOL) t)
                              (list (list 'cleanup-code
                                          (command-list '(END) t)))))
        (setf theCode
              (add-function-code handlername 
                                 (append requiredArgs keywordargs)
                                 returntype
                                 theCode)))
      )
     (function?
      (setf theCode
            (add-function-code handlername 
                               (append requiredArgs keywordargs)
                               returntype
                               `((catch (quote CURRENT-FUNCTION) 
                                   ,@(command-list '(END) t)
                                   (throw (quote CURRENT-FUNCTION) sk8::Undefined)))
                               )))
     (setter?
      (setf theCode
            (add-handler-code handlername 
                              (append requiredArgs keywordargs (list setval))
                              returntype
                              `((catch (quote CURRENT-FUNCTION) 
                                  ,@(command-list '(END) t)
                                  (throw (quote CURRENT-FUNCTION) sk8::Undefined)))))
      )
     (t
      (setf theCode
            (add-handler-code handlername 
                              (append requiredArgs keywordargs)
                              returntype
                              `((catch (quote CURRENT-FUNCTION) 
                                  ,@(command-list '(END) t)
                                  (throw (quote CURRENT-FUNCTION) sk8::Undefined))))))
     )
    (if setter? (confirm-and-consume-token-symbol 'SET))
    (if with? (confirm-and-consume-token-symbol 'WITH))
    (confirm-and-consume-token-symbol (if (listp handlerName) (second handlername) handlername))
    (setf endingLine (sk8-token-line-number (peek-token)))
    (confirm-and-consume-token-symbol 'EOL)
    (setf cur-handler-name nil)
   
    (when generate-readable-code
      (setf current-local-count 0)
      ;; cull up all comments in this handler
      (dolist (i comment-tokens)
        (when (and (>= (sk8-token-line-number i) last-function-definition-line-number)
                   (<= (sk8-token-line-number i) endingLine))
          (setf comment-tokens (nremove i comment-tokens))
          (setf comments (nconc comments (list (sk8-token-correspondingtoken i))))))
      (when (cdr comments)
        (setf (cdr theCode) (cons comments (cdr theCode)))))
    (setf last-function-definition-line-number endingLine)

    theCode
    ))

;;;------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------
;;;;  Signal Error commands    

#|
;;;NEEDDETERMINE SYNTAX MORE!!!!! ****   what should the invocation of this be On Signal Of Error?
;;;if so we needhave this code be invoked by on-command which checks if cur-handler-name is defined and first symbol is Signal

;;;only allowed in handler definitions

(<error def> ->
                ON-ERROR <validErrorObjectNameToken> <with argument definition> EOL <commands> END-SIGNAL-ERROR-EOL

|#


(defun on-error-Command ()
  (let (errName keywordArgs)
    (if (not cur-handler-name)
      (parse-error "Signal of Error can only occur in a handler or function definition."))
    (if (eq (peek-token-symbol) 'of) (consume-token-symbol))
    (if (eq (peek-token-symbol) 'EOL)
      (setf errName 'sk8::error)
      (setf errName (consume-token-symbol)))
    (unless (and (symbolp errname) (boundp errname))
      (parse-error "On Signal Error must be followed by an existing error object"))
    (setf keywordArgs (parse-keyword-args-definition))
    (confirm-and-consume-token-symbol 'EOL)
    ;;;fix me to skil!!!
    (setf handler-errors
          (append
           handler-errors
           `((on-error
             ,errName 
             (script (input ,@keywordArgs) ,@(command-list '(END-ERROR-EOL) t))
             ))))
    nil
    ))





;;;------------------------------------------------------------------------------------

;;;------------------------------------------------------------------------------------

;;;;This dispatchesthe appropriate parsing function keyed on the first token.
;;;;Note that if we don't recognize the first item, then we assume it is a function call
;;;;and tryparse it as such.



(defun compile-sk8-command ()
  (let ((curtoken (peek-token))
        res)
    (setf res
          ;;;Then we compile the command
          (case (peek-token-symbol)
            ((EOL) (consume-token-symbol) nil)
            ((IF) (consume-token-symbol) (if-Command))
            ((IF-ONE-OF-EOL) (consume-token-symbol) (if-one-of-command))
            ((UNLESS) (consume-token-symbol) (unless-Command))
            ((GET) (consume-token-symbol) (get-Command))
            ((SET) (consume-token-symbol) (set-Command))
            ((WAIT) (consume-token-symbol) (wait-Command))
            ((REPEAT) (consume-token-symbol) (repeat-Command))
            ((REPEAT-EOL) (consume-token-symbol) (repeat-EOL-Command))
            ((NEXT-REPEAT-EOL) (consume-token-symbol) (next-repeat-EOL-Command))
            ((EXIT-REPEAT-EOL) (consume-token-symbol) (exit-repeat-EOL-Command))
            ((RETURN) (consume-token-symbol) (return-Command))
            ((DO-INHERITED) (consume-token-symbol) (do-inherited-Command))
            ((WITH) (consume-token-symbol) (with-Command))
            ((GLOBAL LOCAL) (global-local-Command))
            ((ON) (consume-token-symbol) (on-Command))
            ((ON-ERROR) (consume-token-symbol) (on-error-Command))
            (t (sk8compile-expression  '(EOL) t))))
    (if (and *Compile-With-Debugging* 
             cur-handler-name
             (neq (sk8-token-val curtoken) 'EOL))
      `(script 
        (debugging-setup ,(sk8-token-line-number curtoken))
        ,res)
      res)
    ))

(defun compile-sk8-commands ()
  (cons 'script (Command-list '(END-OF-STREAM) nil)))



;;;----------------------------------------------------------
;;;----------------------------------------------------------
;;;----------------------------------------------------------


(defun setup-globals-for-compile (str)
  (let ((tokens (string->multi-token-list (concatenate 'string str (string #\newline)))))
    (setf input-tokens (first tokens))
    ;;Strip away initial blank space...up until the last eol
    (ccl::while (and (> (length input-tokens) 2)
                     (eq (peek-token-symbol) 'EOL))
      (consume-token))
    (setf comment-tokens (second tokens))
    (setf handler-globals nil)
    (setf handler-locals nil)
    (setf current-warning-list nil)
    (setf last-function-definition-line-number -1)
    (setf cur-handler-name nil)))


(defun SK8Script-Compile (str multipleCommands)
  (catch :PARSING-ERROR
    (progn
      (setup-globals-for-compile str)
      (if multipleCommands 
        (compile-sk8-commands)
        (compile-sk8-command)
        ))))

(defun SK8Script-Compile-and-Eval (str multipleCommands)
  (let ((code (SK8Script-Compile str multipleCommands)))
    (if (eq (first code) 'ERROR)
      code
      (skil-eval code))))




#|
	Change History (most recent last):
	3  	 4/ 3/96	Brian   	
	4  	 4/ 3/96	Brian   	new collection mappers.
	8  	 4/15/96	Brian   	Making everything happen in the SKIL package.
	9  	 4/17/96	Brian   	fixed up setters.  now use backquotes.
	14 	 4/29/96	Brian   	making globals to compile to declareGlobal.
	21 	 5/ 6/96	Brian   	Removing dashes from SKIL locals.
	22 	 5/20/96	Brian   	turning on debugging.
	24 	 5/21/96	Brian   	Adding some debugging things.
	26 	 7/ 8/96	Brian   	added type info for java output
	27 	 7/ 9/96	Brian   	dealing with local variables in repeat statements.
	28 	 7/15/96	Brian   	added stuff for alpha renaming.
	29 	 7/16/96	Brian   	Fixing problem with if-one-of-command-list .
	30 	 7/25/96	Brian   	Fixing:
						set {my x, my y} to my width
						repeat - end repeat as a substitute  repeat forever.
	31 	 8/ 5/96	Brian   	adding returning.
	32 	 8/ 8/96	Brian   	Changing declare global
	37 	 8/28/96	Brian   	adding comments
	38 	 8/28/96	Brian   	adding types to set handler to vars and to repeat vars.
	44 	10/ 7/96	Brian   	Fixing types in destructuring-set-command
	45 	10/ 8/96	Brian   	making return do inherited and set x to do inherited work.
	46 	10/10/96	Brian   	Fixing on error, and one-of-on-line.
	47 	10/10/96	Brian   	Making a handler with no known object to 
						define it on cause a parse error.
	48 	10/11/96	Brian   	Adding warnings.
	52 	10/18/96	Brian   	fixing spelling mistake
	54 	11/ 7/96	Brian Roddy	Fixing processing handler type's which are 
						expressions.
	57 	12/16/96	Brian Roddy	fixing the way local declarations are processed.
	60 	12/17/96	Brian Roddy	removing print statement
	61 	12/17/96	Brian Roddy	
	62 	 2/11/97	Brian Roddy	Allowing else-if's to be a else if x is one of and else if one of
	63 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
