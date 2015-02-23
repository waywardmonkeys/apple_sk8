;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  5-13-96   4:12 pm
                  SK8::HANDLEROBJECTTOHANDLERID)


;;; _______________________________ 
;;; _______________________________ 
;;; Debugging
;;; _______________________________ 

;;; _______________________________ 
;;; Utilities
;;; _______________________________ 

(define-sk8-function handlerObjectToHandlerId nil (handlerObj)
  (if (inheritsFrom handlerObj Handler)
    (list (project handlerObj)
          (name handlerObj)
          (object handlerObj)
          nil ;; qualifier?
          )
    ;; a function.
    (progn
      (if (symbolp handlerObj) (setf handlerObj (symbol-function handlerobj)))
      (list (project handlerObj)
            (name handlerObj)
            nil 
            nil))))

(defun handlerIdToHandlerObject (handlerId)
  (destructuring-bind (proj handlerName &optional handlerObject handlerQualifier) handlerId
    (declare (ignore proj handlerQualifier))
    (if handlerObject
      (mf::find-handler handlerName handlerObject)
      (when (fboundp handlerName)
        (symbol-function handlerName)))))

;;;this should return the handler name and the object it is defined on.
(defun handlerIDFromScript (proj script)
  (let ((*package* (package proj))
        name 
        obj
        wordList 
        (cnt 0))
    (block nil
      (mapWordChunks script nil nil 
                     #'(lambda (x y) 
                         (progn
                           (incf cnt)
                           (if (> cnt 7) (return))
                           (push (subseq script x y) WordList)))))
    (setf wordlist (nreverse wordlist))
    (unless (string-equal (first wordlist) "on") (error "Script is not a handler definition"))
    (setf name (read-from-string (second wordlist) nil nil))
    (when (eq name 'set)
      (setf wordList (rest wordList))
      (setf name (read-from-string (second wordlist) nil nil))
      (setf name (gethash name CCL::%setf-function-names%))
      )
    (setf wordList (rest (rest wordList)))
    (if (string= (first wordList) "of") (setf wordList (rest wordList)))
    (when (string= (first wordList) "me") 
      (if (and (or (string= (second wordList) "(a")
                   (string= (second wordList) "(an"))
               (third wordlist))
        (progn
          (setf obj (third wordList))
          (setf obj (subseq obj 0 (1- (length obj))))
          (setf obj (read-from-string obj nil nil))
          (if (boundp obj) (setf obj (symbol-value obj))))
        (setf obj 'sk8::me)  ;;unnamed object
        ))
    (sk8-multivals name obj)))

(defun !newGetLfun (proj handlerName &optional handlerObject handlerQualifier)
  (declare (ignore proj handlerQualifier)) ; since befores & afters are no longer supported
  (let ((gf (when (fboundp handlerName) (symbol-function handlerName))))
    (if handlerObject
      (when (typep gf 'standard-generic-function)
        ;; two cases to consider: object in first arg and object in second arg.
        (let* ((specializer-length (let ((cnt 0))
                                     (dolist (arg (arglist gf))
                                       (if (member arg lambda-list-keywords) 
                                         (return))
                                       (incf cnt))
                                     cnt))
               specializers
               object-class)
          (setf specializers (make-list  specializer-length :initial-element CCL::*t-class*))
          (setf object-class (class-of handlerObject))
          (if (eq (symbol-package handlername) (find-package :setf))
            (setf (second specializers) object-class)
            (setf (first specializers) object-class))
          (let ((meth (find-method gf nil specializers nil)))
            (cond (meth
                   (method-function meth))
                  ((and (> specializer-length 1)
                        (progn (setf (car specializers) t)
                               (setf (cadr specializers) object-class)
                               (setq meth (find-method gf nil specializers nil))))
                   (method-function meth))
                  (t nil)))))
      gf)))





;;; _______________________________ 
;;; Breakpoints
;;; _______________________________ 

;;; The *ski-break-alist* is an alist of all current sk8 processes in a break state.
;;;
;;; At a break, the breaking function places a form like:
;;;    (handlerID process line-number nil)
;;; on this list.
;;;
;;; To continue, step, or abort the break we'd like to tell the process, but we
;;; can't do that directly, so instead we frob that list entry to:
;;;    (handlerID process line-number command)
;;; 
(defparameter *sk8-break-alist* nil)
(defparameter *current-debug-process* nil)


(define-sk8-function resetEditorWithID nil (handlerID)
  (let ((curEditor (findHandlerEditor (first handlerID) (second handlerID) (third handlerID) (fourth handlerID) )))
    (when curEditor
      (switch-out-of-running-mode (editor curEditor)))
    t))

;;; Break-function contract:
;;; This puppy gets called repeatedly during a SK8 function with 
;;; an argument of the current line number.  If this is a breakpoint
;;; line, then we break.
;;; 
;;; If called with an argument of
;;;  :breakpoints
;;;  :set-breakpoint
;;;  :clear-breakpoint
;;;  :clear-breakpoints
;;; It will do the obvious stuff.

(defun make-break-function (handlerID)
  (let ((lines nil))
    (flet ((break-fn (location &rest rest)
             (if (symbolp location)
               (case location
                 (:breakpoints lines)
                 (:set-breakpoint (pushnew (car rest) lines))
                 (:clear-breakpoint (setq lines (delete (car rest) lines)))
                 (:clear-breakpoints (setq lines nil))
                 (:done 
                  (when (memq :step lines) 
                    (setf lines (delete :step lines))
                    (resetEditorWithID handlerID)))
                 )
               ;; location at this point is a pair (beginning-line ending-line)
               (let ((line-number location) 
                     (reservedProcess? (or (eql *current-process* CCL::*initial-process*)
                                           (eql *current-process* GS::*SK8-System-process*)
                                           (string-equal (process-name *current-process*) "Listener")
                                           ))
                     entry)
                 (when (or (member (setq location line-number) lines)
                           (memq :step lines))
                   (when (memq :step lines) (setf lines (delete :step lines)))
                   (add-to-break-alist handlerid *current-process* line-number nil)
                   (bringUpHandlerWithRuntimeProblem handlerID line-number :breakpoint nil '(:abort :go :step) (generate-backtrace-list (ccl::%get-frame-ptr)))
                   ;; This is where we stop executing...
                   ;;we check that we never arrest the core lisp 
                   (if reservedProcess?
                     (break "~%~%Breaking because the current process is reserved!~%Stepping, Going and Aborting will not work.~%The Expression Watcher will work.~%Type (continue) or (abort) in this listener~%to continue.~%")
                     (process-enable-arrest-reason *current-process* :halt))
                   ;;Below is where we start again...
                   (setf entry (get-from-break-alist handlerID))
                   (remove-from-break-alist handlerid)
                   (unless reservedProcess?
                     (case (fourth entry) 
                       (:step (pushnew :step lines))
                       (:go  
                        (resetEditorWithID handlerID)
                        nil)
                       (:abort 
                        (resetEditorWithID handlerID)
                        (abort))
                       (t (error "Illegal debug command ~s" (fourth entry))))
                     ))))))
      #'break-fn)))




;;(breakpoint-setter-from-id (list sk8::foo 'mousedown foo::bang nil))
;;(debuggo (list sk8::foo 'mousedown foo::bang nil))
;;(setBreakpoint (list sk8::foo 'mousedown foo::bang nil) 3)
;;(clearAllBreakpoints (list sk8::foo 'mousedown foo::bang nil))

;;; Retrieves the breakpoint setter function from the lfun-info of the
;;; function/handler pointed to by the handlerID.
;;; Emits annoying error messages along the way.
;;; Only used for the stuff below.
(defun breakpoint-setter-from-id (handlerid)
  (let ((fcn (handlerIdToHandlerObject handlerID)))
    (typecase fcn
      (null (error "Handler not found"))
      (standard-method (setf fcn (method-function fcn)))
      (compiled-lexical-closure (setf fcn (ccl::closure-function fcn))))
    (getf (ccl::%lfun-info (ccl::closure-function fcn)) :breakpoint-setter)))

(define-sk8-function containsActiveBreakpoints nil (handlerId)
  (let ((bps (breakpoint-setter-from-id handlerid)))
    (when bps
      (let ((break-fn (funcall bps :get-break-fn)))
        (when break-fn 
          (funcall break-fn :breakpoints))))))

(define-sk8-function clearAllBreakpoints nil (handlerId)
  (let ((bps (breakpoint-setter-from-id handlerid)))
    (when bps
      (let ((break-fn (funcall bps :get-break-fn)))
        (when break-fn
          (funcall break-fn :clear-breakpoints))))))


(define-sk8-function setBreakpoint nil (handlerId lineNumber)
  (let ((bps (breakpoint-setter-from-id handlerid)))
    (unless bps (MessageToUser "This function hasn't been compiled to accept breakpoints." :beep t))
    (let ((break-fn (funcall bps :get-break-fn)))
      ;; No break-fn?  Add one.
      (unless break-fn
        (setq break-fn (make-break-function handlerid))
        (funcall bps break-fn))
      (funcall break-fn :set-breakpoint lineNumber))))

(define-sk8-function clearBreakpoint nil (handlerId lineNumber)
  (let ((bps (breakpoint-setter-from-id handlerid)))
    (unless bps (error "This function hasn't been compiled to accept breakpoints."))
    (let ((break-fn (funcall bps :get-break-fn)))
      ;; No break-fn?  Add one.
      (if  break-fn
        (funcall break-fn :clear-breakpoint linenumber)
        ;;(warn "No breakpoints set")
        ))))

(define-sk8-function listBreakpoints nil (handlerId)
  (let ((bps (breakpoint-setter-from-id handlerid)))
    (unless bps (error "This function hasn't been compiled to accept breakpoints."))
    (let ((break-fn (funcall bps :get-break-fn)))
      ;; No break-fn?  Add one.
      (if  break-fn
        (funcall break-fn :breakpoints)
        ;;(warn "No breakpoints set")
        ))))

;;(listBreakpoints (list sk8::foo 'foo::testfun nil nil))


;;; Returns the line number of the next line to be executed in the handler
;;; where the breakpoint happened. 

;;; See the notes for *sk8-break-alist*
(defun debug-command-helper (handlerID keyword)
  (let ((entry (get-from-break-alist handlerID)))
    (if entry
      (destructuring-bind (handlerid process line-number command) entry
        (declare (ignore handlerid command))
        (setf (fourth entry) keyword)
        (process-disable-arrest-reason process :halt)
        ;; return the line number
        line-number)
      (progn
        (sendtolog "Process no longer appears active.  Aborting." :attention t)
        (resetEditorWithID handlerID)
        nil)
      )))

(define-sk8-function debugStep nil (handlerId)
  (debug-command-helper handlerid :step))

(define-sk8-function debugGo nil (handlerId)
  (debug-command-helper handlerid :go))

(define-sk8-function debugAbort nil (handlerId)
  (debug-command-helper handlerid :abort))

(define-sk8-function debugActive nil (handlerId)
  (let ((entry (get-from-break-alist handlerID)))
    (if entry
      t
      nil)
    ))

;;; Given the Id of a handler that is running but paused, this function returns
;;; a list of the ids of every handler in the stack. The runtime state of any of these handlers could
;;; be inspected by the user by opening them in an editor window. 

(define-sk8-function examinableHandlers nil (handlerId)
  nil)


;;; _______________________________ 
;;; Expression watching
;;; _______________________________ 

;;; Entering an expression into the watcher: This function returns a form that
;;; is predigested so that it can be evaluated cheaply at runtime. This is called at edit time.

;;; We need an alist of handlerIDs and expressions.

(defun add-to-break-alist (handlerid proc line-number state)
  (cleanup-break-alist)
  (remove-from-break-alist handlerid)
  (push (list handlerid proc line-number state) *sk8-break-alist*)
  )

(defun remove-from-break-alist (handlerid)
  (cleanup-break-alist)
  (ccl::while (assoc handlerid *sk8-break-alist* :test #'handleridequal)
    (setf *sk8-break-alist* 
          (delete (assoc handlerid *sk8-break-alist* :test #'handleridequal)
                  *sk8-break-alist*)))
  )

(defun get-from-break-alist (handlerid)
  (cleanup-break-alist)
  (assoc handlerid *sk8-break-alist* :test #'handleridequal)
  )

(defun cleanup-break-alist ()
  (dolist (i *sk8-break-alist*)
    (if (or (not (ccl::processp (second i)))
            (CCL::process-exhausted-p (second i)))
      (setf *sk8-break-alist* 
            (delete i *sk8-break-alist*)))))
      
;;THIS HAS TO BE A MACRO
;;so that alpha renaming of the local variables of the code generated by registerExpressionForRuntimeUse
;;will similarly rename the sym passed to get-debug-symbol-value.  Note that alpha renaming automatically
;;skips over renaming quoted symbols.  In this case we don't want it to, so we trick it with a macro.
(defmacro get-debug-symbol-value (sym)
  `(symbol-value-in-process ',sym *current-debug-process*))

(define-sk8-function registerExpressionForRuntimeUse nil (handlerId expressionString)
  (let ((trans (translateScriptExpression (car handlerId) expressionString))
        (locs (get-handler-locals))
        (locList (list 'skil::local)))
    (if (eq (car trans) 'error) 
      trans
      (progn
        (setf trans (second (fifth trans)))
        (when locs
          (dolist (i locs)
            (setf LocList 
                  (nconc locList
                         (list `(,i ((skil::lib :sk8dev function get-debug-symbol-value) ,i) 
                                    )))))
          (setf trans `(skil::script ,loclist ,trans)))
        (skil::skil-compile-to-lisp trans)))
    ))

;;(setf skil::*compile-with-debugging* t)
;;(trace registerExpressionForRuntimeUse evaluateExpressionInLocalContext)
;;(registerExpressionForRuntimeUse (list sk8::foo 'foo::testfun nil nil) "zz")
;;(evaluateExpressionInLocalContext (list sk8::jiz 'jiz::testfun2) *)
;;(symbol-value-in-process 'a *current-debug-process*)


(defun handlerRunning (handlerId)
  (let ((breakInfo (get-from-break-alist handlerId)))
    (if breakInfo
      (third breakInfo)  ;;return the line number
      nil)))


;;; At run time this function is called to evaluate the form produced in the
;;; edit time function. This evaluation is cheap. Returns the value of the 
;;; expression. 
(define-sk8-function evaluateExpressionInLocalContext nil (handlerId form)
  (let ((curItem (get-from-break-alist handlerId))
        (error? t)
        (CCL::*SUPPRESS-COMPILER-WARNINGS* T)
        res)
    (setf *current-debug-process* (second curItem))
    (if *current-debug-process*
      (progn
        (multiple-value-bind (val1 err?) 
                             (ignore-errors (setf res (eval form)))
          (declare (ignore val1))
          (setf error? err?))
        (when error? (setf res "Cannot Evaluate"))
        res)
      "Not Running")))

(define-sk8-function getLocalVarInLocalContext nil (handlerId loc)
  (if (symbolp loc) (setf loc (sk8::name loc)))
  (evaluateExpressionInLocalContext handlerid (registerExpressionForRuntimeUse handlerid loc)))


(define-sk8-function setLocalVarInLocalContext nil (handlerId loc val)
  (if (stringp loc) 
    (let ((*package* (package (first handlerID))))
      (setf loc (read-from-string loc nil nil))))
  (let ((curItem (get-from-break-alist handlerId))
        (error? t)
        res)
    (setf *current-debug-process* (second curItem))
    (if *current-debug-process*
      (progn
        (multiple-value-bind (val1 err?) 
                             (ignore-errors 
                              (setf res (funcall (ccl::setf-function-name 'symbol-value-in-process) val
                                                 loc *current-debug-process*)))
          (declare (ignore val1))
          (setf error? err?))
        (when error? (setf res "Cannot Evaluate"))
        res)
      "Not Running")))



#|
	Change History (most recent last):
	2  	 5/20/96	Brian   	
	3  	 5/20/96	Brian   	Added real stepper stuff.  Commented out for now
						until we rework the processes.
	4  	 5/20/96	Brian   	
	5  	 5/21/96	Brian   	
	6  	 5/21/96	Brian   	
	7  	 5/23/96	Brian   	
	8  	 7/ 7/96	sidney  	typo in a define-sk8-function
	9  	 7/15/96	Brian   	
	10 	 8/ 5/96	Brian   	
	11 	 8/ 8/96	Brian   	
	12 	 9/ 5/96	Brian   	
	13 	 9/12/96	Brian   	
	14 	10/ 7/96	Brian   	Making newgetlfun handle setters properly.
	15 	10/10/96	Brian   	Fixng hadnlerID from script to return 'sk8::me if
						it is an unnamed handler.
	16 	10/14/96	Brian   	made getlocalvar work properly.
	17 	10/18/96	Brian   	suppressing compiler warnings when evaling watcher expressions
	18 	11/14/96	Brian   	Making break-functions generate a backtrace-list.
	19 	11/14/96	sidney  	don't block sk8 system process when entering debugger
	20 	 2/11/97	Brian Roddy	Fixing registerExpression so alpha renaming
						can happen properly.
	21 	 2/11/97	Brian Roddy	
	22 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
