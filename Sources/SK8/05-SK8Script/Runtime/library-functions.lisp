;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)


;;;;---------------------------------------------------------------------------
;;;;True and false need to be handled carefully.
;;;;---------------------------------------------------------------------------

(sk8dev::SK8-declare-syms :SK8 :public ; Updated  7-19-96   5:44 pm
                          SK8::FALSE SK8::TRUE SK8::BOOLEAN SK8::VISITSTATE)
(sk8dev::define-sk8-var SK8::True :initial-Value t)
(sk8dev::define-sk8-var SK8::False :initial-Value nil)
(sk8::new sk8::object :objectname "Boolean" :project sk8::sk8)
(sk8::new sk8::object :objectname "VISITSTATE" :project sk8::sk8)



;;;;---------------------------------------------------------------------------
;;;;Internal functions
;;;;---------------------------------------------------------------------------



(defmacro sk8::wait-for-condition (cond &key (events t))
  (let ((body `(loop
                 (event-dispatch)
                 (,(if (eq 'WHILE (car cond)) 'unless 'when) ,(second cond) (return)))))
    `(if ,events
       (ccl::without-event-processing
         ,body)
       (ccl::with-event-processing-enabled
         ,body))))

(defun wait-time-period* (numticks &key (events t))
  (setq numTicks (truncate numTicks))
  (unless (>= numTicks 0)
    (SK8::SK8-error SK8::TypeMismatchError :object numTicks :expectedType 'SK8::nonNegativeNumber))
  (locally (declare (integer numTicks))
    (incf numTicks (#_TickCount))
    (sk8::wait-for-condition (when (>= (#_TickCount) numTicks)) :events events)))

(defmacro sk8::wait-time-period (unit count &key (events t))
  (let* ((UNITS '(
                  ;(SK8::DAY . 518400) (SK8::DAY . 518400)
                  ;(SK8::HOUR . 21600) (SK8::HOURS . 21600)
                  (SK8::MINUTE . 360) (SK8::MINUTES . 360)
                  (SK8::SECOND . 60) (SK8::SECONDS . 60)
                  (SK8::TICK . 1) (SK8::TICKS . 1)
                  (SK8::MILLISECOND . 6S-2) (SK8::MILLISECONDS . 6S-2)
                  )))
    (unless (setq unit (cdr (assq unit units))) (error "Bad time unit for wait command"))
    `(wait-time-period* (* ,unit ,count) :events ,events)))


;; BEEP
(mf::publish-function-symbol 'beep  mf::*SK8-package*)

;;; Returns a thing that is funcallable to set the value of a property.
;;; Can take a symbol, a handler object or a function (eg. a generic function).

(define-sk8-function setter nil (handlerObj &key sameObject)
  (let (name)
    (cond
     ((symbolp handlerObj)
      (if (!setHandlerName? handlerObj)
        handlerObj
        (gethash handlerObj CCL::%SETF-FUNCTION-NAMES%)))
     
     ((functionp handlerObj)
      (setq name (CCL::LFUN-VECTOR-NAME (CCL::%LFUN-VECTOR handlerObj)))
      (if (consp name)
        handlerObj
        (when (setq name (gethash name CCL::%SETF-FUNCTION-NAMES%))
          (fboundp name))))
     
     (t ; must be a method
      (setq name (method-name handlerObj))
      (if (consp name)
        handlerObj
        (when (setq name (gethash name CCL::%SETF-FUNCTION-NAMES%))
          (let ((hObj (MF::get-handler-object-from-lfun (method-function handlerObj))))
            (or (when hObj (if sameObject
                             (MF::find-local-handler name hObj)
                             (MF::find-applicable-handler name hObj)))
                (unless sameObject (fboundp name))))))))))


(define-sk8-function SK8::canDo nil (handlerName &optional obj)
  (when (fboundp handlerName)
    (if obj
      (mf::find-applicable-handler handlerName obj)
      t)))



(define-sk8-function handlerArgumentsString nil (handlerObj &key declaration call
                                                                (primaryArgument t) (setterArgument t))
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
         obj)

    ;;Macro's seemed to be broken, so i added this -BJR 7/19/96
    (if (and (symbolp name) (macro-function name))
      (setf lfun (macro-function name)))

    (setf obj (nth-value 2 (!exactLfunInfo lfun)))
    (with-output-to-string (strm)
      (!printHandlerArgs strm name obj (arglist lfun)
                             :declaration declaration :call call :primaryArgument primaryArgument
                             :setterArgument setterArgument))))




#|
	Change History (most recent last):
	2		6/14/93	chip	added "squareRoot" synonym for sqrt.
	3		6/23/93	chip	added "sign" function for numbers
	9		11/24/93	chip	handlerObjectArgString now has :declaration and :call options
	10		11/29/93	chip	added SETTER
	11		1/7/94	sidney	big mf/ss merge
	12		2/18/94	chip	
	13		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	14		3/1/94	chip	
	15		3/6/94	chip	fixed wrapper problem in handlerArgumentsString; added canDo function; added *character-names* alist
	16		3/13/94	sidney	get rid of some ugliness
	17 	 9/ 1/94	chip    	publish-project-symbol --> publish-global/function-symbol (radar #1183935)
	18 	 9/19/94	chip    	obsoleted uses of with-edittext-string
	19 	 9/23/94	chip    	temporarily simplified position (to stabilize it for release)
	20 	10/17/94	sidney  	get-handler-object-from-lfun now unwraps its argument
	21 	10/20/94	chip    	position is now fully general
	22 	10/24/94	chip    	applicableHandler --> find-applicable-handler
	23 	10/31/94	chip    	removed old definition of MOD (for radar #1196467)
	24 	11/20/94	chip    	fixed case of character constants
	25 	 3/31/95	Hernan  	Fixing setter and possibly changing the load order.
	2  	 7/24/95	Hernan  	Adding the characters Home, End, PageUp, and PageDown.
	3  	 7/28/95	sidney  	rename End to EndKey so existing code will compile ok
	4  	 1/19/96	sidney  	removing/rewriting references to old sk8script compiler
	5  	 2/ 9/96	sidney  	!printHandlerArgs changed package
	2  	 4/10/96	Hernan  	defun->define-sk8-function.
	3  	 4/16/96	Brian   	getting rid of delegation
	4  	 4/19/96	Brian   	
	5  	 4/19/96	Brian   	
	6  	 4/19/96	Brian   	
	7  	 7/19/96	Brian   	Fixing handlerArgumentsString to deal with macros.
	8  	 7/19/96	Brian   	oops typo.
	9  	 8/30/96	Brian   	Moving true, false,etc. here.
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
