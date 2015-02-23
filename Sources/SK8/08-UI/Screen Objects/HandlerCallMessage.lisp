;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)



;;;
;;;  When call is made, the HandlerCallLogObject's properties hold the values as named
;;;  When call returns, the HandlerCallLogObject's ARGUMENTS property holds the return value, and its CONTEXT property points to the in
;;;
(new InspectableLogObject :objectName "HandlerCallLogObject" :project UI
     :properties '(handlerName arguments level context))

;;; The t case was neglecting to initialize the arguments slot (that is where the
;;; return value was being passed.

(define-handler initialize (HandlerCallLogObject original isNew initArgs)
  (declare (ignore original isNew))
  (call-next-method)
  (let ((handlerName (initializerArgument initArgs 'handlerName))
        (arguments (initializerArgument initArgs 'arguments))
        (context (initializerArgument initArgs 'context)))
    (cond
     ;; The message is for a call being made
     ((listp context)
      (setf (handlerName me) handlerName
            (arguments me) (when arguments
                             (cond
                              ((or (consp handlerName) (sk8dev::!setHandlerName? handlerName))
                               (if (memq :to arguments)
                                 arguments
                                 (nconc (cdr arguments) (list :to (first arguments)))))
                              (t
                               arguments)))
            (context me) context))
     ;; The message is for a call returning
     (t
      (setf (handlerName me) (handlerName context)
            (arguments me) arguments
            (context me) context
            (level me) (level context))))))


(define-handler writeLogObject (HandlerCallLogObject strm)
  (dotimes (i (level me)) (stream-tyo strm #\.))
  (let ((handlerName (handlerName me))
        (context (context me))
        (argsOrVal (arguments me)))
    (cond
     ;; The message is for a call being made
     ((listp context)
      (write-string "Calling " strm)
      (writeObject handlerName strm nil)
      (unless (keywordp (first argsOrVal)) (write-string " of" strm))
      (SK8::writeArgsNicely argsOrVal strm))
     ;; The message is for a call returning
     (t
      (writeObject handlerName strm nil)
      (write-string " returned " strm)
      (writeObject argsOrVal strm t)))))


(define-handler inspectLogObject (HandlerCallLogObject)
  (let ((context (context me)))
    (unless (listp context) (setq context (context context)))
    (ed-beep)
      ;(describe me)
    ;; *** BRING UP AN INSPECTOR FOR THE CONTEXT!
    ))



#|
	Change History (most recent last):
	1	9/24/93	chip	new file
	2	11/29/93	chip	new logging protocol
	4	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	5	3/6/94	chip	print... --> write...; "logObject" renaming
	6  	10/ 5/94	chip    	initialize args now accessed with initializerArgument
	7  	11/15/94	chip    	initialize of HandlerCallLogObject now sets the handlerName property (since it "uses" the initarg) (radar #1200079)
	8  	 2/16/95	sidney  	readable argument names for initialize handler
	2  	 6/23/95	Hernan  	1256209
	3  	 1/19/96	sidney  	removing/rewriting refrences to old sk8script compiler
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
