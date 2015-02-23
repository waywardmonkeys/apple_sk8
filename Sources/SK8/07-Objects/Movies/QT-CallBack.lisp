(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(unless (boundp 'sk8dev::*quicktime-callbacks-to-activate*)
  (defvar        sk8dev::*quicktime-callbacks-to-activate* nil)) ;? should be a population

;;; Called by do-event.
(when (fboundp 'sk8dev::activate-quicktime-callbacks)
  (fmakunbound 'sk8dev::activate-quicktime-callbacks))

(defun sk8dev::activate-quicktime-callbacks ()
  (dolist (callBack sk8dev::*quicktime-callbacks-to-activate*)
    (ignore-errors
     (activate callBack)))
  (setf sk8dev::*quicktime-callbacks-to-activate* '())
  nil)

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(defun callBackSymbolsToBitsInclusive (parameters mapTable handlerName)
  (let ((flags 0))
    (dolist (theSymbol (mklist (first parameters)))
      (setf flags (logior flags (let (bit)
                                  (if (integerp (setf bit (gethash theSymbol mapTable)))
                                    bit
                                    (sk8-error IncorrectArgumentsError
                                               :handlername handlerName
                                               :arguments (list :parameters parameters)))))))
    flags))

(defun callBackSymbolToBitsExclusive (parameters mapTable handlerName)
  (let (bit)
    (if (integerp (setf bit (gethash (first parameters) mapTable)))
      bit
      (sk8-error IncorrectArgumentsError
                 :handlername handlerName
                 :arguments (list :parameters parameters)))))

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new Object
     :project SK8
     :objectName "QuickTimeCallBack")

(addProperty QuickTimeCallBack 'callBackGCOSPtr                       :private t )
(addProperty QuickTimeCallBack 'timeBase                                         )
(addProperty QuickTimeCallBack 'type                                             )
(addProperty QuickTimeCallBack 'parameters       :initialValue '(0 0 0)          )
(addProperty QuickTimeCallBack 'parametersSK8    :initialValue '(0 0 0)  :private t)
(addProperty QuickTimeCallBack 'repeating        :initialValue t                 )
(addProperty QuickTimeCallBack 'active           :initialValue t                 )

(define-handler localVirtualProperties :private (QuickTimeCallBack)
  '(awaitingActivation))

(define-handler initialize :private (QuickTimeCallBack original isNew initArgs)
  (declare (ignore original isNew))
  (call-next-method)
  (let ((timeBase (initializerArgument initargs 'timeBase :default 'xyzzy)))
    (cond
     ((eq timeBase 'xyzzy)
      (sk8-error IncorrectArgumentsError
                 :handlername 'new
                 :arguments (list :timeBase nil)))
     ((not timeBase)
      )
     ((not (inheritsFrom timeBase QuickTimeTimeBase))
      (sk8-error IncorrectArgumentsError
                 :handlername 'new
                 :arguments (list :timeBase timeBase))))
    (setValue 'timeBase me timeBase))
  (setValue 'active me (and (initializerArgument initargs 'active :default t)
                            t))
  (finishInitialize me)
  (sk8-return-nothing))

(define-handler reconstructParameters :private (QuickTimeCallBack)
  (sk8-return-nothing))

(define-handler finishInitialize :private (QuickTimeCallBack)
  (let ((timeBase (timeBase me)))
    (when timeBase
      (setf (callBackGCOSPtr me) (T_NewCallBackGC
                                  (timeBaseOSPtr timeBase)
                                  (type me)
                                  :movie (let ((movie (movie timeBase)))
                                           (and movie
                                                (mediaData movie)))))
      (pushNew me (callbacks timeBase))
      (when (active me)
        (activate me))))
  (sk8-return-nothing))

;;; Returns nil if not initialized.
(define-handler restoreOrInitializeFromStore :private (QuickTimeCallBack whichFunction)
  (trace-print-bracketing ("initialize callBack" :debug-initialize)
    (or (callBackGCOSPtr me)
        (eq me QuickTimeCallBack)
        (let ((timeBase (timeBase me)))
          (or (not timeBase)
              ;; Must be an instance.
              (and (funcall whichFunction timeBase)
                   ;; Timebase initialization went ok.
                   (progn
                     (finishInitialize me)
                     (unless (getValue 'parametersSK8 me)
                       (reconstructParameters me)) ;; so we can read in B3 projects
                     t)))))))

(define-handler initializeFromStore :private (QuickTimeCallBack)
  (and (call-next-method)
       (restoreOrInitializeFromStore me #'initializeFromStore)))

(define-handler restore :private (QuickTimeCallBack)
  (and (call-next-method)
       (restoreOrInitializeFromStore me #'restore)))

(define-handler localPropertiesToSaveAsFalse :private (QuickTimeCallBack)
  (and (eq me QuickTimeCallBack)
       '(callBackGCOSPtr)))

(define-handler preserve :private (QuickTimeCallBack)
  (call-next-method)
  ;; (preserveLikeSave me)
  (sk8-return-nothing))

(define-handler dispose (QuickTimeCallBack)
  (let ((callBackGCOSPtr (callBackGCOSPtr me)))
    (and callBackGCOSPtr
         (let ((timeBase (timeBase me)))
           (T_deactivateGCOSPtr callBackGCOSPtr :disposeNow t)
           (setf callBackGCOSPtr nil)
           ;; Don't need to remove from *quicktime-callbacks-to-activate*
           ;; because this couldn't be called while we're in that list.
           (setf (callbacks timeBase) (delete me (callBacks timeBase)))
           (setValue 'timeBase me nil)
           t))))

;;; Returns true if movie is playing and it is being serviced
(define-handler awaitingActivation :private (QuickTimeCallBack)
  (and (member me sk8dev::*quicktime-callbacks-to-activate*)
       t))

(define-handler (setf active) (newValue QuickTimeCallBack &key force)
  (let ((wasActive (getValue 'active me)))
    (if (or (xor newValue wasActive)
            force)
      (assumeOSPtrWithError (me callBackGCOSPtr)
        (if newValue
          (progn
            (when wasActive
              (T_CancelCallBack callBackGCOSPtr))
            (pushnew me sk8dev::*quicktime-callbacks-to-activate*))
          (progn
            (T_CancelCallBack callBackGCOSPtr)
            (setf sk8dev::*quicktime-callbacks-to-activate*
                  (remove me sk8dev::*quicktime-callbacks-to-activate*))))
        (setValue 'active me newValue))
      newValue)))

(define-handler activate :private (QuickTimeCallBack)
  (assumeOSPtrWithError (me callBackGCOSPtr)
    (apply #'T_CallMeWhen
           callBackGCOSPtr
           (objectIDFromObject me)
           (getValue 'parameters me))
    (setValue 'active me t) ; this isn't really necessary
    (sk8-return-nothing)))

(define-handler reactivateWithNewParameters :private (QuickTimeCallBack)
  (assumeOSPtrWithError (me callBackGCOSPtr)
    (when (getValue 'active me)
      (T_CancelCallBack callBackGCOSPtr))
    (apply #'T_CallMeWhen
           callBackGCOSPtr
           (objectIDFromObject me)
           (getValue 'parameters me))
    (setValue 'active me t)
    (sk8-return-nothing)))

(define-handler (setf type) (newValue QuickTimeCallBack)
  (declare (ignore newValue))
  (raiseUnsettablePropertyError 'type me))

(define-handler (setf timeBase) (newValue QuickTimeCallBack)
  (declare (ignore newValue))
  (raiseUnsettablePropertyError 'timeBase me))

(define-handler parameters (QuickTimeCallBack)
  (getValue 'parametersSK8 me))

(define-handler (setf parameters) (newValue QuickTimeCallBack &key (reactivate t))
  (when (eq me QuickTimeCallBack)
    (raiseUnsettablePropertyError 'parameters me))
  (setValue 'parametersSK8 me newValue)
  (when reactivate
    (reactivateWithNewParameters me))
  newValue)

;;; This is the handler that's called when the callback is triggered.
(define-handler callBackEvent (QuickTimeCallBack count)
  (declare (ignore count))
  (sk8-return-nothing))

;;; This is the handler that's called when the callback is triggered.
(define-handler doCallBackEvent :private (QuickTimeCallBack count)
  (setValue 'active me nil)
  (callBackEvent me count)
  (when (repeating me)
    (setf (active me) t))
  (sk8-return-nothing))

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new QuickTimeCallBack
     :project SK8
     :objectName "QuickTimeCallBackAtTime"
     :timeBase nil)

(setValue 'type          QuickTimeCallBackAtTime 'Time)
(setValue 'parameters    QuickTimeCallBackAtTime (list #$triggerTimeEither 0 1))
(setValue 'parametersSK8 QuickTimeCallBackAtTime (list 'either 0 1))

(define-handler reconstructParameters :private (QuickTimeCallBackAtTime)
  (setValue 'parametersSK8 me
            (let ((parameters (getValue 'parameters me)))
              (list
               (let ((direction (first parameters)))
                 (case direction
                   (#.#$triggerTimeEither   'either)
                   (#.#$triggerTimeFwd      'forward)
                   (#.#$triggerTimeBwd      'backward)))
               (second parameters)
               (third  parameters))))
  (sk8-return-nothing))

(defvar callBackAtTimeSymbols (make-hash-table))
(setf (gethash 'forward  callBackAtTimeSymbols) #$triggerTimeFwd)
(setf (gethash 'backward callBackAtTimeSymbols) #$triggerTimeBwd)
(setf (gethash 'either   callBackAtTimeSymbols) #$triggerTimeEither)

(define-handler (setf parameters) (newValue QuickTimeCallBackAtTime &key (reactivate t))
  (declare (ignore reactivate))
  (let ((scale (third newValue)))
    (when (or (not (integerp scale))
              (zerop scale))
      (sk8-error IncorrectArgumentsError
                 :handlername "set parameters"
                 :arguments (list :parameters newValue))))
  (setValue 'parameters me
            (list (callBackSymbolsToBitsInclusive newValue callBackAtTimeSymbols "set parameters")
                  (require-type (second newValue) integer)
                  (require-type (third  newValue) integer)))
  (call-next-method))

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new QuickTimeCallBack
     :project SK8
     :objectName "QuickTimeCallBackAtRate"
     :timeBase nil)

(setValue 'type          QuickTimeCallBackAtRate 'Rate)
(setValue 'parameters    QuickTimeCallBackAtRate (list #$triggerRateGT (T_numberToFixed 1) 0))
(setValue 'parametersSK8 QuickTimeCallBackAtRate (list 'greaterThan 1 0))

(define-handler reconstructParameters :private (QuickTimeCallBackAtRate)
  (setValue 'parametersSK8 me
            (let ((parameters (getValue 'parameters me)))
              (list
               (case (first parameters)
                 (#.#$triggerRateChange   'change)
                 (#.#$triggerRateLT       'lessThan)
                 (#.#$triggerRateGT       'greaterThan)
                 (#.#$triggerRateEqual    'equal)
                 (#.#$triggerRateLTE      'lessEqual)
                 (#.#$triggerRateGTE      'greaterEqual)
                 (#.#$triggerRateNotEqual 'notEqual))
               (T_fixedToNumber (second parameters))
               (third parameters)))))

(defvar callBackAtRateSymbols (make-hash-table))
(setf (gethash 'change       callBackAtRateSymbols) #$triggerRateChange)
(setf (gethash 'lessThan     callBackAtRateSymbols) #$triggerRateLT)
(setf (gethash 'greaterThan  callBackAtRateSymbols) #$triggerRateGT)
(setf (gethash 'equal        callBackAtRateSymbols) #$triggerRateEqual)
(setf (gethash 'lessEqual    callBackAtRateSymbols) #$triggerRateLTE)
(setf (gethash 'greaterEqual callBackAtRateSymbols) #$triggerRateGTE)
(setf (gethash 'notEqual     callBackAtRateSymbols) #$triggerRateNotEqual)

(define-handler (setf parameters) (newValue QuickTimeCallBackAtRate &key (reactivate t))
  (declare (ignore reactivate))
  (unless (zerop (third newValue))
    (sk8-error IncorrectArgumentsError
               :handlername "set parameters"
               :arguments (list :newValue newValue)))
  (setValue 'parameters me
            (list (callbackSymbolToBitsExclusive newValue callBackAtRateSymbols "set parameters")
                  (T_numberToFixed (require-type (second newValue) number))
                  0))
  (call-next-method))


;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new QuickTimeCallBack
     :project SK8
     :objectName "QuickTimeCallBackAtTimeJump"
     :timeBase nil)

(setValue 'type QuickTimeCallBackAtTimeJump 'TimeJump)

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new QuickTimeCallBack
     :project SK8
     :objectName "QuickTimeCallBackAtExtremes"
     :timeBase nil)

(setValue 'type          QuickTimeCallBackAtExtremes 'Extremes)
(setValue 'parameters    QuickTimeCallBackAtExtremes (list #$triggerAtStop 0 0))
(setValue 'parametersSK8 QuickTimeCallBackAtExtremes (list 'stop 1 0))

(define-handler reconstructParameters :private (QuickTimeCallBackAtExtremes)
  (setValue 'parametersSK8 me
            (let ((parameters (getValue 'parameters me)))
              (list
               (let ((bits (first parameters)))
                 (append (if (zerop (logand bits #$triggerAtStart)) '() '(start))
                         (if (zerop (logand bits #$triggerAtStop )) '() '(stop))))
               (second parameters)
               (third  parameters)))))

(defvar callBackAtExtremesSymbols (make-hash-table))
(setf (gethash 'start callBackAtExtremesSymbols) #$triggerAtStart)
(setf (gethash 'stop  callBackAtExtremesSymbols) #$triggerAtStop)
(setf (gethash 'both  callBackAtExtremesSymbols) (logior #$triggerAtStart
                                     #$triggerAtStop))

(define-handler (setf parameters) (newValue QuickTimeCallBackAtExtremes &key (reactivate t))
  (declare (ignore reactivate))
  (dolist (parameter (rest newValue))
    (unless (zerop parameter)
      (sk8-error IncorrectArgumentsError
                 :handlername "set parameters"
                 :arguments (list :newValue newValue))))
  (setValue 'parameters me
            (list (callBackSymbolsToBitsInclusive newValue callBackAtExtremesSymbols "set parameters")
                  0
                  0))
  (call-next-method))

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new QuickTimeCallBackAtExtremes
     :project SK8
     :objectName "QuickTimeCallBackForRenderer"
     :timeBase nil)

(setValue 'parameters QuickTimeCallBackForRenderer (list (logior #$triggerAtStart
                                                                 #$triggerAtStop)
                                                         0
                                                         0))
(setValue 'parametersSK8 QuickTimeCallBackForRenderer (list '(start stop) 0 0))

(define-handler callBackEvent (QuickTimeCallBackForRenderer count)
  (declare (ignore count))
  (let ((myMovie (movie (timeBase me))))
    (when (T_IsMovieDone (mediaData myMovie))
      (done (renderer myMovie))))
  (call-next-method))

(define-handler (setf parameters) (newValue QuickTimeCallBackForRenderer &key (reactivate t))
  (declare (ignore newValue reactivate))
  (raiseUnsettablePropertyError 'parameters me))


#|
	Change History (most recent last):
	1  	 3/ 6/95	dy      	new
	2  	 3/10/95	dy      	fixes including hopefully save/restore, and make done of QuickTimeRenderer work
	3  	 3/11/95	dy      	introduce ready of QuickTimeCallBack, delete sk8:cancel, make callAgain private, delete "set awaitingReschedule
	4  	 3/11/95	dy      	delete callMeWhen, ready -> active
	5  	 3/11/95	dy      	move raiseUnsettablePropertyError to another source file and finish providing default parameters for each type of QuickTimeCallBack
	6  	 3/11/95	dy      	oops, type of QTCBTimeJump wasn't TimeJump
	7  	 3/13/95	dy      	fix some oopses from the previous checkin
	8  	 3/14/95	dy      	QTCallBackForRenderer uses start and stop - still not a good solution.
	9  	 3/21/95	dy      	try to fix store
	10 	 3/22/95	dy      	By default, a new QTCallBack is active
	11 	 3/22/95	dy      	error in finishInitialize when timeBase is False
	12 	 3/24/95	dy      	use T_IsMovieDone with QTCallBackForRenderer to make done of QTRenderer work correctly
	13 	 3/28/95	dy      	Try to fix preserve/restore.
	14 	 3/29/95	dy      	disallow third parameter of 0 for QTCallBackAtTime;
							redo callBackEvent so the active property is always correctly maintained.
	15 	 4/18/95	dy      	Raise an error on attempt to set type property.
	16 	 4/24/95	dy      	tweaks discovered when documenting
	17 	 4/26/95	dy      	Fix bug that prevented setting item n in the parameters of a QTCallback
	18 	 4/26/95	Brian   	Fix build problem
	19 	 4/26/95	dy      	The previous fix was wrong.  Now it really requires a timeBase argument to either be a QuickTimeTimeBase or to be explicitly False.
	20 	 4/28/95	dy      	project saved with b3 wouldn't load parameters properly in b4
	3  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
