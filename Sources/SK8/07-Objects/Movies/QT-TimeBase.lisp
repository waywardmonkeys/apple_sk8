(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;;? Bugs: when a TimeBase is disposed, and it has a master clock, the master clock is disposed (CloseComponent is called)
;;; As it is now, if that clock component instance is represented by a QuickTimeClockComponentInstance
;;; SK8 object, then the QuickTimeClockComponentInstance's instanceOSPtr will point to a dead macptr.

(SK8-declare-syms :SK8 :public
                  SK8::instanceOSPtr ;;? because of a bug in initializer argument handling this is declared public instead of private
                  )

(SK8-declare-syms :SK8 :public
                SK8::timeScale
                SK8::maintainTimeBaseZero)

(new Object
     :project SK8
     :objectName "QuickTimeComponentInstance")

(addProperty QuickTimeComponentInstance 'instanceOSPtr        :private t )

(addParent QuickTimeComponentInstance SpecialStorageMixin)

(define-handler localVirtualProperties :private (QuickTimeComponentInstance)
  '(name
    info))

(define-handler localPropertiesToSaveAsFalse (QuickTimeComponentInstance)
  (and (eq me QuickTimeComponentInstance)
       '(instanceOSPtr)))

(define-handler name (QuickTimeComponentInstance)
  (assumeOSPtr (me instanceOSPtr)
    (sk8-multival-bind (description name info iconGCHandle)
                       (T_GetComponentInfoGC instanceOSPtr
                                             :componentDescriptionGCHandle nil
                                             :componentName t
                                             :componentInfo nil
                                             :componentIconGCHandle nil)
      (declare (ignore description info iconGCHandle))
      name)))

(define-handler info (QuickTimeComponentInstance)
  (assumeOSPtr (me instanceOSPtr)
    (sk8-multival-bind (description name info iconGCHandle)
                       (T_GetComponentInfoGC instanceOSPtr
                                             :componentDescriptionGCHandle nil
                                             :componentName nil
                                             :componentInfo t
                                             :componentIconGCHandle nil)
      (declare (ignore description name iconGCHandle))
      info)))

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new QuickTimeComponentInstance
     :project SK8
     :objectName "QuickTimeClockComponentInstance")

(addProperty QuickTimeClockComponentInstance 'timeBase)

;;;? untested
(define-handler restoreOrInitializeFromStore :private (QuickTimeClockComponentInstance whichFunction)
  (let ((timeBase (timeBase me)))
    (or (not timeBase)
        (and (funcall whichFunction timeBase)
             (setf (instanceOSPtr me) (instanceOSPtr (master timeBase)))))))

;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

(new Object
     :project SK8
     :objectName "QuickTimeTimeBase")

(addProperty QuickTimeTimeBase 'timeBaseOSPtr                       :private t )
(addProperty QuickTimeTimeBase 'callbacks                                      )
(addProperty QuickTimeTimeBase 'master                                         )
(addProperty QuickTimeTimeBase 'movie                                          )

(define-handler localVirtualProperties :private (QuickTimeTimeBase)
  '(timeValue
    rate
    effectiveRate
    startTime
    stopTime
    repeating
    maintainTimeBaseZero
    timeScale
    ))

(define-handler initialize :private (QuickTimeTimeBase original isNew initArgs)
  (declare (ignore original isNew))
  (let ((timeBaseOSPtr (initializerArgument initArgs 'timeBaseOSPtr)))
    (setValue 'timeBaseOSPtr me
              (or timeBaseOSPtr
                  (makeGCOSPtr (T_NewTimeBaseGC) 'T_DisposeTimeBase))))
  (sk8-return-nothing))

;;; Returns nil if initialization couldn't complete.
(define-handler restoreOrInitializeFromStore :private (QuickTimeTimeBase whichFunction)
  (trace-print-bracketing ("initialize TimeBase" :debug-initialize)
    (or (macptrp (timeBaseOSPtr me))
        (eq me QuickTimeTimeBase)
        (let (rateToRestore)
          (and (let ((movie (movie me)))
                 (if movie
                   (when (funcall whichFunction movie)
                     (setValue 'timeBaseOSPtr me (T_GetMovieTimeBase (mediaData movie)))
                     ;;? deal with the master
                     t)
                   (let ((savedValueList (timeBaseOSPtr me)))
                     (unless (null savedValueList)
                       (setValue 'timeBaseOSPtr me (makeGCOSPtr (T_NewTimeBaseGC) 'T_DisposeTimeBase))
                       (setf (timeValue            me) (pop savedValueList))
                       (setf (startTime            me) (pop savedValueList))
                       (setf (stopTime             me) (pop savedValueList))
                       (setf (repeating            me) (pop savedValueList))
                       (setf (maintainTimeBaseZero me) (pop savedValueList))
                       (setf rateToRestore             (pop savedValueList))
                       ;;? deal with the master
                       t))))
               (progn
                 (dolist (callback (callbacks me))
                   (funcall whichFunction callback))
                 (when rateToRestore
                   (setf (rate me) rateToRestore))
                 t))))))

(define-handler initializeFromStore :private (QuickTimeTimeBase)
  (and (call-next-method)
       (restoreOrInitializeFromStore me #'initializeFromStore)))

(define-handler restore :private (QuickTimeTimeBase)
  (and (call-next-method)
       (restoreOrInitializeFromStore me #'restore)))

(define-handler localPropertiesToSaveSpecially (QuickTimeTimeBase)
  (and (eq me QuickTimeTimeBase)
       '(timeBaseOSPtr)))

(define-handler saveToStore :private (QuickTimeTimeBase property)
  (case property
    (timeBaseOSPtr
     (and (timeBaseOSPtr me)
          (list (timeValue            me)
                (startTime            me)
                (stopTime             me)
                (repeating            me)
                (maintainTimeBaseZero me)
                (rate                 me))))
    (otherwise
     (call-next-method))))

(define-handler preserve :private (QuickTimeTimeBase)
  (call-next-method)
  ;; (preserveLikeSave me)
  (sk8-return-nothing))

(defun timeBaseMasterClock (me timeBaseOSPtr)
  (let ((instanceOSPtr (T_GetTimeBaseMasterClock timeBaseOSPtr)))
    (and (not (eql instanceOSPtr TC_nil))
         (or (sk8ObjectRepresentingOSPtr instanceOSPtr)
             (let ((masterClock (new QuickTimeClockComponentInstance
                                     :instanceOSPtr instanceOSPtr
                                     :timeBase me
                                     :project (project me))))
               (registerSk8ObjectRepresentingOSPtr masterClock
                                                   instanceOSPtr
                                                   :masterGCOSPtr timeBaseOSPtr)
               (setValue 'master me masterClock))))))

(defun timeBaseMasterTimeBase (me timeBaseOSPtr)
  (let ((masterTimeBaseOSPtr (T_GetTimeBaseMasterTimeBase timeBaseOSPtr)))
    (and (not (eql masterTimeBaseOSPtr TC_nil))
         (or (sk8ObjectRepresentingOSPtr masterTimeBaseOSPtr)
             (let ((masterTimeBase (new QuickTimeTimeBase
                                        :timeBaseOSPtr masterTimeBaseOSPtr
                                        :project (project me))))
               (registerSk8ObjectRepresentingOSPtr masterTimeBase
                                                   masterTimeBaseOSPtr
                                                   :masterGCOSPtr timeBaseOSPtr)
               (setValue 'master me masterTimeBase))))))

(define-handler master (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (or (getValue 'master me)
        (timeBaseMasterTimeBase me timeBaseOSPtr)
        (timeBaseMasterClock    me timeBaseOSPtr))))

;;;? To do: Dispose of all the callbacks, then reconnect them to the new timeBase.
(define-handler (setf master) (newValue QuickTimeTimeBase)
  (assumeOSPtrWithError (me timeBaseOSPtr)
    ;; If there's already a master clock, QT will call CloseComponent on it
    ;; so we'd better reflect that in the clock's sk8 object.
    (let ((master (getValue 'master me)))
      (when (and master
                 (inheritsFrom master QuickTimeClockComponentInstance)
                 (neq master newValue))
        (setf (instanceOSPtr master) nil)))
    (cond
     ((inheritsFrom newValue QuickTimeTimeBase)
      (if (movie me)
        (T_SetMovieMasterTimeBase    (mediaData (movie me)) (timeBaseOSPtr newValue))
        (T_SetTimeBaseMasterTimeBase (timeBaseOSPtr me)     (timeBaseOSPtr newValue))))
     ((inheritsFrom newValue QuickTimeClockComponentInstance)
      (if (movie me)
        (T_SetMovieMasterClock    (mediaData (movie me)) (instanceOSPtr newValue))
        (T_SetTimeBaseMasterClock (timeBaseOSPtr me)     (instanceOSPtr newValue))))
     (t
      (sk8-error IncorrectArgumentsError
                 :handlername "set master"
                 :arguments (list :to newValue))))
    (setValue 'master me newValue)))

(define-handler timeScale (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (let ((master (master me)))
      (if (inheritsFrom master QuickTimeClockComponentInstance)
        (T_GetTimeBaseTimeScale timeBaseOSPtr)
        (let ((combinedRate 1))
          (do ((prevTimeBase nil timeBase)
               (timeBase (master me) (master timeBase)))
              ((inheritsFrom timeBase QuickTimeClockComponentInstance)
               (* combinedRate (T_GetTimeBaseTimeScale (timeBaseOSPtr prevTimeBase))))
            (setf combinedRate (* combinedRate (rate timeBase)))))))))

(define-handler timeValue (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (T_GetTimeBaseTime timeBaseOSPtr)))

(define-handler (setf timeValue) (timeValue QuickTimeTimeBase)
  (assumeOSPtrWithError (me timeBaseOSPtr)
    (T_SetTimeBaseTime timeBaseOSPtr timeValue)
    timeValue))

(define-handler rate (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (T_GetTimeBaseRate timeBaseOSPtr)))

(define-handler (setf rate) (newRate QuickTimeTimeBase)
  (assumeOSPtrWithError (me timeBaseOSPtr)
    (T_SetTimeBaseRate timeBaseOSPtr newRate)
    newRate))

(define-handler effectiveRate (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (T_GetTimeBaseRate timeBaseOSPtr)))

(define-handler startTime (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (T_GetTimeBaseStartTime timeBaseOSPtr)))

(define-handler (setf startTime) (timeValue QuickTimeTimeBase)
  (assumeOSPtrWithError (me timeBaseOSPtr)
    (T_SetTimeBaseStartTime timeBaseOSPtr timeValue)
    timeValue))

(define-handler stopTime (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (T_GetTimeBaseStopTime timeBaseOSPtr)))

(define-handler (setf stopTime) (timeValue QuickTimeTimeBase)
  (assumeOSPtrWithError (me timeBaseOSPtr)
    (T_SetTimeBaseStopTime timeBaseOSPtr timeValue)
    timeValue))

#|
(define-handler rangePosition (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (sk8-multival-bind (timeValue status)
                       (T_GetTimeBaseStatus timeBaseOSPtr)
      status)))
|#

(define-handler repeating (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (case (logand (T_GetTimeBaseFlags timeBaseOSPtr)
                  (logior #$loopTimeBase
                          #$palindromeLoopTimeBase))
      (#.#$loopTimeBase           'SK8::loop)
      (#.#$palindromeLoopTimeBase 'palindrome)
      (0                           nil)
      (otherwise                  'SK8::loop))))
    
(define-handler (setf repeating) (newRepeating QuickTimeTimeBase)
  (assumeOSPtrWithError (me timeBaseOSPtr)
    (T_SetTimeBaseFlags timeBaseOSPtr
                        :flags (logior (cond
                                        ((eq newRepeating 'SK8::loop)            #$loopTimeBase)
                                        ((eq newRepeating 'palindrome) #$palindromeLoopTimeBase)
                                        ((eq newRepeating nil)                                0)
                                        (t (error "invalid repeating style")))
                                       (logandc2 (T_GetTimeBaseFlags timeBaseOSPtr)
                                                 (logior #$loopTimeBase
                                                         #$palindromeLoopTimeBase))))
    newRepeating))

(define-handler maintainTimeBaseZero (QuickTimeTimeBase)
  (assumeOSPtr (me timeBaseOSPtr)
    (not (zerop (logand (T_GetTimeBaseFlags timeBaseOSPtr)
                        #$maintainTimeBaseZero)))))
    
(define-handler (setf maintainTimeBaseZero) (newValue QuickTimeTimeBase)
  (assumeOSPtrWithError (me timeBaseOSPtr)
    (T_SetTimeBaseFlags timeBaseOSPtr
                        :flags (logior (if newValue #$maintainTimeBaseZero 0)
                                       (logandc2 (T_GetTimeBaseFlags timeBaseOSPtr)
                                                 #$maintainTimeBaseZero)))
    newValue))

#|
	Change History (most recent last):
	1  	 3/ 6/95	dy      	new
	2  	 3/ 8/95	dy      	set masterTimeBase and masterClock
	3  	 3/10/95	dy      	fixes including hopefully save/restore
	4  	 3/11/95	dy      	fix a typo in initializeFromStore
	5  	 3/11/95	dy      	
	6  	 3/21/95	dy      	try to fix store
	7  	 3/24/95	dy      	fix loading from store
	8  	 3/28/95	dy      	Try to fix preserve/restore.
	9  	 4/18/95	dy      	Back out masterTimeBase and masterClock, which should be merged (see comment).
							Added real property: zero so it's gettable as well as settable.
	10 	 4/28/95	dy      	fix timeScale and master
	2  	 6/12/95	dy      	If QTTimeBase object was created from scratch we have to save enough info to reconstitute it properly from the store.
	3  	 7/ 3/95	dy      	preserve now works
	3  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
