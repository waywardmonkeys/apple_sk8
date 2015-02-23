;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  4-29-96   3:15 pm
                  SK8:::LOCKDATA SK8::BACKGROUND SK8::CREATIONTIME SK8::LASTRUNTIME SK8::PRIORITY
                  SK8::PROCESS SK8::PROCESSDATA SK8::PROCESSFUNCTION SK8::PROCESSLOCK SK8::PROCESSWAIT
                  SK8::STATE SK8::TEMPORARYPROCESS SK8::TOTALRUNTIME)

(provide "PROCESSES")

;;; _______________________________ 
;;; Process
;;; _______________________________ 

(new object :objectname "Process" :project sk8
     :properties '((ProcessData :value nil)))

;;; Valid initargs are background, priority, quantum, function and state.

(define-handler initialize (Process original isNew initArgs)
  (declare (ignore-if-unused isNew initArgs original))
  (call-next-method)
  ;; Create the process data structure. 
  (let ((name (or (initializerArgument initArgs :name :use t)
                  (objectName me)))
        (background (initializerArgument initArgs :background :default t :use t))
        (priority (initializerArgument initArgs :priority :default 0 :use t))
        (quantum (initializerArgument initArgs :quantum :default *default-quantum* :use t))
        (function (initializerArgument initArgs :processFunction :use t))
        (state (initializerArgument initArgs :state :use t)))
    (setf (ProcessData me)
          (make-process (or name "User Process")
                        :background-p background
                        :priority priority
                        :quantum quantum))
    (when function (setf (processFunction me) function))
    (when state (setf (state me) state))
    me))

(define-handler backGround (Process)
  (process-background-p (processData me)))

(define-handler (setf background) (newValue Process)
  (setf (process-background-p (processData me)) newValue))

(define-handler priority (Process)
  (process-priority (processData me)))

(define-handler (setf priority) (newValue Process)
  (setf (process-priority (processData me)) (max newValue 0)))

(define-handler processFunction (Process)
  (car (process-initial-form (processData me))))

(define-handler (setf processFunction) (newValue Process)
  (let ((pdata (processData me)))
    (process-preset pdata (fboundp newValue))
    ))

(define-handler localVirtualProperties (Process)
  '(background priority processFunction state))

;;; _______________________________ 
;;; Running and waiting...
;;; _______________________________ 

;;; Returns running, suspended, exhausted or stopped. 

(define-handler state (Process)
  (let ((val (nth-value 0 (read-from-string (process-whostate (processData me)) nil nil))))
    (when (eq val 'suspendedByUser)
      (setf val 'suspended))
    val))

(define-handler (setf state) (newValue Process)
  (if (processFunction me)
    (let* ((pdata (processData me))
           (suspendedByUser? (and (process-active-p pdata)
                                  (string-equal (process-whostate pdata) "SuspendedByUser"))))
      (case newValue
        (running (if suspendedByUser?
                   (process-unblock pdata)
                   (progn (process-preset pdata (car (process-initial-form pdata)))
                          (process-reset-and-enable pdata))))
        (suspended (process-block pdata "SuspendedByUser"))
        (exhausted (process-kill pdata)
                   (when suspendedByUser? (process-unblock pdata)))))
    (error "The process ~a must be given a processFunction before changing its state." me)))

(define-handler start (Process)
  (setf (state me) 'running))

(define-handler stop (Process)
  (setf (state me) 'exhausted))

;;; _______________________________ 
;;; Things that affect the process from within
;;; _______________________________ 

(define-sk8-function sk8::sleep nil (seconds)
  (sleep seconds))

;;; Makes the process wait for the time specified (in seconds) or until the function
;;; provided returns t. 

(defmacro seconds->sixtieths (seconds)
  `(truncate (* ,seconds 60)))

(define-sk8-function processWait nil (&key function time)
  (if time
    (if function
      (process-wait-with-timeout "WaitingByUserRequest" (seconds->sixtieths time) (fboundp function))
      (sleep time))
    (if function
      (process-wait "WaitingByUserRequest" (fboundp function))
      (error "Either a time amount or a wait function must be given to processWait."))))

;;; _______________________________ 
;;; Informational
;;; _______________________________ 

(define-handler lastRunTime (Process)
  (process-last-run-time (processData me)))

(define-handler totalRunTime (Process)
  (process-total-run-time (processData me)))

(define-handler creationTime (Process)
  (process-creation-time (processData me)))

;;; _______________________________ 
;;; Most convenient thing: with temporary process.
;;; _______________________________ 

(defmacro |WITH TEMPORARYPROCESS| ((&key (name "User Process") (priority 0) 
                                          (quantum *default-quantum*) (background t))
                                    &body body)
  `(process-run-function
    '(:name ,name :priority ,priority :quantum ,quantum :background-p ,background)
    #'(lambda () ,@body)))

;;; _______________________________ 
;;; Shared resources: locks.
;;; _______________________________ 

(new object :objectName "ProcessLock" :project sk8
     :properties '((lockData :value nil)))

(define-handler initialize (ProcessLock original isNew initArgs)
  (declare (ignore-if-unused original isNew initArgs))
  (call-next-method)
  (setf (lockData me) (make-lock))
  me)

(defmacro |WITH PROCESSLOCK| ((lock)  &body body)
  `(with-lock-grabbed ((processData ,lock))
     ,@body))

#|

;;; TESTING...

(new process :objectname "proc3" :project sk8)
(state sk8::proc3)
(processfunction sk8::proc3)
(setf (processFunction sk8::proc3) 'counting)
(setf (state sk8::proc3) 'running)
(setf (state sk8::proc3) 'suspended)
(setf (state sk8::proc3) 'exhausted)
(process-active-p (processData sk8::proc))
(process-kill (processData sk8::proc))

(new process :objectname "proc2" :project sk8)
(inspect (processData proc2))

(defvar pro (make-process "Test Process"))
(read-from-string (process-whostate pro) nil nil)

(defun counting ()
  (let ((count 0))
    (loop
      (incf count)
      (when (zerop (mod count 100000))
        (ed-beep))
      (when (= 1000000 count)
        (return)))))

;;; unwinds but does not start it.

(process-preset pro (fboundp 'counting))
(process-reset-and-enable pro)
(process-flush pro) ;; suspended indefinitely
(process-reset pro)
(process-block pro "Suspended by User")
(process-unblock pro)
(process-kill pro)

(new processLock :objectname "plock" :project sk8)

|#


#|
	Change History (most recent last):
	2  	 4/29/96	Hernan  	New file. Implements processes API for SK8 users. Should be
						loaded as part of the object system. Does not require 
						anything outside the Object System (besides support for
						processes, of course).
	3  	 5/ 8/96	Hernan  	???
	4  	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
