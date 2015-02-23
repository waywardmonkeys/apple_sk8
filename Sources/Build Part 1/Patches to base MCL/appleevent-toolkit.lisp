;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
;-*- Mode: Lisp; Package: CCL -*-

;;	Change History (most recent first):
;;  2 5/1/95   akh  copyright
;;  (do not edit before this line!!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appleevent-toolkit.lisp
;; Copyright 1990-1994 Apple Computer, Inc.
;; Copyright 1995 Digitool, Inc.
;;
;;
;;  useful functions for sending & processing appleevents
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change History
;;
;; 03/14/96 bill find-named-process uses #_IUCompString instead of #_CmpString.
;;               The latter is obsolete and generates a warning.
;; 02/09/96 bill wrap (without-event-processing ...) around the call to #_PPCBrowser
;;               in choose-process-dialog.
;; 01/17/96 bill #_DisposHandle => #_DisposeHandle
;; 03/18/92 bill :can-switch-later -> :can-switch-layer in docs
;; ------------- 2.0f3
;; 01/03/92 bill change the HyperCard script a little.
;; ------------- 2.0b4
;; 11/19/91 bill do-processes, find-process-with-signature,
;;          appleevent-filter-proc,
;;          send-eval-to-hypercard, send-dosc-to-hypercard
;; 11/15/91 bill Add a little documentation.
;; 09/13/91 bill Bogus ECASE clause in send-appleevent
;; 08/28/91 Getting Old Fast.
;; 08/24/91 Older.
;; 5/1/91 New.
;; 4/12/93 sidney Put in own package to avoid conflicts with other software

(defpackage AppleEvents (:use ccl common-lisp))  

; Note that the Appleevents docs are kind of misleading in terms of allocating &
; deallocating storage for appleevents. As it turns out, you have to provide storage
; for the AEDesc structure while the AE Manager will allocate storage for the
; contents when you call things like AECreateDesc. Hmmmm. The upshot is that the
; many of the following functions take "the-desc" as the first argument. This should
; be a record of type :AEDesc created either with make-record or rlet. This
; choice is left to the programmer so that you can manage the storage of these
; things appropriately. Note that you are responsible for both calling AEDisposeDesc
; *and* dispose-record (in that order please!) when appropriate!! In general, all
; records should ultimately be disposed, and any descriptor that you called create-xxx
; on should also be disposed of with AEDisposeDesc first. I hope this makes sense.
; 

(in-package :appleevents)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((syms '(create-appleevent choose-appleevent-target choose-process-dialog
            create-self-target create-named-process-target create-psn-target 
            do-processes find-named-process find-process-with-signature
            create-alias-list send-appleevent appleevent-filter-proc
            get-transaction-id get-return-id get-event-class get-error-number
            create-odoc create-pdoc create-oapp create-quit create-eval create-dosc
            send-eval send-dosc send-eval-to-hypercard send-dosc-to-hypercard)))
    (export syms :appleevents)
    (import syms :ccl)
    (export syms :ccl)
  ))

; create-appleevent
; Create an apple event
;
; the-desc         An AEDesc record
; class            An OSType. e.g. :|aevt|
; id               An OSType. e.g. :|odoc|
; target           An AEDesc record as initialized by e.g. choose-appleevent-target
; return-id        an integer.  You'll ususally want the default.
; transaction-id   an integer.  IBID.
;
; Returned value:  the-desc
;
(defun create-appleevent (the-desc class id target &key 
                                   (return-id #$kAutoGenerateReturnID)
                                   (transaction-id #$kAnyTransactionID))
  (ae-error (#_AECreateAppleEvent class id target return-id transaction-id the-desc))
  the-desc)

; choose-appleevent-target
; Pop up the PPCBrowser to choose an appleevent target.
;
; the-desc         An AEDesc record.
; prompt           A string. Appears at the top center of the dialog.
; title            A string. Appears over the list of applications.
;
; Returned value: the-desc
;
(defun choose-appleevent-target (the-desc &key prompt title)
  ; You must eventually AEDisposeDesc the result of choose-appleevent-target!
  (rlet ((port-info :PortInfoRec)
         (target-id :TargetID))
    (choose-process-dialog
     :prompt prompt :title title 
     :location-name-record (rref target-id TargetID.location :storage :pointer)
     :port-info-record port-info)
    (setf (pref target-id TargetID.name) (pref port-info PortInfoRec.name))
 ;; For some reason, in some environments #$typeTargetID comes out as :TARG instead of
 ;; as :|targ|, which breaks things. So here it is hardcoded in order to
 ;; make sure that it works. That's a pretty awful thing to do
 ;; was    (ae-error (#_AECreateDesc #$typeTargetID target-id
    (ae-error (#_AECreateDesc :|targ| target-id
               #.(record-length :TargetID) the-desc))
    the-desc))

; choose-process-dialog
; Pop up the PPC Browser dialog to select a process.
;
; prompt                string. Appears at the top center of the dialog
; title                 string. Appears over the list of applications.
; default-specified     boolean. If true, location-name-record & port-info-record
;                       describe the default value which will be highlighted when
;                       the dialog first appears.
; location-name-record  A LocationNameRec record or NIL. If NIL, the record
;                       will be allocated for you.
; port-info-record      A PortInfoRec record or NIL.  If NIL, the record will
;                       be allocated for you.
; port-filter           A pointer to a Pascal function that will provide filtering.
;                       The function takes two arguments: location-name-rec &
;                       port-info-rec and returns a boolean: true if the process
;                       should be displayed.  The default is no filterring.
;                       See IM VI for more info.
; npb-type              NIL or a string of length 32 or less.  If non-NIL, match
;                       only servers with the given Name Binding Protocol type.
;
; Returns two values:   1) location-name-rec
;                       2) port-info-rec
;
(defun choose-process-dialog (&key prompt title default-specified location-name-record 
                                   port-info-record (port-filter (%null-ptr)) nbp-type)
  (declare (dynamic-extent port-filter))
  (let ((location-name-allocated nil)
        (port-info-allocated nil)
        (TheError))
    (unless (and location-name-record port-info-record)
      (setq default-specified nil))
    (unless location-name-record 
      (setq location-name-record (make-record :LocationNameRec)
            location-name-allocated t))
    (unless port-info-record
      (setq port-info-record (make-record :PortInfoRec)
            port-info-allocated t))
    (%stack-block ((prompt-ptr 256)
                   (title-ptr 256)
                   (nbp-type-ptr 256))
      (if prompt 
        (%put-string prompt-ptr prompt)
        (%setf-macptr prompt-ptr (%null-ptr)))
      (if title
        (%put-string title-ptr title)
        (%setf-macptr title-ptr (%null-ptr)))
      (if nbp-type
        (%put-string nbp-type-ptr nbp-type)
        (%setf-macptr nbp-type-ptr (%null-ptr)))
      (without-event-processing
        (setq TheError (#_ppcbrowser prompt-ptr title-ptr default-specified location-name-record
                     port-info-record port-filter nbp-type-ptr)))
      (cond ((zerop TheError)
             (values location-name-record port-info-record))
            (t
             (when location-name-allocated
               (dispose-record location-name-record :LocationNameRec))
             (when port-info-allocated
               (dispose-record port-info-record :PortInfoRec))
             (if (= TheError #$userCanceledErr)
               (cancel)
               (%err-disp TheError)))))))

; create-self-target
; Create an appleevent target to the current application.
;
; the-desc      An AEDesc record.
;
; Return value: the-desc
;
(defun create-self-target (&optional the-desc)
  (unless the-desc (setq the-desc (make-record :AEDesc)))
  (rlet ((psn :ProcessSerialNumber 
              :highLongOfPSN 0
              :lowLongOfPSN #$kCurrentProcess))
    (ae-error
      (#_AECreateDesc #$typeProcessSerialNumber psn #.(record-length ProcessSerialNumber)
       the-desc))
    the-desc))

; create-named-process-target
; Create an appleEvent target to the process (application) with the given name.
;
; the-desc      An AEDesc record or NIL to allocate one here.
; name          A string: the name of the process.
;
; Return value: the-desc
;
(defun create-named-process-target (the-desc name)
  (multiple-value-bind (psnhigh psnlow)
                       (find-named-process name)
    (unless psnhigh
      (error "Can't find process named: ~s" name))
    (create-psn-target the-desc psnhigh psnlow)))

; create-signature-target
; Create an appleEvent target to the process (application) with the given signature.
;
; the-desc      An AEDesc record or NIL to allocate one here.
; signature     An OSType, e.g. :WILD for HyperCard or :CCL2 for MCL.
;
; Return value: the-desc
;
(defun create-signature-target (the-desc signature)
  (multiple-value-bind (psnhigh psnlow)
                       (find-process-with-signature signature)
    (unless psnhigh
      (error "Can't find process with signature: ~s" signature))
    (create-psn-target the-desc psnhigh psnlow)))

; create-psn-target
; Create an AppleEvent target from a Process Serial Number
;
; the-desc      An AEDesc record.
; psnhigh       An integer: the high 32 bits of the process serial number.
; psnlow        An integer: the low 32 bits of the process serial number.
;
; Return value: the-desc
;
(defun create-psn-target (the-desc psnhigh psnlow)
  (rlet ((psn ProcessSerialNumber))
    (rset psn ProcessSerialNumber.HighLongOfPSN psnhigh)
    (rset psn ProcessSerialNumber.LowLongOfPSN psnlow)
    (rset the-desc AEaDdressDesc.deScriptorType #$typeProcessSerialNumber)
    (rset the-desc aeAddressDesc.datAhandle
          (rlet ((temp :pointer))
            (let ((err (#_PtrToHand psn temp #.(record-length ProcessSerialNumber))))
              (unless (eql #$NoErr err)
                (ccl::%err-disp err)))
            (%get-ptr temp))))
  the-desc)

; do-processes
;
; Executes body with PSN-VAR bound to a ProcessSerialNumber record
; for each active process.
; If PSN-VAR is a list of two symbols, the first will be bound to a ProcessSerialNumber
; record, and the second will be bound to a ProcessInfoRec record for that process.
(defmacro do-processes (psn-var &body body)
  (let ((thunk (gensym))
        inforec-var inforec-tail)
    (when (listp psn-var)
      (setq inforec-var (cadr psn-var)
            inforec-tail (list inforec-var)
            psn-var (car psn-var)))
    `(let ((,thunk #'(lambda (,psn-var ,@inforec-tail)
                       (declare (type macptr ,psn-var ,@inforec-tail))
                       ,@body)))
       (declare (dynamic-extent ,thunk))
       (%do-processes ,thunk ,(not (null inforec-var))))))

(defun %do-processes (thunk inforec-p)
  (rlet ((psn :ProcessSerialNumber)
         (inforec :ProcessInfoRec)
         (fss :fsspec)
         (procname (:string 32)))
    (setf (pref psn :ProcessSerialNumber.highLongOfPSN) #$kNoProcess)
    (setf (pref psn :ProcessSerialNumber.lowLongOfPSN) #$kNoProcess)
    (when inforec-p
      (setf (pref inforec :ProcessInfoRec.processName) procname
            (pref inforec :ProcessInfoRec.processAppSpec) fss))
    (loop
      (unless (eql #$NoErr (#_GetNextProcess psn)) (return))
      (if inforec
        (progn
          (setf (pref inforec :processInfoRec.ProcessInfoLength) #x3c)
          (unless (eql #$NoErr (#_GetProcessInformation psn inforec)) (return))
          (funcall thunk psn inforec))
        (funcall thunk psn)))))

; find-named-process
; Find the process with the given name.
;
; name                  A string: the name of the process
;
; Returns two values:  1) psnhigh - the high 32 bits of the process serial number
;                      2) psnlow  - the low 32 bits of the process serial number
;
; If there is no process with the given name, returns NIL.
;
(defun find-named-process (name)
  (with-pstrs ((nameptr name))
    (let* ((procname (%null-ptr)))
      (declare (dynamic-extent procname))
      (do-processes (psn inforec)
        (%setf-macptr procname (pref inforec :processInfoRec.ProcessName))
        (when (eql 0 (#_IUCompString procname nameptr))
          (return-from find-named-process
            (values (rref psn :ProceSsSerialNumber.highLongOfPSN)
                    (rref psn :ProceSsSerialNumber.lowLongOfPSN))))))))

; find-process-with-signature
; Find a process with the given signature
;
; signature             An OSTYPE: a four-character string or symbol.
;
; Returns two values:  1) psnhigh - the high 32 bits of the process serial number
;                      2) psnlow  - the low 32 bits of the process serial number
;
; If there is no process with the given name, returns NIL.
;
(defun find-process-with-signature (signature)
  (%stack-block ((ostype 4))
    (setf (%get-ostype ostype) signature)
    (let ((high (%get-word ostype))
          (low (%get-word ostype 2)))
      (symbol-macrolet ((offset (get-field-offset :ProcessInfoRec.processSignature)))
        (do-processes (psn inforec)
          (when (and (eql high (%get-word inforec offset))
                     (eql low (%get-word inforec (+ offset 2))))
            (return-from find-process-with-signature
              (values (rref psn :ProceSsSerialNumber.highLongOfPSN)
                      (rref psn :ProceSsSerialNumber.lowLongOfPSN)))))))))

; create-alias-list
; Create an AEDesc record containing a list of pathname aliases.
;
; the-desc       an AEDesc record
; paths          a list of pathnames (or strings)
;
; Return value:  the-desc
;
(defun create-alias-list (the-desc paths)
  (ae-error (#_AECreateList (%null-ptr) 0 nil the-desc))
  (rlet ((alias :aliashandle))
    (dolist (path paths)
      (let ((namestring (mac-namestring path)))
        (with-cstrs ((cname namestring))
          (#_NewAliasMinimalFromFullPath (length namestring) cname (%null-ptr) (%null-ptr)
           alias)))
      (unwind-protect
        (with-dereferenced-handles ((aliasptr (%get-ptr alias)))
          (ae-error (#_AEPutPtr the-desc 0 #$typeAlias aliasptr
                     (rref aliasptr aliasrecord.aliassize))))
        (#_DisposeHandle :errchk (%get-ptr alias)))))
  the-desc)

; A filerproc to use with send-appleevent in the case that you
; want to handle AppleEvents while waiting for a response.
; This filter handles all AppleEvents.
(defpascal appleevent-filter-proc (:ptr event :long returnID
                                   :long transactionID :ptr AddressDesc
                                   :word)
  (declare (ignore event returnID transactionID AddressDesc))
  ; Return true: process the AppleEvent
  -1)

; send-appleevent
; Send an AppleEvent and possibly wait for a reply.
;
; the-appleevent    An AEDesc record for an Apple Event.
; the-reply         An AEDesc record. Reply is put here.
; reply-mode        One of :no-reply :queue-reply, :wait-reply.
;                   Default is :no-reply.
; interact-mode     One of NIL, :never-interact, :can-interact, :always-interact.
; can-switch-layer  boolean.  True if user interaction is allowed to automatically
;                   switch layers.  Default: NIL
; dont-reconnect    boolean.  True if #_AESend should not attempt to reconnect if
;                   it gets a sessClosedErr from the PPC toolbox. Default: NIL.
; want-receipt      boolean. True if you want a receipt sent when the receiver
;                   gets the AppleEvent. Default: NIL
; priority          #$kAENormalPriority puts the apple event at the end of the
;                   event queue.
;                   #$kAEHighPriority puts it at the beginning of the event queue.
;                   Default: #$kAENormalPriority.
; timeout           Number of tickes to wait before a timeout error when reply-mode
;                   is :wait-reply or want-receipt is true.
;                   #$kAEDefaultTimeout "tells the Apple Event manager to provide
;                   an appropriate timeout."  #$kNoTimeOut means wait forever.
;                   Default: #$kAEDefaultTimeout.
; idleproc
; filterproc        Pascal routines.  Use the defaults unless you have read and
;                   understand the Inside Mac description.
;
; Return value:     the-reply if reply-mode is :wait-reply.  NIL, otherwise.
;
; Note that the default filterproc causes processing of incoming AppleEvents to be
; delayed until #_AESend returns.  Use appleevent-filter-proc to process incoming
; AppleEvents right away.
; Note also that if you use :wait-reply mode and the default idleproc, only idle,
; update, activate, and operating-system events will be handled until the #_AESend
; returns (abort will still work).  You should probably put up a watch cursor unless
; you know it will be very fast.
;
(defun send-appleevent (the-appleevent the-reply &key
                                       (reply-mode :no-reply) (interact-mode nil)
                                       (can-switch-layer nil) (dont-reconnect nil)
                                       (want-receipt nil) (priority #$kAENormalPriority)
                                       (timeout #$kAEDefaultTimeout)
                                       (idleproc appleevent-idle)
                                       filterproc)
  (let ((mode (+ (ecase reply-mode
                   (:no-reply #$kAENoReply)
                   (:queue-reply #$kAEQueueReply)
                   (:wait-reply #$kAEWaitReply))
                 (ecase interact-mode
                   ((nil) 0)
                   (:never-interact #$kAENeverInteract)
                   (:can-interact #$kAECanInteract)
                   (:always-interact #$kAEAlwaysInteract))
                 (if can-switch-layer #$kAECanSwitchLayer 0)
                 (if dont-reconnect #$kAEDontReconnect 0)
                 (if want-receipt #$nreturnreceipt 0))))
    (ae-error 
      (let ((ccl::*inside-aesend* t)
            (res (#_AESend the-appleevent the-reply mode priority 
                  timeout idleproc (or filterproc (%null-ptr)))))
        (if (eq res #$errAEWaitCanceled)        ; be silent about aborts
          #$NoErr
          res)))
    (when (eq reply-mode :wait-reply)
      (check-reply-error the-reply)
      the-reply)))

; check-reply-error
;
; Check an Apple Event reply for errors.  Signal an error if there is one.
;
; reply      An AEDesc record containing the reply to an Apple Event.
;
(defun check-reply-error (reply)
  (let ((error-number (get-error-number reply nil)))
    (when (and error-number
               (not (= error-number #$noerr)))
      (error (make-condition 'appleevent-error
                             :oserr error-number
                             :error-string (get-error-string reply nil))))))
    

; some common toplevel thingies:

(defun get-transaction-id (the-desc &optional (errorp t))
  (ccl::ae-get-attribute-longinteger the-desc #$keyTransactionIDAttr errorp))

(defun get-return-id (the-desc &optional (errorp t))
  (ccl::ae-get-attribute-longinteger the-desc #$keyReturnIDAttr errorp))

(defun get-event-class (the-desc &optional (errorp t))
  (ccl::ae-get-attribute-type the-desc #$keyEventClassAttr errorp))

(defun get-event-id (the-desc &optional (errorp t))
  (ccl::ae-get-attribute-type the-desc #$keyEventIDAttr errorp))

(defun get-sender-address (the-desc result-desc &optional (errorp t))
  (ccl::ae-errorp-handler errorp
    (#_AEGetAttributeDesc the-desc #$keyAddressAttr
     #$typeWildCard result-desc)
    result-desc))

(defun get-error-number (the-desc &optional (errorp t))
  (ccl::ae-get-parameter-longinteger the-desc #$keyErrorNumber errorp))

(defun get-error-string (the-desc &optional (errorp t))
  (ccl::ae-get-parameter-char the-desc #$keyErrorString errorp))


; Create four core appleevents:
(defun create-odoc (the-desc the-target paths &rest create-keywords)
  (declare (dynamic-extent create-keywords))
  (with-aedescs (alias-list)
    (apply 'create-appleevent the-desc #$kCoreEventClass #$kAEOpenDocuments the-target
           create-keywords)
    (create-alias-list alias-list paths)
    (ae-error (#_AEPutParamDesc the-desc #$keyDirectObject alias-list))))

(defun create-pdoc (the-desc the-target paths &rest create-keywords)
  (declare (dynamic-extent create-keywords))
  (with-aedescs (alias-list)
    (apply 'create-appleevent the-desc #$kCoreEventClass #$kAEPrintDocuments the-target
           create-keywords)
    (create-alias-list alias-list paths)
    (ae-error (#_AEPutParamDesc the-desc #$keyDirectObject alias-list))))

(defun create-oapp (the-desc the-target &rest create-keywords)
  (apply 'create-appleevent the-desc #$kCoreEventClass #$kAEOpenApplication the-target
         create-keywords))

(defun create-quit (the-desc the-target &rest create-keywords)
  (declare (dynamic-extent create-keywords))
  (apply 'create-appleevent the-desc #$kCoreEventClass #$kAEQuitApplication the-target
         create-keywords))


; Here's Eval and dosc which Hypercard and Lisp understand:
; Note the Hypercard, bizarely enough, distinuguishes between functions which
; return values (i.e. appleevent eval) and procedures which do side-effects
; (i.e. appleevent 'dosc')... You have to use the appropriate one.
; Toolserver understands dosc, but in contradiction to hypercard, treats it
; like an eval, returning a string answer in the reply if the output was
; directed to dev:console.

(defun create-eval (the-desc the-target the-string &rest create-keywords)
  (declare (dynamic-extent create-keywords))
  (apply 'create-appleevent the-desc :|misc| :|eval| the-target
         create-keywords)
  (ccl::ae-put-parameter-char the-desc #$keyDirectObject the-string))

(defun create-dosc (the-desc the-target the-string &rest create-keywords)
  (declare (dynamic-extent create-keywords))
  (apply 'create-appleevent the-desc :|misc| :|dosc| the-target
         create-keywords)
  (ccl::ae-put-parameter-char the-desc #$keyDirectObject the-string))


; Here's simple stripped down sending functions for eval and dosc

(defun send-eval (command-string program-name)
  (with-aedescs (appleevent reply target)
    (create-named-process-target target program-name)
    (create-eval appleevent target command-string)
    (send-appleevent appleevent reply :reply-mode :wait-reply)
    (ccl::ae-get-parameter-char reply #$keyDirectObject nil)))

(defun send-dosc (command-string program-name)
  (with-aedescs (appleevent reply target)
    (create-named-process-target target program-name)
    (create-dosc appleevent target command-string)
    (send-appleevent appleevent reply :reply-mode :wait-reply)
    (ccl::ae-get-parameter-char reply #$keyDirectObject nil)))

(defun send-eval-to-hypercard (command-string)
  (with-aedescs (appleevent reply target)
    (create-signature-target target :WILD)
    (create-eval appleevent target command-string)
    (send-appleevent appleevent reply :reply-mode :wait-reply)
    (ccl::ae-get-parameter-char reply #$keyDirectObject nil)))

; If you wait for HyperCard to finish and it needs to display a dialog,
; MCL will become unresponsive until you abort.
; This is because #_AESend only passes update, activate, idle, & OS events
; to the idle function.
(defun send-dosc-to-hypercard (command-string &optional (wait? nil))
  (with-aedescs (appleevent reply target)
    (create-signature-target target :WILD)
    (create-dosc appleevent target command-string)
    (send-appleevent appleevent reply
                     :reply-mode (if wait? :wait-reply :no-reply))
    (if wait?
      (ccl::ae-get-parameter-char reply #$keyDirectObject nil))))

#|

; here's some testing code:

(with-aedescs (event reply target)
  (choose-appleevent-target target)
  (create-oapp event target)
  (send-appleevent event reply :reply-mode :no-reply))

(with-aedescs (event reply target)
  (create-self-target target)
  (create-oapp event target)
  (send-appleevent event reply :reply-mode :no-reply))

; If you choose HyperCard 2.0 here, you'll get the answer "3"
(with-aedescs (event reply target)
  (choose-appleevent-target target)
  (create-eval event target "1 + 2")
  (send-appleevent event reply :reply-mode :wait-reply)
  (ccl::ae-get-parameter-char reply #$keyDirectObject t))


; Communucating with HyperCard
(send-eval-to-hypercard "5*7")

(send-dosc-to-hypercard "put \"Hello from MCL!\"")

; To communicate from HyperCard to MCL, try typing the following two lines
; to the message box (after loading the file "ccl:examples;eval-server").
;
;  request "(print (+ 1 2))" from program "MCL 2.0"
;  put it
;
; "3" will be printed in MCL's Listener and in HyperCard's message box.

|#

(provide :appleevent-toolkit)

#|
	Change History (most recent last):
	1	4/12/93	SIdney	Move this file from examples folder and put the
				code in its own package instead of ccl
	2	4/12/93	SIdney	Workaround something wrong with definition of
				a constant #$typeTargetID which for some reason
				sometimes comes out as :TARG when it should be
				:|targ|. I'm not sure why it does that.
	1	3/29/94	sidney	Move appleEvent support to base MCL
	2  	 6/13/96	sidney  	update with changes made to corresponding file in examples folder in MCL3.9
	3  	 6/28/96	sidney  	fix a glitch
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
