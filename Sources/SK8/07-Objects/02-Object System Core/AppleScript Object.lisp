;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package sk8dev)

(provide "APPLESCRIPTOBJECT")

;;; From the user's perspective an AppleScript objects consist of a script
;;; (a string with a setter and a getter) and the two methods, precompile
;;; and execute.  Precompile causes the object to do some of the work involved
;;; in executing an AppleScript, thus speeding up the time required for a
;;; subsequent execute.  Simply calling execute without an initial precompile
;;; will automatically do the precompile so any later calls to execute will
;;; be faster.
;;;
;;; The other important AppleScript related object from the user perspective
;;; is an AppleScriptResult.  By default if execution of an AppleScript returns
;;; a value that SK8 understands (e.g. number, string or pict) it will be converted
;;; into an appropriate SK8 object.  If the result of an AppleScript is not
;;; understood by SK8 then an AppleScriptResult is return.  An AppleScriptResult
;;; consists of a Handle and a Type (a string of an OSType indicating what kind of
;;; handle it is).  In toolbox terms this is roughly the same as an AEDesc.
;;; If the InterpretResult property of an AppleScript object is set to False
;;; then the result of execute is always an AppleScriptResult.
;;;
;;; Futures: In the future we may wish to add dialects.  This would probably imply
;;; a browser of some sort since the dialect codes are not standardized, but
;;; rather appear to depend on installation.  See IM-IAC pp 10-67 through 10-71.
;;;
;;; Internals: In order for component instances to be garbage collected properly
;;; requires some kind of hairy extra data structures.  The problem is that
;;; the script ids created during compilation and execution of an AppleScript
;;; must be disposed before the component is closed.  The execution script ids
;;; are dealt with by making the AppleScriptResult object independent of the
;;; component instance with AEDuplicateDesc.  The script id created by
;;; compilation is more difficult.  The current solution is to make two
;;; extra garbage collectable mac handles.  One contains the handle to the
;;; component instance and a reference count.  In the current implementation
;;; only one of this type of handle exists, but if dialects are added in
;;; the future they may be more of these.  The other extra handle contains a
;;; script id and the other handle.  Whenever one of these objects is created
;;; it increments the reference count.  The reference count is initialized to
;;; one.  The dispose functions for both types of machandles only dispose the handle
;;; with the reference count and close the component if the reference count is
;;; zero.  This guarantees that the script id has been properly disposed.

(new object :objectName "AppleScript" :project sk8
     :properties '((scripttext :value nil)
                   (interpretResult :value t)
                   (compiledScriptId :value nil :private)
                   (componentRecord :value nil :private))
     :undisposable t
     :prototype t)

;;; For storing the component handle and the reference count
(defrecord ComponentRecord
  (:component :ComponentInstance)
  (:referenceCount :long))

;;; One of these for each compiled script
(defrecord ScriptId
  (:componentRecord (:handle :ComponentInstance))
  (:id :long))

#|
(defparameter *log-file* "gc-log")

(defun log-format (&rest args)
  (when *log-file*
    (with-open-file (out-stream *log-file* :direction :output :if-exists :append :if-does-not-exist :create)
      (multiple-value-bind (secs mins hr day month) (get-decoded-time)
        (format out-stream "~%~a/~a ~a:~a:~a" month day hr mins secs)
        (apply #'format out-stream args))))
  (apply #'format t args))
|#

(defun extract-osaerror (errCode component)
  (if (eql errCode #$errOSAScriptError)
    (with-aedescs (errText)
      (let ((errResult (#_OSAScriptError component
                        #$kOSAErrorMessage
                        #$typeChar
                        errText)))
        (if (eql errResult #$noErr)
          (let ((aHandle (rref errText aedesc.dataHandle)))
            (if (handlep aHandle)
              (extract-string aHandle)
              errCode))
          errResult)))
    errCode))

(labels ((T_ComponentRecordDispose (aComponentRecord)
;           (log-format "~%T_ComponentRecordDispose: ~a, ~d"
;                       aComponentRecord (href aComponentRecord ComponentRecord.referenceCount))
           (when (and (handlep aComponentRecord)
                      (<= (decf (href aComponentRecord ComponentRecord.referenceCount)) 0))
;             (log-format "~%T_ComponentRecordDispose: closing component")
             (let ((component (href aComponentRecord ComponentRecord.component)))
               (unless (equal component (%null-ptr))
                 (checking-toolbox-error (:OSErr) (#_CloseComponent component))
                 (setf (href aComponentRecord ComponentRecord.component) (%null-ptr))))
             (#_DisposeHandle aComponentRecord))
           t)
         
         (T_ScriptIdDispose (aScriptId)
           (when (handlep aScriptId)
             (let* ((componentRec (href aScriptId ScriptId.componentRecord))
                    (component (and (handlep componentRec) (href componentRec ComponentRecord.component))))
               (when component
                 (checking-toolbox-error (:OSErr (extract-osaerror theError component))
                                         (#_OSADispose component (href aScriptId ScriptId.id)))
                 (T_ComponentRecordDispose componentRec)))
             (#_DisposeHandle aScriptId))
           t)

         ;;; Increments the reference count
         (T_IncASComponentRef (aComponentRecord)
;            (log-format "~%T_IncASComponentRef: ~a, ~d"
;                        aComponentRecord (href aComponentRecord ComponentRecord.referenceCount))
            (incf (href aComponentRecord ComponentRecord.referenceCount))))
  
  (defun T_OpenAppleScriptComponentGC ()
    "Returns :ComponentInstance GCOSptr for running AppleScripts.
Allocates an additional handle for storing a reference count to avoid
closing the component before all the script ids have been deallocated."
;    (log-format "~%T_OpenAppleScriptComponentGC")
    (let ((result (T_makeGCOSPtr (newRecordHandle :ComponentRecord)
                                 #'T_ComponentRecordDispose :terminateOnQuit t)))
      (when (handlep result)
        (setf (href result ComponentRecord.referenceCount) 1)
        (setf (href result ComponentRecord.component) (%null-ptr))
        (setf (href result ComponentRecord.component)
              (checking-toolbox-error (:Pointer :none)
                                      (#_OpenDefaultComponent #$kOSAComponentType #$kAppleScriptSubtype))))
      result))
  
  (defun T_AppleScriptCompileGC (anAppleScript source)
    (let* ((aScriptId (T_makeGCOSPtr (newRecordHandle :ScriptID)
                                     #'T_ScriptIdDispose :terminateOnQuit t))
           (aComponentRecord (componentRecord anAppleScript))
           (aComponent (and (handlep aComponentRecord) (href aComponentRecord ComponentRecord.component))))
      (when (and aComponent (handlep aScriptId))
        (setf (compiledScriptId anAppleScript) aScriptId)
        (rlet ((scriptID :long #$kOSANullScript))
          (checking-toolbox-error (:OSErr (extract-osaerror theError aComponent))
                                  (#_OSACompile aComponent source #$kOSAModeNull scriptID))
          (setf (href aScriptId scriptid.id) (%get-long scriptID))
          (setf (href aScriptId scriptid.componentRecord) aComponentRecord)
          ;        (log-format "~%T_AppleScriptCompileGC ~a, ~a" aComponentRecord aScriptId)
          (incf (href aComponentRecord ComponentRecord.referenceCount)))
        t))))

(flet ((T_AEDescDispose (anAEDescHandle)
;         (log-format "~%T_AEDescDispose")
         (with-dereferenced-handles ((anAEDesc anAEDescHandle))
           (checking-toolbox-error (:OSErr) (#_AEDisposeDesc anAEDesc)))
         (#_DisposeHandle anAEDescHandle)
         t))
  (defun T_NewAEDescHandleGC ()
;    (log-format "~%T_NewAEDescHandleGC")
    (let ((result (T_makeGCOSPtr (newRecordHandle :AEDesc)
                                 #'T_AEDescDispose)))
      (with-dereferenced-handles ((anAEDesc result))
        (checking-toolbox-error (:OSErr) (#_AECreateDesc #$typeNull (%null-ptr) 0 anAEDesc)))
      result)))

(new object :project sk8 :objectname "AppleScriptResult"
     :properties '((aeDesc :value nil :private))
     :undisposable t
     :prototype t)

(define-handler DescType (AppleScriptResult)
  (let ((theDesc (aeDesc me)))
    (and (handlep theDesc) (href theDesc aedesc.descriptorType))))

(define-handler ExtractHandle (AppleScriptResult)
  (let ((theDesc (aeDesc me)))
    (and (handlep theDesc) (href theDesc aedesc.dataHandle))))

;;; Store the component in a gc-able cons so it can be shared if it
;;; gets used repeatedly, but gets closed when nobody is using it.
(defparameter *applescript-component* (ccl::%cons-population nil))

(defun get-the-applescript-component ()
  (let ((result (car (ccl::population-data *applescript-component*))))
    (unless result
      (setq result (T_OpenAppleScriptComponentGC))
      (push result (ccl::population-data *applescript-component*)))
    result))

(define-handler initialize (AppleScript original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (setf (componentRecord me) (get-the-applescript-component)))

(define-handler (setf scripttext) (script AppleScript)
  (if (or (null script) (stringp script))
    (progn
      (setf (compiledScriptId me) nil)
      (setf (slot-value me 'scripttext) script))
    (sk8-error GeneralError :strings '("The script of an AppleScript must be either a string or false"))))

(define-handler precompile (AppleScript)
  (let ((textStr (scripttext me)))
    (with-aedescs (source)
      (let ((size (length textStr)))
        (%vstack-block (buff size)
          (dotimes (i size)
            (%put-byte buff (char-code (char textStr i)) i))
          (checking-toolbox-error (:OSErr) (#_AECreateDesc #$typeChar buff size source)))) ;;; I don't think this generates garbage - NJW 8/12/94
      (T_AppleScriptCompileGC me source))))

(new object :project sk8 :objectname "EmptyAppleScriptResult")

(defun extract-AppleScriptResult (aDesc aProject)
  (let ((final-result (T_NewAEDescHandleGC)))
    (with-dereferenced-handles ((final-ptr final-result))
      (checking-toolbox-error (:OSErr) (#_AEDuplicateDesc aDesc final-ptr)))
    (new AppleScriptResult :project aProject :aeDesc final-result)))

(defun extract-byte (aHandle)
  (with-dereferenced-handles ((shortPtr aHandle))
    (%get-byte shortPtr)))

(defun extract-short (aHandle)
  (with-dereferenced-handles ((shortPtr aHandle))
    (%word-to-int (%get-word shortPtr))))

(defun extract-long (aHandle)
  (with-dereferenced-handles ((longPtr aHandle))
    (%get-long longPtr)))

(defun extract-comp (aHandle)
  (with-dereferenced-handles ((compPtr aHandle))
    (dpb (%get-long compPtr 4) (byte 32 0)
         (ash (%get-long compPtr) 32))))

(defun extract-float (aHandle)
  (with-dereferenced-handles ((floatPtr aHandle))
    (%copy-float floatPtr)))

(defun extract-string (aHandle)
  (with-dereferenced-handles ((strPtr aHandle))
    (ccl::%str-from-ptr strPtr (T_GetHandleSize aHandle))))

(new QDPicture :objectName "AEDescPicture" :project sk8
     :properties '((aeDesc :value nil :private))
     :undisposable t
     :prototype t)

(defun extract-qdpicture (aDesc aProject)
  (let ((aedesc-copy (T_NewAEDescHandleGC)))
    (with-dereferenced-handles ((copy-ptr aedesc-copy))
      (checking-toolbox-error (:OSErr) (#_AEDuplicateDesc aDesc copy-ptr))
      (new AEDescPicture
           :project aProject
           :aeDesc aedesc-copy
           :handle (rref copy-ptr aedesc.datahandle)))))

(new object :objectName "AEDescAlias" :project sk8
     :properties '((aeDesc :value nil :private))
     :undisposable t
     :prototype t)

(defun extract-alias (aDesc aProject)
  (let ((aedesc-copy (T_NewAEDescHandleGC)))
    (with-dereferenced-handles ((copy-ptr aedesc-copy))
      (checking-toolbox-error (:OSErr) (#_AEDuplicateDesc aDesc copy-ptr))
      (new AEDescAlias
           :project aProject
           :aeDesc aedesc-copy
           :handle (rref copy-ptr aedesc.datahandle)))))

(defun extract-list (aDesc aProject)
  (rlet ((macListLength :long))
    (checking-toolbox-error (:OSErr)
                            (#_AECountItems aDesc macListLength))
    (let ((listLength (%get-long macListLength))
          (result nil))
      (rlet ((spareType ostype))
        (dotimes (i listLength)
          (with-aedescs (newDesc)
            (checking-toolbox-error (:OSErr)
                                    (#_AEGetNthDesc aDesc (- listLength i)
                                     #$typeWildCard spareType newDesc))
            (push (interpret-aedesc newDesc aProject) result))))
      result)))

(defun extract-record (aDesc aProject)
  (rlet ((macListLength :long))
    (checking-toolbox-error (:OSErr)
                            (#_AECountItems aDesc macListLength))
    (let ((listLength (%get-long macListLength))
          (result nil))
      (rlet ((spareType ostype))
        (dotimes (i listLength)
          (with-aedescs (newDesc)
            (checking-toolbox-error (:OSErr)
                                    (#_AEGetNthDesc aDesc (- listLength i)
                                     #$typeWildCard spareType newDesc))
            (push (list (%get-ostype spareType) (interpret-aedesc newDesc aProject))
                  result))))
      result)))

(defun interpret-aedesc (aDesc aProject)
  (let ((aDescType (rref aDesc aedesc.descriptorType)))
    (case aDescType
      (#.#$typeTrue t)
      (#.#$typeFalse nil)
      (#.#$typeNull sk8::EmptyAppleScriptResult)
      (t ;;; Types requiring a dataHandle
       (let ((descHandle (rref aDesc aedesc.dataHandle)))
         (if (handlep descHandle)
           (case aDescType
             (#.#$typeLongInteger (extract-long descHandle))
             (#.#$typeShortInteger (extract-short descHandle))
             (#.#$typeComp (extract-comp descHandle)) ;;; These are 64bit ints not complex numbers
             (#.#$typeBoolean (/= (extract-byte descHandle) 0))
             ((#.#$typeShortFloat #.#$typeExtended)
              (with-aedescs (tmpDesc)
                (checking-toolbox-error (:OSErr) (#_AECoerceDesc aDesc #$typeLongFloat tmpDesc))
                (extract-long (rref tmpDesc aedesc.dataHandle))))
             (#.#$typeLongFloat (extract-float descHandle))
             ((#.#$typeChar :|text| :|stxt|)
              (with-aedescs (tmpDesc)
                (checking-toolbox-error (:OSErr)
                                        (#_AECoerceDesc aDesc #$typeChar tmpDesc))
                (extract-string (rref tmpDesc aedesc.dataHandle))))

             (:pict (extract-qdpicture aDesc aProject))
             (#.#$typeAlias (extract-alias aDesc aProject))
             (#.#$typeAEList (extract-list aDesc aProject))
             (#.#$typeAERecord (extract-record aDesc aProject))
             (t (extract-AppleScriptResult aDesc aProject)))
           (extract-AppleScriptResult aDesc aProject))))))) ;;; If there isn't a handle at least return the type

(defun extract-the-result (result-id component asObj)
  (with-aedescs (resultDesc)
    (checking-toolbox-error (:OSErr (extract-osaerror theError component))
                            (#_OSACoerceToDesc component result-id #$typeWildCard #$kOSAModeNull resultDesc))
    (let ((aProject (project asObj)))
      (if (interpretResult asObj)
        (interpret-aedesc resultDesc aProject)
        (extract-AppleScriptResult resultDesc aProject)))))

(define-handler execute (AppleScript)
  (let* ((compRec (componentRecord me))
         (component (and (handlep compRec) (href compRec ComponentRecord.component))))
    (unless component
      (sk8-error GeneralError :strings '("Component instance is invalid.") :objects (list me)))
    (unless (compiledScriptId me)
      (precompile me))
    (let ((idRec (compiledScriptId me)))
      (assert idRec)
      (rlet ((result-id :OSAID -1))
        (let ((id (href idRec :ScriptId.id))
              (result nil))
          (unwind-protect
            (progn
              (checking-toolbox-error (:OSErr (extract-osaerror theError component))
                                      (#_OSAExecute component id 0 0 result-id))
              (setq result (extract-the-result (%get-long result-id) component me)))
            (let ((val (%get-long result-id)))
              (unless (eq val -1)
                (checking-toolbox-error (:OSErr (extract-osaerror theError component))
                                        (#_OSADispose component (%get-long result-id))))))
          result)))))

#|
	Change History (most recent last):
	1  	 9/12/94	nw      	AppleScript and related objects
	2  	 9/12/94	sidney  	
	1  	 9/12/94	sidney  	changed name of this file to AppleScript object.lisp
	2  	 9/15/94	nw      	Now uses new version of checking-toolbox-error and is more careful about checking for valid handles
	3  	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	4  	 2/16/95	sidney  	readable argument names for initialize handler
	5  	 3/17/95	till    	trap-wrapping, gethandlesize in extract-string
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
