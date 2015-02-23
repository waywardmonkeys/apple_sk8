(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;;;;;;;;;;;;;;;;;;;;;
;;; Component Manager
;;;;;;;;;;;;;;;;;;;;;;

(defvar_X TC_nilComponentInstance (ccl:%null-ptr) :private? t :private? t :project SK8)

(defun initialize-components-trap-null-ptr-values ()
  "Called at startup time."
  (setf TC_nilComponentInstance (ccl:%null-ptr))
  )

(defun preserve-components-trap-null-ptr-values ()
  "Called at startup time."
  (setf TC_nilComponentInstance nil)
  )


;;; Components
(defconstant_X TC_gestaltComponentMgr #$gestaltComponentMgr :private? t :project SK8) ; :|cpnt|
(defconstant_X TC_kAnyComponentType #$kAnyComponentType :private? t :project SK8) ; 0
(defconstant_X TC_kAnyComponentSubType #$kAnyComponentSubType :private? t :project SK8) ; 0
(defconstant_X TC_kAnyComponentManufacturer #$kAnyComponentManufacturer :private? t :project SK8) ; 0
(defconstant_X TC_kAnyComponentFlagsMask #$kAnyComponentFlagsMask :private? t :project SK8) ; 0
(defconstant_X TC_cmpWantsRegisterMessage #$cmpWantsRegisterMessage :private? t :project SK8) ; #X80000000
(defconstant_X TC_componentDoAutoVersion #$componentDoAutoVersion :private? t :project SK8) ; 1
(defconstant_X TC_componentWantsUnregister #$componentWantsUnregister :private? t :project SK8) ; 2
(defconstant_X TC_componentAutoVersionIncludeFlags #$componentAutoVersionIncludeFlags :private? t :project SK8) ; 4
(defconstant_X TC_kComponentOpenSelect #$kComponentOpenSelect :private? t :project SK8) ; -1 ComponentInstance for this open
(defconstant_X TC_kComponentCloseSelect #$kComponentCloseSelect :private? t :project SK8) ; -2 ComponentInstance for this close
(defconstant_X TC_kComponentCanDoSelect #$kComponentCanDoSelect :private? t :project SK8) ; -3 selector # being queried
(defconstant_X TC_kComponentVersionSelect #$kComponentVersionSelect :private? t :project SK8) ; -4 no params
(defconstant_X TC_kComponentRegisterSelect #$kComponentRegisterSelect :private? t :project SK8) ; -5 no params
(defconstant_X TC_kComponentTargetSelect #$kComponentTargetSelect :private? t :project SK8) ; -6 ComponentInstance for top of call chain
(defconstant_X TC_kComponentUnregisterSelect #$kComponentUnregisterSelect :private? t :project SK8) ; -7 no params
(defconstant_X TC_defaultComponentIdentical #$defaultComponentIdentical :private? t :project SK8) ; 0
(defconstant_X TC_defaultComponentAnyFlags #$defaultComponentAnyFlags :private? t :project SK8) ; 1
(defconstant_X TC_defaultComponentAnyManufacturer #$defaultComponentAnyManufacturer :private? t :project SK8) ; 2
(defconstant_X TC_defaultComponentAnySubType #$defaultComponentAnySubType :private? t :project SK8) ; 4
(defconstant_X TC_defaultComponentAnyFlagsAnyManufacturer #$defaultComponentAnyFlagsAnyManufacturer :private? t :project SK8) ; (+ #$defaultComponentAnyFlags #$defaultComponentAnyManufacturer)
(defconstant_X TC_defaultComponentAnyFlagsAnyManufacturerAnySubType #$defaultComponentAnyFlagsAnyManufacturerAnySubType :private? t :project SK8) ; (+ #$defaultComponentAnyFlags (+ #$defaultComponentAnyManufacturer #$defaultComponentAnySubType))
(defconstant_X TC_invalidComponentID #$invalidComponentID :private? t :project SK8) ; -3000
(defconstant_X TC_validInstancesExist #$validInstancesExist :private? t :project SK8) ; -3001
(defconstant_X TC_componentNotCaptured #$componentNotCaptured :private? t :project SK8) ; -3002
(defconstant_X TC_componentDontRegister #$componentDontRegister :private? t :project SK8) ; -3003
(defconstant_X TC_badComponentInstance #$badComponentInstance :private? t :project SK8) ; #X80008001
(defconstant_X TC_badComponentSelector #$badComponentSelector :private? t :project SK8) ; #X80008002


;;; Some Component Manager Utilities

(defun_X T_newComponentDescriptionPtr :private ()
  "Returns a new :ComponentDescription struct GCHandle.
Not a trap.
See IM-More Mac Toolbox p. 6-38"
  (newRecordPtrClear :ComponentDescription))

(defun_X T_newComponentDescriptionGCHandle :private ()
  "Returns a new :ComponentDescription struct GCHandle.
Not a trap.
See IM-More Mac Toolbox p. 6-38"
  (newRecordGCHandleClear :ComponentDescription))

(defun_X T_newComponentDescriptionGCPtr :private ()
  "Returns a new :ComponentDescription struct pointer.
Not a trap.
See IM-More Mac Toolbox p. 6-38"
  (newRecordGCPtrClear :ComponentDescription))

;;; The Component Manager Traps

(defun_X T_FindNextComponent :private (aComponent looking)
  "Returns :Component.
Pass T_nil as aComponent arg to find first component.
Raises error if trap returns nil.
See IM-More Mac Toolbox p. 6-42"
  (checking-toolbox-error (:Pointer :none) (#_FindNextComponent aComponent looking)))

(defun_X T_CountComponents :private (looking)
  "Returns :signed-long.
See IM-More Mac Toolbox p. 6-43"
  (#_CountComponents looking))

(defun_X T_GetComponentListModSeed :private ()
  "Returns :signed-long.
See IM-More Mac Toolbox p. 6-44"
  (#_GetComponentListModSeed))

(defun_X T_CloseComponent :private (aComponentInstance)
  "See IM-More Mac Toolbox p. 6-47"
  (checking-toolbox-error
   (:OSErr) (#_CloseComponent aComponentInstance))
  t ; T is required by the GC reclamation code
  )

(defun_X T_OpenDefaultComponentGC :private (componentType componentSubType)
  "Returns :ComponentInstance GCOSptr.
CloseComponent is called automatically when the returned is garbage-collected.
Do not call CloseComponent explicitly!
Raises error if trap returns nil.
See IM-More Mac Toolbox p. 6-45"
  (T_makeGCOSPtr
   (checking-toolbox-error (:Pointer :none) (#_OpenDefaultComponent componentType componentSubType))
   'T_CloseComponent
   :terminateOnQuit t))

(defun_X T_OpenComponentGC :private (aComponent)
  "Returns :ComponentInstance GCOSptr.
CloseComponent is called automatically when the returned is garbage-collected.
Do not call CloseComponent explicitly!
Raises error if trap returns nil.
See IM-More Mac Toolbox p. 6-46"
  (T_makeGCOSPtr
   (checking-toolbox-error (:Pointer :none) (#_OpenComponent aComponent))
   'T_CloseComponent
   :terminateOnQuit t))

(defun_X T_GetComponentInfoGC :private (aComponentOrInstance &key
                                              (ComponentDescriptionGCHandle t)
                                              (ComponentName t)
                                              (ComponentInfo t)
                                              (ComponentIconGCHandle t))
  "Returns multiple values:
  ComponentDescriptionGCHandle
  componentName (a possibly empty String)
  componentInfo (a possibly empty String)
  componentIconGCHandle (an Icon Record Handle), possibly empty
Each GCHandle keyword argument can take 3 possible values:
* a Handle or GCHandle - NOT a Ptr
   This handle will be filled in and returned as the corresponding return value
* t
   A new GCHandle will be allocated for you and filled in.
   This is the default.
* nil
   The corresponding return value will be nil
The ComponentName ComponentInfo arguments can be set to nil to indicate
that the information in the corresponding return value is not needed,
in which case that return value will contain nil.
See IM-More Mac Toolbox p. 6-48"
  (when (eql t ComponentDescriptionGCHandle)
    (setq ComponentDescriptionGCHandle (T_newComponentDescriptionGCHandle)))
  (when (eql t ComponentIconGCHandle)
    (setq componentIconGCHandle (T_NewHandleGC)))
  (let ((componentNameResult nil)
        (componentInfoResult nil)
        (componentNameHandle TC_nil)
        (componentInfoHandle TC_nil))
    (unwind-protect
      (progn
        (when ComponentName (setq componentNameHandle (T_NewHandle 256)))
        (when ComponentInfo (setq componentInfoHandle (T_NewHandle 256)))
        (if ComponentDescriptionGCHandle
          (with-dereferenced-handles ((cdPtr ComponentDescriptionGCHandle))
            (checking-toolbox-error (:OSErr)
                                    (#_GetComponentInfo
                                     aComponentOrInstance
                                     cdPtr
                                     componentNameHandle
                                     componentInfoHandle
                                     (or componentIconGCHandle TC_nil)
                                     )))
          (rlet ((cdJunkPtr :ComponentDescription))
            (checking-toolbox-error (:OSErr)
                                    (#_GetComponentInfo
                                     aComponentOrInstance
                                     cdJunkPtr
                                     componentNameHandle
                                     componentInfoHandle
                                     (or componentIconGCHandle TC_nil)
                                     ))))
        (when ComponentName
          (setq componentNameResult (%get-string componentNameHandle)))
        (when ComponentInfo
          (setq componentInfoResult (%get-string componentInfoHandle))))
      (unless (eql componentInfoHandle TC_nil) (#_DisposeHandle componentInfoHandle))
      (unless (eql componentNameHandle TC_nil) (#_DisposeHandle componentNameHandle)))
    (values_X
      ComponentDescriptionGCHandle
      componentNameResult
      componentInfoResult
      componentIconGCHandle)))

(defun_X T_GetComponentIconSuiteGC :private (aComponent)
  "Returns a GCHandle or nil.
See IM-More Mac Toolbox p. 6-49"
  (rlet ((iconSuite (:Handle)))
    (checking-toolbox-error (:OSErr)
                            (#_GetComponentIconSuite aComponent iconSuite))
    (if (eql TC_nil (%get-ptr iconSuite))
      nil
      (handleToGCHandle (%get-ptr iconSuite)))))

(defun_X T_GetComponentVersion :private (aComponentInstance)
  "See IM-More Mac Toolbox p. 6-49"
  (checking-toolbox-error (:Value (#_GetComponentInstanceError aComponentInstance))
                          (#_GetComponentVersion aComponentInstance)))

(defun_X T_ComponentFunctionImplemented :private (aComponentInstance ftnNumber)
  "Returns true or false, not a long integer.
See IM-More Mac Toolbox p. 6-50"
  (%boolean-to-lisp
   (checking-toolbox-error (:Value (#_GetComponentInstanceError aComponentInstance))
                           (#_ComponentFunctionImplemented
                            aComponentInstance
                            ftnNumber
                            ))))

#|
(defvar cp)
(defvar cd1)
(defvar cd2)
(defvar desc)
(defvar ccp)
(setf cd1 (T_newComponentDescriptionGCHandle))
(setf cd1 (T_newComponentDescriptionGCPtr))
(setf cd2 (T_newComponentDescriptionGCPtr))
(SetRecordField cd1 'ComponentDescription 'componentType TC_kAnyComponentType)
(setf desc (make-hash-table))
(setf (gethash :componentType desc) :|eat |)
(setf (gethash :componentSubtype desc) "AIFF")
(hash-table-to-alist desc)
(T_SetRecordPtrContents cd2 :ComponentDescription desc)
(hash-table-to-alist (T_GetRecordPtrContents cd2 :ComponentDescription))
;(SetRecordField cd1 'ComponentDescription 'componentType :|eat |)
(print (GetRecordField cd1 'ComponentDescription 'componentType))
(T_CountComponents cd1)
(T_GetComponentListModSeed)
(setf cp (T_FindNextComponent TC_nil cd2))
(defvar cd3)
(defvar iconH)
(setq iconH (T_NewHandleGC))
(sk8-multival-bind_X (cdGCHandle name info iconHandle)
                   (T_GetComponentInfoGC cp
                                          :ComponentDescriptionGCHandle cd1
                                          :ComponentName nil
                                          :ComponentInfo nil
                                          :ComponentIconGCHandle iconH
                                          )
  (print name)
  (print info)
  (if cdGCHandle
    (progn
      (print (hash-table-to-alist (T_GetRecordHandleContents cdGCHandle :ComponentDescription)))
      cdGCHandle)
    cdGCHandle)
  ;(T_DisposeComponentDescriptionHandle cdGCHandle)
  iconHandle
  )
(setf ccp (T_OpenComponentGC cp))
(setf cp (T_FindNextComponent cp cd2))
(T_GetComponentInfoGC cp)
|#

;;; maybe later...

;;; (defun_X T_RegisterComponent :private (cd componentEntryPoint glob componentInfo componentIcon))
;;; T_RegisterComponentResource
;;; T_RegisterComponentResourceFile
;;; T_UnregisterComponent

#|
	Change History (most recent last):
	2	2/25/94	dy	add copyright
	3	2/26/94	kleiman	TC_nil fixes
	4	2/28/94	dy	add initialize and preserve functions for bull-ptrs
	5	3/1/94	hernan	handleToGCHandle -> T_handleToGCHandle.
	6	3/28/94	dy	component connection terminable macptrs are flagged to be disposed on quit
	7	7/20/94	dy	privatize symbols
	8	7/20/94	dy	privatize
	9  	10/28/94	dy      	Make T_CloseComponent global
	10 	11/28/94	dy      	gc macro mods
	11 	12/19/94	dy      	Use new %boolean-to-lisp macro
	12 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	2  	 1/ 4/96	dy      	boldface was skewed - source text is unchanged
	2  	 4/10/96	Hernan  	removing case preserving reader macro. 
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
