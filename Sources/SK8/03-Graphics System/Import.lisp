;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :graphics-system)

;;; _______________________________ 
;;; Imports
;;; _______________________________ 

(import '(SK8::SK8-return-nothing 
          SK8::ignore-SK8-multivals
          SK8::SK8-multivals
          SK8::SK8-multivals-from-list
          SK8::sk8-multivals-sequence
          SK8::SK8-multival-setf
          SK8::SK8-multival-bind SK8::with-recoverable-list
          SK8::maybe-recover-list
          SK8::SK8-destructuring-setf
          SK8::SK8-destructuring-bind
          SK8::definitely-recover-list
          SK8::|WITH HIGHLIGHTCOLOR|
          SK8::computeMaybeTruncatedText
          sk8::roundBoxDimensions
          sk8::alignmentOfBoundsToBounds
          sk8::alignSizeToBounds
          sk8::constrainSizeToAspectRatio
          sk8::sk8-error
          sk8::reset-event-system-globals
          sk8::without-events
          )
          :gs)

(import '( sk8::define-sk8-function
           sk8::define-handler
           sk8::define-sk8-constant
           sk8::define-sk8-var
           )
        :gs)

(import '(mf::dovector-in-reverse
           )
        :gs)

(import '(sk8dev::translateStyle
          )
        :gs)

;;; (1) from "Trap Support"

(import '(sk8::copyrecordhandletorecordhandle
          sk8::copyrecordhandletorecordp
          sk8::copyrecordptrtorecordhandle
          sk8::copyrecordptrtorecordptr
          sk8::deactivategcosptr
          sk8::defconstant_x
          sk8::defconstant_xe
          sk8::defmacro_x
          sk8::defpascal_x
          sk8::defun_x
          sk8::defvar_x
          sk8::defvar_xe
          sk8::handletogchandle
          sk8::initialize-trap-null-ptr-values
          sk8::makegcosptr
          sk8::multiple-value-bind_x
          sk8::newRecordGCPtr
          sk8::ptrtogcptr
          sk8::preserve-trap-null-ptr-values
          sk8::T_DeactivateGCOSPtr
          sk8::T_HandleToGCHandle
          sk8::T_MakeGCOSPtr
          sk8::T_PtrToGCPtr
          sk8::values_x)
        :gs)

;;; (2) from "Trap Library"

(import '(sk8::removeres
          sk8::tc_dsmemfullerr
          sk8::T_AddResource
          sk8::t_closeresfile
          sk8::t_createresfile
          sk8::T_DetachResource
          sk8::t_disposehandle
          sk8::t_get1indresource
          sk8dev::t_get1namedresource
          sk8::T_Get1Resource
          sk8::T_Get1ResourceGC
          sk8::t_getcicongc
          sk8::t_getcursorgc
          sk8::T_GetGWorld
          sk8::t_gethandlesize
          sk8::T_GetPixpatGC
          sk8::t_getptrsize
          sk8::t_getresinfo
          sk8::t_getresource
          sk8::t_hlock
          sk8::t_hnopurge
          sk8::t_hunlock
          sk8::t_loadresource
          sk8::T_LockPixels
          sk8::T_NewGWorldGC
          sk8::t_newhandle
          sk8::t_newhandlegc
          sk8::T_NewPixpatGC
          sk8::t_newptr
          sk8::t_newptrgc
          sk8::T_NewRgn
          sk8::T_NewRgnGC
          sk8::t_openresfile
          sk8::T_RectRecordToRectArray
          sk8::t_rectrecordvalues
          sk8::t_releaseresource
          sk8::T_RemoveResource
          sk8::T_Unique1Id
          sk8::t_updateresfile
          sk8::t_useresfile
          sk8::t_with-hlock
          sk8::T_WriteResource
          sk8dev::with-locked-pixels-force
          sk8dev::with-locked-pixels-safe
          )
        :gs)

(import '(sk8::sk8-multivals
          )
        :gs)
         

#|
	Change History (most recent last):
	1  	 4/19/96	sidney  	new file, extracted from import/export to be compiled separately
	2  	 5/ 8/96	Hernan  	Adding sk8-error.
	3  	 7/ 7/96	sidney  	changes for native PPC build
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
