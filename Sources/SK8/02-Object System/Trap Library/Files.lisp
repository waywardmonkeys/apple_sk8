(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :private
                  sk8::T_StandardGetFileGC
                  sk8::T_getFileFSSpecGC
                SK8::T_ResolveAliasGC
)

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant_X TC_fsCurPerm  #$fsCurPerm  :private? t :project SK8)
  (defconstant_X TC_fsWrPerm   #$fsWrPerm   :private? t :project SK8)
  (defconstant_X TC_rAliasType #$rAliasType :private? t :project SK8)
  
  (defconstant_X TC_fnfErr #$fnfErr :private? t :project SK8))


(defun_X T_ResolveAliasGC :private (anAlias &optional fromFile)
  "Returns two values: theFSSpec wasChanged.
See IM-Files, p. 4-19"
  (let ((fsspecResult (newRecordGCHandle :FSSpec)))
    (rlet ((wasChanged :boolean))
      (with-dereferenced-handles ((fssp fsspecResult))
        (#_ResolveAlias (or fromFile (%null-ptr)) anAlias fssp wasChanged))
      (values_X fsspecResult (%get-boolean wasChanged)))))

(defun_X T_StandardGetFileGC :private (typeList &key replyPtr)
  "The typeList arg is a (possibly empty) list or array of file types.
Returns either an allocated StandardFileReply GCPtr or the supplied replyPtr arg if non-nil.
If you supply a reply Ptr arg, it doesn't matter if it's a Ptr or a GCPtr.
The fileFilter arg is not supported in this version.
See IM-Files p. 1-42"
  (unless replyPtr
    (setq replyPtr (newRecordGCPtr :StandardFileReply)))
  (let ((numTypes (length typeList)))
    (unless (<= numTypes 4)
      (error "Too many file types for StandardGetFile")) ;? correct error form needed
    (rlet ((typeListArray :SFTypeList))
      (cond
       ((listp typeList)
        (loop for type in typeList
              for ind from 0
              do (setf (rref typeListArray (SFTypeList.array ind)) type)))
       ((vectorp typeList)
        (loop for type across typeList
              for ind from 0
              do (setf (rref typeListArray (SFTypeList.array ind)) type)))
       (t
        (error "typeList must be an array or a list")))
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_StandardGetFile TC_nil numTypes typeListArray replyPtr))))
  (unless (rref replyPtr StandardFileReply.sfGood)
    (abortEvent))
  replyPtr)

(defun_X T_getFileFSSpecGC :private (typeList)
  "The typeList arg is a (possibly empty) list or array of file types.
Returns an FSSpecGCHandle.
The fileFilter arg is not supported in this version.
Not a trap."
  (rlet ((reply :StandardFileReply))
    (T_StandardGetFileGC typeList :replyPtr reply)
    (let ((FSSpecGCHandle (newRecordGCHandle :FSSpec)))
      (copyRecordPtrToRecordHandle (rref reply StandardFileReply.sfFile)
                                     FSSPecGCHandle
                                     :FSSpec))))

(defun_X T_FSClose :private (refNum)
  "Returns t.
See IM-Files p. 2-114."
  (checking-Toolbox-Error (:OSErr) (#_FSClose refNum))
  (values_X))

(defun_X T_FSMakeFSSpecGCForCreating :private (vRefNum dirID fileName)
  "Returns 2 values: FSSpec (a GCHandle) and error result code.
The result code return value is either t if the file was found, nil if not.
An error is raised if the trap returned any error other than fnfErr.
See IM-Files p. 2-166.
See also T_FSMakeFSSpecForOpening"
  (let ((FSSpecGCHandle (newRecordGCHandle :FSSpec)))
    (with-dereferenced-handles ((FSSpecPtr FSSpecGCHandle))
      (with-pstrs ((fileNamePstr (mac-namestring fileName)))
        (values_X FSSpecGCHandle 
                  (eql #$noErr
                       (checking-Toolbox-Error
                        (:OSErr nil :ok-errors '(#.TC_fnfErr)) ; TC_fnfErr is ok
                        (#_FSMakeFSSpec vRefNum dirID fileNamePstr FSSpecPtr))))))))

(defun_X T_FSMakeFSSpecGCForOpening :private (vRefNum dirID fileName)
  "Returns FSSpec (a GCHandle).
An error is raised if the trap returned any error, including fnfErr.
See IM-Files p. 2-166.
See also T_FSMakeFSSpecForCreating"
  (let ((FSSpecGCHandle (newRecordGCHandle :FSSpec)))
    (with-dereferenced-handles ((FSSpecPtr FSSpecGCHandle))
      (with-pstrs ((fileNamePstr (mac-namestring fileName)))
        (checking-Toolbox-Error (:OSErr) (#_FSMakeFSSpec vRefNum dirID fileNamePstr FSSpecPtr))))
    FSSpecGCHandle))

(defun T_NewAlias (
                     fromFile ; :FSSpec pointer or handle
                     target   ; :FSSpec pointer or handle
                     )
  "Returns an :AliasHandle.
See IM-Files p. 4-15"
  (rlet ((aliasHandlePtr :handle)) ; :AliasHandle
    (with-pointers ((fromFilePtr fromFile)
                    (targetPtr target))
      (checking-Toolbox-Error (:OSErr)
                              (#_NewAlias fromFilePtr targetPtr aliasHandlePtr)))
    (%get-ptr aliasHandlePtr)))

(defrecord (:refnum :handle)
  (value :integer))

(defun T_NewAliasGC (
                     fromFile ; :FSSpec pointer or handle
                     target   ; :FSSpec pointer or handle
                     )
  "Returns an :Alias GCHandle.
See IM-Files p. 4-15"
  (handleToGCHandle (T_NewAlias fromFile target)))

(defrecord (:refnum :handle)
  (value :integer))

(defun_X T_DisposeFileRefNumGCHandle :private (FileRefNumGCHandle)
  "Returns t.
Pulls a FileRefNum out of the handle, closes the file referred to by FileRefNum,
then disposes the handle."
  (T_FSClose (href FileRefNumGCHandle refnum.value))
  (T_DisposeHandle FileRefNumGCHandle))

(defun_X T_FileRefNumToFileRefNumGCHandle nil
         (FileRefNum &optional (terminationFunction 'T_DisposeFileRefNumGCHandle))
  "Returns a GCHandle holding FileRefNum.
The GCHandle's termination function is set to the second argument.
TerminationFunction must close the file referred to by FileRefNum before disposing the handle.
Not a trap."
  (let ((handle (T_NewHandle (record-length :refnum))))
    (setf (href handle refnum.value) FileRefNum)
    (T_makeGCOSPtr handle terminationFunction)))


#| under construction, untested
(defun_X T_PBResolveFileIDRef nil
         (fileID &key volumeRefNum volumeName #| (async nil) (ioCompletion TC_nil) |#)
  "Returns 3 values: resultCode, filename string, parent directory ID.
The resultCode value can be one of: TC_noErr TC_notAFileErr TC_afpObjectTypeErr.
The latter two mean that the file is a directory.
An error is raised for all other result codes.
See IM-Files, p. 2-229"
  (with-pstrs ((volumeNamePstr volumeName))
    (let (theError)
      (rlet ((paramBlock :HParamBlockRec
                         :ioNamePtr volumeNamePstr
                         :ioVRefNum volumeRefNum))
        (setf theError (checking-toolbox-error
                        (:OSErr nil :ok-errors '(#.TC_noErr #.TC_notAFileErr #.TC_afpObjectTypeErr))
                        (#_PBResolveFileIDRef paramBlock nil)))
        (values_X
          theError
          (%get-string (rref paramBlock HParamBlockRec.ioNamePtr))
          (rref paramBlock HParamBlockRec.ioSrcDirID)))))) |#
                              
      

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	3/1/94	hernan	handleToGCHandle -> T_handleToGCHandle.
	3	3/1/94	hernan	makeGCOSPtr -> T_makeGCOSPtr.
	4	6/12/94	dy	begin work on T_PBResolveFileIDRef
	5	6/13/94	till	Bug 1167508, fix callers of checking-toolbox-error's :ok-errors feature.
	6	6/13/94	dy	Bug 1167508, fix didn't build
	7	6/14/94	till	compile time adjustment
	8	7/20/94	dy	privatize symbols
	9  	 8/31/94	dy      	new T_StandardGetFile etc.
	10 	 9/ 8/94	nw      	add T_ResolveAliasGC
	11 	11/28/94	dy      	gc macro mods
	12 	12/19/94	dy      	Use new %get-boolean macro
	13 	 1/17/95	till    	T_OpenResFile, T_CreateResFile, 
							                                          T_UseResFile, T_CloseResFile
	14 	 1/18/95	till    	Resources.lisp is a better place for that stuff.
	15 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	16 	 3/ 9/95	dy      	remove T_ from  T_copyRecordxxx macro names
	17 	 3/ 9/95	dy      	force recomple
	2  	 9/29/95	dy      	Insert TC_fsWrPerm
	3  	 1/ 4/96	dy      	Typo in T_FSClose so it couldn't have worked
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
