(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




(in-package :SK8)

(SK8-declare-syms :SK8 :PRIVATE
                   T_NumberToFract
                   T_fractToNumber
                   T_NumberToFixed
                   T_FixedToNumber
                   T_NumberToShortFixed
                   T_ShortFixedToNumber
                   checkTimeValue
                   TC_useStandardMovieProgessFunction
                   TC_convertAllTracks
                   TC_autoSelectImportComponentInstance
                   TC_autoSelectExportComponentInstance
                   
                   TC_nil
                   TC_true
                   TC_false
                   TC_noErr
                   T_NewHandle
                   T_NewHandleGC
                   T_NewPtr
                   T_NewPtrGC
                   T_DisposeHandle
                   TC_fnfErr
                   T_FSClose
                   T_DisposeFileRefNumGCHandle
                   T_FileRefNumToFileRefNumGCHandle
                   T_FSMakeFSSpecGCForCreating
                   T_FSMakeFSSpecGCForOpening
                   TC_fsCurPerm
                   TC_rAliasType
                   T_NewAlias
                   T_NewAliasGC
                   TC_smSystemScript
                   TC_smCurrentScript
                   T_NewRgn
                   T_NewRgnGC
                   T_GetGWorld
                   TC_nilComponentInstance
                  
                  TC_gestaltComponentMgr
                  TC_kAnyComponentType
                  TC_kAnyComponentSubType
                  TC_kAnyComponentManufacturer
                  TC_kAnyComponentFlagsMask
                  TC_cmpWantsRegisterMessage
                  TC_componentDoAutoVersion
                  TC_componentWantsUnregister
                  TC_componentAutoVersionIncludeFlags
                  TC_kComponentOpenSelect
                  TC_kComponentCloseSelect
                  TC_kComponentCanDoSelect
                  TC_kComponentVersionSelect
                  TC_kComponentRegisterSelect
                  TC_kComponentTargetSelect
                  TC_kComponentUnregisterSelect
                  TC_defaultComponentIdentical
                  TC_defaultComponentAnyFlags
                  TC_defaultComponentAnyManufacturer
                  TC_defaultComponentAnySubType
                  TC_defaultComponentAnyFlagsAnyManufacturer
                  TC_defaultComponentAnyFlagsAnyManufacturerAnySubType
                  TC_invalidComponentID
                  TC_validInstancesExist
                  TC_componentNotCaptured
                  TC_componentDontRegister
                  TC_badComponentInstance
                  TC_badComponentSelector
                  
                T_newComponentDescriptionGCPtr
                T_newComponentDescriptionGCHandle
                   
                   T_FindNextComponent
                   T_CountComponents
                   T_GetComponentListModSeed
                 T_OpenDefaultComponentGC
                 T_OpenComponentGC
                 T_GetComponentInfoGC
                 T_GetComponentIconSuiteGC
                    T_GetComponentVersion
                    T_ComponentFunctionImplemented
                    ;; more component manager routines not implemented
                    TC_kFix1
                   TC_gestaltQuickTime
                   TC_MovieFileType
                   TC_MediaHandlerType
                   TC_DataHandlerType
                   TC_VideoMediaType
                   TC_SoundMediaType
                   TC_TextMediaType
                   TC_BaseMediaType
                   TC_DoTheRightThing
                   TC_kFullVolume
                   TC_kNoVolume
                   TC_dfDontDisplay
                   TC_dfDontAutoScale
                   TC_dfClipToTextBox
                   TC_dfUseMovieBGColor
                   TC_dfShrinkTextBoxToFit
                   TC_dfScrollIn
                   TC_dfScrollOut
                   TC_dfHorizScroll
                   TC_dfReverseScroll
                   TC_dfContinuousScroll
                   TC_dfFlowHoriz
                   TC_dfDropShadow
                   TC_dfAntiAlias
                   TC_dfKeyedText
                   TC_movieProgressOpen
                   TC_movieProgressUpdatePercent
                   TC_movieProgressClose
                   TC_progressOpFlatten
                   TC_progressOpInsertTrackSegment
                   TC_progressOpInsertMovieSegment
                   TC_progressOpPaste
                   TC_progressOpAddMovieSelection
                   TC_progressOpCopy
                   TC_progressOpCut
                   TC_progressOpLoadMovieIntoRam
                   TC_progressOpLoadTrackIntoRam
                   TC_progressOpLoadMediaIntoRam
                   TC_progressOpImportMovie
                   TC_progressOpExportMovie
                   TC_mediaQualityDraft
                   TC_mediaQualityNormal
                   TC_mediaQualityBetter
                   TC_mediaQualityBest
                   TC_loopTimeBase
                   TC_palindromeLoopTimeBase
                   TC_triggerTimeFwd
                   TC_triggerTimeBwd
                   TC_triggerTimeEither
                   TC_triggerRateLT
                   TC_triggerRateGT
                   TC_triggerRateEqual
                   TC_triggerRateLTE
                   TC_triggerRateGTE
                   TC_triggerRateNotEqual
                   TC_triggerRateChange
                   TC_triggerAtStart
                   TC_triggerAtStop
                   TC_timeBaseBeforeStartTime
                   TC_timeBaseAfterStopTime
                   TC_callBackAtTime
                   TC_callBackAtRate
                   TC_callBackAtTimeJump
                   TC_callBackAtExtremes
                   TC_callBackAtInterrupt
                   TC_qtcbNeedsRateChanges
                   TC_qtcbNeedsTimeChanges
                   TC_qtcbNeedsStartStopChanges
                   TC_keepInRam
                   TC_unkeepInRam
                   TC_flushFromRam
                   TC_loadForwardTrackEdits
                   TC_loadBackwardTrackEdits
                   TC_newMovieActive
                   TC_newMovieDontResolveDataRefs
                   TC_newMovieDontAskUnresolvedDataRefs
                   TC_newMovieDontAutoAlternates
                   TC_trackUsageInMovie
                   TC_trackUsageInPreview
                   TC_trackUsageInPoster
                   TC_mediaSampleNotSync
                   TC_mediaSampleShadowSync
                   TC_pasteInParallel
                   TC_showUserSettingsDialog
                   TC_nextTimeMediaSample
                   TC_nextTimeMediaEdit
                   TC_nextTimeTrackEdit
                   TC_nextTimeSyncSample
                   TC_nextTimeEdgeOK
                   TC_nextTimeIgnoreActiveSegment
                   TC_createMovieFileDeleteCurFile
                   TC_createMovieFileDontCreateMovie
                   TC_createMovieFileDontOpenFile
                   TC_flattenAddMovieToDataFork
                   TC_flattenActiveTracksOnly
                   TC_flattenDontInterleaveFlatten
                   TC_movieInDataForkResID
                   TC_movieScrapDontZeroScrap
                   TC_movieScrapOnlyPutMovie
                   TC_hintsScrubMode
                   TC_hintsUseScreenBuffer
                   TC_hintsAllowInterlace
                   TC_hintsUseSoundInterp
                   TC_hintsHighQuality
                   TC_mediaHandlerFlagBaseClient
                   TC_txtProcDefaultDisplay
                   TC_txtProcDontDisplay
                   TC_txtProcDoDisplay
                   TC_findTextEdgeOK
                   TC_findTextCaseSensitive
                   TC_findTextReverseSearch
                   TC_findTextWrapAround
                   TC_findTextUseOffset
                   TC_dropShadowOffsetType
                   TC_dropShadowTranslucencyType
                   TC_dataRefSelfReference
                   TC_dataRefWasNotResolved
                   TC_couldNotResolveDataRef
                   TC_badImageDescription
                   TC_badPublicMovieAtom
                   TC_cantFindHandler
                   TC_cantOpenHandler
                   TC_badComponentType
                   TC_noMediaHandler
                   TC_noDataHandler
                   TC_invalidMedia
                   TC_invalidTrack
                   TC_invalidMovie
                   TC_invalidSampleTable
                   TC_invalidDataRef
                   TC_invalidHandler
                   TC_invalidDuration
                   TC_invalidTime
                   TC_cantPutPublicMovieAtom
                   TC_badEditList
                   TC_mediaTypesDontMatch
                   TC_progressProcAborted
                   TC_movieToolboxUninitialized
                   TC_wfFileNotFound
                   TC_cantCreateSingleForkFile
                   TC_invalidEditState
                   TC_nonMatchingEditState
                   TC_staleEditState
                   TC_userDataItemNotFound
                   TC_maxSizeToGrowTooSmall
                   TC_badTrackIndex
                   TC_trackIDNotFound
                   TC_trackNotInMovie
                   TC_timeNotInTrack
                   TC_timeNotInMedia
                   TC_badEditIndex
                   TC_internalQuickTimeError
                   TC_cantEnableTrack
                   TC_invalidRect
                   TC_invalidSampleNum
                   TC_invalidChunkNum
                   TC_invalidSampleDescIndex
                   TC_invalidChunkCache
                   TC_invalidSampleDescription
                   TC_dataNotOpenForRead
                   TC_dataNotOpenForWrite
                   TC_dataAlreadyOpenForWrite
                   TC_dataAlreadyClosed
                   TC_endOfDataReached
                   TC_dataNoDataRef
                   TC_noMovieFound
                   TC_invalidDataRefContainer
                   TC_badDataRefIndex
                   TC_noDefaultDataRef
                   TC_couldNotUseAnExistingSample
                   TC_featureUnsupported
                   TC_unsupportedAuxiliaryImportData
                   TC_auxiliaryExportDataUnavailable
                   TC_noRecordOfApp
                   TC_MovieControllerComponentType
                   TC_mcTopLeftMovie
                   TC_mcScaleMovieToFit
                   TC_mcWithBadge
                   TC_mcNotVisible
                   TC_mcWithFrame
                   TC_mcActionIdle
                   TC_mcActionDraw
                   TC_mcActionActivate
                   TC_mcActionDeactivate
                   TC_mcActionMouseDown
                   TC_mcActionKey
                   TC_mcActionPlay
                   TC_mcActionGoToTime
                   TC_mcActionSetVolume
                   TC_mcActionGetVolume
                   TC_mcActionStep
                   TC_mcActionSetLooping
                   TC_mcActionGetLooping
                   TC_mcActionSetLoopIsPalindrome
                   TC_mcActionGetLoopIsPalindrome
                   TC_mcActionSetGrowBoxBounds
                   TC_mcActionControllerSizeChanged
                   TC_mcActionSetSelectionBegin
                   TC_mcActionSetSelectionDuration
                   TC_mcActionSetKeysEnabled
                   TC_mcActionGetKeysEnabled
                   TC_mcActionSetPlaySelection
                   TC_mcActionGetPlaySelection
                   TC_mcActionSetUseBadge
                   TC_mcActionGetUseBadge
                   TC_mcActionSetFlags
                   TC_mcActionGetFlags
                   TC_mcActionSetPlayEveryFrame
                   TC_mcActionGetPlayEveryFrame
                   TC_mcActionGetPlayRate
                   TC_mcActionShowBalloon
                   TC_mcActionBadgeClick
                   TC_mcActionMovieClick
                   TC_mcActionSuspend
                   TC_mcActionResume
                   TC_mcFlagSuppressMovieFrame
                   TC_mcFlagSuppressStepButtons
                   TC_mcFlagSuppressSpeakerButton
                   TC_mcFlagsUseWindowPalette
                   TC_mcPositionDontInvalidate
                   TC_mcInfoUndoAvailable
                   TC_mcInfoCutAvailable
                   TC_mcInfoCopyAvailable
                   TC_mcInfoPasteAvailable
                   TC_mcInfoClearAvailable
                   TC_mcInfoHasSound
                   TC_mcInfoIsPlaying
                   TC_mcInfoIsLooping
                   TC_mcInfoIsInPalindrome
                   TC_mcInfoEditingEnabled
                   TC_mcMenuUndo
                   TC_mcMenuCut
                   TC_mcMenuCopy
                   TC_mcMenuPaste
                   TC_mcMenuClear
                   TC_cannotMoveAttachedController
                   TC_controllerHasFixedHeight
                   TC_cannotSetWidthOfAttachedController
                   TC_controllerBoundsNotExact
                   TC_editingNotAllowed
                   TC_badControllerHeight
                   TC_kMCSetMovieSelect
                   TC_kMCRemoveMovieSelect
                   TC_kMCIsPlayerEventSelect
                   TC_kMCSetActionFilterSelect
                   TC_kMCDoActionSelect
                   TC_kMCSetControllerAttachedSelect
                   TC_kMCIsControllerAttachedSelect
                   TC_kMCSetControllerPortSelect
                   TC_kMCGetControllerPortSelect
                   TC_kMCGetVisibleSelect
                   TC_kMCSetVisibleSelect
                   TC_kMCGetControllerBoundsRectSelect
                   TC_kMCSetControllerBoundsRectSelect
                   TC_kMCGetControllerBoundsRgnSelect
                   TC_kMCGetWindowRgnSelect
                   TC_kMCMovieChangedSelect
                   TC_kMCSetDurationSelect
                   TC_kMCGetCurrentTimeSelect
                   TC_kMCNewAttachedControllerSelect
                   TC_kMCDrawSelect
                   TC_kMCActivateSelect
                   TC_kMCIdleSelect
                   TC_kMCKeySelect
                   TC_kMCClickSelect
                   TC_kMCEnableEditingSelect
                   TC_kMCIsEditingEnabledSelect
                   TC_kMCCopySelect
                   TC_kMCCutSelect
                   TC_kMCPasteSelect
                   TC_kMCClearSelect
                   TC_kMCUndoSelect
                   TC_kMCPositionControllerSelect
                   TC_kMCGetControllerInfoSelect
                   TC_kMCSetClipSelect
                   TC_kMCGetClipSelect
                   TC_kMCDrawBadgeSelect
                   TC_kMCSetUpEditMenuSelect
                   TC_kMCGetMenuStringSelect
                   TC_kMCSetActionFilterWithRefConSelect
                   
                 T_disposeMovieFileRefNumGCHandle
                    
                 T_NewMovieFromFileGC
                 T_NewMovieFromHandleGC
                 T_NewMovieGC
                    T_ConvertFileToMovieFile
                    T_ConvertMovieToFile
                    T_DisposeMovie
                  T_CreateMovieFileGC
                  T_OpenMovieFile
                     T_CloseMovieFile
                  T_DeleteMovieFile
                     ;; more
                  T_NewMovieFromDataForkGC
                     ;; more
                  T_StartMovie
                  T_StopMovie
                  T_GoToBeginningOfMovie
                  T_GoToEndOfMovie
                     ;; more
                     T_MoviesTask
                  T_IsMovieDone
                  T_UpdateMovie
                     ;; more
                  T_SetMoviePreferredRate
                  T_GetMoviePreferredRate
                  T_SetMoviePreferredVolume
                  T_GetMoviePreferredVolume
                  T_getMovieTimeAndRate
                  T_PrerollMovie
                  T_SetMovieActiveSegment
                  T_GetMovieActiveSegment
                     ;; more
                     T_LoadMovieIntoRam
                     ;; more
                  T_SetMovieActive
                  T_GetMovieActive
                     ;; more
                  T_GetMoviePict
                  T_GetMoviePictGC
                  T_GetMoviePosterPictGC
                     ;; more
                     T_NewTrackMedia
                     ;; more
                  T_SetMovieGWorld
                  T_GetMovieGWorld
                     ;; more
                  T_SetMovieBox
                  T_GetMovieBoxGC
                     ;; more
                  T_SetMovieDisplayClipRgn
                  T_GetMovieDisplayClipRgnGC
                     ;; more
                  T_GetMovieBoundsRgnGC
                  T_GetTrackMovieBoundsRgnGC
                  T_SetMovieClipRgn
                  T_GetMovieClipRgnGC
                     ;; more
                  T_SetMovieVolume
                  T_GetMovieVolume
                     ;; more
                  T_GetMovieDuration
                  T_SetMovieTimeValue
                  T_SetMovieTime
                  T_GetMovieTimeValue
                  T_GetMovieTimeRecordGC
                  T_SetMovieRate
                  T_GetMovieRate
                  T_SetMovieTimeScale
                  T_GetMovieTimeScale
                  T_GetMovieTimeBase
                     ;; more
                  T_GetMovieNextInterestingTime
                     ;; more
                     T_GetMovieIndTrack
                     ;; more
                     T_GetTrackMedia
                     ;; more
                  T_PutMovieOnScrap
                  T_NewMovieFromScrapGC
                  T_SetMovieSelection
                  T_GetMovieSelection
                  T_CutMovieSelectionGC
                  T_CopyMovieSelectionGC
                  T_PasteMovieSelection
                     ;; more
                  T_GetMovieDataSize
                  T_GetTrackDataSize
                  T_GetMediaDataSize
                     ;; more
                     
                     T_GetMovieUserData
                     T_GetTrackUserData
                     T_GetMediaUserData
                     T_GetNextUserDataType
                     T_CountUserDataType
                     T_AddUserData
                     T_GetUserData
                     T_RemoveUserData
                     T_AddUserDataText
                     T_GetUserDataText
                     T_RemoveUserDataText
                     T_SetUserDataItem
                     T_GetUserDataItem
                     T_NewUserData ;not implemented yet
                     T_DisposeUserData
                     T_PutUserDataIntoHandle ;not implemented yet
                     T_NewUserDataFromHandle
                     
                     ;; more
                     T_DisposeTrackEditState
                     ;; more
                     
                     T_NewTimeBaseGC
                     ;; more
                  T_SetTimeBaseFlags
                  T_GetTimeBaseFlags
                     ;; more
                     T_DisposeTimeBase
                     ;; more
                  T_drawMovieInGWorld
                     
                     T_MCIsPlayerEvent
                     T_MCIdle
                     T_optionalHandleArgs
                     )

(in-package :SK8DEV)

;;; Some constants

;;; These will rarely be needed since the trap interfaces almost always
;;; use t or nil for boolean values
(defconstant_X TC_true 1 :private? t :project SK8)
(defconstant_X TC_false 0 :private? t :project SK8)

(defconstant_X TC_noErr #$noErr :private? t :project SK8)

(defvar_X TC_nil (ccl:%null-ptr) :private? t :project SK8)

;;; These are required because Quicktime is not part of the runtime.

(defun preserve-components-trap-null-ptr-values ()
  )
(defun preserve-movies-trap-null-ptr-values ()
  )
(defun initialize-components-trap-null-ptr-values ()
  )
(defun initialize-movies-trap-null-ptr-values ()
  )

(defun initialize-trap-null-ptr-values ()
  "Called at startup time."
  ;; traps-movies.lisp
  (setf TC_nil (ccl:%null-ptr))
  (initialize-components-trap-null-ptr-values)
  (initialize-movies-trap-null-ptr-values)
  )

(defun preserve-trap-null-ptr-values ()
  "Called at startup time."
  ;; traps-movies.lisp
  (setf TC_nil nil)
  (preserve-components-trap-null-ptr-values)
  (preserve-movies-trap-null-ptr-values)
  )

;;; Some miscellaneous traps and other low-level utilities

;;; on overflow, limit to maximum
(defun_X T_numberToFract :private (num)
  (let ((answer (round num (expt 2 -30))))
    (cond
     ((> answer (- (expt 2 31) 1))
                (- (expt 2 31) 1))
     ((< answer (- (expt 2 31)))
                (- (expt 2 31)))
     (t answer))))

(defun_X T_fractToNumber :private (fract)
  (/ fract (expt 2 30)))

;;; on overflow, limit to maximum
(defun_X T_numberToFixed :private (num)
  (let ((answer (round num (expt 2 -16))))
    (cond
     ((> answer (- (expt 2 31) 1))
                (- (expt 2 31) 1))
     ((< answer (- (expt 2 31)))
                (- (expt 2 31)))
     (t answer))))

(defun_X T_fixedToNumber :private (fixed)
  (/ fixed (expt 2 16)))

;;; on overflow, limit to maximum
(defun_X T_numberToShortFixed :private (num)
  (let ((answer (round num (expt 2 -8))))
    (cond
     ((> answer (- (expt 2 15) 1))
                (- (expt 2 15) 1))
     ((< answer (- (expt 2 15)))
                (- (expt 2 15)))
     (t answer))))

(defun_X T_shortFixedToNumber :private (fixed)
  (/ fixed (expt 2 8)))

(defun_X T_optionalHandleArg :private (arg
                                             &key
                                             (creator #'T_NewHandleGC)
                                             (creatorArgs '()))
  "Process an optional GCHandle keyword argument.
Such an argment can be supplied with one of these three values:
* a Handle or GCHandle - NOT a Ptr
   This handle will be filled in and returned as the corresponding return value
* t
   A new GCHandle will be allocated for you and filled in.
   This is the default.
* nil
   The corresponding return value will be nil"
  (if (eql t arg)
    (apply creator creatorArgs)
    arg))

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	2/26/94	kleiman	TC_nil fixes
	3	2/28/94	dy	preserve-trap-null-ptr-values added
	4	2/28/94	dy	add component null-ptr initialize and preserve
	5	3/3/94	sidney	SK8-Declare-syms in non-header file should not specify a category
	6	3/3/94	sidney	...but they should be in the correct format!
	7	3/29/94	dy	NumberToShortFixed problem
	8	5/31/94	yost	support Fract type
	9	5/31/94	yost	fract type fix
	10	7/20/94	dy	privatize symbols
	11	7/20/94	dy	privatize T_ syms
	12	7/20/94	dy	privatize some more
	13 	 9/ 1/94	dy      	numberToFixed -> T_numberToFixed etc.
	14 	 9/ 2/94	dy      	Fix bug 1182766
	15 	12/14/94	dy      	new T_optionalHandleArg function
	2  	 9/ 1/95	Till    	Fix incorrect use of round on T_numberToFixed, 
						T_numberToFrac, T_numberToShortFixed.
	3  	 9/ 3/96	Hernan  	Defined stubs for quicktime related ptr preserve/restore
						functions so that we can run the code without quicktime
						loaded.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
