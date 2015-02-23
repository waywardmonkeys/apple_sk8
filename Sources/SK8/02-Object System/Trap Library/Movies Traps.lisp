(in-package :SK8DEV)

(in-package :SK8)
(SK8-declare-syms :SK8 :private

                  ("macframes"
                   T_MCGetClipGC
                   T_MCSetControllerPort
                   T_MCGetControllerPort
                   CHECKTIMEVALUE T_ADDFILEPREVIEW T_ADDMOVIERESOURCE T_ADDMOVIESELECTION T_CLOSEMOVIEFILE
                   T_CONVERTFILETOMOVIEFILE T_CONVERTMOVIETOFILE T_COPYMOVIESELECTIONGC T_COUNTUSERDATATYPE
                   T_CREATEMOVIEFILEGC T_CUTMOVIESELECTIONGC T_ClearMovieChanged T_ClearMovieSelection T_CopyMovieSettings
                   T_CopyTrackSettings T_DELETEMOVIEFILE T_DISPOSEMOVIE T_DISPOSEMOVIEFILEREFNUMGCHANDLE
                   T_DISPOSEMOVIETRACK T_DISPOSETRACKEDITSTATE T_DRAWMOVIEINGWORLD T_DeleteMovieSegment T_DeleteTrackSegment
                   T_DisposeMovieEditState T_FlattenMovie T_FlattenMovieDataGC T_GETMEDIACREATIONTIME T_GETMEDIADATASIZE
                   T_GETMEDIAHANDLERDESCRIPTION T_GETMEDIAUSERDATA T_GETMOVIEACTIVE T_GETMOVIEACTIVESEGMENT
                   T_GETMOVIEBOUNDSRGNGC T_GETMOVIEBOXGC T_GETMOVIECLIPRGNGC T_GETMOVIEDATASIZE T_GETMOVIEDISPLAYCLIPRGNGC
                   T_GETMOVIEDURATION T_GETMOVIEFSSPECGC T_GETMOVIEGWORLD T_GETMOVIEINDTRACK T_GETMOVIEMATRIXARRAY
                   T_GETMOVIEMATRIXGC T_GETMOVIENEXTINTERESTINGTIME T_GETMOVIEPICT T_GETMOVIEPICTGC T_GETMOVIEPOSTERPICTGC
                   T_GETMOVIEPOSTERTIME T_GETMOVIEPREFERREDRATE T_GETMOVIEPREFERREDVOLUME T_GETMOVIEPREVIEWMODE
                   T_GETMOVIEPREVIEWTIME T_GETMOVIERATE T_GETMOVIESELECTION T_GETMOVIETIMEBASE T_GETMOVIETIMERECORDGC
                   T_GETMOVIETIMESCALE T_GETMOVIETIMEVALUE T_GETMOVIETRACK T_GETMOVIETRACKCOUNT T_GETMOVIEUSERDATA
                   T_GETMOVIEVOLUME T_GETNEXTUSERDATATYPE T_GETPOSTERBOXGC T_GETTIMEBASEFLAGS T_GETTRACKDATASIZE
                   T_GETTRACKID T_GETTRACKMEDIA T_GETTRACKMOVIEBOUNDSRGNGC T_GETTRACKUSERDATA T_GOTOBEGINNINGOFMOVIE
                   T_GOTOENDOFMOVIE T_GetMediaModificationTime T_GetMovieCreationTime T_GetMovieModificationTime
                   T_GetMovieStatus T_GetTrackAlternate T_GetTrackCreationTime T_GetTrackDuration T_GetTrackEditRate
                   T_GetTrackLayer T_GetTrackMatrixArray T_GetTrackModificationTime T_GetTrackNextInterestingTime
                   T_GetTrackOffset T_GetTrackStatus T_GetTrackUsage T_GetTrackVolume T_HasMovieChanged T_ISMOVIEDONE
                   T_ISVISUALMEDIATYPE T_InsertEmptyMovieSegment T_InsertEmptyTrackSegment T_InsertMediaIntoTrack
                   T_InsertMovieSegment T_InsertTrackSegment T_IsScrapMovie T_LOADMOVIEINTORAM T_LoadMediaIntoRam
                   T_LoadTrackIntoRam T_MATRIXARRAYTOMATRIXRECORD T_MATRIXRECORDTOMATRIXARRAY T_MCIDLE T_MCISPLAYEREVENT
                   T_MOVIESTASK T_MakeFilePreview T_NEWMOVIEFROMDATAFORKGC T_NEWMOVIEFROMFILEGC T_NEWMOVIEFROMHANDLEGC
                   T_NEWMOVIEFROMSCRAPGC T_NEWMOVIEGC T_NEWMOVIETRACK T_NEWTIMEBASEGC T_NEWTRACKMEDIA T_NewMovieEditState
                   T_NewTrackEditState T_OPENMOVIEFILE T_PASTEMOVIESELECTION T_PREROLLMOVIE T_PUTMOVIEONSCRAP
                   T_PasteHandleIntoMovie T_PtInMovie T_PtInTrack T_PutMovieIntoDataFork T_PutMovieIntoHandleGC
                   T_PutMovieIntoTypedHandle T_RemoveMovieResource T_SETMOVIEACTIVE T_SETMOVIEACTIVESEGMENT
                   T_SETMOVIEBOX T_SETMOVIECLIPRGN T_SETMOVIEDISPLAYCLIPRGN T_SETMOVIEGWORLD T_SETMOVIEMATRIX
                   T_SETMOVIEMATRIXFROMARRAY T_SETMOVIEPOSTERTIME T_SETMOVIEPREFERREDRATE T_SETMOVIEPREFERREDVOLUME
                   T_SETMOVIEPREVIEWMODE T_SETMOVIEPREVIEWTIME T_SETMOVIERATE T_SETMOVIESELECTION T_SETMOVIETIME
                   T_SETMOVIETIMESCALE T_SETMOVIETIMEVALUE T_SETMOVIEVOLUME T_SETPOSTERBOX T_SETTIMEBASEFLAGS
                   T_SHOWMOVIEPOSTER T_SHOWMOVIEPOSTERINGWORLD T_STANDARDGETFILEPREVIEWGC T_STARTMOVIE T_STOPMOVIE
                   T_ScaleMovieSegment T_ScaleTrackSegment T_SelectMovieAlternates T_SetAutoTrackAlternatesEnabled
                   T_SetMediaPlayHints T_SetMovieLanguage T_SetMoviePLayHints T_SetTrackAlternate T_SetTrackLayer
                   T_SetTrackMatrixFromArray T_SetTrackOffset T_SetTrackUsage T_SetTrackVolume T_TrackTimeToMediaTime
                   T_UPDATEMOVIE T_UpdateMovieResource T_UseMovieEditState T_UseTrackEditState T_getMediaType)
                T_SetMovieCoverProcs
                CB_QTcoverProc
                CB_QTuncoverProc
                CB_QTErrProc
                CB_QTCalloutProc
                makeFileFilterProc
                CB_QTCallBack
                CB_QTTextProc
                   T_SetMovieDrawingCompleteProc
                   CB_QTMovieDrawingCompleteProc
                coverEvent
                uncoverEvent
                   progressEvent
                   qtErrorEvent
                   previewProgressEvent
                   callBackEvent
                   textDisplayedEvent
                   drawingCompleteEvent
                   T_MCSetControllerBoundsRect
                   T_MCGetControllerBoundsRectGC
                   T_MCGetControllerBoundsRect
                   CB_QTActionFilter
                   T_GetMovieBox
                  )

(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(defvar_X TC_convertAllTracks                     (ccl:%null-ptr) :private? t :private? t :project SK8)
(defvar_X TC_autoSelectImportComponentInstance (ccl:%null-ptr) :private? t :private? t :project SK8)
(defvar_X TC_autoSelectExportComponentInstance (ccl:%null-ptr) :private? t :private? t :project SK8)
;; This next one has to be a separate macptr object, and needs its ptr value set every time at startup to -1
(defvar_X TC_useStandardMovieProgessFunction   (ccl:%int-to-ptr -1) :private? t :private? t :project SK8)

(export '(
          $convertAllTracks
          $autoSelectImportComponentInstance
          $autoSelectExportComponentInstance
          $useStandardMovieProgessFunction
          ))
(defvar $convertAllTracks 0)
(defvar $autoSelectImportComponentInstance 0)
(defvar $autoSelectExportComponentInstance 0)
(defvar $useStandardMovieProgessFunction   -1)

(defun initialize-movies-trap-null-ptr-values ()
  "Called at startup time."
  (setf TC_convertAllTracks                     (ccl:%null-ptr))
  (setf TC_autoSelectImportComponentInstance (ccl:%null-ptr))
  (setf TC_autoSelectExportComponentInstance (ccl:%null-ptr))
  ;; This next one has to be a separate macptr object, and needs its ptr value set every time at startup to -1
  (setf TC_useStandardMovieProgessFunction   (ccl:%int-to-ptr -1))
  )

(defun preserve-movies-trap-null-ptr-values ()
  "Called at startup time."
  (setf TC_convertAllTracks                     nil)
  (setf TC_autoSelectImportComponentInstance nil)
  (setf TC_autoSelectExportComponentInstance nil)
  ;; This next one has to be a separate macptr object, and needs its ptr value set every time at startup to -1
  (setf TC_useStandardMovieProgessFunction   nil)
  )

(eval-when (:compile-toplevel :load-toplevel)
  ;; not yet in interface files, so defined here
  (defconstant traps::$mcActionGetSelectionBegin 53)       ;    param is TimeRecord
  (defconstant traps::$mcActionGetSelectionDuration 54)    ;    param is TimeRecord
  (defconstant traps::$mcActionPrerollAndPlay 55)          ;    param is Fixed, play rate
  
  )

(eval-when (:compile-toplevel :load-toplevel)
   ;; no longer in interface table MCL3.1 and 4.0. May be a bug, but comment this out for now so we can build until it is fixed
  ;; (defconstant_X TC_kFix1 #$kFix1 :private? t :project SK8) ; #X10000
  (defconstant_X TC_gestaltQuickTime #$gestaltQuickTime :private? t :project SK8) ; :|qtim|
  (defconstant_X TC_MovieFileType #$MovieFileType :private? t :project SK8) ; :|MooV|
  (defconstant_X TC_MediaHandlerType #$MediaHandlerType :private? t :project SK8) ; :|mhlr|
  (defconstant_X TC_DataHandlerType #$DataHandlerType :private? t :project SK8) ; :|dhlr|
  (defconstant_X TC_VideoMediaType #$VideoMediaType :private? t :project SK8) ; :|vide|
  (defconstant_X TC_SoundMediaType #$SoundMediaType :private? t :project SK8) ; :|soun|
  (defconstant_X TC_TextMediaType #$TextMediaType :private? t :project SK8) ; :|text|
  (defconstant_X TC_BaseMediaType #$BaseMediaType :private? t :project SK8) ; :|gnrc|
  (defconstant_X TC_DoTheRightThing #$DoTheRightThing :private? t :project SK8) ; 0

  (defconstant_X TC_dfDontDisplay        #$dfDontDisplay        :private? t :project SK8) ; 1  Don't display the text
  (defconstant_X TC_dfDontAutoScale      #$dfDontAutoScale      :private? t :project SK8) ; 2 Don't scale text as track bounds grows
  (defconstant_X TC_dfClipToTextBox      #$dfClipToTextBox      :private? t :project SK8) ; 4 Clip update to the textbox
  (defconstant_X TC_dfUseMovieBGColor    #$dfUseMovieBGColor    :private? t :project SK8) ; 8 Set text background to movie's background color
  (defconstant_X TC_dfShrinkTextBoxToFit #$dfShrinkTextBoxToFit :private? t :project SK8) ; 16 Compute minimum box to fit the sample
  (defconstant_X TC_dfScrollIn           #$dfScrollIn           :private? t :project SK8) ; 32  Scroll text in until last of
  (defconstant_X TC_dfScrollOut          #$dfScrollOut          :private? t :project SK8) ; 64  Scroll text out until last of
  (defconstant_X TC_dfHorizScroll        #$dfHorizScroll        :private? t :project SK8) ; 128 Scroll text horizontally (otherwise it's vertical)
  (defconstant_X TC_dfReverseScroll      #$dfReverseScroll      :private? t :project SK8) ; 256 vert: scroll down rather than up; horiz:
  (defconstant_X TC_dfContinuousScroll   #$dfContinuousScroll   :private? t :project SK8) ; 512 new samples cause previous samples to scroll
  (defconstant_X TC_dfFlowHoriz          #$dfFlowHoriz          :private? t :project SK8) ; 1024  horiz scroll text flows in textbox
  (defconstant_X TC_dfDropShadow         #$dfDropShadow         :private? t :project SK8) ; 4096 display text with a drop shadow
  (defconstant_X TC_dfAntiAlias          #$dfAntiAlias          :private? t :project SK8) ; 8192  attempt display text anti aliased
  (defconstant_X TC_dfKeyedText          #$dfKeyedText          :private? t :project SK8) ; 16384 key the text over background
  (defconstant_X TC_dfInverseHilite      #$dfInverseHilite      :private? t :project SK8) ; 32768

  (defconstant_X TC_searchTextDontGoToFoundTime   #$searchTextDontGoToFoundTime   :private? t :project SK8) ; #x10000
  (defconstant_X TC_searchTextDontHiliteFoundText #$searchTextDontHiliteFoundText :private? t :project SK8) ; #x20000
  (defconstant_X TC_searchTextOneTrackOnly        #$searchTextOneTrackOnly        :private? t :project SK8) ; #x40000
  (defconstant_X TC_searchTextEnabledTracksOnly   #$searchTextEnabledTracksOnly   :private? t :project SK8) ; #x80000

  (defconstant_X TC_movieProgressOpen #$movieProgressOpen :private? t :project SK8) ; 0
  (defconstant_X TC_movieProgressUpdatePercent #$movieProgressUpdatePercent :private? t :project SK8) ; 1
  (defconstant_X TC_movieProgressClose #$movieProgressClose :private? t :project SK8) ; 2
  (defconstant_X TC_progressOpFlatten #$progressOpFlatten :private? t :project SK8) ; 1
  (defconstant_X TC_progressOpInsertTrackSegment #$progressOpInsertTrackSegment :private? t :project SK8) ; 2
  (defconstant_X TC_progressOpInsertMovieSegment #$progressOpInsertMovieSegment :private? t :project SK8) ; 3
  (defconstant_X TC_progressOpPaste #$progressOpPaste :private? t :project SK8) ; 4
  (defconstant_X TC_progressOpAddMovieSelection #$progressOpAddMovieSelection :private? t :project SK8) ; 5
  (defconstant_X TC_progressOpCopy #$progressOpCopy :private? t :project SK8) ; 6
  (defconstant_X TC_progressOpCut #$progressOpCut :private? t :project SK8) ; 7
  (defconstant_X TC_progressOpLoadMovieIntoRam #$progressOpLoadMovieIntoRam :private? t :project SK8) ; 8
  (defconstant_X TC_progressOpLoadTrackIntoRam #$progressOpLoadTrackIntoRam :private? t :project SK8) ; 9
  (defconstant_X TC_progressOpLoadMediaIntoRam #$progressOpLoadMediaIntoRam :private? t :project SK8) ; 10
  (defconstant_X TC_progressOpImportMovie #$progressOpImportMovie :private? t :project SK8) ; 11
  (defconstant_X TC_progressOpExportMovie #$progressOpExportMovie :private? t :project SK8) ; 12
  (defconstant_X TC_mediaQualityDraft #$mediaQualityDraft :private? t :project SK8) ; #X0
  (defconstant_X TC_mediaQualityNormal #$mediaQualityNormal :private? t :project SK8) ; #X40
  (defconstant_X TC_mediaQualityBetter #$mediaQualityBetter :private? t :project SK8) ; #X80
  (defconstant_X TC_mediaQualityBest #$mediaQualityBest :private? t :project SK8) ; #XC0
  (defconstant_X TC_loopTimeBase #$loopTimeBase :private? t :project SK8) ; 1
  (defconstant_X TC_palindromeLoopTimeBase #$palindromeLoopTimeBase :private? t :project SK8) ; 2
  (defconstant_X TC_triggerTimeFwd #$triggerTimeFwd :private? t :project SK8) ; #X1 when curTime exceeds triggerTime going forward
  (defconstant_X TC_triggerTimeBwd #$triggerTimeBwd :private? t :project SK8) ; #X2 when curTime exceeds triggerTime going backwards
  (defconstant_X TC_triggerTimeEither #$triggerTimeEither :private? t :project SK8) ; #X3 when curTime exceeds triggerTime going either direction
  (defconstant_X TC_triggerRateLT #$triggerRateLT :private? t :project SK8) ; #X4 when rate changes to less than trigger
  (defconstant_X TC_triggerRateGT #$triggerRateGT :private? t :project SK8) ; #X8 when rate changes to greater than trigger
  (defconstant_X TC_triggerRateEqual #$triggerRateEqual :private? t :project SK8) ; #X10 when rate changes to equal trigger value
  (defconstant_X TC_triggerRateLTE #$triggerRateLTE :private? t :project SK8) ; #X14
  (defconstant_X TC_triggerRateGTE #$triggerRateGTE :private? t :project SK8) ; #X18
  (defconstant_X TC_triggerRateNotEqual #$triggerRateNotEqual :private? t :project SK8) ; #X1C
  (defconstant_X TC_triggerRateChange #$triggerRateChange :private? t :project SK8) ; 0
  (defconstant_X TC_triggerAtStart #$triggerAtStart :private? t :project SK8) ; #X1
  (defconstant_X TC_triggerAtStop #$triggerAtStop :private? t :project SK8) ; #X2
  (defconstant_X TC_timeBaseBeforeStartTime #$timeBaseBeforeStartTime :private? t :project SK8) ; 1
  (defconstant_X TC_timeBaseAfterStopTime #$timeBaseAfterStopTime :private? t :project SK8) ; 2

  (defconstant_X TC_callBackAtTime         #$callBackAtTime         :private? t :project SK8) ; 1
  (defconstant_X TC_callBackAtRate         #$callBackAtRate         :private? t :project SK8) ; 2
  (defconstant_X TC_callBackAtTimeJump     #$callBackAtTimeJump     :private? t :project SK8) ; 3
  (defconstant_X TC_callBackAtExtremes     #$callBackAtExtremes     :private? t :project SK8) ; 4
  (defconstant_X TC_callBackAtInterrupt    #$callBackAtInterrupt    :private? t :project SK8) ; #X8000
  (defconstant_X TC_callBackAtDeferredTask #$callBackAtDeferredTask :private? t :project SK8) ; #X4000

  (defconstant_X TC_qtcbNeedsRateChanges #$qtcbNeedsRateChanges :private? t :project SK8) ; 1
  (defconstant_X TC_qtcbNeedsTimeChanges #$qtcbNeedsTimeChanges :private? t :project SK8) ; 2
  (defconstant_X TC_qtcbNeedsStartStopChanges #$qtcbNeedsStartStopChanges :private? t :project SK8) ; 4
  (defconstant_X TC_keepInRam #$keepInRam :private? t :project SK8) ; 1
  (defconstant_X TC_unkeepInRam #$unkeepInRam :private? t :project SK8) ; 2
  (defconstant_X TC_flushFromRam #$flushFromRam :private? t :project SK8) ; 4
  (defconstant_X TC_loadForwardTrackEdits #$loadForwardTrackEdits :private? t :project SK8) ; 8
  (defconstant_X TC_loadBackwardTrackEdits #$loadBackwardTrackEdits :private? t :project SK8) ; 16
  (defconstant_X TC_newMovieActive #$newMovieActive :private? t :project SK8) ; #X1
  (defconstant_X TC_newMovieDontResolveDataRefs #$newMovieDontResolveDataRefs :private? t :project SK8) ; #X2
  (defconstant_X TC_newMovieDontAskUnresolvedDataRefs #$newMovieDontAskUnresolvedDataRefs :private? t :project SK8) ; #X4
  (defconstant_X TC_newMovieDontAutoAlternates #$newMovieDontAutoAlternates :private? t :project SK8) ; #X8
  (defconstant_X TC_trackUsageInMovie #$trackUsageInMovie :private? t :project SK8) ; #X2
  (defconstant_X TC_trackUsageInPreview #$trackUsageInPreview :private? t :project SK8) ; #X4
  (defconstant_X TC_trackUsageInPoster #$trackUsageInPoster :private? t :project SK8) ; #X8
  (defconstant_X TC_mediaSampleNotSync #$mediaSampleNotSync :private? t :project SK8) ; 1
  (defconstant_X TC_mediaSampleShadowSync #$mediaSampleShadowSync :private? t :project SK8) ; 2
  (defconstant_X TC_pasteInParallel #$pasteInParallel :private? t :project SK8) ; 1
  (defconstant_X TC_showUserSettingsDialog #$showUserSettingsDialog :private? t :project SK8) ; 2
  (defconstant_X TC_movieToFileOnlyExport #$movieToFileOnlyExport :private? t :project SK8) ; 4
  (defconstant_X TC_nextTimeMediaSample #$nextTimeMediaSample :private? t :project SK8) ; #X1
  (defconstant_X TC_nextTimeMediaEdit #$nextTimeMediaEdit :private? t :project SK8) ; #X2
  (defconstant_X TC_nextTimeTrackEdit #$nextTimeTrackEdit :private? t :project SK8) ; #X4
  (defconstant_X TC_nextTimeSyncSample #$nextTimeSyncSample :private? t :project SK8) ; #X8
  (defconstant_X TC_nextTimeEdgeOK #$nextTimeEdgeOK :private? t :project SK8) ; #X2000
  (defconstant_X TC_nextTimeIgnoreActiveSegment #$nextTimeIgnoreActiveSegment :private? t :project SK8) ; #X4000
  (defconstant_X TC_createMovieFileDeleteCurFile #$createMovieFileDeleteCurFile :private? t :project SK8) ; #X80000000
  (defconstant_X TC_createMovieFileDontCreateMovie #$createMovieFileDontCreateMovie :private? t :project SK8) ; #X40000000
  (defconstant_X TC_createMovieFileDontOpenFile #$createMovieFileDontOpenFile :private? t :project SK8) ; #X20000000
  (defconstant_X TC_flattenAddMovieToDataFork #$flattenAddMovieToDataFork :private? t :project SK8) ; #X1
  (defconstant_X TC_flattenActiveTracksOnly #$flattenActiveTracksOnly :private? t :project SK8) ; #X4
  (defconstant_X TC_flattenDontInterleaveFlatten #$flattenDontInterleaveFlatten :private? t :project SK8) ; #X8
  (defconstant_X TC_movieInDataForkResID #$movieInDataForkResID :private? t :project SK8) ; -1 magic res id
  (defconstant_X TC_movieScrapDontZeroScrap #$movieScrapDontZeroScrap :private? t :project SK8) ; #X1
  (defconstant_X TC_movieScrapOnlyPutMovie #$movieScrapOnlyPutMovie :private? t :project SK8) ; #X2

  (defconstant_X TC_hintsScrubMode       #$hintsScrubMode       :private? t :project SK8) ; #X1
  (defconstant_X TC_hintsUseScreenBuffer #$hintsUseScreenBuffer :private? t :project SK8) ; #X20
  (defconstant_X TC_hintsAllowInterlace  #$hintsAllowInterlace  :private? t :project SK8) ; #X40
  (defconstant_X TC_hintsUseSoundInterp  #$hintsUseSoundInterp  :private? t :project SK8) ; #X80
  (defconstant_X TC_hintsHighQuality     #$hintsHighQuality     :private? t :project SK8) ; #X100
  (defconstant_X TC_hintsPalindrome      #$hintsPalindrome      :private? t :project SK8) ; #X200
  (defconstant_X TC_hintsInactive        #$hintsInactive        :private? t :project SK8) ; #X800

  (defconstant_X TC_movieTrackMediaType      #$movieTrackMediaType      :private? t :project SK8) ; 1
  (defconstant_X TC_movieTrackCharacteristic #$movieTrackCharacteristic :private? t :project SK8) ; 2
  (defconstant_X TC_movieTrackEnabledOnly    #$movieTrackEnabledOnly    :private? t :project SK8) ; 3

  (defconstant_X TC_mediaHandlerFlagBaseClient #$mediaHandlerFlagBaseClient :private? t :project SK8) ; 1

  (defconstant_X TC_txtProcDefaultDisplay #$txtProcDefaultDisplay :private? t :project SK8) ; 0
  (defconstant_X TC_txtProcDontDisplay #$txtProcDontDisplay :private? t :project SK8) ; 1
  (defconstant_X TC_txtProcDoDisplay #$txtProcDoDisplay :private? t :project SK8) ; 2

  (defconstant_X TC_findTextEdgeOK #$findTextEdgeOK :private? t :project SK8) ; 1  Okay to find text at specified
  (defconstant_X TC_findTextCaseSensitive #$findTextCaseSensitive :private? t :project SK8) ; 2 Case sensitive search
  (defconstant_X TC_findTextReverseSearch #$findTextReverseSearch :private? t :project SK8) ; 4 Search from sampleTime backwards
  (defconstant_X TC_findTextWrapAround #$findTextWrapAround :private? t :project SK8) ; 8 Wrap search when beginning or end of
  (defconstant_X TC_findTextUseOffset #$findTextUseOffset :private? t :project SK8) ; 16 Begin search at the given character offset

  (defconstant_X TC_dropShadowOffsetType #$dropShadowOffsetType :private? t :project SK8) ; :|drpo|
  (defconstant_X TC_dropShadowTranslucencyType #$dropShadowTranslucencyType :private? t :project SK8) ; :|drpt|

  (defconstant_X TC_dataRefSelfReference #$dataRefSelfReference :private? t :project SK8) ; 1
  (defconstant_X TC_dataRefWasNotResolved #$dataRefWasNotResolved :private? t :project SK8) ; 2

  (defconstant_X TC_preloadAlways        #$preloadAlways        :private? t :project SK8) ; 1
  (defconstant_X TC_preloadOnlyIfEnabled #$preloadOnlyIfEnabled :private? t :project SK8) ; 2

  (defconstant_X TC_couldNotResolveDataRef #$couldNotResolveDataRef :private? t :project SK8) ; -2000
  (defconstant_X TC_badImageDescription #$badImageDescription :private? t :project SK8) ; -2001
  (defconstant_X TC_badPublicMovieAtom #$badPublicMovieAtom :private? t :project SK8) ; -2002
  (defconstant_X TC_cantFindHandler #$cantFindHandler :private? t :project SK8) ; -2003
  (defconstant_X TC_cantOpenHandler #$cantOpenHandler :private? t :project SK8) ; -2004
  (defconstant_X TC_badComponentType #$badComponentType :private? t :project SK8) ; -2005
  (defconstant_X TC_noMediaHandler #$noMediaHandler :private? t :project SK8) ; -2006
  (defconstant_X TC_noDataHandler #$noDataHandler :private? t :project SK8) ; -2007
  (defconstant_X TC_invalidMedia #$invalidMedia :private? t :project SK8) ; -2008
  (defconstant_X TC_invalidTrack #$invalidTrack :private? t :project SK8) ; -2009
  (defconstant_X TC_invalidMovie #$invalidMovie :private? t :project SK8) ; -2010
  (defconstant_X TC_invalidSampleTable #$invalidSampleTable :private? t :project SK8) ; -2011
  (defconstant_X TC_invalidDataRef #$invalidDataRef :private? t :project SK8) ; -2012
  (defconstant_X TC_invalidHandler #$invalidHandler :private? t :project SK8) ; -2013
  (defconstant_X TC_invalidDuration #$invalidDuration :private? t :project SK8) ; -2014
  (defconstant_X TC_invalidTime #$invalidTime :private? t :project SK8) ; -2015
  (defconstant_X TC_cantPutPublicMovieAtom #$cantPutPublicMovieAtom :private? t :project SK8) ; -2016
  (defconstant_X TC_badEditList #$badEditList :private? t :project SK8) ; -2017
  (defconstant_X TC_mediaTypesDontMatch #$mediaTypesDontMatch :private? t :project SK8) ; -2018
  (defconstant_X TC_progressProcAborted #$progressProcAborted :private? t :project SK8) ; -2019
  (defconstant_X TC_movieToolboxUninitialized #$movieToolboxUninitialized :private? t :project SK8) ; -2020
  (defconstant_X TC_wfFileNotFound #$wfFileNotFound :private? t :project SK8) ; -2021
  (defconstant_X TC_cantCreateSingleForkFile #$cantCreateSingleForkFile :private? t :project SK8) ; -2022
  (defconstant_X TC_invalidEditState #$invalidEditState :private? t :project SK8) ; -2023
  (defconstant_X TC_nonMatchingEditState #$nonMatchingEditState :private? t :project SK8) ; -2024
  (defconstant_X TC_staleEditState #$staleEditState :private? t :project SK8) ; -2025
  (defconstant_X TC_userDataItemNotFound #$userDataItemNotFound :private? t :project SK8) ; -2026
  (defconstant_X TC_maxSizeToGrowTooSmall #$maxSizeToGrowTooSmall :private? t :project SK8) ; -2027
  (defconstant_X TC_badTrackIndex #$badTrackIndex :private? t :project SK8) ; -2028
  (defconstant_X TC_trackIDNotFound #$trackIDNotFound :private? t :project SK8) ; -2029
  (defconstant_X TC_trackNotInMovie #$trackNotInMovie :private? t :project SK8) ; -2030
  (defconstant_X TC_timeNotInTrack #$timeNotInTrack :private? t :project SK8) ; -2031
  (defconstant_X TC_timeNotInMedia #$timeNotInMedia :private? t :project SK8) ; -2032
  (defconstant_X TC_badEditIndex #$badEditIndex :private? t :project SK8) ; -2033
  (defconstant_X TC_internalQuickTimeError #$internalQuickTimeError :private? t :project SK8) ; -2034
  (defconstant_X TC_cantEnableTrack #$cantEnableTrack :private? t :project SK8) ; -2035
  (defconstant_X TC_invalidRect #$invalidRect :private? t :project SK8) ; -2036
  (defconstant_X TC_invalidSampleNum #$invalidSampleNum :private? t :project SK8) ; -2037
  (defconstant_X TC_invalidChunkNum #$invalidChunkNum :private? t :project SK8) ; -2038
  (defconstant_X TC_invalidSampleDescIndex #$invalidSampleDescIndex :private? t :project SK8) ; -2039
  (defconstant_X TC_invalidChunkCache #$invalidChunkCache :private? t :project SK8) ; -2040
  (defconstant_X TC_invalidSampleDescription #$invalidSampleDescription :private? t :project SK8) ; -2041
  (defconstant_X TC_dataNotOpenForRead #$dataNotOpenForRead :private? t :project SK8) ; -2042
  (defconstant_X TC_dataNotOpenForWrite #$dataNotOpenForWrite :private? t :project SK8) ; -2043
  (defconstant_X TC_dataAlreadyOpenForWrite #$dataAlreadyOpenForWrite :private? t :project SK8) ; -2044
  (defconstant_X TC_dataAlreadyClosed #$dataAlreadyClosed :private? t :project SK8) ; -2045
  (defconstant_X TC_endOfDataReached #$endOfDataReached :private? t :project SK8) ; -2046
  (defconstant_X TC_dataNoDataRef #$dataNoDataRef :private? t :project SK8) ; -2047
  (defconstant_X TC_noMovieFound #$noMovieFound :private? t :project SK8) ; -2048
  (defconstant_X TC_invalidDataRefContainer #$invalidDataRefContainer :private? t :project SK8) ; -2049
  (defconstant_X TC_badDataRefIndex #$badDataRefIndex :private? t :project SK8) ; -2050
  (defconstant_X TC_noDefaultDataRef #$noDefaultDataRef :private? t :project SK8) ; -2051
  (defconstant_X TC_couldNotUseAnExistingSample #$couldNotUseAnExistingSample :private? t :project SK8) ; -2052
  (defconstant_X TC_featureUnsupported #$featureUnsupported :private? t :project SK8) ; -2053
  (defconstant_X TC_unsupportedAuxiliaryImportData #$unsupportedAuxiliaryImportData :private? t :project SK8) ; -2057
  (defconstant_X TC_auxiliaryExportDataUnavailable #$auxiliaryExportDataUnavailable :private? t :project SK8) ; -2058
  ;;MCL3.0
  (defconstant_X TC_samplesAlreadyInMedia #$samplesAlreadyInMediaErr :private? t :project SK8) ; -2059
  (defconstant_X TC_movieTextNotFound #$movieTextNotFoundErr :private? t :project SK8) ; -2060

  (defconstant_X TC_noRecordOfApp #$noRecordOfApp :private? t :project SK8) ; #$MovieTOOLBOXUNINITIALIZED replica
  (defconstant_X TC_MovieControllerComponentType #$MovieControllerComponentType :private? t :project SK8) ; :|play|
  (defconstant_X TC_mcTopLeftMovie #$mcTopLeftMovie :private? t :project SK8) ; #X1
  (defconstant_X TC_mcScaleMovieToFit #$mcScaleMovieToFit :private? t :project SK8) ; #X2
  (defconstant_X TC_mcWithBadge #$mcWithBadge :private? t :project SK8) ; #X4
  (defconstant_X TC_mcNotVisible #$mcNotVisible :private? t :project SK8) ; #X8
  (defconstant_X TC_mcWithFrame #$mcWithFrame :private? t :project SK8) ; #X10

  (defconstant_X TC_mcActionIdle #$mcActionIdle :private? t :project SK8) ; 1
  (defconstant_X TC_mcActionDraw #$mcActionDraw :private? t :project SK8) ; 2
  (defconstant_X TC_mcActionActivate #$mcActionActivate :private? t :project SK8) ; 3
  (defconstant_X TC_mcActionDeactivate #$mcActionDeactivate :private? t :project SK8) ; 4
  (defconstant_X TC_mcActionMouseDown #$mcActionMouseDown :private? t :project SK8) ; 5
  (defconstant_X TC_mcActionKey #$mcActionKey :private? t :project SK8) ; 6
  (defconstant_X TC_mcActionPlay #$mcActionPlay :private? t :project SK8) ; 8
  (defconstant_X TC_mcActionGoToTime #$mcActionGoToTime :private? t :project SK8) ; 12
  (defconstant_X TC_mcActionSetVolume #$mcActionSetVolume :private? t :project SK8) ; 14
  (defconstant_X TC_mcActionGetVolume #$mcActionGetVolume :private? t :project SK8) ; 15
  (defconstant_X TC_mcActionStep #$mcActionStep :private? t :project SK8) ; 18
  (defconstant_X TC_mcActionSetLooping #$mcActionSetLooping :private? t :project SK8) ; 21
  (defconstant_X TC_mcActionGetLooping #$mcActionGetLooping :private? t :project SK8) ; 22
  (defconstant_X TC_mcActionSetLoopIsPalindrome #$mcActionSetLoopIsPalindrome :private? t :project SK8) ; 23
  (defconstant_X TC_mcActionGetLoopIsPalindrome #$mcActionGetLoopIsPalindrome :private? t :project SK8) ; 24
  (defconstant_X TC_mcActionSetGrowBoxBounds #$mcActionSetGrowBoxBounds :private? t :project SK8) ; 25
  (defconstant_X TC_mcActionControllerSizeChanged #$mcActionControllerSizeChanged :private? t :project SK8) ; 26
  (defconstant_X TC_mcActionSetSelectionBegin #$mcActionSetSelectionBegin :private? t :project SK8) ; 29
  (defconstant_X TC_mcActionSetSelectionDuration #$mcActionSetSelectionDuration :private? t :project SK8) ; 30
  (defconstant_X TC_mcActionSetKeysEnabled #$mcActionSetKeysEnabled :private? t :project SK8) ; 32
  (defconstant_X TC_mcActionGetKeysEnabled #$mcActionGetKeysEnabled :private? t :project SK8) ; 33
  (defconstant_X TC_mcActionSetPlaySelection #$mcActionSetPlaySelection :private? t :project SK8) ; 34
  (defconstant_X TC_mcActionGetPlaySelection #$mcActionGetPlaySelection :private? t :project SK8) ; 35
  (defconstant_X TC_mcActionSetUseBadge #$mcActionSetUseBadge :private? t :project SK8) ; 36
  (defconstant_X TC_mcActionGetUseBadge #$mcActionGetUseBadge :private? t :project SK8) ; 37
  (defconstant_X TC_mcActionSetFlags #$mcActionSetFlags :private? t :project SK8) ; 38
  (defconstant_X TC_mcActionGetFlags #$mcActionGetFlags :private? t :project SK8) ; 39
  (defconstant_X TC_mcActionSetPlayEveryFrame #$mcActionSetPlayEveryFrame :private? t :project SK8) ; 40
  (defconstant_X TC_mcActionGetPlayEveryFrame #$mcActionGetPlayEveryFrame :private? t :project SK8) ; 41
  (defconstant_X TC_mcActionGetPlayRate #$mcActionGetPlayRate :private? t :project SK8) ; 42
  (defconstant_X TC_mcActionShowBalloon #$mcActionShowBalloon :private? t :project SK8) ; 43
  (defconstant_X TC_mcActionBadgeClick #$mcActionBadgeClick :private? t :project SK8) ; 44
  (defconstant_X TC_mcActionMovieClick #$mcActionMovieClick :private? t :project SK8) ; 45 param is pointer to event record. change
  (defconstant_X TC_mcActionSuspend #$mcActionSuspend :private? t :project SK8) ; 46 no param
  (defconstant_X TC_mcActionResume #$mcActionResume :private? t :project SK8) ; 47 no param
  (defconstant_X TC_mcActionSetControllerKeysEnabled #$mcActionSetControllerKeysEnabled :private? t :project SK8);    param is Boolean
  (defconstant_X TC_mcActionGetTimeSliderRect         #$mcActionGetTimeSliderRect       :private? t :project SK8);    param is pointer to rect
  (defconstant_X TC_mcActionMovieEdited               #$mcActionMovieEdited             :private? t :project SK8); no param
  (defconstant_X TC_mcActionGetDragEnabled            #$mcActionGetDragEnabled          :private? t :project SK8);    param is pointer to Boolean
  (defconstant_X TC_mcActionSetDragEnabled            #$mcActionSetDragEnabled          :private? t :project SK8);    param is Boolean
  (defconstant_X TC_mcActionGetSelectionBegin         #$mcActionGetSelectionBegin       :private? t :project SK8);    param is pointer to TimeRecord
  (defconstant_X TC_mcActionGetSelectionDuration      #$mcActionGetSelectionDuration    :private? t :project SK8);    param is pointer to TimeRecord
  (defconstant_X TC_mcActionPrerollAndPlay            #$mcActionPrerollAndPlay          :private? t :project SK8);    param is Fixed, play rate

  (defconstant_X TC_mcFlagSuppressMovieFrame          #$mcFlagSuppressMovieFrame    :private? t :project SK8) ;  1
  (defconstant_X TC_mcFlagSuppressStepButtons         #$mcFlagSuppressStepButtons   :private? t :project SK8) ;  2
  (defconstant_X TC_mcFlagSuppressSpeakerButton       #$mcFlagSuppressSpeakerButton :private? t :project SK8) ;  4
  (defconstant_X TC_mcFlagsUseWindowPalette           #$mcFlagsUseWindowPalette     :private? t :project SK8) ;  8
  (defconstant_X TC_mcFlagsDontInvalidate             #$mcFlagsDontInvalidate       :private? t :project SK8) ; 16

  (defconstant_X TC_mcPositionDontInvalidate #$mcPositionDontInvalidate :private? t :project SK8) ; 32
  (defconstant_X TC_mcInfoUndoAvailable #$mcInfoUndoAvailable :private? t :project SK8) ; #X1
  (defconstant_X TC_mcInfoCutAvailable #$mcInfoCutAvailable :private? t :project SK8) ; #X2
  (defconstant_X TC_mcInfoCopyAvailable #$mcInfoCopyAvailable :private? t :project SK8) ; #X4
  (defconstant_X TC_mcInfoPasteAvailable #$mcInfoPasteAvailable :private? t :project SK8) ; #X8
  (defconstant_X TC_mcInfoClearAvailable #$mcInfoClearAvailable :private? t :project SK8) ; #X10
  (defconstant_X TC_mcInfoHasSound #$mcInfoHasSound :private? t :project SK8) ; #X20
  (defconstant_X TC_mcInfoIsPlaying #$mcInfoIsPlaying :private? t :project SK8) ; #X40
  (defconstant_X TC_mcInfoIsLooping #$mcInfoIsLooping :private? t :project SK8) ; #X80
  (defconstant_X TC_mcInfoIsInPalindrome #$mcInfoIsInPalindrome :private? t :project SK8) ; #X100
  (defconstant_X TC_mcInfoEditingEnabled #$mcInfoEditingEnabled :private? t :project SK8) ; #X200

  (defconstant_X TC_mcMenuUndo #$mcMenuUndo :private? t :project SK8) ; 1
  (defconstant_X TC_mcMenuCut #$mcMenuCut :private? t :project SK8) ; 3
  (defconstant_X TC_mcMenuCopy #$mcMenuCopy :private? t :project SK8) ; 4
  (defconstant_X TC_mcMenuPaste #$mcMenuPaste :private? t :project SK8) ; 5
  (defconstant_X TC_mcMenuClear #$mcMenuClear :private? t :project SK8) ; 6
  (defconstant_X TC_cannotMoveAttachedController #$cannotMoveAttachedController :private? t :project SK8) ; -9999
  (defconstant_X TC_controllerHasFixedHeight #$controllerHasFixedHeight :private? t :project SK8) ; -9998
  (defconstant_X TC_cannotSetWidthOfAttachedController #$cannotSetWidthOfAttachedController :private? t :project SK8) ; -9997
  (defconstant_X TC_controllerBoundsNotExact #$controllerBoundsNotExact :private? t :project SK8) ; -9996
  (defconstant_X TC_editingNotAllowed #$editingNotAllowed :private? t :project SK8) ; -9995
  (defconstant_X TC_badControllerHeight #$badControllerHeight :private? t :project SK8) ; -9994

   ;; no longer in interface table MCL3.1 and 4.0. May be a bug, but comment this out for now so we can build until it is fixed
#|
  (defconstant_X TC_kMCSetMovieSelect    #$kMCSetMovieSelect    :private? t :project SK8) ; 2
  (defconstant_X TC_kMCGetIndMovieSelect #$kMCGetIndMovieSelect :private? t :project SK8) ; 5
  (defconstant_X TC_kMCRemoveMovieSelect #$kMCRemoveMovieSelect :private? t :project SK8) ; 6
  (defconstant_X TC_kMCIsPlayerEventSelect #$kMCIsPlayerEventSelect :private? t :project SK8) ; 7
  (defconstant_X TC_kMCSetActionFilterSelect #$kMCSetActionFilterSelect :private? t :project SK8) ; 8
  (defconstant_X TC_kMCDoActionSelect #$kMCDoActionSelect :private? t :project SK8) ; 9
  (defconstant_X TC_kMCSetControllerAttachedSelect #$kMCSetControllerAttachedSelect :private? t :project SK8) ; #XA
  (defconstant_X TC_kMCIsControllerAttachedSelect #$kMCIsControllerAttachedSelect :private? t :project SK8) ; #XB
  (defconstant_X TC_kMCSetControllerPortSelect #$kMCSetControllerPortSelect :private? t :project SK8) ; #XC
  (defconstant_X TC_kMCGetControllerPortSelect #$kMCGetControllerPortSelect :private? t :project SK8) ; #XD
  (defconstant_X TC_kMCGetVisibleSelect #$kMCGetVisibleSelect :private? t :project SK8) ; #XE
  (defconstant_X TC_kMCSetVisibleSelect #$kMCSetVisibleSelect :private? t :project SK8) ; #XF
  (defconstant_X TC_kMCGetControllerBoundsRectSelect #$kMCGetControllerBoundsRectSelect :private? t :project SK8) ; #X10
  (defconstant_X TC_kMCSetControllerBoundsRectSelect #$kMCSetControllerBoundsRectSelect :private? t :project SK8) ; #X11
  (defconstant_X TC_kMCGetControllerBoundsRgnSelect #$kMCGetControllerBoundsRgnSelect :private? t :project SK8) ; #X12
  (defconstant_X TC_kMCGetWindowRgnSelect #$kMCGetWindowRgnSelect :private? t :project SK8) ; #X13
  (defconstant_X TC_kMCMovieChangedSelect #$kMCMovieChangedSelect :private? t :project SK8) ; #X14
  (defconstant_X TC_kMCSetDurationSelect #$kMCSetDurationSelect :private? t :project SK8) ; #X15
  (defconstant_X TC_kMCGetCurrentTimeSelect #$kMCGetCurrentTimeSelect :private? t :project SK8) ; #X16
  (defconstant_X TC_kMCNewAttachedControllerSelect #$kMCNewAttachedControllerSelect :private? t :project SK8) ; #X17
  (defconstant_X TC_kMCDrawSelect #$kMCDrawSelect :private? t :project SK8) ; #X18
  (defconstant_X TC_kMCActivateSelect #$kMCActivateSelect :private? t :project SK8) ; #X19
  (defconstant_X TC_kMCIdleSelect #$kMCIdleSelect :private? t :project SK8) ; #X1A
  (defconstant_X TC_kMCKeySelect #$kMCKeySelect :private? t :project SK8) ; #X1B
  (defconstant_X TC_kMCClickSelect #$kMCClickSelect :private? t :project SK8) ; #X1C
  (defconstant_X TC_kMCEnableEditingSelect #$kMCEnableEditingSelect :private? t :project SK8) ; #X1D
  (defconstant_X TC_kMCIsEditingEnabledSelect #$kMCIsEditingEnabledSelect :private? t :project SK8) ; #X1E
  (defconstant_X TC_kMCCopySelect #$kMCCopySelect :private? t :project SK8) ; #X1F
  (defconstant_X TC_kMCCutSelect #$kMCCutSelect :private? t :project SK8) ; #X20
  (defconstant_X TC_kMCPasteSelect #$kMCPasteSelect :private? t :project SK8) ; #X21
  (defconstant_X TC_kMCClearSelect #$kMCClearSelect :private? t :project SK8) ; #X22
  (defconstant_X TC_kMCUndoSelect #$kMCUndoSelect :private? t :project SK8) ; #X23
  (defconstant_X TC_kMCPositionControllerSelect #$kMCPositionControllerSelect :private? t :project SK8) ; #X24
  (defconstant_X TC_kMCGetControllerInfoSelect #$kMCGetControllerInfoSelect :private? t :project SK8) ; #X25
  (defconstant_X TC_kMCSetClipSelect #$kMCSetClipSelect :private? t :project SK8) ; #X28
  (defconstant_X TC_kMCGetClipSelect #$kMCGetClipSelect :private? t :project SK8) ; #X29
  (defconstant_X TC_kMCDrawBadgeSelect #$kMCDrawBadgeSelect :private? t :project SK8) ; #X2A
  (defconstant_X TC_kMCSetUpEditMenuSelect #$kMCSetUpEditMenuSelect :private? t :project SK8) ; #X2B
  (defconstant_X TC_kMCGetMenuStringSelect #$kMCGetMenuStringSelect :private? t :project SK8) ; #X2C
  (defconstant_X TC_kMCSetActionFilterWithRefConSelect #$kMCSetActionFilterWithRefConSelect :private? t :project SK8) ; #X2D
|#
  )

(defvar_XE CB_QTcoverProc nil)
(defvar_XE CB_QTuncoverProc nil)
(defvar_XE CB_QTErrProc nil)
(defvar_XE CB_QTCalloutProc nil)
(defvar_XE CB_QTCallBack nil)
(defvar_XE CB_QTTextProc nil)
(defvar_XE CB_QTMovieDrawingCompleteProc nil)
(defvar_XE CB_QTActionFilter nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Functions and macros for argument defaults

(defmacro progressProcDefaulter (progressProc)
  `(case ,progressProc
    (standardMovieProgessFunction
     (%coerce-to-macptr $useStandardMovieProgessFunction))
    (otherwise
     ,progressProc)))

(defun_X checkTimeValue :private (timeValue)
  "Raise an error if timeValue won't fit into a 32-bit signed integer.
Otherwise, return timeValue.
Not a trap."
  (when (or (< timeValue (- (expt 2 31) 1))
            (> timeValue (- (expt 2 31))))
    (error "QuickTime TimeValue exceeds 32 bits"))
  timeValue)

;;;? should guarantee that startTime is within movie
(defun T_getMovieTimeAndRateArgs (theMovie startTime rate)
  "The startTime arg defaults to the movie's current timeValue.
The rate arg defaults to the preferredRate and is a normal number,
which is converted into a Fixed as required by the traps.
Not a trap."
  (unless startTime
    (setq startTime (checking-toolbox-error (:Value (#_GetMoviesError))
                                            (#_GetMovieTime theMovie (%null-ptr)))))
  (let ((rateFixed (if rate
                     (T_numberToFixed rate)
                     (checking-toolbox-error (:Value (#_GetMoviesError))
                                             (#_GetMoviePreferredRate theMovie)))))
    (values_X startTime rateFixed)))

;;;? should guarantee that startTime is within movie
(defun T_getTrackTimeAndRateArgs (theTrack startTime rate)
  "The startTime arg defaults to the track time corresponding to the current movie time.
The rate arg, a normal number defaulting to the movie's preferredRate,
is returned as a Fixed as required by the traps.
Not a trap."
  (let (rateFixed)
    (if (and startTime rate)
      (setf rateFixed (T_numberToFixed rate))
      (progn
        (when rate
          (set rateFixed (T_numberToFixed rate)))
        (let ((theMovie (checking-toolbox-error (:value (#_GetMoviesError))
                                                (#_GetTrackMovie theTrack))))
          (unless startTime
            (setq startTime (- (checking-toolbox-error (:Value (#_GetMoviesError))
                                                       (#_GetMovieTime theMovie (%null-ptr)))
                               (checking-toolbox-error (:Value (#_GetMoviesError))
                                                       (#_GetTrackOffset theTrack)))))
          (unless rate
            (setf rateFixed (checking-toolbox-error (:Value (#_GetMoviesError))
                                                    (#_GetMoviePreferredRate theMovie)))))))
    (values_X startTime rateFixed)))

(defun T_getMovieTimeAndDurationArgs (theMovie startTime duration)
  "If startTime and duration are nil, we assume you mean the whole movie.
Otherwise, startTime defaults to the movie's current timeValue.
If preferredRate is non-negative, duration defaults to the rest of the movie.
If preferredRate is negative duration defaults to -startTime.
In any case, startTime and duration are adjusted so neither is negative,
and so that the segment doesn't extend beyond the end of the movie.
Not a trap."
  (let ((movieDuration (checking-toolbox-error (:value (#_GetMoviesError))
                                               (#_GetMovieDuration theMovie))))
    (unless (and startTime duration)
      (if (or startTime duration)
        (progn
          (unless startTime
            ;;duration is set, but not startTime
            (setq startTime (checking-toolbox-error (:Value (#_GetMoviesError))
                                                    (#_GetMovieTime theMovie (%null-ptr)))))
          (unless duration
            ;;startTime is set, but not duration
            (if (>= (checking-toolbox-error (:Value (#_GetMoviesError))
                                            (#_GetMoviePreferredRate theMovie))
                    0)
              (setq duration (- movieDuration startTime))
              (setq duration (- startTime)))))
        (progn
          ;; Neither was set, so we assume the whole movie
          (setq startTime 0)
          (setq duration movieDuration))))
    ;; Ensure that the specified segment is within the movie's limits
    (when (< duration 0)
      ;; Adjust startTime and duration so duration is positive.
      (if (< startTime 0)
        (progn
          (setq startTime 0)
          (setq duration 0))
        (progn
          (setf startTime (+ startTime duration))
          (setf duration (- duration)))))
    (let ((gapBefore (- 0 startTime)))
      ;; Limit startTime to no less than 0
      (if (> gapBefore 0)
        (progn
          (setq startTime 0)
          (setq duration (- duration gapBefore)))
        (progn
          ;; Limit startTime to no greater than movieDuration
          (let ((gapAfter (- startTime movieDuration)))
            (when (> gapAfter 0)
              (setq startTime movieDuration)
              (setq duration 0))))))
    ;; Limit duration so it doesn't go beyond end of movie
    (let ((overtime (- (+ startTime duration)
                       movieDuration)))
      (when (> overtime 0)
        (setq duration (- duration overTime)))))
      (values_X startTime duration))

(defun T_getTrackTimeArgs (theTrack timeValue)
  "timeValue defaults to the track time corresponding
to the current movie time or 0, whichever is greater.
Not a trap."
  (unless timeValue
    (let ((theMovie (checking-toolbox-error (:value (#_GetMoviesError))
                                            (#_GetTrackMovie theTrack))))
      (setq timeValue (- (checking-toolbox-error (:Value (#_GetMoviesError))
                                                 (#_GetMovieTime theMovie (%null-ptr)))
                         (checking-toolbox-error (:value (#_GetMoviesError))
                                                 (#_GetTrackOffset theTrack))))
      ;; timeValue might now be negative
      ))
  (when (< timeValue 0)
    ;; Limit timeValue to no less than 0
    (setq timeValue 0))
  timeValue)

(defun T_getTrackTimeAndDurationArgs (theTrack startTime duration)
  "If startTime and duration are nil, we assume you mean the whole track.
Otherwise, startTime defaults to the track time corresponding
to the current movie time or 0, whichever is greater.
If preferredRate is non-negative, duration defaults to the rest of the track.
If preferredRate is negative duration defaults to -startTime.
In any case, startTime and duration are adjusted so neither is negative,
and so that the segment doesn't extend beyond the end of the track.
Not a trap."
  (let* ((trackOffset (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetTrackOffset theTrack)))
         (trackDuration (- (checking-toolbox-error (:value (#_GetMoviesError))
                                                   (#_GetTrackDuration theTrack))
                           trackOffset)))
    (unless (and startTime duration)
      (if (or startTime duration)
        (let ((theMovie (checking-toolbox-error (:value (#_GetMoviesError))
                                                (#_GetTrackMovie theTrack))))
          (unless startTime
            ;;duration is set, but not startTime
            (setq startTime (- (checking-toolbox-error (:Value (#_GetMoviesError))
                                                       (#_GetMovieTime theMovie (%null-ptr)))
                               trackOffset))
            ;; startTime might now be negative
            )
          ;; Now we have a startTime
          (unless duration
            ;;startTime is set, but not duration
            (if (>= (checking-toolbox-error (:Value (#_GetMoviesError))
                                            (#_GetMoviePreferredRate theMovie))
                    0)
              (setq duration (- trackDuration startTime))
              (setq duration (- startTime)))))
        (progn
          ;; Neither was set, so we assume the whole track.
          (setq startTime 0)
          (setq duration trackDuration))))
    ;; Ensure that the specified segment is within the track's limits
    (when (< duration 0)
      ;; Adjust startTime and duration so duration is positive.
      (if (< startTime 0)
        (progn
          (setq startTime 0)
          (setq duration 0))
        (progn
          (setf startTime (+ startTime duration))
          (setf duration (- duration)))))
    (let ((gapBefore (- 0 startTime)))
      ;; Limit startTime to no less than 0
      (if (> gapBefore 0)
        (progn
          (setq startTime 0)
          (setq duration (- duration gapBefore)))
        (progn
          ;; Limit startTime to no greater than trackDuration
          (let ((gapAfter (- startTime trackDuration)))
            (when (> gapAfter 0)
              (setq startTime trackDuration)
              (setq duration 0))))))
    ;; Limit duration so it doesn't go beyond end of track
    (let ((overtime (- (+ startTime duration)
                       trackDuration)))
      (when (> overtime 0)
        (setq duration (- duration overTime)))))
  (values_X startTime duration))

;;;? redo this
(defun T_getMediaTimeAndDurationArgs (theMedia startTime duration)
  "If startTime and duration are nil, we assume you mean the whole media.
The startTime arg defaults to 0.
The duration arg defaults to the rest of the media.
Not a trap."
  (unless (or startTime duration)
    (setq startTime 0))
  (unless startTime
    ;;duration is set, but not startTime
    (setq startTime 0))
  (unless duration
    ;;startTime is set, but not duration
    (setq duration (- (#_GetMediaDuration theMedia)
                      startTime)))
  (values_X startTime duration))

;;; Type transformations

(defun_X T_matrixArrayToMatrixRecord :private (matrixArray)
  "takes a list or lisp array and returns a macptr
to a movie matrix array (with Fixed and Fract numbers in it).
Not a trap."
  (if (arrayp matrixArray)
    (let ((matrixRecordPtr (make-record :MatrixRecord)))
      (dotimes (row 3)
        (setf (rref matrixRecordPtr (matrixRecord.matrix row 0))
              (T_numberToFixed   (aref matrixArray         row 0)))
        (setf (rref matrixRecordPtr (matrixRecord.matrix row 1))
              (T_numberToFixed   (aref matrixArray         row 1)))
        (setf (rref matrixRecordPtr (matrixRecord.matrix row 2))
              (T_numberToFract   (aref matrixArray         row 2))))
      matrixRecordPtr)
    (let ((matrixRecordPtr (make-record (:MatrixRecord :clear t))))
      (loop for ind from 0 below 9
            for row = 0 then (floor ind 3)
            for col = 0 then (mod ind 3)
            for elem in matrixArray
            do (if (= col 2)
                 (setf (rref matrixRecordPtr (matrixRecord.matrix row 2))   (T_numberToFract elem))
                 (setf (rref matrixRecordPtr (matrixRecord.matrix row col)) (T_numberToFixed elem))))
      matrixRecordPtr)))

(defun_X T_matrixRecordToMatrixArray :private (matrixRecordPtr)
  "takes a macptr to a movie matrix array (with Fixed and Fract numbers in it)
and returns a lisp array of numbers.
Not a trap."
  (let ((matrixArray (make-array '(3 3))))
    (dotimes (row 3)
        (setf                                   (aref matrixArray         row 0)
              (T_fixedToNumber (rref matrixRecordPtr (matrixRecord.matrix row 0))))
        (setf                                   (aref matrixArray         row 1)
              (T_fixedToNumber (rref matrixRecordPtr (matrixRecord.matrix row 1))))
        (setf                                   (aref matrixArray         row 2)
              (T_fractToNumber (rref matrixRecordPtr (matrixRecord.matrix row 2)))))
    matrixArray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Lisp wrappers for the QuickTime Movie Toolbox calls
;;;
;;;  These wrappers relieve the caller of the need to use any MCL specific
;;;  constructs, such as rlet, with-pstrs, etc.

(defun_X T_NewMovieFromFileGC nil
         (resRefNum &key
                    (resourceID 0)
                    resourceName
                    (flags 0)
                    ((active active_)                  nil active_Set)
                    (dontResolveDataReferences       nil dontResolveDataReferencesSet)
                    (dontAskUnresovedDataReferences  nil dontAskUnresovedDataReferencesSet)
                    (dontAutoAlternates              nil dontAutoAlternatesSet)
                    notGC)
  "Returns 4 values: movieGCOSPtr resourceID dataReferenceWasChanged couldNotResolveDataReference.
Return value notes:
   If resourceID argument is 0, resourceID return value will be the actual resource ID found.
See IM-QuickTime p. 2-88"
  (declare (type integer resRefNum)
           (type integer resourceID)
           (type string resourceName)
           (type integer flags))
  (trap-flag-arg flags active_                        #$newMovieActive)
  (trap-flag-arg flags dontResolveDataReferences      #$newMovieDontResolveDataRefs)
  (trap-flag-arg flags dontAskUnresovedDataReferences #$newMovieDontAskUnresolvedDataRefs)
  (trap-flag-arg flags dontAutoAlternates             #$newMovieDontAutoAlternates)
  (rlet ((theMovie (:pointer :movie))
         (resIDResult :signed-integer resourceID)
         (dataRefWasChanged :boolean))
    (let ((couldNotResolveDataReference
           (eql #$couldNotResolveDataRef
                (if resourceName
                  (with-pstrs ((resNamePstr resourceName))
                    (checking-toolbox-error
                     (:OSErr nil :ok-errors '(#.#$couldNotResolveDataRef))
                     (#_NewMovieFromFile
                      theMovie
                      resRefNum
                      resIDResult
                      resNamePstr
                      flags
                      dataRefWasChanged
                      )))
                  (checking-toolbox-error
                   (:OSErr nil :ok-errors '(#.#$couldNotResolveDataRef))
                   (#_NewMovieFromFile
                    theMovie
                    resRefNum
                    resIDResult
                    (%null-ptr)
                    flags
                    dataRefWasChanged
                    ))))))
      (values_X
        (if notGC
          (%get-ptr theMovie)
          (T_makeGCOSPtr (%get-ptr theMovie) 'T_DisposeMovie))
        (%get-signed-word resIDResult)
        (%get-boolean dataRefWasChanged)
        couldNotResolveDataReference))))

(defun_X T_NewMovieFromHandleGC nil
         (handle &key
                 (flags 0)
                 ((active active_)                 nil active_Set)
                 (dontResolveDataReferences      nil dontResolveDataReferencesSet)
                 (dontAskUnresovedDataReferences nil dontAskUnresovedDataReferencesSet)
                 (dontAutoAlternates             nil dontAutoAlternatesSet))
  "Returns 3 values: movieGCOSPtr dataReferenceWasChanged couldNotResolveDataReference.
Return value notes:
   dataReferenceWasChanged is always returned.
See IM-QuickTime p. 2-90"
  (declare (type :handle handle)
           (type integer flags))
  (trap-flag-arg flags active_                        #$newMovieActive)
  (trap-flag-arg flags dontResolveDataReferences      #$newMovieDontResolveDataRefs)
  (trap-flag-arg flags dontAskUnresovedDataReferences #$newMovieDontAskUnresolvedDataRefs)
  (trap-flag-arg flags dontAutoAlternates             #$newMovieDontAutoAlternates)
  (rlet ((theMovie (:pointer :movie))
         (dataRefWasChanged :boolean))
    (let ((couldNotResolveDataReference
           (eql #$couldNotResolveDataRef 
                (checking-toolbox-error (:OSErr nil :ok-errors '(#.#$couldNotResolveDataRef))
                                        (#_NewMovieFromHandle
                                         theMovie
                                         handle
                                         flags
                                         dataRefWasChanged
                                         )))))
      (values_X
        (T_makeGCOSPtr (%get-ptr theMovie) 'T_DisposeMovie)
        (%get-boolean dataRefWasChanged)
        couldNotResolveDataReference))))

(defun_X T_NewMovieGC nil
         (&key
          (flags 0)
          ((active active_)   nil active_Set)
          (dontAutoAlternates nil dontAutoAlternatesSet))
  "Returns movieGCOSPtr.
See IM-QuickTime p. 2-90"
  (declare (type integer flags))
  (trap-flag-arg flags active_                  #$newMovieActive)
  (trap-flag-arg flags dontAutoAlternates       #$newMovieDontAutoAlternates)
  (T_makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_NewMovie flags))
                 'T_DisposeMovie))

(defun_X T_ConvertFileToMovieFile :private (inputFileSpecHandle outputFileSpecHandle creator
                                              &key
                                              (scriptTag #$smSystemScript)
                                              (flags #$showUserSettingsDialog)
                                              (deleteCurrentFile      nil deleteCurrentFileSet)
                                              (showUserSettingsDialog t   showUserSettingsDialogSet)
                                              (userComponentInstance $autoSelectExportComponentInstance)
                                              (progressProc 'standardMovieProgessFunction)
                                              (refCon 0))
  "Returns resourceID.
Return value notes:
   Always returns resID.
See IM-QuickTime p. 2-93"
  (declare (type :FSSpec  inputFileSpecHandle)
           (type :FSSpec outputFileSpecHandle)
           (type integer creator) ; :OSType
           (type :ScriptCode scriptTag)
           (type integer flags)
           )
  (trap-flag-arg flags deleteCurrentFile      #$createMovieFileDeleteCurFile)
  (trap-flag-arg flags showUserSettingsDialog #$showUserSettingsDialog)
  (rlet ((resID :signed-integer))
    (with-pointers ((inputFileSpecPtr  inputFileSpecHandle)
                    (outputFileSpecPtr outputFileSpecHandle))
      (checking-toolbox-error (:OSErr)
                              (#_ConvertFileToMovieFile
                               inputFileSpecPtr
                               outputFileSpecPtr
                               creator
                               scriptTag
                               resID
                               flags
                               (%coerce-to-macptr userComponentInstance)
                               (progressProcDefaulter progressProc)
                               refCon)))
    (%get-signed-word resID)))

(defun_X T_ConvertMovieToFile :private (theMovie
                                         outputFileSpecHandle
                                         creator
                                         &key
                                         (fileType "MooV")
                                         (onlyTrack $convertAllTracks)
                                         (scriptTag #$smSystemScript)
                                         (flags 0)
                                         (showUserSettingsDialog nil showUserSettingsDialogSet)
                                         (onlyExport nil onlyExportSet)
                                         (userComponentInstance $autoSelectExportComponentInstance))
  "Returns resID.
Return value notes:
   Always returns resID.
See IM-QuickTime p. 2-95"
  (declare (type :Movie theMovie)
           (type :FSSpec outputFileSpecHandle)
           (type integer creator) ; :OSType
           (type integer fileType) ; :OSType
           (type :ScriptCode scriptTag)
           (type :ComponentInstance userComp)) ; export component or component instance
  (trap-flag-arg flags showUserSettingsDialog #$showUserSettingsDialog)
  (trap-flag-arg flags onlyExport             #$movieToFileOnlyExport)
  (rlet ((resID :signed-integer))
    (with-pointers ((outputFileSpecPtr outputFileSpecHandle))
      (checking-toolbox-error (:OSErr)
                              (#_ConvertMovieToFile
                               theMovie
                               (%coerce-to-macptr onlyTrack)
                               outputFileSpecPtr
                               fileType
                               creator
                               scriptTag
                               resID
                               flags
                               (%coerce-to-macptr userComponentInstance))))
    (%get-signed-word resID)))

(defun_X T_DisposeMovie :private (theMovie)
  "If theMovie is a terminable macptr, then it is deactivated with disposeNow;
otherwise, this simply calls the DisposeMovie trap.
See IM-QuickTime p. 2-96"
  (declare (type :Movie theMovie))
  (if (terminable-macptr-p theMovie)
    (T_deactivateGCOSPtr theMovie :disposeNow t)
    (progn
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_DisposeMovie theMovie))
      ;; MUST return t or it will be called twice by the reaper!
      t)))

(defun_X T_CreateMovieFileGC :private (fileSpecHandle creator
                                                       &key
                                                       (scriptTag #$smSystemScript)
                                                       (flags 0)
                                                       ((active active_)     nil active_Set)
                                                       (deleteCurrentFile  nil deleteCurrentFileSet)
                                                       (dontCreateMovie    nil dontCreateMovieSet)
                                                       (dontOpenFile       nil dontOpenFileSet)
                                                       (dontAutoAlternates nil dontAutoAlternatesSet))
  "Returns multiple values:
   resRefNum    - nil if dontOpenFile    flag or keyword argument is selected
   movieGCOSPtr - nil if dontCreateMovie flag or keyword argument is selected
See IM-QuickTime p. 2-96"
  (declare ; (type :FSSpec fileSpecHandle)
   (type integer creator) ; :OSType
   (type :ScriptCode scriptTag)
   (type integer flags))
  
  (trap-flag-arg flags active_            #$newMovieActive)
  (trap-flag-arg flags deleteCurrentFile  #$createMovieFileDeleteCurFile)
  (trap-flag-arg flags dontCreateMovie    #$createMovieFileDontCreateMovie)
  (trap-flag-arg flags dontOpenFile       #$createMovieFileDontOpenFile)
  (trap-flag-arg flags dontAutoAlternates #$newMovieDontAutoAlternates)
  
  (rlet ((resRefNum :signed-integer)
         (newMovie (:pointer :movie)))
    (with-pointers ((fileSpecPtr fileSpecHandle))
      (checking-toolbox-error (:OSErr)
                              (#_CreateMovieFile
                               fileSpecPtr
                               creator
                               scriptTag
                               flags
                               resRefNum
                               newMovie)))
    (values_X
      (unless (logtest flags #$createMovieFileDontOpenFile)
        (%get-signed-word resRefNum))
      (unless (logtest flags #$createMovieFileDontCreateMovie)
        (T_makeGCOSPtr (%get-ptr newMovie) 'T_DisposeMovie)))))

(defun_X T_OpenMovieFile :private (fileSpecHandle
                                   &key
                                   (permissions #$fsCurPerm))
  "Returns resRefNum.
See also T_disposeMovieFileRefNumGCHandle.
See IM-QuickTime p. 2-98"
  (declare ; (type (:handle :fsspec) fileSpecHandle)
   (type integer permissions))
  (rlet ((resRefNum :signed-integer))
    (with-pointers ((fileSpecPtr fileSpecHandle))
      (checking-toolbox-error (:OSErr)
                              (#_OpenMovieFile fileSpecPtr resRefNum permissions))
      (%get-signed-word resRefNum))))

(defun_X T_CloseMovieFile :private (resRefNum)
  "See also T_disposeMovieFileRefNumGCHandle.
See IM-QuickTime p. 2-99"
  (declare (type integer resRefNum))
  (checking-toolbox-error (:OSErr)
                          (#_CloseMovieFile resRefNum))
  (values_X))

(defun_X T_disposeMovieFileRefNumGCHandle :private (fileRefNumGCHandle)
  "Returns t to make the GC disposal stuff happy.
Gets a FileRefNum out of the handle, calls CloseMovieFile on the file
referred to by FileRefNum, then disposes the handle.
Not a trap.  To be used as a GCOSPtr termination function."
  (T_CloseMovieFile (href fileRefNumGCHandle refnum.value))
  (T_DisposeHandle fileRefNumGCHandle)
  ;; MUST return t or it will be called twice by the reaper!
  t)

(defun_X T_DeleteMovieFile :private (fileSpecHandle)
  "See IM-QuickTime p. 2-100"
  ; (declare (type (:handle :fsspec) fileSpecHandle))
  (with-pointers ((fileSpecPtr fileSpecHandle))
    (checking-toolbox-error (:OSErr)
                            (#_DeleteMovieFile fileSpecPtr)))
  (values_X))

(defun_X T_HasMovieChanged :private (theMovie)
  "See IM-QuickTime p. 2-101"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_HasMovieChanged theMovie))
  (values_X))

(defun_X T_ClearMovieChanged :private (theMovie)
  "See IM-QuickTime p. 2-102"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_ClearMovieChanged theMovie))
  (values_X))

(defun_X T_AddMovieResource :private (theMovie resRefNum
                                             &key
                                             (resourceID 0)
                                             resourceName)
  "Always returns the resourceID result.
See IM-QuickTime p. 2-102"
  (rlet ((resIDResult :signed-integer resourceID))
    (if resourceName
      (with-pstrs ((resNamePstr resourceName))
        (checking-toolbox-error (:OSErr)
                                (#_AddMovieResource
                                 theMovie
                                 resRefNum
                                 resIDResult
                                 resNamePstr)))
      (checking-toolbox-error (:OSErr)
                              (#_AddMovieResource
                               theMovie
                               resRefNum
                               resIDResult
                               (%null-ptr))))
    (%get-signed-word resIDResult)))

(defun_X T_UpdateMovieResource :private (theMovie
                                        resRefNum
                                        resourceID
                                        &key
                                        resourceName)
  "See IM-QuickTime p. 2-103"
  (if resourceName
    (with-pstrs ((resNamePstr resourceName))
      (checking-toolbox-error (:OSErr)
                              (#_UpdateMovieResource
                               theMovie
                               resRefNum
                               resourceID
                               resNamePstr)))
    (checking-toolbox-error (:OSErr)
                            (#_UpdateMovieResource
                             theMovie
                             resRefNum
                             resourceID
                             (%null-ptr))))
  (values_X))

(defun_X T_RemoveMovieResource :private (resRefNum resourceID)
  "See IM-QuickTime p. 2-104"
  (checking-toolbox-error (:OSErr)
                          (#_RemoveMovieResource resRefNum resourceID))
  (values_X))

(defun_X T_PutMovieIntoHandleGC :private (theMovie &optional publicMovie)
  "Returns the handle.
If you don't supply a handle, a GCHandle will be created.
See IM-QuickTime p. 2-104"
  (unless publicMovie
    (setf publicMovie (T_NewHandleGC)))
  (checking-toolbox-error (:OSErr)
                          (#_PutMovieIntoHandle theMovie publicMovie))
  publicMovie)

(defun_X T_FlattenMovie nil
         (theMovie theFile &key
                   (flattenFlags 0)
                   (addMovieToDataFork nil addMovieToDataForkSet)
                   (dontInterleave     nil dontInterleaveSet)
                   (activeTracksOnly   nil activeTracksOnlySet)
                   (creator "MooV")
                   (scriptTag #$smSystemScript)
                   (creationFlags 0)
                   (deleteCurrentFile nil deleteCurrentFileSet)
                   (resourceID 0)
                   resourceName)
  "Always returns the resourceID result.
See IM-QuickTime p. 2-105"
  (trap-flag-arg flattenFlags  addMovieToDataFork #$flattenAddMovieToDataFork)
  (trap-flag-arg flattenFlags  dontInterleave     #$flattenDontInterleaveFlatten)
  (trap-flag-arg flattenFlags  activeTracksOnly   #$flattenActiveTracksOnly)
  (trap-flag-arg creationFlags deleteCurrentFile  #$createMovieFileDeleteCurFile)
  (rlet ((resIDResult :signed-integer resourceID))
    (if resourceName
      (with-pstrs ((resNamePstr resourceName))
        (checking-toolbox-error (:OSErr)
                                (#_FlattenMovie
                                 theMovie
                                 flattenFlags
                                 theFile
                                 creator
                                 scriptTag
                                 creationFlags
                                 resIDResult
                                 resNamePstr)))
      (checking-toolbox-error (:OSErr)
                              (#_FlattenMovie
                               theMovie
                               flattenFlags
                               theFile
                               creator
                               scriptTag
                               creationFlags
                               resIDResult
                               (%null-ptr))))
    (%get-signed-word resIDResult)))

(defun_X T_FlattenMovieDataGC nil
         (theMovie theFile &key
                   (flattenFlags 0)
                   (addMovieToDataFork nil addMovieToDataForkSet)
                   (dontInterleave     nil dontInterleaveSet)
                   (activeTracksOnly   nil activeTracksOnlySet)
                   (creator "MooV")
                   (scriptTag #$smSystemScript)
                   (creationFlags 0)
                   (deleteCurrentFile nil deleteCurrentFileSet))
  "Returns the new Movie.
See IM-QuickTime p. 2-107"
  (trap-flag-arg flattenFlags    addMovieToDataFork #$flattenAddMovieToDataFork)
  (trap-flag-arg flattenFlags    dontInterleave     #$flattenDontInterleaveFlatten)
  (trap-flag-arg flattenFlags    activeTracksOnly   #$flattenActiveTracksOnly)
  (trap-flag-arg creationFlags deleteCurrentFile  #$createMovieFileDeleteCurFile)
  (T_makeGCOSPtr
   (checking-toolbox-error (:pointer (#_GetMoviesError))
                           (#_FlattenMovieData
                            theMovie
                            flattenFlags
                            theFile
                            creator
                            scriptTag
                            creationFlags))
   'T_DisposeMovie))

;;; Find an atom in a QuickTime file data fork
(defun findAtom (theFile &key (startOffset 0) atomType atomNumber)
  "Returns 3 values: fileOffset of the atom, fileOffset of the atom data, atomType (a string).
See IM-QuickTime, p. 4-4."
  (values_X 0 8 "    ")) ; place holders for the real return values when this function is finished
#|
  (let ((pathname (if (integerp theFile)
                    (refNumToPathname fileRefNum)
                    theFile)))
    (with-open-file (fileStream pathname)
      (let ((curPosition startOffset)
            (ind 1))
        (do ()
          (let ((atom (getAtom fileStream)))
            (when (and (or (not atomType)
                           (string= (type atom) atomType))
                       (= ind atomNumber))
                (return))
            (setf curPosition (+ curPosition (size atom)))
            (file-position fileStream curPosition))) ; this will fail if we've gone too far
        (setf fileOffset (+ curPosition atomHeaderSize))))))
 |#

(defun_X T_NewMovieFromDataForkGC nil
         (fileRefNum &key
                     movieNumber
                     fileOffset
                     (flags 0)
                     ((active active_)                  nil active_Set)
                     (dontResolveDataReferences       nil dontResolveDataReferencesSet)
                     (dontAskUnresovedDataReferences  nil dontAskUnresovedDataReferencesSet)
                     (dontAutoAlternates              nil dontAutoAlternatesSet)
                     notGC)
  "Returns 4 values: movieGCOSPtr fileOffset dataReferenceWasChanged couldNotResolveDataReference.
If movieNumber is specified, then the file is assumed to be a packed succession of atoms as
described in IM-QuickTime, p. 4-4, and the nth 'moov' atom in the file is opened.  Otherwise,
if fileOffset is specified, then movie data is expected to start at that position in the file.
If neither is specified, then movieNumber defaults to 1, which refers to the first 'moov' in
the file.  See IM-QuickTime p. 2-109"
  (declare (type integer fileRefNum)
           (type integer flags))
  (trap-flag-arg flags active_                        #$newMovieActive)
  (trap-flag-arg flags dontResolveDataReferences      #$newMovieDontResolveDataRefs)
  (trap-flag-arg flags dontAskUnresovedDataReferences #$newMovieDontAskUnresolvedDataRefs)
  (trap-flag-arg flags dontAutoAlternates             #$newMovieDontAutoAlternates)
  (unless (or movieNumber fileOffset)
    (setq movieNumber 1))
  (when movieNumber
    (multiple-value-bind_X
      (atomOffset atomDataOffset atomType)
      (findAtom fileRefNum :atomType "moov" :atomNumber movieNumber)
      (declare (ignore atomOffset atomType))
      (setf fileOffset atomDataOffset)))
  (rlet ((theMovie (:pointer :movie))
         (dataRefWasChanged :boolean))
    (let ((couldNotResolveDataReference
           (eql #$couldNotResolveDataRef
                (checking-toolbox-error
                 (:OSErr nil :ok-errors '(#.#$couldNotResolveDataRef))
                 (#_NewMovieFromDataFork
                  theMovie
                  fileRefNum
                  fileOffset
                  flags
                  dataRefWasChanged
                  )))))
      (values_X
        (if notGC
          (%get-ptr theMovie)
          (T_makeGCOSPtr (%get-ptr theMovie) 'T_DisposeMovie))
        fileOffset
        (%get-boolean dataRefWasChanged)
        couldNotResolveDataReference))))

(defun_X T_PutMovieIntoDataFork :private (theMovie fRefNum
                                                  &key
                                                  (offset 0)
                                                  (maxSize 0))
  "See IM-QuickTime p. 2-110"
  (checking-toolbox-error (:OSErr)
                          (#_PutMovieIntoDataFork theMovie fRefNum offset maxSize))
  (values_X))

(defun_X T_StartMovie :private (theMovie)
  "See IM-QuickTime p. 2-111"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_StartMovie theMovie))
  (values_X))

(defun_X T_StopMovie :private (theMovie)
  "See IM-QuickTime p. 2-112"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_StopMovie theMovie))
  (values_X))

(defun_X T_GoToBeginningOfMovie :private (theMovie)
  "See IM-QuickTime p. 2-113"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_GoToBeginningOfMovie theMovie))
  (values_X))

(defun_X T_GoToEndOfMovie :private (theMovie)
  "See IM-QuickTime p. 2-113"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_GoToEndOfMovie theMovie))
  (values_X))

(defun_X T_SetTrackUsage :private (theTrack
                                 &key
                                 (usage 0)
                                 (movie   nil movieSet)
                                 (preview nil previewSet)
                                 (poster  nil posterSet))
  "See IM-QuickTime p. 2-115"
  (trap-flag-arg usage movie   #$trackUsageInMovie)
  (trap-flag-arg usage preview #$trackUsageInPreview)
  (trap-flag-arg usage poster  #$trackUsageInPoster)
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTrackUsage theTrack usage))
  (values_X))

(defun_X T_GetTrackUsage :private (theTrack)
  "See IM-QuickTime p. 2-116"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTrackUsage theTrack)))

(defun_X T_ShowMoviePoster :private (theMovie)
  "See IM-QuickTime p. 2-116"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_ShowMoviePoster theMovie))
  (values_X))

;;; same coding as in T_SetMovieBox
(defun_X T_SetPosterBox :private (theMovie &key boxRect left top right bottom)
  "If boxRect is passed it is used and the other args are ignored.
Otherwise, any or all of these left, top, right, bottom can be used to
modify the current movie box rect.
See IM-QuickTime p. 2-117"
  (if boxRect
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetPosterBox theMovie boxRect))
    (rlet ((theBox :Rect))
      (if (and left top right bottom)
        (progn
          (rset theBox rect.left   left  )
          (rset theBox rect.top    top   )
          (rset theBox rect.right  right )
          (rset theBox rect.bottom bottom))
        (progn
          (checking-toolbox-error (:void (#_GetMoviesError))
                                  (#_GetPosterBox theMovie theBox))
          (when left   (rset theBox rect.left   left  ))
          (when top    (rset theBox rect.top    top   ))
          (when right  (rset theBox rect.right  right ))
          (when bottom (rset theBox rect.bottom bottom))))
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_SetPosterBox theMovie theBox))))
  (values_X))

;;; same coding as in T_GetMovieBox
(defun_X T_GetPosterBoxGC :private (theMovie)
  "Returns a :Rect Record GCHandle.
See IM-QuickTime p. 2-118"
  (let ((boxRectGCHandle (newRecordGCHandle :Rect)))
    (with-dereferenced-handles ((boxRectPtr boxRectGCHandle))
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_GetPosterBox theMovie boxRectPtr)))
    boxRectGCHandle))

(defun_X T_SetMoviePosterTime :private (theMovie posterTime)
  "See IM-QuickTime p. 2-118"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMoviePosterTime theMovie posterTime))
  (values_X))

(defun_X T_GetMoviePosterTime :private (theMovie)
  "See IM-QuickTime p. 2-119"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetMoviePosterTime theMovie)))

(defun_X T_SetMoviePreviewMode :private (theMovie previewMode)
  "See IM-QuickTime p. 2-121"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMoviePreviewMode theMovie previewMode))
  (values_X))

(defun_X T_GetMoviePreviewMode :private (theMovie)
  "See IM-QuickTime p. 2-122"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetMoviePreviewMode theMovie)))

(defun_X T_SetMoviePreviewTime :private (theMovie &key startTime duration)
  "See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-122"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime duration)
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetMoviePreviewTime theMovie argStartTime argDuration)))
  (values_X))

(defun_X T_GetMoviePreviewTime :private (theMovie)
  "See IM-QuickTime p. 2-123"
  (rlet ((startTime :TimeValue)
         (duration  :TimeValue))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetMoviePreviewTime theMovie startTime duration))
    (values_X
      (%get-long startTime)
      (%get-long duration))))

(defun_X T_MoviesTask :private (&optional
                                  movie
                                  (maxMilliSecToUse 0))
  "See IM-QuickTime p. 2-124"
  (declare (type :Movie movie)
           (type integer maxMilliSecToUse))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_MoviesTask (or movie (%null-ptr)) maxMilliSecToUse))
  (values_X))

(defun_X T_IsMovieDone :private (theMovie)
  "See IM-QuickTime p. 2-125"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_IsMovieDone theMovie)))

(defun_X T_UpdateMovie :private (theMovie)
  "See IM-QuickTime p. 2-125"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:OSErr)
                          (#_UpdateMovie theMovie))
  (values_X))

(defun_X T_PtInMovie :private (theMovie point)
  "See IM-QuickTime p. 2-127"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_PtInMovie theMovie point)))

(defun_X T_PtInTrack :private (theTrack point)
  "See IM-QuickTime p. 2-128"
  (declare (type :Track theTrack))
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_PtInTrack theTrack point)))

(defun_X T_GetMovieStatus :private (theMovie)
  "Returns two values: theError firstProblemTrack.
See IM-QuickTime p. 2-128"
  (declare (type :Movie theMovie))
  (rlet ((firstProblemTrack (:pointer :Track)))
    (let ((theError
           (checking-toolbox-error (:value (#_GetMoviesError))
                                   (#_GetMovieStatus theMovie firstProblemTrack))))
      (values_X
        theError
        (%get-ptr firstProblemTrack)))))

(defun_X T_GetTrackStatus :private (theTrack)
  "See IM-QuickTime p. 2-129"
  (declare (type :Track theTrack))
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTrackStatus theTrack)))

(defun_X T_SetMoviePreferredRate :private (theMovie rate)
  "The rate arg is a normal number, which is converted
into a Fixed as required by the trap.
See IM-QuickTime p. 2-130"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMoviePreferredRate theMovie (T_numberToFixed rate)))
  (values_X))

(defun_X T_GetMoviePreferredRate :private (theMovie)
  "Returns a normal number, (converted from Fixed returned by the trap).
See IM-QuickTime p. 2-131"
  (T_fixedToNumber (checking-toolbox-error (:value (#_GetMoviesError))
                                         (#_GetMoviePreferredRate theMovie))))

(defun_X T_SetMoviePreferredVolume :private (theMovie volume)
  "The volume arg is a normal number, which is converted
into a ShortFixed as required by the trap.
See IM-QuickTime p. 2-132"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMoviePreferredVolume
                           theMovie
                           (T_numberToShortFixed volume)))
  (values_X))

(defun_X T_GetMoviePreferredVolume :private (theMovie)
  "Returns a normal number (converted from ShortFixed returned by the trap).
See IM-QuickTime p. 2-133"
  (T_shortFixedToNumber (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetMoviePreferredVolume theMovie))))

(defun_X T_PrerollMovie :private (theMovie &key startTime rate)
  "If startTime is not specified, then we assume the movie's current timeValue.
If rate is not specified, then we assume the movie's preferredRate.
The rate argument is a normal number, which is converted into a Fixed
as required by the trap.
See IM-QuickTime p. 2-135"
  (declare (type :Movie theMovie))
  (multiple-value-bind_X
    (argStartTime argRateFixed)
    (T_getMovieTimeAndRateArgs theMovie startTime rate)
    (checking-toolbox-error (:OSErr)
                            (#_PrerollMovie theMovie argStartTime argRateFixed)))
  (values_X))

(defun_X T_SetMovieActiveSegment :private (theMovie &key startTime duration)
  "See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-136"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime duration)
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetMovieActiveSegment theMovie argStartTime argDuration)))
  (values_X))

(defun_X T_GetMovieActiveSegment :private (theMovie)
  "Returns 2 values: startTime and duration
unless entire movie is active, in which case it returns false.
See IM-QuickTime p. 2-137"
  (rlet ((activeSegmentTime     :TimeValue)
         (activeSegmentDuration :TimeValue))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetMovieActiveSegment theMovie activeSegmentTime activeSegmentDuration))
    (let ((startTime (%get-long activeSegmentTime))
          (duration  (%get-long activeSegmentDuration)))
      (if (= startTime -1)
        nil
        (values_X startTime duration)))))

(defun_X T_SetMoviePLayHints nil
         (theMovie &key
                   (flags 0)
                   (scrubMode      nil scrubModeSet)
                   (useSoundInterp nil useSoundInterpSet)
                   (allowInterlace nil allowInterlaceSet)
                   (flagsMask 0)
                   (scrubModeMask      nil scrubModeMaskSet)
                   (useSoundInterpMask nil useSoundInterpMaskSet)
                   (allowInterlaceMask nil allowInterlaceMaskSet))
  "See IM-QuickTime p. 2-137"
  (trap-flag-arg flags scrubMode      #$hintsScrubMode)
  (trap-flag-arg flags useSoundInterp #$hintsUseSoundInterp)
  (trap-flag-arg flags allowInterlace #$hintsAllowInterlace)
  (trap-flag-arg flagsMask scrubModeMask      #$hintsScrubMode)
  (trap-flag-arg flagsMask useSoundInterpMask #$hintsUseSoundInterp)
  (trap-flag-arg flagsMask allowInterlaceMask #$hintsAllowInterlace)
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMoviePLayHints
                           theMovie
                           flags
                           flagsMask))
  (values_X))

(defun_X T_SetMediaPlayHints nil
         (theMedia &key
                   (flags 0)
                   (scrubMode      nil scrubModeSet)
                   (useSoundInterp nil useSoundInterpSet)
                   (allowInterlace nil allowInterlaceSet)
                   (flagsMask 0)
                   (scrubModeMask      nil scrubModeMaskSet)
                   (useSoundInterpMask nil useSoundInterpMaskSet)
                   (allowInterlaceMask nil allowInterlaceMaskSet))
  "See IM-QuickTime p. 2-139"
  (trap-flag-arg flags scrubMode      #$hintsScrubMode)
  (trap-flag-arg flags useSoundInterp #$hintsUseSoundInterp)
  (trap-flag-arg flags allowInterlace #$hintsAllowInterlace)
  (trap-flag-arg flagsMask scrubModeMask      #$hintsScrubMode)
  (trap-flag-arg flagsMask useSoundInterpMask #$hintsUseSoundInterp)
  (trap-flag-arg flagsMask allowInterlaceMask #$hintsAllowInterlace)
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMediaPlayHints
                           theMedia
                           flags
                           flagsMask))
  (values_X))

(defun_X T_LoadMovieIntoRam :private (theMovie &key
                                             startTime
                                             duration
                                             (flags 0)
                                             (keepInRam              nil keepInRamSet)
                                             (unkeepInRam            nil unkeepInRamSet)
                                             (flushFromRam           nil flushFromRamSet)
                                             (loadForwardTrackEdits  nil loadForwardTrackEditsSet)
                                             (loadBackwardTrackEdits nil loadBackwardTrackEditsSet))
  "See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
Returns t if movie loaded successfully (TC_noErr) or nil if not enough memory (TC_dsMemFullErr).
See IM-QuickTime p. 2-140"
  (declare (type :Movie theMovie))
  (trap-flag-arg flags keepInRam              #$keepInRam)
  (trap-flag-arg flags unkeepInRam            #$unkeepInRam)
  (trap-flag-arg flags flushFromRam           #$flushFromRam)
  (trap-flag-arg flags loadForwardTrackEdits  #$loadForwardTrackEdits)
  (trap-flag-arg flags loadBackwardTrackEdits #$loadBackwardTrackEdits)
  (eql #$noErr (multiple-value-bind_X (argStartTime argDuration)
                                       (T_getMovieTimeAndDurationArgs theMovie startTime duration)
                                       (checking-toolbox-error
                                        (:OSErr nil :ok-errors '(#.#$dsMemFullErr))
                                        (#_LoadMovieIntoRam theMovie argStartTime argDuration flags)))))

(defun_X T_LoadTrackIntoRam :private (theTrack &key
                                             startTime
                                             duration
                                             (flags 0)
                                             (keepInRam              nil keepInRamSet)
                                             (unkeepInRam            nil unkeepInRamSet)
                                             (flushFromRam           nil flushFromRamSet)
                                             (loadForwardTrackEdits  nil loadForwardTrackEditsSet)
                                             (loadBackwardTrackEdits nil loadBackwardTrackEditsSet))
  "See T_getTrackTimeAndDurationArgs for info on the startTime and duration arguments.
Returns t if Track loaded successfully (#$noErr) or nil if not enough memory (#$dsMemFullErr).
See IM-QuickTime p. 2-142"
  (declare (type :Track theTrack))
  (trap-flag-arg flags keepInRam              #$keepInRam)
  (trap-flag-arg flags unkeepInRam            #$unkeepInRam)
  (trap-flag-arg flags flushFromRam           #$flushFromRam)
  (trap-flag-arg flags loadForwardTrackEdits  #$loadForwardTrackEdits)
  (trap-flag-arg flags loadBackwardTrackEdits #$loadBackwardTrackEdits)
  (eql #$noErr (multiple-value-bind_X (argStartTime argDuration)
                                       (T_getTrackTimeAndDurationArgs theTrack startTime duration)
                                       (checking-toolbox-error
                                        (:OSErr nil :ok-errors '(#.#$dsMemFullErr))
                                        (#_LoadTrackIntoRam theTrack argStartTime argDuration flags)))))

(defun_X T_LoadMediaIntoRam :private (theMedia &key
                                             startTime
                                             duration
                                             (flags 0)
                                             (keepInRam              nil keepInRamSet)
                                             (unkeepInRam            nil unkeepInRamSet)
                                             (flushFromRam           nil flushFromRamSet)
                                             (loadForwardTrackEdits  nil loadForwardTrackEditsSet)
                                             (loadBackwardTrackEdits nil loadBackwardTrackEditsSet))
  "See T_getMediaTimeAndDurationArgs for info on the startTime and duration arguments.
Returns t if Media loaded successfully (TC_noErr) or nil if not enough memory (TC_dsMemFullErr).
See IM-QuickTime p. 2-143"
  (declare (type :Media theMedia))
  (trap-flag-arg flags keepInRam              #$keepInRam)
  (trap-flag-arg flags unkeepInRam            #$unkeepInRam)
  (trap-flag-arg flags flushFromRam           #$flushFromRam)
  (trap-flag-arg flags loadForwardTrackEdits  #$loadForwardTrackEdits)
  (trap-flag-arg flags loadBackwardTrackEdits #$loadBackwardTrackEdits)
  (eql #$noErr (multiple-value-bind_X (argStartTime argDuration)
                                       (T_getMediaTimeAndDurationArgs theMedia startTime duration)
                                       (checking-toolbox-error
                                        (:OSErr nil :ok-errors '(#.#$dsMemFullErr))
                                        (#_LoadMediaIntoRam theMedia argStartTime argDuration flags)))))

(defun_X T_SetMovieActive :private (theMovie active)
  "See IM-QuickTime p. 2-145"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieActive theMovie active))
  (values_X))

(defun_X T_GetMovieActive :private (theMovie)
  "See IM-QuickTime p. 2-146"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetMovieActive theMovie)))

(defun_X T_SetTrackEnabled :private (theTrack isEnabled)
  "See IM-QuickTime p. 2-147"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTrackEnabled theTrack isEnabled))
  (values_X))

(defun_X T_GetTrackEnabled :private (theTrack)
  "See IM-QuickTime p. 2-147"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTrackEnabled theTrack)))

(defun_X T_GetMoviePict :private (movie &key timeValue)
  "Returns a :PicHandle.
See IM-QuickTime p. 2-148"
  (declare (type :Movie movie))
  (checking-toolbox-error (:pointer (#_GetMoviesError))
                          (#_GetMoviePict movie (or timeValue
                                                    (T_GetMovieTimeValue movie)))))

(defun_X T_GetMoviePictGC :private (movie &key timeValue)
  "Returns a :PicHandle GCOSPtr.
See IM-QuickTime p. 2-148"
  (declare (type :Movie movie))
  (makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_GetMoviePict movie (or timeValue
                                                                   (T_GetMovieTimeValue movie))))
                 #_KillPicture))

(defun_X T_GetMoviePosterPictGC :private (movie)
  "Returns a :PicHandle GCOSPtr.
See IM-QuickTime p. 2-149"
  (declare (type :Movie movie))
  (makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_GetMoviePosterPict movie))
                 #_KillPicture))

(defun_X T_NewMovieTrack :private (theMovie &key (width 0) (height 0) (trackVolume 0))
  "Returns a :Track OSPtr.
The width and height arguments are normal numbers, 
which are converted into Fixed as required by the trap.
Disposal is the responsibility of theMovie.
See IM-QuickTime p. 2-151"
  (declare (type :Movie  theMovie)
           (type number width)
           (type number height)
           (type number trackVolume))
  (checking-toolbox-error
   (:pointer (#_GetMoviesError))
   (#_NewMovieTrack theMovie width height trackVolume)))

(defun_X T_DisposeMovieTrack :private (theTrack)
  "See IM-QuickTime p. 2-152"
  (declare (type :Track  theTrack))
  (checking-toolbox-error
   (:void (#_GetMoviesError))
   (#_DisposeMovieTrack theTrack))
  ;; MUST return t or it will be called twice by the reaper!
  t)

(defun_X T_NewTrackMedia :private (theTrack mediaType timeScale
                                               &optional
                                               dataReference
                                               dataReferenceType)
  "Returns a :Media OSPtr.
Disposal is the responsibility of theTrack.
See IM-QuickTime p. 2-153"
  (declare (type :Track  theTrack)
           (type integer mediaType) ; :OSType
           (type :TimeScale timeScale)
           (type :Handle dataReference)
           (type integer dataReferenceType)) ; :OSType
  (checking-toolbox-error
   (:pointer (#_GetMoviesError))
   (#_NewTrackMedia theTrack mediaType timeScale dataReference dataReferenceType)))

(defun_X T_SetMovieCoverProcs :private (theMovie refcon &key (uncoverProc CB_QTUncoverProc) (coverProc CB_QTCoverProc))
  "See IM-QuickTime p. 2-157" 
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieCoverProcs theMovie uncoverProc coverProc refcon))
  (values_X))

(defun_X T_SetMovieGWorld :private (theMovie thePort gdh)
  "See IM-QuickTime p. 2-159" 
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieGWorld theMovie thePort gdh))
  (values_X))

(defun_X T_GetMovieGWorld :private (theMovie)
  "Returns 2 values: ColorGraphicsPortPtr and GraphicsDeviceHandle.
See IM-QuickTime p. 2-160"
  (rlet ((ColorGraphicsPortPtr (:pointer :CGrafPort))
         (GraphicsDeviceHandle (:handle :Gdevice)))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetMovieGWorld theMovie ColorGraphicsPortPtr GraphicsDeviceHandle))
    (values_X
      (%get-ptr ColorGraphicsPortPtr)
      (%get-ptr GraphicsDeviceHandle))))

;;; same coding as in T_SetPosterBox
(defun_X T_SetMovieBox :private (theMovie &key boxRect left top right bottom)
  "If boxRect is passed it is used and the other args are ignored.
Otherwise, any or all of these left, top, right, bottom can be used to
modify the current movie box rect.
See IM-QuickTime p. 2-161"
  (if boxRect
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetMovieBox theMovie boxRect))
    (rlet ((theBox :Rect))
      (if (and left top right bottom)
        (progn
          (rset theBox rect.left   left  )
          (rset theBox rect.top    top   )
          (rset theBox rect.right  right )
          (rset theBox rect.bottom bottom))
        (progn
          (checking-toolbox-error (:void (#_GetMoviesError))
                                  (#_GetMovieBox theMovie theBox))
          (when left   (rset theBox rect.left   left  ))
          (when top    (rset theBox rect.top    top   ))
          (when right  (rset theBox rect.right  right ))
          (when bottom (rset theBox rect.bottom bottom))))
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_SetMovieBox theMovie theBox))))
  (values_X))

;;; same coding as in T_GetPosterBox
(defun_X T_GetMovieBoxGC :private (theMovie)
  "Returns a :Rect Record GCHandle.
See IM-QuickTime p. 2-162"
  (let ((boxRectGCHandle (newRecordGCHandle :Rect)))
    (with-dereferenced-handles ((boxRectPtr boxRectGCHandle))
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_GetMovieBox theMovie boxRectPtr)))
    boxRectGCHandle))

(defun_X T_GetMovieBox :private (theMovie boxRectPtr)
  "Returns the :Rect Record Ptr given as argument.
See IM-QuickTime p. 2-162"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_GetMovieBox theMovie boxRectPtr))
  boxRectPtr)

(defun_X T_SetMovieDisplayClipRgn :private (theMovie theClip)
  "See IM-QuickTime p. 2-165"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieDisplayClipRgn theMovie theClip))
  (values_X))

(defun_X T_GetMovieDisplayClipRgnGC :private (theMovie)
  "Returns a :Region Record GCHandle.
See IM-QuickTime p. 2-166"
  (multiple-value-bind_X
    (theClipRgn theError)
    (checking-toolbox-error (:pointer (#_GetMoviesError) :ok-errors '(#.#$noErr))
                                            (#_GetMovieDisplayClipRgn theMovie))
    (declare (ignore theError))
    (if theClipRgn
      (makeGCOSPTR theClipRgn #_DisposeRgn)
      nil)))

(defun_X T_SetTrackLayer :private (theTrack layer)
  "See IM-QuickTime p. 2-168"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTrackLayer theTrack layer))
  (values_X))

(defun_X T_GetTrackLayer :private (theTrack)
  "See IM-QuickTime p. 2-169"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTrackLayer theTrack)))


;;; Movie Matrix stuff

(defun_X T_SetMovieMatrix :private (theMovie &optional matrixRecordPtr)
  "See IM-QuickTime p. 2-170"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieMatrix theMovie (or matrixRecordPtr (%null-ptr))))
  (values_X))

(defun_X T_SetMovieMatrixFromArray :private (theMovie &optional matrixArray)
  "The matrixArray is a 3x3 array, not a low-level array.
The numbers in the array are normal numbers, which are converted into Fixed
as required by the trap.
See IM-QuickTime p. 2-170"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieMatrix theMovie
                           (if matrixArray
                             (T_matrixArrayToMatrixRecord matrixArray)
                             (%null-ptr))))
  (values_X))

(defun_X T_GetMovieMatrixGC :private (theMovie &key (matrixRecordGCPtr t))
  "The matrixRecordGCPtr keyword argument can be used to specify a Ptr or GCPtr
which will be filled in and returned as the return value.  When its value
is t (the default), a new GCPtr will be allocated for you and filled in.
See IM-QuickTime p. 2-170"
  (when (eql t matrixRecordGCPtr)
    (setf matrixRecordGCPtr (newRecordGCPtr :matrixRecord)))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_GetMovieMatrix theMovie matrixRecordGCPtr))
  matrixRecordGCPtr)

(defun_X T_GetMovieMatrixArray :private (theMovie)
  "Returns the matrix as a 3x3 array, not a low-level array.
The numbers in the array are normal numbers, which are converted from Fixed
integers returned in the low-level array returned by the trap.
See IM-QuickTime p. 2-170"
  (rlet ((matrixRecordPtr :matrixRecord))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetMovieMatrix theMovie matrixRecordPtr))
    (T_matrixRecordToMatrixArray matrixRecordPtr)))

(defun_X T_GetMovieBoundsRgnGC :private (theMovie)
  "Returns a :Region Record GCHandle.
See IM-QuickTime p. 2-171"
  (makeGCOSPTR
   (checking-toolbox-error (:pointer (#_GetMoviesError))
                           (#_GetMovieBoundsRgn theMovie))
   #_DisposeRgn))

(defun_X T_GetTrackMovieBoundsRgnGC :private (theTrack)
  "Returns a :Region Record GCHandle.
See IM-QuickTime p. 2-172"
  (makeGCOSPTR
   (checking-toolbox-error (:pointer (#_GetMoviesError))
                           (#_GetTrackMovieBoundsRgn theTrack))
   #_DisposeRgn))

(defun_X T_SetMovieClipRgn :private (theMovie theClip)
  "See IM-QuickTime p. 2-172"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieClipRgn theMovie theClip))
  (values_X))

(defun_X T_GetMovieClipRgnGC :private (theMovie)
  "Returns a :Region Record GCHandle.
See IM-QuickTime p. 2-173"
  (multiple-value-bind_X
    (theClipRgn theError)
    (checking-toolbox-error (:pointer (#_GetMoviesError) :ok-errors '(#.#$noErr))
                                            (#_GetMovieClipRgn theMovie))
    (declare (ignore theError))
    (if theClipRgn
      (makeGCOSPTR theClipRgn #_DisposeRgn)
      nil)))

(defun_X T_SetTrackMatrixFromArray :private (theTrack &optional matrixArray)
  "The matrixArray is a 3x3 array, not a low-level array.
The numbers in the array are normal numbers, which are converted into Fixed
as required by the trap.
See IM-QuickTime p. 2-174"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTrackMatrix theTrack
                           (if matrixArray
                             (T_matrixArrayToMatrixRecord matrixArray)
                             (%null-ptr)))))

(defun_X T_GetTrackMatrixArray :private (theTrack)
  "Returns the matrix as a 3x3 array, not a low-level array.
The numbers in the array are normal numbers, which are converted from Fixed
integers returned in the low-level array returned by the trap.
See IM-QuickTime p. 2-175"
  (rlet ((matrixRecordPtr :matrixRecord))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetTrackMatrix theTrack matrixRecordPtr))
    (T_matrixRecordToMatrixArray matrixRecordPtr)))

(defun_X T_SetMovieVolume :private (theMovie volume)
  "The volume arg is a normal number, which is converted
into a ShortFixed as required by the trap.
The volume arg is clamped to a range from -1 through 1.
See IM-QuickTime p. 2-182"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieVolume
                           theMovie
                           (T_numberToShortFixed volume)))
  (values_X))

(defun_X T_GetMovieVolume :private (theMovie)
  "Returns a normal number (converted from ShortFixed returned by the trap).
See IM-QuickTime p. 2-182"
  (T_shortFixedToNumber (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetMovieVolume theMovie))))

(defun_X T_SetTrackVolume :private (theTrack volume)
  "The volume arg is a normal number, which is converted
into a ShortFixed as required by the trap.
The volume arg is clamped to a range from -1 through 1.
See IM-QuickTime p. 2-183"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTrackVolume
                           theTrack
                           (T_numberToShortFixed volume)))
  (values_X))

(defun_X T_GetTrackVolume :private (theTrack)
  "Returns a normal number (converted from ShortFixed returned by the trap).
See IM-QuickTime p. 2-184"
  (T_shortFixedToNumber (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetTrackVolume theTrack))))

(defun_X T_GetMovieDuration :private (theMovie)
  "See IM-QuickTime p. 2-185"
  (checking-toolbox-error (:Value (#_GetMoviesError))
                          (#_GetMovieDuration theMovie)))

(defmacro_X rrefTimeValue64 (timeRec)
  `(+ (ash (rref ,timeRec :TimeRecord.value.hi :storage :pointer) 32)
      (rref ,timeRec :TimeRecord.value.lo :storage :pointer)))

(defmacro_X rsetTimeValue64 (timeRec time)
  `(progn
     (rset ,timeRec :TimeRecord.value.hi (ash ,time -32) :storage :pointer)
     (rset ,timeRec :TimeRecord.value.lo      ,time      :storage :pointer)))

(defun_X T_SetMovieTimeValue :private (theMovie newTime)
  "Returns newTime.  Becuase it actually uses
#_SetMovieTime, newTime can be a 64-bit magnitude number.
See IM-QuickTime p. 2-185"
  (rlet ((timeRec :TimeRecord))
    (checking-toolbox-error (:Value (#_GetMoviesError))
                            (#_GetMovieTime theMovie timeRec))
    (rsetTimeValue64 timeRec newTime)
    ;; if there were going to be an error, we would have caught it already
    (#_SetMovieTime theMovie timeRec))
  newTime)

(defun_X T_SetMovieTime :private (theMovie newTime)
  "The newTime arg is a pointer not a handle.
See IM-QuickTime p. 2-186"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieTime theMovie newTime))
  (values_X))

(defun_X T_GetMovieTimeValue :private (theMovie)
  "Returns a 64-bit TimeValue (using the TimeRecord argument to the trap).
See also T_GetMovieTimeRecord.
See IM-QuickTime p. 2-187"
  (rlet ((timeRec :TimeRecord))
    (checking-toolbox-error (:Value (#_GetMoviesError))
                            (#_GetMovieTime theMovie timeRec))
    (rrefTimeValue64 timeRec)))

(defun_X T_GetMovieTimeRecordGC :private (theMovie &key timeRecordHandle)
  "Returns a TimeRecord GCHandle.
Returns either an allocated TimeRecord GCHandle or the supplied timeRecordHandle arg if non-nil.
If you supply a TimeRecordHandle arg, it doesn't matter if it's a Handle or a GCHandle.
See IM-QuickTime p. 2-187"
  (unless timeRecordHandle
    (setq timeRecordHandle (newRecordGCHandle :TimeRecord)))
  (with-dereferenced-handles ((timeRecPtr timeRecordHandle))
    (checking-toolbox-error (:Value (#_GetMoviesError))
                            (#_GetMovieTime theMovie timeRecPtr)))
  timeRecordHandle)

(defun_X T_SetMovieRate :private (theMovie rate)
  "The rate arg is a normal number, which is converted
into a Fixed as required by the trap.
See IM-QuickTime p. 2-187"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieRate theMovie (T_numberToFixed rate)))
  (values_X))

(defun_X T_GetMovieRate :private (theMovie)
  "Returns a normal number, (converted from Fixed returned by the trap).
See IM-QuickTime p. 2-188"
  (T_fixedToNumber (checking-toolbox-error (:value (#_GetMoviesError))
                                         (#_GetMovieRate theMovie))))

(defun_X T_SetMovieTimeScale :private (theMovie timeScale)
  "See IM-QuickTime p. 2-189"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieTimeScale theMovie timeScale))
  (values_X))

(defun_X T_GetMovieTimeScale :private (theMovie)
  "See IM-QuickTime p. 2-190"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetMovieTimeScale theMovie)))

(defun_X T_GetMovieTimeBase :private (theMovie)
  "See IM-QuickTime p. 2-190"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetMovieTimeBase theMovie)))

(defun_X T_GetTrackDuration :private (theTrack)
  "See IM-QuickTime p. 2-191"
  (checking-toolbox-error (:Value (#_GetMoviesError))
                          (#_GetTrackDuration theTrack)))

(defun_X T_SetTrackOffset :private (theTrack offset)
  "See IM-QuickTime p. 2-192"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTrackOffset theTrack offset))
  (values_X))

(defun_X T_GetTrackOffset :private (theTrack)
  "See IM-QuickTime p. 2-193"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTrackOffset theTrack)))

(defun_X T_TrackTimeToMediaTime :private (value theTrack)
  "See IM-QuickTime p. 2-193"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_TrackTimeToMediaTime value theTrack)))

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant maxInterestingMediaTypes 5)) ; 2 is enough as of QuickTime 2.0

(defrecord interestingMediaTypes (types (:array :OSType maxInterestingMediaTypes)))


(defun_X T_GetMovieNextInterestingTime nil
         (theMovie
          &key
          startTime
          rate
          (flags 0)
          (mediaSample         nil mediaSampleSet)
          (mediaEdit           nil mediaEditSet)
          (trackEdit           nil trackEditSet)
          (syncSample          nil syncSampleSet)
          (edgeOK              nil edgeOKSet)
          (ignoreActiveSegment nil ignoreActiveSegmentSet)
          (mediaTypes '("eyes"))
          interestingDurationWanted)
  "See T_getMovieTimeAndRateArgs for info on the startTime and rate arguments.
If interestingDurationWanted is t, then returns 2 values:
 interestingTime and interestingDuration.
If interestingDurationWanted is nil, then returns only interestingTime.
See IM-QuickTime p. 2-197"
  (declare (type :Movie theMovie))
  (trap-flag-arg flags mediaSample         #$nextTimeMediaSample)
  (trap-flag-arg flags mediaEdit           #$nextTimeMediaEdit)
  (trap-flag-arg flags trackEdit           #$nextTimeTrackEdit)
  (trap-flag-arg flags syncSample          #$nextTimeSyncSample)
  (trap-flag-arg flags edgeOK              #$nextTimeEdgeOK)
  (trap-flag-arg flags ignoreActiveSegment #$nextTimeIgnoreActiveSegment)
  (when (< 1 (logcount (logand flags (logior #$nextTimeMediaSample
                                             #$nextTimeMediaEdit
                                             #$nextTimeTrackEdit
                                             #$nextTimeSyncSample))))
    (error "too many flags selected"))
  (when (zerop flags)
    (setf flags #$nextTimeMediaSample))
  (when (> (length mediaTypes) maxInterestingMediaTypes)
    (error "too many mediaTypes"))
  (multiple-value-bind_X
    (argStartTime argRateFixed)
    (T_getMovieTimeAndRateArgs theMovie startTime rate)
    (rlet ((whichMediaTypes :interestingMediaTypes)
           (numMediaTypes :signed-integer (length mediaTypes))
           (interestingTime :TimeValue)
           (interestingDuration :TimeValue))
      (loop for type in mediaTypes
            for ind from 0
            do (rarset whichMediaTypes type interestingMediaTypes.types ind))
      (checking-toolbox-error (:value (#_GetMoviesError))
                              (#_GetMovieNextInterestingTime
                               theMovie
                               flags
                               numMediaTypes
                               whichMediaTypes
                               argStartTime
                               argRateFixed
                               interestingTime
                               (if interestingDurationWanted interestingDuration (%null-ptr))))
      (if interestingDurationWanted
        (values_X
          (%get-long interestingTime)
          (%get-long interestingDuration))
        (%get-long interestingTime)))))

(defun_X T_GetTrackNextInterestingTime nil
         (theTrack
          &key
          startTime
          rate
          (flags 0)
          (mediaSample         nil mediaSampleSet)
          (mediaEdit           nil mediaEditSet)
          (trackEdit           nil trackEditSet)
          (syncSample          nil syncSampleSet)
          (edgeOK              nil edgeOKSet)
          (ignoreActiveSegment nil ignoreActiveSegmentSet)
          interestingDurationWanted)
  "See T_getTrackTimeAndRateArgs for info on the startTime and rate arguments.
If interestingDurationWanted is t, then returns 2 values:
 interestingTime and interestingDuration.
If interestingDurationWanted is nil, then returns only interestingTime.
See IM-QuickTime p. 2-199"
  (declare (type :Track theTrack))
  (trap-flag-arg flags mediaSample         #$nextTimeMediaSample)
  (trap-flag-arg flags mediaEdit           #$nextTimeMediaEdit)
  (trap-flag-arg flags trackEdit           #$nextTimeTrackEdit)
  (trap-flag-arg flags syncSample          #$nextTimeSyncSample)
  (trap-flag-arg flags edgeOK              #$nextTimeEdgeOK)
  (trap-flag-arg flags ignoreActiveSegment #$nextTimeIgnoreActiveSegment)
  (when (< 1 (logcount (logand flags (logior #$nextTimeMediaSample
                                             #$nextTimeMediaEdit
                                             #$nextTimeTrackEdit
                                             #$nextTimeSyncSample))))
    (error "too many flags selected"))
  (when (zerop flags)
    (setf flags #$nextTimeMediaSample))
  (multiple-value-bind_X
    (argStartTime argRateFixed)
    (T_getTrackTimeAndRateArgs theTrack startTime rate)
    (rlet ((interestingTime :TimeValue)
           (interestingDuration :TimeValue))
      (checking-toolbox-error (:value (#_GetMoviesError))
                              (#_GetTrackNextInterestingTime
                               theTrack
                               flags
                               argStartTime
                               argRateFixed
                               interestingTime
                               (if interestingDurationWanted interestingDuration (%null-ptr))))
      (if interestingDurationWanted
        (values_X
          (%get-long interestingTime)
          (%get-long interestingDuration))
        (%get-long interestingTime)))))

(defun_X T_GetMovieTrackCount :private (theMovie)
  "See IM-QuickTime p. 2-203"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetMovieTrackCount theMovie)))

(defun_X T_GetMovieIndTrack :private (theMovie index)
  "Returns a :Track handle.
See IM-QuickTime p. 2-203"
  (declare (type :Movie theMovie)
           (integer index))
  (checking-toolbox-error (:pointer (#_GetMoviesError))
                          (#_GetMovieIndTrack theMovie index)))

(defun_X T_GetMovieTrack :private (theMovie trackID &key noErrorIfNone)
  "Returns a :Track handle.
See IM-QuickTime p. 2-204"
  (declare (type :Movie theMovie)
           (integer trackID))
  (if noErrorIfNone
    (multiple-value-bind_X (theTrack theError)
                       (checking-toolbox-error (:pointer (#_GetMoviesError) :ok-errors '(#.#$noErr))
                                               (#_GetMovieTrack theMovie trackID))
      (declare (ignore theError))
      theTrack)
    (checking-toolbox-error (:pointer (#_GetMoviesError))
                                            (#_GetMovieTrack theMovie trackID))))

(defun_X T_GetTrackID :private (theTrack)
  "See IM-QuickTime p. 2-205"
  (declare (type :Track theTrack))
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTrackID theTrack)))

(defun_X T_GetTrackMedia :private (theTrack &key noErrorIfNone)
  "Returns a :Media handle.
If there is no Media for the Track an error is raised unless the
noErrorIfNone keyword argument is non-nil, in which case nil is returned.
See IM-QuickTime p. 2-206"
  (declare (type :Track theTrack))
  (if noErrorIfNone
    (multiple-value-bind_X (theMedia theError)
                       (checking-toolbox-error (:pointer (#_GetMoviesError) :ok-errors '(#.#$noErr))
                                               (#_GetTrackMedia theTrack))
      (declare (ignore theError))
      theMedia)
    (checking-toolbox-error (:pointer (#_GetMoviesError))
                                            (#_GetTrackMedia theTrack))))

(defun_X T_SetMovieLanguage :private (theMovie &optional (language #$smSystemScript))
  "See IM-QuickTime p. 2-208"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieLanguage theMovie language))
  (values_X))

(defun_X T_SelectMovieAlternates :private (theMovie)
  "See IM-QuickTime p. 2-209"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SelectMovieAlternates theMovie))
  (values_X))

(defun_X T_SetAutoTrackAlternatesEnabled :private (theMovie enable)
  "See IM-QuickTime p. 2-210"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetAutoTrackAlternatesEnabled theMovie enable)))

(defun_X T_SetTrackAlternate :private (theTrack alternate)
  "See IM-QuickTime p. 2-210"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTrackAlternate theTrack alternate)))

(defun_X T_GetTrackAlternate :private (theTrack)
  "See IM-QuickTime p. 2-211"
  (checking-toolbox-error (:pointer (#_GetMoviesError)) ; (%null-ptr) is supposed to be an impossible return value
                          (#_GetTrackAlternate theTrack)))

(defun repairUnsignedLong (arg)
  (if (< arg 0)
    (+ arg (ash 1 32))
    arg))

(defun_X T_GetMovieCreationTime :private (theMovie)
  "See IM-QuickTime p. 2-220"
  (declare (type :Movie theMovie))
  (repairUnsignedLong (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetMovieCreationTime theMovie))))

(defun_X T_GetMovieModificationTime :private (theMovie)
  "See IM-QuickTime p. 2-220"
  (declare (type :Movie theMovie))
  (repairUnsignedLong (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetMovieModificationTime theMovie))))

(defun_X T_GetTrackCreationTime :private (theTrack)
  "See IM-QuickTime p. 2-220"
  (declare (type :Track theTrack))
  (repairUnsignedLong (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetTrackCreationTime theTrack))))

(defun_X T_GetTrackModificationTime :private (theTrack)
  "See IM-QuickTime p. 2-221"
  (declare (type :Track theTrack))
  (repairUnsignedLong (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetTrackModificationTime theTrack))))

(defun_X T_GetMediaCreationTime :private (theMedia)
  "See IM-QuickTime p. 2-221"
  (declare (type :Media theMedia))
  (repairUnsignedLong (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetMediaCreationTime theMedia))))

(defun_X T_GetMediaModificationTime :private (theMedia)
  "See IM-QuickTime p. 2-222"
  (declare (type :Media theMedia))
  (repairUnsignedLong (checking-toolbox-error (:value (#_GetMoviesError))
                                              (#_GetMediaModificationTime theMedia))))

(defun_X T_GetMovieDataSize :private (theMovie &key startTime duration)
  "See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-223"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime duration)
    (checking-toolbox-error (:Value (#_GetMoviesError))
                            (#_GetMovieDataSize theMovie argStartTime argDuration))))

(defun_X T_GetTrackDataSize :private (theTrack &key startTime duration)
  "See IM-QuickTime p. 2-223"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getTrackTimeAndDurationArgs theTrack startTime duration)
    (checking-toolbox-error (:Value (#_GetMoviesError))
                            (#_GetTrackDataSize theTrack argStartTime argDuration))))

(defun_X T_GetMediaDataSize :private (theMedia &key startTime duration)
  "See IM-QuickTime p. 2-223"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMediaTimeAndDurationArgs theMedia startTime duration)
    (checking-toolbox-error (:Value (#_GetMoviesError))
                            (#_GetMediaDataSize theMedia argStartTime argDuration))))

;;;;;;;;;;;;
;;; User Data
;;;;;;;;;;;;

(defun_X T_GetMovieUserData :private (theMovie)
  "Returns a userData Record handle.
It will be disposed when the movie it came from is disposed.
See IM-QuickTime p. 2-231"
  (declare (type :Movie theMovie))
  (checking-toolbox-error (:pointer (#_GetMoviesError))
                          (#_GetMovieUserData theMovie)))

(defun_X T_GetTrackUserData :private (theTrack)
  "Returns a userData Record handle.
It will be disposed when the track it came from is disposed.
See IM-QuickTime p. 2-232"
  (declare (type :Track theTrack))
  (checking-toolbox-error (:pointer (#_GetMoviesError))
                          (#_GetTrackUserData theTrack)))

(defun_X T_GetMediaUserData :private (theMedia)
  "Returns a userData Record handle.
It will be disposed when the media it came from is disposed.
See IM-QuickTime p. 2-232"
  (declare (type :Media theMedia))
  (checking-toolbox-error (:pointer (#_GetMoviesError))
                          (#_GetMediaUserData theMedia)))

(defun_X T_GetNextUserDataType :private (userData &optional (type 0))
  "Returns an :OSType.
See IM-QuickTime p. 2-233"
  (checking-toolbox-error (:Value (#_GetMoviesError))
                          (#_GetNextUserDataType userData type)))

(defun_X T_CountUserDataType :private (userData type)
  "Returns an integer.
See IM-QuickTime p. 2-234"
  (checking-toolbox-error (:Value (#_GetMoviesError))
                          (#_CountUserDataType userdata type)))

#| These need to be reworked for GCHandles, and some are patently wrong anyway

(defun_X T_AddUserData :private (userData data type)
  "See IM-QuickTime p. 2-235"
  (checking-toolbox-error (:OSErr)
                          (#_AddUserData userdata data type))
  (values_X))

(defun_X T_GetUserData :private (userData data type index)
  "Returns the data argument.
See IM-QuickTime p. 2-235"
  (checking-toolbox-error (:OSErr)
                          (#_GetUserData userData data type index))
  data)

(defun_X T_RemoveUserData :private (userData type index)
  "See IM-QuickTime p. 2-236"
  (checking-toolbox-error (:OSErr)
                          (#_RemoveUserData userData type index))
  (values_X))

(defun_X T_AddUserDataText :private (userData data type index itlRegionTag)
  "See IM-QuickTime p. 2-236"
  (checking-toolbox-error (:OSErr)
                          (#_AddUserDataText userData data type index itlRegionTag))
  (values_X))

(defun_X T_GetUserDataText :private (userData data type index itlRegionTag)
  "Returns the data argument.
See IM-QuickTime p. 2-235"
  (checking-toolbox-error (:OSErr)
                          (#_GetUserDataText userData data type index itlRegionTag))
  data)

(defun_X T_RemoveUserDataText :private (userData udType index itlRegionTag)
  "See IM-QuickTime p. 2-238"
  (checking-toolbox-error (:OSErr)
                          (#_RemoveUserDataText userData udType index itlRegionTag))
  (values_X))

(defun_X T_SetUserDataItem :private (userData data size type index)
  "See IM-QuickTime p. 2-239"
  (checking-toolbox-error (:OSErr)
                          (#_SetUserDataItem userData data size type index))
  (values_X))

(defun_X T_GetUserDataItem :private (userData data size type index)
  "Returns the data argument.
See IM-QuickTime p. 2-240"
  (checking-toolbox-error (:OSErr)
                          (#_GetUserDataItem userData data size type index))
  data)

(defun_X T_NewUserDataFromHandle :private (data)
  "Returns XXXXXXX.
See IM-QuickTime p. 2-241"
  (rlet ((theUserData :handle))
    (checking-toolbox-error (:OSErr)
                            (#_NewUserDataFromHandle data theUserData))
    (%get-ptr theUserData)))

|#

(defun_X T_PutMovieOnScrap :private (theMovie &key (flags 0)
                                               (dontZeroScrap nil dontZeroScrapSet)
                                               (onlyPutMovie  nil onlyPutMovieSet))
  "See IM-QuickTime p. 2-244"
  (trap-flag-arg flags dontZeroScrap #$movieScrapDontZeroScrap)
  (trap-flag-arg flags onlyPutMovie #$movieScrapOnlyPutMovie)
  (checking-toolbox-error (:OSErr)
                          (#_PutMovieOnScrap theMovie flags))
  (values_X))

(defun_X T_NewMovieFromScrapGC nil
         (&key
          (flags 0)
          ((active active_)               nil active_Set)
          (dontResolveDataReferences      nil dontResolveDataReferencesSet)
          (dontAskUnresovedDataReferences nil dontAskUnresovedDataReferencesSet)
          (dontAutoAlternates             nil dontAutoAlternatesSet))
  "Returns 2 values: movieGCOSPtr couldNotResolveDataReference.
See IM-QuickTime p. 2-245"
  (declare (type integer flags))
  (trap-flag-arg flags active_                        #$newMovieActive)
  (trap-flag-arg flags dontResolveDataReferences      #$newMovieDontResolveDataRefs)
  (trap-flag-arg flags dontAskUnresovedDataReferences #$newMovieDontAskUnresolvedDataRefs)
  (trap-flag-arg flags dontAutoAlternates             #$newMovieDontAutoAlternates)
  (multiple-value-bind_X (movieGCOSPtr theError)
                     (T_makeGCOSPtr
                      (checking-toolbox-error
                       (:pointer (#_GetMoviesError) :ok-errors '(#.#$couldNotResolveDataRef))
                       (#_NewMovieFromScrap flags))
                      'T_DisposeMovie)
    (values_X
      movieGCOSPtr
      (eql theError #$couldNotResolveDataRef))))

(defun_X T_SetMovieSelection :private (theMovie &key startTime duration)
  "See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-246"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime duration)
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetMovieSelection theMovie argStartTime argDuration)))
  (values_X))

(defun_X T_GetMovieSelection :private (theMovie)
  "See IM-QuickTime p. 2-247"
  (rlet ((selectionTime     :TimeValue)
         (selectionDuration :TimeValue))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetMovieSelection theMovie selectionTime selectionDuration))
    (values_X
      (%get-long selectionTime)
      (%get-long selectionDuration))))

(defun_X T_CutMovieSelectionGC :private (theMovie)
  "Returns movieGCOSPtr.
See IM-QuickTime p. 2-247"
  (T_makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_CutMovieSelection theMovie))
                 'T_DisposeMovie))

(defun_X T_CopyMovieSelectionGC :private (theMovie)
  "Returns movieGCOSPtr.
See IM-QuickTime p. 2-248"
  (T_makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_CopyMovieSelection theMovie))
                 'T_DisposeMovie))

(defun_X T_PasteMovieSelection :private (toMovie fromMovie)
  "See IM-QuickTime p. 2-249"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_PasteMovieSelection toMovie fromMovie))
  (values_X))

(defun_X T_AddMovieSelection :private (toMovie fromMovie)
  "See IM-QuickTime p. 2-250"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_AddMovieSelection toMovie fromMovie))
  (values_X))

(defun_X T_ClearMovieSelection :private (theMovie)
  "See IM-QuickTime p. 2-251"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_ClearMovieSelection theMovie))
  (values_X))

(defun_X T_IsScrapMovie :private (theTrack)
  "See IM-QuickTime p. 2-252"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_IsScrapMovie theTrack)))

(defun_X T_PasteHandleIntoMovie :private (theMovie
                                         &key
                                         handle ; default is from scrap
                                         (handleType 0) ; first available
                                         (flags 0)
                                         (pasteInParallel nil pasteInParallelSet)
                                         (userComponentOrInstance $autoSelectImportComponentInstance))
  "See IM-QuickTime p. 2-252"
  (trap-flag-arg flags pasteInParallel #$pasteInParallel)
  (checking-toolbox-error (:OSErr)
                          (#_PasteHandleIntoMovie
                           (or handle (%null-ptr))
                           handleType
                           theMovie
                           flags
                           (%coerce-to-macptr userComponentOrInstance)))
  (values_X))

(defun_X T_PutMovieIntoTypedHandle :private (theMovie
                                             &key
                                             publicMovie
                                             selectedTrack ; default is all tracks
                                             (handleType "MooV")
                                             startTime
                                             duration
                                             (flags 0) ; none specified as of QT 2.0
                                             (userComponentOrInstance $autoSelectExportComponentInstance))
  "Returns the handle.
If you don't supply a handle, a GCHandle will be created.
See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-253"
  (unless publicMovie
    (setf publicMovie (T_NewHandleGC)))
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime duration)
    (checking-toolbox-error (:OSErr)
                              (#_PutMovieIntoTypedHandle
                               theMovie
                               (or selectedTrack (%null-ptr))
                               handleType
                               publicMovie
                               argStartTime
                               argDuration
                               flags
                               (%coerce-to-macptr userComponentOrInstance))))
  (values_X))

(defun_X T_NewMovieEditState :private (theMovie)
  "See IM-QuickTime p. 2-255"
  (declare (type :Movie  theMovie))
  (checking-toolbox-error
   (:pointer (#_GetMoviesError))
   (#_NewMovieEditState theMovie)))

(defun_X T_UseMovieEditState :private (theMovie toState)
  "See IM-QuickTime p. 2-255"
  (checking-toolbox-error (:OSErr)
                          (#_UseMovieEditState theMovie toState))
  (values_X))

(defun_X T_DisposeMovieEditState :private (state)
  "See IM-QuickTime p. 2-256"
  (checking-toolbox-error (:OSErr)
                          (#_DisposeMovieEditState state))
  ;; MUST return t or it will be called twice by the reaper!
  t)

(defun_X T_InsertMovieSegment :private (fromMovie toMovie toStartTime &key fromStartTime duration)
  "See T_getMovieTimeAndDurationArgs for info on the fromStartTime and duration arguments.
See IM-QuickTime p. 2-257"
  (multiple-value-bind_X
    (argFromStartTime argDuration)
    (T_getMovieTimeAndDurationArgs fromMovie fromStartTime duration)
    (checking-toolbox-error (:OSErr)
                            (#_InsertMovieSegment
                             fromMovie
                             toMovie
                             argFromStartTime
                             argDuration
                             toStartTime)))
  (values_X))

(defun_X T_InsertEmptyMovieSegment :private (theMovie &key startTime duration)
  "See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-259"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime duration)
    (checking-toolbox-error (:OSErr)
                            (#_InsertEmptyMovieSegment theMovie argStartTime argDuration)))
  (values_X))

(defun_X T_DeleteMovieSegment :private (theMovie &key startTime duration)
  "See T_getMovieTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-260"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime duration)
    (checking-toolbox-error (:OSErr)
                            (#_DeleteMovieSegment theMovie argStartTime argDuration)))
  (values_X))

(defun_X T_ScaleMovieSegment :private (theMovie &key
                                               startTime
                                               oldDuration
                                               newDuration)
  "See T_getMovieTimeAndDurationArgs for info on the startTime and oldDuration arguments.
See IM-QuickTime p. 2-260"
  (multiple-value-bind_X
    (argStartTime argOldDuration)
    (T_getMovieTimeAndDurationArgs theMovie startTime oldDuration)
    (checking-toolbox-error (:OSErr)
                            (#_ScaleMovieSegment theMovie argStartTime argOldDuration newDuration)))
  (values_X))

(defun_X T_CopyMovieSettings :private (fromMovie toMovie)
  "See IM-QuickTime p. 2-261"
  (checking-toolbox-error (:OSErr)
                          (#_CopyMovieSettings fromMovie toMovie))
  (values_X))

(defun_X T_InsertTrackSegment :private (fromTrack toTrack toStartTime &key fromStartTime duration)
  "See T_getTrackTimeAndDurationArgs for info on the fromStartTime and duration arguments.
See IM-QuickTime p. 2-262"
  (multiple-value-bind_X
    (argFromStartTime argDuration)
    (T_getTrackTimeAndDurationArgs fromTrack fromStartTime duration)
    (checking-toolbox-error (:OSErr)
                            (#_InsertTrackSegment
                             fromTrack
                             toTrack
                             argFromStartTime
                             argDuration
                             toStartTime)))
  (values_X))

(defun_X T_InsertEmptyTrackSegment :private (theTrack &key startTime duration)
  "See T_getTrackTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-263"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getTrackTimeAndDurationArgs theTrack startTime duration)
    (checking-toolbox-error (:OSErr)
                            (#_InsertEmptyTrackSegment theTrack argStartTime argDuration)))
  (values_X))

(defun_X T_InsertMediaIntoTrack :private (theTrack &key
                                                  (trackStartTime -1)
                                                  mediaStartTime
                                                  mediaDuration
                                                  (mediaRate 1))
  "See T_getMediaTimeAndDurationArgs for info on the mediaStartTime and mediaDuration arguments.
See IM-QuickTime p. 2-265"
  (multiple-value-bind_X
    (argMediaStartTime argMediaDuration)
    (T_getMediaTimeAndDurationArgs theTrack mediaStartTime mediaDuration)
    (checking-toolbox-error (:OSErr)
                            (#_InsertMediaIntoTrack
                             theTrack
                             trackStartTime
                             argMediaStartTime
                             argMediaDuration
                             mediaRate))))

(defun_X T_DeleteTrackSegment :private (theTrack &key startTime duration)
  "See T_getTrackTimeAndDurationArgs for info on the startTime and duration arguments.
See IM-QuickTime p. 2-266"
  (multiple-value-bind_X
    (argStartTime argDuration)
    (T_getTrackTimeAndDurationArgs theTrack startTime duration)
    (checking-toolbox-error (:OSErr)
                            (#_DeleteTrackSegment
                             theTrack
                             argStartTime
                             argDuration)))
  (values_X))

(defun_X T_ScaleTrackSegment :private (theTrack &key
                                               startTime
                                               oldDuration
                                               newDuration)
  "See T_getTrackTimeAndDurationArgs for info on the startTime and oldDuration arguments.
See IM-QuickTime p. 2-266"
  (multiple-value-bind_X
    (argStartTime argOldDuration)
    (T_getTrackTimeAndDurationArgs theTrack startTime oldDuration)
    (checking-toolbox-error (:OSErr)
                            (#_ScaleTrackSegment theTrack argStartTime argOldDuration newDuration)))
  (values_X))

(defun_X T_CopyTrackSettings :private (fromTrack toTrack)
  "See IM-QuickTime p. 2-267"
  (checking-toolbox-error (:OSErr)
                          (#_CopyTrackSettings fromTrack toTrack))
  (values_X))

(defun_X T_GetTrackEditRate :private (theTrack timeValue)
  "See IM-QuickTime p. 2-268"
  (let ((argTimeValue (T_getTrackTimeArgs theTrack timeValue)))
    (checking-toolbox-error (:value (#_GetMoviesError))
                            (#_GetTrackEditRate theTrack argTimeValue))))


;;;

(defun_X T_NewTrackEditState :private (theTrack)
  "See IM-QuickTime p. 2-269"
  (declare (type :Track  theTrack))
  (checking-toolbox-error
   (:pointer (#_GetMoviesError))
   (#_NewTrackEditState theTrack)))

(defun_X T_UseTrackEditState :private (theTrack toState)
  "See IM-QuickTime p. 2-270"
  (checking-toolbox-error (:OSErr)
                          (#_UseTrackEditState theTrack toState))
  (values_X))

(defun_X T_DisposeTrackEditState :private (state)
  "See IM-QuickTime p. 2-270"
  (declare (type :TrackEditState state))
  (checking-toolbox-error (:OSErr)
                          (#_DisposeTrackEditState state))
  ;; MUST return t or it will be called twice by the reaper!
  t)

(defun_X T_BeginMediaEdits :private (theMedia)
  "See IM-QuickTime p. 2-271"
  (declare (type :Media theMedia))
  (checking-toolbox-error (:OSErr)
                          (#_BeginMediaEdits theMedia))
  (values_X))

(defun_X T_EndMediaEdits :private (theMedia)
  "See IM-QuickTime p. 2-271"
  (declare (type :Media theMedia))
  (checking-toolbox-error (:OSErr)
                          (#_EndMediaEdits theMedia))
  (values_X))


(defun_X T_GetMediaHandlerDescription :private (theMedia &key creatorNameWanted)
  "Set creatorNameWanted to true if you want the creator name return value filled in.
Returns 3 values:
    mediaType
    creatorName (a possibly empty String)
    creatorManufacturer
 (a possibly empty String)
See IM-QuickTime p. 2-282"
  (rlet ((mediaType :OSType)
         (creatorName :Str255)
         (creatorManufacturer :OSType))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetMediaHandlerDescription
                             theMedia
                             mediaType
                             (if creatorNameWanted creatorName (%null-ptr))
                             creatorManufacturer))
    (values_X
      (%get-OSType mediaType)
      (if creatorNameWanted (%get-string creatorName) nil)
      (%get-OSType creatorManufacturer))))

(defun_X T_getMediaType :private (theMedia)
  "Returns mediaType.
Uses GetMediaHandlerDescription trap.
See IM-QuickTime p. 2-282"
  (rlet ((mediaType :OSType)
         (creatorManufacturer :OSType))
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_GetMediaHandlerDescription
                             theMedia
                             mediaType
                             (%null-ptr)
                             creatorManufacturer))
    (%get-OSType mediaType)))

(defun_X T_MakeFilePreview :private (resRefNum &optional
                                (progressProc 'standardMovieProgessFunction))
  "See IM-QuickTime p. 2-302"
  (checking-toolbox-error (:OSErr)
                          (#_MakeFilePreview resRefNum (progressProcDefaulter progressProc)))
  (values_X))

(defun_X T_AddFilePreview :private (resRefNum previewData &optional (previewType "PICT"))
  "Note the different order of the arguments so the previewType can default to PICT.
See IM-QuickTime p. 2-303"
  (checking-toolbox-error (:OSErr)
                          (#_AddFilePreview resRefNum previewType previewData))
  (values_X))

(defun_X T_StandardGetFilePreviewGC :private (&key (typeList '("MooV")) replyPtr)
  "The typeList arg is a list or array of file types (default is a singular list cointaining \"MooV\").
Returns either an allocated StandardFileReply GCPtr or the supplied replyPtr arg if non-nil.
If you supply a reply Ptr arg, it doesn't matter if it's a Ptr or a GCPtr.
The fileFilter arg is not supported in this version.
See IM-QuickTime p. 2-310"
  (unless replyPtr
    (setq replyPtr (newRecordGCPtr :StandardFileReply)))
  (let ((numTypes (length typeList)))
    (unless (<= numTypes 4)
      (error "Too many file types for StandardGetFilePreview")) ;? correct error form needed
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
                              (#_StandardGetFilePreview (%null-ptr) numTypes typeListArray replyPtr))))
  (unless (rref replyPtr StandardFileReply.sfGood)
    (abortEvent))
  replyPtr)

(defun_X T_getMovieFSSpecGC :private (&key (typeList '("MooV")))
  "The typeList arg is a list or array of file types (default is a singular list cointaining \"MooV\").
Returns an FSSpecGCHandle.
The fileFilter arg is not supported in this version.
Not a trap."
  (rlet ((reply :StandardFileReply))
    (T_StandardGetFilePreviewGC :typeList typeList :replyPtr reply)
    (let ((FSSpecGCHandle (newRecordGCHandle :FSSpec)))
      (copyRecordPtrToRecordHandle (rref reply StandardFileReply.sfFile)
                                     FSSPecGCHandle
                                     :FSSpec))))

;;;-----------------------------------------------------------------------------
;;;;;;;;;;;;;
;;; TimeBases
;;;;;;;;;;;;;

(defun_X T_NewTimeBaseGC :private ()
  "Returns a Timebase Record GCHandle.
See IM-QuickTime p. 2-316"
  (T_makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_NewTimeBase))
                 'T_DisposeTimeBase))

(defun_X T_DisposeTimeBase :private (timeBase)
  "See IM-QuickTime p. 2-316"
  (checking-toolbox-error (:void (#_GetMoviesError))
                                         (#_DisposeTimeBase timeBase))
  t)

(defun_X T_SetMovieMasterClock :private (movie clockComponentInstance
                                                         &optional
                                                         (slaveZero 0))
  "See IM-QuickTime p. 2-317"
  (rlet ((timeRec :TimeRecord
                  :scale 0
                  :base  (%null-ptr)))
    (rsetTimeValue64 timeRec slaveZero)
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetMovieMasterClock
                             movie
                             clockComponentInstance
                             timeRec)))
  (values_X))

(defun_X T_SetMovieMasterTimeBase :private (movie master
                                                        &optional
                                                        (slaveZero 0))
  "See IM-QuickTime p. 2-318"
  (rlet ((timeRec :TimeRecord
                  :scale 0
                  :base  (%null-ptr)))
    (rsetTimeValue64 timeRec slaveZero)
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetMovieMasterTimeBase
                             movie
                             master
                             timeRec)))
  (values_X))

(defun_X T_SetTimeBaseMasterClock :private (timeBase clockComponentInstance
                                                         &optional
                                                         (slaveZero 0))
  "See IM-QuickTime p. 2-318"
  (rlet ((timeRec :TimeRecord
                  :scale 0
                  :base  (%null-ptr)))
    (rsetTimeValue64 timeRec slaveZero)
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetTimeBaseMasterClock
                             timeBase
                             clockComponentInstance
                             timeRec)))
  (values_X))

(defun_X T_GetTimeBaseMasterClock :private (timeBase)
  "See IM-QuickTime p. 2-319"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTimeBaseMasterClock timeBase)))

(defun_X T_SetTimeBaseMasterTimeBase :private (timeBase master
                                                             &optional
                                                             (slaveZero 0))
  "See IM-QuickTime p. 2-320"
  (rlet ((timeRec :TimeRecord
                  :scale 0
                  :base  (%null-ptr)))
    (rsetTimeValue64 timeRec slaveZero)
    (checking-toolbox-error (:void (#_GetMoviesError))
                            (#_SetTimeBaseMasterTimeBase
                             timeBase
                             master
                             timeRec)))
  (values_X))

(defun_X T_GetTimeBaseMasterTimeBase :private (timeBase)
  "See IM-QuickTime p. 2-321"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTimeBaseMasterTimeBase timeBase)))

(defun_X T_SetTimeBaseZero :private (timeBase &optional (zero 0))
  "See IM-QuickTime p. 2-322"
  (rlet ((timeRec :TimeRecord
                    :scale 0
                    :base  (%null-ptr)))
      (rsetTimeValue64 timeRec zero)
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_SetTimeBaseZero timeBase timeRec)))
  (values_X))

(defun_X T_SetTimeBaseTime :private (timeBase newTime &optional (scale 0))
  "See IM-QuickTime p. 2-323"
  (rlet ((timeRec :TimeRecord
                    :scale scale
                    :base  (%null-ptr)))
      (rsetTimeValue64 timeRec newTime)
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_SetTimeBaseTime timeBase timeRec)))
  (values_X))

(defun_X T_GetTimeBaseTime :private (timeBase &optional (scale 0))
  "See IM-QuickTime p. 2-324"
  (rlet ((timeRec :TimeRecord
                    :scale scale
                    :base  (%null-ptr)))
      (checking-toolbox-error (:value (#_GetMoviesError))
                              (#_GetTimeBaseTime timeBase scale timeRec))
      (rrefTimeValue64 timeRec)))

(defun_X T_getTimeBaseTimeScale :private (timeBase)
  "See IM-QuickTime p. 2-324"
  (rlet ((timeRec :TimeRecord
                    :scale 0
                    :base  (%null-ptr)))
      (checking-toolbox-error (:value (#_GetMoviesError))
                              (#_GetTimeBaseTime timeBase 0 timeRec))
      (rref timeRec TimeRecord.scale)))

(defun_X T_SetTimeBaseRate :private (timeBase newRate)
  "See IM-QuickTime p. 2-325"
  (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_SetTimeBaseRate timeBase
                               (T_numberToFixed newRate)))
  (values_X))

(defun_X T_GetTimeBaseRate :private (timeBase)
  "See IM-QuickTime p. 2-326"
  (T_fixedToNumber (checking-toolbox-error (:value (#_GetMoviesError))
                                           (#_GetTimeBaseRate timeBase))))

(defun_X T_GetTimeBaseEffectiveRate :private (timeBase)
  "See IM-QuickTime p. 2-326"
  (T_fixedToNumber (checking-toolbox-error (:value (#_GetMoviesError))
                                           (#_GetTimeBaseEffectiveRate timeBase))))

(defun_X T_SetTimeBaseStartTime :private (timeBase newTime &optional (scale 0))
  "See IM-QuickTime p. 2-327"
  (rlet ((timeRec :TimeRecord
                    :scale scale
                    :base  (%null-ptr)))
      (rsetTimeValue64 timeRec newTime)
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_SetTimeBaseStartTime timeBase timeRec)))
  (values_X))

(defun_X T_GetTimeBaseStartTime :private (timeBase &optional (scale 0))
  "See IM-QuickTime p. 2-328"
  (rlet ((timeRec :TimeRecord
                    :scale scale
                    :base  (%null-ptr)))
      (checking-toolbox-error (:value (#_GetMoviesError))
                              (#_GetTimeBaseStartTime timeBase scale timeRec))
      (rrefTimeValue64 timeRec)))

(defun_X T_SetTimeBaseStopTime :private (timeBase newTime &optional (scale 0))
  "See IM-QuickTime p. 2-328"
  (rlet ((timeRec :TimeRecord
                    :scale scale
                    :base  (%null-ptr)))
      (rsetTimeValue64 timeRec newTime)
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_SetTimeBaseStopTime timeBase timeRec)))
  (values_X))

(defun_X T_GetTimeBaseStopTime :private (timeBase &optional (scale 0))
  "See IM-QuickTime p. 2-329"
  (rlet ((timeRec :TimeRecord
                    :scale scale
                    :base  (%null-ptr)))
      (checking-toolbox-error (:value (#_GetMoviesError))
                              (#_GetTimeBaseStopTime timeBase scale timeRec))
      (rrefTimeValue64 timeRec)))

(defun_X T_SetTimeBaseFlags :private (theTimeBase
                                       &key
                                       (flags 0)
                                       ((:loop loop_)             nil loop_Set)
                                       ((:palindrome palindrome_) nil palindrome_Set))
  "See IM-QuickTime p. 2-330"
  (trap-flag-arg flags loop_       #$loopTimeBase)
  (trap-flag-arg flags palindrome_ #$palindromeLoopTimeBase)
  (when (< 1 (logcount (logand flags (logior #$loopTimeBase
                                             #$palindromeLoopTimeBase))))
    (error "can't select both loop and palindrome"))
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetTimeBaseFlags theTimeBase flags))
  (values_X))

(defun_X T_GetTimeBaseFlags :private (theTimeBase)
  "See IM-QuickTime p. 2-330"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetTimeBaseFlags theTimeBase)))

(defun_X T_GetTimeBaseStatus :private (timeBase &optional (scale 0)) ;?
  "Returns 2 values: currentTime and a list of symbols
including 'beforeStartTime and 'afterStartTime.
See IM-QuickTime p. 2-331"
  (rlet ((timeRec :TimeRecord
                  :scale scale
                  :base  (%null-ptr)))
    (let ((result (checking-toolbox-error (:value (#_GetMoviesError))
                                          (#_GetTimeBaseStatus timeBase timeRec))))
      (values
       (rrefTimeValue64 timeRec)
       (append
        (if (logand result #$timeBaseBeforeStartTime)
          '(beforeStartTime)
          '())
        (if (logand result #$timeBaseAfterStopTime)
          '(afterStartTime)
          '()))))))

;;;-----------------------------------------------------------------------------
;;;;;;;;;;;;;
;;; CallBacks
;;;;;;;;;;;;;

(defun_X T_NewCallBackGC :private (timeBase callbackType
                                                 &key
                                                 movie
                                                 interrupt
                                                 deferredTask)
  "If movie is supplied, then the new callBack GCOSPtr will
be a slave to it instead of to the timeBase.
See IM-QuickTime p. 2-336"
 ;; what about $callBackAtDeferredTask
  (let ((type (case callBackType
                (Time     #$callBackAtTime)
                (Rate     #$callBackAtRate)
                (TimeJump #$callBackAtTimeJump)
                (Extremes #$callBackAtExtremes)
                (otherwise (error "Invalid callback type"))))
        (theMaster (or (and (terminable-macptr-p movie)
                            movie)
                       (and (terminable-macptr-p timeBase)
                            timeBase))))
    (makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_NewCallBack
                                          timeBase
                                          (if (or interrupt
                                                  deferredTask)
                                            (logior type
                                                    (if interrupt    #$callBackAtInterrupt    0)
                                                    (if deferredTask #$callBackAtDeferredTask 0))
                                            type)))
                 'T_DisposeCallBack
                 :master theMaster)))

(defun_X T_CallMeWhen :private (callback
                                     refCon
                                     param1
                                     param2
                                     param3
                                     &key callbackProc)
  "See IM-QuickTime p. 2-337"
  (unless callbackProc
    (setf callbackProc CB_QTCallBack))
  (checking-toolbox-error (:OSErr)
                          (#_CallMeWhen
                           callback
                           callbackProc
                           refCon
                           param1
                           param2
                           param3))
  (values_X))

(defun_X T_CancelCallBack :private (callback)
  "See IM-QuickTime p. 2-339"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_CancelCallBack callback))
  (values_X))

(defun_X T_DisposeCallBack :private (callback)
  "See IM-QuickTime p. 2-339"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_DisposeCallBack callback))
  t)

(defun_X T_GetCallBackTimeBase :private (callback)
  "See IM-QuickTime p. 2-340"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_GetCallBackTimeBase callback))
  (values_X))

(defun_X T_GetCallBackType :private (callback)
  "See IM-QuickTime p. 2-340"
  (checking-toolbox-error (:value (#_GetMoviesError))
                          (#_GetCallBackType callback)))

;;;-----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Callback functions
;;; See IM-QuickTime, p. 2-354
;;; and MCL Reference, p. 560
;;; see objectIDFromObject and objectFromObjectID

(defpascal_X CB_QTProgressProc (
                                   :ptr  theMovie      ; Movie
                                   :word message       ; short
                                   :word whatOperation ; short
                                   :long percentDone   ; Fixed
                                   :long refcon        ; long
                                   :long)              ; OSErr
  "Calls progressEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and the remaining args.
See IM-QuickTime p. 2-355"
  (let ((firstArg (objectFromObjectID refcon)))
    (%lisp-to-boolean (and firstArg
                           (fboundp 'progressEvent)
                           (progressEvent firstArg theMovie message whatOperation percentDone)))))

(defpascal_X CB_QTCoverProc (
                                :ptr  theMovie   ; Movie
                                :ptr  changedRgn ; RgnHandle
                                :long refcon     ; long
                                :long)           ; OSErr
  "Calls coverEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and the remaining args.
See IM-QuickTime p. 2-358"
  (let ((firstArg (objectFromObjectID refcon)))
    ; (setf covered (+ 100 covered)) ;? debug
    (%lisp-to-boolean (and firstArg
                           (fboundp 'coverEvent)
                           (coverEvent firstArg theMovie changedRgn)))))

(defpascal_X CB_QTUncoverProc (
                                  :ptr  theMovie   ; Movie
                                  :ptr  changedRgn ; RgnHandle
                                  :long refcon     ; long
                                  :long)           ; OSErr
  "Calls uncoverEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and the remaining args.
See IM-QuickTime p. 2-358"
  (let ((firstArg (objectFromObjectID refcon)))
    ; (setf uncovered (+ 100 uncovered))
    (%lisp-to-boolean (and firstArg
                           (fboundp 'uncoverEvent)
                           (uncoverEvent firstArg theMovie changedRgn)))))

(defpascal_X CB_QTErrProc (
                             :long theErr   ; OSErr
                             :long  refcon)
  "Calls qtErrorEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and theErr as second arg.
See IM-QuickTime p. 2-359"
  (let ((firstArg (objectFromObjectID refcon)))
    (%lisp-to-boolean (and firstArg
                           (fboundp 'qtErrorEvent)
                           (qtErrorEvent firstArg theErr)))))

(defpascal_X CB_QTCalloutProc (
                                  :long  refcon
                                  :word)        ; Boolean
  "Calls previewProgressEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and theErr as second arg.
See IM-QuickTime p. 2-359"
  (let ((firstArg (objectFromObjectID refcon)))
    (and firstArg
         (fboundp 'previewProgressEvent)
         (previewProgressEvent firstArg))))

(defun_X makeFileFilterProc :private (procName function)
  "Creates a low-level-callable FileFilter function named procName
that will call function with parmBlkPtr as argument.
See IM-QuickTime p. 2-359"
  (eval `(defpascal ,procName (:ptr parmBlkPtr) ; parmBlock
           (funcall ,function parmBlkPtr))))

(defpascal_X CB_QTCallBack (
                               :ptr   cb       ; QTCallBack
                               :long  refcon)
  "Calls doCallBackEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and cb as second arg.
See IM-QuickTime p. 2-364"
  (declare (ignore cb))
  (let ((firstArg (objectFromObjectID refcon)))
    (%lisp-to-boolean (and firstArg
                           (fboundp 'doCallBackEvent)
                           (doCallBackEvent firstArg 0)))))

(defpascal_X CB_QTTextProc (
                               :ptr  theText     ; CStringHandle
                               :ptr  theMovie    ; Movie
                               :ptr  displayFlag ; short*
                               :long refcon)
  "Calls textDisplayedEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and the remaining args.
See IM-QuickTime p. 2-364"
  (let ((firstArg (objectFromObjectID refcon)))
    (%lisp-to-boolean (and firstArg
                           (fboundp 'textDisplayedEvent)
                           (textDisplayedEvent firstArg theText theMovie displayFlag)))))

;;MCL3.0  - don't know which is correct, but for now I'm changing it here to match
;;  the trap definition that MCL3.0 has
(defun_X T_SetMovieDrawingCompleteProc :private (theMovie flags proc refcon)
  "See QT 4 - QuickTime 1.6 Features, p. 5"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieDrawingCompleteProc theMovie flags proc refcon))
  (values_X))

#| here's what it was:
(defun_X T_SetMovieDrawingCompleteProc :private (theMovie proc refcon)
  "See QT 4 - QuickTime 1.6 Features, p. 5"
  (checking-toolbox-error (:void (#_GetMoviesError))
                          (#_SetMovieDrawingCompleteProc theMovie proc refcon))
  (values_X))
|#

(defpascal_X CB_QTMovieDrawingCompleteProc (
                                                 :ptr  theMovie ; Movie
                                                 :long refcon)
  "Calls drawingCompleteEvent with the object referred to by refcon (using objecFromObjectID)
as the first argument, and theMovie as arg.
See QT 4 - QuickTime 1.6 Features, p. 5"
  (let ((firstArg (objectFromObjectID refcon)))
    (%lisp-to-boolean (and firstArg
                           (fboundp 'drawingCompleteEvent)
                           (drawingCompleteEvent firstArg theMovie)))))

;;;;;;;;;;;;;

;;; Useful functions

(defun_X T_isVisualMediaType :private (mediaType)
  "Returns t if mediaType is visual, else false if it is audible or some other type.
Not a trap."
  (or (eq mediaType #$VideoMediaType)
      (eq mediaType #$TextMediaType)))

(defmacro_X with-preserved-movie-gworld ((movie) &body body)
  (let ((colorGraphicsPortPtr (gensym))
        (graphicsDeviceHandle (gensym)))
    `(multiple-value-bind_X (,colorGraphicsPortPtr ,graphicsDeviceHandle)
                            (T_GetMovieGWorld ,movie)
                            (unwind-protect
                              (progn ,@body)
                              (T_SetMovieGWorld ,movie ,colorGraphicsPortPtr ,graphicsDeviceHandle)))))
                         
(defmacro_X with-preserved-movieController-gworld ((movieController) &body body)
  (let ((colorGraphicsPortPtr (gensym)))
    `(let ((,colorGraphicsPortPtr (T_MCGetControllerPort ,movieController)))
       (unwind-protect
         (progn ,@body)
         (T_MCSetControllerPort ,movieController ,colorGraphicsPortPtr)))))
                         
(defun_X T_drawMovieInGWorld :private (movie thePort gdh)
  "Displays current timeValue of movie in the desired GWorld.
Not a trap."
  (declare (type :Movie movie))
  (with-preserved-movie-gworld (movie)
    (T_SetMovieGWorld movie thePort gdh)
    (T_UpdateMovie movie)
    (T_MoviesTask movie))
  (values_X))


(defun_X T_showMoviePosterInGWorld :private (movie thePort gdh)
  "Displays poster timeValue of movie in the desired GWorld.
Not a trap."
  (declare (type :Movie movie))
  (with-preserved-movie-gworld (movie)
    (T_SetMovieGWorld movie thePort gdh)
    (T_ShowMoviePoster movie))
  (values_X))

;;;-----------------------------------------------------------------------------
;;; IM-QuickTime Components

(defun_X T_NewMovieControllerGC :private
         (theMovie
          &key
          movieRect
          (flags 0)
          (topLeftMovie    nil topLeftMovieSet)
          (scaleMovieToFit nil scaleMovieToFitSet)
          (withBadge       nil withBadgeSet)
          (notVisible      nil notVisibleSet)
          (withFrame       nil withFrameSet))
  "See IM-QuickTime Components, p. 2-29"
  (declare (type integer flags))
  (trap-flag-arg flags topLeftMovie    #$mcTopLeftMovie)
  (trap-flag-arg flags scaleMovieToFit #$mcScaleMovieToFit)
  (trap-flag-arg flags withBadge       #$mcWithBadge)
  (trap-flag-arg flags notVisible      #$mcNotVisible)
  (trap-flag-arg flags withFrame       #$mcWithFrame)
  (rlet ((localRect :rect :left 0 :top 0 :right 200 :bottom 150)) ;? what is a good default size?
    (unless movieRect
      (setf movieRect localRect))
    (T_makeGCOSPtr
     (checking-toolbox-error
      (:pointer (#_GetMoviesError))
      (progn ;;(break)
      (#_NewMovieController theMovie movieRect flags))
      )
     'T_DisposeMovieController
     :master theMovie
     :terminateOnQuit t)))

(defun_X T_MCSetMovie :private (theController theMovie &key movieWindow where)
  "See IM-QuickTime Components, p. 2-31"
  (checking-toolbox-error (:OSErr)
                       (#_MCSetMovie theController theMovie (or movieWindow (%null-ptr)) (or where (make-point 0)) ))
  (values_X))

(defun_X T_DisposeMovieController :private (theController)
  "See IM-QuickTime Components, p. 2-32"
  (checking-toolbox-error (:void (#_GetMoviesError))
                       (#_DisposeMovieController theController))
  ;; MUST return t or it will be called twice by the reaper!
  t)

(defun_X T_MCPositionController :private (theController
                                              movieRect
                                              controllerRect
                                              &key
                                              (flags 0)
                                              (topLeftMovie           nil topLeftMovieSet)
                                              (scaleMovieToFit        nil scaleMovieToFitSet)
                                              (positionDontInvalidate nil positionDontInvalidateSet))
  "See IM-QuickTime Components, p. 2-33"
  (declare (type integer flags))
  (trap-flag-arg flags topLeftMovie           #$mcTopLeftMovie)
  (trap-flag-arg flags scaleMovieToFit        #$mcScaleMovieToFit)
  (trap-flag-arg flags positionDontInvalidate #$mcPositionDontInvalidate)
  (checking-toolbox-error (:OSErr)
                       (#_MCPositionController theController movieRect controllerRect flags))
  (values_X))

(defun_X T_MCSetVisible :private (theController visible)
  "See IM-QuickTime Components, p. 2-36"
  (checking-toolbox-error (:OSErr) (#_MCSetVisible theController visible))
  (values_X))

(defun_X T_MCGetVisible :private (theController)
  "See IM-QuickTime Components, p. 2-37"
  (%boolean-to-lisp (checking-toolbox-error (:OSErr nil :ok-errors '(0 1)) (#_MCGetVisible theController))))

;;; same coding as in T_SetPosterBox
(defun_X T_MCSetControllerBoundsRect :private (theMovieController &key boxRect left top right bottom)
  "If boxRect is passed it is used and the other args are ignored.
Otherwise, any or all of these left, top, right, bottom can be used to
modify the current movie box rect.
See IM-QuickTime Components p. 2-38"
  (if boxRect
    (checking-toolbox-error (:OSErr nil)
                            (#_MCSetControllerBoundsRect theMovieController boxRect))
    (rlet ((theBox :Rect))
      (if (and left top right bottom)
        (progn
          (rset theBox rect.left   left  )
          (rset theBox rect.top    top   )
          (rset theBox rect.right  right )
          (rset theBox rect.bottom bottom))
        (progn
          (checking-toolbox-error (:OSErr)
                                  (#_MCGetControllerBoundsRect theMovieController theBox))
          (when left   (rset theBox rect.left   left  ))
          (when top    (rset theBox rect.top    top   ))
          (when right  (rset theBox rect.right  right ))
          (when bottom (rset theBox rect.bottom bottom))))
      (checking-toolbox-error (:void (#_GetMoviesError))
                              (#_MCSetControllerBoundsRect theMovieController theBox))))
  (values_X))

;;; same coding as in T_GetPosterBox
(defun_X T_MCGetControllerBoundsRectGC :private (theMovieController)
  "Returns a :Rect Record GCHandle.
See IM-QuickTime Components p. 2-39"
  (let ((boxRectGCHandle (newRecordGCHandle :Rect)))
    (with-dereferenced-handles ((boxRectPtr boxRectGCHandle))
      (checking-toolbox-error (:OSErr)
                              (#_MCGetControllerBoundsRect theMovieController boxRectPtr)))
    boxRectGCHandle))

(defun_X T_MCGetControllerBoundsRect :private (theMovieController boxRectPtr)
  "Returns the :Rect Record Ptr given as argument
See IM-QuickTime Components p. 2-39"
  (checking-toolbox-error (:OSErr)
                          (#_MCGetControllerBoundsRect theMovieController boxRectPtr))
    boxRectPtr)

(defun_X T_MCGetControllerBoundsRgnGC :private (theMovieController &key notGC)
  "See IM-QuickTime Components p. 2-40"
  (let ((theRegion (checking-toolbox-error (:pointer (#_GetMoviesError))
                                           (#_MCGetControllerBoundsRgn theMovieController))))
    (if notGC
      theRegion
      (makeGCOSPtr theRegion #_DisposeRgn))))

(defun_X T_MCGetWindowRgnGC :private (theMovieController theWindow &key notGC)
  "See IM-QuickTime Components p. 2-41"
  (let ((theRegion (checking-toolbox-error (:pointer (#_GetMoviesError))
                                           (#_MCGetWindowRgn theMovieController theWindow))))
    (if notGC
      theRegion
      (makeGCOSPtr theRegion #_DisposeRgn))))

(defun_X T_MCSetClip :private (theMovieController
                                    &key
                                    (theClip   t)
                                    (movieClip t))
  "If theClip is True (the default), it is unchanged; if nil,
it will be cleared.  Same for movieClip.
See IM-QuickTime Components p. 2-42"
  (when (or (eq theClip   t)
            (eq movieClip t))
    (multiple-value-bind_X (theClipNow movieClipNow)
                       (T_MCGetClipGC theMovieController
                                      ;; Make sure they're either t or nil.
                                      :theClipGCHandle   (if (eq   theClip t) t nil)
                                      :movieClipGCHandle (if (eq movieClip t) t nil))
      (when (eq   theClip t) (setf   theClip   theClipNow))
      (when (eq movieClip t) (setf movieClip movieClipNow))))
  ;(pr "~S ~S" theCLip movieClip)
  (checking-toolbox-error
   (:OSErr)
   (#_MCSetClip theMovieController
    (or   theClip (%null-ptr))
    (or movieClip (%null-ptr))))
  (values_X))

(defun_X T_MCGetClipGC :private (theMovieController
                                    &key
                                    (theClipGCHandle t)
                                    (movieClipGCHandle t))
  "Returns two values: theClipGCHandle and movieClipGCHandle
Each GCHandle keyword argument can take 2 possible values:
* t
   A new GCHandle will be allocated for you and filled in.
   This is the default.
* nil
   The corresponding return value will be nil
See IM-QuickTime Components p. 2-43"
  (rlet ((theClipHandlePtr :pointer)
         (theMovieClipHandlePtr :pointer))

    (checking-toolbox-error
     (:OSErr)
     (#_MCGetClip theMovieController
      (if theClipGCHandle
        theClipHandlePtr
        (%null-ptr))
      (if movieClipGCHandle
        theMovieClipHandlePtr
        (%null-ptr))))
  (values_X
    (if theClipGCHandle
      (makeGCOSPtr (%get-ptr theClipHandlePtr) '#_DisposeRgn)
      nil)
    (if movieClipGCHandle
      (makeGCOSPtr (%get-ptr theMovieClipHandlePtr) '#_DisposeRgn)
      nil))))

(defun_X T_MCSetControllerPort :private (theMovieController cGrafPtr)
  "See IM-QuickTime Components p. 2-43"
  (checking-toolbox-error
   (:OSErr)
   (#_MCSetControllerPort theMovieController cGrafPtr))
  (values_X))

(defun_X T_MCGetControllerPort :private (theMovieController)
  "See IM-QuickTime Components p. 2-44"
  (checking-toolbox-error
   (:pointer (#_GetMoviesError)) ;?
   (#_MCGetControllerPort theMovieController)))

(defun_X T_MCIsPlayerEvent :private (theMovieController theEventRecord)
  "Returns t if it handled the event, else nil.
See IM-QuickTime Components p. 2-45"
  (%boolean-to-lisp (#_MCIsPlayerEvent theMovieController theEventRecord)))

(defun_X T_MCDoAction :private (theMovieController action &optional params)
  "The params arg must not be a Handle; it is the caller's responsibility
to dreference a Handle and pass a Ptr.
See IM-QuickTime Components p. 2-47"
  (with-coercing-to-macptr (paramptr params)
    (checking-toolbox-error
     (:OSErr)
     (#_MCDoAction theMovieController action paramptr)))
  (values_X))

(defpascal_X CB_QTActionFilter (
                                   :ptr  theMovieController   ; MovieController
                                   :word  action ; short
                                   :ptr params  ; void*
                                   :long refcon
                                   :word)       ; Boolean
  "Calls actionFilter with the following args:
* The object referred to by refcon (using objecFromObjectID)
* The action (an integer, not a pointer to an integer)
* The params Ptr.
See IM-QuickTime Components p. 2-61 (MyPlayerFilterWithRefCon)"
  (let ((firstArg (objectFromObjectID refcon)))
    (%lisp-to-boolean (and firstArg (fboundp 'actionFilter)
                           (actionFilter firstArg theMovieController action params)))))

(defun_X T_MCSetActionFilterWithRefCon :private (theMovieController refCon &key (filter CB_QTActionFilter))
  "See IM-QuickTime Components p. 2-47"
  (checking-toolbox-error
   (:OSErr)
   (#_MCSetActionFilterWithRefCon theMovieController filter refCon))
  (values_X))

(defun_X T_MCGetControllerInfo :private (theMovieController)
  "See IM-QuickTime Components p. 2-48"
  (rlet ((flagsPtr :unsigned-long))
    (checking-toolbox-error
     (:OSErr)
     (#_MCGetControllerInfo theMovieController flagsPtr))
    (%get-unsigned-long flagsPtr)))

(defun_X T_MCMovieChanged :private (theMovieController theMovie)
  "See IM-QuickTime Components p. 2-49"
  (checking-toolbox-error
   (:OSErr)
   (#_MCMovieChanged theMovieController theMovie))
  (values_X))

(defun_X T_MCDraw :private (theMovieController windowPointer)
  "See IM-QuickTime Components p. 2-59"
  (checking-toolbox-error
   (:OSErr)
   (#_MCDraw theMovieController windowPointer))
  (values_X))

(defun_X T_MCIdle :private (theMovieController)
  "Returns t if it handled the event, else nil.
See IM-QuickTime Components p. 2-60"
  (%boolean-to-lisp (#_MCIdle theMovieController)))

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	2/25/94	dy	add copyright
	3	2/26/94	kleiman	TC_nil fixes
	4	2/26/94	kleiman	palindrome symbol problem
	5	2/28/94	dy	fix null-ptr stuff
	6	2/28/94	dy	preserve null-ptr values
	7	3/1/94	dy	fix T_SetMovieBox
	8	3/1/94	hernan	makeGCOSPtr -> T_makeGCOSPtr.
	9	3/8/94	dy	several NewMovieFromXXX functions now return an extra value: couldNotResolveDataReference
	10	3/11/94	dy	?
	11	3/12/94	dy	timeAndDuration fixes
	12	3/15/94	dy	new traps
	13	3/16/94	dy	
	14	3/16/94	dy	new traps, no #.
	15	3/18/94	dy	T_SetMovieMatrix
	16	3/29/94	dy	fix volume and rate functions
	16	3/29/94	dy	fix Volume and Rate calls
	17	4/7/94	dy	add Get/SetMovieMatrix, T_StandardGetFilePreviewGC, T_GetMovieFSSpecGC
	19	4/19/94	dy	Poster & Preview handling
	20	4/19/94	dy	define previewMode symbol
	21	5/2/94	dy	harmless typos
	22	5/6/94	dy	fix T_GetMovieActiveSegment when activeSegment is entire movie
	23	5/9/94	dy	rlet (:pointer :movie) instead of rlet :movie
	24	5/31/94	yost	fix matrix right column to use Fract type
	25	6/8/94	yost	Fix 1166689 - abortEvent
	26	6/13/94	dy	more stuff like what Don found in Files.lisp
	27	6/13/94	dy	previous "fix" didn't build - something's different with sk8 constants
	28	6/13/94	dy	more of same
	29	6/14/94	till	compile time adjustment
	30	6/14/94	dy	fix %get-word resourceID to %get-signed-word
	31	6/30/94	dy	
	32	7/19/94	dy	traps for saving
	33	7/20/94	dy	privatize symbols
	34	7/20/94	dy	take out debug stuff
	35	7/26/94	sidney	1176655: fix a couple of places where logand was used when logtest was intended
	36	8/19/94	dy	Fix bug 1175749 - QTMovie in Object editor crashmacsbug
	37 	 9/ 1/94	dy      	GetTrackMedia changes and numberToFixed -> T_numberToFixed etc.
	38 	 9/ 6/94	dy      	checking-toolbox-error :pointer rework
	39 	 9/ 6/94	dy      	trap-flag-arg here -> trap-flag-arg in macros-traps.lisp
	40 	 9/ 7/94	dy      	T_NewMovieFromDataForkGC with notGC
	41 	 9/14/94	dy      	add :notGC to T_NewMovieFromFileGC
	42 	 9/15/94	dy      	movieNumber and redo NewMovieFromDataFork
	43 	 9/29/94	dy      	T_DisposeMovie returns t, use symbol 'T_DisposeMovie instead of #' as termination function
	44 	10/19/94	dy      	cosmetic?
	45 	10/28/94	dy      	New traps for Movie Controller
	46 	11/28/94	dy      	gc macro mods; two new movie controller traps
	47 	 1/25/95	dy      	various new MovieController traps
	48 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	49 	 2/15/95	dy      	took out a debug printout in the controller stuff
	50 	 2/28/95	dy      	remove kFullVolume and kNoVolume, add mcActionPrerollAndPlay - from latest movies interface file
	51 	 3/ 6/95	dy      	TimeBase and CallBack wrappers
	52 	 3/ 8/95	dy      	can't remember what I tweaked
	53 	 3/ 9/95	dy      	remove T_ from  T_copyRecordxxx macro names
	54 	 3/ 9/95	dy      	fix some undeclared stuff - not used yet
	55 	 3/10/95	dy      	T_MCNewMovie, (values_X) in many places
	56 	 3/11/95	dy      	Use 'Time etc. instead of :Time in T_NewCallBackGC
	57 	 3/13/95	dy      	Use the new repairUnsignedLong function to fix trap interface bug: traps that are declared to return unsigned-long return signed.  These include creation and modification time of movie, trak, and media
	58 	 3/20/95	dy      	allow symbol 'standardMovieProgessFunction wherever TC_UseStandardMovieProgressFunction is allowed
	59 	 3/22/95	dy      	fix typo on progressProcDefaulter
	60 	 3/28/95	dy      	Fix T_NewCallBackGC, add new movie keyword arg
	61 	 3/29/95	dy      	CB_QTCallBack calls doCallBackEvent instead of callBackEvent directly
	62 	 4/ 3/95	dy      	Move T_rectRecordToRectArray to QuickDraw.lisp
	63 	 4/28/95	dy      	some timebase fixes
	3  	 6/ 9/95	sidney  	Define some trap constants that are not in the MCL3.0 interface (temporarily put in this file?)
	4  	 8/ 5/95	sidney  	remove a break that was left in the middle of things
	5  	 1/12/96	dy      	TC_nil -> (%null-ptr) and other such changes to be less sk8-centric
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	2  	10/22/96	sidney  	comment out a number of trap constants that are not in the MCL 4.0 or 3.1 interfaces. May be a bug there, so leave them here as comments
	3  	11/26/96	Hernan  	Avoiding calling things that are not fboundp.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
