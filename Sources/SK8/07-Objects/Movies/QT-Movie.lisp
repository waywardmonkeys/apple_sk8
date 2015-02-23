;-*- Mode: Lisp; Package: sk8dev -*-
(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :public
                  sk8::getFileFromUserWithPreview)

;;;======================================

(defmacro ignore-errors-but-SendToLog (&body protected-forms)
  "executes protected-forms.  If an error condition is generated,
   ignore the error but log the error message using SendToLog.
   Returns nil if there was an error, otherwise a single value 
   from the protected-forms"
  (let ((result (gensym))
        (condition (gensym)))
    `(multiple-value-bind
       (,result ,condition)
       (ignore-errors ,@protected-forms)
       (if ,condition
         (progn
           (SendToLog ,condition)
           nil)
         ,result))))

;;;? Is this a good idea?  Is it done right?
(define-sk8-function copyRealProperties nil (fromObject toObject)
  (dolist (property (realProperties fromObject))
    (unless (eq property 'knownChildren)
      (setValue property toObject (getValue property fromObject))))
  (sk8-return-nothing))

#| test it
(new Object :objectName "foop" :project SK8)

(addproperty foop 'prop1)
(addproperty foop 'prop2)
(addproperty foop 'prop3)

(new foop :objectName "f1" :project SK8)
(new foop :objectName "f2" :project SK8)
(realProperties f1)

(setf (prop1 f1) 1)
(setf (prop2 f1) 2)
(setf (prop3 f1) 3)
(inspect f1)
(copyRealProperties f1 f2)
(inspect f2)
|#
;;======================================

;;;? Make a bunch of these read-only when the object system supports that
(addProperty QuickTimeMovie 'resFileRefNum               :private t) ; movie's file resource number
(addProperty QuickTimeMovie 'resFileRefNumGCHandle       :private t) ; When terminated, this closes the file
(addProperty QuickTimeMovie 'moviePictGCHandle           :private t) ; current frame for update
(addProperty QuickTimeMovie 'moviePictTimeValue          :private t) ; timeValue of moviePictGCHandle
(addProperty QuickTimeMovie 'resourceName                          ) ; if resource file
(addProperty QuickTimeMovie 'movieNumber                           ) ; if data fork file
(addProperty QuickTimeMovie 'fileOffset                            ) ; if data fork file
(addProperty QuickTimeMovie 'tracks                                ) ; list of movie's track objects
(addProperty QuickTimeMovie 'dataReferenceWasChanged               ) ; set by new in certain circumstances
(addProperty QuickTimeMovie 'progressProc :initialValue 'standardMovieProgessFunction) ; progress function generally used
(addProperty QuickTimeMovie 'couldNotResolveDataReference          ) ; set by new in certain circumstances
(addProperty QuickTimeMovie 'renderer                              ) ; our QuickTimeRenderer child, if any
(addProperty QuickTimeMovie 'suppressingVisualTracks     :private t) ; visual tracks temporarily disabled
(addProperty QuickTimeMovie 'movieBox                    :private t) ; stashed here in case suppressingVisualTracks
(addProperty QuickTimeMovie 'preferredScale :initialValue '(1 1)   ) ; as read from the movie when created
(addProperty QuickTimeMovie 'timeBase                            )
(addProperty QuickTimeMovie 'modificationTime                      )
(addProperty QuickTimeMovie 'creationTime                          )

;;; This is a cached value - be sure to invalidatePreferredSize when visual tracks change contents,
;;; are added or deleted, or when they're enabled or disabled
(addProperty QuickTimeMovie 'preferredSize                         )

(define-handler invalidatePreferredSize :private (QuickTimeMovie)
  (setValue 'preferredSize me nil))

(define-handler restype (QuickTimeMovie)
  "MooV")


;;;;;;;;;;
;;; Macros
;;;;;;;;;;

(defmacro assumeHandle ((me osPtrName) &body body)
  `(let ((,osPtrName (mediaData ,me)))
     (if ,osPtrName
       (progn ,@body)
       *undefined*)))

(defmacro assumeHandleWithError ((me myName) &body body)
  `(let ((,myName (mediaData ,me)))
     (if ,myName
       (progn ,@body)
       (sk8-error GeneralProgrammaticError
                  :strings '("no mediaData for ")
                  :objects (list ,me)))))

(defmacro ifForwardToRenderer (ifPart elsePart)
  `(let ((myRenderer (renderer me)))
    (if (and myRenderer
             (not dontForwardToRenderer))
      ,ifPart
      ,elsePart)))

(defmacro notifyRendererOfChange (me)
  `(let ((myRenderer (renderer ,me)))
     (when myRenderer
       (movieChanged myRenderer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QuickTimeMovie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; user-callable functions

(defun_X deleteMovieFile nil (theFile)
  (T_DeleteMovieFile (appleFileSpecGCHandle theFile)))


;;; internal functions

;;;  getFileFromUserWithPreview - presents user with standard file dialog and returns a movie file object
;;;
(define-sk8-function getFileFromUserWithPreview nil (inProject
                                        &key (typeList '("MooV")))
  (new File :appleFileSpecGCHandle (T_getMovieFSSpecGC :typeList typeList) :project inProject))

;;; Create a Movie resource file
(defun initializeMovieFromResourceFileCreate (me args)
  (let ((theFile		(or (initializerArgument args 'file)
                                  (initializerArgument args 'resourceFile)))
        (creator		(initializerArgument args 'creator		:default "TVOD"))
        (scriptTag		(initializerArgument args 'scriptTag		:default TC_smSystemScript))
        (flags			(initializerArgument args 'flags			:default 0))
        (active			(initializerArgument args 'active			:default *undefined*))
        (deleteCurrentFile	(initializerArgument args 'deleteCurrentFile	:default *undefined*))
        ;; if you specify this, then new QuickTimeMovie returns nil
        (dontCreateMovie	(initializerArgument args 'dontCreateMovie	:default *undefined*))
        ;; if you specify this, then new QuickTimeMovie returns nil
        (dontOpenFile	(initializerArgument args 'dontOpenFile	:default *undefined*))
        (dontAutoAlternates	(initializerArgument args 'dontAutoAlternates	:default *undefined*)))
    
    (trap-flag-initializer flags active			#$newMovieActive)
    (trap-flag-initializer flags deleteCurrentFile	#$createMovieFileDeleteCurFile)
    (trap-flag-initializer flags dontCreateMovie	#$createMovieFileDontCreateMovie)
    (trap-flag-initializer flags dontOpenFile	#$createMovieFileDontOpenFile)
    (trap-flag-initializer flags dontAutoAlternates	#$newMovieDontAutoAlternates)
    ;; If the movie was not given a pathname, then get one from user or signal error
    (when (eql t theFile)
      (let ((thePathString (mac-nameString (choose-new-file-dialog
                                            :prompt "Name of new movie:"
                                            :button-string "Create"))))
        (setq theFile (new file :project (project me) :physicalname thePathString))))
    ;; Create movie file and possibly create movie record and open its resource fork
    (sk8-multival-bind (resFileRefNum movieGCOSPtr)
                       (T_CreateMovieFileGC
                        (appleFileSpecGCHandle theFile)
                        creator
                        :scriptTag scriptTag
                        :flags flags)
      (setValue 'file me theFile)
      (setf (mediaData me) movieGCOSPtr)
      (when (setf (resFileRefNum me) resFileRefNum)
        (setf (resFileRefNumGCHandle me)
              (T_FileRefNumToFileRefNumGCHandle resFileRefNum 'T_disposeMovieFileRefNumGCHandle)))))
  (sk8-return-nothing))

;;; Open an existing Movie resource file
(defun initializeMovieFromResourceFile (me args)
  (let ((resourceFile			(initializerArgument args 'resourceFile))
        (theFile				(initializerArgument args 'file))
        (resourceID				(initializerArgument args 'resourceID	:default 0)) ; 0 means use the first res ID for 'moov' resource found
        (resourceName			(initializerArgument args 'resourceName)) ; nil means we don't care about the resource name for the movie
        (flags					(initializerArgument args 'flags					:default 0))
        (active					(initializerArgument args 'active					:default *undefined*))
        (dontResolveDataReferences	(initializerArgument args 'dontResolveDataReferences	:default *undefined*))
        (dontAskUnresolvedDataReferences  (initializerArgument args 'dontAskUnresolvedDataReferences  :default *undefined*))
        (dontAutoAlternates			(initializerArgument args 'dontAutoAlternates			:default *undefined*)))
    
    (trap-flag-initializer flags active					#$newMovieActive)
    (trap-flag-initializer flags dontResolveDataReferences		#$newMovieDontResolveDataRefs)
    (trap-flag-initializer flags dontAskUnresolvedDataReferences	#$newMovieDontAskUnresolvedDataRefs)
    (trap-flag-initializer flags dontAutoAlternates			#$newMovieDontAutoAlternates)
    
    ;; with file is equivalent to with resourceFile here
    (when theFile
      (setf resourceFile theFile))
    
    ;; If the movie was not given a pathname, then get one from user or signal error
    (when (eql t resourceFile)
      (setq resourceFile (getFileFromUserWithPreview (project me))))
    (let ((resFileRefNum (T_OpenMovieFile (appleFileSpecGCHandle resourceFile))))
      (unwind-protect
        (sk8-multival-bind (movieGCOSPtr newResID dataRefWasChanged couldNotResolveDataReference)
                           (T_NewMovieFromFileGC resFileRefNum
                                                 :resourceID   resourceID
                                                 :resourceName resourceName
                                                 :flags flags)
          (if (= newResID TC_movieInDataForkResID)
            (setf (movieNumber                me) 1)
            (progn
              (setf (resourceID               me) newResID)
              (setf (resourceName             me) resourceName)))
          (setf (dataReferenceWasChanged      me) dataRefWasChanged)
          (setf (couldNotResolveDataReference me) couldNotResolveDataReference)
          (setf (mediaData                    me) movieGCOSPtr)
          (setValue 'file                     me  resourceFile))
        (T_CloseMovieFile resFileRefNum))))
  (sk8-return-nothing))
        
;;; Open an existing Movie data fork file
(defun initializeMovieFromDataFile (me args)
  (let ((dataFile				(initializerArgument args 'dataFile))
        (movieNumber				(initializerArgument args 'movieNumber))
        (fileOffset				(initializerArgument args 'fileOffset))
        (flags					(initializerArgument args 'flags					:default 0))
        (active					(initializerArgument args 'active					:default *undefined*))
        (dontResolveDataReferences	(initializerArgument args 'dontResolveDataReferences	:default *undefined*))
        (dontAskUnresolvedDataReferences  (initializerArgument args 'dontAskUnresolvedDataReferences  :default *undefined*))
        (dontAutoAlternates			(initializerArgument args 'dontAutoAlternates			:default *undefined*)))
    
    (trap-flag-initializer flags active					#$newMovieActive)
    (trap-flag-initializer flags dontResolveDataReferences	#$newMovieDontResolveDataRefs)
    (trap-flag-initializer flags dontAskUnresolvedDataReferences  #$newMovieDontAskUnresolvedDataRefs)
    (trap-flag-initializer flags dontAutoAlternates			#$newMovieDontAutoAlternates)
    ;; If the movie was not given a pathname, then get one from user or signal error
    (when (eql t dataFile)
      (setq dataFile (getFileFromUserWithPreview (project me))))
    (let ((fRefNum (T_OpenMovieFile (appleFileSpecGCHandle dataFile))))
      (unwind-protect
        (let (movieGCOSPtr dataRefWasChanged couldNotResolveDataReference)
          (sk8-multival-setf (movieGCOSPtr fileOffset dataRefWasChanged couldNotResolveDataReference)
                             (T_NewMovieFromDataForkGC fRefNum
                                                       :movieNumber movieNumber
                                                       :fileOffset  fileOffset
                                                       :flags       flags))
          (setf (movieNumber                  me) movieNumber)
          (setf (fileOffset                   me) fileOffset)
          (setf (dataReferenceWasChanged      me) dataRefWasChanged)
          (setf (couldNotResolveDataReference me) couldNotResolveDataReference)
          (setf (mediaData                    me) movieGCOSPtr)
          (setValue 'file                     me  dataFile))
        (T_CloseMovieFile fRefNum))))
  (sk8-return-nothing))

;;; new movie from a handle
(defun initializeMovieFromHandle (me theHandle args)
  (let ((flags					(initializerArgument args 'flags					:default 0))
        (active					(initializerArgument args 'active					:default *undefined*))
        (dontResolveDataReferences	(initializerArgument args 'dontResolveDataReferences	:default *undefined*))
        (dontAskUnresolvedDataReferences  (initializerArgument args 'dontAskUnresolvedDataReferences  :default *undefined*))
        (dontAutoAlternates			(initializerArgument args 'dontAutoAlternates			:default *undefined*)))
    
    (trap-flag-initializer flags active					#$newMovieActive)
    (trap-flag-initializer flags dontResolveDataReferences	#$newMovieDontResolveDataRefs)
    (trap-flag-initializer flags dontAskUnresolvedDataReferences  #$newMovieDontAskUnresolvedDataRefs)
    (trap-flag-initializer flags dontAutoAlternates			#$newMovieDontAutoAlternates)
    
    (sk8-multival-bind (movieGCOSPtr dataRefWasChanged couldNotResolveDataReference)
                       (T_NewMovieFromHandleGC theHandle :flags flags)
      (setf (dataReferenceWasChanged me) dataRefWasChanged)
      (setf (couldNotResolveDataReference me) couldNotResolveDataReference)
      (setf (mediaData me) movieGCOSPtr)))
  (sk8-return-nothing))

;;; new movie from the clipboard
(defun initializeMovieFromScrap (me args)
  (let ((flags					(initializerArgument args 'flags					:default 0))
        (active					(initializerArgument args 'active					:default *undefined*))
        (dontResolveDataReferences	(initializerArgument args 'dontResolveDataReferences	:default *undefined*))
        (dontAskUnresolvedDataReferences  (initializerArgument args 'dontAskUnresolvedDataReferences  :default *undefined*))
        (dontAutoAlternates			(initializerArgument args 'dontAutoAlternates			:default *undefined*)))
    
    (trap-flag-initializer flags active					#$newMovieActive)
    (trap-flag-initializer flags dontResolveDataReferences	#$newMovieDontResolveDataRefs)
    (trap-flag-initializer flags dontAskUnresolvedDataReferences  #$newMovieDontAskUnresolvedDataRefs)
    (trap-flag-initializer flags dontAutoAlternates			#$newMovieDontAutoAlternates)
    
    (sk8-multival-bind (movieGCOSPtr couldNotResolveDataReference)
                       (T_NewMovieFromScrapGC :flags flags)
      (setf (couldNotResolveDataReference me) couldNotResolveDataReference)
      (setf (mediaData me) movieGCOSPtr)))
  (sk8-return-nothing))


;;; new movie from a Movie GCOSPtr
(defun initializeMovieFromMovieGCOSPtr (me args)
  (setf (mediaData me) (initializerArgument args 'movieGCOSPtr))
  (sk8-return-nothing))


;;; new movie from nothing
(defun initializeMovieFromNothing (me args)
  (let ((flags					(initializerArgument args 'flags			:default 0))
        (active					(initializerArgument args 'active			:default *undefined*))
        (dontAutoAlternates			(initializerArgument args 'dontAutoAlternates	:default *undefined*)))
    
    (trap-flag-initializer flags active					#$newMovieActive)
    (trap-flag-initializer flags dontAutoAlternates			#$newMovieDontAutoAlternates)
    
    (setf (mediaData me) (T_NewMovieGC :flags flags)))
  (sk8-return-nothing))

(defun qtmFinalizeInitialization (me isNew)
  (setValue 'progressProc me 'standardMovieProgessFunction)
  (setValue 'tracks me (new QuickTimeTrackCollection :movie me :project (project me)))
  (when isNew
    (setValue 'preferredScale me (scale me)))
  (sk8-multival-bind (left top right bottom) (movieBox me)
    (setValue 'movieBox me (list
                         0
                         0
                         (- right left)
                         (- bottom top))))
  (sk8-return-nothing))


(define-handler initialize (QuickTimeMovie original isNew initArgs)
  (requireQuickTimeInitialized)
  ;? (call-next-method) this should be here, but we've got to be sure that file of me isn't set to :project wantonly
  (when-unwind
    (if isNew
      (initializeNewQuickTimeMovie me initargs)
      (initializeCopyQuickTimeMovie me original initargs))
    (setf (objectName me) nil))
  (sk8-return-nothing))

(defun initializeNewQuickTimeMovie (me args)
  (let ((theFile         (initializerArgument args 'file))
        (theResourceFile (initializerArgument args 'resourceFile))
        (theHandle	 (initializerArgument args 'mediaData))
        (scrap		 (initializerArgument args 'clipBoard))
        (theMovieGCOSPtr (initializerArgument args 'movieGCOSPtr))
        (theDataFile	 (initializerArgument args 'dataFile))
        (movieNumber	 (initializerArgument args 'movieNumber))
        (create		 (initializerArgument args 'create))
        (resourceID	 (initializerArgument args 'resourceID))
        (resourceName    (initializerArgument args 'resourceName))
        (fileOffset	 (initializerArgument args 'fileOffset)))
    
    (let ((initializeNewQuickTimeMovie "new QuickTimeMovie"))
      (unless (<= (count-if-not #'null (list theFile
                                             theResourceFile
                                             theDataFile
                                             theHandle
                                             scrap
                                             theMovieGCOSPtr))
                  1)
        (sk8-error IncorrectArgumentsError
                   :handlername initializeNewQuickTimeMovie
                   :arguments (append (when theFile         (list :file         theFile))
                                      (when theResourceFile (list :resourceFile theDataFile))
                                      (when theDataFile     (list :dataFile     theResourceFile))
                                      (when theHandle       (list :handle       theHandle))
                                      (when scrap           (list :clipBoard    scrap))
                                      (when theMovieGCOSPtr (list :movieGCOSPtr theMovieGCOSPtr)))))
      
      
      (when (and create
                 (or theHandle scrap theMovieGCOSPtr))
        (sk8-error IncorrectArgumentsError
                   #| :strings '("The create: true argument can only accompany with resourceFile or with dataFile") |#
                   :handlername initializeNewQuickTimeMovie
                   :arguments (append (list :create t)
                                      (cond 
                                       (theHandle       (list :handle       theHandle))
                                       (scrap           (list :clipBoard    scrap))
                                       (theMovieGCOSPtr (list :movieGCOSPtr theMovieGCOSPtr))))))
      
      (setValue 'file me nil)
      (cond
       (theResourceFile
        ;; new movie will be associated from a resource file
        (when (or movieNumber fileOffset)
          (sk8-error IncorrectArgumentsError
                     :handlername initializeNewQuickTimeMovie
                     :arguments (append (list :resourceFile theResourceFIle)
                                        (when movieNumber  (list :movieNumber  movieNumber))
                                        (when fileOffset   (list :fileOffset   fileOffset)))))
        (if create
          (initializeMovieFromResourceFileCreate me args)
          (initializeMovieFromResourceFile       me args)))
       
       (theDataFile
        ;; new movie will be associated from a data fork file
        (when (or resourceID resourceName)
          (sk8-error IncorrectArgumentsError
                     :handlername initializeNewQuickTimeMovie
                     :arguments (append (list :dataFile theDataFile)
                                        (when resourceID   (list :resourceID   resourceID))
                                        (when resourceName (list :resourceName resourceName)))))
        (if create
          (initializeMovieFromResourceFileCreate me args) ; same code as for resourceFile
          (initializeMovieFromDataFile me args))
        (setf (movieNumber me) 1))
       
       (theFile
        ;; new movie will be associated from a resource or data fork file, to be determined by the NewMovieFromResourceFile trap
        (when create
          (sk8-error IncorrectArgumentsError
                     #| :strings '("The create: true argument can only accompany with resourceFile or with dataFile") |#
                     :handlername initializeNewQuickTimeMovie
                     :arguments (list :create t :file theFile)))
        (when (and (or resourceID resourceName)
                   (or movieNumber fileOffset))
          (sk8-error IncorrectArgumentsError
                     :handlername initializeNewQuickTimeMovie
                     :arguments (append (list :file theFile)
                                        (when resourceID   (list :resourceID   resourceID))
                                        (when resourceName (list :resourceName resourceName))
                                        (when movieNumber  (list :movieNumber  movieNumber))
                                        (when fileOffset   (list :fileOffset   fileOffset)))))
        (initializeMovieFromResourceFile me args)) ; same code as for resourceFile
       
       (theHandle
        ;; new movie will come from a handle
        (initializeMovieFromHandle me theHandle args))
       
       (scrap
        ;; new movie will come from the ClipBoard
        (initializeMovieFromScrap me args))
       
       (theMovieGCOSPtr
        ;; new movie will come from a pointer gotten from the QuickTime toolbox
        (initializeMovieFromMovieGCOSPtr me args))
       
       (t
        ;; new movie from scratch, not associated with anything
        (initializeMovieFromNothing me args)))))
  (setValue 'movieBox me #(0 0 0 0))
  (qtmFinalizeInitialization me t)
  (sk8-return-nothing))

(defun clearMovieProperties (me isNew)
  (setValue 'resFileRefNum           me nil)
  (setValue 'resFileRefNumGCHandle   me nil)
  (setValue 'movieBox                me #(0 0 0 0)) ;? should be an array
  (setValue 'renderer                me nil)
  (setValue 'suppressingVisualTracks me nil)
  (setValue 'timeBase                me nil)
  (setValue 'creationTime            me nil)
  (setValue 'modificationTime        me nil)
  (invalidatePreferredSize me)
  (qtmFinalizeInitialization me isNew)
  (sk8-return-nothing))

#|
(trace
       initializeCopyQuickTimeMovie
       initializeMovieFromHandle
       clearMovieProperties
       tracks
       trackFromID
       reconcileTrackCollectionWithMovieTracks
       enabled
       (setf reallyEnabled)
       invalidatePreferredSize
       qtmFinalizeInitialization
)
|#

;;;? Seems to work, but strange things happened afterwards.  Keep this under suspicion...  940701 DY

(defun initializeCopyQuickTimeMovie (me original args)
  (initializeMovieFromHandle me (T_PutMovieIntoHandleGC (mediaData original)) args)
  (clearMovieProperties me nil)
  ;; Re-enable any tracks that may have been disabled by suppressingVisualTracks
  (let ((newTrack nil))
    (dolist (theTrack (data (tracks original)))
      (when (enabled theTrack)
        ;; WARNING: the original code failed here because the tracks were NOT
        ;; copied. It is not clear in this mess where the copying should be happening.
        ;; Thus. I added error checking. This is not a good solution! 
        (setf newTrack (trackFromID me (id theTrack)))
        (when newTrack
          (setf (reallyEnabled newTrack) t)))))
  (sk8-return-nothing))

(define-handler importFromResource :private (QuickTimeMovie source destination)
                (declare (ignore destination))
                (initializeMovieFromResourceFile me (new ObjectTable
                                                         :project (project me)
                                                         :data `((resourceFile . ,(findFileInProject (file source) (project me)))
                                                                 (resourceID   . ,(resourceID source)))))
                (clearMovieProperties me t)
                me)

(define-handler importFromDataFork :private (QuickTimeMovie source)
                       (initializeMovieFromResourceFile me (new ObjectTable
                                                                :project (project me)
                                                                :data `((file . ,(findFileInProject source (project me))))))
                       (clearMovieProperties me t)
                       me)

(define-handler importFromClipBoard :private (QuickTimeMovie source destination)
                       (declare (ignore source destination))
                       (initializeMovieFromScrap me (new ObjectTable
                                                         :project (project me)
                                                         :data nil))
                       (clearMovieProperties me t)
                       me)

;; for the media importer
(define-handler displaySample :private (QuickTimeMovie theActor)
  (setf (fillColor theActor) (new QuickTimeRenderer
                                  :media me
                                  :resizingStyle 'resizeMovieFromRenderedRegion
                                  :project (project theActor)))
  ; (setf (media (fillColor theActor)) me)
  (sk8-return-nothing))

(define-handler releaseOSPtrs :private (QuickTimeMovie)
  (when (renderer me)
    (stop (renderer me)))  ; Make sure it's not playing
  (setValue 'mediaData             me nil)
  (setValue 'resFileRefNumGCHandle me nil) ; Make sure resource file, if any, is closed
  (setValue 'moviePictGCHandle     me nil) ; Make sure "current frame" pict, if any, is released
  (setValue 'moviePictTimeValue    me nil) ; Make it known that moviePictGCHandle is invalid
  (sk8-return-nothing))

(define-handler (setf file) (newFile QuickTimeMovie)
  (unless (eq newFile t) ; (This can happen at initialization.)
      (call-next-method)))

;;; trackFromID violates the encapsulation of (tracks me)

(define-handler trackFromID (QuickTimeMovie id)
  (assumeHandle (me myMovieGCOSPtr)
    (dolist (track (data (tracks me)))
      (when (eql id (id track))
        (return track)))))

(define-handler (setf suppressingVisualTracks) :private (suppressing QuickTimeMovie)
  (when (xor suppressing (suppressingVisualTracks me))
    (when suppressing
      (setValue 'movieBox me (movieBox me))) ; do it while we're not suppressing
    (let ((newEnabledStatus (not suppressing)))
      (dolist (track (data (tracks me)))
        (when (and (enabled track)
                   (isVisual (media track)))
          (setf (reallyEnabled track) newEnabledStatus))))
    #|(unless suppressing
      (setf (movieBox me) (getValue 'movieBox me))) ; do it while we're not suppressing |#
    (setValue 'suppressingVisualTracks me suppressing))
  suppressing)

;;; installTrackActors - installs track actors for the movie
;;;

#|
(define-handler installMovieSubActors :private (QuickTimeMovie)
  (let ((tr nil))
    (dotimes (index (numberOfTracks me))
      (when (setq tr (installTrackActor me :trackIndex (1+ index)))
        (new QuickTimeMedia :project (project me) :track tr)))))
|#

(define-handler active (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieActive myMovieGCOSPtr)))

(define-handler (setf active) (newValue 
                                        QuickTimeMovie
                                        &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (setf (active myRenderer) newValue)
     (T_SetMovieActive myMovieGCOSPtr newValue))
    newValue))

(define-handler dataSize (QuickTimeMovie &key segment)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieDataSize myMovieGCOSPtr :startTime (first segment) :duration (second segment))))


#|
(define-handler changed (QuickTimeMovie)
  )
(define-handler (setf changed) (newValue QuickTimeMovie)
  )
 |#

(define-handler repeating (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (repeating (timeBase me))))
    
(define-handler (setf repeating) (newRepeating QuickTimeMovie)
  (prog1
    (setf (repeating (timeBase me)) newRepeating)
    (notifyRendererOfChange me)))

(define-handler timeValue (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieTimeValue myMovieGCOSPtr)))

(define-handler (setf timeValue) (newTimeValue
                                            QuickTimeMovie
                                            &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (setf (timevalue myRenderer) newTimeValue)
     (progn
       (T_SetMovieTimeValue myMovieGCOSPtr newTimeValue)
       (T_GetMovieTimeValue myMovieGCOSPtr)))))

(define-handler goToBeginning (QuickTimeMovie
                                         &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (goToBeginning myRenderer)
     (T_GoToBeginningOfMovie myMovieGCOSPtr))
    (sk8-return-nothing)))

(define-handler goToEnd (QuickTimeMovie
                                  &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (goToEnd myRenderer)
     (T_GoToEndOfMovie myMovieGCOSPtr))
    (sk8-return-nothing)))

(define-handler nextInterestingTime
                       (QuickTimeMovie
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
                        interestingDurationWanted
                        dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (trap-flag-arg flags mediaSample         #$nextTimeMediaSample)
    (trap-flag-arg flags mediaEdit           #$nextTimeMediaEdit)
    (trap-flag-arg flags trackEdit           #$nextTimeTrackEdit)
    (trap-flag-arg flags syncSample          #$nextTimeSyncSample)
    (trap-flag-arg flags edgeOK              #$nextTimeEdgeOK)
    (trap-flag-arg flags ignoreActiveSegment #$nextTimeIgnoreActiveSegment)
    (ifForwardToRenderer
     (nextInterestingTime myRenderer
                          :startTime                 startTime
                          :rate                      rate
                          :flags                     flags
                          :mediaTypes                mediaTypes
                          :interestingDurationWanted interestingDurationWanted)
     (T_GetMovieNextInterestingTime
        myMovieGCOSPtr
        :startTime startTime
        :rate rate
        :flags flags
        :mediaTypes mediaTypes
        :interestingDurationWanted interestingDurationWanted))))

(define-handler activeSegment (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieActiveSegment myMovieGCOSPtr)))

;;; If movie is in preview mode, QuickTime clips active segment to fit within preview segment
(define-handler (setf activeSegment) (segment QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMovieActiveSegment myMovieGCOSPtr
                             :startTime (first segment)
                             :duration (second segment))
    ; (notifyRendererOfChange me)
    (T_GetMovieActiveSegment myMovieGCOSPtr)))

(define-handler boundsRegionGCHandle :private (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieBoundsRgnGC myMovieGCOSPtr)))

(define-handler clipRegionGCHandle :private (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieClipRgnGC myMovieGCOSPtr)))

(define-handler (setf clipRegionGCHandle) :private (region QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMovieClipRgn myMovieGCOSPtr region)))

(define-handler displayClipRegionGCHandle :private (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieDisplayClipRgnGC myMovieGCOSPtr)))

(define-handler (setf displayClipRegionGCHandle) :private (region QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMovieDisplayClipRgn myMovieGCOSPtr region)))

(define-handler (setf movieBox) (newBox
                                           QuickTimeMovie
                                           &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (unless (= (length newBox) 4)
      (sk8-error IncorrectArgumentsError
                 :handlername "set movieBox"
                 :arguments (list newBox)))
    (ifForwardToRenderer
     (sk8-error GeneralProgrammaticError
                  :strings '("Can't set movieBox of " " because it is associated with a renderer.")
                  :objects (list me))
     (if (suppressingVisualTracks me)
       (sk8-multivals-sequence 4 (setValue 'movieBox me newBox))
       (progn
         (T_SetMovieBox myMovieGCOSPtr
                        :left   (if (elt newBox 0) (round (elt newBox 0)) nil)
                        :top    (if (elt newBox 1) (round (elt newBox 1)) nil)
                        :right  (if (elt newBox 2) (round (elt newBox 2)) nil)
                        :bottom (if (elt newBox 3) (round (elt newBox 3)) nil))
         (movieBox me))))))

(define-handler movieBox (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (if (suppressingVisualTracks me)
      (sk8-multivals-sequence 4 (getValue 'movieBox me))
      (let ((theBox (T_GetMovieBoxGC myMovieGCOSPtr)))
        (SK8-multivals (href theBox :rect.left)
                       (href theBox :rect.top)
                       (href theBox :rect.right)
                       (href theBox :rect.bottom))))))

(define-handler height (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (sk8-multival-bind
      (left top right bottom)
      (movieBox me)
      (declare (ignore left right))
      (- bottom top))))

(define-handler width (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (sk8-multival-bind
      (left top right bottom)
      (movieBox me)
      (declare (ignore top bottom))
      (- right left))))

(define-handler preferredAspectRatio (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (sk8-multival-bind
      (width height)
      (preferredSize me)
      (if (zerop height)
        0
        (/ width height)))))

(define-handler (setf scale) (newScale
                                        QuickTimeMovie
                                        &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (setf (scale myRenderer) newScale)
     (let ((theMatrix (T_GetMovieMatrixArray myMovieGCOSPtr)))
       (if (null newScale)
         (progn
           (setf (aref theMatrix 0 0) 1)
           (setf (aref theMatrix 1 1) 1))
        (progn
         (ensureType newScale Collection)
         (assert (= (length newScale) 2))
         (setf (aref theMatrix 0 0) (elt newScale 0))
         (setf (aref theMatrix 1 1) (elt newScale 1))))
        (T_SetMovieMatrixFromArray myMovieGCOSPtr theMatrix)
       (scale me)))))

(define-handler scale (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (let ((theMatrix (T_GetMovieMatrixArray myMovieGCOSPtr)))
      (sk8-multivals (aref theMatrix 0 0) (aref theMatrix 1 1)))))

(define-handler (setf matrix) (newMatrix
                                        QuickTimeMovie
                                        &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (setf (matrix myRenderer) newMatrix)
     (progn
       (cond
        ((zone-pointerp newMatrix)
         (T_SetMovieMatrix myMovieGCOSPtr newMatrix))
        ((or (eql newMatrix nil)
             (eql newMatrix TC_nil))
         (T_SetMovieMatrix myMovieGCOSPtr TC_nil))
        (t
         (T_SetMovieMatrixFromArray myMovieGCOSPtr newMatrix)))
       (T_GetMovieMatrixArray myMovieGCOSPtr)))))

(define-handler matrix (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieMatrixArray myMovieGCOSPtr)))

(defun calculatePreferredSize (me saveStuff)
  (gs:boxSize (if saveStuff
                (with-preserved-values (((suppressingVisualTracks me) nil)
                                        ((movieBox me)                   ))
                  (setf (scale me :dontForwardToRenderer t) (preferredScale me))
                  (movieBox me))
                (progn
                  (setf (scale me :dontForwardToRenderer t) (preferredScale me))
                  (movieBox me)))))

(define-handler preferredSize (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (let ((cachedValue (getValue 'preferredSize me)))
      (if cachedValue
        (sk8-multivals-sequence 2 cachedValue)
        (let* ((myRenderer (renderer me))
               (calculatedValue (if (and myRenderer
                                         (actorOwner myrenderer))
                                  ;; do it the expensive way
                                  (calculatePreferredSize (sk8::copy me :project (project me))
                                                          nil)
                                  ;; do it the efficient way
                                  (calculatePreferredSize me t))))
          (setValue 'preferredSize me calculatedValue)
          (sk8-multivals-sequence 2 calculatedValue))))))

#|
(define-handler setMoviePlayHints (QuickTimeMovie flags flagsMask)
  (declare (ignore flags flagsMask))
  (error "Not implemented"))
|#

;;;;;;;;;;;;;;;;
;;; Editing Movies
;;;;;;;;;;;;;;;;

(define-handler putOnScrap (QuickTimeMovie
                                       &key
                                       (flags 0)
                                       (dontZeroScrap nil dontZeroScrapSet)
                                       (onlyPutMovie  nil onlyPutMovieSet))
  (assumeHandleWithError (me myMovieGCOSPtr)
    (trap-flag-arg flags dontZeroScrap #$movieScrapDontZeroScrap)
    (trap-flag-arg flags onlyPutMovie  #$movieScrapOnlyPutMovie)
    (T_PutMovieOnScrap myMovieGCOSPtr :flags flags)
    (sk8-return-nothing)))

(define-handler selection (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieSelection myMovieGCOSPtr)))

(define-handler (setf selection) (segment QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMovieSelection myMovieGCOSPtr
                         :startTime (first segment)
                         :duration (second segment))
    (notifyRendererOfChange me)
    (T_GetMovieSelection myMovieGCOSPtr)))

(define-handler cutSelection (QuickTimeMovie
                                       &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (cutSelection myRenderer)
     (progn
       (new QuickTimeMovie
            :project (project me)
            :movieGCOSPtr (T_CutMovieSelectionGC myMovieGCOSPtr))
       (invalidateTracks (tracks me)))) ;? is this overkill
    (sk8-return-nothing)))

(define-handler copySelection (QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (new QuickTimeMovie
         :project (project me)
         :movieGCOSPtr (T_CopyMovieSelectionGC myMovieGCOSPtr))))

(define-handler pasteSelection (QuickTimeMovie
                                          sourceMovie
                                          &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (pasteSelection myRenderer sourceMovie)
     (progn
       (T_PasteMovieSelection myMovieGCOSPtr (mediaData sourceMovie))
       (invalidateTracks (tracks me))))
    (sk8-return-nothing)))

#|
(define-handler newEditState (QuickTimeMovie)
  (prog1 (#_NewMovieEditState (mediaData me))
    (checkMovieError)))

(define-handler useEditState (QuickTimeMovie s)
  (#_UseMovieEditState (mediaData me) s)
    (notifyRendererOfChange me)
  (checkMovieError))

(defun disposeMovieEditState (s)
  (#_DisposeMovieEditState s) ; disposal timing of edit states is really messy.  Be careful
  (checkMovieError))

(define-handler insertSegment (QuickTimeMovie
                                           destinationMovie
                                           segment
                                           destinationTime)
  (#_InsertMovieSegment
   (mediaData me)
   (mediaData destinationMovie)
   (first segment) (second segment) destinationTime)
    (notifyRendererOfChange me)
  (checkMovieError))

(define-handler insertEmptySegment (QuickTimeMovie segment)
  (#_InsertEmptyMovieSegment (mediaData me) tim dur)
  (notifyRendererOfChange me)
  (checkMovieError))

(define-handler deleteSegment (QuickTimeMovie (first segment) (second segment))
  (#_DeleteMovieSegment (mediaData me) tim dur)
  (notifyRendererOfChange me)
  (checkMovieError))

(define-handler scaleSegment (QuickTimeMovie tim oldDur dur)
  (#_ScaleMovieSegment (mediaData me) tim olddur dur)
  (notifyRendererOfChange me)
  (checkMovieError))

(define-handler copySettings (QuickTimeMovie destinationMovie)
  (#_CopyMovieSettings (mediaData me) (mediaData destinationMovie))
  (checkMovieError))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movie Control Calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Movie Preloading
;;;;;;;;;;;;;;;;;;

(define-handler preroll (QuickTimeMovie
                                  &key startTime rate)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_PrerollMovie myMovieGCOSPtr :startTime startTime :rate rate)))

(define-handler loadIntoRam (QuickTimeMovie
                                      &key
                                      segment
                                      (flags 0)
                                      (keepInRam              nil keepInRamSet)
                                      (unkeepInRam            nil unkeepInRamSet)
                                      (flushFromRam           nil flushFromRamSet)
                                      (loadForwardTrackEdits  nil loadForwardTrackEditsSet)
                                      (loadBackwardTrackEdits nil loadBackwardTrackEditsSet))
  (assumeHandleWithError (me myMovieGCOSPtr)
    (trap-flag-arg flags keepInRam              #$keepInRam)
    (trap-flag-arg flags unkeepInRam            #$unkeepInRam)
    (trap-flag-arg flags flushFromRam           #$flushFromRam)
    (trap-flag-arg flags loadForwardTrackEdits  #$loadForwardTrackEdits)
    (trap-flag-arg flags loadBackwardTrackEdits #$loadBackwardTrackEdits)
    (T_LoadMovieIntoRam myMovieGCOSPtr
                        :startTime (first segment)
                        :duration (second segment)
                        :flags flags)))

;;;;;;;;;;;;;;;
;;; Movie Timing
;;;;;;;;;;;;;;;


(define-handler preferredRate (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMoviePreferredRate myMovieGCOSPtr)))

(define-handler (setf preferredRate) (rate QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (when (zerop rate)
      (sk8-error IncorrectArgumentsError
                 :handlername "set preferredRate"
                 :arguments (list rate)))
    (T_SetMoviePreferredRate myMovieGCOSPtr rate)
    (notifyRendererOfChange me)
    (T_GetMoviePreferredRate myMovieGCOSPtr)))

(define-handler rate (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieRate myMovieGCOSPtr)))

;;; we don't allow you to set the rate here, because that's like
;;; tellthe movie to play or stop, and that functionality
;;; is reserved for QuickTimeRenderer

(define-handler timeScale (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieTimeScale myMovieGCOSPtr)))

(define-handler (setf timeScale) (timeScale QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMovieTimeScale myMovieGCOSPtr timeScale)
    (notifyRendererOfChange me)
    (T_GetMovieTimeScale myMovieGCOSPtr)))

(define-handler duration (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieDuration myMovieGCOSPtr)))

#| what should we do about these?  They involve low-level records.
(define-handler (setf timeRecordGCHandle) (newTime QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_SetMovieTime myMovieGCOSPtr newTime)))

(define-handler timeRecordGCHandle (QuickTimeMovie &key timeRecordHandle)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieTimeRecordGC myMovieGCOSPtr :timeRecordHandle timeRecordHandle)))
|#

(define-handler creationTime (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (or (getValue 'creationTime me)
        (setValue 'creationTime me
                  (new DateTimeIndirect
                       :project (project me)
                       :getterFunction 'T_GetMovieCreationTime
                       :getterArguments (list myMovieGCOSPtr))))))

(define-handler modificationTime (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (or (getValue 'modificationTime me)
        (setValue 'modificationTime me
                  (new DateTimeIndirect
                       :project (project me)
                       :getterFunction 'T_GetMovieModificationTime
                       :getterArguments (list myMovieGCOSPtr))))))

(define-handler timeBase (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (or (getValue 'timeBase me)
        (let ((timeBaseOSPtr (T_GetMovieTimeBase myMovieGCOSPtr)))
          (or (sk8ObjectRepresentingOSPtr timeBaseOSPtr)
              (let ((timeBase (new QuickTimeTimeBase
                                   :timeBaseOSPtr timeBaseOSPtr
                                   :movie me
                                   :project (project me))))
                (registerSk8ObjectRepresentingOSPtr timeBase timeBaseOSPtr :masterGCOSPtr myMovieGCOSPtr)
                (setf (timeBase me) timeBase)))))))

;;;;;;;;;
;;; Sound
;;;;;;;;;

(define-handler preferredVolume (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMoviePreferredVolume myMovieGCOSPtr)))

(define-handler (setf preferredVolume) (volume QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMoviePreferredVolume myMovieGCOSPtr volume)
    (notifyRendererOfChange me)
    (T_GetMoviePreferredVolume myMovieGCOSPtr)))

(define-handler volume (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMovieVolume myMovieGCOSPtr)))

(define-handler (setf volume) (volume QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMovieVolume myMovieGCOSPtr volume)
    (notifyRendererOfChange me)
    (T_GetMovieVolume myMovieGCOSPtr)))


;;;;;;;;;;;;
;;; User Data
;;;;;;;;;;;;

#|

    This should be changed to return a userdata object

(define-handler userData (QuickTimeMovie)
	(#_GetMovieUserData (mediaData me)))

|#

;;;;;;;;;;;;;;;;;
;;; Language
;;;;;;;;;;;;;;;;;

#|
(define-handler (setf movieLanguage) (language QuickTimeMovie)
  (#_SetMovieLanguage (mediaData me) language)
  (checkMovieError))
|#

;;;;;;;;;;;;;;;;;
;;; Alternate Tracks
;;;;;;;;;;;;;;;;;
#|
(define-handler selectMovieAlternates (QuickTimeMovie)
	(#_SelectMovieAlternates (mediaData me))
	(checkMovieError))

(define-handler (setf autoTrackAlternatesEnabled) (boolean QuickTimeMovie)
	(#_SetAutoTrackAlternatesEnabled (mediaData me) boolean))
|#

;;;;;;;;;
;;; Poster and Preview
;;;;;;;;;

(define-handler posterTime (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMoviePosterTime myMovieGCOSPtr)))

(define-handler (setf posterTime) (newPosterTime
                                             QuickTimeMovie
                                             &key dontForwardToRenderer)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (ifForwardToRenderer
     (setf (posterTime myRenderer) newPosterTime)
     (progn
       (unless newPosterTime
         (setf newPosterTime (timeValue me)))
       (T_SetMoviePosterTime myMovieGCOSPtr newPosterTime)
       (T_GetMoviePosterTime myMovieGCOSPtr)))))

(define-handler posterBox :private (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (let ((theBox (T_GetPosterBoxGC myMovieGCOSPtr)))
      (SK8-multivals (href theBox :rect.left)
                     (href theBox :rect.top)
                     (href theBox :rect.right)
                     (href theBox :rect.bottom)))))

(define-handler (setf posterBox) :private (newPosterBox QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetPosterBox myMovieGCOSPtr
                    :left   (round (first  newPosterBox))
                    :top    (round (second newPosterBox))
                    :right  (round (third  newPosterBox))
                    :bottom (round (fourth newPosterBox)))))


;;; the QuickTime nomenclature for this is PreviewTime
(define-handler previewSegment (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMoviePreviewTime myMovieGCOSPtr)))

;;; the QuickTime nomenclature for this is PreviewTime
(define-handler (setf previewSegment) (segment QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMoviePreviewTime myMovieGCOSPtr
                           :startTime (first segment)
                           :duration (second segment))
    (notifyRendererOfChange me)
    (T_GetMoviePreviewTime myMovieGCOSPtr)))

(define-handler previewMode (QuickTimeMovie)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMoviePreviewMode myMovieGCOSPtr)))

(define-handler (setf previewMode) (newPreviewMode QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (T_SetMoviePreviewMode myMovieGCOSPtr newPreviewMode)
    (notifyRendererOfChange me))
  newPreviewMode)

(define-handler (setf progressProc) (newValue QuickTimeMovie)
  (assumeHandleWithError (me myMovieGCOSPtr)
    (raiseUnsettablePropertyError 'progressProc me)))

#|
(define-handler posterPict (QuickTimeMovie)
  (with-movie-active (me)
    (T_GetMoviePosterPictGC ((mediaData me) me))))
|#

;;;;;;;;
;;; PICT
;;;;;;;;

#|
(define-handler makeFilePreview (QuickTimeMovie)
  (unless (file me)
    (error "This movie is not associated with a file"))
    (if (resFileRefNum me)
      (T_MakeFilePreview (resFileRefNum me) Wont work this way (progressProc me))
      (progn
        (setf resFileRefNum (cl-user::open-resource-file (physicalname (file me)))) ;? does this raise error?
        (unwind-protect
          (T_MakeFilePreview resFileRefNum Wont work this way (progressProc me))
          (cl-user::close-file resFileRefNum))))) ;? what to use here?
|#

#|
(define-handler addFilePreview (QuickTimeMovie ffile &optional (tim 0))
  (let ((resFileRefNum (cl-user::open-resource-file (physicalname ffile))))
    ;; OJO! do this with file handlers.
    (when resFileRefNum
      (#_AddFilePreview resFileRefNum "PICT" (moviePictGCHandle me :timeValue tim))
      (checkMovieError)
      (#_CloseResFile resFileRefNum))))
|#

(define-handler moviePictGCHandle :private (QuickTimeMovie &key timeValue)
  (or (and (moviePictTimeValue me)
           (eql (moviePictTimeValue me) (timeValue me))
           (getValue 'moviePictGCHandle me))
      (progn
        (assumeHandle (me myMovieGCOSPtr)
          (setf (moviePictTimeValue me) (timeValue me))
          (setValue 'moviePictGCHandle me
                    (T_GetMoviePictGC myMovieGCOSPtr :timeValue timeValue))))))

(define-handler posterPictGCHandle :private (QuickTimeMovie &key)
  (assumeHandle (me myMovieGCOSPtr)
    (T_GetMoviePosterPictGC myMovieGCOSPtr)))

(define-handler moviePicture (QuickTimeMovie &key timeValue)
  (let ((handle (moviePictGCHandle me :timeValue timeValue)))
    (new QDPicture :handle handle :project (project me))))

(define-handler posterPicture (QuickTimeMovie)
  (let ((handle (posterPictGCHandle me)))
    (new QDPicture :handle handle :project (project me))))

;;;;;;;;;;;;;;;
;;; Saving Movies
;;;;;;;;;;;;;;;

(define-handler save (QuickTimeMovie
                       &key
                       toFile
                       resourceID
                       #+DataForkMovieSave toDataFork
                       )
  (let ((fileTarget (or toFile (file me))))
    (unless fileTarget
      (sk8-error GeneralProgrammaticError
                 :strings '("Movie " " is not associated with a file, so you must specify one when saving.")
                 :objects (list me)))
    (when (eq fileTarget t)
      ;;? not implemented yet
      (sk8-error GeneralProgrammaticError
                 :strings '("SK8 doesn't yet know how to use a save dialog for saving a movie to another file.")))
    (if (eq fileTarget
            (file me))
      ;; Save to the same file
      (let ((resFileRefNum (or (resFileRefNum me)
                               (T_OpenMovieFile (appleFileSpecGCHandle fileTarget) :permissions TC_fsWrPerm))))
        (unwind-protect
          (if (movieNumber me)
            ;; in the data fork
            (T_UpdateMovieResource (mediaData me) resFileRefNum TC_movieInDataForkResID)
            ;; in the resource fork
            (if (resourceID me)
              ;; to existing resource
              (T_UpdateMovieResource (mediaData me) resFileRefNum (resourceID me))
              ;; to a new resource
              (setf (resourceID me)
                    (T_AddMovieResource (mediaData me) resFileRefNum :resourceID (or resourceID 0)))))
          (unless (eq resFileRefNum (resFileRefNum me))
            (T_CloseMovieFile resFileRefNum))))
      ;; Save to some other file.
      (when (eq fileTarget t)
        ;;? not implemented yet
        (sk8-error GeneralProgrammaticError
                   :strings '("SK8 doesn't yet know how to save a movie to another file.")))
      #+DataForkMovieSave
      (let ((resFileRefNum (T_OpenMovieFile (appleFileSpecGCHandle fileTarget) :permissions TC_fsWrPerm)))
        (unwind-protect
          (if toDataFork
            ;; in the data fork
            (T_UpdateMovieResource (mediaData me) resFileRefNum TC_movieInDataForkResID)
            ;; in the resource fork
            (if  resourceID already exists
                 ;; to existing resource
                 (T_UpdateMovieResource (mediaData me) resFileRefNum resourceID)
                 ;; to a new resource
                 return this: (T_AddMovieResource (mediaData me) resFileRefNum :resourceID (or resourceID 0)))))
        (unless (eq resFileRefNum (resFileRefNum me))
          (T_CloseMovieFile resFileRefNum)))
      ))
  (sk8-return-nothing))

#|
(define-handler changed (QuickTimeMovie)
  (T_HasMovieChanged (mediaData me)))

(define-handler (setf changed) (newValue QuickTimeMovie)
  (if changed
    (error "you can only set changed to false")
    (T_ClearMovieChanged (mediaData me)))
  newValue)

(define-handler flatten (QuickTimeMovie ffile &key
                                  (name "")
                                  (id 0))
  (assumeHandleWithError (me myMovieGCOSPtr)
  (unless ffile (unless (setq ffile (file me))
                  (error "Movie ~s is no associated with a file")))
  (rlet ((resID :integer id))
    (with-pstrs ((resName name))
      (with-dereferenced-handles ((fileSpecPtr (appleFileSpecGCHandle ffile)))
        (#_FlattenMovie
         myMovieGCOSPtr
         (+ (if (not dataFork)     0 #$flattenAddMovieToDataFork)
            (if optimize           0 #$flattenDontInterleaveFlatten)
            (if (not activeTracks) 0 #$flattenActiveTracksOnly))
         fileSpecPtr
         "TVOD"
         0
         (if createNewFile #$createMovieFileDeleteCurFile 0)
         resID
         resName)))
    (checkMovieError))))
|#
;;; -----------------------------------
;;; PRESERVING AND RESTORING MOVIES...
;;; -----------------------------------

(define-handler preserve :private (QuickTimeMovie)
  (call-next-method)
  (assumeHandle (me mediaData)
    (T_StopMovie mediaData)) ;? To be safe, but it might be too late.
  ;; (preserveLikeSave me)
  (sk8-return-nothing))

(define-handler localPropertiesToSaveAsFalse (QuickTimeMovie)
  (and (eq me QuickTimeMovie)
       '(mediaData
         resFileRefNumGCHandle
         moviePictGCHandle
         moviePictTimeValue
         dataReferenceWasChanged
         couldNotResolveDataReference
         suppressingVisualTracks
         preferredAspectRatio
         progressProc
         movieBox
         preferredSize
         preferredScale
         )))

(define-handler localPropertiesToSaveSpecially (QuickTimeMovie)
  (and (eq me QuickTimeMovie)
       '(file)))

(define-handler saveToStore :private (QuickTimeMovie property)
  (case property
    (file
     (if (and (not (file me))
              (mediaData me)
              (not (zerop (T_GetMovieTrackCount (mediaData me))))
              (progn
                (sendToLog (format nil "The contents of QuickTimeMovie ~A can't be saved in the project in this version of SK8 because it was not created from a file." me))
                t))
       nil
       (call-next-method)))
    (otherwise
     (call-next-method))))

(unless (boundp 'sk8dev::*movies-with-bogus-file*)
  (defvar        sk8dev::*movies-with-bogus-file* nil))

(defun initializeMovie (me)
  (if (file me)
    (if (or (resourceName me)
            (resourceID   me)
            (eql (movieNumber me) 1))
      (trace-print-bracketing (`("initializeMovie file ~S" ,(file me)) :debug-initialize)
        (initializeMovieFromResourceFile
         me
         (new ObjectTable
              :project (project me)
              :data `((file . ,(file me))
                      ,(cond
                        ((resourceName me) `(resourceName . ,(resourceName me)))
                        ((resourceID   me) `(resourceID   . ,(resourceID   me)))
                        ((movieNumber  me) `(movieNumber  . ,(movieNumber  me))))))))
      (initializeMovieFromDataFile
       me
       (new ObjectTable
            :project (project me)
            :data `((file . ,(file me))
                    ,(if (movieNumber me)
                       `(movieNumber . ,(movieNumber me))
                       `(fileOffset  . ,(fileOffset  me)))))))
    (initializeMovieFromNothing me '()))
  (sk8-return-nothing))

(defun helpInitializeMovieTime (me whichTime whichFunction)
  (let ((theTime (getValue whichTime me)))
    (when theTime
      (funcall whichFunction theTime) ; We assume that initializeFromStore and restore of a DateTimeIndirect will always succeed.
      (setf (getterArguments theTime) (list (mediaData me))))))

(defun qtmStoreFinalizeInitialization (me whichFunction)
  (qtmFinalizeInitialization me t)
  (let ((timeBase (getValue 'timeBase me))
        (myRenderer (renderer me)))
    (trace-print (:debug-initialize) "timebase, renderer ~S ~S" timebase myRenderer)
    (when timeBase   (funcall whichFunction timeBase))
    (when myRenderer (funcall whichFunction myRenderer)))
  (helpInitializeMovieTime me     'creationTime whichFunction)
  (helpInitializeMovieTime me 'modificationTime whichFunction))

;;; Returns nil if not initialized.
(define-handler restoreOrInitializeFromStore :private (QuickTimeMovie whichFunction)
  ;; Reconstitute the Movie macptr
  (or (mediaData me) ; already initialized
      (eq me QuickTimeMovie) ; doesn't need initialization
      (ignore-errors-but-sendToLog
        (requireQuickTimeInitialized)
        (setf (preferredSize me) nil) ;? backward compatibility.  Can remove for GM
        (block setMovieHandle
          (if (file me)
            (progn
              (dolist (movie *movies-with-bogus-file*)
                (when (eq me movie)
                  (return-from setMovieHandle nil)))
              (funcall whichfunction (file me)) ; Assume this will succeed.
              (or (ignore-errors-but-sendToLog
                    (initializeMovie me)
                    t)
                  (progn
                    (push me *movies-with-bogus-file*)
                    (return-from setMovieHandle nil))))
            (progn
              (sendToLog (format nil "Creating an empty QuickTimeMovie for ~a, which has no file" me))
              (initializeMovie me))) ; Give 'em an empty movie
          (qtmStoreFinalizeInitialization me whichFunction)
          t))))

(define-handler restore :private (QuickTimeMovie)
  (trace-print-bracketing ("initialize movie" :debug-initialize)
    (and (call-next-method)
         (restoreOrInitializeFromStore me #'restore))))

(define-handler initializeFromStore :private (QuickTimeMovie)
  (trace-print-bracketing ("initialize movie" :debug-initialize)
    (and (call-next-method)
         (restoreOrInitializeFromStore me #'initializeFromStore))))

(defun finalizeQuickTimeMovies (whichFunction)
  (trace-print-bracketing ("initialize bogus movies" :debug-initialize)
    ;; We need a hash table because more than one reference to the same
    ;; movie may have been put onto the *movies-with-bogus-file* queue.
    (let ((relocatedMovieFiles (make-hash-table)))
      (dolist (movie *movies-with-bogus-file*)
        (trace-print-bracketing ("initialize a bogus movie" :debug-initialize)
          (ignore-errors-but-sendToLog
            (if (or (gethash (file movie) relocatedMovieFiles)
                    (ignore-errors
                     (message-dialog (format nil "Movie file \"~a\" was not found!" (Name (file movie))) :ok-text "Find it!")
                     (copyRealProperties (getFileFromUserWithPreview (project movie)) (file movie))
                     (setf (gethash (file movie) relocatedMovieFiles) t)))
              (progn
                (when (or (resourceID movie)
                          (resourceName movie)
                          (eq (movieNumber movie) 1))
                  ;; We'll take the first movie we find in the substitute file.
                  (setf (resourceID movie) 0)
                  (setf (resourceName movie) nil)
                  (setf (movieNumber  movie) nil)))
              (sendToLog (format nil "Movie file not found for ~a; creating an empty movie" movie)))
            (initializeMovie movie)
            (qtmStoreFinalizeInitialization movie whichFunction)))))
    (setf *movies-with-bogus-file* '())))

(define-handler localVirtualProperties (QuickTimeMovie)
  (and (eq me QuickTimeMovie)
       '(height
         width
         dataSize
         repeating
         timeValue
         active
         activeSegment
         ; preferredSize already a property
         matrix
         movieBox
         scale
         selection
         preferredRate
         rate
         timeScale
         duration
         preferredVolume
         volume
         posterTime
         posterBox
         previewSegment
         previewMode
        clipRegionGCHandle
        displayClipRegionGCHandle
        restype
          )))

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	2/26/94	kleiman	fixed uses of the symbol 'project
	3	2/26/94	kleiman	comment out restore-or-initialize for now
	4	2/26/94	kleiman	unbalanced paren(ts
	5	3/8/94	dy	add property couldNotResolveDataReference, other fixes
	6	3/11/94	dy	:privates plus various changes
	7	3/12/94	dy	various minor fixes before I leave for LA
	8	3/22/94	dy	various fixes
	9	3/24/94	dy	move repeating virtual property from QTSP to here
	10	3/28/94	Hernan	AddingMeAsParent -> addedMeAsParent.
	11	4/1/94	dy	?
	12	4/7/94	dy	fix new quicktime movie with file true
	13	4/11/94	dy	startTime, duration -> segment
	14	4/13/94	sidney	Add parameters to addedMeAsParent and removingMeAsParent to allow handlers to avoid work when an ancestor is added back in after a remove
	15	4/19/94	dy	Poster & Preview handling
	16	4/20/94	dy	new QTmovie when parent was made from a file
	17	4/28/94	dy	posterPicture and moviePicture
	18	5/2/94	dy	clean up load warning on *movies-to-initialize*
	19	5/2/94	dy	harmless typos
	20	5/6/94	dy	add 'repeating' to localVirtualProperties
	21	5/31/94	yost	various fixes, move set/get matrix here from QTSP
	22	5/31/94	yost	move previewMode from QTSP to QTM
	23	6/3/94	yost	add to localVirtualProperties
	24	6/8/94	yost	defer various handlers to my player
	25	6/12/94	dy	working on Bug #1159303 - can't save and restore movies
	26	6/12/94	dy	more save-restore work
	27	6/12/94	kleiman	
	28	6/13/94	dy	rearrange save-restore project code
	29	6/14/94	dy	fixes from code reading; new movie with file foo cleanup
	30	6/16/94	dy	fix new quicktimemovie with file true
	31	6/17/94	dy	Fix project store/load
	32	6/17/94	kleiman	QuickTimeMovie's parent is Media
	33	6/20/94	dy	working on ing project
	34	6/20/94	dy	fix assumeMovieGCOSPtr macro
	35	6/22/94	dy	addproperty tracks
	36	6/30/94	dy	tracks property
	37	7/11/94	Hernan	Removing the resourceId property since the movie
				will inherit it from Media. Also adding the restype
				handler for Movie.
	38	7/11/94	Hernan	Removing the file property also.
	39	7/19/94	dy	QT Renderer changes
	40	7/21/94	dy	Fix 1174332
	41	8/7/94	dy	make movieBox property always a list
	42	8/11/94	dy	documentation
	43	8/11/94	dy	remove enabledVisualTracks
	44	8/11/94	dy	invalidate tracks when approprate
	45 	 8/22/94	dy      	call-next-method in initialize, gotoBeginning&End
	46 	 8/24/94	dy      	gotoX moved here, localVirtProps to end
	47 	 8/24/94	dy      	balance parens
	48 	 9/ 1/94	dy      	Fix 1183789
	49 	 9/ 2/94	dy      	Fix bug 1182766
	50 	 9/ 6/94	dy      	Fix object editor bugs; do-flag-arg -> trap-flag-arg
	51 	 9/ 6/94	dy      	remove "set file" while trying to fix importing
	52 	 9/ 9/94	dy      	import fixes
	53 	 9/14/94	dy      	prevent file from being set to True
	54 	 9/15/94	dy      	movieNumber property and related changes
	55 	 9/19/94	dy      	RequireMoviesInitialized -> requireQuickTimeInitialized
	56 	 9/21/94	dy      	modify saveToStore to falsify some more properties
	57 	 9/21/94	dy      	modify saveToStore to falsify some more properties
	58 	 9/23/94	dy      	make aspectRatio work
	59 	 9/23/94	dy      	add height & width virtual properties
	60 	 9/27/94	dy      	NoValuexxx -> *undefined*; assumeHandleWithError introduced
	61 	 9/29/94	dy      	
	62 	 9/29/94	dy      	unbreak set timeValue, etc.
	63 	 9/30/94	dy      	return *undefined* when no useful return value instead of (sk8-multivals)
	64 	 9/30/94	dy      	return *undefined* when no useful return value instead of (sk8-multivals)
	65 	10/ 3/94	dy      	handle -> mediaData, sk8-return-nothing, other changes
	66 	10/ 5/94	dy      	
	67 	10/ 5/94	dy      	new initialization
	68 	10/ 5/94	dy      	SK8-declare-syms of option args
	69 	10/ 6/94	dy      	move public symbols to earlier in the build
	70 	10/ 6/94	dy      	handle -> mediaData problem
	71 	10/ 6/94	dy      	resizing fixes
	72 	10/ 7/94	dy      	change args to SK8-multivals-sequence
	73 	10/ 7/94	dy      	preferredSize fixes
	74 	10/17/94	dy      	fix resizing stuff & possibly other changes
	75 	10/19/94	dy      	no setf matrix, replaced by setf scale
	76 	10/19/94	dy      	remove debug printout
	77 	10/28/94	dy      	new property preferredScale, no more parenting
	78 	11/ 4/94	dy      	
	79 	11/14/94	dy      	fix initializerArgument bugs (affecting import and initializing from store);  support propertiesToSaveSpecially and propertiesToSaveAsFalse
	80 	11/19/94	dy      	fix typo that caused movies in projects to not load
	81 	11/22/94	dy      	add movieBox to localVirtualProperties
	82 	12/19/94	dy      	Use count-if-not instead of other hack
	83 	 1/25/95	dy      	make "with create" work without crashing
	84 	 2/16/95	sidney  	readable argument names for initialize handler
	85 	 2/28/95	dy      	MovieController!
	86 	 3/ 6/95	dy      	timeBase changes
	87 	 3/ 6/95	dy      	error if set preferredRate to 0
	88 	 3/ 8/95	dy      	saveToStore fixes
	89 	 3/10/95	dy      	set masterTimeBase and set masterClock now handled by TimeBase object.  Fixes for store?
	90 	 3/11/95	dy      	tweak to sk8dev::*movies-with-bogus-file* defvar
	91 	 3/13/95	dy      	introduce creationTime and modificationTime
	92 	 3/13/95	dy      	indirectFunction -> getterFunction
	93 	 3/14/95	dy      	add calls to notifyRendererOfChange where appropriate.
	94 	 3/16/95	rod     	fileSpecGCHandle -> appleFileSpecGCHandle
	95 	 3/21/95	dy      	try to fix store
	96 	 3/24/95	dy      	fix loading from store when movie not found
	97 	 3/28/95	dy      	Try to fix preserve/restore.
	98 	 3/29/95	dy      	preserve looks for mediaData instead of movieGCOSPtr or whatever it was doing.
							finalizeQuickTimeMovies takes a new argument.
	99 	 3/29/95	dy      	(setf (preferredSize me) nil) in restoreOrInitializeFromStore to be safe with recently made projects
	100	 3/31/95	dy      	remove tracks from localPropertiesToSaveAsFalse
	101	 4/ 3/95	dy      	Forgot something in restoreOrInitializeFromStore
	102	 4/ 3/95	dy      	In restoreOrInitializeFromStore, funcall whichfunction instead of calling initializeFromStore (duh)
	103	 4/18/95	dy      	Fix 1238194.  Correct the rehydration of DateTime objects referenced by modificationTime and creationTime properties.
	104	 4/19/95	rod     	Fixing importFromResource and 
							importFromDataFork to use findFileInProject
							to make the right file object.
	105	 5/ 1/95	dy      	modificationTime was erroneously showing creationTime
	2  	 6/ 9/95	dy      	rename getMovieFileFromUser -> getFileFromUserWithPreview
	3  	 6/12/95	dy      	minor fixes from codereading
	4  	 9/29/95	dy      	New save handler, not documented yet
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	5  	 5/ 7/96	sidney  	Changes for new object system
	6  	 5/ 7/96	Hernan  	Fixing undefined function warnings.
	7  	 9/23/96	Hernan  	Synching with new track collection.
	8  	11/26/96	Hernan  	Adding ignore-errors-but-sendtolog.
	9  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
