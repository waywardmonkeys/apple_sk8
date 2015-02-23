(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(addProperty QuickTimeTrack 'movie     ) ; QuickTimeMovie to which the track belongs
(addProperty QuickTimeTrack 'trackOSPtr) ; the macptr - it is owned by the movie object
(addProperty QuickTimeTrack 'media     ) ; QuickTimeMedia owned by this track
(addProperty QuickTimeTrack 'id        ) ; The ID of the track.  Need this to be sure the track still exists
(addProperty QuickTimeTrack 'enabled   ) ; Official enabled status

(define-handler localVirtualProperties (QuickTimeTrack)
  (and (eq me QuickTimeTrack)
       #+QTEdit
       '(dataSize
         duration
         offset
         datasize
         volume
         layer
         matrix)
       #-QTEdit
       '(reallyEnabled)))

(defmacro assumeTrackOSPtr ((me osPtrName) &body body)
  `(let ((,osPtrName (trackOSPtr ,me)))
     (if ,osPtrName
       (progn ,@body)
       *undefined*)))

(defmacro assumeTrackOSPtrWithError ((me osPtrName) &body body)
  `(let ((,osPtrName (trackOSPtr ,me)))
     (if ,osPtrName
       (progn ,@body)
       (sk8-error GeneralProgrammaticError
                  :strings '("Track " " is not associated with a movie")
                  :objects (list ,me)))))


;;; Since these "act like properties" during initialization, but aren't really properties,
;;; they must be declared explicitly
;;;
(SK8-declare-syms :SK8 :public SK8::mediaType SK8::dataReference SK8::dataReferenceType)


;;; A new QuickTimeTrack is created only from the getter of baseCollection of QuickTimeTrackCollection.
(define-handler initialize :private (QuickTimeTrack original isNew initArgs)
  (declare (ignore original isNew))
  (let ((movie                (initializerArgument initargs 'movie         :use nil))
        (trackOSPtr		(initializerArgument initargs 'trackOSPtr    :use nil))
        (width			(initializerArgument initargs 'width         :default *undefined*))
        (height			(initializerArgument initargs 'height        :default *undefined*))
        (volume			(initializerArgument initargs 'volume        :default *undefined*))
        (mediaType		(initializerArgument initargs 'mediaType     :default 'video))
        (timeScale		(initializerArgument initargs 'timeScale     :default *undefined*))
        (dataReference	(initializerArgument initargs 'dataReference :default *undefined*))
        (dataReferenceType	(initializerArgument initargs 'dataReferenceType)))
    
    (unless movie
      (sk8-error GeneralProgrammaticError
                 :strings '("not enough initialzers")))
    (call-next-method)

    (if trackOSPtr
      (if (or (neq width *undefined*) (neq height *undefined*) (neq volume *undefined*))
        (sk8-error IncorrectArgumentsError
                   :handlername 'initialize
                   :arguments (append (list :trackOSPtr trackOSPtr)
                                      (when (neq width  *undefined*)    (list :width   width))
                                      (when (neq height *undefined*)    (list :height height))
                                      (when (neq volume *undefined*)    (list :volume volume)))))
      
      (setf trackOSPtr (apply #'T_NewMovieTrack (mediaData movie)
                              (append (if (neq width  *undefined*) (list :width  width)  '())
                                      (if (neq height *undefined*) (list :height height) '())
                                      (if (neq volume *undefined*) (list :volume volume) '())))))

    
    (setValue 'id me (T_GetTrackID trackOSPtr))
    (setValue 'enabled me (T_GetTrackEnabled trackOSPtr))

    ;; See if the track has a media associated with it.
    ;; If not, then create one.
    ;; We don't allow a Track to exist without a corresponding Media
    (let ((mediaOSPtr (T_GetTrackMedia trackOSPtr :noErrorIfNone t)))
      (unless mediaOSPtr
        (when (eq timeScale *undefined*)
          (setf timeScale (timeScale movie)))
        (when (eq dataReference *undefined*)
          (if (file movie)
            (progn
              ;; (setf dataReference (aliasGCHandle (file movie)))
              (setf dataReferenceType "alis"))
            (progn
              (setf dataReference TC_nil)
              (setf dataReferenceType TC_nil))))
        (setf mediaOSPtr (T_NewTrackMedia
                          trackOSPtr
                          mediaType
                          timeScale
                          dataReference
                          dataReferenceType)))
      (setValue 'media me
                (if mediaOSPtr
                  (new QuickTimeMedia
                       :mediaOSPtr mediaOSPtr
                       :movie movie
                       :track me
                       :project (project me))
                  (sk8-return-nothing))))))

;;; Don't use this to enable a track.  Use set enabled.
(define-handler (setf reallyEnabled) :private (enabled QuickTimeTrack)
  (assumeTrackOSPtrWithError (me myTrackOSPtr)
    (T_SetTrackEnabled myTrackOSPtr enabled))
  enabled)

(define-handler reallyEnabled (QuickTimeTrack)
  (assumeTrackOSPtr (me myTrackOSPtr)
    (T_GetTrackEnabled myTrackOSPtr)))

(define-handler (setf enabled) (enabled QuickTimeTrack)
  (assumeTrackOSPtrWithError (me myTrackOSPtr)
    (when (xor enabled (enabled me))
      (setValue 'enabled me enabled)
      (if (isVisual (media me))
        (progn
          (invalidatePreferredSize (movie me))
          (unless (and enabled
                       (suppressingVisualTracks (movie me)))
            (setf (reallyEnabled me) enabled)))
        (setf (reallyEnabled me) enabled)))))

#+QTEdit
(progn
;;; virtual property
;;; dataSize - returns the size in bytes of a segment of the track
;;;      start <-> start time (default: beginning of track)
;;;      duration <-> duration (default: duration of track)
;;;
(define-handler dataSize (QuickTimeTrack &key segment)
  (assumeTrackOSPtr (me myTrackOSPtr)
    (T_GetTrackDataSize myTrackOSPtr :startTime (first segment) :duration (second segment))))

;;; virtual property
(define-handler duration (QuickTimeTrack)
  (assumeTrackOSPtr (me myTrackOSPtr)
    (T_GetTrackDuration myTrackOSPtr)))

(define-handler (setf offset) (newOffset QuickTimeTrack)
  (T_SetTrackOffset (trackOSPtr me) newOffset)
  newOffset)

(define-handler offset (QuickTimeTrack)
  (assumeTrackOSPtr (me myTrackOSPtr)
    (T_GetTrackOffset myTrackOSPtr)))


;;;;;;;;;;
;;; SOUND
;;;;;;;;;;

(define-handler (setf volume) (volume QuickTimeTrack)
  (T_SetTrackVolume (trackOSPtr me) volume)
  volume)

(define-handler volume (QuickTimeTrack)
  (assumeTrackOSPtr (me myTrackOSPtr)
    (T_GetTrackVolume myTrackOSPtr)))


(define-handler (setf layer) (layer QuickTimeTrack)
  (T_SetTrackLayer (trackOSPtr me) layer))

(define-handler layer (QuickTimeTrack)
  (assumeTrackOSPtr (me myTrackOSPtr)
    (T_SetTrackLayer myTrackOSPtr me)))


(define-handler (setf matrix) (newMatrix QuickTimeTrack)
  (cond
   ((zone-pointerp newMatrix)
    (T_SetTrackMatrix (trackOSPtr me) newMatrix))
   ((or (eql newMatrix nil)
        (eql newMatrix TC_nil))
    (T_SetTrackMatrix (trackOSPtr me) TC_nil))
   (t
    (T_SetTrackMatrixFromArray (trackOSPtr me) newMatrix)))
  (T_GetTrackMatrixArray (trackOSPtr me)))

(define-handler matrix (QuickTimeTrack)
  (assumeTrackOSPtr (me myTrackOSPtr)
    (T_GetTrackMatrixArray myTrackOSPtr)))
)

#|========================================

really old stuff

;;;;;;;;;;;;;;;;;;
;;; Alternate Tracks
;;;;;;;;;;;;;;;;;;

#|
allTrackAlternates - returns all alternates for a track actor in a movie

|#

(define-handler (setf trackAlternate) (theMovieTrack QuickTimeTrack)
  (with-track-identifier (identifier self)
    (with-track-identifier (identifier1 theMovieTrack)
      (#_settrackalternate identifier identifier1)
      (checkMovieError))))

(define-handler trackAlternate (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackalternate identifier)))

(define-handler allTrackAlternates (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (let ((identifiers nil)
          (tracks nil))
      (loop
        (pushnew (#_gettrackalternate identifier) identifiers)
        (if (%ptr-eql (car identifiers) identifier)
          (return nil)
          (setq identifier (car identifiers))))
      (dolist (theTrack (tracks (movie self)) tracks)
        (when (member (identifier theTrack) identifiers :test #'%ptr-eql)
          (push theTrack tracks))))))
  
#|
#| Movie Tracks Design

findNextEmptyMediaTime - returns the time at which the track media is empty, given some search parameters

;;; Creating new track actors
;;;   NOTE: track actors are created on demand whenever you ask for them via
;;;        the getMovieTrackFromIndex or getMovieTrackFromID handlers.
;;;        You should call makeNewMovieTrack only when you want to create a new track.
;;;
makeNewMovieTrack [movie] - returns a new track actor for the movie (never use new [movieTrack])

|#


(eval-when (compile eval)
  (defmacro with-track-identifier ((identifier theMovieTrack) &body body)
    `(let ((,identifier (identifier ,theMovieTrack)))
       (when ,identifier
         ,@body))))


;;;;;;;;;;;;;;;;;;;;;;
;;; CREATING AND EDITING
;;;;;;;;;;;;;;;;;;;;;;

;;; newMovieTrack - returns a new movie track for the movie
;;;       width - the width of the track (if it's not displayed, make it 0); default: the movie's current width
;;;       height - the height of the track (if it's not displayed, make it 0); default: the movie's current height
;;;       volume- the volume for the track; default: full volume
;;;

(define-handler makeNewMovieTrack (QuickTimeMovie &key
                                                   (width (width self))
                                                   (height (height self))
                                                   (mediaType #$videoMediaType)
                                                   (mediaDataRef nil)
                                                   (mediaDataRefType nil)
                                                   (mediaTimeScale nil)
                                                   (volume (if (eq mediaType #$videoMediaType)
                                                             #$kNoVolume
                                                             #$kFullVolume)))
  (declare (ignore mediaTimeScale mediaDataRefType mediadataRef))
  (with-movie-handle (mediaData self)
    (let ((identifier (#_newmovietrack handle width height volume))
          (theTrack nil))
      (checkMovieError)
      (setf theTrack (new QuickTimeTrack :inProject (project self))
            (identifier theTrack) identifier
            (movie theTrack) self)
      (pushNew theTrack (tracks self))
      (new QuickTimeMedia :inProject (project self)
           :properties `((track :value ,theTrack)
                         (mediaType :value ,mediaType)))
      theTrack)))

#|
;;; dispose - disposes of the movie track
;;;

(define-handler dispose (QuickTimeTrack &key)
  (unless (mf::undisposable-object? self)
    (with-track-identifier (identifier self)
      (#_disposemovietrack identifier)
      (checkMovieError))
    (let ((theMedium (media self)))
      (when (and theMedium (not (disposed theMedium)))
        (dispose theMedium)))
    (call-next-method)))
  |#



;;; trackTimeToMediaTime - returns the media time corresponding to the track time
;;;
(define-handler trackTimeToMediaTime (QuickTimeTrack timeValue)
  (with-track-identifier (identifier self)
    (#_tracktimetomediatime timeValue identifier)))

(define-handler trackNextInterestingTime (QuickTimeTrack)
  (error "not implemented"))

;;; creationTime - time track was created
;;;
(define-handler creationTime (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackcreationtime identifier)))

;;; modificationTime - time track was last modified
;;;
(define-handler modificationTime (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackmodificationtime identifier)))

;;; findNextEmptyMediaTime - finds the first time where the media is empty
;;;     start <-> start time for search
;;;
(define-handler findNextEmptyMediaTime (QuickTimeTrack &key (start 0))
  (let ((max (duration self))
        (identifier (identifier self)))
    (do ((timeValue start (1+ timeValue)))
        ((> timeValue max) nil)
      (when (eql -1 (#_tracktimetomediatime timeValue identifier))
        (return timeValue)))))


;;;;;;;;;;;
;;; EDITING
;;;;;;;;;;;

;;; call beginMediaEdits on destination media before using this!
(define-handler insertSegment (QuickTimeTrack theTrack theTime duration destTime)
  (with-track-identifier (identifier self)
    (with-track-identifier (identifier1 theTrack)
      (#_inserttracksegment identifier identifier1 theTime duration destTime)
      (checkMovieError))))

(define-handler insertEmptySegment (QuickTimeTrack theTime duration)
  (with-track-identifier (identifier self)
    (#_insertemptytracksegment identifier theTime duration)
    (checkMovieError)))

;;; be sure to add samples to media first!
(define-handler insertMedia (QuickTimeTrack &key (start -1) ; default: beginning
                                                 (theTime 0) (duration 0)
                                                 (rate 1.0))
  (with-track-identifier (identifier self)
    (#_insertmediaintotrack identifier start theTime duration rate)
    (checkMovieError)))

(define-handler deleteSegment (QuickTimeTrack theTime duration)
  (with-track-identifier (identifier self)
    (#_deletetracksegment identifier theTime duration)
    (checkMovieError)))

(define-handler scaleSegment (QuickTimeTrack theTime oldDuration duration)
  (with-track-identifier (identifier self)
    (#_scaletracksegment identifier theTime oldduration duration)
    (checkMovieError)))

(define-handler copySettings (QuickTimeTrack theTrack)
  (with-track-identifier (identifier self)
    (with-track-identifier (identifier1 theTrack)
      (#_copytracksettings identifier identifier1)
      (checkMovieError))))

(define-handler newEditState (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (prog1 (#_newtrackeditstate identifier)
      (checkMovieError))))

(define-handler useEditState (QuickTimeTrack state)
  (with-track-identifier (identifier self)
    (prog1 (#_usetrackeditstate identifier state)
      (checkMovieError))))

(defun disposeTrackEditState (state)
  (#_disposeTrackEditState state)
  (checkMovieError))


;;;;;;;;;
;;; PICTS
;;;;;;;;;

(define-handler trackPict (QuickTimeTrack theTime)
  (with-track-identifier (identifier self)
    (prog1 (#_gettrackpict identifier theTime)
      (checkMovieError))))

;;;;;;;;;;;;
;;; User Data
;;;;;;;;;;;;

(define-handler userData (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackuserdata identifier)))

;;;;;;;;;;;
;;; DRAWING
;;;;;;;;;;;

(define-handler trackDisplayBoundsRegion (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (prog1 (#_gettrackdisplayboundsrgn identifier)
      (checkMovieError))))

(define-handler trackSegmentDisplayBoundsRegion (QuickTimeTrack theTime duration)
  (with-track-identifier (identifier self)
    (prog1 (#_gettracksegmentdisplayboundsrgn identifier theTime duration)
      (checkMovieError))))

(define-handler trackMovieBoundsRgn (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackmovieboundsrgn identifier)))

(define-handler trackBoundsRegion (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackboundsrgn identifier)))

(define-handler setTrackDimensions (QuickTimeTrack width height)
  (with-track-identifier (identifier self)
    (#_settrackdimensions identifier width height)
    (checkMovieError)))

(define-handler trackDimensions (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (%stack-block ((width 4) (height 4))
      (#_gettrackdimensions identifier width height)
      (SK8-multivals (%get-long width) (%get-long height)))))

(define-handler (setf trackClipRegion) (region QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_settrackcliprgn identifier region)
    (checkMovieError)))

(define-handler trackClipRegion (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackcliprgn identifier)))

(define-handler (setf trackMatte) (pixmap QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_settrackmatte identifier pixmap)
    (checkMovieError)))

(define-handler trackMatte (QuickTimeTrack)
  (with-track-identifier (identifier self)
    (#_gettrackmatte identifier)))

;;; WARNING: Use this ONLY to dispose of mattes returned by trackMatte!
;;;
(defun disposeMatte (pixmap)
  (#_disposematte pixmap))


;;;;;;;;;;;;;;;;;;;;;
;;; LOW-LEVEL HANDLERS
;;;;;;;;;;;;;;;;;;;;;

;;; installTrackActor - used to install track actors for an existing movie. Supply either the track index or ID.
;;;     trackIndex <-> the track index
;;;     trackID <-> the track ID
;;;
(define-handler installTrackActor (QuickTimeMovie &key trackIndex trackID)
  (with-movie-handle (mediaData self)
    (let ((identifier (if trackIndex
                        (#_getmovieindtrack handle trackIndex)
                        (if trackID
                          (#_getmovietrack handle trackID)
                          (error "Missing track index or ID"))))
          (theTrack nil))
      (checkMovieError)
      (setf theTrack (new QuickTimeTrack :inProject (project self))
            (identifier theTrack) identifier
            (movie theTrack) self)
      (pushNew theTrack (tracks self))
      theTrack)))

|#

|#

(define-handler dontSave :private (QuickTimeTrack)
  t)

(define-handler localPropertiesToSaveSpecially (QuickTimeTrack)
  (and (eq me QuickTimeTrack)
    '(movie
      trackOSPtr
      media)))

(define-handler preserve :private (QuickTimeTrack)
  (call-next-method)
  ;; Just in case it gets saved, eviscerate it.  A new one will be built up on restore.
  ;; (preserveLikeSave me)
  (sk8-return-nothing))

(define-handler saveToStore :private (QuickTimeTrack property)
  ;; Just in case it gets saved, eviscerate it.  A new one will be built up on restore.
  (case property
    ((movie
      trackOSPtr
      media)
     (and (getValue property me)
          *undefined*))
    (otherwise
     (call-next-method))))

#|
	Change History (most recent last):
	1	7/1/94	dy	
	2	7/1/94	dy	add )
	3	7/1/94	dy	fix )
	4	7/19/94	dy	QT Renderer changes
	5	8/10/94	dy	symbols with preserved case
	6  	 9/ 1/94	dy      	force creation of new Media with new Track
	7  	 9/27/94	dy      	NoValuexxx -> *undefined*
	8  	10/ 3/94	dy      	handle -> mediaData, other changes
	9  	10/ 5/94	chip    	initialize args now accessed with initializerArgument
	10 	10/ 5/94	dy      	
	11 	10/17/94	dy      	
	12 	11/14/94	dy      	add dontSave handler returning True
	13 	 2/16/95	sidney  	readable argument names for initialize handler
	14 	 2/28/95	dy      	MovieController! (no real changes)
	15 	 3/28/95	dy      	Try to fix preserve/restore.
	16 	 5/ 1/95	dy      	localVirtualProperties were not local.  Made dontSave private.
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3  11/26/96Hernan  Commenting out aliasGCHandle which is not defined.
|# ;(do not edit past this line!!)
