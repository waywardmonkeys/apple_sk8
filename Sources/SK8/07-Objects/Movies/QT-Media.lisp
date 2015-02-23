(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(addProperty QuickTimeMedia 'mediaOSPtr) ; the macptr - it is owned by the movie object
(addProperty QuickTimeMedia 'movie     )      ; QuickTimeMovie to which the track belongs
(addProperty QuickTimeMedia 'track     )      ; QuickTimeTrack that owns this QuickTimeMedia
(addProperty QuickTimeMedia 'isVisual  )   ; has a visual manifestation

(define-handler localVirtualProperties (QuickTimeMedia)
  (and (eq me QuickTimeMedia)
       #+QTEdit
       '()
       #-QTEdit
       '()))

(defmacro assumeMediaOSPtr ((me osPtrName) &body body)
  `(let ((,osPtrName (mediaOSPtr ,me)))
     (if ,osPtrName
      (progn ,@body)
      (sk8-return-nothing))))

(define-handler initialize :private (QuickTimeMedia original isNew initArgs)
  (declare (ignore original))
  (unless isNew
    (sk8-error GeneralProgrammaticError
               :strings '("Can't copy a QuickTimeMedia object")))
  
  (let ((mediaOSPtr (initializerArgument initArgs 'mediaOSPtr :use nil))
        (movie      (initializerArgument initArgs 'movie      :use nil))
        (track      (initializerArgument initArgs 'track      :use nil)))
    (unless (and mediaOSPtr
                 movie
                 track)
      (sk8-error GeneralProgrammaticError
                 :strings '("not enough initialzers")))
    (call-next-method)
    
    
    ;;? Beware - in future QuickTime could define new visual mediaTypes.
    ;;  I've posted as a bug that QuickTime should have a trap to determine if a mediaType is visual.
    (setf (isVisual me) (T_isVisualMediaType (T_getMediaType mediaOSPtr)))
    )
  (sk8-return-nothing))

#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATING AND EDITING MEDIA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
makeNewTrackMedia [track] - returns a new media for the track
dispose - disposes of the media
          NOTE: a media is automatically disposed whenever its track or movie are disposed
|#

(define-handler initialize (QuickTimeMedia original isNew initArgs)
  (declare (ignore original isNew))
  (let ((theTrack (or (track me) (initializerArgument initargs 'track)))
        (mediaType (mediaType me))
        dataRef dataRefType)
    (unless theTrack (error "Can't have a media without a track"))
    (when (media theTrack) (error "Track ~s already has media ~s" theTrack (media theTrack)))
    (let* ((trackIdentifier (identifier theTrack))
           (mediaIdentifier (#_gettrackmedia trackIdentifier))
           (theError (#_getmovieserror)))
      (#_clearmoviesstickyerror)
      (cond ((eql theError #$invalidMedia)
             (unless (fsspec (movie theTrack)) 
               (error "Movie ~s is not associated with a file" (movie theTrack)))
             (setq dataRef (#_newhandle 0))
             (%stack-block ((ptr 4))
               (%put-ptr ptr dataRef)
               (#_newalias mf::*null-ptr* (fsspec (movie theTrack)) dataRef))
             (unless dataRefType (setq dataRefType #$rAliasType))
             (setq mediaIdentifier (#_newtrackmedia trackIdentifier mediaType
                                    (timeScale (movie theTrack)) dataRef dataRefType))
             (checkMovieError))
            ((eql theError #$noErr))
            (t (error "Unexpected error ~s" theError)))
      (setf (identifier me) mediaIdentifier
            (media theTrack) me
            (track me) theTrack))))

(define-handler dispose (QuickTimeMedia &key)
  (unless (mf::undisposable-object? me)
    (let ((identifier (identifier me)))
      (when (handlep identifier)    ; SHOULD BE DONE BY dispose [movieTrack]
        (#_disposetrackmedia identifier)
        (checkMovieError)))
    (call-next-method)))
  
(define-handler dataSize (QuickTimeMedia &optional (start 0) (duration (duration me)))
  (#_getmediadatasize (identifier me) start duration))

(define-handler sampleDescriptionCount (QuickTimeMedia)
  (#_getmediasampledescriptioncount (identifier me)))

(define-handler sampleDescription (QuickTimeMedia index &optional (handle (#_newhandle 0)))
  (#_getmediasampledescription (identifier me) index handle)
  (checkMovieError)
  handle)

(define-handler timeToSampleNumber (QuickTimeMedia theTime)
  (%stack-block ((sampleTime 4) (sampleNum 4) (duration 4))
    (#_mediatimetosamplenum (identifier me) theTime sampleNum sampleTime duration)
    (checkMovieError)
    (SK8-multivals (%get-long sampleNum) (%get-long sampleTime) (%get-long duration))))

(define-handler sampleNumberToTime (QuickTimeMedia num)
  (%stack-block ((theTime 4) (duration 4))
    (#_samplenumtomediatime (identifier me) num theTime duration)
    (checkMovieError)
    (SK8-multivals (%get-long theTime) (%get-long duration))))

;;;;;;;;;;;;;;
;;; Editing Media
;;;;;;;;;;;;;;

(define-handler beginMediaEdits (QuickTimeMedia)
  (#_beginmediaedits (identifier me))
  (checkMovieError))

(define-handler endMediaEdits (QuickTimeMedia)
  (#_endmediaedits (identifier me))
  (checkMovieError))

(define-handler addSample (QuickTimeMedia handle              ; handle to sample data
                                               offset             ; offset into data
                                               size               ; number of bytes of data
                                               durationPerSample  ; duration of each sample
                                               description        ; sample description
                                               num                ; number of samples
                                               flags)             ; sample flags
  (%stack-block ((theTime 4))
    (#_addmediasample (identifier me) handle offset size durationpersample description num flags theTime)
    (checkMovieError)
    (%get-long theTime)))

(define-handler addSampleReference (QuickTimeMedia offset             ; offset into data
                                                         size               ; number of bytes of data
                                                         durationPerSample  ; duration of each sample
                                                         description        ; sample description
                                                         num                ; number of samples
                                                         flags)             ; sample flags
  (%stack-block ((theTime 4))
    (#_addmediasamplereference (identifier me) offset size durationpersample description num flags theTime)
    (checkMovieError)
    (%get-long theTime)))

(define-handler getMediaSample (QuickTimeMedia handle maxSize theTime description maxSamples)
  (%stack-block ((size 4)
                 (sampleTime 4)
                 (durationPerSample 4)
                 (index 4)
                 (samples 4)
                 (flags 2))
    (#_getmediasample (identifier me) handle maxSize size theTime sampletime durationpersample
     description index maxSamples samples flags)
    (checkMovieError)
    (SK8-multivals (%get-long size)
                   (%get-long sampleTime)
                   (%get-long durationPerSample)
                   (%get-long index)
                   (%get-long samples)
                   (%get-word flags))))

(define-handler getmediasamplereference (QuickTimeMedia theTime description)
  (%stack-block ((dataoffset 4)
                 (size 4)
                 (sampletime 4)
                 (durationpersample 4)
                 (index 4)
                 (maxnumberofsamples 4)
                 (numberofsamples 4)
                 (flags 2))
    (#_getmediasamplereference (identifier me) DATAOFFSET SIZE theTime SAMPLETIME DURATIONPERSAMPLE
     description index maxnumberofsamples numberofsamples flags) 
    (checkMovieError)
    (SK8-multivals (%get-long size)
                   (%get-long dataoffset)
                   (%get-long sampleTime)
                   (%get-long durationPerSample)
                   (%get-long index)
                   (%get-long maxNumberOfSamples)
                   (%get-long numberOfSamples)
                   (%get-word flags))))

;;;;;;;;;;;;;;;;;;;
;;; Video Media Handler
;;;;;;;;;;;;;;;;;;;

(define-handler mediaHandler (QuickTimeMedia)
  (prog1 (#_getmediahandler (identifier me))
    (checkMovieError)))

(define-handler mediaHandlerDescription (QuickTimeMedia)
  (rlet ((mediaType :ostype)
         (creatorName :str255)
         (manufacturer :ostype))
    (#_getmediahandlerdescription (identifier me)  mediaType creatorName manufacturer)
    (checkMovieError)
    (SK8-multivals (%get-ostype mediaType) (%get-string creatorName) (%get-ostype manufacturer))))

(define-handler setGraphicsMode (QuickTimeMedia mode opcolor)
  (#_setvideomediagraphicsmode (mediaHandler me) mode opcolor)
  (checkMovieError))

(define-handler graphicsMode (QuickTimeMedia)
  (rlet ((mode :longint)
         (opColor :rgbcolor))
    (#_getvideomediagraphicsmode (mediaHandler me) mode opcolor)
    (checkMovieError)
    (SK8-multivals mode opcolor)))


;;;;;;;;;;;;;;;;;;;;
;;; Sound Media Handler
;;;;;;;;;;;;;;;;;;;;

(define-handler (setf soundBalance) (balance QuickTimeMedia)
  (#_setsoundmediabalance (mediaHandler me) balance)
  (checkMovieError))

(define-handler soundBalance (QuickTimeMedia)
  (rlet ((balance :integer))
    (#_getsoundmediabalance (mediaHandler me) balance)
    (checkMovieError)
    balance))
  
  

;;;;;;;;;;;;
;;; User Data
;;;;;;;;;;;;

(define-handler userData (QuickTimeMedia)
  (#_getmediauserdata (identifier me)))

;;;;;;;;
;;; TIME
;;;;;;;;

(define-handler duration (QuickTimeMedia)
  (#_getmediaduration (identifier me)))

(define-handler (setf timeScale) (timeScale QuickTimeMedia)
  (#_setmediatimescale (identifier me) timescale)
  (checkMovieError))

(define-handler timeScale (QuickTimeMedia)
  (#_getmediatimescale (identifier me)))

(define-handler mediaNextInterestingTime (QuickTimeMedia)
  (error "not implemented"))

(define-handler creationTime (QuickTimeMedia)
  (#_getmediacreationtime (identifier me)))

(define-handler modificationTime (QuickTimeMedia)
  (#_getmediamodificationtime (identifier me)))

;;;;;;;;;;;;;
;;; Alternates
;;;;;;;;;;;;;

(define-handler (setf mediaLanguage) (language QuickTimeMedia)
  (#_setmedialanguage (identifier me) language)
  (checkMovieError))

(define-handler mediaLanguage (QuickTimeMedia)
  (#_getmedialanguage (identifier me)))

(define-handler (setf mediaQuality) (quality QuickTimeMedia)
  (#_setmediaquality (identifier me) quality)
  (checkMovieError))

(define-handler mediaQuality (QuickTimeMedia)
  (#_getmediaquality (identifier me)))


;;;;;;;;;;;;;;;;;
;;; Data References
;;;;;;;;;;;;;;;;;

(define-handler mediaDataRefCount (QuickTimeMedia)
  (%stack-block ((count 2))
    (#_getmediadatarefcount (identifier me) count)
    (%get-word count)))

(define-handler setMediaDataRef (QuickTimeMedia index dataRef)
  (%stack-block ((count 2))
    (%put-word count index)
    (#_addmediadataref (identifier me) count dataRef #$rAliasType)
    (checkMovieError)
    (%get-word count)))

(define-handler mediaDataRef (QuickTimeMedia index)
  (%stack-block ((dataRef 4)
                 (dataRefType 4)
                 (dataRefAttributes 4))
    (#_getmediadataref (identifier me) index dataRef dataRefType dataRefAttributes)
    (checkMovieError)
    (SK8-multivals (%get-ptr dataRef)
                   (%get-ostype dataRefType)
                   (%get-long dataRefAttributes))))
|#

(define-handler dontSave (QuickTimeMedia)
  t)

(define-handler localPropertiesToSaveSpecially (QuickTimeMedia)
  (and (eq me QuickTimeMedia)
    '(movie
      mediaOSPtr
      track)))

(define-handler preserve :private (QuickTimeMedia)
  (call-next-method)
  ;; (preserveLikeSave me)
  (sk8-return-nothing))

(define-handler saveToStore :private (QuickTimeMedia property)
  ;; Just in case it gets saved, eviscerate it.  A new one will be built up on restore.
  (case property
    ((movie
      mediaOSPtr
      track)
     (and (getValue property me)
          *undefined*))
    (otherwise
     (call-next-method))))

#|
	Change History (most recent last):
	1	7/1/94	dy	
	2	7/19/94	dy	QT Renderer changes
	3	8/10/94	dy	symbols with preserved case
	4  	 9/27/94	dy      	NoValuexxx -> *undefined*
	5  	 9/30/94	dy      	return *undefined* when no useful return value instead of (sk8-multivals)
	6  	10/ 3/94	dy      	sk8-return-nothing instead of *undefined*
	7  	10/ 5/94	chip    	initialize args now accessed with initializerArgument
	8  	10/17/94	dy      	
	9  	11/14/94	dy      	add dontSave handler returning True
	10 	 2/16/95	sidney  	readable argument names for initialize handler
	11 	 3/28/95	dy      	Try to fix preserve/restore.
	2  	 6/12/95	dy      	fix localVirtualProperties bug
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
