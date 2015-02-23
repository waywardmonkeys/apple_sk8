(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;;
;;; A QuickTimeTrackCollection is an IndirectCollection whose baseCollection property
;;; is a list of QuickTimeTrack objects.  We leave it to the List Collection code
;;; to do everything, except that when a track is to be inserted or removed, we make
;;; sure that the proper side effects happen.

(new collection
     :objectname "QuickTimeTrackCollection"
     :project SK8)

(addProperty QuickTimeTrackCollection 'movie                  )
(addProperty QuickTimeTrackCollection 'movieGCOSPtr :private t)
(addProperty QuickTimeTrackCollection 'reconciled   :private t :initialValue t)
(addProperty QuickTimeTrackCollection 'reconciling :private t)
(addProperty QuickTimeTrackCollection 'data :private t)

(define-handler invalidateTracks :private (QuickTimeTrackCollection)
  (unless (reconciling me)
    (setf (reconciled me) nil)) ;; see reconcileTrackCollectionWithMovieTracks below
  (setValue 'preferredSize (movie me) nil)
  (sk8-return-nothing))

(define-handler initialize (QuickTimeTrackCollection original isNew initargs)
  (declare (ignore original))
  (unless isNew
    (sk8-error GeneralProgrammaticError
               :strings '("You can't copy a QuickTimeTrackCollection")))
  (let ((movie (initializerArgument initargs 'movie :use nil)))
    (unless movie
      (sk8-error IncorrectArgumentsError
                 :arguments (list me :movie movie)))
    (ensureType movie QuickTimeMovie)
    (call-next-method)
    (setValue 'data me '())
    (setf (movieGCOSPtr me) (mediaData movie)))
  (setf (reconciled me) nil)
  (sk8-return-nothing))

;;; We lazy-evaluate the creation and updating of the track list (which is our baseCollection)
;;; The reconciled property is t or nil.  When it's nil, we either have to build the list anew or revise it.
;;; We reconcileTrackCollectionWithMovieTracks only as needed.

(define-handler reconcileTrackCollectionWithMovieTracks :private (QuickTimeTrackCollection)
  (unless (reconciled me)
    (with-preserved-values (((reconciling me) t))
      (let ((theMovieGCOSPtr (movieGCOSPtr me)))
        (when (macptrp theMovieGCOSPtr)
          (setf (reconciled me) t)
          ;; Go through the collection of SK8 QuickTimeTrack objects and remove ones no longer in the movie.
          (do ((state
                (initialVisitState me)
                (succeedingVisitState me state)))
              ((null state))
            (let ((item (elementAtVisitState me state)))
              (unless (T_GetMovieTrack theMovieGCOSPtr (id item) :noErrorIfNone t)
                (removeVisitState me state))))
          ;; Go through the tracks in the movie and add to the collection of SK8 QuickTimeTrack objects
          ;; any tracks not already in the collection.
          (let ((theMovie (movie me))
                (myProject (project me)))
            (loop for index from 1 to (T_GetMovieTrackCount theMovieGCOSPtr)
                  do (let* ((theTrackOSPtr (T_GetMovieIndTrack theMovieGCOSPtr index))
                            (id (T_GetTrackID theTrackOSPtr)))
                       (unless (trackFromID theMovie id)
                         (let ((newTrack (new QuickTimeTrack
                                              :movie      theMovie
                                              :trackOSPtr theTrackOSPtr
                                              :id         id
                                              :project    myProject)))
                           ;; Add a new QuickTimeTrack object to the collection
                           (insertAtVisitState me nil newTrack))))))))))
  (sk8-return-nothing))

(defun checkTrackInsert (me newItem)
  (EnsureType newItem QuickTimeTrack)
  (let ((myMovie (movie me)))
    (unless (eq (movie newItem) myMovie)
      (sk8-error GeneralProgrammaticError
                 :strings '("Track to be inserted ("
                            ") doesn't belong to this movie ("
                            ")")
                 :objects (list newItem myMovie)))
    (when (trackFromID myMovie (id newItem))
      (sk8-error GeneralProgrammaticError
                 :strings '("Track "
                            " is already in movie ("
                            ")")
                 :objects (list newItem myMovie))))
  (invalidateTracks me)
  (sk8-return-nothing))

;;; _______________________________ 
;;; Collection protocol starts here. 
;;; _______________________________ 

(defun remove-track (me state)
  (let ((track (elementAtVisitState me state)))
    (ignore-errors
     (T_DisposeMovieTrack (trackOSPtr track)))
    (setf (trackOSPtr track)         *undefined*)
    (setf (mediaOSPtr (media track)) *undefined*))
  (invalidateTracks me))

(defmethod collect ((me (eql QuickTimeTrackCollection))
                      &rest objects)
  (new QuickTimeTrackCollection :data (copy-list objects)))

(define-handler initialVisitState (QuickTimeTrackCollection)
  (data me))

(define-handler succeedingVisitState (QuickTimeTrackCollection stateObj)
  (declare (ignore me))
  (cdr stateObj))

(define-handler elementAtVisitState (QuickTimeTrackCollection stateObj)
  (declare (ignore me))
  (car stateObj))

(define-handler setElementAtVisitState (QuickTimeTrackCollection stateObj newVal)
  (remove-track me stateObj)
  (checkTrackInsert me newval)
  (setf (car stateObj) newVal)
  (data me))

(define-handler removeVisitState (QuickTimeTrackCollection stateObj)
  (cond
   ((null stateObj)
    (data me))
   ((eq stateObj (data me))
    (remove-track me stateObj)
    (setf (data me) (cdr (data me))))
   (t
    (let ((prevState (data me))
          (curState (cdr (data me))))
      (loop
        (when (eq curState stateObj) (return))
        (unless curstate (error "item not found"))
        (setq prevState curState
              curState (cdr curState)))
      (remove-track me curState)
      (setf (cdr (the list prevState)) (cdr curState))
      (data me)))))

(define-handler insertAtVisitState (QuickTimeTrackCollection 
                                       stateObj
                                       obj
                                       &key (after nil))
  (checkTrackInsert me obj)
  (cond
   ((null (data me))               ;;;empty list, so we need to make a new list
    (setf (data me) (cons obj nil)))
   ((and after stateObj)    ;;;after state is easy, we just set the cdr of the state
    (setf (cdr (the cons stateObj)) (cons obj (cdr stateObj))))
   ((or after (eq stateObj (data me))) ;;this is the insert at the front
    (let ((hd (car (data me))))
      (setf (car (data me)) obj)
      (setf (data me) (rplacd (data me) (cons hd (cdr (data me)))))))
   ((null stateObj)             ;;;before an empty state means append to the end..
    (nconc (data me) (list obj)))
   (t                        ;;;otherwise find state and then shove it right before it.
    (let* ((prevState (data me))
           (st (cdr (data me))))
      (loop
        (when (eq st stateObj) (return))
        (setq prevState st
              st (cdr st)))
      (setf obj (cons obj stateObj)
            (cdr prevState) obj))))
  (data me))

(define-handler indexAtVisitState (QuickTimeTrackCollection stateObj)
  (1+ (search (data me) stateObj :test #'eq)))

(define-handler visitStateAtIndex (QuickTimeTrackCollection indexObj)
  (nthcdr (1- indexObj) (data me)))

(define-handler isFinalVisitState (QuickTimeTrackCollection stateObj)
  (equal stateObj (last (data me))))

;;; _______________________________ 
;;; Store stuff. 
;;; _______________________________ 

(define-handler dontSave (QuickTimeTrackCollection)
  (declare (ignore me))
  t)

(define-handler localPropertiesToSaveAsFalse (QuickTimeTrackCollection)
  (and (eq me QuickTimeTrackCollection)
       '(data
         movie
         movieGCOSPtr
         reconciled
         reconciling)))

(define-handler preserve (QuickTimeTrackCollection)
  (declare (ignore me))
  (call-next-method)
  ;; (preserveLikeSave me)
  (sk8-return-nothing))

#|
	Change History (most recent last):
	1	7/1/94	dy	
	2	7/19/94	dy	QT Renderer changes
	3	8/10/94	dy	symbols with preserved case
	4  	 9/ 1/94	dy      	Bug fix: call T_GetMovieTrack with :noErrorIfNone
	5  	 9/27/94	dy      	NoValuexxx -> *undefined*
	6  	 9/28/94	chip    	AliasedCollection --> IndirectCollection
	7  	 9/30/94	dy      	return *undefined* when no useful return value instead of (sk8-multivals)
	8  	10/ 3/94	dy      	handle -> mediaData, other changes
	9  	10/ 5/94	chip    	initialize args now accessed with initializerArgument
	10 	10/ 5/94	dy      	fix reconciling, which caused looping
	11 	10/17/94	dy      	
	12 	11/14/94	dy      	add dontSave handler returning True
	13 	 2/16/95	sidney  	readable argument names for initialize handler
	14 	 2/28/95	dy      	MovieController!
	15 	 3/28/95	dy      	Try to fix preserve/restore.
	16 	 3/31/95	dy      	baseCollection used to return an error if movieGCOSPtr was not valid
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/16/96	Hernan  	need to be reimplemented.
	4  	 9/23/96	Hernan  	Reimplementing it under the new collection protocol.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
