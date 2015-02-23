;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)

(provide "MEDIATYPES")

;;; _____________________________________
;;; Reading a PICT handle from a file. 
;;; _____________________________________

;;; First the NOT IN ROM traps (taken from not it rom folder in MCL).

(defun _FSOpen (aFileName vRefNum refnum)
  (rlet ((pb :ParamBlockRec 
             :ioNamePtr aFileName
             :ioVRefNum vRefNum
             :ioVersNum 0
             :ioPermssn #$fsCurPerm
             :ioMisc (%null-ptr)))
    (prog1
      (#_PBOpen pb)
      (%put-word refNum (pref pb :ParamBlockRec.ioRefNum)))))

(defun _SetFPos (refnum posmode posOff)
  (rlet ((pb :ParamBlockRec
             :ioRefNum    refNum
             :ioPosMode   posMode
             :ioPosOffset posOff))
    (#_PBSetFPos pb)))

(defun _FSClose (refnum)
  (rlet ((pb :ParamBlockRec
             :ioCompletion (%null-ptr)
             :ioRefNum refNum))
    (let ((result (#_PBFlushFile pb)))
      (if (eql #$noErr result)
        (#_PBClose pb)
        result))))

(defun _FSRead (refnum count buffPtr)
  (rlet ((pb :ParamBlockRec
             :ioVRefNum   0
             :ioRefNum    refNum
             :ioBuffer    buffPtr
             :ioReqCount  (%get-signed-long count)
             :ioPosMode   #$fsAtMark
             :ioPosOffset 0))
    (prog1
      (#_PBRead pb)
      (%put-long count (pref pb :ParamBlockRec.ioActCount)))))

(defun _GetEOF (refnum logEOF)
  (rlet ((pb :ParamBlockRec
             :ioRefNum refNum))
    (prog1
      (#_PBGetEOF pb)
      (%put-long logEOF (%ptr-to-int (pref pb :ParamBlockRec.ioMisc))))))

;;; Reads in a pict handle from the data fork of a file. 

(defvar globalRef)


;;moved here because pictfiletohandle needs this. -bjr
(defun newFsspec (pathname)
  (let ((fsspec (sk8dev::newRecordGCHandle :fsspec :name (make-string 64 :initial-element #\Null)))
        (done? nil)
        (error nil))
    (unwind-protect
      (with-pointers ((fsspecptr fsspec))
        (with-pstrs ((filename (mac-namestring pathname)))
          (unless (or (eql #$noErr (setq error (#_fsmakefsspec 0 0 filename fsspecptr)))
                      (eql #$fnfErr error))
            (error "File error ~s" error))
          (setq done? t)))
      (unless done? (setf fsspec nil))) ;; (dispose-record fsspec)))
    fsspec))

(defun PICTFileToHandle (theFile)
  (when (fileExists theFile)
    (gs:let+ ((fileSize (:pointer))
           (fsspec (newFsspec (physicalName theFile)))
           thePictHandle)
      (without-interrupts 
       (%stack-block ((globalRefPtr 2))
         (unless (eql #$noErr 
                      (with-pstrs ((aFileName (mac-namestring (physicalName thefile))))
                        (_FSOpen aFileName (rref fsspec :fsspec.vrefnum) globalRefPtr)))
           (dotimes (i 4)
             (ed-beep))
           (break))
         (setq globalRef (%get-word globalRefPtr)))
       ;; Get the size of the pict.
       (_GetEOF globalRef fileSize)
       (setf thePictHandle (T_newHandleGC (%get-long fileSize)))
       (unless (handlep thePictHandle)
         (sk8-error OSHeapFullError))
       ;; Skip the header.
       (_SetFPos globalRef 1 512) 
       ;; Read in the whole pict.
       (with-dereferenced-handles ((pictPtr thePictHandle))
         (_FSRead globalRef fileSize pictPtr))
       ;; Close the file.
       (_FSClose globalRef)
       thePictHandle
       ))))

;;; ______________
;;; All Media.
;;; ______________

(new Object :objectname "Media"
     :properties '((file)       ;; File where media sits.
                   (resourceId) ;; Res num of media (if false, media is in data fork).
                   (mediaData  :private))    ;; Stores the media once read from disk.
     :project sk8)

(define-handler resType (Media)
  nil)

;;; SK8 resource objects look for the MAC rsrcs in their 
;;; project file (actually the current working file of the project)!

;;; Here we hack the file property of resource so that when it refers to the project file
;;; we will end up using the current swap file of the project if there is one

(define-handler initialize (Media original isNew initArgs)
  (declare (ignore original isNew initargs))
  (call-next-method)
  (setvalue 'file me :project))

(define-handler file (Media)
  (let ((fl (getvalue 'file me))
        (proj (project me)))
    (case fl
      (:project (or (sk8::swapfile proj) (sk8::file proj)))
      (:application SK8ApplicationFile)
      (t fl))))

(define-handler (setf file) (newfile Media)
  (setvalue 'file me
            (cond ((null newfile) nil)
                  ((eq newfile SK8ApplicationFile) :application)
                  ((let ((prj (project me)))
                     (or (eq newfile (sk8::file prj))
                         (eq newfile (sk8::swapfile prj))))
                   :project)
                  (t newfile)))
  newfile)

(defmacro report-file-missing-error (theMedia)
  `(sk8-error GeneralProgrammaticError
              :strings '("Tried to load " "'s media but its file was False.")
              :objects (list ,theMedia)))

;;; Media types should redefine this handler to look for stuff in the data forks of their files
;;; when the resourceId is false.

(define-handler loadMedia (Media)
  (let ((handle (mediaData me))
        (thefile (file me)))
    (if (and (handlep handle) (not (ccl::%null-ptr-p handle)))
      handle
      (if theFile
        (setf (mediaData me)
              (macGetResourceHandleFromIdForDrawing theFile (resType me) (resourceId me)))
        (report-file-missing-error me)))))

(define-handler unloadMedia (Media)
  (setf (mediaData me) nil))

(define-handler preserve (Media)
  (unloadMedia me))

;;; These are for the store to find unnamed media in other projects.
;;; It used to use the file and resourceid as identifiers, but that fails when you do something as simple as change the name of a project file.
;;; Now it instead assumes that the media object is the media property of some other object in the same project

(define-handler sk8::helpfindmychild (Media)
  (let* ((myproj (project me))
         (myowner (block searchblock
                    (mapprojectobjects
                     myproj
                     #'(lambda(obj)
                         (when (and (property obj 'media)
                                    (eq (media obj) me)
                                    (objectname obj))
                           (return-from searchblock obj)))))))
    (if myowner
      (list 'media myowner)
      (call-next-method))))

(define-handler sk8::pleaseFindMyChild (Media args)
  (if (and (listp args) (eq (pop args) 'media))
    (if (null (cdr args))   ;; this is the only proper situation, the other is only for backwards compatibility with old project files
      (media (car args))
      ;; we can get rid of this code after beta 3, it is only to support some old project files
      (let* ((projname (pop args))
             (pars (pop args))
             (slots (pop args))
             (myid (pop args))
             (aFileName (pop args))
             (myresid (pop args)))
        ;; this is taken from find-unique-unnamed-object, but also looks for filename and resid
        (let ((proj (and (symbolp projname) (boundp projname) (symbol-value projname))))
          (when (sk8::is-a proj sk8::project)
            (let ((parent-classes (mapcar #'class-of pars))
                  retobj)
              (SK8::mapProjectObjects
               proj
               #'(lambda (child)
                   (unless (mf::named-object? child)
                     (let* ((child-class (class-of child))
                            (classname (class-name child-class)))
                       (when (and (or (null classname)  ;; only use real sk8 classes, which are unnamed
                                      (not (symbolp classname)))
                                  (equal (class-direct-superclasses child-class)
                                         parent-classes))
                         (when (and (eql myresid (resourceid child))
                                    (let ((childfile (getvalue 'file child)))
                                      (when (is-a childfile sk8::file) (setf childfile (physicalname childfile)))
                                      (or (eq aFileName childfile) (string-equal aFileName childfile)))
                                    (not (set-exclusive-or slots
                                                           (mapcar #'car (class-direct-instance-slots child-class))))
                                    ;; child is a valid candidate. use it if id matches too, else save one in case we never find an id match
                                    (if (eql myid (sk8::sk8_id child))
                                      (return-from sk8::pleasefindmychild child)
                                      (unless retobj
                                        (setf retobj child))))))))))
              retobj))))
      )
    (call-next-method)))

;;; ImportFromPtr and ExportToPtr for apple events.

(define-handler importfromptr (Media data-ptr data-size proj)
  (let ((mediaObj (new me :project proj))
        (theHandle (#_newhandle data-size)))
    (#_ptrtoxhand data-ptr theHandle data-size)
    (setf (mediadata mediaObj) (makeGCOSPtr theHandle #_DisposeHandle))
    mediaObj))

(define-handler exporttoptr (Media)
  (let (thehandle)
    ;; load the media.
    (loadmedia me)
    (setf thehandle (mediadata me))
    ;; complain if no media.
    (unless (handlep thehandle)
      (error "Expected a valid handle to be loaded into ~a." me))
    ;; get the pointer out of it. 
    (sk8-multivals (%get-ptr thehandle) (#_gethandlesize thehandle))))

;;; ______________
;;; Media Types.
;;; ______________

;;; ____
;;; PICT
;;; ____

(new Media :objectname "QDPicture"
     :prototype t
     :project sk8)

(define-handler restype (qdPicture) 
  "PICT")

(define-handler boundsRect (qdPicture &key)
  (let ((handle (loadMedia me))
        rect)
    (if (handlep handle)
      (progn
        (setq rect (gs:hrref handle :picture.picFrame :storage :pointer))
        (SK8-multivals (rref rect :rect.left)
                       (rref rect :rect.top)
                       (rref rect :rect.right)
                       (rref rect :rect.bottom)))
      (error "The Media of ~a is not valid." (objectString me)))))

(define-handler size (qdPicture &key)
  (SK8-multival-bind (ll tt rr bb) (boundsRect me)
    (SK8-multivals (- rr ll) (- bb tt))))

;;; If the resourceId is set, the pict is a resource in the resource fork.

(define-handler loadMedia (QDPicture)
  (if (resourceId me)
    (call-next-method)
    (let ((theHandle (mediaData me)))
      (if (and (handlep theHandle) (not (ccl::%null-ptr-p theHandle)))
        theHandle
        (let ((theFile (file me)))
          (if theFile
            (when (fileExists theFile)
              (setf theHandle (pictFileToHandle theFile))
              (setf (mediaData me) theHandle)
              theHandle)
            (report-file-missing-error me)))))))

;;; ____
;;; cicn
;;; ____

(new Media :objectname "IconRSRC"
     :prototype t
     :project sk8)

(define-handler resType (IconRSRC)
  "cicn")

(define-handler boundsRect (IconRSRC &key)
  (let ((handle (loadMedia me)))
    (if (handlep handle)
      (with-dereferenced-handles ((cicon handle))
        (let ((rect (rref (rref cicon :cicon.iconpMap :storage :pointer) :pixmap.bounds :storage :pointer)))
          (SK8-multivals (rref rect :rect.left)
                         (rref rect :rect.top)
                         (rref rect :rect.right)
                         (rref rect :rect.bottom))))
      (error "The Media of ~a is not valid." (objectString me)))))
    
(define-handler size (IconRSRC &key)
  (SK8-multival-bind (ll tt rr bb) (boundsRect me)
    (SK8-multivals (- rr ll) (- bb tt))))

;;; ____
;;; snd
;;; ____

(new Media :objectname "SoundRSRC"
     :prototype t
     :properties '((channel :value nil))       ; Sound Channel
     :project sk8)

(define-handler resType (soundRsrc) 
  "snd ")

;;; SK8's built in sound resources!

(new soundRSRC :objectname "ModalSoundRsrc" :project sk8)
(setf (resourceId ModalSoundRsrc) 1000)
(new soundRSRC :objectName "ModelessSoundRsrc" :project sk8)
(setf (resourceId ModelessSoundRsrc) 1001)
(new soundRSRC :objectname "ZoomdownSoundRsrc" :project sk8)
(setf (resourceId ZoomdownSoundRsrc) 1002)
(new soundRSRC :objectname "ZoomupSoundRsrc" :project sk8)
(setf (resourceId ZoomupSoundRsrc) 1003)

;;; Low-Level Sound Utilities for Macintosh

(defparameter *current-sounds* nil)

(defun stop-all-sounds ()
  (when *current-sounds*
    ;; Will be GCed.
    ;;(mapc #'(lambda (cs)
    ;;          (#_SndDisposeChannel (%get-ptr cs) t)
    ;;          (#_DisposPtr cs))
    ;;      *current-sounds*)
    (setq *current-sounds* nil)
    t))

(define-handler running (soundRSRC)
  (and (channel me)
       (let (err)
         (rlet ((status :SCStatus))
           (setf err (#_SndChannelStatus (%get-ptr (channel me)) 24 status))
           (unless (eql err 0) (error "OS Error while checking the status of the sound."))
           (rref status :SCStatus.scChannelBusy :storage :pointer)))))

(define-handler play (soundRsrc &key (synchronously nil))
  (let ((snd (loadMedia me)))
    (when (handlep snd)
      (let ((sndPtr (T_newPtr 4))
            err)
        (unless (handlep snd)
          (T_LoadResource snd))
        (%put-ptr sndPtr (%null-ptr))
        (when (eq (length *current-sounds*) 4)
          (#_SndDisposeChannel (%get-ptr (car (last *current-sounds*))) t)
          (#_DisposPtr (car (last *current-sounds*)))
          (setq *current-sounds* (remove (car (last *current-sounds*))
                                         *current-sounds*)))
        (setq err (#_SndNewChannel sndptr 5 #$initStereo (%null-ptr)))
        (cond
         ((eq 0 err)
          (pushnew sndptr *current-sounds*)
          (setf (channel me) sndptr)
          (#_SndPlay (%get-ptr sndptr) snd (not synchronously))
          sndptr)
         (t 
          (ed-beep)
          (format t "~%error: ~a, could not make a new sound channel." err)))))))

(define-handler stop (soundRsrc &key)
  (let ((channel (channel me)))
    (when channel
      (without-interrupts
       (#_SndDisposeChannel (%get-ptr channel) t)
       (setf (channel me) nil)
       (setf *current-sounds* (delete channel *current-sounds*))))))

(define-handler displaySample (SoundRSRC theActor)
  (declare (ignore theActor))
  (play me :synchronously t))

;;; ____
;;; crsr
;;; ____

(new Media :objectname "CursorRSRC"
     :prototype t
     :project sk8)

(define-handler resType (cursorRsrc) 
  "CURS")

(new cursorRSRC :objectName "ArrowCursor" :project sk8)
(define-handler loadMedia (arrowCursor)
  *arrow-cursor*)

(new cursorRSRC :objectName "WatchCursor" :project sk8)
(define-handler loadMedia (watchCursor)
  *watch-cursor*)

(new cursorRSRC :objectName "IbeamCursor" :project sk8)
(define-handler loadMedia (IbeamCursor)
  *i-beam-cursor*)

(new cursorRSRC :objectName "StandardCursor" :project sk8)
(define-handler loadMedia (standardCursor)
  #'ccl::cursorHook)

(define-handler cursorHook (CursorRSRC) 
  nil)

(defun set-cursor-internal (theCursor)
  (when theCursor
    (setf ccl::*cursorHook*
          (or (cursorHook theCursor)
              (loadMedia theCursor)
              #'ccl::cursorhook))
    (update-cursor)))

(define-handler install (CursorRSRC)
  (set-cursor-internal me))

(define-handler displaySample (CursorRSRC theActor)
  (declare (ignore theActor))
  (setf (cursor Stage) me))

;;; ____
;;; PAT
;;; ____

(new Media :objectname "BWPattern"
     :prototype t
     :project sk8)

(define-handler resType (bwpattern)
  "PAT ")

(define-handler size (BWPattern)
  (sk8-multivals 8 8))

(define-handler boundsRect (BWPattern &key)
  (sk8-multivals 0 0 8 8))

;; the built-in patterns

(new bwPattern :objectname "BlackPatternRSRC" :undisposable t :project sk8)

(define-handler loadMedia (blackPatternRSRC)
  *black-pattern*)

(new bwPattern :objectname "DarkGrayPatternRSRC" :undisposable t :project sk8)

(define-handler loadMedia (darkGrayPatternRSRC)
  *dark-gray-pattern*)

(new bwPattern :objectname "GrayPatternRSRC" :undisposable t :project sk8)

(define-handler loadMedia (grayPatternRSRC)
  *gray-pattern*)

(new bwPattern :objectname "LightGrayPatternRSRC" :undisposable t :project sk8)

(define-handler loadMedia (lightGrayPatternRSRC)
  *light-gray-pattern*)

(new bwPattern :objectname "WhitePatternRSRC" :undisposable t :project sk8)

(define-handler loadMedia (whitePatternRSRC)
  *white-pattern*)

;;; ____
;;; ppat
;;; ____

;; For pixel patterns

(new Media :objectname "ColorPattern"
     :prototype t
     :project sk8)

(define-handler resType (colorPattern) 
  "ppat")

(define-handler boundsRect (colorPattern &key)
  (let ((handle (loadMedia me)))
    (if (handlep handle)
      (with-dereferenced-handles ((ppat handle)
                                  (pmap (rref ppat :pixpat.patMap :storage :pointer)))
        (with-macptrs ((rect (rref pmap :pixmap.bounds :storage :pointer)))
          (SK8-multivals (rref rect :rect.left)
                         (rref rect :rect.top)
                         (rref rect :rect.right)
                         (rref rect :rect.bottom))))
      (error "The Media of ~a is not valid." (objectString me)))))

(define-handler size (colorPattern &key)
  (SK8-multival-bind (ll tt rr bb) (boundsRect me)
    (SK8-multivals (- rr ll) (- bb tt))))

;;; ______________________
;;; Fonts (they could be media!)
;;; ______________________

(new media :objectname "Font" :project sk8 :undisposable t
     :properties '((fontData :value 0)
                   (fontName :value "Chicago")))

;;; textfont helpers.

(defun fontName-to-num (fontName)
  (%stack-block ((font-number-ptr 2))
    (with-pstrs ((pascal-name fontName))
      (#_GetFNum pascal-name font-number-ptr)
      (%get-word font-number-ptr))))

;;; Any font that is not found is set to a fontNum of 0!

(define-handler (setf fontName) (fontName font)
  ;; Set the slot.
  (setf (slot-value me 'fontName) fontName)
  ;; Update the fontData.
  (setf (fontData me) (fontName-to-num fontName))
  (fontName me))

;;; Given a font name it returns a font object whose fontName is the string given or nil
;;; if none is found. No consing...

(defun fontname-to-font-helper (theFont theName)
  (when (and (string-equal theName (fontName theFont))
             (neq theFont font))
    (throw :done theFont))
  ;; Recurse for kids.
  (dolist (c (knownchildren theFont) nil)
    (fontname-to-font-helper c theName)))

(defun fontName-to-font (fontName)
  (catch :done
    (fontname-to-font-helper font fontName)))

(defun removeSpaces (theString)
  ;; First get rid of external spaces.
  (setf theString (string-trim '(#\space) theString))
  ;; Get rid of internal spaces.
  (let ((result ""))
    (dotimes (i (length theString))
      (when (alphanumericp (elt theString i))
        (setf result (concatenate 'string result (string (elt theString i))))))
    ;; Add an f if the first char is a number.
    (when (digit-char-p (aref result 0))
      (setf result (concatenate 'string "F" result)))
    result))
  
(defparameter *font-population* (ccl::%cons-population nil))

;;; Makes a population, adds all the fonts to it and makes them all unnamed.
;;; By the time we return to sk8, a full GC will have happened (at save app time)
;;; and all the unused fonts will be gone from here.

(defun preserve-sk8-fonts ()
  (let ((theFonts (knownChildren Font)))
    ;; Unname them all.
    (dolist (c theFonts)
      (setf (objectName c :force t) nil))
    ;; build the population and remove them from the knownChildren of Font.
    (setf *font-population* (ccl::%cons-population theFonts))))

;;; Now, on returning, we check which fonts survived and we union them to the stuff
;;; that is not already there, which is created. At the end we clean up the population,
;;; and all fonts are added to the knownChildren of Font.

(defun restore-sk8-fonts ()
  (let ((usedFonts (ccl::population.data *font-population*))
        (fontsAvailable *font-list*)
        theName theSym)
    (let ((*warn-if-redefine* nil))
      (declare (special *warn-if-redefine*))
      ;; Make all the fonts that are not in used fonts but are in fontsAvailable.
      (dolist (c fontsAvailable)
        (unless (member c  usedFonts :test #'string-equal :key #'fontName)
          (setf theName (concatenate 'string (removeSpaces c) "Font"))
          (setf theSym (intern-legal-varName thename :SK8 nil T))
          (new font :project sk8 :objectName theName :fontname c)))
      ;; Now name all the fonts in the usedFonts list. Also set their fontNames 
      ;; to the names they already have to ensure the fontData is recomputed.
      (dolist (c usedFonts)
        (setf theName (concatenate 'string (removeSpaces (fontname c)) "Font"))
        (setf theSym (intern-legal-varName thename :SK8 nil T))
        (setf (objectName c) theName)
        (setf (fontName c) (fontName c)))
      ;; Clear the population.
      (setf *font-population* nil))))

(restore-sk8-fonts)


#|
	Change History (most recent last):
	2		6/25/93	kleiman	initializeObject -> initialize
	8		10/1/93	kleiman	obsolete struct
	9		10/15/93	hernan	Adding size and boundRect of patterns.
	10		11/1/93	hernan	Macpathname -> file.
	11		11/8/93	kleiman	FileMedia
	12		11/8/93	hernan	Adding pictFileMedia to the children of fileMedia.
	13		11/19/93	hernan	Making boundsRect of media signal an error when
							the media is bogus.
	14		12/21/93	hernan	Making sure we have resources before we release
							them.
	15		1/10/94	hernan	Adding fonts to the media file!
	16		1/10/94	hernan	Adding restore-sk8-fonts and calling it to have the
							required fonts available during the build.
	18		1/14/94	hernan	RemoveSpaces uses alphanumericp because our
							very own alphanumeric is not defined yet.
	19		2/14/94	sidney	rename descendants to knowndescendants, children to knownchildren
	20		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	21		2/28/94	hernan	Adding an "f" at the front of fonts whose names
							start with a number.
	22		2/28/94	hernan	Turning Cicons into GCable handles.
	23		3/2/94	till	Frob callers of macFileType ((setf file) of fileMedia.
	24		3/4/94	kleiman	font-num -> fontData
	25		3/8/94	Hernan	Making machandle handlers use the resource file
							utilities. Thus could get rid of every machandle
							defined on children of resource.
	26		3/23/94	Hernan	Restypes become strings.
	27		4/18/94	Hernan	Specializing machandle of QDPicture to cover
							the case when the Pict resides in a pict file.
	28		5/18/94	sidney	look for resource in project's swapFile instead of File
	29		5/18/94	sidney	same resource problem, this time a bit more sophisticated fix
	30		6/7/94	kleiman	Problem 1166864: added localVirtualProperties for Media which returns 'size'.
	31		6/15/94	Hernan	1168111: Removing size from the localVirtualProperties of media.
	32		6/23/94	sidney	support lazy creation of project's swapfile
	33		6/25/94	sidney	declared the fonts public, since their creation is hidden
	34		6/29/94	chip	case-saving-intern --> intern-symbol/intern-legal-varName/intern-legal-handlerName (for radar #107741)
	35		7/5/94	chip	restore-sk8-fonts now passes T for the forceCase optional arg to intern-legal-varName
	36		7/8/94	Hernan	Added boundsRect and size handlers of pict file
							media.
	37		7/11/94	Hernan	Flattening the media hierarchy (no more resource and FileMedia).
	38		8/5/94	sidney	1177107: findmychild handlers to facilitate storing unnamed media
	39 	 8/31/94	Hernan  	1181590: New font preservation mechanism.
	40 	 9/ 1/94	Hernan  	Chaging the symbol declaration form at the head
							of this file.
	41 	 9/ 1/94	Hernan  	Fixing restore-sk8-fonts.
	42 	 9/ 1/94	Hernan  	Fixing set fontname to actually set the slot.
	43 	 9/ 2/94	hernan  	Adding a force argument to setf objectname.
	44 	 9/ 2/94	sidney  	supress warning messages when recreating fonts
	45 	10/ 3/94	Hernan  	handle -> mediaData.
	46 	10/12/94	Hernan  	Now really: supress warning messages when recreating fonts.
	47 	10/26/94	sidney  	allow non-nil non-symbol class-names for sk8 classes
	48 	10/31/94	sidney  	use application file for resources for media if there is no project file
							typo in the last change
	49 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	50 	 2/10/95	sidney  	let media specify if resource is in project or application file
							got rid of fontnum-to-name because nothing calls it
	51 	 2/16/95	sidney  	readable argument names for initialize handler
	52 	 4/11/95	Hernan  	1238083: loadMedia now produces a nice error message
							when the file of the media is false.
	53 	 4/20/95	sidney  	better way of dealing with references to unnamed media objects in another project
	54 	 4/26/95	kend    	Changed 'fileName' to 'aFileName' to avoid (M)CL's broken implementation of lexical scoping {can't shadow globals}
	2  	 6/12/95	dy      	Make mediaData of Media private
	3  	 6/13/95	dy      	mistake in new Object with objectName "Media
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/16/96	sidney  	move definition of gc hook for stage's cursor until after stage is defined
	4  	 5/ 7/96	sidney  	Changes for new object system
	5  	 7/16/96	Brian   	adding newfsspec to here.
	6  	10/17/96	Hernan  	Fixed boundsrect of cicn to correctly get the pixmap.
	7  	11/14/96	Hernan  	Added importFromPtr and exportToPtr to support
						Apple events passing of media.
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
