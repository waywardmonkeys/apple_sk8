(in-package :graphics-system)


;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#|
Modification History

02-02-93 ruben without-interrupts on restore-sk8
01-31-93 ruben preserve [menu] took out reference to saved-instant-menu
01-26-93 ruben restore for resource and for sk8 project
01-22-93 ruben id -> objectName
11-01-92 ruben from restore-sk8 d46 build
09-26-92 ruben added optional close-p arg to preserve-clos-window (called by SAVE)
09-20-92 hernan changing this to work with MF VI
09-03-92 hernan making this stuff work in 1.0
06-13-92 ruben *null-ptr* in restore-sk8-globals and preserve-sk8-globals;
               preserve [actor] temporary :pict colors gone
05-27-92 adam  Fixed restore for project (uses "myWindows" instead of "get-windows")
05-12-92 ruben d29 conversion
03-29-92 ruben begun d29; preserve-records => preserve, restore-records => restore
               swept "get-" and "set-"
03-24-92 ruben new preserve-sk8-globals; modified preserve-sk8, restore-sk8-globals and
                   restore-records [project] to handle new blank window wdef;
                   restore-sk8 handles hidden menubar
02-15-92 ruben restore-records [project] now correctly restores window style information
01-19-92 ruben converted to MacFrames II
15:39:45  09-18-1991 RUBEN restore-sk8 uses *print-nice-doobie*
15:35:03  09-18-1991 RUBEN preserve-sk8 uses *print-nice-doobie*
11:21:10  09-13-1991 RUBEN  preserve-records [Actor] cleans up dead pict color handles
09-09-91  RUBEN *movies-initialized* nixed
12:21:38  08-28-1991 ADAM  preserve-sk8 & restore-sk8 turn off events to avoid nasty anomalies during preserve
12:35:29  08-20-1991 RUBEN preserve-records [Actor]
12:14:43  08-20-1991 RUBEN restore-sk8
10-01-91 adam  preserve-records & restore-records [card] ya no special-case preserve/restore connectors
08-17-91 ruben moved out simpled-edit-field and groupActor methods to their respective files
08-02-91 ruben/adam/entrails new LOAD-FIELD-RESOURCE & mod to restore-sk8 [editText]
07-30-91 ruben preserve-records & restore-records [card] were not preserv/restor[ing] connectors
07-29-91 ruben nuked text field caches !cached_textfieldhandle! !cached_textfield! on preserve-records
05-31-91 adam  restore-records for field unbound _tf_quiet error fixed

|#

;;; sk8-setEventMask -- sets the system mask so that SK8 can get keyUp events.

(defun sk8-setEventMask ()
  (%put-word (%int-to-ptr #x0144) #xffff))

(defun restore-systemEventMask ()
  (%put-word (%int-to-ptr #x0144) #xffef))

(push #'restore-systemEventMask mf::*quit-functions*) ;; OJO restoring the system mask...

;;; 1.0 *PRESERVED-RECORDS* -- a hash table that hashes on objects. For each object we keep a plist
;;;                         of things to be preserved.

(defvar *preserved-records* (make-hash-table :test #'eq))

(defun set-preserved-info (obj plist)
  (setf (gethash obj *preserved-records*) plist))

(defun get-preserved-info (obj)
  (gethash obj *preserved-records*))

(defun clear-preserved-info ()
  (clrhash *preserved-records*))

;;; Kludge to make the UI not show up on restore!!!

(defvar *no-ui-windows* nil)

;;; And now a kludge to cover the stage when sk8 gets restored.

(defvar *stage-covered-when-app-saved* nil)

;;; 1.0
;;; *WINDOWS-RECREATED* -- contains a list of the windows we have created during the 
;;;                       restore process.

(defvar *windows-recreated*)

;;; 1.0
;;; PRESERVE-CLOS-WINDOW -- this function is called to preserve the information of a clos
;;;                       window. A plist will all the data is returned.

(defun preserve-clos-window (me &optional (close-p t))
  (let ((clos-window (node-window me))
        plist)
    (setf (getf plist :my-actor-object) (slot-value clos-window 'my-actor-object))
    (setf (getf plist :owners) (slot-value clos-window 'owners))
    (setf (getf plist :want-idle) (slot-value clos-window 'want-idle))
    (setf (getf plist :first-click) (slot-value clos-window 'first-click))
    (setf (getf plist :locked) (slot-value clos-window 'locked))
    (setf (getf plist :depth) (slot-value clos-window 'depth))
    (setf (getf plist :dither) (slot-value clos-window 'dither))
    (setf (getf plist :movies) (slot-value clos-window 'movies))
    (setf (getf plist :dirty-nodes) (slot-value clos-window 'dirty-nodes))
    (setf (getf plist :window-loc) (view-position clos-window))
    (setf (getf plist :window-size) (view-size clos-window))
    (setf (getf plist :style) (slot-value clos-window 'style))
    (setf (getf plist :shown) (window-shown-p clos-window))
    (setf (getf plist :window-title) (window-title clos-window))
    (setf (getf plist :window-layer) (sk8::layer me))
    (setf (getf plist :floating) (sk8::floating me))
    ;; Closing the window.
    (when close-p
      (without-interrupts
       (sk8-close-window clos-window)
       ))
    ;; Returning the plist.
    plist))

;;; 1.0
;;; RESTORE-CLOS-WINDOW -- restores the window of a top level actor from an image.

(defun restore-clos-window (theActor &optional plist)
  (let* ((plist (or plist (get-preserved-info theActor)))
         (savedStyle (getf plist :style))
         (special-wdef (car (memq savedStyle '(sk8::sk8Window sk8::blank sk8::floating))))
         (mcl-style (unless special-wdef (translateStyle savedStyle)))
         (window-type (or (if special-wdef :single-edge-box (car mcl-style))
                          :single-edge-box))
         (floating (getf plist :floating nil))
         (clos-window (make-instance (if floating '*sk8-windoid* '*SK8-window*)
                        :view-position (getf plist :window-loc #@(10 50))
                        :view-size     (getf plist :window-size #@(300 200))
                        :window-title  (getf plist :window-title (sk8::objectString theActor))
                        :window-show   nil
                        :color-p       t
                        :window-type   window-type
                        :close-box-p   (if mcl-style (cadr mcl-style) t)
                        :depth         (getf plist :depth 0)
                        :drawEnabled nil)))
    ;; Window created.
    (when special-wdef (install-sk8-wdef (wptr clos-window) special-wdef))
    (setf (slot-value clos-window 'style) savedStyle)
    ;; Restore the windows slots.
    (setf (slot-value clos-window 'my-actor-object) (getf plist
                                                              :my-actor-object))
    (unless (slot-value clos-window 'my-actor-object)
      (error "Trying to restore a window with no actor!"))
    (setf (slot-value clos-window 'owners) (getf plist :owners))
    (setf (slot-value clos-window 'want-idle) (getf plist :want-idle))
    (setf (slot-value clos-window 'first-click) (getf plist :first-click))
    (setf (slot-value clos-window 'locked) 1)
    (setf (slot-value clos-window 'depth) (getf plist :depth))
    (setf (slot-value clos-window 'dither) (getf plist :dither))
    (setf (slot-value clos-window 'movies) (getf plist :movies))
    (setf (slot-value clos-window 'dirty-nodes) (getf plist :dirty-nodes))
    ;; Pushing the actor who owns a window into the *windows-recreated* var.
    (push (cons theActor (getf plist :window-layer)) *windows-recreated*)
    clos-window))
 
;;; 1.0
;;; PRESERVE-SK8-GLOBALS -- clears all globals that contain pointers to the heap.

(defun preserve-sk8-globals ()
  (setf *help-message-record* nil
        *movies-initialized* NIL
        mf::*savegrayregion* nil
        mf::*savembarheight* nil
        *SK8-window-wdef-handle* nil
        *shaped-window-wdef-handle* nil
        ;; *current-sounds* nil
        sk8dev::*quicktime-callbacks-to-activate* nil
        sk8dev::*active-movie-controllers* nil
        sk8dev::*active-movies-without-controllers* nil
        sk8dev::*objects-that-want-lowLevelIdle* nil
        ;; sk8dev::*movies-with-bogus-file* nil
        !*rgn-rect-initializer*! nil
        !*temp-region-pool*! nil
        !*temp-port-pool*! nil
        ;; *dirty-rect* nil
        *null-ptr* nil
        mf::*keyMap* nil
        *currentTla* nil)
  (preserve-trap-null-ptr-values)
  )

;;; Stops all clocks that are currently running. Leaves *clock-running* null.

(defun preserve-sk8-clock-state ()
  (dolist (aClock mf::*clocks-running*)
    (sk8::stop aClock)))

;;MCL3.0
(defun restore-sk8-setf-mouseloc ()
  (declare (special *$MTemp* *$RawMouse* *$Mouse* *$CrsrNew*))
  (setf *$MTemp* (%int-to-ptr #$MTemp))        ; Low Level interupt mouse location
  (setf *$RawMouse* (%int-to-ptr #$RawMouse))     ; Unprocessed mouse location
  (setf *$Mouse* (%int-to-ptr #$Mouse))        ; Processed mouse location
  (setf *$CrsrNew* (%int-to-ptr #$CrsrNew))      ; Non-zero if mouse has moved
  )

;;; RESTORE-SK8-INTERNALS -- restores very low level system variables and functions.
;;;                       So far it is required for the menus to work and for the
;;;                       setting of the mouseloc code.

(defun restore-sk8-internals ()
  ;; (restore-sk8-instant-menus)
  (sk8-setEventMask))

;;; 1.0
;;; PRESERVE-SK8 -- This is done in the simplest possible way: just iterate through
;;;               all objects defined and call their preserve method.

;; This used to do things like set the watch-cursor and run with events and interrupts off to keep bad stuff
;; from happening in the middle of saving the state of the system. But that didn't work out to well since events and
;; so on should not be allowed to happen after preserve-sk8 is called. All that protection was moved back into
;; the BuildStandalone function that calls this one so that it could be more encompassing

(defun preserve-sk8 ()
  (mf::clear-all-caches)
  ;; Preserving each project.
  (dolist (proj (sk8::knownchildren sk8::project))
    (sk8::preserve proj))
  ;; Clear the globals.
  ;; (preserve-sk8-fonts)
  (preserve-sk8-globals)
  (preserve-sk8-clock-state)
  )

;;; 1.0
;;; RESTORE-SK8-GLOBALS -- restores global variables which point at things
;;;                      in the heap.
;;MCL3.0

(defun restore-sk8-globals ()
  (flet ((restore-wdef (name)
           (with-open-res-file (nil (ccl::startup-pathname))
             (let ((theHandle (T_Get1NamedResource "WDEF" name)))
               (when (handlep theHandle)
                 (T_detachResource theHandle)
                 theHandle)))))
    (if (boundp 'sk8dev::*$MTemp*)
      (setq sk8dev::*$MTemp* (%int-to-ptr #$MTemp)))              ; Low Level interupt mouse location
    (if (boundp 'sk8dev::*$RawMouse*)
      (setq sk8dev::*$RawMouse* (%int-to-ptr #$RawMouse)))     ; Unprocessed mouse location
    (if (boundp 'sk8dev::*$Mouse*)
      (setq sk8dev::*$Mouse* (%int-to-ptr #$Mouse)))         ; Processed mouse location
    (if (boundp 'sk8dev::*$CrsrNew*)
      (setq sk8dev::*$CrsrNew* (%int-to-ptr #$CrsrNew)))       ; Non-zero if mouse has moved
    
    (setq *help-message-record* (newRecordGCPtr :hmmessageRecord :hmmHelptype 1)
          *SK8-window-wdef-handle* (restore-wdef "sedWDEF")
          *blank-window-wdef-handle*  
          #-ppc-target (ccl::make-wdef-handle shaped-window-wdef)
          #+ppc-target (restore-wdef "BlankWDEF")
          *bkgnd-window-wdef-handle* (ccl::make-wdef-handle bkgnd-window-wdef)
          ;; *sk8script-output* *top-listener*
          mf::*savembarheight* (%get-word (%int-to-ptr #$mbarheight))
          ;; *current-sounds* nil
          sk8dev::*active-movies-without-controllers* nil
          sk8dev::*objects-that-want-lowLevelIdle* nil
          sk8dev::*active-movie-controllers* nil
          !*rgn-rect-initializer*! (newRecordGCPtr :rect)
          !*temp-region-pool*! (new-region-pool)
          !*temp-port-pool*! (new-port-pool)
          ;; *temp-qd-rect* (newRecordGCPtr :rect)
          ;; *dirty-rect* (newRecordGCPtr :rect)
          *null-ptr* (%null-ptr)
          mf::*keyMap* (newRecordGCPtr :keyMap)
          *windows-recreated* nil
          )
    (initialize-trap-null-ptr-values)
    ))

(defun restore-sk8-menubar ()
  (when (SK8::menubar sk8::Stage)
    (sk8dev::install-menubar (SK8::menubar sk8::Stage))))

;;; If this returns anything but nil, an error has happened.

(defun safely-call-entering-stage (theActor)
  (catch :crash 
    (map-nodes* #'(lambda (subActor)
                        (multiple-value-bind 
                          (dummy errorp) (ignore-errors (sk8::enteringStage subActor))
                          (declare (ignore dummy))
                          (when errorp
                            (throw :crash subActor))))
                    theActor)))

;;; Given a subset of *windows-recreated*, this function

(defun restore-project-windows (windowPairs)
  ;; Unlock all the windows and draw them.
  (dolist (tla (sort windowPairs #'(lambda (tla1 tla2) (< (cdr tla1) (cdr tla2)))))
    (setf tla (car tla))
    (sk8::unlock tla)
    ;; Call enteringStage and make sure actor's state is ok.
    (if (safely-call-entering-stage tla)
      ;; An enteringStage error has happened! Clean up.
      (sk8dev::abort-attach-to-stage nil tla)
      ;; Everything ok: make the window visible.
      (sk8-bring-up-new-window (node-window tla) nil t))
    ))

;;; 1.0
;;; RESTORE-SK8 -- restores SK8 in the following order:
;;;              1. globals
;;;              2. proto picts (not anymore since obsoleted!)
;;;              3. SK8 project
;;;              4. Interface
;;;              5. All projects (first sk8 and ui, then the rest in order of creation)

(defun restore-sk8 ()
  (let ((*windows-recreated* nil))
    (declare (special *windows-recreated*))
    (without-interrupts ; *** wouldn't be necessary if user couldn't Cmd-. !!!
     (without-events
      (with-cursor *watch-cursor*
        (let ((projects (reverse (sk8::knownChildren sk8::project)))
              ;; This prevents graphical updates to the menubar while we come up.
              (*menubar-frozen* t))
          (with-splash-screen 
            (sk8dev::restore-file-environment)
            (restore-sk8-globals)
            ;; (restore-sk8-instant-menus)
            (restore-sk8-internals)
            ;; (restore-sk8-fonts)
            ;; Restoring SK8
            (when (member sk8::sk8 projects) ;; this had better be true
              (sk8::restore sk8::sk8)
              (when *stage-covered-when-app-saved*
                (setf (sk8::covered sk8::Stage) t))
              (setf projects (delete sk8::sk8 projects)))
            ;; Restoring the UI
            (when (and (boundp 'sk8::ui) (member sk8::ui projects))
              (sk8::restore sk8::ui)
              (setf projects (delete sk8::ui projects)))  ;;;with splash ended here..
            ;; Restoring other projects.
            (dolist (proj projects) 
              (sk8::restore proj))
            ;; Clear the hash table.
            (clear-preserved-info)
            (restore-sk8-menubar)
            ))
        (draw-menubar-if)
        )))))

#|
	Change History (most recent last):
	2	5/11/93	Brian Roddy	Added (restore-sk8-constraints) to restore-sk8 in order to restore the condor C environment.
	3	5/12/93	Brian Roddy	
	4	5/24/93	Hernan	All sorts of work to get the restoring of menus to
				work. Mainly: (1) make menus restore all its items
				and submenus, (2) make sure menus restore their
				actor parts as well.
	5	5/25/93	Hernan	Took the print out of restore of menu.
	6	6/1/93	Hernan	Removed want-mouseWithin slot of *sk8-window*.
	7	6/1/93	Hernan	
	8	6/25/93	Hernan	The Great Renaming of 93.
	9	6/29/93	Hernan	restore-sk8 now calls mf::install-menubar to install the menubar of the stage.
	15	7/10/93	chip	qualified occurrences of SK8:menubar
	16	8/13/93	kleiman	preserve-sk8 animated clock cursor feedback and
				restore-sk8-globals inits *current-sounds*
	17	8/17/93	kleiman	restore-sk8-globals typo in setq
	18	8/31/93	hernan	*new-region* -> *original-dirty-region*.
	19	8/31/93	hernan	Changed restore of simpleText to make the new
				field work.
	20	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	21	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	22	10/8/93	hernan	PrepareForStage now called to get the actors ready
				for addition to the stage.
	23	10/8/93	hernan	PrepareForStage has to be called AFTER the restore
				handler is done with its stuff. Restore of actor is
				now in charge of calling it after a subactor has been
				restored.
	24	10/11/93	hernan	Restoring the background window WDEF
	25	10/18/93	kleiman	restore-sk8 egc explicitly turned off if no ptable
	26	10/20/93	hernan	Clearing the windows recreated var before we
				start restoring.
	27	10/20/93	kleiman	open-application-document working at startup
	28	10/21/93	kleiman	preserve-sk8 clear-clos-caches -> clear-all
	29	10/22/93	kleiman	took out calls to updateCursor
	30	10/22/93	kleiman	preserve-clos-window (it's HIS fault!)
	31	10/22/93	kleiman	sk8Project -> SK8
	32	10/25/93	kleiman	*application* not used
	33	11/1/93	kleiman	d4 store changes
	34	11/1/93	hernan	Macpathname -> file.
	35	11/1/93	kleiman	restore for SK8 project removed
	36	11/5/93	kleiman	Accomodating scriptEditText's needs.
	37	11/5/93	hernan	preserve-actor does not dispose the owned region
				when it is not there.
	38	11/12/93	kleiman	Adding restore-file-environment to all the stuff
				restore-SK8 does.
	39	11/19/93	hernan	Added *oldWindow* variable. Clearing it for
				preserve.
	40	11/22/93	hernan	Removing autotabs slot from *sk8-window*.
	40	11/22/93	hernan	Second try!
	41	11/24/93	hernan	Restoring the layer of windows. And much more!
				Preserve now gets done on a project by project
				basis. Restore also.
	42	11/24/93	chip	fixed preserve/restore for ScriptEditText (now deals with wptr)
	43	11/24/93	hernan	Fixing restore of scriptEditText to set the wptr only
				when there is one.
	44	11/29/93	hernan	Adding the pool of ports to the things to preserve.
	45	11/29/93	chip	restore for ScriptEditText wasn't doing anything for object's not on-stage; now it does what it needs to
	46	11/29/93	kleiman	
	47	11/29/93	chip	restore for ScriptEditText now resets the frec's fr.selrgn by hand (since the MCL code can't seem to do it)
	48	12/2/93	hernan	Adding a stupid variable that controls whether the
				UI brings up its windows when it restores.
	49	12/2/93	hernan	Now calling it the right thing.
	50	12/3/93	kleiman	Restore of quickTimeMovie now works with files!
	50	12/3/93	kleiman	Fixed restore of quicktimemovie to use files.
	51	12/3/93	kleiman	frec-delay-cursor-off for restore scriptedittext
	52	12/7/93	kleiman	restore-sk8-globals initializes $mtemp, $rawmouse, $mouse and $crsrnew
	53	12/7/93	kleiman	Use SK8's creator for saved application file
	54	12/21/93	hernan	Making sure the regions are there before 
				disposing them.
	55	12/21/93	hernan	Checking for memory errors when making new
				regions.
	56	1/10/94	hernan	Adding code to reconstitute the fonts on the way
				up (when sk8 restores).
	57	1/10/94	hernan	moving restore-sk8-fonts to 15-media (we need
				to call this function during the build process to 
				have the fonts available).
	58	1/10/94	hernan	Ooops. Fixing typo.
	59	1/11/94	hernan	self -> me
	60	1/20/94	dy	Moved the QuickTime stuff from this file to a new
				file ...;Movies;quicktime-save.lisp
	61	2/12/94	kleiman	name changes
	62	2/14/94	sidney	rename children to knownchildren
	63	2/15/94	kleiman	Fixed restore-sk8-menubar not to call optional
				argument of install-menubar.  This arg was 
				apparently removed by Ruben.
	64	2/18/94	dy	changed name of *movies-are-playing-p* to *active-movies-without-controllers* and added *active-movie-controllers* global
	65	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	66	2/21/94	hernan	window -> sk8::window.
	67	2/25/94	hernan	Using symbols instead of keywords for options!!!
	68	2/25/94	kleiman	Preserve of menu calls next method!
	69	2/28/94	hernan	Now using GCable mac things.
	69	2/28/94	hernan	Second try!
	69	2/28/94	hernan	Third try!
	70	2/28/94	dy	add calls to deal with %null-ptrs in the trap stuff
	71	3/1/94	kleiman	handle some pointers correctly
	72	3/2/94	Hernan	Porting to Fred 3.0
	73	3/3/94	Hernan	The great handler argument name renaming of 94!
	74	3/4/94	kleiman	font-num -> fontData
	75	3/6/94	Hernan	Adding a variable to remember whether the stage
				was covered when we saved the application.
	76	3/9/94	Hernan	Fixed bug in preserve-clos-window which crashed
				SK8 when trying to save an appication with non
				rectangular windows on the stage.
	77	3/10/94	chip	obsoleted Stage's real-contents property
	78	3/31/94	Hernan	Adding a function that stops running clocks when
				we save an image.
	79	4/6/94	sidney	remove conditional enabling of egc: it was wrong and is being done correctly in startup-sk8
	80	5/4/94	Hernan	Adding the splash screen to restore sk8.
	81	6/9/94	chip	removed definition of restore-sk8-instant-menus; put it into "instant-menus.lisp" (to avoid duplicating the definition of the menutrackproc!)
	82	6/14/94	Hernan	The Stage is now covered by with-splash-screen.
	83	6/23/94	sidney	without-events macro changed, so force recompile of this file
	84	7/19/94	kleiman	1175301 restore-clos-window translateStyle added
	85	7/20/94	Hernan	1175599: the code that gets the WDEF resource
				need to detach the resource!
	86	7/20/94	Hernan	Getting rid of extranous beep.
	87	7/25/94	Hernan	1175911: making actors remember their keyTarget
				when they are not on the Stage. This gets rid of
				the mf::curKey slot of windows.
	88	8/10/94	chip	removed some really outdated comments
	89 	 8/31/94	Hernan  	Added *currentTla*.
	90 	 8/31/94	Hernan  	1181590: preserving session dependent fonts.
	91 	 8/31/94	Hernan  	Getting rid of font restoring method.
	92 	10/17/94	dy      	ownedRegion is now in p-list
	93 	10/28/94	sidney  	removed save-sk8-application, an obsolete function
	94 	10/31/94	sidney  	load wdefs from the application file if SK8 project file does not exist
	95 	11/ 1/94	Hernan  	Making restore of EditText restore the updateRgn.
	96 	11/ 1/94	Hernan  	Ooops. The world is not ready for this yet!
	97 	11/ 4/94	Hernan  	Allowing the new EditText (that draws into gWorlds) to be saved and preserved.
	98 	11/ 9/94	Hernan  	Restore of EditText has to create the new selRgn.
							This is required in order to draw the field into
							other ports (for capturePicture).
	99 	11/16/94	sidney  	folder containing SK8 project file changed to SK8 Resources
	100	11/22/94	dy      	restore-actor dirties the bounds, frame and fill
	101	 1/11/95	Hernan  	Covers the stage at the end of restore-sk8.
	102	 1/17/95	till    	trapwrapper for get1namedresource
	103	 1/20/95	Hernan  	restore-wdef has to detach the resource.
	104	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	105	 2/10/95	sidney  	use safer resource file functions
	106	 2/16/95	sidney  	restore wdef from application file, no more sk8 resources;sk8 file
	107	 3/ 3/95	Hernan  	No need for unwind-protect in restore-sk8.
	108	 3/13/95	dy      	Set sk8dev::*movies-with-bogus-file* to nil when saving
	109	 3/14/95	Hernan  	Adding new windowStyles that do not have a close
							box.
	110	 3/17/95	till    	trap-wrapping, #_detachResource in  restore-sk8-globals
	111	 3/18/95	rod     	Fixing restore-clos-window to deal with floating
							windows.
	112	 3/19/95	sidney  	startup-pathname moved to ccl package when put in build part 1
	113	 3/24/95	Hernan  	1232244: forgot to save whether a window is a 
							floating window. Adding code to preserve-clos-window and 
							restore-clos-window.
	114	 3/25/95	sidney  	restore sk8 project first, then restore covered stage, then ui, then other projects. startup looks cleaner that way.
							forceredraw of actors that are on stage after they are restored. Some windows seem to need that to display properly
	115	 3/27/95	sidney  	allow restire sk8 to work ok when there is no ui project
							move without-event and watch cursor stuff out of preserve-sk8 to encompass more of buildstandalone
	116	 3/28/95	dy      	Introduce preserveLikeSave function, make restore of object return True
	117	 3/31/95	Hernan  	1234693: restore-project-windows calls add-one-node-to-
							window to prepare the actors for attachment to the Stage.
							This ends up calling enteringStage.
	118	 4/ 3/95	Hernan  	restore-sk8 now removes the splash window before the
							user projects get loaded.
	119	 4/ 6/95	sidney  	move forced redraw of new windows on restore out to correct place, i.e., in sk8-bring-up-new-window
	120	 4/24/95	Hernan  	1228501: implementing 2 real layers: windows and windoids.
	3  	 6/23/95	Hernan  	1250076: preventing the "menu dance" from showing through.
	4  	 8/ 2/95	Hernan  	Preserve-sk8-globals also clears *objects-that-want-lowLevel-Idle*.
	2  	 4/ 9/96	Hernan  	Changing !*temp-port-pool*! to be a pool object. (also 
						changing the regions one).
	3  	 4/16/96	Hernan  	Getting rid of *temp-region*. Also got rid of *sk8-penState*
						and *original-dirty-region*.
	4  	 4/18/96	Hernan  	Restoring the menubar of the Stage. If it is not nil, it is 
						assumed that the menus have been loaded and we may 
						proceed.
	5  	 4/26/96	Hernan  	Fixing the way restore-clos-window figure out the window
						style of the new window (and->or).
	6  	 5/ 3/96	Hernan  	Showing the splash screen again.
	7  	 7/23/96	Hernan  	When PPC, restore blank wdef also. When 68K use WDEF code.
	8  	 9/ 3/96	Hernan  	Just experimenting...
	9  	10/ 7/96	sidney  	disposing-clos-window and selecting-clos-window no longer used
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
