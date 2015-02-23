;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

;;; Save Project Store
;;;

(in-package :SK8Development)

;;; *debug* -- if true, then print debugging info on listener
;;;
(defparameter *debug* nil)
(defun debug-msg (format-string &rest args)
  (if *debug*
    (ignore-errors (apply #'format t format-string args))))

;;; *toplevel-actors* -- contains a list of all toplevel actors which
;;;   are have their own windows (i.e., are attached to the stage).
;;;   Each item in the list is of the form (actor . window-layer-number)
;;;   so that at the load's finalization the windows are created in the right layer order.
;;;
(defvar *toplevel-actors* nil)

;;; *virtual-menuitem-properties* -- names of virtual property states that must be saved with a menuitem
;;; (obsolete - not used anywhere)
;;(defvar *virtual-menuItem-properties* '(enabled commandkey checkmark text textStyle))

;;; sk8::*actors-contained-in-other-projects* -- special keeps list of all loaded actors whose container
;;;     exist in a different project. This allows us to reconstitute the containment hierarchy.
;;;
(defvar sk8::*actors-contained-in-other-projects*)

;; *stage-menubar* really should be something local to a project. It is the menubr to attach
;; to the stage when a project is loaded
(defvar sk8::*stage-menubar* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initializeFromStore
;;;
;;; initializeFromStore used to be called immediately after the object was created and its properties initialized.
;;; That led to problems when the function referred to an object that had not yet been loaded. Some handlers pushed something on to
;;; a finalization list for the particular object to deal with that, but it is difficult to catch when that is necessary, leading to bugs.
;;; So instead, initializeFromStore will be called on each object in the project that is being loaded during a finalization phase.
;;; There is something that has to be done right after object creation, and that is being put into an earlyInitializeFromStore handler.
;;; There is one defined on Object, and it is expected that there will be no need to define it on any others.
;;; earlyInitializeFromStore cannot rely on the existence any other object in the project unless it is part of a data structure in this object's properties

;;; IMPORTANT: It is essential that all initializeFromStore handlers call the next handler
;;;

(define-handler earlyInitializeFromStore (Object)
  (declare (special ps::*project-being-loaded*))
  
  (let ((id (sk8::sk8_id me)))

    (when (numberp id)   ;; an id number must be uniquely generated at run time, can't used the one that was saved
      (setf (slot-value me 'sk8::sk8_id) (setf id (mf::get-new-object-oid))))
    
    (when (mf::named-object? me) ;; If named, then bind to constant, maybe publish the symbol
      (ccl::%defconstant id me)
      )
    
    ;; Add to project's object table:
    (setf (gethash id (sk8::objecttable ps::*project-being-loaded*)) me)
    
    (debug-msg "~%Early initialized ~a from store" id)
    )
  t)

;;; initializeFromStore -- links object to parents and adds it to the object table
;;;
(define-handler initializeFromStore (Object)
  (let ((myproj (sk8:project me))
        (priv? (sk8:private me))
        (named? (mf::named-object? me)))
    (when named?
      (dolist (parent (parents me))
        (pushnew me (slot-value parent 'knownchildren)))
      (unless priv?
        (mf::publish-project-symbol (sk8_id me) myproj)))
    (unless priv?
      (dolist (slt (class-direct-instance-slots (class-of me)))
        (let ((name (car slt)))
          (unless (mf::private-property-p me (car slt))
            (mf::publish-project-symbol name myproj))))))
  (debug-msg "~%Initialized ~a from store" me)
  t)

;;; initializeFromStore/Actor -- initializes the actor's regions
;;;
(define-handler initializeFromStore (Actor)
  (declare (special *toplevel-actors* sk8::*actors-contained-in-other-projects*))
  (call-next-method)
  (let ((flags (gs:node-flags me)))
    ;; hack flags so we can store them as lists in sk8script save
    (when (and flags (listp (cdr flags)))
      (setf flags (cons (first flags) (second flags)))
      (setf (gs:node-flags me) flags))
    (setf (fillregion me)   (T_NewRgnGC)
          (frameregion me)  (T_NewRgnGC)
          (boundsregion me) (T_NewRgnGC))
    (gs:boundsDirty! flags)
    (gs:frameDirty!  flags)
    (gs:fillDirty!   flags)
    (cond ((gs:hasWindow? flags)
           (push (cons me (getf (gs:node-window me) :wlayer)) *toplevel-actors*)
           (setf (gs:node-window me) (load-clos-window me)))
          (t ;; If contained in actor in another project, then select it for post-processing:
           (let ((container (container me)))
             (when (and container
                        (slot-boundp container 'sk8::project)
                        (neq (project container) (project me)))
               (pushnew me sk8::*actors-contained-in-other-projects*)))))
    (if (gs:cachesPixmap? flags)
      (gs:pixmapDirty! flags))))

;;; *** any Connector initializeFromStore (checking with Hern‡n)

(defmethod InitializeFromStore ((me ccl::buffer-stream) &key)
  (declare (ignore me)))

(define-handler initializeFromStore (sk8::Menubar)
  (declare (special sk8::*stage-menubar*))
  (call-next-method)
  (if (onStage me) (setf sk8::*stage-menubar* me)))

;;make sure that menus and menuitems are really installed where they say they are
(define-handler initializeFromStore (sk8::Menu)
  (let ((installedthing (installedin me)))
    (cond ((null installedthing)
           (setf (sk8::menubar me) nil
                 (sk8::menu me) nil))
          ((inheritsFrom installedthing SK8::Menubar)
           (unless (eq installedthing (sk8::menubar me))
             (setf (sk8::menubar me) installedthing)))
          ((inheritsFrom installedthing SK8::Menu)
           (unless (eq installedthing (sk8::menu me))
             (setf (sk8::menu me) installedthing)))))
  ;; since it is an actor. 
  (call-next-method))

(define-handler initializeFromStore (sk8::MenuItem)
  (let ((installedmenu (installedin me)))
    (unless (eq installedmenu (sk8::menu me))
      (setf (sk8::menu me) installedmenu))
    ))

;;; initializeFromStore called on the MCL menu classes because they're not descendants of Object
;;;
;;; Note: could have it defined for standard-object but that will make things harder to debug
;;;
(defmethod initializeFromStore  ((me gs:*sk8-menu*) &key)
  )

(defmethod initializeFromStore  ((me gs:*sk8-menu-item*) &key)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SaveToStore
;;;

(define-handler saveToStore (Object property)
  (slot-value me property))

(define-handler localPropertiesToSaveSpecially (Object)
  '())

(define-handler inheritedPropertiesToSaveSpecially :private (Object)
  (let ((props '()))
    (dolist (each (parents me))
      (setq props (union props (propertiesToSaveSpecially each))))
    props))

(define-handler propertiesToSaveSpecially (Object)
  (union     (localPropertiesToSaveSpecially me)
         (inheritedPropertiesToSaveSpecially me)))

(define-handler localPropertiesToSaveAsFalse (Object)
  '())

(define-handler inheritedPropertiesToSaveAsFalse :private (Object)
  (let ((props '()))
    (dolist (each (parents me))
      (setq props (union props (propertiesToSaveAsFalse each))))
    props))

(define-handler propertiesToSaveAsFalse (Object)
  (union     (localPropertiesToSaveAsFalse me)
         (inheritedPropertiesToSaveAsFalse me)))

(define-handler propertiesToSaveNormally (Object)
  (nset-difference
   (nset-difference (realProperties me)
                    (propertiesToSaveAsFalse me))
   (propertiesToSaveSpecially me)))

(define-handler propertiesToSaveNormallyIncludingInternals :private (Object)
  (nset-difference
   (nset-difference (mapcar #'car (class-instance-slots (class-of me)))
                    (propertiesToSaveAsFalse me))
   (propertiesToSaveSpecially me)))

;;; finalizeActorHierarchy I'LL MAKE IT SO WHEN I'M SURE THAT IT IS A RELIABLE THING TO DO
;;; SaveToStore/Actor -- saves a low-level SK8 window object in a nice format for later reconstitution
;;;
;;; *** this might not be necessary because wood-slot-value for *sk8-window* is now doing this work
;;;
(define-handler localPropertiesToSaveSpecially (Actor)
  (if (eq me Actor)
    '(sk8::flags OSWindow)  ;; flags is only made special for save as text, but that doesn't hurt save to store
    '()))

(define-handler saveToStore (Actor property)
  (if (eq property 'OSWindow)
    (if (gs:hasWindow? (gs:node-flags me))
      (let ((plist nil)
            (clos-window (gs:node-window me)))
        ;; window layer to restore window in right layer
        (setf (getf plist :wlayer) (layer me))
        ;; curkey.
        ;; (setf (getf plist :curKey) (slot-value clos-window 'gs:curKey))
        ;; owners.
        (setf (getf plist :owners) (slot-value clos-window 'gs:owners))
        ;; want-Idle.
        (setf (getf plist :want-idle) (slot-value clos-window 'gs:want-idle))
        ;; first-click.
        (when (slot-value clos-window 'gs:first-click) 
          (setf (getf plist :first-click) t))
        ;; locked.
        (when (slot-value clos-window 'gs:locked)
          (setf (getf plist :locked) 1))
        ;; depth.
        (setf (getf plist :depth) (slot-value clos-window 'gs:depth))
        ;; dither.
        (setf (getf plist :dither) (slot-value clos-window 'gs:dither))
        ;; movies.
        (setf (getf plist :movies) (slot-value clos-window 'gs:movies))
        ;; window-loc, window-size, style and shown.
        (setf (getf plist :window-loc) (view-position clos-window))
        (setf (getf plist :window-size) (view-size clos-window))
        (setf (getf plist :style) (slot-value clos-window 'gs:style))
        (when (window-shown-p clos-window) (setf (getf plist :shown) t))
        plist)
      nil) ; has no window, so no need to save window info
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous load-time functions
;;;

;;; initializeMenubars -- installs any menubar that must go into the stage
;;;                 This must be done at the end of a project load.
;;;
(defun initializeMenubars ()
  (declare (special sk8::*stage-menubar*))
  (when sk8::*stage-menubar*
    (setf (SK8::menuBar Stage) sk8::*stage-menubar*)
    (install-menubar sk8::*stage-menubar*))) ; *** this call should not be necessary!!! - Ruben

#|
initialize for stefield creates a testyl into the field's hte slot
the default backcolors are added
_teautoview and _tedeactivate are called on the style
calls set-field-size on field using size
calls set-field-sel-modes on field using '(:empty :word :line) default arg

initargs for new-field include
  :initialize-stylerec - the field's style (a macptr--i.e., textStyle record)
        if not provided, then the default TEText style is used
        
  :size - an mcl point (equal to the actor's (boundsrect - framerect))


SimpleText initialize
 calls new-field to create a new field (see above) with size arg passed
 field's wptr set to actor's node-window
 actor's :fld property set to field
 ownedRegion created
 :rszing initialized to nil (true only while resizing field!)
 set-field-owner called with 'field-force-update and no second function
 set-field-sel-modes called with click-chunktypes of parent
 textfront set with that of parent
 update-field-if-necessary called on field to make sure there's no garbage in them
|#


;;; load-clos-window -- recreates the clos window for this toplevel actor with a window.
;;;
(defun load-clos-window (theActor)
  (let* ((plist (gs:node-window theActor))
         (savedStyle (getf plist :style))
         (special-wdef (car (memq savedStyle '(sk8Window blank floating))))
         (mcl-style (unless special-wdef (translateStyle savedStyle)))
         (clos-window (make-instance     (if (floating theActor)
                                           'gs:*sk8-windoid* 
                                           'gs:*SK8-window*)
                        :my-actor-object  theActor
                        :view-position   (getf plist :window-loc #@(10 50))
                        :view-size       (getf plist :window-size #@(300 200))
                        :window-title    (if (inheritsFrom theActor rectangle)
                                           (windowTitle theActor)
                                           (objectString theActor))
                        :window-show     nil
                        :color-p         t
                        :window-type     (if special-wdef :document-with-zoom (car mcl-style))
                        :close-box-p     (if mcl-style (cadr mcl-style) t)
                        :depth           (getf plist :depth 0)
                        :dither          (getf plist :dither)
                        :drawEnabled     nil)))
    ;; Window created.
    (when special-wdef (gs:install-sk8-wdef (wptr clos-window) special-wdef))
    ;; style.
    (setf (slot-value clos-window 'gs:style) savedStyle)
    ;; curkey.
    ;; (setf (slot-value clos-window 'gs:curKey) (getf plist :curKey nil))
    ;; owners.
    (setf (slot-value clos-window 'gs:owners) (getf plist :owners))
    ;; want-Idle.
    (setf (slot-value clos-window 'gs:want-idle) (getf plist :want-Idle))
    ;; first-click.
    (setf (slot-value clos-window 'gs:first-click) (getf plist :first-click))
    ;; locked.
    (setf (slot-value clos-window 'gs:locked) (getf plist :locked))
    ;; movies.
    (setf (slot-value clos-window 'gs:movies) (getf plist :movies))
    
    clos-window))

;;;  load-in-containment-hierarchy -- reconstitutes an actor from toplevel actor on down
;;;   me - the actor object
;;;   clos-window - if non-nil, then me's a toplevel actor--else window is the actor's window
;;;
(defun sk8::cacheActorWindows (me clos-window)
  
  ;; 1. Cache this actor:
  (setf (gs:node-window me) clos-window)
  (gs:dirty-one-node (gs:node-flags me))
  (enteringStage me) ;; This makes Freds restore their wptrs!
  
  ;; 2. Restore the contents of this actor.
  (dovector (subnode (gs:node-contents me))
    (if subnode (sk8::cacheActorWindows subnode clos-window))))

;;; This will be called on actor's whose contents are in other projects. 

(defun ensure-translucency-counter-is-ok (anActor)
  (let ((result 0))
    (gs:docontents (subActor anActor)
      (unless (gs:opaque? (gs:node-flags subActor))
        (incf result)))
    (unless (eql (gs:node-translucentContents anActor) result)
      (setf (gs:node-translucentContents anActor) result))))

;;; finalizeEnvironment -- This is called after all objects and their nodes (if actors) have been recreated. The task is to 
;;;                   restore the state of the project and then bring up all the windows.
;;; This used to only process some special cases that were set up by some objects' intializeFromStore handlers.
;;; It turns out that it is too hard to know when there is such a special case, so instead initializeFromStore is called here on
;;;  every object that is in the project that is being loaded, and there may be no need to push things on finalization lists
;;;
(define-handler sk8::finalizeEnvironment (Project)
  (declare (special *toplevel-actors*
                    sk8::*actors-contained-in-other-projects*))
  (gs:without-events
    (let (*windows-recreated*)
      (declare (special *windows-recreated*))
      (without-interrupts

       ;; 0. Initialize all objects that have been loaded into the project (do we want to call (sk8::tickEventClock) in between each object?)

       (mapProjectObjects me #'(lambda(obj) (sk8::tickEventClock) (initializeFromStore obj)))

       ;; 1. Restore QuickTimeMovies that weren't found. Only do this if the movies are loaded (think runtime).
       (when (fboundp 'finalizeQuickTimeMovies)
         (finalizeQuickTimeMovies #'initializeFromStore))
       
       ;; 2. Cache window in all actors
       (dolist (tla *toplevel-actors*)
         (sk8::tickEventClock)
         (sk8::cacheActorWindows (car tla) (gs:node-window (car tla))))
       
       ;; 5. Set objects contained by objects in other projects into their containers:
       (let (container)
         (dolist (obj sk8::*actors-contained-in-other-projects*)
           (setq container (container obj))
           (withActorLocked (container)
             (setf (container obj) nil          ; officially remove it from its container
                   (container obj) container)   ; cleanly add it to its container
             ;; KLUDGE: the stuff above is EVIL for a number of reasons
             ;; which Rodrigo and me can document if hard pressed to do so.
             (ensure-translucency-counter-is-ok container))))
       
       ;; 6. Now ready to bring up windows in their original order and draw everything:
       (when *toplevel-actors*
         (dolist (tla (setq *toplevel-actors*
                            (sort *toplevel-actors* #'(lambda (tla1 tla2) (< (cdr tla1) (cdr tla2))))))
           (sk8::tickEventClock)
           (setq tla (car tla))
           ;; Activate the current key.
           (setf (keyTarget tla) (keyTarget tla))
           (unlock tla)
           ;; making the window visible.
           (gs:sk8-bring-up-new-window (gs:node-window tla))))
       
       ;; 7. Show them in the right order
       (do ((tla *toplevel-actors* (cdr tla))
            (i 1 (1+ i)))
           ((null tla))
         (sk8::tickEventClock)
         (setf (layer (caar tla)) i))
       
       ;; 8. Initialize menubars on stage:
       (sk8::initializeMenubars)
       (sk8::tickEventClock)
       )))
  ;; 9. Return t for succeess or die
  (sk8::clearSystemStateForProjectLoad)
  t)
  
#|

;;; load-field -- restores a text field's internals
;;;   me - any descendant of SimpleText
;;;
(defun load-field (me)
  (let* ((properties (gs:node-properties me))
         (refnum (getf properties :textResID))
         (plist (getf properties :fld))
         (field (new-field))
         (clos-window (gs:node-window me))
         width height
         (selrange (getf plist :selRange '(0 0))))
    (setf (getf properties :fld) field)
    (if (inheritsFrom me edittext)
      (set-field-owner field me 'field-force-update 'update-fieldPartners)
      (set-field-owner field me 'field-force-update 'empty-function))
    (if clos-window (add-field-to-window field clos-window))
    (sk8-multival-setf (width height) (rect-size (gs:node-logicalBoundsRect me)))
    (gs:fdecf width (gs:fdouble (pt-h (gs:node-frameSize me))))
    (gs:fdecf height (gs:fdouble (pt-v (gs:node-frameSize me))))
    (set-field-size field (gs:SK8Coords-to-point width height) t)
    (set-field-sel-modes field (click-chunktypes me))
    (set-field-autowrap field (getf plist :autowrap t))
    (if (inheritsfrom me ScriptEditText)
      (setf (wptr (editData me)) (when clos-window (wptr clos-window)))
      (unless (and refnum
                   (load-text-resource me refnum :field field))
        (with-rgb (theRgbColor (mcl-color (getf properties :text-color black)))
          (set-field-typeStyle field
                               :font (fontData (getf properties :text-font))
                               :size (getf properties :text-size)
                               :face (getf properties :text-style)
                               :color theRgbColor))))
    (set-field-selection field (first selrange) (second selrange))
    (when (eq me (keyTarget (sk8::window me)))
      (activate-field field (getf plist :activated)))
    (hilite-field-selection field (getf plist :sel-hilited))
    (update-field-if-necessary field)
    field))
|#

;;; save-pixmap-resource -- saves pixmap part of pixelmap object into the
;;;                     project's resource file as a pict
;;;  obj - a pixelmap object
;;;
;;; Returns resource ID of saved pixmap resource
;;;
;;; This will create a swapfie for the project if one does not exist, thus will not write to the saved application even if
;;;    the pixmap originally came from there

(defun save-pixmap-resource (obj)
  (when (file obj)
    (let ((gWorld (mediaData obj))
          (resourceid (resourceid obj))
          (proj (project obj))
          (resourcetype "PICT")
          pictH helperGWorld)
      ;; Only do this if the pixel map actually has a gWorld to save.
      (when (macptrp gWorld)
        ;; NOTE: when the origin of the source gWorld is not 0, copying the pixmap onto
        ;; itself does not work. Thus, we need to copy it to another gWorld, but with
        ;; its top left set to 0. The following piece of code does that.
        ;; [1] Initialize helper gWorld for this size.
        (sk8-multival-bind (width height) (size obj)
          (initialize-snapshot-pixmap swatchTempSnapShotMedia nil :width width :height height))
        (setf helperGWorld (mediaData swatchTempSnapShotMedia))
        ;; [2] Blit the image to a similar rect whose topleft is at 0.
        (gs:with-offscreen-gWorld  
         (helperGWorld)
         (gs:with-gWorld-rectangle 
          (gWorld-rect helperGWorld)
          (let ((sourcePixm (#_getGWorldPixmap gWorld))
                (destPixm (#_getGWorldPixmap helperGWorld)))
            (setf pictH (#_OpenPicture gWorld-rect))
            (gs:Copy-Bits sourcePixm destPixm 
                                (gs:gWorld-rectangle gWorld) gWorld-rect 8 (%null-ptr))
            (#_ClosePicture))))
        ;; [3] Do resource stuff.
        (when (and (handlep pictH)
                   (not (%null-ptr-p pictH))
                   (> (rref pictH :Picture.picFrame.right) (rref pictH :Picture.picFrame.left)))  ; We've got a handle
          (withResourceFile ((ps::create-swapfile-if-needed proj))
            ;; 1. Remove an old resource with the same ID
            (when resourceid
              (sk8-multival-bind (h theError)
                                 (T_Get1Resource resourcetype resourceid :notFoundOK t)
                (declare (ignore theError))
                (when h 
                  (T_RemoveResource h)
                  (#_disposHandle h)))))
          ;; 2. Save the resource in the project's file
          (let ((res (copyResourceToProject proj pictH resourcetype)))
            (setf (resourceid obj) res)
            (setf (file obj) (swapfile proj))
            (#_KillPicture pictH)
            res)))) ; MUST return resource ID
    ))
    
;;; REMOVED restore-pixmap-data.

#|
	Change History (most recent last):
	2	6/1/93	Hernan	load-in-containment-hierarchy restores the 
				window BEFORE all node data is restored.
	3	6/1/93	Hernan	Removed want-mouseWithin slot from *sk8-window*.
	4	6/2/93	hernan	Oooops! The node's window has to be set AFTER we
				get the right container in load-in-containment-hierarchy.
	5	6/25/93	kleiman	forceUpdate -> forceRedraw
	6	6/26/93	kleiman	(number -> (layer
	7	6/29/93	Hernan	Made load-clos-window use the new windowTitle
				property when making the window.
	14	10/1/93	hernan	Inserting stubs instead of actor saving functions.
	15	10/4/93	kleiman	Never on a monday.
	16	10/25/93	kleiman	initializeFromStore updated for new store
	17	11/1/93	kleiman	d4 store changes
	18	11/1/93	hernan	Macpathname -> file.
	19	11/1/93	kleiman	load-clkos-window simplified
	20	11/1/93	hernan	gs:inheritsFrom? -> inheritsFrom.
	21	11/2/93	kleiman	take home
	22	11/2/93	kleiman	editText
	23	11/5/93	kleiman	more
	24	11/8/93	kleiman	more nice stuff
	25	11/10/93	kleiman	take home
	26	11/12/93	kleiman	take home
	27	11/12/93	kleiman	initializemenubars installed
	28	11/15/93	kleiman	take home
	29	11/16/93	kleiman	take home
	30	11/17/93	kleiman	added some comments
	36	12/3/93	sidney	Allow save and load of projects with ScriptEditText objects
	37	12/5/93	sidney	Left out part of the previous checkin
	38	12/21/93	sidney	Changes so files can be compiled
	39	1/10/94	hernan	Fonts just became objects!!!
	40	1/11/94	hernan	More me -> me.
	41	1/17/94	sidney	Check event clock when save/load project so cursor can spin
	42	1/20/94	dy	Moved the QuickTime stuff from this file to a new
				file ...;Movies;quicktime-save.lisp
	43	2/11/94	sidney	Unnamed objects that can be gc'd
	44	2/12/94	kleiman	logit -> sendToLog
	45	2/14/94	sidney	rename children to knownchildren
	46	2/17/94	kleiman	sk8::*actors-contained-in-other-projects* allows
				proper reconstitution of containment hierarchy
				when a container is in a different project than
				the loaded actor's
	47	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	48	2/21/94	hernan	window -> sk8::window.
	49	2/25/94	hernan	Using symbols instead of keywords for options!!!
	50	2/28/94	hernan	Now using GCable mac stuff.
	51	3/1/94	kleiman	sk8_name -> sk8_id
	52	3/1/94	sidney	handle some pointers correctly
	53	3/2/94	Hernan	Porting to Fred 3.0
	54	3/4/94	kleiman	base-addr -> baseAddress
	55	3/4/94	Hernan	Patching things to make saving of Fred based
				editText work.
	56	3/8/94	kleiman	OBSOLETE DISPOSE!
	57	3/11/94	Hernan	Changing saving and loading of pixelMap because
				we now use gWorlds instead of pixmaps. The
				approach is to save the gWorld-pixmap as a 
				resource and the rebuild the gWorld on the way
				up.
	58	4/19/94	sidney	use pushnew instead of push to make some operations more robust
	59	5/2/94	dy	clean up load warning on *movies-to-initialize*
	60	5/17/94	sidney	ps::*project-filename* was not used, so remove reference to it
	61	6/12/94	dy	call new function finalizeQuickTimeMovies instead of doing movie stuff inline
	62	6/13/94	sidney	initializefromstore, savetostore of object no longer special case knownchildren
	63	6/17/94	chip	!set-up-project-error-tables --> !set-up-project-condition-tables
	64	6/18/94	sidney	initializefromstore moved to finalization time
	65	6/21/94	sidney	1169600: pixmap resources should come from swapfile, not file of project
	66	6/23/94	sidney	support lazy creation of swapfile
				            save data when saving a pixelmap
	67	6/25/94	sidney	use safer trap calls for save/load of pixmap
	68	6/25/94	sidney	still better and safer use of the trap calls, release some memory
	69	7/1/94	Hernan	1168946: save-pixmap-resource does nothing if
				there is no gWorld to be saved.
	70	7/1/94	Hernan	1172048: fixing leak detected in save-pixmap-resource.
	71	7/13/94	sidney	1170896: publish object symbol when loading a project from store
	72	7/21/94	dy	switch order of cacheing windows and finalizeQT in finalizeEnvironment
	73	7/25/94	Hernan	1175911: making actors remember their keyTarget
				when they are not on the Stage. This gets rid of
				the gs:curKey slot of windows.
	74	7/25/94	sidney	1175385: publish property names when loading an object from the store
	75	8/4/94	sidney	more on 1172988
	76	8/4/94	sidney	put newly loaded named object in parents' knownchildren list
	77 	 9/12/94	chip    	removed the totally extraneous & wrong initializeFromStore handler on TypeTable!
	78 	10/ 4/94	sidney  	speedup storing of projects through various hacks
	79 	10/17/94	dy      	ownedRegion is now in p-list
	80 	10/19/94	sidney  	use unique id numbers when loading objects from store
	81 	10/31/94	dy      	fix save-pixmap-resource to handle the 2 return values from T_Get1Resource
	82 	11/ 4/94	Hernan  	Allowing EditText to load from the store.
	83 	11/ 5/94	Hernan  	1182426: fixing save-pixmap-resource to deal
							with the case when the original gWorld's origin
							is not 0.
	84 	11/ 9/94	Hernan  	initializeFromStore of EditText has to create the new selRgn.
							This is required in order to draw the field into
							other ports (for capturePicture).
	85 	11/14/94	dy      	propertiesToSaveSpecially, propertiesToSaveAsFalse, and related new handlers
	86 	11/21/94	Hernan  	The floating windowStyle requires a special WDEF.
							Changing load-clos-window to recognize this fact.
	87 	11/22/94	dy      	initializeFromStore of Actor dirties the bounds, frame and fill
	88 	11/28/94	Hernan  	Before enteringStage gets called we have to make
							all the regions dirty.
	89 	 1/ 4/95	sidney  	remove remaining traces of obsoleted lib functionality
	90 	 1/12/95	sidney  	tweak save/load of project store to work with changes to save as text
	91 	 1/17/95	till    	ReleaseResource
	92 	 1/20/95	Hernan  	restore-pixmap-data has to lock the pict handle
	93 	 1/21/95	sidney  	*virtual-menuItem-properties* obsoleted
	94 	 1/27/95	sidney  	properly initialize menu containment
	95 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	96 	 2/ 8/95	Hernan  	initializeFromStore of Menu has to call next method.
	97 	 2/14/95	Hernan  	Just forcing this file to recompile.
	98 	 3/ 1/95	Hernan  	Adding code to initializeFromStore of EditText to
							allow projects saved in Beta 2 to load under the 
							new Fred that we will have in Beta 3.
	99 	 3/ 1/95	Hernan  	Fixing initializeFromStore of EditText to only put
							out the color migration warning when loss of
							color really happened.
	100	 3/ 9/95	sidney  	added flags to localPropertiesToSaveSpecially of Actor, needed for save as text
	101	 3/14/95	Hernan  	Adding new windowStyles that do not have a close
							box.
	102	 3/17/95	till    	trap wrapping, hlock, hunlock in restore-pixmap-data
	103	 3/17/95	till    	damn, I was completely wrong about the hlock stuff
	104	 3/20/95	Hernan  	Making pixelMap behave like a real child of Media.
	105	 3/20/95	dy      	initializeFromStore returns t
	106	 3/20/95	dy      	Correction: initializeFromStore and earlyInitializeFromStore of Object now return t
	107	 3/24/95	sidney  	save and restore pixmap was using the projects file, not the pixmap's, breaking standalone apps
	108	 3/29/95	dy      	change  to (finalizeQuickTimeMovies) to (finalizeQuickTimeMovies #'initializeFromStore)
	2  	 6/ 8/95	sidney  	MCL 3.0 changes
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/12/96	Hernan  	Fixing method congruency problems.
	4  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	5  	 5/ 8/96	Hernan  	Fixing undefined function warnings.
	6  	 7/ 7/96	sidney  	changes for native PPC build
	7  	 7/29/96	Hernan  	Getting saveASText to work.
	8  	 9/ 3/96	Hernan  	finalizeEnvironment assumed that QuickTime was loaded.
						Not any more.
	9  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
