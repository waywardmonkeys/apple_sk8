(in-package :SK8Development)

(provide "GRAPHIC")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; SK8 Media Definitions
;;;

#| Modification History

03-21-93 ruben contents [graphic] returns nil
01-28-93 hernan Ruben now takes care of currentMenubar of the stage.
01-26-93 hernan mouseenter of the stage only sets the cursor if the stage was
                entered directly.
01-22-93 hernan adding the currentMenubar property of the stage. Also added the commandKeyEvent
                handler of the stage.
01-22-93 ruben id -> objectName
11-13-92 hernan renderer is now defined in color.lisp
09-20-92 hernan changing this to work with MF VI
07-26-92 ruben changed to SK8 package
05-12-92 ruben d29 conversion
03-17-92 ruben added WANTS-IDLE property to graphic-object
03-16-92 ruben graphic-object from multimedia; obsoleted cluster
08-21-91 ruben sed-object -> graphic-object

|#

;;; _______________________________ 
;;; Low level helpers.
;;; _______________________________

(defmacro do-sk8-window-actors ((theActor &optional fromActor toActor) &body body)
  (let* ((wptr (gensym))
         (closWind (gensym))
         (fromActorTestPassed (gensym)))
    `(let ((,fromActorTestPassed (not ,fromActor))
           ,theActor ,closWind)
       (ccl::do-wptrs ,wptr
         (setq ,closWind (window-object ,wptr))
         (when (and ,closWind (gs:sk8-window-p ,closWind))
           (setf ,theActor (slot-value ,closWind 'gs:my-actor-object))
           ;; Have we found the actor we want to start from? 
           (unless ,fromActorTestPassed
             (when (eq ,theActor ,fromActor)
               (setf ,fromActorTestPassed t)))
           ;; Now, if we are ready do apply the function, do it!
           (when ,fromActorTestPassed
             ;; If we reached the end actor, return. 
             (when (and ,toActor (eq ,theActor ,toActor)) (return))
             ;; Finally, if there are no further impediments, let's apply the function!
             ,@body))))))

#|

;;; Old, subversive version of the one above. 

(defvar *SK8-window-class* (find-class 'gs:*sk8-window*))

(defmacro do-SK8-window-actors ((theActor &optional fromActor toActor) &body body)
  (let* ((wptr (gensym))
         (closWind (gensym)))
    `(let (,theActor ,closWind)
       (declare (special *SK8-window-class*))
       (with-macptrs ((,wptr (%get-ptr (%int-to-ptr (require-trap-constant #$WindowList)))))
         ,(when fromActor
            `(when ,fromActor
               (loop
                 (when (%null-ptr-p ,wptr) (return))
                 (setq ,closWind (window-object ,wptr))
                 (when (and (CCL::inherit-from-p ,closWind *SK8-window-class*)
                            (eq (slot-value ,closWind 'gs:my-actor-object) ,fromActor))
                   (return))
                 (%setf-macptr ,wptr (rref ,wptr windowrecord.nextwindow)))))
         (loop
           (when (%null-ptr-p ,wptr) (return))
           (setq ,closWind (window-object ,wptr))
           (%setf-macptr ,wptr (rref ,wptr windowrecord.nextwindow))
           (when (CCL::inherit-from-p ,closWind *SK8-window-class*)
             (setq ,theActor (slot-value ,closWind 'gs:my-actor-object))
             ,(when toActor `(when (eq ,theActor ,toActor) (return)))
             ,@body))))))

|#

;;; _______________________________ 
;;;  Graphic
;;; _______________________________ 

(new object :objectname "Graphic" :undisposable t :project sk8)

;;; _______________________________ 
;;;  Stage
;;; _______________________________ 

(new AliasedCollection :otherParents Graphic :objectname "Stage" :project sk8 :undisposable t
     :properties '(aboutBox cursor cursorLocked fillColor sk8::menubar eventInterests))

(setf (private Stage :property 'sk8::menubar) t)
(setf (fillcolor Stage) Vomit)

(setf (eventInterests Stage) (makeEventInterestsHashTable))

(define-handler collectionProperty (stage)
  'contents)

;;; _______________________________ 
;;; The Stage is interested in suspend and resume.
;;; _______________________________ 

;;; KLUDGE: hiding the SK8 windows when Suspend happens.

#+ppc-target
(defvar *windows-to-hide-because-of-custom-wdef* nil)

#+ppc-target
(defun hide-windows-with-custom-wdefs ()
  (dolist (c (contents Stage))
    ;; Since MCL now does this for all floating windows...
    (when (floating c) ;; (eq (windowStyle c) 'floating)
      (push c *windows-to-hide-because-of-custom-wdef*)
      (hide c))))

#+ppc-target
(defun show-windows-with-custom-wdefs ()
  (dolist (c *windows-to-hide-because-of-custom-wdef*)
    (show c))
  (setf *windows-to-hide-because-of-custom-wdef* nil))

#+ppc-target
(defun restore-wdef-regions-if-necessary ()
  (let ((wptr nil))
    (dolist (c (contents Stage))
      (setf wptr (wptr (gs::node-window c)))
      (when (and (eq (windowStyle c) 'blank)
                 (#_emptyRgn (rref wptr :windowRecord.strucRgn)))
        (gs::wdef-calc-regions wptr)
        (#_calcVisBehind wptr (rref wptr windowRecord.contRgn))
        (#_copyRgn (rref wptr windowRecord.contRgn) 
         (rref wptr windowRecord.updateRgn))
        (gs:sk8-update-event (gs:node-window c))))))

(addEventInterest Stage 'suspend System)
(addEventInterest Stage 'resume System)

(define-handler resume (Stage)
  ;; In the power PC, when we return we have to check if the windows still
  ;; have their regions with them. The regions might be empty if SK8 was
  ;; hidden from the finder, which seems to do something with the WDEFs...
  #+ppc-target
  (restore-wdef-regions-if-necessary)
  ;; [1] Activate the keyTarget of the currentWindow if any.
  (let ((clos-window gs:*currentTla*)
        theActor thekey)
    (when (typep clos-window 'gs:*sk8-window*)
      (setf theActor (slot-value clos-window 'gs:my-actor-object)
            theKey (keyTarget theActor))
      (when (and theKey (inheritsFrom theKey gs:*EditText-the-object*))
        (activateText theKey))))
  ;; [2] Show the background window...
  (let ((thewindow (gs:findbackground)))
    (when theWindow
      (window-show theWindow)
      ;; For some reason, the window does not get drawn when resuming at launch
      ;; time...
      (gs:redraw-background)))
  #+ppc-target
  (show-windows-with-custom-wdefs)
  ;; [3] Check whether the depth of any monitors has changed. If so, 
  ;; we update the actors by calling update-windows-for-new-depth.
  (unless (equal gs:*monitor-depths* (mapcar #'colorDepth (monitors system)))
    (update-windows-for-new-depth)))

(define-handler suspend (Stage)
  ;; [1] Deactivate the keyTarget of the currentWindow.
  (let ((clos-window (car (windows)))
        theActor thekey)
    (when (typep clos-window 'gs:*sk8-window*)
      (setf theActor (slot-value clos-window 'gs:my-actor-object)
            theKey (keyTarget theActor))
      (when (and theKey (inheritsFrom theKey gs:*EditText-the-object*))
        (deactivateText theKey))))
  ;; [2] Hide the background window.
  (let ((thewindow (gs:findbackground)))
    (when theWindow
      (window-hide theWindow)))
  #+ppc-target
  (hide-windows-with-custom-wdefs)
  ;; [3] Save the state of the monitors in mf::*monitor-depths*.
  (setf gs:*monitor-depths* (mapcar #'colorDepth (monitors system))))

;;; _______________________________ 
;;; Stage Events
;;; _______________________________ 

;;; So that it only changes the cursor when it is the original recipient of the event.

(define-handler mouseEnter (Stage)
  (dispatchToInterestedObjects me 'mouseEnter)
  (when (eq (eventActor) me)
    (setf (cursor me) standardCursor)))

(define-handler mouseLeave (Stage)
  (dispatchToInterestedObjects me 'mouseLeave))

(define-handler mouseUp (Stage)
  (dispatchToInterestedObjects me 'mouseUp))

(define-handler mouseDown (Stage)
  (dispatchToInterestedObjects me 'mouseDown))

(define-handler mouseUp (Stage)
  (dispatchToInterestedObjects me 'mouseUp))

(define-handler dropped (Stage thing)
  (dispatchToInterestedObjects me 'dropped thing))

;;; _______________________________ 
;;; Other Stage Stuff
;;; _______________________________ 

;;; Dispatches the command key event to the MCL menubar... The code is taken from do-event.

(define-handler commandKeyEvent (Stage theChar)
  (let (mi
        (menu-p t))
    (unwind-protect
      (progn
        (if (ccl::%izerop (ccl::%ilogand2 -65536 (setq mi (#_MenuKey theChar))))
          (setq menu-p nil)
          (progn
            (setq ccl::*interrupt-level* 0)
            (ccl::menu-selected mi))))
      (if menu-p (#_HiliteMenu 0)))))

;;; inherits the type-filtered portion of the Collection Protocol from Collection

;;; Fillcolor allowed: 
;;; 1. RGBColors, 2. BWPatterns, 3. ColorPatterns.

(define-handler (setf fillColor) (newValue Stage)
  (let ((newColorType nil))
    (cond ((inheritsFrom newValue RGBColor) (setf newColorType :rgb))
          ((inheritsFrom (media newValue) BWPattern) (setf newColorType :bwpat))
          ((inheritsFrom (media newValue) ColorPattern) (setf newColorType :colorPat)))
    (if newColorType
      (progn 
        (setf gs:*bkgnd-color-type* newColorType)
        (setf (slot-value me 'fillcolor) newValue)
        (gs:redraw-background)
        newValue)
      (sk8-error PropertyTypeMismatchError
                 :object        newValue
                 :expectedType  RGBColor
                 :ownerObject   Stage
                 :propertyName  'fillcolor
                 ))))

(define-handler contents (Stage)
  (let ((reverseActorsList nil))
    (do-SK8-window-actors (theActor)
      (push theActor reverseActorsList))
    (nreverse reverseActorsList)))

(define-handler SK8::windows (stage)
  (contents Stage))

(define-handler (setf contents) (newValue Stage)
  (let ((oldContents (contents me))
        (count 1)
        (previousActor nil))
    ;;; [1] Remove old contents not in the new contents.
    (dolist (c oldContents)
      (unless (memq c newValue)
        (setf (container c) nil)))
    ;; [2] Add new contents and fix the layers. 
    (dolist (c newValue)
      (if (eq (container c) me)
        (unless (eql (layer c) count)
          (setf (layer c) count))
        (if previousActor
          (setf (container c :following previousActor) me)
          (setf (container c) me)))
      (incf count)
      (setf previousActor c))
    newValue))

(define-handler deepContents (Stage)
  (let (result)
    (dolist (c (contents me) result)
      (setf result (nconc (cons c (deepContents c)) result)))))

(define-handler container (Stage)
  (declare (ignore me))
  nil)

(define-handler collectionlike (Stage)
  (declare (ignore me))
  nil)

(setf gs:*stage* stage)

(define-handler mouseLoc (Stage)
  (let ((mousePos (view-mouse-position nil)))
    (sk8-multivals (point-h mousePos) (point-v mousePos))))

(define-handler hMouse (Stage)
  (SK8-multival-bind (x y) (mouseloc stage)
    (declare (ignore y))
    x))

(define-handler vMouse (Stage)
  (SK8-multival-bind (x y) (mouseloc stage)
    (declare (ignore x))
    y))

;;;---Virtual Property for showing and hiding the background window of the stage...
(define-handler covered (Stage)
  (when (gs:findbackground) t))

(define-handler (setf covered) (boolean Stage)
  (unless (eq boolean (covered Stage))
    (if boolean
      (gs:install-background)
      (gs:deinstall-background)))
  boolean)

;;; Setting the mouseloc of the stage by hand.
;;;MCL3.0 mouse constants are trap constants now
(defparameter *$MTemp* (%int-to-ptr #$MTemp))        ; Low Level interupt mouse location
(defparameter *$RawMouse* (%int-to-ptr #$RawMouse))  ; Unprocessed mouse location
(defparameter *$Mouse* (%int-to-ptr #$Mouse))        ; Processed mouse location
(defparameter *$CrsrNew* (%int-to-ptr #$CrsrNew))      ; Non-zero if mouse has moved

(define-handler (setf mouseLoc) (newpos Stage)
  (let (x)
    (unless (eql (setq x (make-point (truncate (first newpos))
                                     (truncate (second newpos))))
                 (%get-point *$Mouse*))
      (%put-long *$Mouse* x)
      (%put-long *$RawMouse* x)
      (%put-long *$MTemp* x)
      (%put-byte *$CrsrNew* -1))))

(define-handler boundedByContents (Stage)
  nil)

(define-handler resizesContents (Stage)
  nil)

(define-handler sk8::window (Stage)
  nil)

(define-handler boundsRect (Stage &key physical)
  (declare (ignore physical))
  (let* ((grayRgn (%get-ptr (%int-to-ptr #x9ee)))
         (rect (gs:hrref grayRgn :region.rgnbbox :storage :pointer)))
    (SK8-multivals (min (rref rect :rect.left) 0)
                   (min (rref rect :rect.top) 0)
                   (rref rect :rect.right)
                   (rref rect :rect.bottom))))

(define-handler size (Stage &key)
  (SK8-multival-bind (ll tt rr bb) (boundsRect me)
    (SK8-multivals (- rr ll) (- bb tt))))

(define-handler xScale (Stage)
  1.0s0)

(define-handler yScale (Stage)
  1.0s0)

;;; 1.0
;;; new -- Stage cannot be instantiated

(define-handler new (Stage &rest ignore)
  (declare (ignore ignore))
  (sk8-error GeneralProgrammaticError
             :strings '("Cannot make a new Stage")))


(define-handler (setf container) (theObject graphic)
  (declare (ignore theObject))
  (sk8-error GeneralProgrammaticError
             :strings '("" " cannot be contained by anything!")
             :objects (list me)))

(define-handler contents (Graphic)
  nil)

;;; _______________________________ 
;;; Editable properties.
;;; _______________________________ 

(define-handler localvirtualproperties (stage)
  (when (eq me Stage)
    '(contents covered)))

;;; _______________________________ 
;;; Cursors. 
;;; _______________________________ 

(define-handler (setf cursor) (cursor stage)
  (unless (cursorLocked stage)
    (setf (slot-value me 'cursor) cursor)
    (when cursor
      (install cursor))
    ;; Return the cursor.
    cursor))

(defun restore-cursor ()
  (setf (cursor stage) standardcursor))

;; WITHCURSOR
;;   Use a different cursor for a body of code.  Commonly used for Watch cursor operations.

(defmacro SK8:withCursor (theCursor &body body)
  (let ((oldCursor (genSym)))
    `(let ((,oldCursor (or (SK8:cursor SK8:Stage) SK8:StandardCursor)))
       (unwind-protect
         (progn
           (setf (SK8:cursor SK8:Stage) ,theCursor)
           ,@body)
         (setf (SK8:cursor SK8:Stage) ,oldCursor)))))

;; A macro that locks the cursor in place...

(defmacro withLockedCursor (theCursor &body body)
  (let* ((oldCursor (gensym))
         (lockedAlready? (gensym)))
    `(let ((,oldCursor (or (SK8:cursor SK8:Stage) SK8:StandardCursor))
           (,lockedAlready? (SK8:cursorLocked SK8:Stage)))
       (unwind-protect
         (progn
           (unless ,lockedAlready?
             (setf (SK8:cursor SK8::stage) ,theCursor)
             (setf (SK8:cursorLocked SK8:stage) t))
           ,@body)
         (unless ,lockedAlready? 
           (setf (SK8:cursorLocked SK8:stage) nil)
           (setf (SK8:cursor SK8:stage) ,oldCursor))))))

(defmacro |WITH CURSOR| ((cursr) &body body)
  `(withCursor ,cursr
    ,@body))

(defmacro |WITH LOCKEDCURSOR| ((cursr) &body body)
  `(withLockedCursor ,cursr
     ,@body))

(defmacro |WITH EVENTSDISABLED| (() &body body)
  `(without-interrupts
    ,@body))

;;;Thread macros
(defmacro Eval-In-New-Thread (name &body body)
  `(process-run-function (list :name ,name :background-p t) #'(lambda () (eval ,@body))))

(defmacro Eval-In-New-Thread-With-Wait (name noargPredFun &body body)
  `(process-run-function (list :name ,name :background-p t) #'(lambda () 
                                                (process-wait  nil ,noargPredFun)
                                                (eval ,@body))))

(defmacro |WITH DEFERREDACTION| (() &body body)
  `(eval-in-new-thread "Deferred Action"
     ,@body))


(setf (cursor stage) standardCursor)


;;; *post-gc-cursor* -- holds stage's cursor before GC
;;;
(defvar *post-gc-cursor* nil)

;;; hook saves the stage's cursor

(add-pre-gc-hook #'(lambda ()
                     (setq *post-gc-cursor* (cursor stage))))

;;; hook restore the stage's cursor

(add-post-gc-hook #'(lambda ()
                      (unless (eq (cursor stage) *post-gc-cursor*)
                        (setf (cursor stage) *post-gc-cursor*)
                        (setq *post-gc-cursor* nil))))

;;; _______________________________ 
;;; AboutBox
;;; _______________________________ 

(defun sk8-raw-about-box ()
  (modal-dialog
   (make-instance 'dialog
     :view-size #@(190 100)
     :window-show nil
     :view-position
     (make-point (- (truncate *screen-width* 2) 95)
                 (- (truncate *screen-height* 2) 80))
     :window-type :double-edge-box
     :view-subviews
     `(,(make-instance 'static-text-dialog-item
          :view-font '("Courier" 24 :bold :underline)
          :view-size #@(180 130)
          :view-position #@(35 10)
          :dialog-item-text "SK8 1.1")
       ,(make-instance 'static-text-dialog-item
          :view-font '("Geneva" 9)
          :view-size #@(180 130)
          :view-position #@(10 40)
          :dialog-item-text "©ARL, Apple Computer Inc., 1996")
       ,(make-instance 'button-dialog-item
          :dialog-item-text "  Wow!  "
          :view-position #@(60 70)
          :dialog-item-action #'(lambda (item)
                                  (declare (ignore item))
                                  (return-from-modal-dialog t)))
       ))))

(defun about-menu-item ()
  (car (menu-items *apple-menu*)))

(let ((m (about-menu-item)))
  (set-menu-item-title m "About SK8É")
  (setf (slot-value m 'menu-item-action) 
        #'(lambda () 
            (if (aboutBox Stage)
              (progn
                (setf (location (aboutBox Stage)) (mainMonitorCenter))
                (setf (container (aboutBox Stage)) stage))
              (sk8-raw-about-box)))))

(define-handler (setf aboutBox) (newValue Stage &key (itemText "About This Title"))
  (if newValue
    (progn (set-menu-item-title (about-menu-item) itemText)
           (setValue 'aboutBox Stage newValue))
    (progn (set-menu-item-title (about-menu-item) "About SK8...")
           (setValue 'aboutBox Stage newValue))))

#|
	Change History (most recent last):
	2	6/1/93	Hernan	Added hMouse and vMouse properties to the stage.
	9	9/24/93	hernan	Adding dropped event to the stage.
	10	9/29/93	hernan	%struct% slot obsoleted
	11	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	12	10/1/93	hernan	struct is no more!!!
	13	10/11/93	rod	Added virtual property "Covered" to the stage
				as an interface to the background window.
	14	1/10/94	sidney	change to (deepcontents stage) because (deepcontents actor) no longer includes self in return
	15	1/11/94	hernan	self -> me
	16	1/14/94	hernan	Labeling private properties and handlers.
	17	1/17/94	hernan	Adding the cursorLocked property to the Stage.
	18	1/31/94	hernan	Adding a fillcolor property to the Stage.
	18	1/31/94	hernan	Second try to check in...
	19	1/31/94	kleiman	DesktopColor is not defined yet! Make it be set
				after the build is done!
	20	1/31/94	hernan	Setting the fillcolor of the stage now redraws the
				stage (if the background is intalled, of course!)
	21	2/12/94	kleiman	renaming
	22	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	23	2/22/94	hernan	window -> sk8::window.
	24	3/3/94	kleiman	properties -> addproperty
	25	3/3/94	kleiman	private properties declared via makeprivateproperty
	26	3/6/94	Hernan	Making covered return true (not the window).
	27	3/9/94	Hernan	Making myWindows get windoids.
	28	3/10/94	chip	Stage now has collection protocol defined on it directly, instead of mixing in one
	29	3/10/94	chip	Stage now has collection protocol defined on it directly, instead of mixing in one; obsoleted its real-contents property; obsoleted myWindows
	30	3/10/94	chip	added |CONTENTS FOR MODS| handler for Stage
	31	3/10/94	chip	got rid of an obsolete property of Stage
	32	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	33	3/15/94	Hernan	Only covering the stage when it is not covered.
	34	3/18/94	Hernan	Not allowing people to set the fillcolor of the 
				stage to a pixpat. (until we figure out what 
				makes SK8 crash at quit time).
	35	3/21/94	chip	Fixed glitch in nextState of Stage
	36	3/21/94	Hernan	Fixing boundsRect of the stage to make sure the
				topleft is at least (0,0).
	37	3/21/94	Hernan	Setf fillcolor now lets you set the fillcolor to 
				patterns again.
	38	3/21/94	Hernan	Setting the fillcolor of the stage to DeskTopColor
				again.
	39	3/21/94	Hernan	Adding windows of the stage (calls contents).
	40	3/24/94	Hernan	Making the fillcolor of the stage be gray and 
				blocking the setter to allow RGBs only. (again!)
	41	3/26/94	Brian	changing default stage color to darkblue.
				looks better with the UI this way.
	42	4/13/94	Hernan	Added require-trap-constant to line that refers
				to #$windowList in do-sk8-window-actors.
	43	6/14/94	Hernan	Why was the fillcolor of the Stage set at the end
				of the build? Setting it right away!
	44	6/14/94	Hernan	I found out why: the color does not exist yet!
	45	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	46	7/6/94	Hernan	1172509: getting rid of attached and detached
				of Stage (obsolete handlers).
	47	7/14/94	Hernan	Adding the eventInterests of the Stage.
	48	7/14/94	Hernan	contents for mods should be CONTENTS FOR MODS
	49	7/14/94	Hernan	Giving all window related stuff in suspend and
				resume to the Stage.
	50	7/26/94	chip	required every toStateHolder (in mapStatesForMods) to be a cons, not a list (radar #1176415)
	51 	 8/19/94	Hernan  	
	52 	 8/24/94	dy      	Change Stage color for b2.  Change it again if you don't like it.
	53 	 8/31/94	Hernan  	*currentWindow* -> *currentTla*.
	54 	 9/ 2/94	Hernan  	1183700: Replacing references to EditText by
							references to mf::*editText-the-object*.
	55 	 9/ 7/94	dy      	better(?) stage color
	56 	 9/28/94	chip    	obsoleted copyNextState
	57 	 9/29/94	chip    	marked Stage as not-simple-instantiable
	58 	10/ 6/94	chip    	took out explicit marking of Stage as not-simple-instantiable (define-handler on new does it now)
	59 	11/16/94	chip    	obsoleted stateGreaterOrEqual (sufficient functionality in stateGreaterThan) and rangeCollectionAndEndpoints (unnecessary)
	60 	11/28/94	dy      	new color for b3
	61 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	62 	12/13/94	Hernan  	Letting the fillcolor of the Stage be a pattern again.
	63 	12/16/94	rod     	Moving set fillcolor of stage to standard-resources.
	64 	 1/11/95	Hernan  	In resume, we force a redraw of the background
							window (if the stage is covered).
	65 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	3  	 6/23/95	Hernan  	1252857: The Stage should not return itself as its window.
	4  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/16/96	sidney  	move definition of gc hook for stage's cursor until after stage is defined
	4  	 4/17/96	Brian   	commenting out old collection protocol
	5  	 4/18/96	Hernan  	Adding set contents.
	6  	 4/19/96	Brian   	
	7  	 4/25/96	Hernan  	Making the about box come up in the center of the Stage.
	8  	 5/ 7/96	sidney  	move withcursor and withlockedcursor macros here
	9  	 5/13/96	sidney  	move definition of eval-in-new-thread to this file
	10 	 7/ 7/96	sidney  	changes for native PPC build
	11 	 7/22/96	Brian   	Until I recode the Sk8 custom WDEFs in C, I will have to
						make SK8 windows hide themselves on suspend and show
						themselves on resume.
	12 	 7/23/96	Hernan  	when on the PPC, suspend and resume hide and show the floating windows
						only.
	13 	 7/25/96	Hernan  	Added restore-wdef-regions-if-necessary.
	14 	 7/26/96	sidney  	add collectionlike handler for writeobject to work better
	15 	10/17/96	Hernan  	ATG -> ARL
	16 	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	17 	11/12/96	sidney  	name process in eval-in-new-thread to make debugging easier
	18 	11/26/96	Hernan  	Now hiding all floating windows on suspend. We need to
						do it ourselves because MCL will do it anyway and in a 
						lower level way that would not cause a SK8 redraw to happen.
	19 	12/12/96	Hernan  	Rewrote do-sk8-window-actors to be a bit less subversive.
	20 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
