(in-package :SK8Development)
(provide "MENUS")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________
;;;  instant-menus.lisp
;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________

;;;  this file implements pop-up menus, according to the Apple standard.
;;;  it also shows how multiple-inheritance can be handy!

#| Modification History

01-22-93 ruben id -> objectName
12-08-92 hernan integrated Adam's patches to make these menus act properly on a menubar.
08-01-91 ruben menu-select will check for and eval a closure as a menu-item action
3/29/91  ice converted to MCL 2.0 CLOS S Weyer
6/14/89  ice Hacked into instant menus by David Vronay, Apple Computer Inc.

|#

(defconstant $menuhook #xA30)
(defvar *menuTrackProcMenutitle.left* 0)
(defvar *menuTrackProcMenutitle.top* 0)
(defvar *menuTrackProcMenutitle.right* 0)
(defvar *menuTrackProcMenutitle.bottom* 0)
(defvar *specialMenuTrack?* nil)

;;; The stuff in the *menuTrackProcMenutitle.xxx vars is the boundsRect (in physical cords).
;;; of the menu (the actor) from where the real menu is popped up. 

(defun SK8::restore-sk8-instant-menus ()
  ;; Define the proc
  (let ((*warn-if-redefine* nil))
    (defccallable SK8::menuTrackProc ()
      (unless sk8::eventsStopForMenus
        (SK8::tickEventClock))
      (when *specialMenuTrack?*
        (let (mouseloc)
          (rlet ((mouselocPtr :pointer))
            (#_GetMouse mouselocPtr)
            (setq mouseloc (%get-long mouselocPtr)))
          (when (and (CCL::%i< (point-v mouseloc) *menuTrackProcMenutitle.bottom*)
                     (CCL::%i>= (point-v mouseloc) *menuTrackProcMenutitle.top*)
                     (or (CCL::%i< (point-h mouseloc) *menuTrackProcMenutitle.left*)
                         (CCL::%i> (point-h mouseloc) *menuTrackProcMenutitle.right*)))
            (#_PostEvent #$mouseUp mouseloc))))))
  ;; Install it in the menuhook
  (%put-ptr (%INT-TO-PTR $MENUHOOK) SK8::menuTrackProc)
  ;; For safety, clear this flag!
  (setf *specialMenuTrack?* nil))

(SK8::restore-sk8-instant-menus)

;; Now we put into the quit functions a form that clears this pointer.

(push (nfunction clear-menutrackproc-hook (lambda () (%put-ptr (%INT-TO-PTR $MENUHOOK) (%null-ptr))))
      mf::*quit-functions*)

(defclass instant-menu (menu)
  ((menu-loc :initform nil)
   (default-item :initarg :default-item :initform 1)
   (item-display :initarg :item-display :initform :selection)
   (width-correction :initform 0)       ; ***? :allocation :class (see pop-up)
   (my-window :initform nil)
   (my-pos :initform nil)
   (menu-handle :initform nil)
   (menuTitleBounds :initform (list nil nil nil nil))  ; *** 12/7/92 - adam
   ))

(defmethod size-rectangles ((self instant-menu) where)
  ;"does a lot of tweaking to get the thing to draw right"
  (setf (slot-value self 'menu-loc) where))

;;;  "Update the menu's items then displays the pop-menu.  Default-item is the
;;;  item which will come up selected  when the menu is displayed."

(defmethod menu-select ((self instant-menu) num)
  (declare (ignore num))
  (with-slots (default-item menu-handle menuTitleBounds my-window) self
    (let* ((*specialMenuTrack?* (first menuTitleBounds))
           (wpos (when my-window (view-position my-window)))
           (pos (when my-window (ccl::%local-to-global (wptr my-window) (slot-value self 'menu-loc))))
           selection
           selected-menu
           selected-menu-item)
      (when my-window
        (when *specialMenuTrack?*
          (setq *menuTrackProcMenutitle.left* (+ (point-h wpos) (first menuTitleBounds))
                *menuTrackProcMenutitle.top* (+ (point-v wpos) (second menuTitleBounds))
                *menuTrackProcMenutitle.right* (+ (point-h wpos) (third menuTitleBounds))
                *menuTrackProcMenutitle.bottom* (+ (point-v wpos) (fourth menuTitleBounds))))
        (menu-update self)
        (setq selection (#_PopUpMenuSelect menu-handle (point-v pos)
                         (- (point-h pos) 2) (or default-item 0))
              ;we get the selected menu in case you want to break the rules
              ;and use hierarchical menus in a pop-up menu
              selected-menu (ccl::menu-object (ash (logand #xFFFF0000 selection) -16))
              selected-menu-item (logand #x0000FFFF selection))
        ; some var needed from selected-menu??
        (unless (eql selected-menu-item 0)
          ;;(setq default-item selected-menu-item)
          (let ((thing (menu-item-action-function (elt (menu-items selected-menu) (- selected-menu-item 1))))) ; @@@ changed 8/1/91
            (if (functionp thing)
              (funcall thing)
              thing)))))))

(defmethod menu-install ((self instant-menu))
  ;"Creates the actual Macintosh menu with all of the menu's current items."
  (with-slots (menu-id menu-handle) self
    (let ((menu-items (menu-items self)))
      (apply #'remove-menu-items self menu-items)
      (ccl::init-menu-id self)
      (with-pstrs ((menu-title (menu-title self)))
        (setq menu-handle (#_NewMenu menu-id menu-title)))
      (unless (handlep menu-handle)
        (error "Could not create menu handle!"))
      (#_InsertMenu menu-handle -1)
      (apply #'add-menu-items self menu-items)     
      ;; run through the items we have made and disable all the ones that are disabled.
      (let ((count 1))
        (dolist (i menu-items)
          (when (and (typep i 'ccl::menu) (not (menu-enabled-p i)))
            (#_disableItem menu-handle count))
          (incf count)))
      ;; Install the colors
      (let* ((colors (part-color-list self)))
        (loop
          (unless colors (return))
          (set-part-color self (pop colors) (pop colors))))
      T)))

(defmethod menu-deinstall ((self instant-menu))
  (let* ((*menubar-frozen* t))
    (call-next-method))
  (setf (slot-value self 'menu-loc) nil))

#|
	Change History (most recent last):
	2	5/28/93	Hernan	No change, just messed with the colors for a 
				while.
	3	12/21/93	hernan	Error checking trap stuff.
	4	3/10/94	dy	try TickEventClock in menuTrackProc - may be too slow when playing movies.  Let's see.
	5	3/10/94	Hernan	get sourceserver back in synch on this file
	6	3/10/94	Hernan	Fixing the flaky menus problem. (No need to 
				post a mousedown event in menutrackproc).
	7	3/25/94	Hernan	Fixing menu-install to disable menu items which
				are hierarchical menus with no items in them.
	8	6/9/94	chip	fixed the quit-function for clearing the menutrackproc; consolidated the "restore" behavior for the trackproc with the definition
	9	6/9/94	chip	oops -- putting restore-sk8-instant-menus in the correct package (SK8)!
	10	7/1/94	Hernan	1171196: modified the menu track proc to keep
				the menu up when the mouse goes over the top
				of the menubar.
	11	8/10/94	Hernan	1178226: menu-install now disables items which
				are menus and which are disabled.
	12 	 9/27/94	Hernan  	1167547: Defining a global variable that controls
							whether the event system gets ticked while a 
							menu is pulled down.
|# ;(do not edit past this line!!)

;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________
;;; MENUS.lisp
;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________

#| Modification History

04-29-93 hernan add-menu now adds a menu AT THE END of the menubar.
03-11-93 ruben initializeFromStore menuitems and initializeMenubar
02-22-93 ruben inheritsFrom? -> inheritsFrom
02-19-93 ruben Bug #475 selectFromMenu
02-10-93 ruben Bug #450 make-instant-menu preserves check mark
                        selectFromMenu relative position
02-09-93 ruben menuItems no longer in ContainerCollection bug #440
02-03-93 hernan setf currentmenubar of the stage now accepts menuBarActors.
02-01-93 ruben store::*mbar*, containerCollection
01-31-93 ruben obsoleted saved-instant-menu property of menu (caching of popups)
01-28-93 ruben Totally and definitely overhauled
----------------------------------------------------------------------------------------
01-28-93 ruben delete-object -> dispose & fix for bug #308
01-25-93 hernan added the file/edit and windows menus as SK8 objects. Also a menubar with these
                three called the sk8simpleMenubar.
01-22-93 ruben id -> objectName
01-07-93 ruben obsoleted (setf contents) & check for struct before calling remove-menu-items
11-15-92 ruben modified to be compatible with other menus
09-20-92 hernan changing this to work with MF VI
08-04-92 ice   Made it compliant with new SK8 frameset
07-26-92 ice   SK8 1.0 conversion
05-12-92 ruben d29 conversion
03-29-92 ruben began d29; fillcolor, textcolor, textstyle, checkcolor, checkmark,
               keycolor, title, project, commandkey, enabled, itemcolor
               swept for "get-" and "set-"
03-26-92 ruben NEW w/create-object & gentemp name generation fix
03-24-92 ruben dipose [menu & menuitem] no longer check whether they are in sk8-ui
               reasons: someone may WANT to dispose them and 1 step on way to sk8-ui-independence
03-18-92 ruben typos; add-menu, add-menuItem return menu and menuItem, respectively
                remove-self [menubar] calls remove-menubar for each of its projects;
                make-code typos
03-17-92 ruben made menubar, menu and menuitem objects member-of prototypes
03-16-92 ruben dispose doesn't disembowel sk8-ui items & make-code doesn't try to pee with it
               new hair get-excepted-menus-and-menuitems helps puke factor;
               redefined menubar, menu and menuitem objects
01-19-92 ruben converted to MacFrames II
08-21-91 ruben sed-object -> graphic-object

|#

;;;
;;; The Macintosh Menubar, Menus and Menu Items; including popup menu capabilities
;;;

;;; MenuItem -- holds the menu it belongs to
;;;

(new Graphic
     :objectname "MenuItem"
     :undisposable t
     :prototype t
     :properties `((keycolor      :value ,black)            ; Command key color
                   (textcolor     :value ,black)            ; Menu item title's text color
                   (checkcolor    :value ,black)            ; Menu item checkmark's color
                   (OSPointer :value nil)                   ; The MCL mac menu item.
                   (layer         :value 1)                 ; Menu item order in containing menu
                   (installedIn            :value nil))     ; Menu in which it is installed (used as stag by project store)
     :project sk8)

(setf (private MenuItem :property 'OSPointer) t)
(setf (private MenuItem :property 'installedIn) t)

;;; Add a menu item to menuitem.

(let ((OSPointer (make-instance 'gs:*SK8-menu-item*)))
  (setf (OSPointer menuItem) OSPointer)
  (setf (mf::my-frame OSPointer) menuitem))

;;; Menu -- Now an actor. The menu subsumes the functinality of the old menus, the popUpmenuActor
;;;       and the menuActor. The menu is now a complex object that knows how to behave in several 
;;;       cases as follows:
;;;
;;;       (1) The menu is on a mac menubar. This is the simplest case. Only a mac menu.
;;;       (2) The menu is installed on a menubar on an actor. The menu becomes a menuActor (a rectangle
;;;          that pops up the menu).
;;;       (3) The menu is installed directly on an actor. Becomes a popUpMenu (the rectangle with the
;;;          triangle pointing downWards).
;;;
;;;       Note that a popUpmenu becomes a menuActor by hiding its shade and arrow. For both menus and
;;;       menubars we use a separate structure to associate them: the menus of the menubar and the
;;;       menuItems of the menu. 

(new rectangle
     :objectname "Menu"
     :undisposable t
     :prototype t
     :properties `((defaultMenuItem    :value 1)          ; for popup menu capability
                   (menutextColor :inherit :value ,black)         ; the menu's text color
                   (menufillcolor :inherit :value ,white)         ; the menu's fillcolor.
                   (layer :value 1)                       ; menu order in containing menubar
                   (installedIn :value nil)      ; menubar, menu (for hierarchicals).
                   (macMenu :value nil)                   ; holds the struct.
                   (menuType :value nil)                  ; whether menu acts as popup or menuactor...
                   ;; PopUpMenuActor properties...
                   (fitToText :inherit :value t)
                   (depresses :inherit :value nil)
                   (Hspace :inherit)
                   (Vspace :inherit)
                   (arrowColor :value ,black)            ; the color of the arrow
                   (arrowsize :value (12 7))            ; the color of the arrow
                   (depressed :value nil))                ; whether the menu is currently depressed.
     :project sk8)

(setf (localCreationRelations Menu) '(SK8::[installedIn]))

(setf (private Menu :property 'installedIn) t)

(setFrameSize menu 0 0)
(setf (fillcolor menu) white)
(setBoundsRect menu 10 10 132 28 :physical t)
(setf (Hspace menu) 2)
(setf (Vspace menu) 2)
(setf (menuType menu) 'popUp)

;;; The menu's drawfunction!!!

;;; This one just draws a frame around the menu...

(defun draw-menu-as-popup (theMenu thePort)
  (gs:let+ ((tempRgn (:region))
         (hs (hspace theMenu))
         (vs (vSpace theMenu))
         (themenutext (text theMenu))
         (arrowsize (arrowsize theMenu))
         (arrowsizeh (car arrowsize))
         (arrowsizev (cadr arrowsize))
         (theRect (:rect))
         ll tt rr bb)
    ;; [1] Draw frame.
    (gs:recompute-fill-region theMenu (gs:node-flags theMenu))
    (gs:region-into-rect (gs:node-fillRegion theMenu) theRect)
    (setf ll (rref theRect :rect.left)
          tt (rref theRect :rect.top)
          rr (rref theRect :rect.right)
          bb (rref theRect :rect.bottom))
    (with-rgb (theColor (mcl-color (framecolor theMenu)))
      (#_rgbForeColor theColor)
      (#_penSize 1 1)
      ;; Draw the lines!
      (#_moveTo ll tt)
      (#_lineTo (- rr hs) tt)
      (#_lineTo (- rr hs) (- bb vs))
      (#_lineTo ll (- bb vs))
      (#_lineTo ll tt))
    ;; [2] Draw Shade.
    (unless (depressed theMenu)
      (rlet ((tempRect :rect))
        ;; horizontal shade.
        (rset tempRect :rect.left (+ ll (1+ hs)))
        (rset tempRect :rect.top (- bb vs))
        (rset tempRect :rect.right rr)
        (rset tempRect :rect.bottom bb)
        (#_paintRect tempRect)
        ;; vertical shade.
        (rset tempRect :rect.left (- rr hs))
        (rset tempRect :rect.top (+ tt (1+ vs)))
        (rset tempRect :rect.right rr)
        (rset tempRect :rect.bottom bb)
        (#_paintRect tempRect)))
    ;; [3] Draw Arrow.
    (setf tt (- (+ tt (round (- bb tt) 2)) (round arrowsizev 2))  ;;;Halfway down
          rr (if (and themenutext (not (string= themenutext "")))
               (- rr 9)
               (+ (+ ll (round (- rr ll) 2)) (round arrowsizeh 2))  ;;;Halfway down
               ))
    ;; OJO! we add and subtract relative to the new starting point!
    (#_openRgn)
    (#_moveTo rr tt)
    (#_lineTo (- rr (round arrowsizeh 2)) (+ tt arrowsizev))
    (#_lineTo (- rr arrowsizeh) tt)
    (#_lineTo rr tt)
    (#_closeRgn tempRgn)
    (render (arrowcolor theMenu) theMenu tempRgn thePort)
    ;; Restore the pen!
    (#_penNormal)
    ))
  
;;; Pop up menus draw as if their textLocation was centerLeft.

(defmacro preserving-textLocation-but-drawing-as-centerLeft (theActor &body body)
  (let ((oldLocation (gensym)))
    `(let ((,oldLocation (textLocation ,theActor)))
       (setf (getf (gs:node-properties ,theActor) :location) 'centerLeft)
       ,@body
       (setf (getf (gs:node-properties ,theActor) :location) ,oldLocation))))

(defun popup-menu-draw (theActor thePort drawRegion)
  (gs:let+ ((flags (gs:node-flags theActor))
         (tempRgn (:region)))
    ;; Recomputing the regions on demand if necessary.
    (gs:recompute-frame-region theActor flags)
    (gs:recompute-fill-region theActor flags)
    (gs:recompute-bounds-region theActor flags)
    ;; Rendering the frame and the fill.
    (render (gs:node-frameColor theActor) theActor (gs:node-frameRegion theActor) thePort)
    (render (gs:node-fillColor theActor) theActor (gs:node-fillRegion theActor) thePort)
    ;; Prepare for weird drawing.
    (#_sectRgn drawRegion (gs:node-fillRegion theActor) tempRgn)
    ;; Do special menu stuff.
    (draw-menu-as-popup theActor thePort)
    ;; Render the text!
    (unless (equal (text theActor) "")
      (gs:with-clipped-region tempRgn
        (preserving-textLocation-but-drawing-as-centerLeft theActor
          (sk8::render-text theActor thePort)
          )))
    ;; Do inverting if highlighted.
    (when (and (gs:hilited? flags) (gs:inverted? flags))
      (#_invertRgn (gs:node-boundsRegion theActor)))))

;;; Menubar -- holds the menu and the menubar's color

(new rectangle
     :objectname "MenuBar"
     :undisposable t
     :prototype t
     :properties '((menus        :value nil)           ; list of menu objects
                   (onStage      :value nil)           ; is menubar on stage (used by store)
                   (installedIn  :value nil))          ; menubar actor that manifests the menubar (if any) 
     :project sk8)

(setf (localCreationRelations SK8::MenuBar) '(SK8::menus*))

(setf (private SK8::MenuBar :property 'installedIn) t)


(setBoundsRect SK8::MenuBar 0 0 300 30)

;;; _______________
;;; MENUBAR METHODS
;;; _______________

(define-handler (setf visible) (boolean SK8::MenuBar)
  (if (onstage me)
    (if boolean
      (gs:showmenubar)
      (gs:hidemenubar)))
  (call-next-method))

;;; (setf menubar) -- installs the menus of theMenubar on the stage and shows the menubar...

(define-handler (setf SK8::menuBar) (theMenubar Stage &key (visible t))
  (let ((oldMenuBar (sk8::menubar Stage)))
    (unless (eq oldMenubar theMenubar)
      (if theMenubar
        ;; Setting a menubar.
        (progn
          (ensureType theMenubar SK8::Menubar)
          (unless (installed theMenubar)
            (setf (container theMenubar) nil)
            (install-menubar theMenubar)
            (setValue 'sk8::menubar stage theMenubar))
          (setf (visible theMenubar) visible))
        ;; Removing the menubar.
        (progn
          (deinstall-menubar)
          (setValue 'sk8::menubar stage nil)
          ))
      (when oldmenubar
        (setf (onStage oldmenubar) nil)))))

(define-handler installed (SK8::MenuBar)
  (eq me (SK8::menuBar Stage)))

;;; install-menubar -- actually installs the menubar in the MCL sense of it

(defun install-menubar (theMenubar) ;  &optional (showing t))
  (ensureType theMenubar SK8::MenuBar)
  (let ((menus (menus theMenubar)))
    (set-menubar (mapcar #'(lambda (curMenu) (macMenu curMenu)) menus))
    (setf (onStage theMenubar) t)
    (let ((fillColor (fillcolor theMenubar)))
      (set-part-color *menubar* :menubar
                      (or (and fillColor (MCL-color fillColor))
                          (getf ccl::%initial-menubar-colors :menubar)))
      (when (visible themenubar) (gs:showmenubar :force t))
      )))

;;; deinstall-menubar -- Actually removes the menus of theMenubar from the current Menubar in the MCL sense of it
;;;

(defun deinstall-menubar ()
  (set-menubar nil)
  (gs:hidemenubar))

;;; (setf container) -- this method turns a menubar into a menubar actor...

(define-handler (setf container) (thing SK8::MenuBar)
  (if thing
    ;; Menubar becomes a menubaractor!
    (progn 
      ;; If current menubar of the stage, deinstall it!
      (when (eq me (SK8::MenuBar Stage))
        (setf (SK8::MenuBar Stage) nil))
      (unless (container me)
        ;; Just in case: Recompute the rects of all menus!
        (computeSize me))
      (call-next-method))
    (call-next-method)))

;;; add-menu -- adds menu object to the menubar

(define-handler add-menu :private (SK8::MenuBar theMenu &key after (listUpdate t))
  (when (or (null listUpdate) (not (memq theMenu (menus me))))
    ;; Updating the menu list.
    (when listUpdate
      (if after
        (setf (slot-value me 'menus) 
              (gs:moveToPosition theMenu (cons theMenu (menus me)) (layer after)))
        (setf (slot-value me 'menus) (append (menus me) (list theMenu)))))
    ;; and update containement.
    (setf (menuType theMenu) 'menu)
    (setf (container theMenu) me)
    ;; Make the menubar recompute...
    (computeSize me)
    (setf (installedIn theMenu) me)
    (setf (slot-value themenu 'layer) (dynamic-number theMenu))
    (when (installed me) 
      (install-menubar me))
    theMenu))

(define-handler add-menus :private (SK8::MenuBar theMenus)
  (dolist (oneMenu theMenus)
    ;; and update containement.
    (setf (menuType oneMenu) 'menu)
    (setf (container oneMenu) me)
    (setf (installedIn oneMenu) me))
  ;; Make the menubar recompute...
  (setf (slot-value me 'menus) theMenus)
  (computeSize me)
  (reset-menu-numbers (menus me))
  (when (installed me) 
    (install-menubar me)))

;;; remove-menu -- removes menu objects from the menubar

(define-handler remove-menu :private (SK8::MenuBar theMenu &key (listUpdate t))
  (let ((installed (installed me)))
    ;; Fixes the menu list.
    (when listUpdate
      (setf (slot-value me 'menus) (delete theMenu (menus me))))
    (setf (installedIn theMenu) nil)
    (when (container theMenu)
      (detach (container theMenu) theMenu))
    (computeSize me)
    (when installed (install-menubar me))))

(define-handler remove-menus :private (SK8::MenuBar theMenus)
  (let ((installed (installed me))
        (allMenus (menus me)))
    (dolist (aMenu theMenus)
      (setf (installedIn aMenu) nil)
      (when (container aMenu)
        (detach (container aMenu) aMenu))
      (setf allMenus (delete aMenu allMenus)))
    (setf (slot-value me 'menus) allMenus)
    (computeSize me)
    (when installed 
      (install-menubar me))))

(define-handler menuSelect (SK8::MenuBar)
  (when (installed me) (menuSelect Stage))
  me)

(define-handler menuselect (stage)
  )

(define-handler (setf menus) (theMenus SK8::MenuBar)
  (let ((oldMenus (remove-if-not #'(lambda (aMenu) (eq (sk8::menubar aMenu) me)) (menus me))))
    ;; First remove the old menus!
    (when oldMenus
      (remove-menus me oldMenus))
    ;; And add the new ones!
    (add-menus me theMenus)))

(define-handler menuTextSize (menu)
  (if (and (text me) (not (equal (text me) "")))
    (if (eq (menuType me) 'popup)
      ;; if it is a popup then we leave room for the arrow plus the text
      (sk8-multival-bind (ww hh) (ActorTextSize me)
        (sk8-multivals (+ ww 28) (1+ hh)))
      ;; if it's not a popup then just leave a little horizontal spacing...
      (sk8-multival-bind (ww hh) (ActorTextSize me)
        (sk8-multivals (+ ww 5) (1+ hh)))) 
    ;; Otherwise, we leave enough room for just the arrow!
    (sk8-multivals 28 (height me))))

;;; Does nothing if the menu is in popup form and does not fitToText.

(define-handler resizeToText (menu)
  (unless (and (eq (menutype me) 'popup)
               (not (fitToText me)))
    (sk8-multival-bind (mh mv) (menuTextSize me)
      (sk8-multival-bind (ll tt rr bb) (boundsrect me)
        (let ((loc (textlocation me)))
          (cond 
           ((memq loc '(centerleft topleft bottomleft))
            (setboundsrect me ll tt (+ ll mh) (+ tt mv)))
           ((memq loc '(centerright topright bottomright))
            (setboundsrect me (- rr mh) (- bb mv) rr bb))
           ((memq loc '(center topcenter bottomcenter))
            (setSize me mh mv))
           ))))))

(define-handler computeSize (SK8::MenuBar)
  (let ((v (height me))
        (x 5))
    ;; add new menus
    (dolist (i (menus me))
      ;; move it into position
      (sk8-multival-bind (th tv) (menuTextSize i)
        (declare (ignore tv))
        (setBoundsRect i x 0 (+ x th) v)
        (setq x (+ x th))))))

(define-handler update (SK8::MenuBar)
  (when (eq me (currentMenubar menuSelectMode))
    (dolist (theMenu (menus me))
      (update theMenu))))

(define-handler resized (SK8::MenuBar)
  (computeSize me))

;;; ____________
;;; MENU METHODS
;;; ____________

(define-handler initialize (menu original isNew initArgs) ; called by initializeFromStore
  (declare (ignore isNew initArgs))
  (call-next-method)
  (let ((theMenu (make-instance 'gs:*SK8-menu*))
        (theProj (project me))
        newItem)
    (setf (macMenu me) theMenu)
    (setf (mf::my-frame theMenu) me)
    ;; Fix the text and containment stuff.
    (let ((theText (text me)))
      (if (string= theText "")
        (set-menu-title (macMenu me) "Untitled")
        (set-menu-title (macMenu me) theText)))
    ;; Ok, now copy the menuitems!
    (setf original (originalAncestor me original Menu))
    (when original
      (setf (arrowColor me) (arrowColor original))
      (dolist (oldItem (menuItems original))
        (setf newItem (new oldItem :project theProj))
        (setf (menu newItem) me)))))

;;; Add a real menu to the menu object!

(let ((newMenu (make-instance 'gs:*SK8-menu*)))
  (setf (mf::my-frame newMenu) menu)
  (setf (macMenu menu) newMenu))
      
;;; This handler associates a menu a menubar or another menu. It might be necessary to mess with the
;;; containment hierarchy...

(defun install-menu (me otherMenu &key following (listUpdate t))
  (unless (eq (installedIn me) otherMenu)
    ;; It might be necessary to deinstall the menu...
    (cond ((installedIn me)
           (deinstall-menu me))
          ((container me) (setf (container me) nil)))
    ;; Now do the installation...
    (add-menu otherMenu me :after following :listUpdate listUpdate)))

(defun deinstall-menu (me &key (listUpdate t))
  (let ((container (installedIn me)))
    (when container
      (remove-menu container me :listUpdate listUpdate))))

(define-handler (setf menuType) (newType menu)
  (setf (slot-value me 'menuType) newType)
  (gs:making-self-dirty (me t nil nil nil (gs:node-boundsRegion me) t t)
    (if (eq newType 'menu)
      (progn (setf (gs:node-drawFunction me) 'gs:sk8-text-draw)
             (setf (textHOffset me) 0))
      (progn (setf (gs:node-drawFunction me) 'popup-menu-draw)
             (setf (textHOffset me) 11)))
    (when (eq newType 'popUp)
      (resizeToText me))))

(define-handler SK8::menuBar (Menu)
  (let ((theMenubar (installedIn me)))
    (when (inheritsFrom theMenubar SK8::Menubar)
      theMenubar)))

(define-handler (setf SK8::menuBar) (theMenubar Menu &key following (listUpdate t))
  (if themenubar
    (progn
      (ensureType theMenubar SK8::Menubar)
      (install-menu me theMenubar :following following :listUpdate listUpdate))
    (deinstall-menu me)))

(define-handler menu (menu)
  (let ((theMenu (installedIn me)))
    (when (inheritsFrom theMenu menu)
      theMenu)))

(define-handler (setf menu) (theMenu menu &key following)
  (if theMenu
    (progn
      (ensureType theMenu menu)
      (install-menu me theMenu :following following))
    (deinstall-menu me)))

(define-handler (setf hSpace) (newValue menu)
  (ensureType newValue integer)
  (sk8::setValue 'hSpace me newValue)
  (lightForceRedraw me))

(define-handler (setf vSpace) (newValue menu)
  (ensureType newValue integer)
  (sk8::setValue 'vSpace me newValue)
  (lightForceRedraw me))

;;; ____________________
;;; Menu Layering Methods...
;;; ____________________

(defun reset-menu-numbers (theMenus)
  (let ((counter 1))
    (dolist (m theMenus)
      (setf (slot-value m 'layer) counter)
      (incf counter))))

;;; Figures out the right number for the menu.

(defun dynamic-number (theMenu)
  (cond ((inheritsFrom theMenu menu)
         (let ((theMenubar (or (menu theMenu) (SK8::menuBar theMenu))))
           (if theMenubar
             (if (inheritsFrom theMenubar SK8::Menubar)
               (1+ (position theMenu (menus theMenubar)))
               (1+ (position theMenu (menuItems theMenubar))))
             1)))
        (t ;; Just a menuitem.
         (if (menu theMenu)
           (1+ (position theMenu (menuItems (menu theMenu))))
           1))))

(define-handler (setf layer) (num menu)
  (unless (inheritsFrom num PositiveInteger)
    (sk8-error PropertyTypeMismatchError
               :object        num
               :expectedType  PositiveInteger
               :ownerObject   Menu
               :propertyName 'layer
               ))
  (decf num 1) ;; Subtracting 1 for internal processing.
  (let* ((macMenu (macMenu me))
         (container (or (menu me) (SK8::menuBar me))))
    (cond (container
           ;; OK, now set the number AND update the menus...
           (setf (slot-value me 'layer) (1+ num))
           (if (inheritsFrom container SK8::Menubar)
             ;; A menubar: reorder the menu list by hand!
             (let ((theMenus (gs:MoveToPosition me (menus container) num)))
               (setf (slot-value container 'menus) theMenus)
               (reset-menu-numbers theMenus)
               (call-next-method) ;; To set the actor's number as well!
               (computeSize container)
               )
             ;; A menu: remove all menus, arrange the list and add them again!
             (let* ((containerMenu (macMenu container))
                    (theItems (menu-items containerMenu)))
               (apply #'remove-menu-items containerMenu theItems)
               (apply #'add-menu-items containerMenu (gs:moveToPosition macMenu theItems num))
               (reset-menu-numbers (menuitems container)))))
          ((container me) ;; Must be a popUp on an actor!
           (call-next-method))
          (t (sk8-error GeneralProgrammaticError
                        :strings '("" " doesn't have a container.")
                        :objects (list me))))
    (layer me)))

(define-handler bringToFront (menu)
  (when (installedIn me)
    (setf (layer me) 1)))

(define-handler menuSiblings (menu)
  (let ((container (installedIn me)))
    (when container
      (if (inheritsFrom container SK8::Menubar)
        (menus container)
        (menuItems container)))))

(define-handler sendToBack (menu)
  (when (installedIn me)
    (setf (layer me) (length (menuSiblings me)))))

(define-handler sendFarther (menu)
  (when (installedIn me)
    (let ((oldNumber (layer me)))
      (unless (= oldNumber (length (menuSiblings me)))
        (setf (layer me) (1+ oldNumber))))))

(define-handler bringCloser (menu)
  (when (installedIn me)
    (let ((oldNumber (layer me)))
      (unless (= oldNumber 1)
        (setf (layer me) (1- oldNumber))))))

;;; if the menu is installed, make sure to deinstall it before changing the container.
;;; The user should only use this method to turn the menu into a popUp...

(define-handler (setf container) (theActor menu)
  (let ((container (installedIn me)))
    (unless (eq container theActor)
      (when (installedIn me)
        (deinstall-menu me)))
    (if (inheritsFrom theActor SK8::Menubar)
      (setf (menuType me) 'menu)
      (setf (menuType me) 'popUp))
    (call-next-method)))

(define-handler add-menu :private (menu otherMenu &key after listUpdate)
  (declare (ignore after listUpdate))
  (when (eq me otherMenu) 
    (sk8-error GeneralProgrammaticError 
               :strings '("You can't install menu " " into itself!")
               :objects (list me)))
  (unless (memq otherMenu (menuItems me))
    (setf (installedIn otherMenu) me)
    (add-menu-items (macMenu me) (macMenu otherMenu))))

(define-handler remove-menu :private (menu oldMenu &key listUpdate)
  (declare (ignore listUpdate))
  (setf (installedIn oldMenu) nil)
  (remove-menu-items (macMenu me) (macMenu oldmenu)))

(define-handler installed (menu)
  (menu-installed-p (macMenu me)))

(define-handler (setf fillColor) :after (color menu)
   (if (menuFillColor me)
     (set-part-color (macmenu me) :menu-background (mcl-color (menuFillColor me)))
     ;; Only use this color if the menuFillcolor is false!
     (set-part-color (macMenu me) :menu-background (mcl-color color))))

(define-handler (setf menuFillColor) :after (color menu)
  (if color
    (set-part-color (macMenu me) :menu-background (mcl-color color))
    ;; Use the fillcolor!
    (set-part-color (macMenu me) :menu-background (mcl-color (fillcolor me)))))

(define-handler (setf menuTextColor) :after (color menu)
  (if color
    (progn (set-part-color (macMenu me) :default-menu-item-title (mcl-color color))
           (setf (textcolor me) color))
    ;; Use the textColor!
    (set-part-color (macMenu me) :default-menu-item-title (mcl-color (textColor me)))
    ))

(define-handler (setf text) (title menu &key)
  (prog1
    (call-next-method)
    (set-menu-title (macMenu me) title)
    ;; resize the menu!
    (resizeToText me)
    (when (SK8::menuBar me)
      (computeSize (SK8::menuBar me)))))

;;; The textColor only affects the menu's title when no menuTextColor is specified, 
;;; but affects both otherwise.

(define-handler (setf textColor) :after (color menu &key)
  (let ((menuTextColor (menuTextColor me)))                     
    (if menuTextColor
      (set-part-color (macMenu me) :menu-title (mcl-color menutextColor))
      (progn 
        (set-part-color (macMenu me) :menu-title (mcl-color color))
        (set-part-color (macMenu me) :default-menu-item-title (mcl-color color))))))

(define-handler (setf textfont) (theFont menu)
  (call-next-method)
  ;; resize the menu!
  (resizeToText me)
  (when (SK8::menuBar me)
    (computeSize (SK8::menuBar me))))

(define-handler (setf textSize) (size menu)
  (call-next-method)
  ;; resize the menu!
  (resizeToText me)
  (when (SK8::menuBar me)
    (computeSize (SK8::menuBar me))))

(define-handler enabled (menu)
  (menu-enabled-p (macMenu me)))

;;; In addition to doing the menu-enable should change the textColor of the
;;; menu and save the old textColor...

(define-handler (setf enabled) (enable menu)
  (if enable
    ;; Restore the old color.
    (progn
      (setf (textColor me) (getf (gs:node-properties me) :enabled-menuTextColor Black))
      (menu-enable (macMenu me)))
    ;; Save the enabled textColor, set it to gray.
    (progn
      (setf (getf (gs:node-properties me) :enabled-menuTextColor) (textColor me))
      (setf (textColor me) Gray)
      (menu-disable (macMenu me)))))

(define-handler menuSelect (menu)
  (when (installedIn me) (menuselect (installedIn me)))
  me)

;;; update [menu] -- called by SK8 when the menu is selected but before it shows the menu items

(define-handler update (menu)
  (mapc #'update (menuItems me)))

(define-handler menuItems (menu)
  (and (macMenu me)
       (mapcar #'mf::my-frame (menu-items (macMenu me)))))

(define-handler (setf menuitems) (newValue Menu)
  (let ((oldItems (menuItems me))
        (count 1))
    ;;; [1] Remove old contents not in the new contents.
    (dolist (c oldItems)
      (unless (memq c newValue)
        (setf (menu c) nil)))
    ;; [2] Add new contents and fix the layers. 
    (dolist (c newValue)
      (unless (eq me (menu c))
        (setf (menu c) me))
      (unless (eq (layer c) count)
        (setf (layer c) count))
      (incf count))
    newValue))

(define-handler (Setf arrowColor) (value menu)
  (if (inheritsFrom value Renderer)
    (progn
      (setf (slot-value me 'arrowColor) value)
      (lightForceRedraw me))
    (sk8-error PropertyTypeMismatchError
               :object        value
               :expectedType  Renderer
               :ownerObject   Menu
               :propertyName 'arrowColor
               )))

(define-handler (Setf textLocation) (value menu)
  (withActorLocked (me)
    (call-next-method)
    (when (and (eq (textLocation me) 'center) (FitToText me)) 
      (resizeToText me))))


;;;; FOR COMPATABILITY WITH MENUITEMS!!!
(define-handler commandKey (menu)
  nil)

(define-handler (setf commandKey) (key menu)
  (declare (ignore key))
  (sk8-error GeneralProgrammaticError
             :strings '("Menus cannot have a command key")))


(define-handler checkMark (menu)
  nil)

(define-handler (setf checkMark) (ch menu)
  (declare (ignore ch))
  (sk8-error GeneralProgrammaticError
             :strings '("Menus cannot have a check mark.")))

(define-handler keycolor (menu)
  black)

(define-handler (setf keycolor) (ch menu)
  (declare (ignore ch))
  (sk8-error GeneralProgrammaticError
             :strings '("Menus cannot have a key color.")))


(define-handler checkcolor (menu)
  black)

(define-handler (setf checkcolor) (ch menu)
  (declare (ignore ch))
  (sk8-error GeneralProgrammaticError
             :strings '("Menus cannot have a check color.")))

;;; _________
;;; MENU ITEMS
;;; _________

(define-handler initialize (menuItem original isNew initArgs)
  (declare (ignore isNew initArgs))
  (call-next-method)
  (let ((OSPointer (make-instance 'gs:*SK8-menu-item*)))
    (setf (OSPointer me) OSPointer)
    (setf (mf::my-frame OSPointer) me)
    ;; copy some properties...
    (setf original (originalAncestor me original MenuItem))
    (when original
      (setf (commandKey me) (commandKey original))
      (setf (text me) (text original))
      (setf (enabled me) (enabled original)))))

(define-handler menu (menuItem)
  (let ((owner (slot-value (OSPointer me) 'ccl::owner)))
    (and owner
         (mf::my-frame owner))))

(define-handler (setf menu) (theMenu menuItem)
  (unless (eq theMenu (menu me))
    (cond (theMenu
           (ensureType theMenu menu)
           (when (menu me)
             (remove-menu-items (macMenu (menu me)) (OSPointer me)))
           (add-menu-items (macMenu theMenu) (OSPointer me))
           (setf (installedIn me) theMenu))
          ((menu me)
           (remove-menu-items (macMenu (menu me)) (OSPointer me))))
    (setf (slot-value me 'layer) (dynamic-number me))
    (setf (installedIn me) theMenu)))

(define-handler menuSelect (menuItem)
  (when (menu me) (menuselect (menu me)))
  me)

;;; Easier to do since always contained by a menu...

(define-handler (setf layer) (num MenuItem)
  (unless (inheritsFrom num PositiveInteger)
    (sk8-error PropertyTypeMismatchError
               :object        num
               :expectedType  PositiveInteger
               :ownerObject   MenuItem
               :propertyName 'layer
               ))  
  (decf num 1) ;; Subtracting 1 for internal processing.
  (let ((node (OSPointer me))
        (container (menu me)))
    (cond (container
           (let ((contents (menuItems container)))
             (unless (<= 0 num (length contents))
               (sk8-error GeneralProgrammaticError
                          :strings '("The layer " " must be between 0 and ")
                          :objects (list num (length contents))))
             ;; OK, now set the number AND update the menus...
             (setf (slot-value me 'layer) (1+ num))
             ;; A menu: remove all menus, arrange the list and add them again!
             (let* ((containerNode (macMenu container))
                    (theItems (menu-items containerNode)))
               (apply #'remove-menu-items containerNode theItems)
               (apply #'add-menu-items containerNode (gs:moveToPosition node theItems num))
               (reset-menu-numbers (menuItems container)))))
          (t (sk8-error GeneralProgrammaticError
                        :strings '("" " is not in a menu.")
                        :objects (list me))))))

(define-handler bringToFront (menuItem)
  (when (menu me)
    (setf (layer me) 1)))

(define-handler sendToBack (menuItem)
  (when (menu me)
    (setf (layer me) (length (menuItems (menu me))))))

(define-handler sendFarther (menuItem)
  (when (menu me)
    (let ((oldNumber (layer me)))
      (unless (= oldNumber (length (menuItems (menu me))))
        (setf (layer me) (1+ oldNumber))))))

(define-handler bringCloser (menuItem)
  (when (menu me)
    (let ((oldNumber (layer me)))
      (unless (= oldNumber 1)
        (setf (layer me) (1- oldNumber))))))

(define-handler enabled (menuItem)
  (menu-item-enabled-p (OSPointer me)))

(define-handler (setf enabled) (enable menuItem)
  (if enable
    (menu-item-enable (OSPointer me))
    (menu-item-disable (OSPointer me))))

(define-handler (setf keyColor) :after (color menuItem)
                       (set-part-color (OSPointer me) :item-key (mcl-color color)))

(define-handler (setf textColor) :after (color menuItem &key)
                       (set-part-color (OSPointer me) :item-title (mcl-color color)))

(define-handler (setf checkColor) :after (color menuItem)
                       (set-part-color (OSPointer me) :item-mark (mcl-color color)))

(define-handler commandKey (menuItem)
  (command-key (OSPointer me)))

(define-handler (setf commandKey) (key menuItem)
  (when key 
    (cond ((characterp key) t)
          ((and (stringp key) (= 1 (length key)))
           (setf key (elt key 0)))
          (t (sk8-error PropertyTypeMismatchError
                        :object        key
                        :expectedType  Character
                        :ownerObject   MenuItem
                        :propertyName 'commandKey
                        ))))
  (set-command-key (OSPointer me) key)
  key)

(define-handler checkMark (menuItem)
  (menu-item-check-mark (OSPointer me)))

(define-handler (setf checkMark) (ch menuItem)
  (set-menu-item-check-mark (OSPointer me) ch))

(define-handler text (menuItem &key)
  (menu-item-title (OSPointer me)))

(define-handler (setf text) (title menuItem &key)
  (require-type title 'string)
  (set-menu-item-title (OSPointer me) title))

(define-handler textStyle (menuItem &key num start end)
  (declare (ignore num start end))
  (let ((menu-item-style (menu-item-style (OSPointer me))))
    (mapcar #'(lambda (s) 
                (when (eq s :extend) (setf s :expand))
                (find-symbol (symbol-name s) :sk8))
            (if (listp menu-item-style)
              menu-item-style
              (cons menu-item-style nil)))))

(defmacro expand->extend (styleList)
  `(substitute :extend :expand ,styleList))

(define-handler (setf textStyle) (styleList menuItem &key)
  (when stylelist (require-type stylelist 'list))
  (set-menu-item-style (OSPointer me) 
                       (or (expand->extend (mapcar #'ccl::make-keyword styleList)) '(:plain)))
  (or styleList '(plain)))

(define-handler update (menuitem)
  )

;;; ___________
;;; POP UP MENUS 
;;; ___________

;; Brings up a menu instantly.  Should never be called by the user.

(defun instant-menu (theMenu where)
  (menu-install theMenu)
  (Setq where (subtract-points where (view-position gs:*currentTla*)))
  (with-slots (my-pos my-window) theMenu
    (setf my-pos where
          my-window gs:*currentTla*)
    (size-rectangles theMenu where)
    (prog1 (menu-select theMenu 0)
      (menu-deinstall theMenu))))

(define-handler menuSize (menu)
  (let ((theMenu (macmenu me))
        (*menubar-frozen* t)
        width height)
    (menu-install theMenu)
    (unwind-protect
      (let ((menu-handle (slot-value theMenu 'menu-handle)))
        (when (handlep menu-handle)
          (#_calcmenusize menu-handle)
          (setq width (rref menu-handle :menuInfo.menuWidth)
                height (rref menu-handle :menuInfo.menuHeight))))
      (menu-deinstall theMenu))
    (SK8-multivals width height)))

;;; Makes an instant menu from a normal menu frame. This function is complicated a lot to ensure that
;;; items of a menu that is disabled are disabled to. Since you cannot specify whether a menu is disabled
;;; in its initargs, we need that uglyness of binding the menu to a variable and then calling menuDisable...

(define-handler make-instant-menu :private (menu &key (defaultItem (defaultMenuItem me)))
   (let ((disabled? (not (enabled me)))
         theMenu)
     (setf theMenu
           (make-instance 'instant-menu
             :default-item defaultItem
             :menu-title (text me)
             :menu-colors (list :menu-background (mcl-color (or (menuFillcolor me) (fillcolor me)))
                                :default-item-title (mcl-color (or (menutextcolor me) (textColor me))))
             :menu-items
             (mapcar #'(lambda (curItem)
                         (if (inheritsFrom curItem menu)
                           (let ((subMenu (make-instant-menu curItem)))
                             (when disabled? (menu-disable subMenu))
                             subMenu)
                           (make-instance
                             'menu-item
                             :style (menu-item-style (OSPointer curItem))
                             :command-key (commandKey curItem)
                             :menu-item-checked (checkMark curItem)
                             :menu-item-title (text curItem)
                             :disabled (or disabled? (not (enabled curItem)))
                             :menu-item-action #'(lambda () curItem))))
                     (menuItems me))))
     ;; Disable the low level menu if the menu is disabled.
     (when disabled? (menu-disable theMenu))
     theMenu))

;;; getPopUpMenu
;; Returns a popup menu object for the supplied menu. It is also cached in
;; the menu for future retrieval.

;;; Note that if the defaultMenuItem is not 0, the menu will ignore the topleft position suggested for
;;; its popup location. When the menu is poped up near the bottom of the screen, then, the menu will
;;; go above the actor that poped it up, causing problems with our menuTrackProc hack.

(define-handler getPopUpMenu (menu &key (defaultmenuitem 0))
  (make-instant-menu me :defaultitem defaultmenuitem))

;; SELECTFROMMENU
;; Pops up a menu out of nowhere and allows the user to choose from it.

(define-handler selectFromMenu (menu &key
                                               (h 0) (v 0)
                                               (millisecondDelay 0)
                                               (RelativeActor nil)
                                               (position 'topleft)
                                               (defaultMenuItem nil))
  (let ((where #@(0 0))
        (result nil)
        (startTime (get-internal-real-time)))
    ;; Update the menu!
    (update me)
    (unless defaultMenuItem
      (setf defaultMenuItem (defaultMenuItem me)))
    ;; Computing the location where the menu will popUp. If a relative actor is provided we
    ;; use the position argument. Otherwise we use h and v.
    (if relativeActor
      (SK8-multival-bind (left top right bottom) (boundsRect relativeActor :physical t)
        ;; When it is a popup, make the menu start where the shadow begins.
        (when (eq (menuType me) 'popup)
          (setf bottom (- bottom (1- (vSpace me)))))
        (setq where
              (case position
                (topleft (gs:SK8Coords-to-point (+ 3 left) top))
                (topright (gs:SK8Coords-to-point (+ 3 right) top))
                (bottomleft (gs:SK8Coords-to-point (+ 3 left) bottom))
                (bottomright (gs:SK8Coords-to-point (+ 3 right) bottom))
                (mouseLoc (SK8-multival-bind (xx yy) (mouseLoc stage)    ;; Use the stage!!!
                            (gs:SK8Coords-to-point (+ xx 3) yy)))
                (bottomRightAligned 
                 ;; Compute the location of the top left given the actor's top right and the menusize.
                 (sk8-multival-bind (menuH menuV) (menuSize me)
                   (declare (ignore menuV))
                   (setf defaultMenuItem 1)  ;;; Always popup just below the menu in this case.
                   (gs:sk8Coords-to-point (- right menuH) bottom))
                 )
                (otherwise 
                 (SK8-error ArgumentTypeMismatchError
                            :handlerName 'selectFromMenu :argumentName 'position
                            :object position 
                            :expectedType '(topleft topright bottomleft bottomRight mouseLoc bottomRightAligned))))))
      (setq where (add-points where (make-point (+ 3 (gs:f.round h)) (gs:f.round v)))))
    ;; If a milisecond delay is provided, enforce it!
    (unless (zerop millisecondDelay)
      (loop (unless (mouse-down-p) (return-from selectFromMenu nil))
            (when (>= (- (get-internal-real-time) startTime)
                      millisecondDelay)
              (return nil))))
    ;; Make it and pop it up!
    (let ((popup (make-instant-menu me :defaultItem defaultMenuItem)))
      (when popUp
        ;; Poping up the menu and calling menuSelect.
        (if (setq result (instant-menu popup where))
          (progn
            (setf (defaultMenuItem me) (1+ (or (position result (menuItems me)) -1)))
            (menuSelect result))
          ;; no result - maybe dispatch click
          (when (eventactor)
            (setq gs:*eventTime* (#_TickCount))
            (mouseUp (eventActor))
            ;; Returns false to signify that nothing was selected.
            nil))))))

;;; makes an instant menu from a list.

(defun make-item-menu (items &key (currentItem 0))
  (make-instance 'instant-menu
    :default-item (if (numberp currentItem)
                    currentItem
                    (or (position currentItem items)
                        4))
    :menu-items
    (mapcar #'(lambda (curItem)
                (make-instance
                  'menu-item
                  :menu-item-title
                  (princ-to-string curItem)
                  :menu-item-action #'(lambda ()
                                        curItem)))
            items)))

;;; ____________
;;; UTILITY MENUS!
;;; ____________

;;; We end by providing 3 usefull menus to the user: the Edit, file and Windows menu.
;;; These can be made into any menubars automatically gaining the correct functionality.
;;; We also provide a simple SK8 menubar which shields the user from the Lisp related
;;; parts of the environment... This is for users that can't be without it.

(new menu :objectName "EditMenu" :undisposable t :project sk8 :text "Edit")

(let ((mi (new menuItem :objectName "EditMenuCut"
               :undisposable t :project sk8 :text "Cut" :commandKey #\X)))
  (declare (special mi))
  (eval `(define-handler menuSelect (,mi)
           (cut gs:*currentTla*)))
  (setf (menu mi) EditMenu)
  (setq mi (new menuItem :objectName "EditMenuCopy"
                :undisposable t :project sk8 :text "Copy" :commandKey #\C))
  (eval `(define-handler menuSelect (,mi)
           (copy gs:*currentTla*)))
  (setf (menu mi) EditMenu)
  (setq mi (new menuItem :objectName "EditMenuPaste"
                :undisposable t :project sk8 :text "Paste" :commandKey #\V))
  (eval `(define-handler menuSelect (,mi)
           (paste gs:*currentTla*)))
  (setf (menu mi) EditMenu)
  (setq mi (new menuItem :objectName "EditMenuClear"
                :undisposable t :project sk8 :text "Clear"))
  (eval `(define-handler menuSelect (,mi)
           (clear gs:*currentTla*)))
  (setf (menu mi) EditMenu))

;;; Disables all menu items in the edit menu except Cut/Copy/Paste. Returns the list of menu items
;;; that were disabled by this operation.

(defun disable-non-standard-menu-items (theEditMenu)
  (let (result)
    (dolist (c (menuitems theEditMenu) result)
      (unless (memq (commandKey c) '(#\C #\X #\V #\A #\.))
        (when (enabled c)
          (setf (enabled c) nil)
          (pushnew c result))))))

;;; WindowsMenu -- this is a very special case that is not replicated anywhere in the system. The special
;;;              case arises because we want to use Lisp's window menu directly. We need to do the
;;;              following things:
;;;
;;; (1) set the macmenu slot directly to Lisp's menu.
;;; (2) redefine make-instant-menu to make an instant menu from a macmenu.
;;; (3) make menuItems of windowsmenu return false.
;;; (4) define menuSelect on the menuItems of the windows menu to select the proper window.

(new menu :objectName "WindowsMenu" :project sk8)
(setf (text windowsMenu) "Windows")

(new menuItem :objectName "WindowsMenuItem" :project sk8
     :properties '((sk8::window :value nil)))

(define-handler menuSelect (WindowsMenuItem)
  (when (sk8::window me)
    (bringup (sk8::window me))))

;;; Make the items be the ordered list of windows.

(define-handler update (WindowsMenu)
  (let ((theWindows (contents Stage))
        theItem)
    ;; Clear the items.
    (dolist (c (menuitems me))
      (setf (menu c) nil))
    ;; Now make new items and add them to the menu.
    (dolist (c theWindows)
      (setf theItem (new WindowsMenuItem :text (objectString c) :project (project me)))
      (setf (sk8::window theItem) c)
      (setf (sk8::menu theItem) me))))

(new menu :objectName "FileMenu" :undisposable t :project sk8 :text "File")

(let ((mi (new menuItem :objectName "FileMenuClose"
               :undisposable t :project sk8 :text "Close" :commandKey #\W)))
  (declare (special mi))
  (eval `(define-handler menuSelect (,mi)
           (window-close gs:*currentTla*)))
  (setf (menu mi) FileMenu)
  (setq mi (new menuItem :objectName "FileMenuQuit"
                :undisposable t :project sk8 :text "Quit" :commandKey #\Q))
  (eval `(define-handler menuSelect (,mi)
           (quit)))
  (setf (menu mi) FileMenu))

(new SK8::MenuBar :objectName "SimpleMenubar" :undisposable t :project sk8)
(setf (menus SimpleMenubar) (list fileMenu editMenu windowsMenu))

;;; ___________________
;;; THE MENU SELECT MODE!
;;; ___________________

(new eventMode :objectName "MenuSelectMode" :project sk8
     :properties '(currentMenuBar menuenterLock))

(define-handler deactivate (MenuSelectMode)
  ;; reset the slots
  (setf (currentMenuBar me) nil
        (menuenterLock me) nil)
  (call-next-method))

(define-handler SelectFromMenubar (SK8::Menubar &key initialMenu)
  (withCursor watchCursor
    (setf (currentMenuBar MenuSelectMode) me
          (menuenterLock MenuSelectMode) initialMenu))
  (enterMode MenuSelectMode)
  )

(define-handler handleMouseUp (MenuSelectMode)
  (exitMode me)
  t)

(define-handler handleMouseDown (MenuSelectMode)
  t)

(define-handler handleidle (MenuSelectMode)
  (if (not (mouse-down-p)) 
    (exitMode me)
    (let* ((initial-menu (menuenterLock MenuSelectMode))
           ;; The eventActor will not do here!!! Since within an event mode, the eventActor
           ;; is not being updated.
           (menu-to-select (or initial-menu (gs:object-under-mouse))))
      (when (or initial-menu
                (and (inheritsFrom menu-to-select menu)
                     (eq (container menu-to-select) (currentMenuBar me))))
        (menuEnter menu-to-select)
        (when initial-menu (setf (menuenterLock MenuSelectMode) nil)))))
  t)

(define-handler mouseDown (SK8::Menubar)
  (when (eq me (eventActor))
    (selectFromMenubar me)))

(define-handler mouseDown :before (menu)
                (when (and (eq (menuType me) 'popup) (macMenu me) (depresses me))
                  (setf (depressed me) t)
                  (lightForceRedraw me)))

(define-handler mouseDown (menu)
  (call-next-method)
  (if (sk8::menubar me)
    (selectFromMenubar (SK8::menubar me) :initialMenu me)
    (if (equal (text me) "")
      (SelectFromMenu me :relativeActor me :position 'bottomRightAligned)
      (SelectFromMenu me :relativeActor me))))

(define-handler mouseDown :after (menu)
                (when (and (eq (menutype me) 'popup) (depresses me))
                  (setf (depressed me) nil)
                  (lightForceRedraw me)))

(define-handler menuEnter (menu)
  (let (where popUp result)
    (unwind-protect
      (progn
        (setf (highlight me) t)
        ;; compute the menu escape rect
        (let* ((physRect (gs:recompute-physicalboundsRect me))
               (windowpos (view-position (gs:node-window me)))
               (windowLeft (point-h windowpos))
               (windowTop (point-v windowpos))
               (ll (gs:rect-left physRect))
               (tt (gs:rect-top physRect))
               (rr (gs:rect-right physRect))
               (bb (gs:rect-bottom physRect)))
          (setf where (make-point (+ windowLeft ll 3) (+ windowTop bb)))
          (update me)
          (setq popup (sk8::getpopupmenu me))
          (when popUp
            ;; install the limitRect
            (setf (slot-value popUp 'menuTitleBounds) (list ll tt rr bb))
            ;; select from the menu
            (if (setq result (sk8::instant-menu popup where))
              (progn
                (exitMode MenuSelectMode)
                ;; if there was a result, do it
                (return-from menuEnter (menuSelect result))))
            ;; no result - maybe dispatch click
            (when (eventactor)
              (setq gs:*eventTime* (#_TickCount))
              (mouseUp (eventActor))))))
      (setf (highlight me) nil))))

(define-handler discard (menuItem)
  (setf (menu me) nil)
  (call-next-method))

(define-handler discard (menu)
  (if (menu me)
    (setf (menu me) nil)
    (when (SK8::menubar me)
      (setf (SK8::menubar me) nil)))
  (call-next-method))

(define-handler discard (SK8::Menubar)
  (when (eq me (sk8::menubar Stage))
    (setf (sk8::menubar Stage) nil))
  (call-next-method))

(new menuitem :objectname "MenuSpacer" :project sk8)
(setf (enabled menuspacer) nil)
(setf (text menuSpacer) "-")

;;; _______________________________ 
;;; Preserve and restore
;;; _______________________________ 

;; since we are screwing around with the menu-handle by setting it to nil sometimes (like in preserve)
;; we had better make sure that it does not bomb remove-menu-items
(defmethod ccl::remove-menu-items :before ((me menu) &rest args &aux item-num)
  (declare (dynamic-extent args))
  (without-interrupts
   (unless (slot-value me 'ccl::menu-handle)
     (dolist (item args)
       (when (memq item (slot-value me 'ccl::item-list))
         (setq item-num (ccl::menu-item-number item))
         (when (slot-value me 'ccl::menu-id)
           (when ccl::*color-available*
             (#_DeleteMCEntries  (slot-value me 'ccl::menu-id) item-num)))
         (setf (slot-value item 'ccl::owner) nil)
         (when (typep item 'menu)
           (let ((ccl::*menubar-frozen* t))
             (ccl::menu-deinstall item)))
         (setf (slot-value me 'ccl::item-list)
               (nremove item (slot-value me 'ccl::item-list)))))
     (dolist (item (slot-value me 'ccl::item-list))
       (ccl::update-color-defaults item)))))

;;; 1.0
;;; PRESERVE MENU -- tosses the computed menu. Clears the menu handle!

(define-handler preserve (menu)
  (let ((theMenu (macmenu me)))
    (when (typep theMenu 'menu)
      (setf (slot-value theMenu 'menu-handle) nil)))
  (call-next-method))

;;; 1.0
;;; RESTORE MENU -- recomputes the menu handle.  This is a major pain in the ass.

(defun sk8RestoreMenu (theMenu &aux (n 0) mh)
  ;; install the title
  (with-pstrs ((tp (slot-value theMenu 'ccl::title)))
    (when (eq 0 (%get-byte tp)) (%put-word tp #x0120))
    (setq mh (%setf-macptr (slot-value theMenu 'menu-handle)
                           (#_NewMenu (slot-value theMenu 'menu-id) tp))))
  ;; install the items
  (doList (item (slot-value theMenu 'ccl::item-list))
    (setq n (ccl::%i+ n 1))
    (ccl::install-menu-item mh item n))
  (menu-deinstall theMenu)
  ;; set the enable
  (when (not (slot-value theMenu 'ccl::enabledp))
    (setf (slot-value theMenu 'ccl::enabledp) t)
    (menu-disable theMenu)))

(define-handler restore (menu)
  (if (eq me menu)
    (restore-sk8-instant-menus)
    (progn
      ;; restore the items.
      (dolist (item (menuItems me))
        (restore item))
      ;; Restore the low level menu.
      (let* ((theMenu (macmenu me))
             (handle (slot-value theMenu 'menu-handle)))
        (unless (and (handlep handle)
                     (slot-value theMenu 'ccl::owner)) ;; OJO!!!
          (ccl::init-menu-id theMenu)
          (setf (slot-value theMenu 'menu-handle) (%null-ptr))
          (sk8RestoreMenu theMenu)
          (doList (item (slot-value theMenu 'ccl::item-list))
            (when (typep item 'menu) (menu-install item)))
          (ccl::set-part-color-loop theMenu (slot-value theMenu 'ccl::color-list))))))
  ;; And do the actor stuff!
  (call-next-method))

;;; _______________________________ 
;;; Command Key Handling
;;; _______________________________ 

;;; Loops through a menubarActor checking for a menu item with the command
;;; key theKey. Scans left to right and top to bottom. If no menuitem is found,
;;; returns nil.

(defun checkMenuBarForCmdKey (theMenuBar theKey)
  (catch :menuFound
    (dolist (oneMenu (menus theMenubar))
      (when (visible oneMenu)
        (update oneMenu) ; added this - chip
        (when (enabled oneMenu)
          (checkMenuForCmdKey oneMenu theKey))))))

;;; Needs to be recursive to deal with hierarchical menus.  

(defun checkMenuForCmdKey (theMenu theKey)
  (dolist (theItem (menuItems theMenu))
    (if (inheritsFrom theItem menu)
      (when (enabled theItem)
        (checkMenuForCmdKey theItem theKey))
      (when (and (enabled theItem) (commandKey theItem) 
                 (char= (char-upcase (commandKey theItem)) 
                        (char-upcase theKey)))
        ;; (menuSelect theItem)
        (throw :menuFound theItem)))))

;;; If theChar is found on the menubar, its menu is selected. Otherwise, we
;;; pass the event to the menubar of the stage (for normal MCL processing).

(define-handler commandKeyEvent (SK8::Menubar theChar)
  (let ((theItem (checkMenuBarForCmdKey me theChar)))
    (if theItem
      (unwind-protect 
        (progn (setf (Highlight (menu theItem)) t)
               (menuSelect theItem))
        (setf (Highlight (menu theItem)) nil))
      (commandKeyEvent stage theChar))))

;;; _______________________________ 
;;; Editable properties. 
;;; _______________________________ 

(define-handler localVirtualProperties (SK8::Menu)
  (when (eq me sk8::Menu)
    '(enabled)))

(define-handler localVirtualProperties (MenuItem)
  (when (eq me MenuItem)
    '(checkMark commandKey enabled menu text textStyle)))


#|
	Change History (most recent last):
	2	5/24/93	Hernan	The great unification: menus and menuActors
				are one. Menubars and menubarActors are one.
				This should simplify the whole menu experience
				for the SK8 user.
	3	5/24/93	Hernan	Set textfont and textSize of a menu do the work
				on its menuname. Still trying to make the new
				menus work...
	4	5/24/93	Hernan	Stoped ccl from complaining about redefinition
				of the menubar function.
	5	5/25/93	Hernan	(1) make menus inherit their menuType (done in
				initializeObject), (2) move the framecolor around
				when the menu changes type.
	6	5/25/93	Hernan	Second try to check in...
	7	5/25/93	Hernan	Framecolor of the menu equals the framecolor of
				its menuname. This avoids problems when
				switching menu types.
	8	5/25/93	chip	(1) MenuItem has a real low level menu item.
				(2) actorTextSize returns bogus values when
				conditions do not apply. This will have to do for
				now.
	9	5/25/93	rod	Now making sure the menu property is preserved
				when we make a new menubar.
	10	5/28/93	Hernan	Changed the way menus deal with colors.
	11	5/28/93	hernan	Menufillcolor and menutextcolor have become
				inheritable properties.
	12	6/15/93	Hernan	Menus remove their handles to let the store save
				them, and the menubar is restored to the top of
				the stage after the load is completed.
	13	6/21/93	Hernan	The menu object now has a CLOS menu. This makes
				the enabled property work.
	14	6/22/93	Hernan	Pop up menus now resize properly they change heights.
	15	6/22/93	Hernan	Moved actorTextSize to the actor file.
	16	6/25/93	Hernan	The Great Renaming of 93.
	17	6/25/93	Hernan	InitializeObject -> initialize.
	18	6/29/93	Hernan	mf::install-menubar takes an optional arg to 
				determine whether to show the menubar. Defaults to t.
	19	7/6/93	chip	
	24	7/10/93	chip	qualified occurrences of SK8::menubar
	26	7/28/93	chip	changed args to SAVE-TO-STORE of Menu & Menuitem
	27	8/31/93	hernan	(1) The menus now save to store correctly. This
				involves removing the handles from the menus
				before the save.
				(2) Fixing problem with menuFillcolor and
				menuTextColor
				(3) The resized method of the menuName refrains
				from crashing the system.
	28	9/1/93	hernan	The great integrating build for d4!!!
	29	9/2/93	hernan	Made the menuTextColor and menuFillcolor setters
				affect the textcolor of the menuname.
	30	9/22/93	kleiman	mf::inheritsFrom? -> inheritsFrom
	31	9/28/93	kleiman	added resume/suspend eventMode protocol
				exitMode handler -> deactivate handler
	32	9/29/93	hernan	Changed some menu addition/removal functions
				to allow the menu collections to work.
	33	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	34	10/1/93	hernan	Changing struct of menuitem to something else.
	35	10/5/93	hernan	Event modes that redefine handle-idle cannot
				expect eventActor to be in synch. Thus handle-idle
				of menuSelectMode uses the object under the mouse.
	36	10/8/93	hernan	The menus have been reimplemented using
				renderers. No more of this menuname nonsense.
	37	10/8/93	hernan	Changed arrowColor to not be inherited. We fake
				inheritance in the initialize method of the menu.
	38	10/8/93	hernan	Made pop ups pop up the menus in the right place.
	39	10/8/93	hernan	Thinking about computeSize and the menu's size.
	40	10/15/93	rod	
	41	10/19/93	rod	modified menutextsize to discriminate between popups and nonpopups
	42	11/1/93	kleiman	d4 store changes
	43	11/1/93	hernan	mf::inheritsFrom? -> inheritsFrom.
	44	11/2/93	rod	Fixed Menuarrow so it has an arrow size and
				the arrow is placed nicely
	45	11/2/93	rod	
	46	11/8/93	hernan	Making selectFromMenu update the menu before
				the place it pops up in is computed!
	47	11/8/93	kleiman	Added the menuspacer to this file.
	48	11/12/93	hernan	Made mousedown of menu propagate the event.
	49	11/22/93	hernan	Fixing setf menus to only remove menus whose 
				menubar is the menubar the method dispatched
				on. (This fixes a problem we got when the object
				system started copying stuff from the parents 
				when making children).
	50	11/23/93	hernan	Removing print statement left over in setf menus.
	51	11/24/93	hernan	Mousedown of menubar only calls selectFromMbar
				when the menubar is the event actor! Otherwise,
				selectFromMenubar is called twice (also by the 
				mousedown of menu) causing the menuSelectMode
				to be entered twice but only exited once. The 
				system is thus left in the wrong mode and 
				windows cannot be activated anymore.
	52	12/10/93	hernan	Fixing initialize of menu to set the text and clear 
				the menubar of a menu...
	53	12/21/93	sidney	Changes so files can be compiled
	54	1/10/94	hernan	Fonts just became objects!!!
	55	1/11/94	hernan	self -> me
	56	1/14/94	hernan	Labeling private properties private.
	57	2/12/94	kleiman	name changes
	58	2/15/94	kleiman	setf visible for menubar & set stage's menubar
				showing keyword
	59	2/15/94	kleiman	showing -> visible
	60	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	61	2/21/94	hernan	window -> sk8::window.
	62	2/23/94	rod	Fixing menubars updates.  Also making "Onstage"
				property public as a hook for users to catch
				putting a menubar in the stage.
	63	2/25/94	hernan	Using symbols instead of keywords for options!!!
	64	2/26/94	rod	Fixed a keyword to a symbol.
	65	2/28/94	hernan	Commenting out dispose methods.
	66	3/2/94	Hernan	The window's cut/paste methods are the usual
				ones. In clos-glue we define cut to call 
				cutSelectionToClipboard and so on...
	67	3/3/94	Hernan	checkRequiredObject -> ensureType.
	68	3/3/94	kleiman	private properties declared via makeprivateproperty
	69	3/6/94	rod	Fixed bugs.  Added stuff to menus to make them
				behave as menuitems.
	70	3/7/94	Hernan	Fixing set container of menu to actually do
				something!
	71	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	72	3/16/94	chip	Menu & Menubar no longer need their own print-object methods
	73	3/17/94	Hernan	Removing evil line that was somehow commented
				out of deinstall-menubar (hides the menubar).
	74	3/21/94	Hernan	Removing duplicate setf fillcolor method of menu.
	75	3/21/94	Hernan	Adding methods for setting the menu's hSpace and
				vSpace and force a redraw.
	77	3/24/94	Hernan	Added a function to disable all menusitems of an
				editmenu except the cut/copy/paste ones.
	78	3/25/94	Hernan	Also avoiding deselection select all and abort from
				the edit menu when a modal dialog comes up.
	79	3/28/94	Hernan	Reimplemented menuEnter to be a little more 
				efficient.
	80	3/29/94	Hernan	Setting the menu's location to something closer
				to the topleft of its container.
	81	3/31/94	Hernan	Setf sk8::menubar should not do anything if the
				stage's menubar is already set the right thing.
	82	4/8/94	Hernan	By making sure no defaultMenuItem is specified
				for our menus, we fix the problem of menus
				appearing near the bottom of the screen and 
				going above their menubars which causes our
				menutrackproc to be confused.
	83	4/12/94	Hernan	Avoiding use of contents when not necessary.
	84	4/13/94	kleiman	:plain -> 'plain' default textSTyle of menuItem
	85	4/18/94	rod	Fixing bug in menuEnter and selectfrommenu
	86	4/20/94	Hernan	Replacing calls to get-record-field with calls to
				region-into-rect (which does not cons).
	87	4/26/94	Hernan	Letting menu items have textStyles. This involved
				adding an initArg to the call to make-instance in
				make-instant-menu.
	88	4/27/94	Hernan	Make-instant-menu saves consing by getting the
				textStyle straight out of the original mf::menuitem.
	89	6/13/94	Hernan	Discard of menubar sets the menubar of the Stage
				to false it the menubar being discarded was installed.
	90	6/23/94	rod	Fixing defaultMenuItem problem in menus with
				no text.  I.E. Menus which are BottomRightAligned.
	91	6/28/94	Hernan	1170404: SelectFromMenu now returns whatever
				the menuSelect method returned.
	92	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	93	7/18/94	Hernan	1174188: find-symbol screwed up because of
				current package.
	94	8/9/94	Hernan	Removing the fillcolor handlers for menuitems.
	95	8/10/94	Hernan	1178226: make-instant-menu disables menus that
				are disabled. Also fixed the set enabled method to 
				disable menus when in all different forms (popup, 
				in menubar and in Mac menubar).
	96 	 8/22/94	Hernan  	1181996: the windows menu will now be a normal
							SK8 menu (no longer using the *windows-menu*
							to get this functionality).
	97 	 8/30/94	rod     	Changing order in select from menu so the user
							can set the defaultmenuitem in the update of
							the menu.
	98 	 8/31/94	Hernan  	*currentWindow* -> *currentTla*.
	99 	 9/ 2/94	rod     	Removing cursor.
	100	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	101	11/ 7/94	chip    	'installedIn' is now taken care of by creationRelations instead of initialize handlers of Menu & Menubar (radar #1195410)
	102	11/16/94	Hernan  	Fixing the error message that setf layer of menuitem yields
							when the menuitem is not related to a menu.
	103	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	104	 1/12/95	sidney  	removed some old dead code
	105	 2/16/95	sidney  	readable argument names for initialize handler
	106	 2/17/95	Hernan  	in install of Menu, after -> behind.
	107	 2/17/95	Hernan  	Fixing typo.
	108	 2/17/95	Hernan  	behind and below -> following
	109	 3/10/95	rod     	Fixing resizetotext of menu to take into
							account the textlocation.
	110	 3/13/95	Hernan  	1227813: fixed popup-menu-draw to call render-text
							assuming the textLocation is 'centerLeft'. This lets
							popups draw their text correctly.
	111	 3/20/95	rod     	Fixing menutype to not bash textlocation.
	112	 4/12/95	Hernan  	1238431: textStyle looks up the symbol in the sk8 package.
							(not in sk8dev).
	113	 4/14/95	Hernan  	1238417: MCL menuitems do not understand :expand. I am
							now translating to :extend and back when required (set
							textStyle of menuItem).
	2  	 6/23/95	Hernan  	1253540
	2  	 4/10/96	Hernan  	mf::instant-menu -> instant-menu.
	3  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 4/12/96	Hernan  	Fixing method congruency problems.
	5  	 4/18/96	Hernan  	Restoring the instant menus.
	6  	 4/18/96	Hernan  	Adding set menuitems.
	8  	 4/26/96	Hernan  	Adding "sneaky dev mode" functionality to SimpleMenubar.
	9  	 7/ 7/96	sidney  	changes for native PPC build
	10 	 9/30/96	sidney  	eventmode handlers must now return T to indicate that they handled the event
	11 	10/ 1/96	sidney  	when selecting a menu with an initial item, dispatch the handling to the same event queue as any menu selection
	12 	11/15/96	Hernan  	Initialize cannot assume that the "original" arg will have an
						object of the right type. Calling originalAncestor to deal with it.
	13 	 2/27/97	sidney  	Move code for "sneaky dev mode" to separate file loaded only in developer build
	14 	 2/27/97	Hernan  	
	15 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
