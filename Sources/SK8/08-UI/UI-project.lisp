;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  2-14-96   1:40 pm
                  SK8::ADDCLEAREDOBJECT SK8::CLEARS SK8::CREATEDNEWPROJECT SK8::UICLEAREDOBJECTS)


;; make the UI project, namespace, etc.

(define-sk8-var uiSelection :initial-value nil)

(new project
     :objectname "UI"
     :old t    ; Uses existing project file
     :filename "sk8;SK8 Temporary Files:ProjectBuilder"
     :project sk8)

(mf::initialize-bogus-project sk8::ui)  ;;; The ui is such a bogus project.

;;; __________________
;;; The ui's event interests!
;;; __________________

(addEventInterest UI 'setInform System)
(addEventInterest UI 'openedProject System)
(addEventInterest UI 'createdNewProject System)

;;;;;--------------------------------------------------------------

(addProperty ui 'targetProject)

(define-handler targetProject (ui)
  (let ((xx (sk8::getValue 'targetproject me)))
    (if (and (inheritsfrom xx project) (neq xx ui))
      xx
      sk8)))

(define-handler (setf targetProject) (theval ui)
  (unless (eq theVal (targetProject ui))
    (withlockedcursor animatedclock
      (uidev::storescripteds (sk8::getValue 'targetproject me))
      (if (and (inheritsfrom theval project) (neq theval ui))
        (sk8::setValue 'targetproject me theval)
        (sk8::setValue 'targetproject me sk8))
      (clearreferences UndoableSetLog)
      (setupforproject ui :project (sk8::getValue 'targetproject me)))))

(addProperty ui 'Clears)
(setf (Clears ui) (ccl::%cons-population nil))

(defun uiClearedObjects () (ccl::population-data (clears ui)))
(defun addClearedObject (obj) (push obj (ccl::population-data (clears ui))))

(define-handler clearReferences (ui &key ((:objects theobjects)))
  (unless (listp theobjects) (setf theobjects (list theobjects)))
  (mapc #'addClearedObject theobjects)
  (dolist (i (delete-duplicates (append (list ui::drawpalette ui::selectionhalo ui::propertyeditor) 
                                        (remove-if-not #'(lambda (x) (and (eq (project x) ui)) (is-a x ui::uisimplewindow))
                                                       (contents stage)))))
    (clearreferences i :objects theobjects))
  (dolist (i theobjects)
    (when (is-a i actor)
      (dolist (j (cons i (containers i)))
        (uidev::updateSwatchofActor j))))
  (setf (ui::SelectedItem ui::PickActorMode) nil)
  (setf (ui::ItemOver ui::PickActorMode) nil)
  (dolist (i theobjects) (uidev::RemoveSwatchFromTable i))
  )

(define-handler SetUpForProject (ui &key ((:project theproject)))
  (dolist (i (delete-duplicates (append (list ui::drawpalette ui::selectionhalo ui::propertyeditor ui::uiScripteditor) 
                                        (remove-if-not #'(lambda (x) (and (eq (project x) ui) 
                                                                          (is-a x ui::uisimplewindow)
                                                                          (not (is-a x ui::uiScripteditor))))
                                                       (contents stage)))))
    (SetUpForProject i :project theproject)))

(define-handler openedProject (ui proj)
  (sk8dev::withLockedCursor animatedClock
    (setf (targetProject ui) proj)
    (ui::loadPreferences ui proj)
    ))

(define-handler CreatedNewProject (ui proj)
  (sk8dev::withLockedCursor animatedClock
    (setf (targetProject ui) proj)
    (unless gs:*no-ui-windows*
      (withactorlocked (ui::projectoverviewer)
        (bringup ui::projectoverviewer)
        (unless (currenttool ui::projectoverviewer) (mousedown ui::POBObjects))
        ))))


;;;;;--------------------------------------------------------------
;;MCL3.0
(define-handler restore (ui)
  (Call-next-method)
  ;; Now bring up our windows.
  (unless gs:*no-ui-windows*
    (setf (inputobjects ui::userpalette :includeselection t) nil)
    (setf (selecteditem UI::LibraryPaletteMenu) sk8)
    (menuselect ui::LibraryPaletteMenu)
    ;; Putting windows in the right place.
    (unless (remove sk8 (remove ui (knownchildren project))) ;; only do if no user projects?
      (sk8-multival-bind (ll tt rr bb) (boundsrect (mainmonitor system))
        (declare (ignore ll tt))
        (setf (top ui::infowindow :resizing nil) 20)
        (setf (right ui::infowindow :resizing nil) (- rr 5))
        (setf (top ui::drawpalette :resizing nil) 20)
        (setf (left ui::drawpalette :resizing nil) 5)
        (setf (bottom ui::projectoverviewer :resizing nil) (- bb 5))
        (setf (left ui::projectoverviewer :resizing nil) 5)
        (setf (bottom MessageBox :resizing nil) (- bb 5))
        (setf (right MessageBox :resizing nil) (- rr 5))
        ))
    (push ui (eventListeners system))
    (process-run-function
     '(:name "Restoring UI" :background-p t)
     #'(lambda (files-p)
         (ui::loadTexture (nth (random 5) (list ui::UIGrayTextures ui::UIBlueTextures ui::UIPinkTextures ui::UIGreenTextures ui::UIYellowTextures)))
         (let ((otherproj (car (remove sk8 (remove ui (knowndescendants project))))))
           (unless (eq (sk8::menubar stage) ui::projectbuildermenubar)
             (setf (sk8::onstage ui::projectbuildermenubar) nil))
           (unless (or otherproj files-p)
             (bringUp ui::drawpalette)
             (bringUp ui::infowindow)
             ;;; Make sure this is brought up last, so if the user 
             ;;; cancels the open project dialog, then this is the first keytarget.
             (bringUp MessageBox))
           (when otherproj
             (setf (targetProject ui) otherproj))
           (unless (or otherproj (not (ActiveApplication System)) files-p)
             (OpenProjectDialog :newbutton t))
           ))
     (not (null (cdr mf::*files-loaded-from-finder*))))
    ))

(define-handler preserve (ui)
  ;; Remove windows from the stage.
  (dolist (i (contents stage))
    (if (eq (project i) ui)
      (setf (container i) nil)))
  ;; Make absolutely sure they have no references...
  (mapknowndescendants ui::uisimplewindow #'clearreferences)
  ;; Preserve everything.
  (call-next-method)
  ;;;Deal with our PixelMaps as we are not a "REAL" project...
  (setf (inputobjects ui::userpalette :includeselection t) nil)
  (setf (inputobjects UI::LibraryPalette :includeselection nil) nil)
  (setf ui::SwatchHashTable (make-hash-table :test #'eq))
  )

;; some cleanup for when we do a buildstandalone without the projectbuilder ui
(define-handler closeProject (ui)
  (when (eq me ui) ;; don't bother if someone makes and closes a child of UI
    (setf (systemLog sk8::SK8) nil)
    (when (and (sk8::menubar Stage) (eq (project (sk8::menubar Stage)) ui))
      (setf (sk8::menubar Stage) SimpleMenuBar))
    (when (and *user-menubar-obj* (eq (project *user-menubar-obj*) ui))
      (setf *user-menubar-obj* SimpleMenuBar)))
  (call-next-method))

(import '(ignore-sk8-multivals SK8-multivals SK8-multival-setf SK8-multival-bind) :ui)

#|
	Change History (most recent last):
	3	5/11/93	Brian Roddy	
	4	5/12/93	Brian Roddy	makes a bogus palette so the demo is fast
	5	5/12/93	Brian Roddy	
	6	5/21/93	false	Got rid of demo kludge
	7	6/11/93	Brian Roddy	
	8	6/26/93	kleiman	initialize -> openProject
				initializeObject -> initialize
	9	6/28/93	rod	wantsIdle is set to true
	1	7/28/93	chip	changed name to "UI-poject.lisp" (so the lisp-file scanner will recognize it)
	2	9/20/93	rod	
	3	9/24/93	kleiman	
	4	9/27/93	kleiman	duplicate fix
	5	10/8/93	rod	
	6	10/11/93	rod	
	7	10/15/93	rod	
	8	10/19/93	rod	
	9	10/21/93	kleiman	Changed to use new store
	10	10/21/93	kleiman	sk8;ui -> sk8;ProjectBuilder
	11	10/22/93	kleiman	sk8;ui -> sk8;ProjectBuilder
	12	10/25/93	kleiman	!set-up-project-error-tables added after NEW.
				This is done because the Project Builder is opened
				in a  non-standard manner (i.e., it is not opened
				by calling openProject because it is not actually
				in a project file).
	13	10/27/93	rod	
	14	11/1/93	kleiman	restore File object used in Project's file property
	15	11/5/93	rod	
	16	11/6/93	rod	
	17	11/12/93	kleiman	Correctly initializing the UI's project file.
	18	11/15/93	rod	
	19	11/15/93	rod	
	20	11/23/93	rod	
	21	11/24/93	hernan	Rewriting openProject into restore of UI. This 
				method restores the drawing palette and makes 
				sure the windows are at the right places.
				Also adding preserve and restore for UI. Preserve
				sets the container of all the windows to nil. 
				Restore gets the windows back up.
	22	11/24/93	hernan	Has to eval-enqueue the last form in restore!
	23	11/30/93	rod	
	24	12/2/93	hernan	Making restore of ui not bring up the windows if 
				the no-ui-windows variable is true.
	25	12/3/93	hernan	Removing moveOffStage of swatchtemprect from
				restore of ui.
	26	12/5/93	rod	
	28	2/3/94	kleiman	added initialize-bogus-project -- moved it
				from make.lisp
	29	2/16/94	rod	
	30	2/16/94	rod	
	31	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	32	2/22/94	rod	
	33	2/22/94	rod	
	34	2/22/94	kleiman	
	35	2/25/94	hernan	Using symbols instead of keywords for options!!!
	36	2/26/94	rod	
	37	2/26/94	rod	
	38	2/28/94	rod	targetproject
	39	2/28/94	rod	Added targetProject property.
	40	3/1/94	hernan	projectOverviewer is in the ui!
	41	3/5/94	rod	Changing draw palette
	42	3/7/94	rod	
	43	3/9/94	rod	Doing Project Switching and Reference Clearing.
	44	3/10/94	rod	
	45	3/10/94	rod	
	46	3/16/94	sidney	Don't open file dialog on startup if finder is passing file arguments in
	47	3/18/94	rod	
	48	3/18/94	rod	
	61	5/6/94	rod	
	62	5/6/94	rod	
	63	6/3/94	rod	
	64	6/12/94	rod	Making project switching behave.
	65	6/14/94	Hernan	Commenting out covering of the Stage which is
				done by restore of SK8 from now on.
	66	6/17/94	chip	!set-up-project-condition-tables no longer called directly on UI (new of Project does it fine)
	67	7/14/94	Hernan	Expressing the UI's interest in the following 
				system events: createdNewProject, openedProject and setInform.
	68	7/15/94	rod	Fixing setupproject for scripteditor
	69	8/4/94	rod	adding openedlibrary.
	70	8/4/94	rod	adding openedlibrary.
	71 	 9/ 1/94	rod     	Optimizations.
	72 	 9/21/94	rod     	
	73 	 9/28/94	rod     	
	74 	10/19/94	rod     	Removing libraries.
	75 	11/16/94	sidney  	sk8 folder -> SK8 Resources folder
	76 	12/ 6/94	rod     	Jamies Request: if starting from a project, don't
							show the windows first, just have the prefs 
							handle the windows.
	77 	12/ 7/94	rod     	
	78 	12/21/94	rod     	
	79 	 1/ 4/95	sidney  	remove remaining traces of obsoleted lib functionality
	80 	 1/16/95	rod     	assuming the pbmenubar will be in the stage.
	81 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	82 	 2/16/95	rod     	clear reference work.
	83 	 2/16/95	sidney  	ui project file moved to temporary files folder
	84 	 3/23/95	rod     	
	85 	 3/27/95	sidney  	change default menubar when building standalone without projectbuilder ui
	86 	 3/31/95	sidney  	1233851: remove messagebox as system log when building standalone without projectbuilder ui
	87 	 4/12/95	rod     	
	88 	 4/14/95	Hernan  	set targetProject only does anything if the new value
							is a new value.
	3  	10/ 3/95	Brian   	
	4  	 2/14/96	Brian   	Cleaning up our need for a population.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 5/ 7/96	sidney  	Changes for new object system
	4  	11/12/96	sidney  	name anonymous threads
	5  	 1/ 6/97	Brian Roddy	
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
