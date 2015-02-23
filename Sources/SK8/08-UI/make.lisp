;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

;; Make File for the UI

(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  2-14-96  10:39 am
                  UI::DEFAULTUIFONT UI::DEFAULTUIFONTSIZE)

(def-logical-directory "ui;" "sk8;08-UI:")

(defvar defaultUIFont sk8::EspySansFont)   ;;;; This is a global variable to make font experiments easy...
(defvar defaultUIFontSize 9)   ;;;; This is a global variable to make font experiments easy...

;;; *override-quit-questions* -- maybeSaveProject (called during SK8 quit) will not ask questions when this is true
(defvar *override-quit-questions* nil)

(cl-user::sk8-build-files "ui;UI-project")

(import '(sk8::withCursor sk8::withActorLocked) :ui)  ;;;; Is this still necessary?

;;;;;;;;  All of these guys are SK8 things which should be in the SK8 library...
(cl-user::sk8-build-files "ui;Screen Objects:UI Renderers"
                          "ui;Screen Objects:uiwindow"             ;; SK8 Dialog/Error Window.
                          "ui;Utilities:popUpPalette"
                          "ui;utilities:Menu Components"
                          "ui;utilities:Handler Tracing"
                          "ui;Dialogs:Layout DIalog"
                                                    
                          ;; UI prototypes and initial junk...
                          "ui;Screen Objects:Swatches and Updates"
                          "ui;Screen Objects:UIWindowWidgets"
                          "ui;Screen Objects:Edit Modes"
                          "ui;utilities:PortConnectors"
                          
                          ;;  For selection halo
                          "ui;Screen Objects:Selection"
                          "ui;Screen Objects:Selection Keys"
                          "ui;Screen Objects:Preferences Dialogs"
                          
                          ;;; Core UI Tools...
                          "ui;Screen Objects:Draw Palette"
                          "ui;Screen Objects:Finder"
                          "ui;Screen Objects:Property Editor"
                          "ui;Screen Objects:Object Editor"
                          "ui;Screen Objects:SystemBrowser"
                          "ui;Screen Objects:Menu Builder"
                          "ui;Screen Objects:Renderer Editors"
                          "ui;Screen Objects:MediaBrowser"
                          "ui;Screen Objects:UI Menubar"

                          ;;"ui;Screen Objects:SaveAsFasl"
                          "ui;Screen Objects:MessageBox"
                          "ui;Screen Objects:HandlerCallMessage"
                          "ui;Screen Objects:Project Editor"
                          "ui;Screen Objects:Stage Overviewer"
                          "ui;Screen Objects:Inheritance Viewer"
                          "ui;Screen Objects:Handler Tracer"
                          ;;;"ui;utilities:StackWatcher"
                          ;;;"ui;Screen Objects:Library Editor"
                          ;;;"ui;Screen Objects:Save As Editor"
                          
                          ;; Additional Error Dialogs
                          ;;"ui;Dialogs:ErrorInspectorDialog"
                          
                          "ui;Screen Objects:Selection Mode")


#|

;;; WHY WAS THIS DONE? Hernan.

;;;_____________________________________________
;;; This code marks all named dialogs in SK8 and their named deepcontents as private. 
(dolist (i (knowndescendants dialogbox))
  (when (eq (project i) sk8)
    (gs:map-nodes* #'(lambda (x) (if (objectname x) (setf (private x) t))) i)))

|#

(cl-user::sk8-build-files "SK8Script;editor:expressionWatcher"
                          "SK8Script;editor:SK8ScriptEditor"
                          "SK8Script;editor:editorInterface"
                          "ui;Screen Objects:ScriptWarningsWindow"
                          )

;; Make sure the Stage is covered.
(setf (sk8::fillcolor sk8::Stage) sk8::darkblue)
(setf (sk8::covered sk8::stage) t)

(cl-user::sk8-build-files "ui;Screen Objects:UI Script Editor" 
                          "ui;Screen Objects:UI Preferences")

;;_______________________________________________________

(uicolorize ui)

(setvalue 'targetproject ui sk8)  ;;; Do this here so we don't have to do it at startup.

(setf (container MessageBox) nil)

#|
	Change History (most recent last):
	2	5/11/93	Brian Roddy	
	3	5/11/93	Brian Roddy	
	4	5/21/93	false	
	5	5/24/93	Hernan	No need to load popUpMenu anymore. Also, make
				sure to load uiWindowWidgets before selection.
	6	5/25/93	Hernan	Added Finder -BJR
	7	6/1/93	Brian Roddy	Cleaned up
	17	7/28/93	chip	changed filename "UI.proj" to "UI-project.lisp" (so the lisp-file scanner would recognize it)
	18	8/30/93	chip	doesn't load a bunch of the editors (which were causing "vfileReal" bus errors)
	19	9/1/93	hernan	The great integrating build for d4!!!
	20	9/2/93	chip	now loads all the editors again
	21	9/20/93	rod	
	22	9/22/93	chip	now loads the "ChooseResponseDialog.lisp" dialog
	23	9/24/93	kleiman	corrected load problems with component browser
	24	9/24/93	chip	added HandlerCallMessage.lisp to load order
	25	9/24/93	kleiman	
	27	9/29/93	kleiman	Fixed dialog load ordering.  NEED TO CLEAN UP 
				ERROR INSPECTOR DIALOG ETC FROM USING UI STUFF
	32	10/13/93	rod	removed snapshotrenderer to renderers...
	33	10/15/93	rod	
	34	10/21/93	kleiman	moved "UI Renderer" before PortConnectors.lisp
	35	10/22/93	kleiman	nuked InfoWindow for the time being
	36	10/27/93	rod	
	37	10/29/93	It 	Added new MEDIA BROWSER STUFF!!!
	38	11/1/93	rod	
	40	11/1/93	rod	
	41	11/2/93	rod	
	42	11/3/93	kleiman	
	43	11/5/93	rod	
	44	11/5/93	rod	
	45	11/5/93	rod	
	46	11/5/93	rod	
	47	11/8/93	rod	
	48	11/8/93	kleiman	initialize-bogus-projects
	49	11/8/93	kleiman	initialize-bogus-projects typo
	50	11/8/93	rod	
	51	11/8/93	kleiman	initialize-bogus-jounrney
	52	11/15/93	rod	
	53	11/15/93	rod	
	54	11/15/93	rod	
	55	11/29/93	rod	
	56	11/29/93	rod	PrintMessage->Logit
	57	12/7/93	rod	
	58	12/10/93	rod	
	59	12/17/93	till	faslling
	60	12/21/93	sidney	Changes so files can be compiled
	61	12/23/93	rod	Adding PropertyEditorObjects
	62	1/24/94	rod	
	63	1/31/94	rod	
	64	2/3/94	kleiman	taking out initialize-bogus-project -- moving it
				to ui-project.lisp
	65	2/10/94	rod	
	66	2/14/94	rod	
	67	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	68	2/22/94	rod	
	69	2/22/94	kleiman	
	70	2/22/94	kleiman	
	71	2/23/94	rod	
	72	2/25/94	rod	
	73	2/25/94	rod	
	74	3/3/94	rod	Added New UI Script Editor
	75	3/5/94	rod	Added layout tool
	76	3/7/94	rod	
	77	3/11/94	rod	
	78	3/17/94	kleiman	
	79	3/24/94	rod	Making layout dialog a sk8 thing.  As well
				as simple property editors.
	84	6/3/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
				814/11/94Brian
				825/10/94rodobsoleting reliefbutton and objectgrid as they
				are no longer used.
				835/26/94rod
	85	6/6/94	kleiman	Problem 1165206: *override-quit-questions* special added. Used to
				prevent maybeSaveProject from asking questions.
	86	8/2/94	rod	Marking Dialogs as Private.
	87	8/2/94	rod	whoops.
	88	8/3/94	rod	Adding Help Window.
	89 	 8/30/94	Brian   	Putting renderers at the top.
	90 	 9/ 1/94	Hernan  	espysans -> espysansFont.
	91 	 9/ 1/94	rod     	
	92 	 9/ 1/94	Hernan  	
	93 	10/19/94	rod     	Removing libraries.
	94 	10/19/94	rod     	Adding the save as text window.
	95 	12/ 4/94	rod     	removing renderer components.
	96 	12/22/94	rod     	removing save as editor.
	2  	 7/ 5/95	sidney  	Add new file for temporary addition of menu for new save/load
	3  	12/ 8/95	Brian   	
	4  	12/11/95	Brian   	Removing all references to the stackwatcher, as it is
						going away.  It's too slow and cumbersome.  It's not
						general.  No one uses it.  The new debugging API doesn't
						support it.  Good riddance.
	5  	 2/ 6/96	Hernan  	Removing wood from SK8. No longer load the help window.
	6  	 2/ 9/96	sidney  	remove save/load fasl format menu items
	7  	 2/14/96	Brian   	removing constraint math.  no longer needed.
	8  	 2/15/96	Brian   	removing sk8 uitilities.
	2  	 5/ 7/96	Hernan  	Removing chooseResponseDialog.
	3  	 5/ 9/96	Hernan  	Loading the new interface to trace that the handler tracer
						uses.
	4  	 5/21/96	Brian   	For now, getting rid of Error Inspector Dialog.
	5  	 7/ 7/96	sidney  	changes for native PPC build
	6  	10/11/96	Brian   	removing crossauthorable.
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
